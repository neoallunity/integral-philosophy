#!/usr/bin/env python3
"""
Comprehensive unit tests for Web Scraper component
Tests Selenium-based scraping functionality, content extraction, and error handling
"""

import pytest
import json
import tempfile
from pathlib import Path
from unittest.mock import Mock, MagicMock, patch, call
import time
from selenium.common.exceptions import TimeoutException, WebDriverException

from tests.utils.base_test_classes import ComponentTestCase, SeleniumTestCase


class TestWebScraper(ComponentTestCase):
    """Test cases for WebScraper class"""

    def setup_method(self):
        """Setup test environment"""
        super().setup_method()
        self.base_url = "https://example.com"
        self.output_dir = self.temp_dir / "scraped_content"
        self.scraper_config = {
            "max_pages": 5,
            "timeout": 30,
            "rate_limit": 1.0,
            "respect_robots": True,
            "user_agent": "Integral-Philosophy-Test/1.0",
        }

    @pytest.fixture
    def mock_driver(self, mock_webdriver):
        """Mock Selenium WebDriver"""
        return mock_webdriver

    @pytest.fixture
    def sample_site_ast(self):
        """Sample site AST for testing"""
        return {
            "pages": {
                "https://example.com": {
                    "title": "Example Site",
                    "content": "# Welcome\n\nThis is a test site.",
                    "metadata": {
                        "title": "Example Site",
                        "description": "Test site for scraping",
                        "language": "en",
                    },
                    "links": ["https://example.com/about"],
                },
                "https://example.com/about": {
                    "title": "About Us",
                    "content": "# About\n\nInformation about us.",
                    "metadata": {
                        "title": "About Us",
                        "description": "About page",
                        "language": "en",
                    },
                    "links": [],
                },
            },
            "metadata": {
                "base_url": "https://example.com",
                "domain": "example.com",
                "scraped_at": "2024-01-01 12:00:00",
                "total_pages": 2,
            },
        }

    @patch("selenium.webdriver.Chrome")
    def test_scraper_initialization(self, mock_chrome, mock_driver):
        """Test WebScraper initialization"""
        mock_chrome.return_value = mock_driver

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir), delay=0.5)

        assert scraper.base_url == self.base_url
        assert scraper.domain == "example.com"
        assert scraper.output_dir == self.output_dir
        assert scraper.delay == 0.5
        assert len(scraper.visited_urls) == 0
        assert len(scraper.downloaded_urls) == 0
        assert len(scraper.failed_urls) == 0
        assert scraper.site_ast["metadata"]["base_url"] == self.base_url

    @patch("selenium.webdriver.Chrome")
    def test_setup_driver_success(self, mock_chrome, mock_driver):
        """Test successful WebDriver setup"""
        mock_chrome.return_value = mock_driver

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))
        result = scraper.setup_driver()

        assert result is True
        assert scraper.driver == mock_driver
        mock_chrome.assert_called_once()

        # Check Chrome options
        call_args = mock_chrome.call_args[0][0]
        assert "--headless" in call_args.arguments
        assert "--no-sandbox" in call_args.arguments
        assert "--disable-dev-shm-usage" in call_args.arguments

    @patch("selenium.webdriver.Chrome")
    def test_setup_driver_failure(self, mock_chrome):
        """Test WebDriver setup failure"""
        mock_chrome.side_effect = Exception("Driver initialization failed")

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))
        result = scraper.setup_driver()

        assert result is False
        assert scraper.driver is None

    @patch("selenium.webdriver.Chrome")
    def test_wait_for_page_load_success(self, mock_chrome, mock_driver):
        """Test successful page load wait"""
        mock_chrome.return_value = mock_driver
        mock_driver.execute_script.side_effect = [
            "complete",  # Document ready
            False,  # jQuery not present
        ]

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))
        scraper.driver = mock_driver

        result = scraper.wait_for_page_load(timeout=5)

        assert result is True
        assert mock_driver.execute_script.call_count >= 1

    @patch("selenium.webdriver.Chrome")
    def test_wait_for_page_load_timeout(self, mock_chrome, mock_driver):
        """Test page load wait timeout"""
        mock_chrome.return_value = mock_driver
        mock_driver.execute_script.return_value = "loading"

        from scripts.web_scraper import WebScraper
        from selenium.webdriver.support.ui import WebDriverWait

        with patch("selenium.webdriver.support.ui.WebDriverWait") as mock_wait:
            mock_wait.side_effect = TimeoutException("Page load timeout")

            scraper = WebScraper(self.base_url, str(self.output_dir))
            scraper.driver = mock_driver

            result = scraper.wait_for_page_load(timeout=1)

            assert result is False

    @patch("selenium.webdriver.Chrome")
    def test_extract_links(self, mock_chrome, mock_driver):
        """Test link extraction from page"""
        mock_chrome.return_value = mock_driver

        # Mock anchor elements
        mock_link1 = Mock()
        mock_link1.get_attribute.return_value = "/page1"
        mock_link2 = Mock()
        mock_link2.get_attribute.return_value = "https://example.com/page2"
        mock_link3 = Mock()
        mock_link3.get_attribute.return_value = "https://external.com/page"  # External
        mock_link3.get_attribute.side_effect = Exception("Invalid link")

        mock_driver.find_elements.return_value = [mock_link1, mock_link2, mock_link3]
        mock_driver.current_url = "https://example.com"

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))
        scraper.driver = mock_driver

        links = scraper.extract_links()

        # Should return same-domain links only
        assert len(links) == 2
        assert "https://example.com/page1" in links
        assert "https://example.com/page2" in links

    @pytest.mark.parametrize(
        "url,expected",
        [
            ("https://example.com/page", True),
            ("https://www.example.com/page", True),
            ("https://external.com/page", False),
            ("", False),
            ("invalid-url", False),
        ],
    )
    def test_is_same_domain(self, url, expected):
        """Test same domain checking"""
        from scripts.web_scraper import WebScraper

        scraper = WebScraper("https://example.com", str(self.output_dir))
        result = scraper.is_same_domain(url)

        assert result == expected

    @patch("selenium.webdriver.Chrome")
    def test_extract_content_success(self, mock_chrome, mock_driver):
        """Test successful content extraction"""
        mock_chrome.return_value = mock_driver

        # Mock JavaScript execution
        page_info = {
            "title": "Test Page",
            "description": "Test description",
            "keywords": "test, page",
            "language": "en",
            "author": "Test Author",
            "canonical": "https://example.com/test",
            "structure": {
                "headings": [{"level": 1, "text": "Test Title", "id": ""}],
                "lists": 2,
                "tables": 1,
                "images": 3,
                "links": 5,
            },
        }

        mock_driver.execute_script.return_value = page_info
        mock_driver.page_source = (
            "<html><body><h1>Test Title</h1><p>Test content</p></body></html>"
        )
        mock_driver.current_url = "https://example.com/test"

        # Mock main content extraction
        mock_element = Mock()
        mock_element.get_attribute.return_value = (
            "<main><h1>Test Title</h1><p>Test content</p></main>"
        )
        mock_driver.find_element.return_value = mock_element

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))
        scraper.driver = mock_driver

        content = scraper.extract_content()

        assert content is not None
        assert "metadata" in content
        assert "content" in content
        assert "raw_html" in content
        assert content["metadata"]["title"] == "Test Page"
        assert content["metadata"]["language"] == "en"

    @patch("selenium.webdriver.Chrome")
    def test_extract_content_failure(self, mock_chrome, mock_driver):
        """Test content extraction failure"""
        mock_chrome.return_value = mock_driver
        mock_driver.execute_script.side_effect = Exception(
            "JavaScript execution failed"
        )

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))
        scraper.driver = mock_driver

        content = scraper.extract_content()

        assert content is None

    @patch("selenium.webdriver.Chrome")
    def test_get_main_content_with_selectors(self, mock_chrome, mock_driver):
        """Test main content extraction with different selectors"""
        mock_chrome.return_value = mock_driver

        # Mock first selector (main) to fail, second (article) to succeed
        mock_driver.find_element.side_effect = [
            Exception("main not found"),  # main selector fails
            Mock(
                get_attribute=lambda attr: "<article>Main content</article>"
            ),  # article succeeds
        ]

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))
        scraper.driver = mock_driver

        content = scraper.get_main_content()

        assert "Main content" in content
        assert mock_driver.find_element.call_count >= 2

    @patch("selenium.webdriver.Chrome")
    def test_html_to_markdowntex_conversion(self, mock_chrome, mock_driver):
        """Test HTML to MarkdownTeX conversion"""
        mock_chrome.return_value = mock_driver

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))

        html_content = """
        <html>
        <body>
            <h1>Main Title</h1>
            <p>This is <strong>important</strong> and <em>emphasized</em> text.</p>
            <h2>Subsection</h2>
            <ul>
                <li>Item 1</li>
                <li>Item 2</li>
            </ul>
            <blockquote>
                <p>This is a quote</p>
            </blockquote>
        </body>
        </html>
        """

        markdowntex = scraper.html_to_markdowntex(html_content)

        assert "# Main Title" in markdowntex
        assert "**important**" in markdowntex
        assert "*emphasized*" in markdowntex
        assert "## Subsection" in markdowntex
        assert "- Item 1" in markdowntex
        assert "- Item 2" in markdowntex

    @patch("selenium.webdriver.Chrome")
    def test_convert_list_unordered(self, mock_chrome, mock_driver):
        """Test unordered list conversion"""
        mock_chrome.return_value = mock_driver

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))

        list_html = "<ul><li>Item 1</li><li>Item 2</li></ul>"
        result = scraper.convert_list(list_html, "ul")

        assert "- Item 1" in result
        assert "- Item 2" in result

    @patch("selenium.webdriver.Chrome")
    def test_convert_list_ordered(self, mock_chrome, mock_driver):
        """Test ordered list conversion"""
        mock_chrome.return_value = mock_driver

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))

        list_html = "<ol><li>First</li><li>Second</li></ol>"
        result = scraper.convert_list(list_html, "ol")

        assert "1. First" in result
        assert "2. Second" in result

    @patch("selenium.webdriver.Chrome")
    def test_save_page_success(self, mock_chrome, mock_driver):
        """Test successful page saving"""
        mock_chrome.return_value = mock_driver

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))

        content = {
            "metadata": {
                "title": "Test Page",
                "language": "en",
                "scraped_at": "2024-01-01 12:00:00",
                "content_length": 100,
            },
            "content": "# Test Page\n\nTest content here.",
        }

        result = scraper.save_page("https://example.com/test", content)

        assert result is True
        assert "https://example.com/test" in scraper.downloaded_urls

        # Check if files were created
        md_file = self.output_dir / "pages" / "test.md"
        meta_file = self.output_dir / "meta" / "test.md.json"

        assert md_file.exists()
        assert meta_file.exists()

        # Check content
        md_content = md_file.read_text(encoding="utf-8")
        assert "# Test Page" in md_content
        assert "Test content here." in md_content

    @patch("selenium.webdriver.Chrome")
    def test_save_page_failure(self, mock_chrome, mock_driver):
        """Test page saving failure"""
        mock_chrome.return_value = mock_driver

        from scripts.web_scraper import WebScraper

        # Use invalid path to trigger error
        scraper = WebScraper(self.base_url, "/invalid/path/that/does/not/exist")

        content = {"metadata": {"title": "Test Page"}, "content": "# Test Page"}

        result = scraper.save_page("https://example.com/test", content)

        assert result is False
        assert "https://example.com/test" not in scraper.downloaded_urls

    @patch("selenium.webdriver.Chrome")
    @patch("time.sleep")
    def test_scrape_page_success(self, mock_sleep, mock_chrome, mock_driver):
        """Test successful page scraping"""
        mock_chrome.return_value = mock_driver

        # Mock successful page load and content extraction
        mock_driver.execute_script.return_value = {
            "title": "Test Page",
            "description": "",
            "keywords": "",
            "language": "en",
            "author": "",
            "canonical": "",
            "structure": {
                "headings": [],
                "lists": 0,
                "tables": 0,
                "images": 0,
                "links": 0,
            },
        }

        mock_driver.page_source = "<html><body><h1>Test</h1></body></html>"
        mock_driver.find_element.return_value = Mock(
            get_attribute=lambda attr: "<main>Content</main>"
        )

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))
        scraper.driver = mock_driver
        scraper.wait_for_page_load = Mock(return_value=True)
        scraper.extract_content = Mock(
            return_value={
                "metadata": {"title": "Test Page", "language": "en"},
                "content": "# Test Page\n\nContent",
                "raw_html": "<html>...</html>",
            }
        )
        scraper.save_page = Mock(return_value=True)
        scraper.extract_links = Mock(return_value=["https://example.com/page2"])

        result = scraper.scrape_page("https://example.com/test")

        assert result == ["https://example.com/page2"]
        assert scraper.driver.get.called
        scraper.wait_for_page_load.assert_called_once()
        scraper.extract_content.assert_called_once()
        scraper.save_page.assert_called_once()

    @patch("selenium.webdriver.Chrome")
    def test_scrape_page_content_extraction_failure(self, mock_chrome, mock_driver):
        """Test page scraping with content extraction failure"""
        mock_chrome.return_value = mock_driver

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))
        scraper.driver = mock_driver
        scraper.wait_for_page_load = Mock(return_value=True)
        scraper.extract_content = Mock(return_value=None)

        result = scraper.scrape_page("https://example.com/test")

        assert result == []
        assert "https://example.com/test" in scraper.failed_urls
        assert "https://example.com/test" not in scraper.downloaded_urls

    @patch("selenium.webdriver.Chrome")
    def test_scrape_page_exception(self, mock_chrome, mock_driver):
        """Test page scraping with exception"""
        mock_chrome.return_value = mock_driver
        mock_driver.get.side_effect = WebDriverException("Navigation failed")

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))
        scraper.driver = mock_driver

        result = scraper.scrape_page("https://example.com/test")

        assert result == []
        assert "https://example.com/test" in scraper.failed_urls

    @patch("selenium.webdriver.Chrome")
    @patch("time.sleep")
    @patch("json.dump")
    def test_recursive_scrape_success(
        self, mock_json_dump, mock_sleep, mock_chrome, mock_driver
    ):
        """Test successful recursive scraping"""
        mock_chrome.return_value = mock_driver

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))
        scraper.setup_driver = Mock(return_value=True)
        scraper.scrape_page = Mock(
            side_effect=[
                [
                    "https://example.com/page2",
                    "https://example.com/page3",
                ],  # First page
                ["https://example.com/page4"],  # Second page
                [],  # Third page
                [],  # Fourth page
            ]
        )

        with patch("builtins.open", create=True) as mock_open:
            mock_open.return_value.__enter__.return_value = Mock()

            scraper.recursive_scrape(max_pages=3, max_depth=2)

            # Should have scraped base URL + 2 more pages
            assert scraper.driver.get.call_count >= 3
            assert len(scraper.downloaded_urls) == 3

    @patch("selenium.webdriver.Chrome")
    def test_recursive_scrape_setup_failure(self, mock_chrome):
        """Test recursive scraping with driver setup failure"""
        mock_chrome.return_value = Mock()

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))
        scraper.setup_driver = Mock(return_value=False)

        scraper.recursive_scrape(max_pages=1, max_depth=1)

        # Should not attempt any scraping if driver setup fails
        assert len(scraper.downloaded_urls) == 0
        assert len(scraper.failed_urls) == 0

    def test_generate_sitemap(self):
        """Test sitemap generation"""
        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))
        scraper.downloaded_urls = [
            "https://example.com/",
            "https://example.com/about",
            "https://example.com/contact",
        ]

        scraper.generate_sitemap()

        sitemap_file = self.output_dir / "sitemap.txt"
        assert sitemap_file.exists()

        sitemap_content = sitemap_file.read_text(encoding="utf-8")
        for url in scraper.downloaded_urls:
            assert url in sitemap_content

    def test_teardown(self):
        """Test proper cleanup"""
        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))
        scraper.driver = Mock()

        # Call recursive_scrape to trigger cleanup
        with patch.object(scraper, "setup_driver", return_value=True):
            with patch.object(scraper, "scrape_page", return_value=[]):
                with patch("builtins.open", create=True):
                    scraper.recursive_scrape(max_pages=0, max_depth=0)

        scraper.driver.quit.assert_called_once()

    @patch("selenium.webdriver.Chrome")
    def test_edge_cases(self, mock_chrome, mock_driver):
        """Test edge cases and boundary conditions"""
        mock_chrome.return_value = mock_driver

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))

        # Test with empty HTML
        result = scraper.html_to_markdowntex("")
        assert result == "\n"

        # Test with None content
        result = scraper.extract_content()
        assert result is None

        # Test with malformed URL
        result = scraper.is_same_domain("not-a-url")
        assert result is False

        # Test with empty page links
        mock_driver.find_elements.return_value = []
        links = scraper.extract_links()
        assert links == []

    @patch("selenium.webdriver.Chrome")
    def test_performance_monitoring(self, mock_chrome, mock_driver):
        """Test performance monitoring aspects"""
        mock_chrome.return_value = mock_driver

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))

        # Test delay is respected
        with patch("time.sleep") as mock_sleep:
            scraper.delay = 2.0
            scraper.driver = mock_driver
            scraper.wait_for_page_load = Mock(return_value=True)
            scraper.extract_content = Mock(return_value=None)

            scraper.scrape_page("https://example.com/test")

            mock_sleep.assert_called_with(2.0)

    @patch("selenium.webdriver.Chrome")
    def test_error_recovery(self, mock_chrome, mock_driver):
        """Test error recovery mechanisms"""
        mock_chrome.return_value = mock_driver

        from scripts.web_scraper import WebScraper

        scraper = WebScraper(self.base_url, str(self.output_dir))
        scraper.driver = mock_driver

        # Test main content fallback
        mock_driver.find_element.side_effect = Exception("Element not found")
        mock_driver.page_source = "<html><body>Fallback content</body></html>"

        content = scraper.get_main_content()
        assert "Fallback content" in content
