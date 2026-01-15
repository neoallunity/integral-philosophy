#!/usr/bin/env python3
"""
Comprehensive unit tests for TEI Generator component
Tests academic XML generation, TEI compliance, and document structure
"""

import pytest
import json
import tempfile
from pathlib import Path
from unittest.mock import Mock, MagicMock, patch
import xml.etree.ElementTree as ET
from datetime import datetime

from tests.utils.base_test_classes import ComponentTestCase


class TestTEIGenerator(ComponentTestCase):
    """Test cases for TEIGenerator class"""

    def setup_method(self):
        """Setup test environment"""
        super().setup_method()

    @pytest.fixture
    def sample_site_metadata(self):
        """Sample site metadata for testing"""
        return {
            "base_url": "https://example.com",
            "domain": "example.com",
            "scraped_at": "2024-01-01 12:00:00",
            "total_pages": 2,
            "failed_pages": 0,
            "title": "Example Site",
            "description": "Example site for testing",
            "language": "en",
            "author": "Test Author",
        }

    @pytest.fixture
    def sample_pages(self):
        """Sample pages data for testing"""
        return {
            "https://example.com/": {
                "title": "Home Page",
                "metadata": {
                    "title": "Home Page",
                    "description": "Home page description",
                    "language": "en",
                    "author": "Test Author",
                    "scraped_at": "2024-01-01 12:00:00",
                    "keywords": "home, welcome",
                },
                "content": "# Welcome\\n\\nThis is the home page content.",
                "links": ["https://example.com/about"],
            },
            "https://example.com/about": {
                "title": "About Us",
                "metadata": {
                    "title": "About Us",
                    "description": "About page description",
                    "language": "en",
                    "author": "Test Author",
                    "scraped_at": "2024-01-01 12:05:00",
                    "keywords": "about, company",
                },
                "content": "# About\\n\\nInformation about our company.",
                "links": [],
            },
        }

    @pytest.fixture
    def sample_site_ast(self, sample_site_metadata, sample_pages):
        """Sample complete site AST"""
        return {
            "metadata": sample_site_metadata,
            "pages": sample_pages,
            "links": {
                "https://example.com/": ["https://example.com/about"],
                "https://example.com/about": [],
            },
        }

    def test_generator_initialization(self):
        """Test TEIGenerator initialization"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        assert generator.tei_ns == "http://www.tei-c.org/ns/1.0"
        assert generator.xml_ns == "http://www.w3.org/XML/1998/namespace"
        assert "tei" in generator.ns_map
        assert "xml" in generator.ns_map

    def test_generate_tei_header_basic(self, sample_site_metadata):
        """Test basic TEI header generation"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()
        tei_element = generator.generate_tei_header(sample_site_metadata)

        assert tei_element.tag.endswith("TEI")
        assert tei_element.get(f"{{{generator.xml_ns}}}lang") == "en"

        # Check for required TEI elements
        tei_header = tei_element.find(f".//{{{generator.tei_ns}}}teiHeader")
        assert tei_header is not None

        file_desc = tei_header.find(f".//{{{generator.tei_ns}}}fileDesc")
        assert file_desc is not None

        title_stmt = file_desc.find(f".//{{{generator.tei_ns}}}titleStmt")
        assert title_stmt is not None

        title = title_stmt.find(f".//{{{generator.tei_ns}}}title")
        assert title is not None
        assert title.text == "Example Site"

    def test_generate_tei_header_missing_metadata(self):
        """Test TEI header generation with minimal metadata"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()
        minimal_metadata = {}

        tei_element = generator.generate_tei_header(minimal_metadata)

        # Should still create valid TEI structure
        assert tei_element.tag.endswith("TEI")

        title = tei_element.find(f".//{{{generator.tei_ns}}}title")
        assert title is not None
        assert title.text == "Untitled Site" or "base_url" in title.text

    def test_generate_tei_header_with_custom_language(self):
        """Test TEI header with custom language"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()
        metadata = {"language": "fr", "title": "Site Français"}

        tei_element = generator.generate_tei_header(metadata)

        assert tei_element.get(f"{{{generator.xml_ns}}}lang") == "fr"

        title = tei_element.find(f".//{{{generator.tei_ns}}}title")
        assert title.text == "Site Français"

        language_element = tei_element.find(f".//{{{generator.tei_ns}}}language")
        assert language_element.get("ident") == "fr"

    def test_generate_text_body(self, sample_pages):
        """Test TEI text body generation"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()
        body_element = generator.generate_text_body(sample_pages)

        assert body_element.tag.endswith("text")

        # Check for front matter with table of contents
        front = body_element.find(f".//{{{generator.tei_ns}}}front")
        assert front is not None

        toc_div = front.find(f".//{{{generator.tei_ns}}}div[@type='contents']")
        assert toc_div is not None

        toc_head = toc_div.find(f".//{{{generator.tei_ns}}}head")
        assert toc_head.text == "Table of Contents"

        # Check for main body with page content
        main_body = body_element.find(f".//{{{generator.tei_ns}}}body")
        assert main_body is not None

        page_divs = main_body.findall(f".//{{{generator.tei_ns}}}div[@type='page']")
        assert len(page_divs) == 2  # Should have 2 pages

    def test_generate_text_body_empty(self):
        """Test TEI text body generation with no pages"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()
        body_element = generator.generate_text_body({})

        assert body_element.tag.endswith("text")

        # Should still have front and body elements
        front = body_element.find(f".//{{{generator.tei_ns}}}front")
        body = body_element.find(f".//{{{generator.tei_ns}}}body")

        assert front is not None
        assert body is not None

    def test_convert_page_to_tei_div(self):
        """Test converting single page to TEI div"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        page_info = {
            "title": "Test Page",
            "metadata": {
                "title": "Test Page",
                "description": "Test description",
                "language": "en",
                "author": "Test Author",
                "scraped_at": "2024-01-01 12:00:00",
                "keywords": "test, page",
            },
            "content": "# Test Heading\\n\\nTest paragraph content.",
            "links": ["https://example.com/other"],
        }

        div_element = generator._convert_page_to_tei_div(
            "https://example.com/test", page_info
        )

        assert div_element.tag.endswith("div")
        assert div_element.get("type") == "page"
        assert div_element.get(f"{{{generator.xml_ns}}}id") is not None

        # Check for page header
        head = div_element.find(f".//{{{generator.tei_ns}}}head")
        assert head.text == "Test Page"

        # Check for metadata div
        meta_div = div_element.find(f".//{{{generator.tei_ns}}}div[@type='metadata']")
        assert meta_div is not None

        # Check for content
        content_div = div_element.find(
            f".//{{{generator.tei_ns}}}div[@type='document']"
        )
        # Content div will be created if content exists

        # Check for links
        links_div = div_element.find(f".//{{{generator.tei_ns}}}div[@type='links']")
        assert links_div is not None

    def test_convert_page_to_tei_div_minimal(self):
        """Test converting page with minimal information"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        page_info = {"title": "Minimal Page"}

        div_element = generator._convert_page_to_tei_div(
            "https://example.com/minimal", page_info
        )

        assert div_element.tag.endswith("div")
        assert div_element.get("type") == "page"

        head = div_element.find(f".//{{{generator.tei_ns}}}head")
        assert head.text == "Minimal Page"

    def test_convert_ast_to_tei_document_node(self):
        """Test converting AST document node to TEI"""
        from scripts.tei_generator import TEIGenerator
        from scripts.markdowntex_parser import NodeType, ASTNode

        generator = TEIGenerator()

        # Create document AST node
        doc_node = {
            "type": "document",
            "content": [{"type": "paragraph", "content": "This is a paragraph."}],
        }

        tei_element = generator._convert_ast_to_tei(doc_node)

        assert tei_element is not None
        assert tei_element.tag.endswith("div")
        assert tei_element.get("type") == "document"

    def test_convert_ast_to_tei_heading_node(self):
        """Test converting AST heading node to TEI"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        heading_node = {
            "type": "heading",
            "attributes": {"level": 2},
            "content": "Section Title",
        }

        tei_element = generator._convert_ast_to_tei(heading_node)

        assert tei_element is not None
        assert tei_element.tag.endswith("head")
        assert tei_element.get("type") == "heading-2"
        assert tei_element.text == "Section Title"

    def test_convert_ast_to_tei_paragraph_node(self):
        """Test converting AST paragraph node to TEI"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        paragraph_node = {
            "type": "paragraph",
            "content": "This is a paragraph with text.",
        }

        tei_element = generator._convert_ast_to_tei(paragraph_node)

        assert tei_element is not None
        assert tei_element.tag.endswith("p")
        assert tei_element.text == "This is a paragraph with text."

    def test_convert_ast_to_tei_link_node(self):
        """Test converting AST link node to TEI"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        link_node = {
            "type": "link",
            "attributes": {"href": "https://example.com"},
            "content": "Example Link",
        }

        tei_element = generator._convert_ast_to_tei(link_node)

        assert tei_element is not None
        assert tei_element.tag.endswith("ref")
        assert tei_element.get("target") == "https://example.com"
        assert tei_element.text == "Example Link"

    def test_convert_ast_to_tei_image_node(self):
        """Test converting AST image node to TEI"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        image_node = {
            "type": "image",
            "attributes": {"src": "image.jpg", "alt": "Example image"},
        }

        tei_element = generator._convert_ast_to_tei(image_node)

        assert tei_element is not None
        assert tei_element.tag.endswith("figure")

        graphic = tei_element.find(f".//{{{generator.tei_ns}}}graphic")
        assert graphic is not None
        assert graphic.get("url") == "image.jpg"

        fig_desc = tei_element.find(f".//{{{generator.tei_ns}}}figDesc")
        assert fig_desc is not None
        assert fig_desc.text == "Example image"

    def test_convert_ast_to_tei_code_block_node(self):
        """Test converting AST code block node to TEI"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        code_node = {
            "type": "code_block",
            "attributes": {"language": "python"},
            "content": "def hello():\\n    print('Hello')",
        }

        tei_element = generator._convert_ast_to_tei(code_node)

        assert tei_element is not None
        assert tei_element.tag.endswith("quote")
        assert tei_element.get("type") == "code"
        assert tei_element.get(f"{{{generator.xml_ns}}}lang") == "python"
        assert "def hello" in tei_element.text

    def test_convert_ast_to_tei_math_node(self):
        """Test converting AST math node to TEI"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        math_node = {
            "type": "math_block",
            "attributes": {"format": "latex"},
            "content": "E = mc^2",
        }

        tei_element = generator._convert_ast_to_tei(math_node)

        assert tei_element is not None
        assert tei_element.tag.endswith("formula")
        assert tei_element.get("notation") == "latex"
        assert tei_element.text == "E = mc^2"

    def test_convert_ast_to_tei_list_node(self):
        """Test converting AST list node to TEI"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        list_node = {
            "type": "list",
            "attributes": {"ordered": False},
            "content": [
                {"type": "list_item", "content": "Item 1"},
                {"type": "list_item", "content": "Item 2"},
            ],
        }

        tei_element = generator._convert_ast_to_tei(list_node)

        assert tei_element is not None
        assert tei_element.tag.endswith("list")
        assert tei_element.get("type") == "bulleted"

        items = tei_element.findall(f".//{{{generator.tei_ns}}}item")
        assert len(items) == 2

    def test_convert_ast_to_tei_quote_node(self):
        """Test converting AST quote node to TEI"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        quote_node = {"type": "quote", "content": "This is a blockquote."}

        tei_element = generator._convert_ast_to_tei(quote_node)

        assert tei_element is not None
        assert tei_element.tag.endswith("quote")
        assert tei_element.text == "This is a blockquote."

    def test_convert_ast_to_tei_emphasis_nodes(self):
        """Test converting AST emphasis nodes to TEI"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        # Test strong/bold
        strong_node = {"type": "strong", "content": "Bold text"}

        strong_element = generator._convert_ast_to_tei(strong_node)
        assert strong_element is not None
        assert strong_element.tag.endswith("hi")
        assert strong_element.get("rend") == "bold"
        assert strong_element.text == "Bold text"

        # Test emphasis/italic
        emphasis_node = {"type": "emphasis", "content": "Italic text"}

        emphasis_element = generator._convert_ast_to_tei(emphasis_node)
        assert emphasis_element is not None
        assert emphasis_element.tag.endswith("hi")
        assert emphasis_element.get("rend") == "it"
        assert emphasis_element.text == "Italic text"

    def test_convert_ast_to_tei_unsupported_node(self):
        """Test converting unsupported AST node to TEI"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        unsupported_node = {"type": "unsupported_type", "content": "Some content"}

        tei_element = generator._convert_ast_to_tei(unsupported_node)

        assert tei_element is None

    def test_create_page_id(self):
        """Test page ID creation from URLs"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        # Test normal URL
        page_id = generator._create_page_id("https://example.com/about")
        assert page_id.startswith("https___example_com_about")
        assert len(page_id) <= 50

        # Test URL with special characters
        page_id = generator._create_page_id(
            "https://example.com/page-with-special&chars.html"
        )
        assert "_" in page_id
        assert "&" not in page_id

        # Test empty URL
        page_id = generator._create_page_id("")
        assert page_id.startswith("page_")
        assert len(page_id) > 8  # Should have UUID suffix

    def test_generate_tei_document_complete(self, sample_site_ast):
        """Test complete TEI document generation"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        # Mock the parser since we're testing TEI generation
        with patch("scripts.tei_generator.MarkdownTeXParser") as mock_parser_class:
            mock_parser = Mock()
            mock_parser.parse.return_value = {
                "type": "document",
                "content": [{"type": "paragraph", "content": "Parsed content"}],
            }
            mock_parser_class.return_value = mock_parser

            tei_xml = generator.generate_tei_document(sample_site_ast)

            assert tei_xml is not None
            assert '<?xml version="1.0" encoding="UTF-8"?>' in tei_xml
            assert "<!DOCTYPE TEI" in tei_xml
            assert "<TEI" in tei_xml
            assert 'xmlns="http://www.tei-c.org/ns/1.0"' in tei_xml
            assert "<teiHeader>" in tei_xml
            assert "<text>" in tei_xml
            assert "<body>" in tei_xml

    def test_generate_tei_document_minimal(self):
        """Test TEI document generation with minimal data"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        minimal_ast = {"metadata": {}, "pages": {}, "links": {}}

        with patch("scripts.tei_generator.MarkdownTeXParser"):
            tei_xml = generator.generate_tei_document(minimal_ast)

            assert tei_xml is not None
            assert '<?xml version="1.0"' in tei_xml
            assert "<TEI" in tei_xml

    def test_save_tei_document(self):
        """Test saving TEI document to file"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        tei_content = '<?xml version="1.0"?><TEI><body>Test content</body></TEI>'
        output_file = self.temp_dir / "test_tei.xml"

        generator.save_tei_document(tei_content, output_file)

        assert output_file.exists()
        saved_content = output_file.read_text(encoding="utf-8")
        assert saved_content == tei_content

    def test_save_tei_document_creates_directories(self):
        """Test that saving TEI document creates necessary directories"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        tei_content = '<?xml version="1.0"?><TEI><body>Test</body></TEI>'
        nested_dir = self.temp_dir / "nested" / "directories" / "path"
        output_file = nested_dir / "document.xml"

        generator.save_tei_document(tei_content, output_file)

        assert output_file.exists()
        assert nested_dir.exists()

    def test_generate_tei_document_with_revision_history(self, sample_site_ast):
        """Test TEI document generation includes revision history"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        with patch("scripts.tei_generator.MarkdownTeXParser"):
            tei_xml = generator.generate_tei_document(sample_site_ast)

            # Check for revision description
            assert "<revisionDesc>" in tei_xml
            assert "<change" in tei_xml
            assert 'who="#web_scraper"' in tei_xml
            assert "Initial TEI XML generation" in tei_xml

    def test_generate_tei_document_with_standoff_markup(self, sample_site_ast):
        """Test TEI document generation includes standoff markup"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        with patch("scripts.tei_generator.MarkdownTeXParser"):
            tei_xml = generator.generate_tei_document(sample_site_ast)

            # Check for standoff markup
            assert "<standOff>" in tei_xml
            assert '<listBibl type="links">' in tei_xml
            assert "<bibl>" in tei_xml

    def test_tei_xml_validity(self, sample_site_ast):
        """Test that generated TEI XML is well-formed"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        with patch("scripts.tei_generator.MarkdownTeXParser"):
            tei_xml = generator.generate_tei_document(sample_site_ast)

            # Try to parse the XML
            try:
                root = ET.fromstring(tei_xml)
                assert root.tag.endswith("TEI")

                # Check for required TEI elements
                tei_header = root.find(f".//{{{generator.tei_ns}}}teiHeader")
                assert tei_header is not None

                text = root.find(f".//{{{generator.tei_ns}}}text")
                assert text is not None

            except ET.ParseError as e:
                pytest.fail(f"Generated TEI XML is not well-formed: {e}")

    def test_tei_xml_namespace_handling(self):
        """Test proper TEI XML namespace handling"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        simple_metadata = {"title": "Test Document", "language": "en"}

        tei_element = generator.generate_tei_header(simple_metadata)

        # Check that namespaces are properly set
        assert tei_element.tag == f"{{{generator.tei_ns}}}TEI"

        # Check that children inherit namespace
        title = tei_element.find(f".//{{{generator.tei_ns}}}title")
        assert title is not None
        assert title.tag == f"{{{generator.tei_ns}}}title"

    def test_tei_with_unicode_content(self):
        """Test TEI generation with unicode content"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        pages_with_unicode = {
            "https://example.com/unicode": {
                "title": "Unicode Test 🌟",
                "metadata": {
                    "title": "Unicode Test 🌟",
                    "description": "Testing unicode: café, naïve, 中文",
                    "language": "en",
                },
                "content": "# Unicode Test 🌟\\n\\nTesting unicode: café, naïve, 中文",
            }
        }

        body_element = generator.generate_text_body(pages_with_unicode)

        # Should handle unicode properly
        assert "🌟" in str(body_element)
        assert "café" in str(body_element)
        assert "中文" in str(body_element)

    def test_edge_cases(self):
        """Test edge cases and error conditions"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        # Test with None content
        page_info = {"title": "Test", "metadata": {}, "content": None, "links": []}

        div_element = generator._convert_page_to_tei_div(
            "https://example.com/test", page_info
        )
        assert div_element is not None

        # Test with empty content
        empty_ast = {"type": "document", "content": []}
        tei_element = generator._convert_ast_to_tei(empty_ast)
        assert tei_element is not None

        # Test with missing attributes
        heading_no_level = {"type": "heading", "content": "Heading"}

        heading_element = generator._convert_ast_to_tei(heading_no_level)
        assert heading_element is not None
        assert heading_element.get("type") == "heading-1"  # Should default to level 1

    def test_large_document_handling(self):
        """Test handling of large documents"""
        from scripts.tei_generator import TEIGenerator

        generator = TEIGenerator()

        # Create a large pages dataset
        large_pages = {}
        for i in range(100):
            url = f"https://example.com/page{i}"
            large_pages[url] = {
                "title": f"Page {i}",
                "metadata": {"title": f"Page {i}", "language": "en"},
                "content": f"# Page {i}\\n\\nContent for page {i}.",
            }

        body_element = generator.generate_text_body(large_pages)

        # Should handle large number of pages
        page_divs = body_element.findall(f".//{{{generator.tei_ns}}}div[@type='page']")
        assert len(page_divs) == 100
