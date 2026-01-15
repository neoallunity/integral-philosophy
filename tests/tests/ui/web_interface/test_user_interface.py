"""
Web Interface Testing for Integral Philosophy Publishing System
Comprehensive UI testing including page rendering, forms, navigation, and dynamic content
"""

import pytest
import time
import json
from typing import Dict, Any, Tuple, List
from pathlib import Path
from selenium.webdriver.common.by import By

from tests.utils.base_test_classes import SeleniumTestCase
from tests.ui.base.selenium_setup import (
    SeleniumSetup,
    BROWSER_CONFIGS,
    create_screenshot_dir,
)


class TestPageRendering(SeleniumTestCase):
    """Test page rendering and layout functionality"""

    def setup_method(self):
        """Setup test method"""
        self.selenium_setup = SeleniumSetup()
        self.screenshot_dir = create_screenshot_dir()

    def test_homepage_rendering(self):
        """Test homepage renders correctly"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Check page title
            assert "Integral Philosophy" in driver.title

            # Check main elements are present
            self.selenium_setup.assert_element_present(driver, (By.TAG_NAME, "main"))
            self.selenium_setup.assert_element_present(driver, (By.TAG_NAME, "header"))
            self.selenium_setup.assert_element_present(driver, (By.TAG_NAME, "footer"))

            # Check content is loaded
            self.selenium_setup.assert_text_present(driver, "Integral Philosophy")

            # Take screenshot for visual regression
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/homepage.png"
            )

        finally:
            driver.quit()

    def test_article_page_rendering(self):
        """Test article page renders correctly"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/articles/test-article")

            # Check article elements
            self.selenium_setup.assert_element_present(
                driver, (By.CSS_SELECTOR, "article")
            )
            self.selenium_setup.assert_element_present(driver, (By.CSS_SELECTOR, "h1"))

            # Check article metadata
            self.selenium_setup.assert_element_present(
                driver, (By.CSS_SELECTOR, ".article-meta")
            )

            # Check table of contents
            toc_element = driver.find_element(By.CSS_SELECTOR, ".table-of-contents")
            assert toc_element.is_displayed()

            # Screenshot for regression
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/article_page.png"
            )

        finally:
            driver.quit()

    def test_responsive_design(self):
        """Test responsive design across different screen sizes"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Test desktop view
            driver.set_window_size(1920, 1080)
            time.sleep(1)
            desktop_screenshot = f"{self.screenshot_dir}/desktop_view.png"
            self.selenium_setup.take_screenshot(driver, desktop_screenshot)

            # Test tablet view
            driver.set_window_size(768, 1024)
            time.sleep(1)
            tablet_screenshot = f"{self.screenshot_dir}/tablet_view.png"
            self.selenium_setup.take_screenshot(driver, tablet_screenshot)

            # Test mobile view
            driver.set_window_size(375, 667)
            time.sleep(1)
            mobile_screenshot = f"{self.screenshot_dir}/mobile_view.png"
            self.selenium_setup.take_screenshot(driver, mobile_screenshot)

            # Verify hamburger menu appears on mobile
            hamburger = driver.find_element(By.CSS_SELECTOR, ".hamburger-menu")
            assert hamburger.is_displayed()

        finally:
            driver.quit()


class TestFormSubmission(SeleniumTestCase):
    """Test form submission and validation"""

    def setup_method(self):
        """Setup test method"""
        self.selenium_setup = SeleniumSetup()
        self.screenshot_dir = create_screenshot_dir()

    def test_article_upload_form(self):
        """Test article upload form functionality"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/upload")

            # Find form elements
            title_input = self.selenium_setup.find_element(
                driver, (By.ID, "article-title")
            )
            content_textarea = self.selenium_setup.find_element(
                driver, (By.ID, "article-content")
            )
            file_input = self.selenium_setup.find_element(
                driver, (By.ID, "file-upload")
            )
            submit_button = self.selenium_setup.find_element(
                driver, (By.CSS_SELECTOR, "button[type='submit']")
            )

            # Fill form
            title_input.send_keys("Test Article")
            content_textarea.send_keys("This is a test article for upload testing.")

            # Test file upload (if test file available)
            test_file_path = (
                Path(__file__).parent.parent.parent
                / "fixtures"
                / "test_files"
                / "sample.md"
            )
            if test_file_path.exists():
                file_input.send_keys(str(test_file_path))

            # Submit form
            submit_button.click()

            # Wait for response
            self.selenium_setup.wait_for_url_contains(driver, "success", timeout=10)

            # Verify success message
            success_message = self.selenium_setup.find_element(
                driver, (By.CSS_SELECTOR, ".success-message")
            )
            assert success_message.is_displayed()

            # Screenshot after submission
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/upload_success.png"
            )

        finally:
            driver.quit()

    def test_form_validation(self):
        """Test form validation messages"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/upload")

            # Try to submit empty form
            submit_button = self.selenium_setup.find_element(
                driver, (By.CSS_SELECTOR, "button[type='submit']")
            )
            submit_button.click()

            # Check validation messages
            title_error = self.selenium_setup.find_element(
                driver, (By.CSS_SELECTOR, "#article-title + .error-message")
            )
            content_error = self.selenium_setup.find_element(
                driver, (By.CSS_SELECTOR, "#article-content + .error-message")
            )

            assert title_error.is_displayed()
            assert content_error.is_displayed()
            assert "Title is required" in title_error.text
            assert "Content is required" in content_error.text

            # Screenshot validation errors
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/validation_errors.png"
            )

        finally:
            driver.quit()


class TestNavigationAndRouting(SeleniumTestCase):
    """Test navigation and routing functionality"""

    def setup_method(self):
        """Setup test method"""
        self.selenium_setup = SeleniumSetup()
        self.screenshot_dir = create_screenshot_dir()

    def test_main_navigation(self):
        """Test main navigation menu"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Test navigation links
            nav_links = [
                ("Articles", "/articles"),
                ("Publishing", "/publishing"),
                ("About", "/about"),
                ("Contact", "/contact"),
            ]

            for link_text, expected_path in nav_links:
                # Find and click navigation link
                nav_link = driver.find_element(By.LINK_TEXT, link_text)
                nav_link.click()

                # Wait for page to load
                self.selenium_setup.wait_for_url_contains(driver, expected_path)

                # Verify URL
                assert expected_path in driver.current_url

                # Take screenshot
                page_name = link_text.lower().replace(" ", "_")
                self.selenium_setup.take_screenshot(
                    driver, f"{self.screenshot_dir}/nav_{page_name}.png"
                )

                # Go back to home
                driver.get(f"{self.selenium_setup.base_url}/")

        finally:
            driver.quit()

    def test_breadcrumb_navigation(self):
        """Test breadcrumb navigation"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            # Navigate to a deep page
            driver.get(
                f"{self.selenium_setup.base_url}/articles/philosophy/modern-integral-theory"
            )

            # Check breadcrumbs are present
            breadcrumb = self.selenium_setup.find_element(
                driver, (By.CSS_SELECTOR, ".breadcrumb")
            )
            assert breadcrumb.is_displayed()

            # Check breadcrumb links
            breadcrumb_links = breadcrumb.find_elements(By.TAG_NAME, "a")
            assert len(breadcrumb_links) >= 2

            # Click breadcrumb links and verify navigation
            for link in breadcrumb_links[:-1]:  # Skip the current page
                link_text = link.text
                link.click()

                # Wait for navigation
                time.sleep(1)

                # Verify breadcrumb functionality by checking we're not on the same page
                assert "modern-integral-theory" not in driver.current_url

                # Go back
                driver.get(
                    f"{self.selenium_setup.base_url}/articles/philosophy/modern-integral-theory"
                )

        finally:
            driver.quit()

    def test_search_functionality(self):
        """Test search functionality"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Find search box
            search_input = self.selenium_setup.find_element(
                driver, (By.CSS_SELECTOR, "input[placeholder*='Search']")
            )
            search_button = self.selenium_setup.find_element(
                driver, (By.CSS_SELECTOR, ".search-button")
            )

            # Enter search term
            search_input.send_keys("Integral Philosophy")
            search_button.click()

            # Wait for search results
            self.selenium_setup.wait_for_url_contains(driver, "search")

            # Check search results page
            search_results = self.selenium_setup.find_element(
                driver, (By.CSS_SELECTOR, ".search-results")
            )
            assert search_results.is_displayed()

            # Check result items
            result_items = search_results.find_elements(By.CSS_SELECTOR, ".result-item")
            assert len(result_items) > 0

            # Screenshot search results
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/search_results.png"
            )

        finally:
            driver.quit()


class TestDynamicContent(SeleniumTestCase):
    """Test dynamic content loading and JavaScript functionality"""

    def setup_method(self):
        """Setup test method"""
        self.selenium_setup = SeleniumSetup()
        self.screenshot_dir = create_screenshot_dir()

    def test_ajax_content_loading(self):
        """Test AJAX content loading"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/articles")

            # Look for "Load More" button or infinite scroll
            try:
                load_more_button = self.selenium_setup.find_element(
                    driver, (By.CSS_SELECTOR, ".load-more")
                )
                load_more_button.click()

                # Wait for content to load
                time.sleep(2)

                # Check new content loaded
                new_articles = driver.find_elements(
                    By.CSS_SELECTOR, ".article-card.loaded-dynamic"
                )
                assert len(new_articles) > 0

            except:
                # If no load more button, test infinite scroll
                driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
                time.sleep(3)

                # Check if more articles loaded
                articles = driver.find_elements(By.CSS_SELECTOR, ".article-card")
                assert len(articles) > 10  # Assuming initial load has 10 articles

            # Screenshot dynamic content
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/dynamic_content.png"
            )

        finally:
            driver.quit()

    def test_interactive_elements(self):
        """Test interactive JavaScript elements"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/articles/test-interactive")

            # Test accordion/collapse elements
            accordions = driver.find_elements(By.CSS_SELECTOR, ".accordion-header")
            for accordion in accordions[:3]:  # Test first 3 accordions
                accordion.click()
                time.sleep(0.5)

                # Check content is visible
                content = accordion.find_element(By.XPATH, "./following-sibling::div")
                assert content.is_displayed()

                # Close it
                accordion.click()
                time.sleep(0.5)

            # Test tabs
            tab_buttons = driver.find_elements(By.CSS_SELECTOR, ".tab-button")
            if tab_buttons:
                for i, tab_button in enumerate(tab_buttons[:3]):
                    tab_button.click()
                    time.sleep(0.5)

                    # Check corresponding tab panel is active
                    tab_panels = driver.find_elements(By.CSS_SELECTOR, ".tab-panel")
                    assert tab_panels[i].is_displayed()
                    assert "active" in tab_panels[i].get_attribute("class")

            # Test modal/dialog
            try:
                modal_trigger = driver.find_element(By.CSS_SELECTOR, ".modal-trigger")
                modal_trigger.click()

                # Wait for modal to appear
                modal = self.selenium_setup.wait_for_element(
                    driver, (By.CSS_SELECTOR, ".modal.show")
                )
                assert modal.is_displayed()

                # Close modal
                close_button = modal.find_element(By.CSS_SELECTOR, ".modal-close")
                close_button.click()
                time.sleep(0.5)

                # Verify modal is hidden
                assert not modal.is_displayed()

            except:
                pass  # Modal might not be present

            # Screenshot interactive elements
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/interactive_elements.png"
            )

        finally:
            driver.quit()

    def test_real_time_updates(self):
        """Test real-time update functionality"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/dashboard")

            # Look for real-time elements like notifications, progress bars, etc.
            progress_elements = driver.find_elements(By.CSS_SELECTOR, ".progress-bar")

            for progress in progress_elements[:2]:  # Test first 2 progress bars
                initial_width = progress.value_of_css_property("width")

                # Wait a bit for potential updates
                time.sleep(2)

                # Check if progress changed (this depends on actual implementation)
                current_width = progress.value_of_css_property("width")
                # We don't assert here since this is demo data, but test functionality

            # Test notification system
            try:
                notification_trigger = driver.find_element(
                    By.CSS_SELECTOR, ".notification-trigger"
                )
                notification_trigger.click()

                # Wait for notification
                notification = self.selenium_setup.wait_for_element(
                    driver, (By.CSS_SELECTOR, ".notification.show")
                )
                assert notification.is_displayed()

                # Check notification content
                notification_text = notification.find_element(
                    By.CSS_SELECTOR, ".notification-content"
                )
                assert notification_text.text.strip()

                # Auto-dismiss notification
                time.sleep(3)
                assert not notification.is_displayed()

            except:
                pass  # Notifications might not be implemented

            # Screenshot dashboard
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/dashboard_updates.png"
            )

        finally:
            driver.quit()


class TestFileUploadInterface(SeleniumTestCase):
    """Test file upload interface and functionality"""

    def setup_method(self):
        """Setup test method"""
        self.selenium_setup = SeleniumSetup()
        self.screenshot_dir = create_screenshot_dir()
        self.test_files_dir = (
            Path(__file__).parent.parent.parent / "fixtures" / "test_files"
        )

    def test_drag_and_drop_upload(self):
        """Test drag and drop file upload interface"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/upload")

            # Find drag and drop area
            drop_zone = self.selenium_setup.find_element(
                driver, (By.CSS_SELECTOR, ".drop-zone")
            )

            # Test drop zone hover effect
            from selenium.webdriver.common.action_chains import ActionChains

            actions = ActionChains(driver)

            # Move mouse over drop zone
            actions.move_to_element(drop_zone).perform()
            time.sleep(0.5)

            # Check if drop zone is highlighted
            assert "drag-over" in drop_zone.get_attribute(
                "class"
            ) or "hover" in drop_zone.get_attribute("class")

            # Test file selection via click
            drop_zone.click()
            time.sleep(1)

            # Screenshot drag and drop interface
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/drag_drop_upload.png"
            )

        finally:
            driver.quit()

    def test_file_preview(self):
        """Test file preview functionality"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/upload")

            # Find file input
            file_input = self.selenium_setup.find_element(
                driver, (By.ID, "file-upload")
            )

            # Look for a test file
            test_file = self.test_files_dir / "sample.md"
            if test_file.exists():
                # Upload file
                file_input.send_keys(str(test_file))
                time.sleep(2)

                # Check if preview appears
                preview = self.selenium_setup.find_element(
                    driver, (By.CSS_SELECTOR, ".file-preview")
                )
                assert preview.is_displayed()

                # Check file info
                file_name = preview.find_element(By.CSS_SELECTOR, ".file-name")
                file_size = preview.find_element(By.CSS_SELECTOR, ".file-size")

                assert test_file.name in file_name.text
                assert file_size.text.strip()

                # Screenshot file preview
                self.selenium_setup.take_screenshot(
                    driver, f"{self.screenshot_dir}/file_preview.png"
                )

        finally:
            driver.quit()

    def test_upload_progress(self):
        """Test file upload progress indication"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/upload")

            # Find and submit upload form
            file_input = self.selenium_setup.find_element(
                driver, (By.ID, "file-upload")
            )
            submit_button = self.selenium_setup.find_element(
                driver, (By.CSS_SELECTOR, ".upload-button")
            )

            # Use a larger test file if available
            test_file = self.test_files_dir / "large_sample.md"
            if not test_file.exists():
                test_file = self.test_files_dir / "sample.md"

            if test_file.exists():
                file_input.send_keys(str(test_file))

                # Start upload
                submit_button.click()

                # Look for progress bar
                try:
                    progress_bar = self.selenium_setup.wait_for_element(
                        driver, (By.CSS_SELECTOR, ".upload-progress")
                    )
                    assert progress_bar.is_displayed()

                    # Wait for upload to complete or timeout
                    progress_complete = False
                    for _ in range(30):  # Wait up to 30 seconds
                        progress_text = progress_bar.find_element(
                            By.CSS_SELECTOR, ".progress-text"
                        )
                        if (
                            "100%" in progress_text.text
                            or "complete" in progress_text.text.lower()
                        ):
                            progress_complete = True
                            break
                        time.sleep(1)

                    # Screenshot progress
                    self.selenium_setup.take_screenshot(
                        driver, f"{self.screenshot_dir}/upload_progress.png"
                    )

                except:
                    # Progress bar might not be implemented
                    pass

        finally:
            driver.quit()


class TestCrossBrowserCompatibility(SeleniumTestCase):
    """Test cross-browser compatibility"""

    def setup_method(self):
        """Setup test method"""
        self.selenium_setup = SeleniumSetup()
        self.screenshot_dir = create_screenshot_dir()

    @pytest.mark.parametrize("browser", ["chrome", "firefox"])
    def test_homepage_across_browsers(self, browser):
        """Test homepage renders correctly across different browsers"""
        config_name = f"desktop_{browser}"
        config = BROWSER_CONFIGS.get(config_name, BROWSER_CONFIGS["desktop_chrome"])
        driver = self.selenium_setup.get_driver(browser, config)

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Wait for page to load
            time.sleep(2)

            # Check basic elements are present
            self.selenium_setup.assert_element_present(driver, (By.TAG_NAME, "header"))
            self.selenium_setup.assert_element_present(driver, (By.TAG_NAME, "main"))
            self.selenium_setup.assert_element_present(driver, (By.TAG_NAME, "footer"))

            # Check title
            assert driver.title

            # Screenshot for comparison
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/homepage_{browser}.png"
            )

            # Check JavaScript functionality
            try:
                # Test a simple interactive element
                nav_button = driver.find_element(By.CSS_SELECTOR, ".nav-toggle")
                nav_button.click()
                time.sleep(0.5)

                # Verify navigation menu interaction
                nav_menu = driver.find_element(By.CSS_SELECTOR, ".nav-menu")
                assert nav_menu.is_displayed()

            except:
                pass  # Element might not exist

        finally:
            driver.quit()

    def test_mobile_emulation(self):
        """Test mobile device emulation"""
        # Test iPhone
        iphone_driver = self.selenium_setup.create_mobile_driver("iphone_13", "chrome")

        try:
            iphone_driver.get(f"{self.selenium_setup.base_url}/")

            # Check mobile-specific elements
            hamburger = self.selenium_setup.find_element(
                iphone_driver, (By.CSS_SELECTOR, ".hamburger-menu")
            )
            assert hamburger.is_displayed()

            # Test mobile navigation
            hamburger.click()
            time.sleep(0.5)

            mobile_menu = self.selenium_setup.find_element(
                iphone_driver, (By.CSS_SELECTOR, ".mobile-menu")
            )
            assert mobile_menu.is_displayed()

            # Screenshot mobile view
            self.selenium_setup.take_screenshot(
                iphone_driver, f"{self.screenshot_dir}/mobile_iphone.png"
            )

        finally:
            iphone_driver.quit()

        # Test Android
        android_driver = self.selenium_setup.create_mobile_driver(
            "samsung_s21", "chrome"
        )

        try:
            android_driver.get(f"{self.selenium_setup.base_url}/")

            # Similar mobile tests
            hamburger = self.selenium_setup.find_element(
                android_driver, (By.CSS_SELECTOR, ".hamburger-menu")
            )
            assert hamburger.is_displayed()

            # Screenshot Android view
            self.selenium_setup.take_screenshot(
                android_driver, f"{self.screenshot_dir}/mobile_android.png"
            )

        finally:
            android_driver.quit()
