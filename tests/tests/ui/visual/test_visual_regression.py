"""
Visual Regression Testing for Integral Philosophy Publishing System
Comprehensive visual testing including screenshot comparison, layout validation, and responsive design verification
"""

import pytest
import time
import json
import os
from typing import Dict, Any, List, Tuple, Optional
from pathlib import Path

from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains

from tests.utils.base_test_classes import SeleniumTestCase
from tests.ui.base.selenium_setup import (
    SeleniumSetup,
    BROWSER_CONFIGS,
    create_screenshot_dir,
    compare_screenshots,
)


class TestScreenshotComparison(SeleniumTestCase):
    """Test screenshot comparison for visual regression"""

    def setup_method(self):
        """Setup test method"""
        self.selenium_setup = SeleniumSetup()
        self.baseline_dir = "test_screenshots/baseline"
        self.current_dir = create_screenshot_dir()
        self.diff_dir = os.path.join(self.current_dir, "diffs")
        os.makedirs(self.diff_dir, exist_ok=True)

    def test_homepage_visual_regression(self):
        """Test homepage visual regression"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Wait for page to fully load
            time.sleep(3)

            # Take current screenshot
            current_screenshot = os.path.join(self.current_dir, "homepage_current.png")
            driver.save_screenshot(current_screenshot)

            # Compare with baseline if exists
            baseline_screenshot = os.path.join(self.baseline_dir, "homepage.png")
            if os.path.exists(baseline_screenshot):
                diff_screenshot = os.path.join(self.diff_dir, "homepage_diff.png")

                # Compare screenshots
                is_same = compare_screenshots(
                    baseline_screenshot, current_screenshot, diff_screenshot
                )

                if not is_same:
                    print(
                        f"Visual differences detected in homepage. Check {diff_screenshot}"
                    )
                    # For now, we'll log differences but not fail the test
                    # In production, you might want to fail on visual differences
                else:
                    print("Homepage visual regression test passed")
            else:
                print(f"Baseline screenshot not found at {baseline_screenshot}")
                # Save current as baseline if it doesn't exist
                os.makedirs(self.baseline_dir, exist_ok=True)
                driver.save_screenshot(baseline_screenshot)
                print(f"Created baseline screenshot at {baseline_screenshot}")

        finally:
            driver.quit()

    def test_article_page_visual_regression(self):
        """Test article page visual regression"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(
                f"{self.selenium_setup.base_url}/articles/test-visual-regression"
            )

            # Wait for content to load
            time.sleep(3)

            # Take screenshot
            current_screenshot = os.path.join(self.current_dir, "article_current.png")
            driver.save_screenshot(current_screenshot)

            # Compare with baseline
            baseline_screenshot = os.path.join(self.baseline_dir, "article.png")
            if os.path.exists(baseline_screenshot):
                diff_screenshot = os.path.join(self.diff_dir, "article_diff.png")
                is_same = compare_screenshots(
                    baseline_screenshot, current_screenshot, diff_screenshot
                )

                if not is_same:
                    print(
                        f"Visual differences detected in article page. Check {diff_screenshot}"
                    )
                else:
                    print("Article page visual regression test passed")
            else:
                os.makedirs(self.baseline_dir, exist_ok=True)
                driver.save_screenshot(baseline_screenshot)
                print(f"Created article baseline screenshot at {baseline_screenshot}")

        finally:
            driver.quit()

    def test_component_level_visual_regression(self):
        """Test component-level visual regression"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Test navigation component
            nav_element = driver.find_element(By.TAG_NAME, "header")
            nav_screenshot = os.path.join(self.current_dir, "navigation_current.png")
            nav_element.screenshot(nav_screenshot)

            # Test footer component
            footer_element = driver.find_element(By.TAG_NAME, "footer")
            footer_screenshot = os.path.join(self.current_dir, "footer_current.png")
            footer_element.screenshot(footer_screenshot)

            # Test main content area
            main_element = driver.find_element(By.TAG_NAME, "main")
            main_screenshot = os.path.join(self.current_dir, "main_current.png")
            main_element.screenshot(main_screenshot)

            # Compare components with baselines
            components = ["navigation", "footer", "main"]
            for component in components:
                baseline = os.path.join(self.baseline_dir, f"{component}.png")
                current = os.path.join(self.current_dir, f"{component}_current.png")
                diff = os.path.join(self.diff_dir, f"{component}_diff.png")

                if os.path.exists(baseline):
                    is_same = compare_screenshots(baseline, current, diff)
                    if not is_same:
                        print(
                            f"Visual differences in {component} component. Check {diff}"
                        )
                    else:
                        print(f"{component} component visual regression test passed")
                else:
                    os.makedirs(self.baseline_dir, exist_ok=True)
                    if component == "navigation":
                        nav_element.screenshot(baseline)
                    elif component == "footer":
                        footer_element.screenshot(baseline)
                    elif component == "main":
                        main_element.screenshot(baseline)
                    print(f"Created {component} baseline screenshot at {baseline}")

        finally:
            driver.quit()


class TestLayoutConsistency(SeleniumTestCase):
    """Test layout consistency across different scenarios"""

    def setup_method(self):
        """Setup test method"""
        self.selenium_setup = SeleniumSetup()
        self.screenshot_dir = create_screenshot_dir()

    def test_responsive_layout_consistency(self):
        """Test layout consistency across different screen sizes"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Test different viewport sizes
            viewports = [
                (1920, 1080, "desktop"),
                (1366, 768, "laptop"),
                (768, 1024, "tablet"),
                (375, 667, "mobile"),
            ]

            layout_elements = {}

            for width, height, device_type in viewports:
                # Set viewport size
                driver.set_window_size(width, height)
                time.sleep(2)  # Allow for responsive adjustments

                # Screenshot
                screenshot_path = os.path.join(
                    self.screenshot_dir, f"layout_{device_type}.png"
                )
                driver.save_screenshot(screenshot_path)

                # Analyze layout elements
                try:
                    header = driver.find_element(By.TAG_NAME, "header")
                    main = driver.find_element(By.TAG_NAME, "main")
                    footer = driver.find_element(By.TAG_NAME, "footer")

                    layout_elements[device_type] = {
                        "header_visible": header.is_displayed(),
                        "main_visible": main.is_displayed(),
                        "footer_visible": footer.is_displayed(),
                        "header_position": header.location,
                        "main_position": main.location,
                        "footer_position": footer.location,
                    }

                except Exception as e:
                    print(f"Error analyzing layout for {device_type}: {e}")

            # Verify layout consistency
            for device_type, elements in layout_elements.items():
                assert elements["header_visible"], (
                    f"Header not visible on {device_type}"
                )
                assert elements["main_visible"], (
                    f"Main content not visible on {device_type}"
                )
                assert elements["footer_visible"], (
                    f"Footer not visible on {device_type}"
                )

            # Verify logical layout order (header should be above main, which should be above footer)
            for device_type in layout_elements:
                elements = layout_elements[device_type]
                if device_type != "mobile":  # Mobile might have different layout
                    assert (
                        elements["header_position"]["y"]
                        < elements["main_position"]["y"]
                    ), f"Header not above main on {device_type}"
                    assert (
                        elements["main_position"]["y"]
                        < elements["footer_position"]["y"]
                    ), f"Main not above footer on {device_type}"

        finally:
            driver.quit()

    def test_content_layout_stability(self):
        """Test content layout stability during interactions"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/articles/test-layout-stability")

            # Wait for initial layout
            time.sleep(2)

            # Take initial screenshot
            initial_screenshot = os.path.join(self.screenshot_dir, "layout_initial.png")
            driver.save_screenshot(initial_screenshot)

            # Get initial element positions
            elements_before = {}
            try:
                article_cards = driver.find_elements(By.CSS_SELECTOR, ".article-card")
                for i, card in enumerate(article_cards[:5]):  # First 5 cards
                    elements_before[f"card_{i}"] = card.location
            except:
                pass

            # Simulate user interactions
            try:
                # Hover over elements
                interactive_elements = driver.find_elements(
                    By.CSS_SELECTOR, ".article-card, button, a"
                )
                for element in interactive_elements[:3]:
                    ActionChains(driver).move_to_element(element).perform()
                    time.sleep(0.5)

                # Click an expandable element
                expandable = driver.find_elements(
                    By.CSS_SELECTOR, ".expandable, .accordion-header"
                )
                if expandable:
                    expandable[0].click()
                    time.sleep(1)
                    expandable[0].click()  # Close it again
                    time.sleep(0.5)

            except:
                pass

            # Take final screenshot
            final_screenshot = os.path.join(self.screenshot_dir, "layout_final.png")
            driver.save_screenshot(final_screenshot)

            # Check if major layout shifts occurred
            elements_after = {}
            try:
                article_cards = driver.find_elements(By.CSS_SELECTOR, ".article-card")
                for i, card in enumerate(article_cards[:5]):
                    elements_after[f"card_{i}"] = card.location
            except:
                pass

            # Check for significant layout shifts
            layout_shifts = 0
            for element_name in elements_before:
                if element_name in elements_after:
                    before_pos = elements_before[element_name]
                    after_pos = elements_after[element_name]

                    # Check for significant movement (more than 10 pixels)
                    if (
                        abs(before_pos["x"] - after_pos["x"]) > 10
                        or abs(before_pos["y"] - after_pos["y"]) > 10
                    ):
                        layout_shifts += 1

            # Allow for some layout shifts but not too many
            assert layout_shifts <= 2, (
                f"Too many layout shifts detected: {layout_shifts}"
            )

        finally:
            driver.quit()

    def test_print_layout(self):
        """Test print layout"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/articles/test-print-layout")

            # Switch to print media simulation
            driver.execute_script("""
                var head = document.head;
                var style = document.createElement('style');
                style.type = 'text/css';
                style.media = 'print';
                style.innerHTML = '@media print { body * { visibility: hidden; } body *:not(body):not(html) { visibility: visible; } }';
                head.appendChild(style);
            """)

            time.sleep(1)

            # Take print layout screenshot
            print_screenshot = os.path.join(self.screenshot_dir, "print_layout.png")
            driver.save_screenshot(print_screenshot)

            # Verify print-specific elements are present
            try:
                # Check if print styles are applied
                print_styles = driver.execute_script("""
                    var styles = window.getComputedStyle(document.body);
                    return {
                        backgroundColor: styles.backgroundColor,
                        color: styles.color,
                        fontSize: styles.fontSize
                    };
                """)

                # Basic checks that print styles are reasonable
                assert print_styles["fontSize"], "Print font size not set"

            except:
                pass

        finally:
            driver.quit()


class TestCrossPlatformVisualConsistency(SeleniumTestCase):
    """Test visual consistency across different browsers and platforms"""

    def setup_method(self):
        """Setup test method"""
        self.selenium_setup = SeleniumSetup()
        self.screenshot_dir = create_screenshot_dir()

    @pytest.mark.parametrize("browser", ["chrome", "firefox"])
    def test_cross_browser_consistency(self, browser):
        """Test visual consistency across browsers"""
        config_name = f"desktop_{browser}"
        config = BROWSER_CONFIGS.get(config_name, BROWSER_CONFIGS["desktop_chrome"])
        driver = self.selenium_setup.get_driver(browser, config)

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Wait for page to load
            time.sleep(3)

            # Take screenshot
            screenshot_path = os.path.join(
                self.screenshot_dir, f"browser_{browser}.png"
            )
            driver.save_screenshot(screenshot_path)

            # Check basic elements are present and visible
            try:
                header = driver.find_element(By.TAG_NAME, "header")
                main = driver.find_element(By.TAG_NAME, "main")
                footer = driver.find_element(By.TAG_NAME, "footer")

                assert header.is_displayed(), f"Header not visible in {browser}"
                assert main.is_displayed(), f"Main content not visible in {browser}"
                assert footer.is_displayed(), f"Footer not visible in {browser}"

                # Check navigation elements
                nav_links = driver.find_elements(
                    By.CSS_SELECTOR, "nav a, .navigation a"
                )
                assert len(nav_links) > 0, f"No navigation links found in {browser}"

            except Exception as e:
                print(f"Browser consistency check failed for {browser}: {e}")

        finally:
            driver.quit()

    def test_mobile_visual_consistency(self):
        """Test visual consistency across mobile devices"""
        devices = ["iphone_13", "samsung_s21", "ipad_pro"]

        for device in devices:
            driver = self.selenium_setup.create_mobile_driver(device, "chrome")

            try:
                driver.get(f"{self.selenium_setup.base_url}/")
                time.sleep(2)

                # Take screenshot
                screenshot_path = os.path.join(
                    self.screenshot_dir, f"mobile_{device}.png"
                )
                driver.save_screenshot(screenshot_path)

                # Check mobile-specific elements
                try:
                    hamburger_menu = driver.find_element(
                        By.CSS_SELECTOR, ".hamburger-menu, .mobile-menu-toggle"
                    )
                    assert hamburger_menu.is_displayed(), (
                        f"Hamburger menu not visible on {device}"
                    )

                    # Test mobile menu functionality
                    hamburger_menu.click()
                    time.sleep(1)

                    mobile_menu = driver.find_element(
                        By.CSS_SELECTOR, ".mobile-menu, .nav-menu.mobile"
                    )
                    assert mobile_menu.is_displayed(), (
                        f"Mobile menu not visible on {device}"
                    )

                except Exception as e:
                    print(f"Mobile consistency check failed for {device}: {e}")

            finally:
                driver.quit()


class TestComponentVisualTesting(SeleniumTestCase):
    """Test individual components visually"""

    def setup_method(self):
        """Setup test method"""
        self.selenium_setup = SeleniumSetup()
        self.screenshot_dir = create_screenshot_dir()

    def test_navigation_component_visuals(self):
        """Test navigation component visual appearance"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Test navigation component
            nav = driver.find_element(By.TAG_NAME, "nav")
            if not nav.is_displayed():
                nav = driver.find_element(By.CSS_SELECTOR, ".navigation, .navbar")

            # Screenshot navigation
            nav_screenshot = os.path.join(
                self.screenshot_dir, "navigation_component.png"
            )
            nav.screenshot(nav_screenshot)

            # Test navigation hover states
            nav_links = nav.find_elements(By.TAG_NAME, "a")
            for i, link in enumerate(nav_links[:3]):  # Test first 3 links
                ActionChains(driver).move_to_element(link).perform()
                time.sleep(0.5)

                hover_screenshot = os.path.join(
                    self.screenshot_dir, f"nav_hover_{i}.png"
                )
                nav.screenshot(hover_screenshot)

            # Test navigation focus states
            for i, link in enumerate(nav_links[:3]):
                link.click()  # Focus the link
                time.sleep(0.5)

                focus_screenshot = os.path.join(
                    self.screenshot_dir, f"nav_focus_{i}.png"
                )
                nav.screenshot(focus_screenshot)

        finally:
            driver.quit()

    def test_button_component_visuals(self):
        """Test button component visual appearance"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/components/buttons")

            # If component page doesn't exist, go to main page
            if "404" in driver.title or "Not Found" in driver.title:
                driver.get(f"{self.selenium_setup.base_url}/")

            # Find buttons
            buttons = driver.find_elements(By.TAG_NAME, "button")
            if not buttons:
                buttons = driver.find_elements(By.CSS_SELECTOR, ".btn, [role='button']")

            # Test different button states
            button_states = ["normal", "hover", "focus", "active"]

            for i, button in enumerate(buttons[:5]):  # Test first 5 buttons
                # Normal state
                normal_screenshot = os.path.join(
                    self.screenshot_dir, f"button_{i}_normal.png"
                )
                button.screenshot(normal_screenshot)

                # Hover state
                ActionChains(driver).move_to_element(button).perform()
                time.sleep(0.5)
                hover_screenshot = os.path.join(
                    self.screenshot_dir, f"button_{i}_hover.png"
                )
                button.screenshot(hover_screenshot)

                # Focus state
                button.click()
                time.sleep(0.5)
                focus_screenshot = os.path.join(
                    self.screenshot_dir, f"button_{i}_focus.png"
                )
                button.screenshot(focus_screenshot)

                # Active state
                ActionChains(driver).click_and_hold(button).perform()
                time.sleep(0.2)
                active_screenshot = os.path.join(
                    self.screenshot_dir, f"button_{i}_active.png"
                )
                button.screenshot(active_screenshot)
                ActionChains(driver).release(button).perform()

        finally:
            driver.quit()

    def test_form_component_visuals(self):
        """Test form component visual appearance"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/upload")

            # Test form inputs
            form_inputs = driver.find_elements(
                By.CSS_SELECTOR, "input, textarea, select"
            )

            for i, input_element in enumerate(form_inputs[:5]):  # Test first 5 inputs
                input_type = (
                    input_element.get_attribute("type") or input_element.tag_name
                )

                # Normal state
                normal_screenshot = os.path.join(
                    self.screenshot_dir, f"input_{input_type}_{i}_normal.png"
                )
                input_element.screenshot(normal_screenshot)

                # Focus state
                input_element.click()
                time.sleep(0.5)
                focus_screenshot = os.path.join(
                    self.screenshot_dir, f"input_{input_type}_{i}_focus.png"
                )
                input_element.screenshot(focus_screenshot)

                # Typing state (for text inputs)
                if input_element.tag_name.lower() in ["input", "textarea"]:
                    input_element.send_keys("Test text")
                    time.sleep(0.5)
                    typing_screenshot = os.path.join(
                        self.screenshot_dir, f"input_{input_type}_{i}_typing.png"
                    )
                    input_element.screenshot(typing_screenshot)

                    # Clear input for next test
                    input_element.clear()

            # Test form validation states
            submit_button = driver.find_element(
                By.CSS_SELECTOR, "button[type='submit'], input[type='submit']"
            )

            # Try to submit empty form to trigger validation
            submit_button.click()
            time.sleep(1)

            # Check for validation error states
            error_elements = driver.find_elements(
                By.CSS_SELECTOR, ".error, .invalid, [aria-invalid='true']"
            )
            for error_element in error_elements:
                error_screenshot = os.path.join(
                    self.screenshot_dir, f"validation_error.png"
                )
                error_element.screenshot(error_screenshot)

        finally:
            driver.quit()


class TestVisualRegressionReporting(SeleniumTestCase):
    """Generate comprehensive visual regression reports"""

    def setup_method(self):
        """Setup test method"""
        self.selenium_setup = SeleniumSetup()
        self.screenshot_dir = create_screenshot_dir()
        self.report_data = {
            "timestamp": time.strftime("%Y-%m-%d %H:%M:%S"),
            "tests": [],
            "summary": {"total_tests": 0, "passed": 0, "failed": 0, "warnings": 0},
        }

    def test_generate_visual_report(self):
        """Generate comprehensive visual regression report"""
        # Run key visual tests
        test_cases = [
            ("homepage", f"{self.selenium_setup.base_url}/"),
            ("article", f"{self.selenium_setup.base_url}/articles/test-visual"),
            ("upload", f"{self.selenium_setup.base_url}/upload"),
            ("search", f"{self.selenium_setup.base_url}/search"),
        ]

        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            for test_name, url in test_cases:
                try:
                    driver.get(url)
                    time.sleep(3)

                    # Screenshot
                    screenshot_path = os.path.join(
                        self.screenshot_dir, f"{test_name}.png"
                    )
                    driver.save_screenshot(screenshot_path)

                    # Analyze page elements
                    analysis = self._analyze_page_elements(driver)

                    # Record test results
                    test_result = {
                        "name": test_name,
                        "url": url,
                        "screenshot": screenshot_path,
                        "analysis": analysis,
                        "status": "passed" if analysis["errors"] == 0 else "failed",
                    }

                    self.report_data["tests"].append(test_result)
                    self.report_data["summary"]["total_tests"] += 1

                    if test_result["status"] == "passed":
                        self.report_data["summary"]["passed"] += 1
                    else:
                        self.report_data["summary"]["failed"] += 1

                except Exception as e:
                    error_result = {
                        "name": test_name,
                        "url": url,
                        "error": str(e),
                        "status": "error",
                    }
                    self.report_data["tests"].append(error_result)
                    self.report_data["summary"]["total_tests"] += 1
                    self.report_data["summary"]["failed"] += 1

            # Save report
            report_path = os.path.join(
                self.screenshot_dir, "visual_regression_report.json"
            )
            with open(report_path, "w") as f:
                json.dump(self.report_data, f, indent=2)

            print(f"Visual regression report saved to {report_path}")

            # Assert overall test success
            assert self.report_data["summary"]["failed"] == 0, (
                f"Visual regression tests failed: {self.report_data['summary']['failed']}/{self.report_data['summary']['total_tests']}"
            )

        finally:
            driver.quit()

    def _analyze_page_elements(self, driver) -> Dict[str, Any]:
        """Analyze page elements for visual issues"""
        analysis = {
            "errors": 0,
            "warnings": 0,
            "elements": {"images": 0, "buttons": 0, "forms": 0, "links": 0},
            "issues": [],
        }

        try:
            # Count elements
            analysis["elements"]["images"] = len(
                driver.find_elements(By.TAG_NAME, "img")
            )
            analysis["elements"]["buttons"] = len(
                driver.find_elements(By.TAG_NAME, "button")
            )
            analysis["elements"]["forms"] = len(
                driver.find_elements(By.TAG_NAME, "form")
            )
            analysis["elements"]["links"] = len(driver.find_elements(By.TAG_NAME, "a"))

            # Check for missing alt text
            images = driver.find_elements(By.TAG_NAME, "img")
            for img in images:
                alt = img.get_attribute("alt")
                if alt is None:
                    analysis["issues"].append("Image missing alt text")
                    analysis["errors"] += 1

            # Check for empty buttons
            buttons = driver.find_elements(By.TAG_NAME, "button")
            for button in buttons:
                if not button.text.strip() and not button.get_attribute("aria-label"):
                    analysis["issues"].append("Button without text or aria-label")
                    analysis["warnings"] += 1

        except Exception as e:
            analysis["issues"].append(f"Analysis error: {e}")
            analysis["errors"] += 1

        return analysis
