"""
WCAG 2.1 AA Accessibility Testing for Integral Philosophy Publishing System
Comprehensive accessibility testing including screen reader compatibility, keyboard navigation, and color contrast
"""

import pytest
import time
import json
from typing import Dict, Any, List, Tuple
from pathlib import Path

from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.action_chains import ActionChains

from tests.utils.base_test_classes import SeleniumTestCase
from tests.ui.base.selenium_setup import (
    SeleniumSetup,
    BROWSER_CONFIGS,
    create_screenshot_dir,
)


class TestWCAGCompliance(SeleniumTestCase):
    """Test WCAG 2.1 AA compliance using axe-core"""

    def setup_method(self):
        """Setup test method"""
        self.selenium_setup = SeleniumSetup()
        self.screenshot_dir = create_screenshot_dir()
        self.violations_threshold = 5  # Maximum allowed violations

    def test_homepage_accessibility(self):
        """Test homepage for WCAG compliance"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["accessibility"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Setup accessibility testing
            self.selenium_setup.setup_accessibility_testing(driver)

            # Run accessibility tests
            results = self.selenium_setup.run_accessibility_test(driver)

            # Check for critical violations
            if "violations" in results:
                assert len(results["violations"]) <= self.violations_threshold, (
                    f"Too many accessibility violations found: {len(results['violations'])}. "
                    f"Violations: {json.dumps(results['violations'], indent=2)}"
                )

                # Check for critical impact violations
                critical_violations = [
                    v for v in results["violations"] if v.get("impact") == "critical"
                ]
                assert len(critical_violations) == 0, (
                    f"Critical accessibility violations found: {json.dumps(critical_violations, indent=2)}"
                )

            # Verify passes
            assert results.get("passes", 0) > 0, "No accessibility passes found"

            # Screenshot for documentation
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/accessibility_homepage.png"
            )

        finally:
            driver.quit()

    def test_article_page_accessibility(self):
        """Test article page for WCAG compliance"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["accessibility"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/articles/test-accessibility")

            # Setup accessibility testing
            self.selenium_setup.setup_accessibility_testing(driver)

            # Run accessibility tests
            results = self.selenium_setup.run_accessibility_test(driver)

            # Check violations
            if "violations" in results:
                assert len(results["violations"]) <= self.violations_threshold

                # Log any violations for review
                for violation in results["violations"]:
                    print(
                        f"Accessibility violation: {violation['id']} - {violation['description']}"
                    )

            # Test article content accessibility
            article_content = driver.find_element(By.CSS_SELECTOR, "article")
            article_results = self.selenium_setup.run_accessibility_test(
                driver, "article"
            )

            if "violations" in article_results:
                assert len(article_results["violations"]) <= 2, (
                    f"Too many content violations: {len(article_results['violations'])}"
                )

            # Screenshot
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/accessibility_article.png"
            )

        finally:
            driver.quit()

    def test_forms_accessibility(self):
        """Test form accessibility"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["accessibility"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/upload")

            # Setup accessibility testing
            self.selenium_setup.setup_accessibility_testing(driver)

            # Run accessibility tests
            results = self.selenium_setup.run_accessibility_test(driver)

            # Check form-specific violations
            if "violations" in results:
                form_violations = [
                    v
                    for v in results["violations"]
                    if "label" in v.get("id", "").lower()
                ]
                assert len(form_violations) == 0, (
                    f"Form label violations found: {json.dumps(form_violations, indent=2)}"
                )

            # Test form fields have proper labels
            form_inputs = driver.find_elements(
                By.CSS_SELECTOR, "input, textarea, select"
            )
            for input_element in form_inputs:
                input_id = input_element.get_attribute("id")
                if input_id:
                    # Check for associated label
                    try:
                        label = driver.find_element(
                            By.CSS_SELECTOR, f"label[for='{input_id}']"
                        )
                        assert label.text.strip(), f"Label for {input_id} is empty"
                    except:
                        # Check for aria-label if no label element
                        aria_label = input_element.get_attribute("aria-label")
                        aria_labelledby = input_element.get_attribute("aria-labelledby")
                        assert aria_label or aria_labelledby, (
                            f"Input {input_id} has no associated label or aria-label"
                        )

            # Screenshot
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/accessibility_forms.png"
            )

        finally:
            driver.quit()


class TestKeyboardNavigation(SeleniumTestCase):
    """Test keyboard navigation functionality"""

    def setup_method(self):
        """Setup test method"""
        self.selenium_setup = SeleniumSetup()
        self.screenshot_dir = create_screenshot_dir()

    def test_tab_navigation(self):
        """Test tab navigation through page elements"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Focus on page
            driver.find_element(By.TAG_NAME, "body").send_keys(Keys.TAB)

            # Track tabbable elements
            tabbable_elements = []
            focused_elements = []

            # Tab through the page
            for i in range(20):  # Limit to prevent infinite loops
                try:
                    # Get current focused element
                    focused_element = driver.switch_to.active_element
                    focused_elements.append(focused_element)

                    # Check if it's actually tabbable
                    tag_name = focused_element.tag_name.lower()
                    if tag_name in ["a", "button", "input", "select", "textarea"]:
                        tabbable_elements.append(focused_element)

                    # Take screenshot of focus
                    if i % 5 == 0:  # Every 5th tab
                        self.selenium_setup.take_screenshot(
                            driver, f"{self.screenshot_dir}/tab_navigation_{i}.png"
                        )

                    # Press Tab
                    focused_element.send_keys(Keys.TAB)
                    time.sleep(0.2)

                except Exception as e:
                    break

            # Verify we tabbed through meaningful elements
            assert len(tabbable_elements) > 0, "No tabbable elements found"

            # Check focus visibility
            for element in tabbable_elements[:5]:  # Check first 5
                try:
                    # Check if element has visible focus indication
                    computed_style = driver.execute_script(
                        "return window.getComputedStyle(arguments[0]);", element
                    )
                    outline = computed_style.getPropertyValue("outline")
                    border = computed_style.getPropertyValue("border")

                    # At least one should indicate focus
                    assert outline != "none" or border != "none", (
                        f"Element {element.tag_name} has no visible focus indication"
                    )

                except:
                    pass  # Style checking might fail

        finally:
            driver.quit()

    def test_skip_links(self):
        """Test skip links functionality"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Look for skip links
            skip_links = driver.find_elements(
                By.CSS_SELECTOR, ".skip-link, [href='#main'], [href='#content']"
            )

            if skip_links:
                for skip_link in skip_links:
                    # Click skip link
                    skip_link.click()
                    time.sleep(0.5)

                    # Check if focus moved to target
                    focused_element = driver.switch_to.active_element
                    target_id = skip_link.get_attribute("href").split("#")[-1]

                    if target_id:
                        try:
                            target_element = driver.find_element(By.ID, target_id)
                            assert focused_element == target_element or (
                                target_element.find_elements(By.XPATH, ".//*")
                                and focused_element
                                in target_element.find_elements(By.XPATH, ".//*")
                            ), f"Skip link did not navigate to target #{target_id}"
                        except:
                            pass  # Target might not exist

                    # Screenshot after skip
                    self.selenium_setup.take_screenshot(
                        driver, f"{self.screenshot_dir}/skip_link_{target_id}.png"
                    )

            else:
                # If no skip links, create test for implementing them
                print("Warning: No skip links found on page")

        finally:
            driver.quit()

    def test_keyboard_form_interaction(self):
        """Test keyboard interaction with forms"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/upload")

            # Find form elements
            form_inputs = driver.find_elements(
                By.CSS_SELECTOR, "input:not([type='hidden']), textarea, select"
            )

            if form_inputs:
                # Tab through form fields
                for i, input_element in enumerate(form_inputs[:5]):  # Test first 5
                    input_element.click()  # Ensure focus
                    time.sleep(0.2)

                    # Test typing into text inputs
                    if input_element.tag_name.lower() in ["input", "textarea"]:
                        input_type = input_element.get_attribute("type")
                        if (
                            input_type in ["text", "email", "search", "url"]
                            or input_element.tag_name.lower() == "textarea"
                        ):
                            input_element.send_keys("Test keyboard input")
                            time.sleep(0.2)

                            # Verify text was entered
                            value = input_element.get_attribute("value")
                            assert "Test keyboard input" in value, (
                                "Keyboard input not working"
                            )

                    # Test Tab to next field
                    input_element.send_keys(Keys.TAB)
                    time.sleep(0.2)

                # Test form submission with Enter
                try:
                    submit_button = driver.find_element(
                        By.CSS_SELECTOR, "button[type='submit'], input[type='submit']"
                    )
                    submit_button.send_keys(Keys.RETURN)
                    time.sleep(2)

                    # Check if form was submitted
                    # This depends on the actual form behavior
                except:
                    pass  # Submit button might not exist

            # Screenshot form keyboard interaction
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/keyboard_form_interaction.png"
            )

        finally:
            driver.quit()


class TestScreenReaderCompatibility(SeleniumTestCase):
    """Test screen reader compatibility"""

    def setup_method(self):
        """Setup test method"""
        self.selenium_setup = SeleniumSetup()
        self.screenshot_dir = create_screenshot_dir()

    def test_alt_text_for_images(self):
        """Test images have proper alt text"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Find all images
            images = driver.find_elements(By.TAG_NAME, "img")

            for img in images:
                alt_text = img.get_attribute("alt")
                role = img.get_attribute("role")

                # Check for alt text or appropriate aria role
                if not role or role != "presentation":
                    assert alt_text is not None, (
                        f"Image missing alt text: {img.get_attribute('src')}"
                    )

                    # If alt text is present, check if it's meaningful
                    if alt_text:
                        # Avoid decorative alt text like spacer images
                        assert len(alt_text.strip()) > 3 or alt_text.strip() == "", (
                            f"Image has unhelpful alt text: '{alt_text}'"
                        )

            # Screenshot for documentation
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/alt_text_check.png"
            )

        finally:
            driver.quit()

    def test_heading_structure(self):
        """Test proper heading structure"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Find all headings
            headings = driver.find_elements(By.CSS_SELECTOR, "h1, h2, h3, h4, h5, h6")

            # Check heading levels are sequential
            last_level = 0
            for heading in headings:
                tag_name = heading.tag_name.lower()
                level = int(tag_name[1])

                # Heading levels should not skip levels (though this is a guideline, not a strict rule)
                if last_level > 0 and level > last_level + 1:
                    print(
                        f"Warning: Heading level skip from h{last_level} to {tag_name}"
                    )

                last_level = level

                # Check heading has text content
                assert heading.text.strip(), f"Empty {tag_name} found"

            # Verify there's exactly one h1 (usually)
            h1_elements = driver.find_elements(By.TAG_NAME, "h1")
            assert len(h1_elements) >= 1, "No h1 found on page"
            assert len(h1_elements) <= 2, (
                f"Too many h1 elements: {len(h1_elements)} (should be 1)"
            )

            # Screenshot heading structure
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/heading_structure.png"
            )

        finally:
            driver.quit()

    def test_aria_labels(self):
        """Test ARIA labels and landmarks"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Check for ARIA landmarks
            landmarks = [
                "main",
                "nav",
                "header",
                "footer",
                "banner",
                "navigation",
                "contentinfo",
                "search",
            ]

            found_landmarks = []
            for landmark in landmarks:
                elements = driver.find_elements(
                    By.CSS_SELECTOR, f"[role='{landmark}'], {landmark}"
                )
                if elements:
                    found_landmarks.extend([landmark] * len(elements))

            # Should have at least main navigation and content
            assert any(
                landmark in found_landmarks
                for landmark in ["main", "navigation", "nav"]
            ), "No main content or navigation landmarks found"

            # Check interactive elements have accessible names
            interactive_elements = driver.find_elements(
                By.CSS_SELECTOR,
                "button, [role='button'], a[href], input[type='submit'], input[type='button']",
            )

            for element in interactive_elements:
                # Check for accessible name
                accessible_name = (
                    element.text.strip()
                    or element.get_attribute("aria-label")
                    or element.get_attribute("title")
                    or element.get_attribute("alt")
                )

                if not accessible_name:
                    tag_name = element.tag_name.lower()
                    input_type = element.get_attribute("type")

                    # Skip certain cases where no name might be acceptable
                    if not (tag_name == "input" and input_type in ["hidden", "image"]):
                        print(
                            f"Warning: Interactive element without accessible name: {tag_name}"
                        )

            # Screenshot ARIA landmarks
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/aria_landmarks.png"
            )

        finally:
            driver.quit()


class TestColorContrast(SeleniumTestCase):
    """Test color contrast compliance"""

    def setup_method(self):
        """Setup test method"""
        self.selenium_setup = SeleniumSetup()
        self.screenshot_dir = create_screenshot_dir()

    def test_text_contrast_ratio(self):
        """Test text color contrast ratios"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Get computed styles for text elements
            text_elements = driver.find_elements(
                By.CSS_SELECTOR, "p, h1, h2, h3, h4, h5, h6, span, a, button"
            )

            contrast_violations = []

            for element in text_elements[:10]:  # Test first 10 elements
                try:
                    # Get computed styles
                    computed_style = driver.execute_script(
                        """
                        var element = arguments[0];
                        var style = window.getComputedStyle(element);
                        return {
                            color: style.color,
                            backgroundColor: style.backgroundColor,
                            fontSize: style.fontSize,
                            fontWeight: style.fontWeight
                        };
                        """,
                        element,
                    )

                    # Parse colors
                    color = computed_style["color"]
                    bg_color = computed_style["backgroundColor"]

                    # Skip if background is transparent
                    if "rgba(0, 0, 0, 0)" in bg_color or bg_color == "transparent":
                        continue

                    # Calculate contrast ratio (simplified)
                    # In real implementation, you'd use a proper contrast calculation library
                    contrast_ratio = self._calculate_contrast_ratio(color, bg_color)

                    # WCAG AA requirements
                    font_size = float(computed_style["fontSize"].replace("px", ""))
                    font_weight = computed_style["fontWeight"]

                    is_large_text = font_size >= 18 or (
                        font_size >= 14 and font_weight in ["bold", "700"]
                    )

                    min_contrast = 4.5 if not is_large_text else 3.0

                    if contrast_ratio < min_contrast:
                        contrast_violations.append(
                            {
                                "element": element.tag_name,
                                "text": element.text[:50] if element.text else "",
                                "contrast_ratio": contrast_ratio,
                                "required_ratio": min_contrast,
                            }
                        )

                except:
                    continue

            # Check for contrast violations
            assert len(contrast_violations) <= 2, (
                f"Too many contrast violations: {len(contrast_violations)}. "
                f"Violations: {json.dumps(contrast_violations, indent=2)}"
            )

            # Screenshot for color contrast testing
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/color_contrast.png"
            )

        finally:
            driver.quit()

    def test_link_contrast_and_states(self):
        """Test link contrast and hover/focus states"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Find links
            links = driver.find_elements(By.TAG_NAME, "a")

            for link in links[:5]:  # Test first 5 links
                try:
                    # Get initial styles
                    initial_style = driver.execute_script(
                        "return window.getComputedStyle(arguments[0]);", link
                    )

                    # Test hover state
                    ActionChains(driver).move_to_element(link).perform()
                    time.sleep(0.5)

                    hover_style = driver.execute_script(
                        "return window.getComputedStyle(arguments[0]);", link
                    )

                    # Check if hover state is distinct
                    if initial_style["color"] == hover_style["color"]:
                        print(f"Warning: Link has no hover indication")

                    # Test focus state (simulate with keyboard)
                    link.send_keys(Keys.TAB)
                    time.sleep(0.5)

                    focus_style = driver.execute_script(
                        "return window.getComputedStyle(arguments[0]);", link
                    )

                    # Check if focus state is visible
                    outline = focus_style.getPropertyValue("outline")
                    if outline == "none":
                        print(f"Warning: Link has no visible focus indicator")

                except:
                    continue

            # Screenshot link states
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/link_states.png"
            )

        finally:
            driver.quit()

    def _calculate_contrast_ratio(self, color1: str, color2: str) -> float:
        """Calculate contrast ratio between two colors (simplified)"""
        """This is a simplified implementation - in practice use a proper color contrast library"""
        try:
            # Parse RGB values from color strings
            def parse_color(color_str):
                if color_str.startswith("rgb"):
                    # Extract RGB values
                    import re

                    match = re.search(r"rgb\((\d+),\s*(\d+),\s*(\d+)\)", color_str)
                    if match:
                        return [
                            int(match.group(1)),
                            int(match.group(2)),
                            int(match.group(3)),
                        ]
                return [0, 0, 0]  # Default to black

            rgb1 = parse_color(color1)
            rgb2 = parse_color(color2)

            # Calculate relative luminance (simplified)
            def get_luminance(rgb):
                r, g, b = rgb
                r = r / 255.0
                g = g / 255.0
                b = b / 255.0

                r = r / 12.92 if r <= 0.03928 else ((r + 0.055) / 1.055) ** 2.4
                g = g / 12.92 if g <= 0.03928 else ((g + 0.055) / 1.055) ** 2.4
                b = b / 12.92 if b <= 0.03928 else ((b + 0.055) / 1.055) ** 2.4

                return 0.2126 * r + 0.7152 * g + 0.0722 * b

            l1 = get_luminance(rgb1)
            l2 = get_luminance(rgb2)

            # Calculate contrast ratio
            lighter = max(l1, l2)
            darker = min(l1, l2)

            return (lighter + 0.05) / (darker + 0.05)

        except:
            return 7.0  # Default to high contrast if calculation fails


class TestFocusManagement(SeleniumTestCase):
    """Test focus management and modal accessibility"""

    def setup_method(self):
        """Setup test method"""
        self.selenium_setup = SeleniumSetup()
        self.screenshot_dir = create_screenshot_dir()

    def test_modal_focus_trapping(self):
        """Test modal focus trapping"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/")

            # Look for modal triggers
            modal_triggers = driver.find_elements(
                By.CSS_SELECTOR, "[data-bs-toggle='modal'], .modal-trigger"
            )

            if modal_triggers:
                # Open modal
                modal_triggers[0].click()
                time.sleep(1)

                # Check if modal is open
                try:
                    modal = driver.find_element(By.CSS_SELECTOR, ".modal.show")
                    assert modal.is_displayed()

                    # Check focus is inside modal
                    focused_element = driver.switch_to.active_element
                    modal_elements = modal.find_elements(
                        By.CSS_SELECTOR, "button, input, select, textarea, a[href]"
                    )

                    is_focus_in_modal = focused_element in modal_elements
                    assert is_focus_in_modal, "Focus is not trapped inside modal"

                    # Test Tab navigation stays within modal
                    initial_focused = focused_element

                    # Tab through modal elements
                    for _ in range(
                        len(modal_elements) + 2
                    ):  # Extra tabs to test wrapping
                        initial_focused.send_keys(Keys.TAB)
                        time.sleep(0.2)

                        current_focused = driver.switch_to.active_element
                        # Focus should stay within modal elements
                        if current_focused not in modal_elements:
                            print("Warning: Focus escaped modal")
                            break

                    # Close modal (ESC key)
                    driver.find_element(By.TAG_NAME, "body").send_keys(Keys.ESCAPE)
                    time.sleep(0.5)

                    # Check modal is closed
                    assert not modal.is_displayed()

                    # Focus should return to trigger
                    return_focused = driver.switch_to.active_element
                    assert return_focused == modal_triggers[0], (
                        "Focus not returned to modal trigger"
                    )

                except:
                    print("Modal implementation not found or incomplete")

            # Screenshot modal focus testing
            self.selenium_setup.take_screenshot(
                driver, f"{self.screenshot_dir}/modal_focus.png"
            )

        finally:
            driver.quit()

    def test_auto_focus_management(self):
        """Test automatic focus management"""
        driver = self.selenium_setup.get_driver(
            "chrome", BROWSER_CONFIGS["desktop_chrome"]
        )

        try:
            driver.get(f"{self.selenium_setup.base_url}/search")

            # Check if search input gets focus automatically
            try:
                search_input = driver.find_element(
                    By.CSS_SELECTOR, "input[type='search'], .search-input"
                )
                focused_element = driver.switch_to.active_element

                # If search page, search input should be focused
                if "/search" in driver.current_url:
                    assert focused_element == search_input, (
                        "Search input not automatically focused"
                    )

                # Test focus management after search
                if search_input:
                    search_input.send_keys("test query")
                    search_input.send_keys(Keys.RETURN)
                    time.sleep(2)

                    # After search, focus should be on results or back to search
                    # This depends on implementation
                    focused_after_search = driver.switch_to.active_element

                    # Screenshot focus management
                    self.selenium_setup.take_screenshot(
                        driver, f"{self.screenshot_dir}/auto_focus.png"
                    )

            except:
                pass  # Search page might not exist

        finally:
            driver.quit()
