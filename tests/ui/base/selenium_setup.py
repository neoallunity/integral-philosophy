"""
Selenium WebDriver Setup for UI Testing
Provides comprehensive WebDriver management for multiple browsers and devices
"""

import os
import time
import platform
import tempfile
from pathlib import Path
from typing import Dict, Optional, Tuple, Any, List
from dataclasses import dataclass
from selenium import webdriver
from selenium.webdriver.chrome.options import Options as ChromeOptions
from selenium.webdriver.firefox.options import Options as FirefoxOptions
from selenium.webdriver.safari.options import Options as SafariOptions
from selenium.webdriver.edge.options import Options as EdgeOptions
from selenium.webdriver.chrome.service import Service as ChromeService
from selenium.webdriver.firefox.service import Service as FirefoxService
from selenium.webdriver.edge.service import Service as EdgeService
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.remote.webelement import WebElement
from selenium.webdriver.common.action_chains import ActionChains
from selenium.common.exceptions import (
    TimeoutException,
    NoSuchElementException,
    WebDriverException,
)

try:
    from webdriver_manager.chrome import ChromeDriverManager
    from webdriver_manager.firefox import GeckoDriverManager
    from webdriver_manager.microsoft import EdgeChromiumDriverManager

    WEBDRIVER_MANAGER_AVAILABLE = True
except ImportError:
    WEBDRIVER_MANAGER_AVAILABLE = False
    ChromeDriverManager = None
    GeckoDriverManager = None
    EdgeChromiumDriverManager = None


@dataclass
class BrowserConfig:
    """Browser configuration for testing"""

    name: str
    headless: bool = True
    window_size: Tuple[int, int] = (1920, 1080)
    mobile_emulation: Optional[Dict[str, Any]] = None
    user_agent: Optional[str] = None
    extensions: Optional[List[str]] = None
    proxy: Optional[str] = None
    download_dir: Optional[str] = None
    javascript_enabled: bool = True
    accept_insecure_certs: bool = True


@dataclass
class DeviceConfig:
    """Mobile device configuration"""

    name: str
    width: int
    height: int
    pixel_ratio: float
    user_agent: str


# Predefined device configurations
DEVICES = {
    "iphone_13": DeviceConfig(
        name="iPhone 13",
        width=390,
        height=844,
        pixel_ratio=3.0,
        user_agent="Mozilla/5.0 (iPhone; CPU iPhone OS 15_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.0 Mobile/15E148 Safari/604.1",
    ),
    "samsung_s21": DeviceConfig(
        name="Samsung Galaxy S21",
        width=360,
        height=640,
        pixel_ratio=3.0,
        user_agent="Mozilla/5.0 (Linux; Android 11; SM-G991B) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.120 Mobile Safari/537.36",
    ),
    "ipad_pro": DeviceConfig(
        name="iPad Pro",
        width=1024,
        height=1366,
        pixel_ratio=2.0,
        user_agent="Mozilla/5.0 (iPad; CPU OS 15_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.0 Mobile/15E148 Safari/604.1",
    ),
}


class SeleniumSetup:
    """Comprehensive Selenium WebDriver setup and management"""

    def __init__(self, base_url: str = "http://localhost:5000"):
        self.base_url = base_url
        self.drivers: Dict[str, webdriver.Remote] = {}
        self.temp_dirs: List[tempfile.TemporaryDirectory] = []

    def create_chrome_driver(self, config: BrowserConfig) -> webdriver.Chrome:
        """Create Chrome WebDriver with specified configuration"""
        options = ChromeOptions()

        # Basic configuration
        if config.headless:
            options.add_argument("--headless=new")

        options.add_argument(
            f"--window-size={config.window_size[0]},{config.window_size[1]}"
        )
        options.add_argument("--no-sandbox")
        options.add_argument("--disable-dev-shm-usage")
        options.add_argument("--disable-gpu")
        options.add_argument("--disable-extensions")
        options.add_argument("--disable-notifications")
        options.add_argument("--disable-popup-blocking")

        # Security settings
        if config.accept_insecure_certs:
            options.add_argument("--ignore-certificate-errors")
            options.add_argument("--ignore-ssl-errors")
            options.add_argument("--ignore-certificate-errors-spki-list")

        # JavaScript control
        if not config.javascript_enabled:
            options.add_argument("--disable-javascript")

        # Mobile emulation
        if config.mobile_emulation:
            options.add_experimental_option("mobileEmulation", config.mobile_emulation)

        # User agent
        if config.user_agent:
            options.add_argument(f"--user-agent={config.user_agent}")

        # Proxy configuration
        if config.proxy:
            options.add_argument(f"--proxy-server={config.proxy}")

        # Download directory
        if config.download_dir:
            prefs = {
                "download.default_directory": config.download_dir,
                "download.prompt_for_download": False,
                "download.directory_upgrade": True,
                "safebrowsing.enabled": True,
            }
            options.add_experimental_option("prefs", prefs)

        # Extensions
        if config.extensions:
            for extension in config.extensions:
                options.add_extension(extension)

        # Performance optimizations
        options.add_argument("--disable-background-timer-throttling")
        options.add_argument("--disable-backgrounding-occluded-windows")
        options.add_argument("--disable-renderer-backgrounding")

        try:
            if WEBDRIVER_MANAGER_AVAILABLE and ChromeDriverManager:
                service = ChromeService(ChromeDriverManager().install())
                driver = webdriver.Chrome(service=service, options=options)
            else:
                driver = webdriver.Chrome(options=options)
            driver.set_page_load_timeout(30)
            driver.implicitly_wait(10)
            return driver
        except Exception as e:
            raise WebDriverException(f"Failed to create Chrome driver: {e}")

    def create_firefox_driver(self, config: BrowserConfig) -> webdriver.Firefox:
        """Create Firefox WebDriver with specified configuration"""
        options = FirefoxOptions()

        # Basic configuration
        if config.headless:
            options.add_argument("--headless")

        options.add_argument(f"--width={config.window_size[0]}")
        options.add_argument(f"--height={config.window_size[1]}")

        # User agent
        if config.user_agent:
            options.set_preference("general.useragent.override", config.user_agent)

        # Proxy configuration
        if config.proxy:
            proxy_host, proxy_port = config.proxy.split(":")
            options.set_preference("network.proxy.type", 1)
            options.set_preference("network.proxy.http", proxy_host)
            options.set_preference("network.proxy.http_port", int(proxy_port))

        # Download directory
        if config.download_dir:
            options.set_preference("browser.download.folderList", 2)
            options.set_preference("browser.download.dir", config.download_dir)
            options.set_preference(
                "browser.helperApps.neverAsk.saveToDisk", "application/octet-stream"
            )

        # Security settings
        if config.accept_insecure_certs:
            options.set_preference("webdriver_accept_untrusted_certs", True)
            options.set_preference("webdriver_assume_untrusted_issuer", True)

        # JavaScript control
        options.set_preference("javascript.enabled", config.javascript_enabled)

        try:
            if WEBDRIVER_MANAGER_AVAILABLE and GeckoDriverManager:
                service = FirefoxService(GeckoDriverManager().install())
                driver = webdriver.Firefox(service=service, options=options)
            else:
                driver = webdriver.Firefox(options=options)
            driver.set_page_load_timeout(30)
            driver.implicitly_wait(10)
            return driver
        except Exception as e:
            raise WebDriverException(f"Failed to create Firefox driver: {e}")

    def create_safari_driver(self, config: BrowserConfig) -> webdriver.Safari:
        """Create Safari WebDriver (limited options support)"""
        if platform.system() != "Darwin":
            raise WebDriverException("Safari is only available on macOS")

        options = SafariOptions()

        # Safari has limited configuration options
        driver = webdriver.Safari(options=options)
        driver.set_page_load_timeout(30)
        driver.implicitly_wait(10)

        if config.window_size != (1920, 1080):
            driver.set_window_size(config.window_size[0], config.window_size[1])

        return driver

    def create_edge_driver(self, config: BrowserConfig) -> webdriver.Edge:
        """Create Edge WebDriver with specified configuration"""
        options = EdgeOptions()

        # Basic configuration
        if config.headless:
            options.add_argument("--headless")

        options.add_argument(
            f"--window-size={config.window_size[0]},{config.window_size[1]}"
        )
        options.add_argument("--no-sandbox")
        options.add_argument("--disable-dev-shm-usage")

        # User agent
        if config.user_agent:
            options.add_argument(f"--user-agent={config.user_agent}")

        # Mobile emulation
        if config.mobile_emulation:
            options.add_experimental_option("mobileEmulation", config.mobile_emulation)

        try:
            if WEBDRIVER_MANAGER_AVAILABLE and EdgeChromiumDriverManager:
                service = EdgeService(EdgeChromiumDriverManager().install())
                driver = webdriver.Edge(service=service, options=options)
            else:
                driver = webdriver.Edge(options=options)
            driver.set_page_load_timeout(30)
            driver.implicitly_wait(10)
            return driver
        except Exception as e:
            raise WebDriverException(f"Failed to create Edge driver: {e}")

    def get_driver(
        self, browser: str = "chrome", config: Optional[BrowserConfig] = None
    ) -> webdriver.Remote:
        """Get WebDriver for specified browser"""
        if config is None:
            config = BrowserConfig(name=browser)

        driver_key = f"{browser}_{id(config)}"

        if driver_key not in self.drivers:
            if browser.lower() == "chrome":
                driver = self.create_chrome_driver(config)
            elif browser.lower() == "firefox":
                driver = self.create_firefox_driver(config)
            elif browser.lower() == "safari":
                driver = self.create_safari_driver(config)
            elif browser.lower() == "edge":
                driver = self.create_edge_driver(config)
            else:
                raise ValueError(f"Unsupported browser: {browser}")

            self.drivers[driver_key] = driver

        return self.drivers[driver_key]

    def create_mobile_driver(
        self, device_name: str, browser: str = "chrome", headless: bool = True
    ) -> webdriver.Remote:
        """Create mobile device driver"""
        if device_name not in DEVICES:
            raise ValueError(
                f"Unknown device: {device_name}. Available: {list(DEVICES.keys())}"
            )

        device = DEVICES[device_name]

        mobile_config = BrowserConfig(
            name=f"{browser}_{device_name}",
            headless=headless,
            window_size=(device.width, device.height),
            user_agent=device.user_agent,
            mobile_emulation={
                "deviceMetrics": {
                    "width": device.width,
                    "height": device.height,
                    "pixelRatio": device.pixel_ratio,
                },
                "userAgent": device.user_agent,
            },
        )

        return self.get_driver(browser, mobile_config)

    def wait_for_element(
        self, driver: webdriver.Remote, locator: Tuple[str, str], timeout: int = 10
    ) -> WebElement:
        """Wait for element to be present"""
        try:
            wait = WebDriverWait(driver, timeout)
            return wait.until(EC.presence_of_element_located(locator))
        except TimeoutException:
            raise NoSuchElementException(
                f"Element {locator} not found within {timeout} seconds"
            )

    def wait_for_clickable(
        self, driver: webdriver.Remote, locator: Tuple[str, str], timeout: int = 10
    ) -> WebElement:
        """Wait for element to be clickable"""
        try:
            wait = WebDriverWait(driver, timeout)
            return wait.until(EC.element_to_be_clickable(locator))
        except TimeoutException:
            raise NoSuchElementException(
                f"Element {locator} not clickable within {timeout} seconds"
            )

    def wait_for_text_in_element(
        self,
        driver: webdriver.Remote,
        locator: Tuple[str, str],
        text: str,
        timeout: int = 10,
    ) -> bool:
        """Wait for text to appear in element"""
        try:
            wait = WebDriverWait(driver, timeout)
            return wait.until(EC.text_to_be_present_in_element(locator, text))
        except TimeoutException:
            return False

    def wait_for_url_contains(
        self, driver: webdriver.Remote, text: str, timeout: int = 10
    ) -> bool:
        """Wait for URL to contain specific text"""
        try:
            wait = WebDriverWait(driver, timeout)
            return wait.until(EC.url_contains(text))
        except TimeoutException:
            return False

    def scroll_to_element(self, driver: webdriver.Remote, element: WebElement):
        """Scroll to specific element"""
        driver.execute_script("arguments[0].scrollIntoView(true);", element)
        time.sleep(0.5)  # Allow smooth scroll

    def scroll_to_bottom(self, driver: webdriver.Remote):
        """Scroll to bottom of page"""
        driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
        time.sleep(0.5)

    def take_screenshot(self, driver: webdriver.Remote, filename: str) -> str:
        """Take screenshot and save to file"""
        driver.save_screenshot(filename)
        return filename

    def get_element_screenshot(
        self, driver: webdriver.Remote, element: WebElement, filename: str
    ) -> str:
        """Take screenshot of specific element"""
        element.screenshot(filename)
        return filename

    def execute_async_script(self, driver: webdriver.Remote, script: str, *args) -> Any:
        """Execute asynchronous JavaScript"""
        return driver.execute_async_script(script, *args)

    def get_page_load_time(self, driver: webdriver.Remote) -> float:
        """Get page load time using Navigation Timing API"""
        script = """
        return performance.timing.loadEventEnd - performance.timing.navigationStart;
        """
        return driver.execute_script(script)

    def get_dom_content_loaded_time(self, driver: webdriver.Remote) -> float:
        """Get DOM content loaded time"""
        script = """
        return performance.timing.domContentLoadedEventEnd - performance.timing.navigationStart;
        """
        return driver.execute_script(script)

    def get_network_requests(self, driver: webdriver.Remote) -> List[Dict[str, Any]]:
        """Get network request information (requires performance logging)"""
        try:
            logs = driver.get_log("performance")
            requests = []

            for entry in logs:
                message = entry["message"]
                if "Network.responseReceived" in message:
                    try:
                        data = eval(message)
                        requests.append(
                            {
                                "url": data["message"]["params"]["response"]["url"],
                                "status": data["message"]["params"]["response"][
                                    "status"
                                ],
                                "method": data["message"]["params"]["response"][
                                    "requestMethod"
                                ],
                                "type": data["message"]["params"]["type"],
                                "timestamp": data["message"]["params"]["timestamp"],
                            }
                        )
                    except:
                        continue

            return requests
        except Exception:
            return []

    def setup_accessibility_testing(self, driver: webdriver.Remote):
        """Setup accessibility testing by injecting axe-core"""
        axe_script = """
        (function() {
            var script = document.createElement('script');
            script.src = 'https://cdnjs.cloudflare.com/ajax/libs/axe-core/4.6.3/axe.min.js';
            script.onload = function() {
                window.axeInstalled = true;
            };
            document.head.appendChild(script);
        })();
        """
        driver.execute_script(axe_script)

        # Wait for axe to load
        wait = WebDriverWait(driver, 10)
        wait.until(lambda d: d.execute_script("return window.axeInstalled === true"))

    def run_accessibility_test(
        self, driver: webdriver.Remote, context: Optional[str] = None
    ) -> Dict[str, Any]:
        """Run accessibility tests using axe-core"""
        axe_script = f"""
        return axe.run({f'document.querySelector("{context}")' if context else "document"}, {{
            include: [['#content']],
            exclude: [['[aria-hidden="true"]']],
            rules: {{
                'color-contrast': {{ enabled: true }},
                'keyboard-navigation': {{ enabled: true }},
                'aria-labels': {{ enabled: true }},
                'focus-management': {{ enabled: true }}
            }}
        }}).then(results => {{
            return {{
                passes: results.passes.length,
                violations: results.violations.map(v => ({{
                    id: v.id,
                    impact: v.impact,
                    description: v.description,
                    nodes: v.nodes.length
                }})),
                incomplete: results.incomplete.length,
                timestamp: results.testEnvironment.timestamp
            }};
        }}).catch(error => {{
            return {{ error: error.message }};
        }});
        """

        return driver.execute_script(axe_script)

    def cleanup_all_drivers(self):
        """Clean up all active drivers"""
        for driver_key, driver in self.drivers.items():
            try:
                driver.quit()
            except:
                pass

        self.drivers.clear()

        # Clean up temporary directories
        for temp_dir in self.temp_dirs:
            try:
                temp_dir.cleanup()
            except:
                pass

        self.temp_dirs.clear()


# Predefined browser configurations for testing
BROWSER_CONFIGS = {
    "desktop_chrome": BrowserConfig(
        name="desktop_chrome",
        headless=True,
        window_size=(1920, 1080),
        javascript_enabled=True,
    ),
    "desktop_firefox": BrowserConfig(
        name="desktop_firefox",
        headless=True,
        window_size=(1920, 1080),
        javascript_enabled=True,
    ),
    "mobile_chrome": BrowserConfig(
        name="mobile_chrome",
        headless=True,
        window_size=(375, 667),
        user_agent="Mozilla/5.0 (iPhone; CPU iPhone OS 14_0 like Mac OS X) AppleWebKit/605.1.15",
    ),
    "tablet_chrome": BrowserConfig(
        name="tablet_chrome",
        headless=True,
        window_size=(768, 1024),
        user_agent="Mozilla/5.0 (iPad; CPU OS 14_0 like Mac OS X) AppleWebKit/605.1.15",
    ),
    "accessibility": BrowserConfig(
        name="accessibility",
        headless=True,
        window_size=(1920, 1080),
        javascript_enabled=True,
    ),
}


# Utility functions for common operations
def create_screenshot_dir(base_dir: str = "test_screenshots") -> str:
    """Create screenshot directory with timestamp"""
    timestamp = time.strftime("%Y%m%d_%H%M%S")
    screenshot_dir = os.path.join(base_dir, f"ui_test_{timestamp}")
    os.makedirs(screenshot_dir, exist_ok=True)
    return screenshot_dir


def compare_screenshots(baseline_path: str, current_path: str, diff_path: str) -> bool:
    """Compare two screenshots (requires PIL)"""
    try:
        from PIL import Image, ImageChops

        baseline = Image.open(baseline_path)
        current = Image.open(current_path)

        diff = ImageChops.difference(baseline, current)

        if diff.getbbox():
            diff.save(diff_path)
            return False

        return True
    except ImportError:
        print("PIL not available for screenshot comparison")
        return False
