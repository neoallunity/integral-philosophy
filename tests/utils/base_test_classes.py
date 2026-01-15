"""
Base test classes for Integral Philosophy Publishing System tests
"""

import pytest
import asyncio
import tempfile
import shutil
from pathlib import Path
from typing import Dict, List, Any, Optional, Union, Tuple
from unittest.mock import Mock, MagicMock, patch
import time
import json
import threading

from .test_helpers import (
    TestDataManager,
    WebDriverManager,
    MockServer,
    PerformanceMonitor,
    AsyncTestHelper,
    FileComparator,
    HTTPTestHelper,
    SystemTestHelper,
)


class BaseTestCase:
    """Base test class with common functionality"""

    @pytest.fixture(autouse=True)
    def setup_test_environment(self, temp_dir):
        """Setup test environment for each test"""
        self.temp_dir = temp_dir
        self.data_manager = TestDataManager(temp_dir)
        self.http_helper = HTTPTestHelper()
        self.system_helper = SystemTestHelper()

        yield

        # Cleanup
        self.data_manager.cleanup()

    def assert_files_equal(self, file1: Union[str, Path], file2: Union[str, Path]):
        """Assert two files are equal"""
        path1 = Path(file1) if isinstance(file1, str) else file1
        path2 = Path(file2) if isinstance(file2, str) else file2
        assert FileComparator.compare_files(path1, path2), (
            f"Files {path1} and {path2} differ"
        )

    def assert_valid_json(self, content: str):
        """Assert content is valid JSON"""
        try:
            json.loads(content)
        except json.JSONDecodeError as e:
            pytest.fail(f"Invalid JSON: {e}")

    def assert_valid_xml(self, content: str):
        """Assert content is valid XML"""
        import xml.etree.ElementTree as ET

        try:
            ET.fromstring(content)
        except ET.ParseError as e:
            pytest.fail(f"Invalid XML: {e}")

    def create_sample_files(self) -> Dict[str, Path]:
        """Create sample files for testing"""
        return {
            "sample.md": self.data_manager.create_sample_file(
                "sample.md", "# Sample Document\n\nThis is a sample markdown document."
            ),
            "sample.html": self.data_manager.create_sample_file(
                "sample.html",
                "<html><body><h1>Sample Document</h1><p>This is a sample HTML document.</p></body></html>",
            ),
            "sample.json": self.data_manager.create_sample_file(
                "sample.json",
                json.dumps({"title": "Sample Document", "content": "Test content"}),
            ),
            "config.yaml": self.data_manager.create_sample_file(
                "config.yaml", "test: value\nversion: 1.0"
            ),
        }


class ComponentTestCase(BaseTestCase):
    """Base class for testing individual components"""

    def setup_method(self):
        """Setup for component tests"""
        self.component = None
        self.component_config = {}

    def create_mock_component(self, component_methods: List[str]):
        """Create mock component with specified methods"""
        mock_component = Mock()
        for method in component_methods:
            setattr(mock_component, method, Mock())
        return mock_component

    def assert_component_response(self, response, expected_keys: List[str]):
        """Assert component response contains expected keys"""
        if isinstance(response, dict):
            for key in expected_keys:
                assert key in response, f"Response missing key: {key}"
        else:
            pytest.fail("Response is not a dictionary")


class APITestCase(BaseTestCase):
    """Base class for API testing"""

    @pytest.fixture(autouse=True)
    def setup_api_environment(self, api_client):
        """Setup API testing environment"""
        self.api_client = api_client
        self.base_url = "/api/v1"
        self.api_key = "test-api-key-2025"

        # Set default headers
        self.headers = {"Content-Type": "application/json", "X-API-Key": self.api_key}

    def post(
        self, endpoint: str, data: Dict[str, Any], expected_status: int = 200
    ) -> Tuple[int, Dict[str, Any]]:
        """Make POST request to API"""
        response = self.api_client.post(
            f"{self.base_url}{endpoint}", json=data, headers=self.headers
        )

        try:
            response_data = response.get_json()
        except:
            response_data = {}

        return response.status_code, response_data

    def get(
        self,
        endpoint: str,
        expected_status: int = 200,
        params: Optional[Dict[str, Any]] = None,
    ) -> Tuple[int, Dict[str, Any]]:
        """Make GET request to API"""
        response = self.api_client.get(
            f"{self.base_url}{endpoint}", headers=self.headers, query_string=params
        )

        try:
            response_data = response.get_json()
        except:
            response_data = {}

        return response.status_code, response_data

    def delete(
        self, endpoint: str, expected_status: int = 200
    ) -> Tuple[int, Dict[str, Any]]:
        """Make DELETE request to API"""
        response = self.api_client.delete(
            f"{self.base_url}{endpoint}", headers=self.headers
        )

        try:
            response_data = response.get_json()
        except:
            response_data = {}

        return response.status_code, response_data

    def assert_api_response(
        self,
        status_code: int,
        response_data: Dict[str, Any],
        expected_status: int = 200,
        expected_keys: Optional[List[str]] = None,
    ):
        """Assert API response has expected status and structure"""
        assert status_code == expected_status, (
            f"Expected status {expected_status}, got {status_code}"
        )

        if expected_keys:
            for key in expected_keys:
                assert key in response_data, f"Response missing key: {key}"

    def create_test_job(
        self, job_type: str, params: Dict[str, Any]
    ) -> Tuple[str, Dict[str, Any]]:
        """Create a test job and return job ID"""
        endpoint_map = {
            "convert": "/convert",
            "scrape": "/scrape",
            "tei": "/tei",
            "uml": "/uml",
            "pipeline": "/pipeline",
        }

        endpoint = endpoint_map.get(job_type)
        if not endpoint:
            pytest.fail(f"Unknown job type: {job_type}")

        status, response = self.post(endpoint, params, expected_status=202)

        assert "job_id" in response, "Response missing job_id"
        return response["job_id"], response


class WebTestCase(BaseTestCase):
    """Base class for web interface testing"""

    @pytest.fixture(autouse=True)
    def setup_web_environment(self, web_client):
        """Setup web testing environment"""
        self.web_client = web_client

    def get_page(self, endpoint: str, expected_status: int = 200) -> Tuple[int, str]:
        """Get a web page"""
        response = self.web_client.get(endpoint)
        return response.status_code, response.get_data(as_text=True)

    def post_form(
        self, endpoint: str, data: Dict[str, Any], expected_status: int = 200
    ) -> Tuple[int, str]:
        """Submit a form"""
        response = self.web_client.post(endpoint, data=data)
        return response.status_code, response.get_data(as_text=True)

    def assert_page_contains(self, content: str, text: str):
        """Assert page contains specific text"""
        assert text in content, f"Page does not contain expected text: {text}"

    def assert_page_title(self, content: str, title: str):
        """Assert page has correct title"""
        assert (
            f"<title>{title}</title>" in content or f"title>{title}</title>" in content
        )


class SeleniumTestCase(BaseTestCase):
    """Base class for Selenium WebDriver testing"""

    @pytest.fixture(autouse=True)
    def setup_selenium_environment(self):
        """Setup Selenium testing environment"""
        self.driver_manager = WebDriverManager(headless=True)
        self.driver = None

        yield

        if self.driver:
            self.driver.quit()
        self.driver_manager.cleanup()

    def setup_driver(self, browser: str = "chrome"):
        """Setup WebDriver for specified browser"""
        if browser == "chrome":
            self.driver = self.driver_manager.get_chrome_driver()
        elif browser == "firefox":
            self.driver = self.driver_manager.get_firefox_driver()
        else:
            pytest.fail(f"Unsupported browser: {browser}")

    def navigate_to(self, url: str):
        """Navigate to URL"""
        if not self.driver:
            self.setup_driver()
        self.driver.get(url)

    def find_element(self, locator: Tuple[str, str], timeout: int = 10):
        """Find element with wait"""
        return self.driver_manager.wait_for_element(self.driver, locator, timeout)

    def find_elements(self, locator: Tuple[str, str]):
        """Find multiple elements"""
        return self.driver.find_elements(*locator)

    def click_element(self, locator: Tuple[str, str], timeout: int = 10):
        """Click element with wait"""
        element = self.driver_manager.wait_for_clickable(self.driver, locator, timeout)
        element.click()

    def type_text(self, locator: Tuple[str, str], text: str, timeout: int = 10):
        """Type text into element"""
        element = self.find_element(locator, timeout)
        element.clear()
        element.send_keys(text)

    def wait_for_text(self, locator: Tuple[str, str], text: str, timeout: int = 10):
        """Wait for text to appear in element"""
        return self.driver_manager.wait_for_text_in_element(
            self.driver, locator, text, timeout
        )

    def assert_element_present(self, locator: Tuple[str, str]):
        """Assert element is present on page"""
        element = self.find_element(locator)
        assert element is not None, f"Element {locator} not found"

    def assert_text_present(self, text: str):
        """Assert text is present on page"""
        assert text in self.driver.page_source, f"Text '{text}' not found in page"


class PerformanceTestCase(BaseTestCase):
    """Base class for performance testing"""

    @pytest.fixture(autouse=True)
    def setup_performance_environment(self):
        """Setup performance testing environment"""
        self.performance_monitor = PerformanceMonitor()

        yield

        # Cleanup
        self.performance_monitor.stop_monitoring()

    def start_performance_monitoring(self):
        """Start performance monitoring"""
        self.performance_monitor.start_monitoring()

    def stop_performance_monitoring(self) -> Dict[str, Any]:
        """Stop performance monitoring and get metrics"""
        self.performance_monitor.stop_monitoring()
        return self.performance_monitor.get_metrics()

    def assert_performance_within(
        self,
        metrics: Dict[str, Any],
        max_duration: Optional[float] = None,
        max_memory_mb: Optional[float] = None,
        max_cpu_percent: Optional[float] = None,
    ):
        """Assert performance metrics are within limits"""
        if max_duration and metrics["duration_seconds"] > max_duration:
            pytest.fail(
                f"Duration {metrics['duration_seconds']}s exceeds limit {max_duration}s"
            )

        if max_memory_mb and metrics["peak_memory_mb"] > max_memory_mb:
            pytest.fail(
                f"Memory {metrics['peak_memory_mb']}MB exceeds limit {max_memory_mb}MB"
            )

        if max_cpu_percent and metrics["average_cpu_percent"] > max_cpu_percent:
            pytest.fail(
                f"CPU {metrics['average_cpu_percent']}% exceeds limit {max_cpu_percent}%"
            )


class IntegrationTestCase(BaseTestCase):
    """Base class for integration testing"""

    @pytest.fixture(autouse=True)
    def setup_integration_environment(self):
        """Setup integration testing environment"""
        self.services = {}
        self.background_threads = []

        yield

        # Cleanup services
        self.cleanup_services()

    def start_service(self, name: str, service_func: callable, *args, **kwargs):
        """Start a background service"""
        if name in self.services:
            pytest.fail(f"Service {name} already running")

        thread = threading.Thread(
            target=service_func, args=args, kwargs=kwargs, daemon=True
        )
        thread.start()
        self.background_threads.append(thread)
        self.services[name] = {"thread": thread, "function": service_func}

    def stop_service(self, name: str):
        """Stop a background service"""
        if name not in self.services:
            return

        # Note: Actual stopping depends on the service implementation
        # This is a placeholder for service cleanup logic
        del self.services[name]

    def cleanup_services(self):
        """Cleanup all background services"""
        for name in list(self.services.keys()):
            self.stop_service(name)

        # Wait for threads to finish
        for thread in self.background_threads:
            if thread.is_alive():
                thread.join(timeout=5)


class AsyncTestCase(BaseTestCase):
    """Base class for async testing"""

    @pytest.fixture(autouse=True)
    def setup_async_environment(self):
        """Setup async testing environment"""
        self.async_helper = AsyncTestHelper()

    def run_async(self, coro, timeout: float = 30.0):
        """Run async coroutine"""
        return self.async_helper.run_async_in_sync(coro)

    def run_async_with_timeout(self, coro, timeout_seconds: float):
        """Run async coroutine with timeout"""
        return self.run_async(self.async_helper.run_with_timeout(coro, timeout_seconds))


class SecurityTestCase(BaseTestCase):
    """Base class for security testing"""

    def assert_no_xss_vulnerability(self, content: str):
        """Assert content doesn't contain XSS vulnerabilities"""
        xss_patterns = [
            "<script",
            "javascript:",
            "onerror=",
            "onload=",
            "onclick=",
            "onmouseover=",
            "<iframe",
            "<object",
            "<embed",
            "vbscript:",
            "data:text/html",
        ]

        content_lower = content.lower()
        for pattern in xss_patterns:
            assert pattern not in content_lower, (
                f"Potential XSS vulnerability found: {pattern}"
            )

    def assert_no_sql_injection(self, query: str):
        """Assert query doesn't contain SQL injection patterns"""
        sql_patterns = [
            "drop table",
            "delete from",
            "insert into",
            "update set",
            "union select",
            "exec(",
            "xp_",
            "sp_",
            "--",
            "/*",
            "*/",
            "' or '1'='1",
            "' or 1=1",
            '" or "1"="1',
            "1=1 --",
        ]

        query_lower = query.lower()
        for pattern in sql_patterns:
            assert pattern not in query_lower, (
                f"Potential SQL injection found: {pattern}"
            )

    def assert_secure_headers(self, headers: Dict[str, str]):
        """Assert HTTP headers contain security headers"""
        security_headers = [
            "x-frame-options",
            "x-content-type-options",
            "x-xss-protection",
            "strict-transport-security",
            "content-security-policy",
        ]

        for header in security_headers:
            assert header in headers, f"Missing security header: {header}"


class MockFactory:
    """Factory for creating mock objects"""

    @staticmethod
    def create_mock_webdriver():
        """Create mock WebDriver"""
        driver = Mock()
        driver.get = Mock()
        driver.quit = Mock()
        driver.current_url = "https://example.com"
        driver.title = "Example Page"
        driver.page_source = "<html><body>Test content</body></html>"
        driver.find_element = Mock(return_value=Mock())
        driver.find_elements = Mock(return_value=[Mock()])
        driver.execute_script = Mock(return_value=None)
        driver.save_screenshot = Mock(return_value=True)
        driver.add_cookie = Mock()
        driver.get_cookies = Mock(return_value=[{"name": "test", "value": "value"}])
        driver.delete_all_cookies = Mock()
        return driver

    @staticmethod
    def create_mock_response(
        status_code: int = 200,
        data: Optional[Dict[str, Any]] = None,
        text: Optional[str] = None,
    ):
        """Create mock HTTP response"""
        response = Mock()
        response.status_code = status_code
        response.json.return_value = data or {}
        response.text = text or str(data or {})
        response.headers = {"content-type": "application/json"}
        return response

    @staticmethod
    def create_mock_job(job_id: str, job_type: str, status: str = "completed"):
        """Create mock job object"""
        job = Mock()
        job.job_id = job_id
        job.job_type = job_type
        job.status = status
        job.progress = 100 if status == "completed" else 50
        job.result = {"output": "test_output"} if status == "completed" else None
        job.error = None if status != "failed" else "Test error"
        job.start_time = "2024-01-01T00:00:00"
        job.end_time = (
            "2024-01-01T00:01:00" if status in ["completed", "failed"] else None
        )
        return job

    @staticmethod
    def create_mock_validator():
        """Create mock validator"""
        validator = Mock()
        validator.validate = Mock(return_value={"valid": True, "errors": []})
        validator.validate_content = Mock(return_value=True)
        validator.validate_structure = Mock(return_value=True)
        return validator
