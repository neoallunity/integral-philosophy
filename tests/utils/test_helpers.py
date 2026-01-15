"""
Common test utilities and helper functions for Integral Philosophy Publishing System tests
"""

import asyncio
import tempfile
import time
import json
import hashlib
import os
import sys
import threading
import subprocess
import uuid
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple, Union, Callable
from unittest.mock import Mock, MagicMock, patch
from datetime import datetime, timedelta
import requests
import psutil
import pytest

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.chrome.options import Options as ChromeOptions
from selenium.webdriver.firefox.options import Options as FirefoxOptions
from selenium.common.exceptions import TimeoutException, WebDriverException


class TestDataManager:
    """Manages test data generation and cleanup"""

    def __init__(self, base_dir: Optional[Path] = None):
        self.base_dir = base_dir or Path(tempfile.mkdtemp(prefix="integral_test_data_"))
        self.base_dir.mkdir(parents=True, exist_ok=True)
        self.created_files = []
        self.created_dirs = []

    def create_sample_file(
        self, filename: str, content: str, encoding: str = "utf-8"
    ) -> Path:
        """Create a sample file with given content"""
        file_path = self.base_dir / filename
        file_path.write_text(content, encoding=encoding)
        self.created_files.append(file_path)
        return file_path

    def create_sample_binary(self, filename: str, content: bytes) -> Path:
        """Create a sample binary file"""
        file_path = self.base_dir / filename
        file_path.write_bytes(content)
        self.created_files.append(file_path)
        return file_path

    def create_directory(self, dirname: str) -> Path:
        """Create a test directory"""
        dir_path = self.base_dir / dirname
        dir_path.mkdir(parents=True, exist_ok=True)
        self.created_dirs.append(dir_path)
        return dir_path

    def create_file_structure(self, structure: Dict[str, Any]) -> Dict[str, Path]:
        """Create a complex file structure from nested dictionary"""
        created = {}

        def _create_structure(data: Dict[str, Any], base_path: Path):
            for name, content in data.items():
                path = base_path / name

                if isinstance(content, dict):
                    path.mkdir(parents=True, exist_ok=True)
                    self.created_dirs.append(path)
                    _create_structure(content, path)
                else:
                    if isinstance(content, bytes):
                        path.write_bytes(content)
                    else:
                        path.write_text(str(content), encoding="utf-8")
                    self.created_files.append(path)
                    created[name] = path

        _create_structure(structure, self.base_dir)
        return created

    def get_file_hash(self, file_path: Path) -> str:
        """Get SHA256 hash of a file"""
        hash_sha256 = hashlib.sha256()
        with open(file_path, "rb") as f:
            for chunk in iter(lambda: f.read(4096), b""):
                hash_sha256.update(chunk)
        return hash_sha256.hexdigest()

    def cleanup(self):
        """Clean up all created files and directories"""
        for file_path in self.created_files:
            if file_path.exists():
                try:
                    file_path.unlink()
                except OSError:
                    pass

        for dir_path in self.created_dirs:
            if dir_path.exists():
                try:
                    dir_path.rmdir()
                except OSError:
                    # Directory not empty, use shutil
                    import shutil

                    shutil.rmtree(dir_path, ignore_errors=True)

        if self.base_dir.exists():
            import shutil

            shutil.rmtree(self.base_dir, ignore_errors=True)


class WebDriverManager:
    """Manages Selenium WebDriver instances for testing"""

    def __init__(self, headless: bool = True):
        self.headless = headless
        self.drivers = []
        self.default_wait_timeout = 10

    def get_chrome_driver(
        self, options: Optional[ChromeOptions] = None
    ) -> webdriver.Chrome:
        """Create Chrome WebDriver instance"""
        if options is None:
            options = ChromeOptions()
            if self.headless:
                options.add_argument("--headless")
            options.add_argument("--no-sandbox")
            options.add_argument("--disable-dev-shm-usage")
            options.add_argument("--disable-gpu")
            options.add_argument("--window-size=1920,1080")
            options.add_argument("--disable-extensions")
            options.add_argument("--disable-plugins")
            options.add_argument("--disable-images")

        try:
            driver = webdriver.Chrome(options=options)
            driver.set_page_load_timeout(30)
            self.drivers.append(driver)
            return driver
        except WebDriverException as e:
            pytest.skip(f"Chrome WebDriver not available: {e}")

    def get_firefox_driver(
        self, options: Optional[FirefoxOptions] = None
    ) -> webdriver.Firefox:
        """Create Firefox WebDriver instance"""
        if options is None:
            options = FirefoxOptions()
            if self.headless:
                options.add_argument("--headless")
            options.add_argument("--width=1920")
            options.add_argument("--height=1080")

        try:
            driver = webdriver.Firefox(options=options)
            driver.set_page_load_timeout(30)
            self.drivers.append(driver)
            return driver
        except WebDriverException as e:
            pytest.skip(f"Firefox WebDriver not available: {e}")

    def wait_for_element(
        self,
        driver: webdriver.Remote,
        locator: Tuple[str, str],
        timeout: Optional[int] = None,
    ) -> Any:
        """Wait for element to be present and return it"""
        timeout = timeout or self.default_wait_timeout
        try:
            return WebDriverWait(driver, timeout).until(
                EC.presence_of_element_located(locator)
            )
        except TimeoutException:
            pytest.fail(f"Element {locator} not found within {timeout} seconds")

    def wait_for_clickable(
        self,
        driver: webdriver.Remote,
        locator: Tuple[str, str],
        timeout: Optional[int] = None,
    ) -> Any:
        """Wait for element to be clickable and return it"""
        timeout = timeout or self.default_wait_timeout
        try:
            return WebDriverWait(driver, timeout).until(
                EC.element_to_be_clickable(locator)
            )
        except TimeoutException:
            pytest.fail(f"Element {locator} not clickable within {timeout} seconds")

    def wait_for_text_in_element(
        self,
        driver: webdriver.Remote,
        locator: Tuple[str, str],
        text: str,
        timeout: Optional[int] = None,
    ) -> bool:
        """Wait for specific text to appear in element"""
        timeout = timeout or self.default_wait_timeout
        try:
            return WebDriverWait(driver, timeout).until(
                EC.text_to_be_present_in_element(locator, text)
            )
        except TimeoutException:
            return False

    def cleanup(self):
        """Close all managed drivers"""
        for driver in self.drivers:
            try:
                driver.quit()
            except:
                pass
        self.drivers.clear()


class MockServer:
    """Mock HTTP server for testing API endpoints"""

    def __init__(self, host: str = "localhost", port: int = 8080):
        self.host = host
        self.port = port
        self.server = None
        self.thread = None
        self.responses = {}
        self.requests = []

    def add_response(
        self,
        path: str,
        response_data: Any,
        status_code: int = 200,
        headers: Optional[Dict[str, str]] = None,
    ):
        """Add a mock response for a specific path"""
        self.responses[path] = {
            "data": response_data,
            "status_code": status_code,
            "headers": headers or {},
        }

    def handle_request(self, path: str, method: str, data: Any = None):
        """Handle incoming request"""
        self.requests.append(
            {
                "path": path,
                "method": method,
                "data": data,
                "timestamp": datetime.now().isoformat(),
            }
        )

        if path in self.responses:
            response = self.responses[path]
            return response["data"], response["status_code"], response["headers"]
        else:
            return {"error": "Not found"}, 404, {}

    def start(self):
        """Start the mock server in a background thread"""
        from flask import Flask, request, jsonify

        app = Flask(__name__)

        @app.route("/<path:path>", methods=["GET", "POST", "PUT", "DELETE"])
        def handle_path(path):
            data = request.get_json() if request.is_json else request.form.to_dict()
            response_data, status_code, headers = self.handle_request(
                path, request.method, data
            )
            response = jsonify(response_data)
            response.status_code = status_code
            for key, value in headers.items():
                response.headers[key] = value
            return response

        def run_server():
            app.run(host=self.host, port=self.port, debug=False, use_reloader=False)

        self.thread = threading.Thread(target=run_server, daemon=True)
        self.thread.start()
        time.sleep(0.1)  # Give server time to start

    def stop(self):
        """Stop the mock server"""
        if self.server:
            self.server.shutdown()
        if self.thread:
            self.thread.join(timeout=1)

    def get_requests(self, path: Optional[str] = None) -> List[Dict[str, Any]]:
        """Get recorded requests, optionally filtered by path"""
        if path:
            return [req for req in self.requests if req["path"] == path]
        return self.requests.copy()

    def clear_requests(self):
        """Clear recorded requests"""
        self.requests.clear()


class PerformanceMonitor:
    """Monitor performance metrics during tests"""

    def __init__(self):
        self.start_time = None
        self.end_time = None
        self.peak_memory = 0
        self.cpu_samples = []
        self.monitoring = False
        self.monitor_thread = None

    def start_monitoring(self, process: Optional[Any] = None):
        """Start performance monitoring"""
        self.start_time = time.time()
        self.monitoring = True
        self.peak_memory = 0
        self.cpu_samples = []

        target_process = process or psutil.Process()

        def monitor():
            while self.monitoring:
                try:
                    memory_info = target_process.memory_info()
                    current_memory = memory_info.rss / 1024 / 1024  # MB
                    self.peak_memory = max(self.peak_memory, current_memory)

                    cpu_percent = target_process.cpu_percent()
                    self.cpu_samples.append(cpu_percent)

                    time.sleep(0.1)
                except (psutil.NoSuchProcess, psutil.AccessDenied):
                    break

        self.monitor_thread = threading.Thread(target=monitor, daemon=True)
        self.monitor_thread.start()

    def stop_monitoring(self):
        """Stop performance monitoring"""
        self.monitoring = False
        self.end_time = time.time()

        if self.monitor_thread:
            self.monitor_thread.join(timeout=1)

    def get_metrics(self) -> Dict[str, Any]:
        """Get collected performance metrics"""
        duration = (self.end_time or time.time()) - (self.start_time or time.time())
        avg_cpu = (
            sum(self.cpu_samples) / len(self.cpu_samples) if self.cpu_samples else 0
        )

        return {
            "duration_seconds": duration,
            "peak_memory_mb": self.peak_memory,
            "average_cpu_percent": avg_cpu,
            "cpu_samples_count": len(self.cpu_samples),
        }


class AsyncTestHelper:
    """Helper utilities for async testing"""

    @staticmethod
    async def run_with_timeout(coro, timeout_seconds: float):
        """Run coroutine with timeout"""
        try:
            return await asyncio.wait_for(coro, timeout=timeout_seconds)
        except asyncio.TimeoutError:
            pytest.fail(f"Coroutine timed out after {timeout_seconds} seconds")

    @staticmethod
    async def gather_with_exceptions(*coros):
        """Gather coroutines and capture exceptions"""
        results = await asyncio.gather(*coros, return_exceptions=True)
        return results

    @staticmethod
    def run_async_in_sync(coro):
        """Run async coroutine in sync context"""
        try:
            loop = asyncio.get_event_loop()
        except RuntimeError:
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
        return loop.run_until_complete(coro)


class FileComparator:
    """Compare files and directories for testing"""

    @staticmethod
    def compare_files(
        file1: Path, file2: Path, ignore_whitespace: bool = False
    ) -> bool:
        """Compare two text files"""
        if not file1.exists() or not file2.exists():
            return False

        if ignore_whitespace:
            content1 = file1.read_text(encoding="utf-8").strip()
            content2 = file2.read_text(encoding="utf-8").strip()
            return content1 == content2
        else:
            return file1.read_bytes() == file2.read_bytes()

    @staticmethod
    def compare_directories(
        dir1: Path, dir2: Path, ignore_files: Optional[List[str]] = None
    ) -> Dict[str, Any]:
        """Compare two directories and return differences"""
        ignore_files = ignore_files or []
        results = {
            "identical": True,
            "only_in_dir1": [],
            "only_in_dir2": [],
            "different_files": [],
        }

        files1 = {f.relative_to(dir1) for f in dir1.rglob("*") if f.is_file()}
        files2 = {f.relative_to(dir2) for f in dir2.rglob("*") if f.is_file()}

        # Remove ignored files
        files1 = {f for f in files1 if str(f) not in ignore_files}
        files2 = {f for f in files2 if str(f) not in ignore_files}

        # Find files only in one directory
        results["only_in_dir1"] = list(files1 - files2)
        results["only_in_dir2"] = list(files2 - files1)

        # Compare common files
        common_files = files1 & files2
        for file_path in common_files:
            file1 = dir1 / file_path
            file2 = dir2 / file_path
            if not FileComparator.compare_files(file1, file2):
                results["different_files"].append(str(file_path))

        results["identical"] = (
            not results["only_in_dir1"]
            and not results["only_in_dir2"]
            and not results["different_files"]
        )

        return results


class HTTPTestHelper:
    """Helper utilities for HTTP testing"""

    @staticmethod
    def create_test_session() -> requests.Session:
        """Create test HTTP session with reasonable defaults"""
        session = requests.Session()
        session.timeout = 30
        session.headers.update({"User-Agent": "Integral-Philosophy-Test/1.0"})
        return session

    @staticmethod
    def wait_for_server(url: str, timeout_seconds: int = 30) -> bool:
        """Wait for server to be available"""
        start_time = time.time()
        while time.time() - start_time < timeout_seconds:
            try:
                response = requests.get(url, timeout=5)
                return response.status_code < 500
            except requests.exceptions.RequestException:
                time.sleep(0.5)
        return False


class SystemTestHelper:
    """Helper utilities for system-level testing"""

    @staticmethod
    def get_available_port() -> int:
        """Get an available port for testing"""
        import socket

        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.bind(("", 0))
            s.listen(1)
            port = s.getsockname()[1]
        return port

    @staticmethod
    def run_command(
        cmd: List[str], timeout_seconds: int = 30, cwd: Optional[Path] = None
    ) -> Tuple[int, str, str]:
        """Run command and return exit code, stdout, stderr"""
        try:
            result = subprocess.run(
                cmd, timeout=timeout_seconds, capture_output=True, text=True, cwd=cwd
            )
            return result.returncode, result.stdout, result.stderr
        except subprocess.TimeoutExpired:
            return -1, "", "Command timed out"

    @staticmethod
    def kill_process_by_name(process_name: str):
        """Kill processes by name"""
        for proc in psutil.process_iter(["pid", "name"]):
            try:
                if proc.info["name"] == process_name:
                    proc.kill()
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                pass


# Utility functions for common test patterns
def assert_valid_xml(content: str):
    """Assert that content is valid XML"""
    import xml.etree.ElementTree as ET

    try:
        ET.fromstring(content)
    except ET.ParseError as e:
        pytest.fail(f"Invalid XML: {e}")


def assert_valid_json(content: str):
    """Assert that content is valid JSON"""
    try:
        json.loads(content)
    except json.JSONDecodeError as e:
        pytest.fail(f"Invalid JSON: {e}")


def create_random_text(length: int = 100) -> str:
    """Create random text for testing"""
    import random
    import string

    return "".join(
        random.choices(string.ascii_letters + string.digits + " \n", k=length)
    )


def create_uuid_string() -> str:
    """Create UUID string for testing"""
    return str(uuid.uuid4())


def get_current_timestamp() -> str:
    """Get current timestamp in ISO format"""
    return datetime.now().isoformat()


def assert_files_equal(file1: Path, file2: Path, encoding: str = "utf-8"):
    """Assert two files have identical content"""
    if not file1.exists():
        pytest.fail(f"File {file1} does not exist")
    if not file2.exists():
        pytest.fail(f"File {file2} does not exist")

    content1 = file1.read_text(encoding=encoding)
    content2 = file2.read_text(encoding=encoding)

    assert content1 == content2, f"Files {file1} and {file2} differ"
