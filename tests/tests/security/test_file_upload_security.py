"""
Security tests for file upload vulnerabilities
Testing malicious file detection, DoS prevention, file type validation, and path traversal attacks
"""

import pytest
import os
import tempfile
import magic
from pathlib import Path
from typing import Dict, Any, List, Optional, Union
from unittest.mock import Mock, patch, MagicMock

from tests.utils.base_test_classes import SecurityTestCase


class TestFileUploadSecurity(SecurityTestCase):
    """Test file upload security vulnerabilities"""

    @pytest.fixture(autouse=True)
    def setup_file_upload_security(self):
        """Setup file upload security testing environment"""
        # Use parent class temp_dir setup
        if hasattr(self, "temp_dir"):
            self.upload_dir = self.temp_dir / "uploads"
        else:
            import tempfile

            self.upload_dir = Path(tempfile.mkdtemp()) / "uploads"
        self.upload_dir.mkdir(exist_ok=True)

        # Mock file upload handler
        self.file_handler = Mock()
        self.file_handler.allowed_extensions = {
            ".txt",
            ".md",
            ".html",
            ".xml",
            ".json",
            ".pdf",
        }
        self.file_handler.max_file_size = 10 * 1024 * 1024  # 10MB
        self.file_handler.upload_dir = str(self.upload_dir)

        # Security scanner mock
        self.security_scanner = Mock()
        self.security_scanner.scan_file = Mock(side_effect=self.mock_scan_file)

        yield

        # Cleanup uploaded files
        self.cleanup_uploaded_files()

    def cleanup_uploaded_files(self):
        """Clean up uploaded test files"""
        for file_path in self.upload_dir.glob("*"):
            if file_path.is_file():
                file_path.unlink()

    def mock_scan_file(self, file_path: str) -> Dict[str, Any]:
        """Mock security scanner for files"""
        path = Path(file_path)

        # Check for malicious patterns
        malicious_patterns = [
            b"<script",
            b"javascript:",
            b"eval(",
            b"document.write",
            b"alert(",
            b"shell_exec",
            b"system(",
            b"exec(",
            b"__import__",
            b"subprocess",
            b"os.system",
            b"<?php",
            b"<%",
            b"vbscript:",
            b"data:text/html",
        ]

        try:
            with open(path, "rb") as f:
                content = f.read()

            for pattern in malicious_patterns:
                if pattern in content.lower():
                    return {
                        "safe": False,
                        "threat_type": "malicious_content",
                        "detected_pattern": pattern.decode("utf-8", errors="ignore"),
                        "severity": "high",
                    }

            # Check file size
            if path.stat().st_size > self.file_handler.max_file_size:
                return {
                    "safe": False,
                    "threat_type": "file_too_large",
                    "size": path.stat().st_size,
                    "max_size": self.file_handler.max_file_size,
                    "severity": "medium",
                }

            # Check extension
            if path.suffix.lower() not in self.file_handler.allowed_extensions:
                return {
                    "safe": False,
                    "threat_type": "invalid_extension",
                    "extension": path.suffix,
                    "severity": "medium",
                }

            return {"safe": True, "threat_type": None, "severity": "none"}

        except Exception as e:
            return {
                "safe": False,
                "threat_type": "scan_error",
                "error": str(e),
                "severity": "low",
            }

    def create_malicious_file(self, filename: str, content: Union[str, bytes]) -> Path:
        """Create a malicious test file"""
        file_path = self.upload_dir / filename

        if isinstance(content, str):
            file_path.write_text(content)
        else:
            file_path.write_bytes(content)

        return file_path

    def test_malicious_script_detection(self):
        """Test detection of malicious scripts in uploaded files"""
        # Test cases with malicious content
        malicious_files = [
            (
                "malicious.html",
                "<html><body><script>alert('XSS')</script></body></html>",
            ),
            ("malicious.js", "javascript:alert('XSS attack')"),
            ("malicious.php", "<?php system($_GET['cmd']); ?>"),
            ("malicious.asp", '<% Response.Write(eval(Request("cmd"))) %>'),
            ("malicious.txt", "eval('malicious code execution')"),
            ("malicious.xml", "<?xml version='1.0'?><script>alert('XSS')</script>"),
        ]

        for filename, content in malicious_files:
            file_path = self.create_malicious_file(filename, content)

            # Scan file for security threats
            scan_result = self.security_scanner.scan_file(str(file_path))

            assert scan_result["safe"] is False, (
                f"Malicious file {filename} was not detected"
            )
            assert scan_result["threat_type"] == "malicious_content"
            assert scan_result["severity"] == "high"

    def test_file_size_limits_and_dos_prevention(self):
        """Test file size limits and DoS prevention"""
        # Test normal sized file
        normal_content = "A" * 1024  # 1KB
        normal_file = self.create_malicious_file("normal.txt", normal_content)

        scan_result = self.security_scanner.scan_file(str(normal_file))
        assert scan_result["safe"] is True

        # Test oversized file
        oversized_content = "B" * (self.file_handler.max_file_size + 1024)  # Over limit
        oversized_file = self.create_malicious_file("oversized.txt", oversized_content)

        scan_result = self.security_scanner.scan_file(str(oversized_file))
        assert scan_result["safe"] is False
        assert scan_result["threat_type"] == "file_too_large"
        assert scan_result["size"] > self.file_handler.max_file_size

        # Test zero-byte file (edge case)
        zero_file = self.create_malicious_file("zero.txt", "")
        scan_result = self.security_scanner.scan_file(str(zero_file))
        assert scan_result["safe"] is True  # Zero-byte files are generally safe

    def test_file_type_validation_and_spoofing(self):
        """Test file type validation and extension spoofing detection"""
        # Test valid file types
        valid_files = [
            ("document.txt", "Plain text document"),
            ("article.md", "# Markdown Article"),
            ("page.html", "<html><body>Valid HTML</body></html>"),
            ("data.xml", '<?xml version="1.0"?><data>Valid XML</data>'),
            ("config.json", '{"key": "value"}'),
        ]

        for filename, content in valid_files:
            file_path = self.create_malicious_file(filename, content)
            scan_result = self.security_scanner.scan_file(str(file_path))
            assert scan_result["safe"] is True, (
                f"Valid file {filename} was incorrectly flagged"
            )

        # Test invalid file extensions
        invalid_files = [
            ("malicious.exe", "Fake executable content"),
            ("script.bat", "@echo off"),
            ("shell.sh", "#!/bin/bash\necho 'test'"),
            ("binary.dll", "Binary content"),
        ]

        for filename, content in invalid_files:
            file_path = self.create_malicious_file(filename, content)
            scan_result = self.security_scanner.scan_file(str(file_path))
            assert scan_result["safe"] is False, (
                f"Invalid file {filename} was not detected"
            )
            assert scan_result["threat_type"] == "invalid_extension"

        # Test extension spoofing (malicious content with valid extension)
        spoofed_content = "<script>alert('XSS')</script>"
        spoofed_file = self.create_malicious_file("innocent.txt", spoofed_content)

        scan_result = self.security_scanner.scan_file(str(spoofed_file))
        # Should detect malicious content despite valid extension
        assert scan_result["safe"] is False
        assert scan_result["threat_type"] == "malicious_content"

    def test_path_traversal_attacks(self):
        """Test path traversal attack prevention"""

        # Mock file upload handler that checks for path traversal
        def secure_upload_handler(filename: str, content: str) -> Dict[str, Any]:
            """Secure file upload handler that prevents path traversal"""
            # Normalize path and check for traversal
            normalized_path = os.path.normpath(filename)

            # Check for path traversal patterns
            traversal_patterns = ["..", "\\", "/", "%2e%2e", "%2f", "%5c"]
            for pattern in traversal_patterns:
                if pattern in normalized_path.lower():
                    return {
                        "success": False,
                        "error": "Path traversal detected",
                        "pattern": pattern,
                    }

            # Ensure filename doesn't start with / or \
            if normalized_path.startswith(("/", "\\")):
                return {"success": False, "error": "Absolute path not allowed"}

            # If passes checks, create the file
            safe_filename = os.path.basename(normalized_path)
            file_path = self.upload_dir / safe_filename
            file_path.write_text(content)

            return {"success": True, "filepath": str(file_path)}

        # Test path traversal attempts
        traversal_attempts = [
            "../../../etc/passwd",
            "..\\..\\..\\windows\\system32\\config\\sam",
            "%2e%2e%2f%2e%2e%2fetc%2fpasswd",
            "/etc/shadow",
            "\\windows\\system32\\drivers\\etc\\hosts",
            "....//....//....//etc/passwd",
            "..%252f..%252f..%252fetc%252fpasswd",
        ]

        for malicious_filename in traversal_attempts:
            result = secure_upload_handler(malicious_filename, "test content")
            assert result["success"] is False, (
                f"Path traversal attack {malicious_filename} was not blocked"
            )
            assert "Path traversal detected" in result.get(
                "error", ""
            ) or "Absolute path not allowed" in result.get("error", "")

        # Test legitimate filenames
        legitimate_names = [
            "document.txt",
            "article.md",
            "data.json",
            "report.html",
            "file_with_underscores.xml",
            "file-with-dashes.txt",
            "file123.txt",
        ]

        for legitimate_name in legitimate_names:
            result = secure_upload_handler(legitimate_name, "legitimate content")
            assert result["success"] is True, (
                f"Legitimate filename {legitimate_name} was incorrectly blocked"
            )
            assert Path(result["filepath"]).exists()

    def test_temporary_file_security(self):
        """Test temporary file security during upload process"""
        # Test temporary file creation and cleanup
        temp_files_created = []

        def secure_temp_upload(
            content: bytes, original_filename: str
        ) -> Dict[str, Any]:
            """Secure temporary file upload with proper cleanup"""
            # Create temporary file with random name
            import uuid

            temp_id = str(uuid.uuid4())
            temp_filename = (
                f"temp_{temp_id}.txt"  # Use .txt extension for allowed file type
            )
            temp_path = self.upload_dir / temp_filename

            try:
                # Write content to temporary file
                temp_path.write_bytes(content)
                temp_files_created.append(temp_path)

                # Validate file in temporary location
                scan_result = self.security_scanner.scan_file(str(temp_path))

                if not scan_result["safe"]:
                    # Clean up malicious temporary file
                    temp_path.unlink()
                    return {
                        "success": False,
                        "error": f"File rejected: {scan_result['threat_type']}",
                    }

                # Move to permanent location with safe name
                safe_filename = sanitize_filename(original_filename)
                permanent_path = self.upload_dir / safe_filename
                temp_path.rename(permanent_path)

                return {"success": True, "filepath": str(permanent_path)}

            except Exception as e:
                # Ensure cleanup on error
                if temp_path.exists():
                    temp_path.unlink()
                return {"success": False, "error": f"Upload failed: {str(e)}"}

        def sanitize_filename(filename: str) -> str:
            """Sanitize filename for secure storage"""
            import re

            # Remove path components
            filename = os.path.basename(filename)
            # Remove dangerous characters
            filename = re.sub(r'[<>:"/\\|?*]', "_", filename)
            # Limit length
            if len(filename) > 255:
                name, ext = os.path.splitext(filename)
                filename = name[: 255 - len(ext)] + ext
            return filename

        # Test successful temporary upload
        safe_content = b"This is safe content for testing"
        result = secure_temp_upload(safe_content, "test_document.txt")

        print(f"Debug - Result: {result}")
        if not result["success"]:
            print(f"Debug - Error: {result.get('error', 'Unknown error')}")

        assert result["success"] is True
        assert Path(result["filepath"]).exists()
        assert "test_document.txt" in result["filepath"]

        # Test malicious content cleanup
        malicious_content = b"<script>alert('XSS')</script>"
        result = secure_temp_upload(malicious_content, "malicious.html")

        assert result["success"] is False
        assert "File rejected" in result["error"]

        # Verify temporary file was cleaned up
        remaining_temp_files = [f for f in temp_files_created if f.exists()]
        assert len(remaining_temp_files) == 0

    def test_concurrent_upload_security(self):
        """Test security under concurrent upload conditions"""
        import threading
        import time

        upload_results = []
        upload_lock = threading.Lock()

        def concurrent_upload_worker(worker_id: int, content: str, filename: str):
            """Worker function for concurrent upload testing"""
            try:
                file_path = self.create_malicious_file(
                    f"worker_{worker_id}_{filename}", content
                )
                scan_result = self.security_scanner.scan_file(str(file_path))

                with upload_lock:
                    upload_results.append(
                        {
                            "worker_id": worker_id,
                            "filename": filename,
                            "safe": scan_result["safe"],
                            "threat_type": scan_result.get("threat_type"),
                        }
                    )

            except Exception as e:
                with upload_lock:
                    upload_results.append(
                        {"worker_id": worker_id, "error": str(e), "safe": False}
                    )

        # Create multiple upload threads
        threads = []
        test_data = [
            (1, "Safe content 1", "safe1.txt"),
            (2, "<script>alert('XSS')</script>", "malicious1.html"),
            (3, "Safe content 2", "safe2.txt"),
            (4, "<?php system('ls'); ?>", "malicious2.php"),
            (5, "Safe content 3", "safe3.txt"),
        ]

        # Start concurrent uploads
        for worker_id, content, filename in test_data:
            thread = threading.Thread(
                target=concurrent_upload_worker, args=(worker_id, content, filename)
            )
            threads.append(thread)
            thread.start()

        # Wait for all threads to complete
        for thread in threads:
            thread.join(timeout=10)

        # Verify results
        assert len(upload_results) == 5

        # Check safe files were accepted
        safe_results = [
            r for r in upload_results if r.get("safe") is True and "error" not in r
        ]
        assert len(safe_results) == 3

        # Check malicious files were rejected
        malicious_results = [
            r
            for r in upload_results
            if r.get("safe") is False and r.get("threat_type") == "malicious_content"
        ]
        assert len(malicious_results) == 2

    def test_file_content_encoding_bypass(self):
        """Test prevention of encoding-based bypass attempts"""
        # Test various encoding bypass attempts
        encoding_bypasses = [
            # Base64 encoded malicious content
            (
                "base64_malicious.txt",
                "PHNjcmlwdD5hbGVydCgnWFNTJyk8L3NjcmlwdD4=",
                "base64",
            ),
            # URL encoded malicious content
            (
                "urlencoded.txt",
                "%3Cscript%3Ealert%28%27XSS%27%29%3C%2Fscript%3E",
                "url",
            ),
            # HTML entity encoded
            (
                "htmlentity.txt",
                "&lt;script&gt;alert(&#39;XSS&#39;)&lt;/script&gt;",
                "html_entity",
            ),
            # Unicode encoding attempts
            (
                "unicode.txt",
                "\\u003cscript\\u003ealert('XSS')\\u003c/script\\u003e",
                "unicode",
            ),
        ]

        def detect_encoding_bypass(content: str, encoding_type: str) -> bool:
            """Detect and decode various encoding bypass attempts"""
            try:
                if encoding_type == "base64":
                    import base64

                    decoded = base64.b64decode(content).decode("utf-8")
                elif encoding_type == "url":
                    import urllib.parse

                    decoded = urllib.parse.unquote(content)
                elif encoding_type == "html_entity":
                    import html

                    decoded = html.unescape(content)
                elif encoding_type == "unicode":
                    # Simple unicode escape decoding
                    decoded = content.encode().decode("unicode_escape")
                else:
                    decoded = content

                # Check for malicious patterns in decoded content
                malicious_patterns = ["<script", "javascript:", "eval(", "alert("]
                for pattern in malicious_patterns:
                    if pattern in decoded.lower():
                        return True

                return False

            except Exception:
                return True  # Treat decoding errors as suspicious

        for filename, content, encoding_type in encoding_bypasses:
            file_path = self.create_malicious_file(filename, content)

            # Test encoding bypass detection
            is_malicious = detect_encoding_bypass(content, encoding_type)
            assert is_malicious, (
                f"Encoding bypass attempt {encoding_type} was not detected"
            )

            # Even if original file passes basic scan, encoding bypass should be caught
            scan_result = self.security_scanner.scan_file(str(file_path))
            if scan_result["safe"]:  # If basic scan passed
                # Encoding bypass detection should catch it
                assert is_malicious, (
                    "Encoding bypass should be detected even if basic scan passes"
                )

    def test_file_metadata_security(self):
        """Test file metadata security and validation"""

        # Mock file with dangerous metadata
        def create_file_with_metadata(
            filename: str, content: str, metadata: Dict[str, Any]
        ) -> Path:
            """Create file with custom metadata"""
            file_path = self.create_malicious_file(filename, content)

            # Simulate metadata that could be exploited
            if metadata:
                metadata_file = self.upload_dir / f"{file_path.stem}.meta"
                metadata_file.write_text(str(metadata))

            return file_path

        # Test dangerous metadata scenarios
        dangerous_metadata_tests = [
            {
                "filename": "document.txt",
                "content": "Safe content",
                "metadata": {"author": "../../../etc/passwd"},
                "expected_violation": "path_traversal_in_metadata",
            },
            {
                "filename": "article.md",
                "content": "Safe markdown",
                "metadata": {"tags": "<script>alert('XSS')</script>"},
                "expected_violation": "xss_in_metadata",
            },
            {
                "filename": "data.json",
                "content": '{"safe": "content"}',
                "metadata": {"permissions": "777", "executable": True},
                "expected_violation": "dangerous_permissions",
            },
        ]

        def validate_metadata_security(metadata: Dict[str, Any]) -> List[str]:
            """Validate file metadata for security issues"""
            violations = []

            for key, value in metadata.items():
                # Check for path traversal in metadata
                if isinstance(value, str):
                    if ".." in value or value.startswith("/") or "\\" in value:
                        violations.append("path_traversal_in_metadata")

                    # Check for XSS in metadata
                    if any(
                        pattern in value.lower()
                        for pattern in ["<script", "javascript:", "onerror="]
                    ):
                        violations.append("xss_in_metadata")

                # Check for dangerous permission settings
                if key == "permissions" and isinstance(value, str):
                    if value in ["777", "666", "555"] or "w" in value:
                        violations.append("dangerous_permissions")

                # Check for executable flags
                if key in ["executable", "runnable"] and value is True:
                    violations.append("executable_flag")

            return violations

        for test_case in dangerous_metadata_tests:
            file_path = create_file_with_metadata(
                test_case["filename"], test_case["content"], test_case["metadata"]
            )

            # Validate metadata security
            violations = validate_metadata_security(test_case["metadata"])
            assert len(violations) > 0, (
                f"Security violation not detected in metadata: {test_case['metadata']}"
            )
            assert test_case["expected_violation"] in violations
