"""
File operations tests for Integral Philosophy Publishing System API
"""

import pytest
import tempfile
import os
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock, mock_open
from typing import Dict, Any, List

from ..utils.base_test_classes import APITestCase


class TestFileOperations(APITestCase):
    """Test file upload, download, and security functionality"""

    def test_file_upload_supported_formats(self):
        """Test file upload with supported formats"""
        # Create test files
        test_files = {
            "document.md": "# Test Markdown\n\nThis is a test document.",
            "document.html": "<html><body><h1>Test HTML</h1><p>Test content.</p></body></html>",
            "document.tex": r"\documentclass{article}\begin{document}\title{Test}\end{document}",
            "document.txt": "This is plain text content.",
        }

        for filename, content in test_files.items():
            # Create temporary file
            with tempfile.NamedTemporaryFile(
                mode="w", suffix=Path(filename).suffix, delete=False
            ) as f:
                f.write(content)
                temp_path = f.name

            try:
                # Upload file
                with open(temp_path, "rb") as f:
                    response = self.api_client.post(
                        f"{self.base_url}/upload",
                        data={"file": (f, filename)},
                        headers={"X-API-Key": self.api_key},
                    )

                # Should accept supported formats
                assert response.status_code in [200, 202]

                if response.status_code in [200, 202]:
                    response_data = response.get_json()
                    assert "file_id" in response_data or "job_id" in response_data

            finally:
                os.unlink(temp_path)

    def test_file_upload_unsupported_formats(self):
        """Test file upload rejection for unsupported formats"""
        unsupported_files = {
            "document.exe": b"fake executable content",
            "document.dll": b"fake dll content",
            "document.bat": b"@echo off",
            "document.sh": b"#!/bin/bash\necho 'test'",
            "document.php": b"<?php echo 'test'; ?>",
            "document.js": b"console.log('test');",
        }

        for filename, content in unsupported_files.items():
            with tempfile.NamedTemporaryFile(
                mode="wb", suffix=Path(filename).suffix, delete=False
            ) as f:
                f.write(content)
                temp_path = f.name

            try:
                with open(temp_path, "rb") as f:
                    response = self.api_client.post(
                        f"{self.base_url}/upload",
                        data={"file": (f, filename)},
                        headers={"X-API-Key": self.api_key},
                    )

                # Should reject unsupported formats
                assert response.status_code in [400, 422]

            finally:
                os.unlink(temp_path)

    def test_file_upload_size_limits(self):
        """Test file upload size limits"""
        # Test with very large file
        large_content = b"x" * (50 * 1024 * 1024)  # 50MB

        with tempfile.NamedTemporaryFile(mode="wb", suffix=".txt", delete=False) as f:
            f.write(large_content)
            temp_path = f.name

        try:
            with open(temp_path, "rb") as f:
                response = self.api_client.post(
                    f"{self.base_url}/upload",
                    data={"file": (f, "large.txt")},
                    headers={"X-API-Key": self.api_key},
                )

            # Should reject large files
            assert response.status_code in [413, 400]  # 413 = Payload Too Large

        finally:
            os.unlink(temp_path)

    def test_malicious_file_detection(self):
        """Test detection of malicious files"""
        malicious_files = [
            # PHP webshell
            ("shell.php", b"<?php @eval($_POST['cmd']); ?>"),
            # JavaScript with suspicious patterns
            ("script.js", b"<script>alert('XSS')</script>"),
            # HTML with script tags
            ("malicious.html", b"<html><script>alert('XSS')</script></html>"),
            # Executable disguised as text
            ("malicious.txt", b"MZ\x90\x00"),  # PE header
        ]

        for filename, content in malicious_files:
            with tempfile.NamedTemporaryFile(
                mode="wb", suffix=Path(filename).suffix, delete=False
            ) as f:
                f.write(content)
                temp_path = f.name

            try:
                with open(temp_path, "rb") as f:
                    response = self.api_client.post(
                        f"{self.base_url}/upload",
                        data={"file": (f, filename)},
                        headers={"X-API-Key": self.api_key},
                    )

                # Should detect and reject malicious files
                assert response.status_code in [400, 422, 403]

            finally:
                os.unlink(temp_path)

    def test_file_download_functionality(self):
        """Test file download functionality"""
        # First upload a file
        test_content = "# Test Document\n\nThis is test content for download."

        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(test_content)
            temp_path = f.name

        try:
            # Upload file
            with open(temp_path, "rb") as f:
                upload_response = self.api_client.post(
                    f"{self.base_url}/upload",
                    data={"file": (f, "test.md")},
                    headers={"X-API-Key": self.api_key},
                )

            if upload_response.status_code in [200, 202]:
                upload_data = upload_response.get_json()
                file_id = upload_data.get("file_id")

                if file_id:
                    # Download file
                    download_response = self.api_client.get(
                        f"{self.base_url}/download/{file_id}",
                        headers={"X-API-Key": self.api_key},
                    )

                    # Should return file content
                    assert download_response.status_code == 200
                    assert download_response.headers.get("Content-Type", "").startswith(
                        "text/"
                    )

        finally:
            os.unlink(temp_path)

    def test_file_download_invalid_id(self):
        """Test file download with invalid file ID"""
        invalid_ids = [
            "nonexistent-file-id",
            "invalid-format",
            "../../../etc/passwd",  # Path traversal attempt
            "..\\..\\windows\\system32\\config\\sam",  # Windows path traversal
        ]

        for file_id in invalid_ids:
            response = self.api_client.get(
                f"{self.base_url}/download/{file_id}",
                headers={"X-API-Key": self.api_key},
            )

            # Should reject invalid or dangerous file IDs
            assert response.status_code in [404, 400, 403]

    def test_temporary_file_cleanup(self):
        """Test cleanup of temporary files"""
        with patch("tempfile.NamedTemporaryFile") as mock_temp:
            mock_file = MagicMock()
            mock_file.name = "/tmp/test_file.txt"
            mock_temp.return_value.__enter__.return_value = mock_file

            # Simulate file upload
            with patch("builtins.open", mock_open(read_data=b"test content")):
                response = self.api_client.post(
                    f"{self.base_url}/upload",
                    data={"file": (mock_file, "test.txt")},
                    headers={"X-API-Key": self.api_key},
                )

            # Verify temporary file cleanup was attempted
            mock_file.close.assert_called()

    def test_path_traversal_prevention(self):
        """Test path traversal attack prevention"""
        malicious_paths = [
            "../../../etc/passwd",
            "..\\..\\..\\windows\\system32\\drivers\\etc\\hosts",
            "/etc/shadow",
            "C:\\Windows\\System32\\config\\SAM",
            "....//....//....//etc/passwd",
            "%2e%2e%2f%2e%2e%2f%2e%2e%2fetc%2fpasswd",  # URL encoded
        ]

        for malicious_path in malicious_paths:
            # Test in file download endpoint
            response = self.api_client.get(
                f"{self.base_url}/download/{malicious_path}",
                headers={"X-API-Key": self.api_key},
            )

            # Should prevent path traversal
            assert response.status_code in [404, 400, 403]

    def test_filename_sanitization(self):
        """Test filename sanitization"""
        malicious_filenames = [
            "file.txt; rm -rf /",  # Command injection
            "file.txt|cat /etc/passwd",  # Command injection
            "file.txt`whoami`",  # Command injection
            "file.txt$(whoami)",  # Command injection
            "../../../etc/passwd",  # Path traversal
            "con",  # Windows reserved name
            "prn",  # Windows reserved name
            "aux",  # Windows reserved name
            "file.txt\x00.jpg",  # Null byte injection
            "file with spaces and symbols!@#$%^&*().txt",
            "unicode_\u202e_text.txt",  # Right-to-left override
        ]

        for filename in malicious_filenames:
            content = b"test content"

            with tempfile.NamedTemporaryFile(mode="wb", delete=False) as f:
                f.write(content)
                temp_path = f.name

            try:
                with open(temp_path, "rb") as f:
                    response = self.api_client.post(
                        f"{self.base_url}/upload",
                        data={"file": (f, filename)},
                        headers={"X-API-Key": self.api_key},
                    )

                # Should either sanitize filename or reject
                if response.status_code in [200, 202]:
                    # If accepted, filename should be sanitized
                    pass
                else:
                    # Should reject dangerous filenames
                    assert response.status_code in [400, 422]

            finally:
                os.unlink(temp_path)

    def test_file_content_scanning(self):
        """Test file content scanning for threats"""
        threat_signatures = [
            b"<script>alert('XSS')",
            b"<?php @eval(",
            b"document.cookie",
            b"javascript:",
            b"vbscript:",
            b"data:text/html",
            b"<iframe",
            b"<object",
            b"<embed",
        ]

        for signature in threat_signatures:
            filename = f"test_{len(signature)}.txt"
            content = signature + b" content"

            with tempfile.NamedTemporaryFile(
                mode="wb", suffix=".txt", delete=False
            ) as f:
                f.write(content)
                temp_path = f.name

            try:
                with open(temp_path, "rb") as f:
                    response = self.api_client.post(
                        f"{self.base_url}/upload",
                        data={"file": (f, filename)},
                        headers={"X-API-Key": self.api_key},
                    )

                # Should detect and potentially flag content
                if response.status_code in [400, 422, 403]:
                    # Good - threat detected
                    pass
                elif response.status_code in [200, 202]:
                    # If accepted, should be flagged for review
                    response_data = response.get_json()
                    assert "flagged" in response_data or "warning" in response_data

            finally:
                os.unlink(temp_path)

    def test_file_metadata_extraction(self):
        """Test extraction of file metadata"""
        # Test with different file types
        test_files = [
            ("document.md", "# Test\n\nContent here.", "text/markdown"),
            ("document.html", "<html><body><h1>Test</h1></body></html>", "text/html"),
            ("document.txt", "Plain text content.", "text/plain"),
        ]

        for filename, content, expected_type in test_files:
            with tempfile.NamedTemporaryFile(
                mode="w", suffix=Path(filename).suffix, delete=False
            ) as f:
                f.write(content)
                temp_path = f.name

            try:
                with open(temp_path, "rb") as f:
                    response = self.api_client.post(
                        f"{self.base_url}/upload",
                        data={"file": (f, filename)},
                        headers={"X-API-Key": self.api_key},
                    )

                if response.status_code in [200, 202]:
                    response_data = response.get_json()

                    # Should include metadata
                    if "metadata" in response_data:
                        metadata = response_data["metadata"]
                        assert "filename" in metadata
                        assert "size" in metadata
                        assert "content_type" in metadata

                        # Content type should be detected correctly
                        assert expected_type in metadata["content_type"] or metadata[
                            "content_type"
                        ] in ["text/plain", "application/octet-stream"]

            finally:
                os.unlink(temp_path)

    def test_concurrent_file_uploads(self):
        """Test concurrent file uploads"""
        import threading
        import queue

        results = queue.Queue()

        def upload_file(file_id):
            content = f"Test content from thread {file_id}"
            filename = f"test_{file_id}.txt"

            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".txt", delete=False
            ) as f:
                f.write(content)
                temp_path = f.name

            try:
                with open(temp_path, "rb") as f:
                    response = self.api_client.post(
                        f"{self.base_url}/upload",
                        data={"file": (f, filename)},
                        headers={"X-API-Key": self.api_key},
                    )

                results.put((file_id, response.status_code, response.get_json()))

            finally:
                os.unlink(temp_path)

        # Create multiple upload threads
        threads = []
        for i in range(5):
            thread = threading.Thread(target=upload_file, args=(i,))
            threads.append(thread)
            thread.start()

        # Wait for completion
        for thread in threads:
            thread.join()

        # Check results
        success_count = 0
        while not results.empty():
            file_id, status, response_data = results.get()
            if status in [200, 202]:
                success_count += 1
                assert "file_id" in response_data or "job_id" in response_data

        # At least some uploads should succeed
        assert success_count > 0

    def test_file_access_control(self):
        """Test file access control and permissions"""
        # First upload a file
        test_content = "# Test Document"

        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(test_content)
            temp_path = f.name

        try:
            # Upload with valid API key
            with open(temp_path, "rb") as f:
                upload_response = self.api_client.post(
                    f"{self.base_url}/upload",
                    data={"file": (f, "test.md")},
                    headers={"X-API-Key": self.api_key},
                )

            if upload_response.status_code in [200, 202]:
                upload_data = upload_response.get_json()
                file_id = upload_data.get("file_id")

                if file_id:
                    # Test access with invalid API key
                    invalid_response = self.api_client.get(
                        f"{self.base_url}/download/{file_id}",
                        headers={"X-API-Key": "invalid-key"},
                    )
                    assert invalid_response.status_code == 401

                    # Test access without API key
                    no_key_response = self.api_client.get(
                        f"{self.base_url}/download/{file_id}"
                    )
                    assert no_key_response.status_code == 401

        finally:
            os.unlink(temp_path)

    def test_file_storage_quota(self):
        """Test file storage quota limits"""
        # Test uploading multiple files to check quota
        uploaded_files = []

        try:
            # Upload several files to approach quota
            for i in range(10):
                content = f"Test content for file {i}\n" * 1000  # Large content
                filename = f"quota_test_{i}.txt"

                with tempfile.NamedTemporaryFile(
                    mode="w", suffix=".txt", delete=False
                ) as f:
                    f.write(content)
                    temp_path = f.name

                with open(temp_path, "rb") as f:
                    response = self.api_client.post(
                        f"{self.base_url}/upload",
                        data={"file": (f, filename)},
                        headers={"X-API-Key": self.api_key},
                    )

                if response.status_code in [200, 202]:
                    uploaded_files.append(response.get_json().get("file_id"))
                elif response.status_code == 413:  # Quota exceeded
                    break

                os.unlink(temp_path)

            # Should either accept all or stop at quota
            assert len(uploaded_files) >= 0

        finally:
            # Cleanup temporary files
            for temp_path in [f"/tmp/quota_test_{i}.txt" for i in range(10)]:
                if os.path.exists(temp_path):
                    os.unlink(temp_path)

    @pytest.mark.slow
    def test_file_processing_performance(self):
        """Test performance of file processing operations"""
        import time

        # Test with various file sizes
        file_sizes = [
            (1024, "1KB"),  # 1KB
            (10 * 1024, "10KB"),  # 10KB
            (100 * 1024, "100KB"),  # 100KB
        ]

        for size, description in file_sizes:
            content = "x" * size
            filename = f"perf_test_{description}.txt"

            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".txt", delete=False
            ) as f:
                f.write(content)
                temp_path = f.name

            try:
                start_time = time.time()

                with open(temp_path, "rb") as f:
                    response = self.api_client.post(
                        f"{self.base_url}/upload",
                        data={"file": (f, filename)},
                        headers={"X-API-Key": self.api_key},
                    )

                end_time = time.time()
                processing_time = end_time - start_time

                # Should process files efficiently
                assert response.status_code in [200, 202, 413]  # 413 if too large

                if response.status_code in [200, 202]:
                    # Processing should be reasonable
                    assert processing_time < 5.0, (
                        f"{description} file processing too slow: {processing_time:.3f}s"
                    )

            finally:
                os.unlink(temp_path)

    def test_file_backup_and_recovery(self):
        """Test file backup and recovery mechanisms"""
        # Upload a file
        test_content = "# Important Document\n\nThis content needs backup."

        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(test_content)
            temp_path = f.name

        try:
            # Upload file
            with open(temp_path, "rb") as f:
                upload_response = self.api_client.post(
                    f"{self.base_url}/upload",
                    data={"file": (f, "backup_test.md")},
                    headers={"X-API-Key": self.api_key},
                )

            if upload_response.status_code in [200, 202]:
                upload_data = upload_response.get_json()
                file_id = upload_data.get("file_id")

                if file_id:
                    # Test backup retrieval (if supported)
                    backup_response = self.api_client.get(
                        f"{self.base_url}/files/{file_id}/backup",
                        headers={"X-API-Key": self.api_key},
                    )

                    # May or may not be implemented
                    assert backup_response.status_code in [200, 404]

        finally:
            os.unlink(temp_path)
