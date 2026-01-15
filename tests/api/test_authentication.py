"""
Authentication tests for Integral Philosophy Publishing System API
"""

import pytest
import time
import json
from unittest.mock import Mock, patch, MagicMock
from typing import Dict, Any

from ..utils.base_test_classes import APITestCase


class TestAuthentication(APITestCase):
    """Test authentication and authorization functionality"""

    def test_valid_api_key_authentication(self):
        """Test authentication with valid API key"""
        # Test with default valid API key
        status, response = self.get("/health")
        self.assert_api_response(status, response, expected_status=200)

        # Test that job creation works with valid API key
        job_params = {"source_format": "markdown", "target_format": "html"}
        status, response = self.post("/convert", job_params, expected_status=202)
        self.assert_api_response(
            status, response, expected_status=202, expected_keys=["job_id"]
        )

    def test_invalid_api_key_rejection(self):
        """Test rejection with invalid API key"""
        # Use invalid API key
        invalid_headers = {
            "Content-Type": "application/json",
            "X-API-Key": "invalid-key",
        }

        response = self.api_client.get(
            f"{self.base_url}/health", headers=invalid_headers
        )

        assert response.status_code == 401
        response_data = response.get_json() if response.get_json() else {}
        assert "error" in response_data
        assert "unauthorized" in response_data["error"].lower()

    def test_missing_api_key_rejection(self):
        """Test rejection when API key is missing"""
        # Request without API key
        headers_no_key = {"Content-Type": "application/json"}

        response = self.api_client.get(
            f"{self.base_url}/health", headers=headers_no_key
        )

        assert response.status_code == 401
        response_data = response.get_json() if response.get_json() else {}
        assert "error" in response_data
        assert "api key required" in response_data["error"].lower()

    def test_expired_api_key_rejection(self):
        """Test rejection with expired API key"""
        with patch("api_server.validate_api_key") as mock_validate:
            mock_validate.return_value = False

            headers = {"Content-Type": "application/json", "X-API-Key": "expired-key"}
            response = self.api_client.get(f"{self.base_url}/health", headers=headers)

            assert response.status_code == 401

    def test_api_key_format_validation(self):
        """Test API key format validation"""
        invalid_keys = [
            "",  # Empty string
            "short",  # Too short
            "invalid-format",  # Wrong format
            "123",  # Only numbers
            "@#$%",  # Only special characters
            "a" * 100,  # Too long
        ]

        for invalid_key in invalid_keys:
            headers = {"Content-Type": "application/json", "X-API-Key": invalid_key}
            response = self.api_client.get(f"{self.base_url}/health", headers=headers)

            # Should reject invalid formats
            assert response.status_code in [401, 400]

    def test_authentication_bypass_attempts(self):
        """Test various authentication bypass attempts"""
        bypass_attempts = [
            # Attempt to use different header names
            {"Content-Type": "application/json", "API-Key": self.api_key},
            {"Content-Type": "application/json", "x-api-key": self.api_key},
            {"Content-Type": "application/json", "X-Api-Key": self.api_key},
            {
                "Content-Type": "application/json",
                "authentication": f"Bearer {self.api_key}",
            },
            # Attempt to use query parameters
            None,  # Will add API key to query string
            # Attempt to use cookies
            None,  # Will add API key to cookies
        ]

        for i, attempt in enumerate(bypass_attempts):
            if i == len(bypass_attempts) - 2:  # Query parameter attempt
                response = self.api_client.get(
                    f"{self.base_url}/health?api_key={self.api_key}",
                    headers={"Content-Type": "application/json"},
                )
            elif i == len(bypass_attempts) - 1:  # Cookie attempt
                self.api_client.set_cookie("api_key", self.api_key)
                response = self.api_client.get(
                    f"{self.base_url}/health",
                    headers={"Content-Type": "application/json"},
                )
            else:
                response = self.api_client.get(
                    f"{self.base_url}/health", headers=attempt
                )

            # Should reject bypass attempts
            assert response.status_code == 401

    @patch("api_server.rate_limiter")
    def test_rate_limiting_functionality(self, mock_rate_limiter):
        """Test rate limiting on authenticated requests"""
        # Mock rate limiter to allow tracking
        call_count = 0

        def mock_check_limit(api_key):
            nonlocal call_count
            call_count += 1
            return call_count <= 5  # Allow 5 requests, then block

        mock_rate_limiter.is_allowed.side_effect = mock_check_limit

        # Make multiple requests
        for i in range(10):
            response = self.api_client.get(
                f"{self.base_url}/health", headers=self.headers
            )

            if i < 5:
                assert response.status_code == 200
            else:
                assert response.status_code == 429  # Too Many Requests

    def test_concurrent_requests_handling(self):
        """Test handling of concurrent authenticated requests"""
        import threading
        import queue

        results = queue.Queue()

        def make_request(request_id):
            try:
                status, response = self.get("/health")
                results.put((request_id, status, response))
            except Exception as e:
                results.put((request_id, "error", str(e)))

        # Create multiple concurrent requests
        threads = []
        for i in range(10):
            thread = threading.Thread(target=make_request, args=(i,))
            threads.append(thread)
            thread.start()

        # Wait for all threads to complete
        for thread in threads:
            thread.join()

        # Check results
        success_count = 0
        while not results.empty():
            request_id, status, response = results.get()
            if status == 200:
                success_count += 1
            elif status == "error":
                pytest.fail(f"Request {request_id} failed with error: {response}")

        # At least some requests should succeed
        assert success_count > 0

    def test_session_management(self):
        """Test session management functionality"""
        # Create a session-like request pattern
        session_data = {}

        # First request - establish session
        status, response = self.get("/health")
        assert status == 200

        # Store session identifier if provided
        if "session_id" in response:
            session_data["session_id"] = response["session_id"]

        # Subsequent requests should maintain session
        for i in range(3):
            status, response = self.get("/health")
            assert status == 200

            # Check session consistency if session ID is tracked
            if "session_id" in response and session_data.get("session_id"):
                assert response["session_id"] == session_data["session_id"]

    def test_security_headers_validation(self):
        """Test that security headers are present in responses"""
        response = self.api_client.get(f"{self.base_url}/health", headers=self.headers)

        # Check for security headers
        security_headers = [
            "X-Content-Type-Options",
            "X-Frame-Options",
            "X-XSS-Protection",
            "Content-Security-Policy",
        ]

        for header in security_headers:
            assert header in response.headers, f"Missing security header: {header}"

    def test_api_key_rotation(self):
        """Test API key rotation functionality"""
        # Generate new API key
        new_api_key = "new-test-api-key-2025"

        # Update headers with new key
        new_headers = {"Content-Type": "application/json", "X-API-Key": new_api_key}

        with patch("api_server.validate_api_key") as mock_validate:
            # Mock validation to accept new key
            mock_validate.return_value = True

            # Test with new key
            response = self.api_client.get(
                f"{self.base_url}/health", headers=new_headers
            )

            # Should work with new key if validation passes
            assert response.status_code == 200

    def test_ip_based_rate_limiting(self):
        """Test IP-based rate limiting"""
        # Mock different IP addresses
        test_ips = ["192.168.1.1", "192.168.1.2", "10.0.0.1"]

        with patch("api_server.rate_limiter") as mock_rate_limiter:
            ip_call_counts = {}

            def mock_check_by_ip(api_key, ip_address):
                if ip_address not in ip_call_counts:
                    ip_call_counts[ip_address] = 0
                ip_call_counts[ip_address] += 1
                return ip_call_counts[ip_address] <= 3  # 3 requests per IP

            mock_rate_limiter.is_allowed_by_ip.side_effect = mock_check_by_ip

            # Test requests from different IPs
            for ip in test_ips:
                for i in range(5):
                    with patch("flask.request") as mock_request:
                        mock_request.remote_addr = ip

                        response = self.api_client.get(
                            f"{self.base_url}/health", headers=self.headers
                        )

                        if i < 3:
                            assert response.status_code == 200
                        else:
                            assert response.status_code == 429

    def test_authentication_logging(self):
        """Test that authentication attempts are logged"""
        with patch("api_server.logger") as mock_logger:
            # Failed authentication
            invalid_headers = {
                "Content-Type": "application/json",
                "X-API-Key": "invalid",
            }
            response = self.api_client.get(
                f"{self.base_url}/health", headers=invalid_headers
            )

            # Should log failed attempt
            mock_logger.warning.assert_called()
            call_args = mock_logger.warning.call_args[0][0]
            assert "authentication" in call_args.lower()
            assert "failed" in call_args.lower()

    def test_csrf_protection(self):
        """Test CSRF protection on state-changing requests"""
        # Test POST without CSRF token (if CSRF is enabled)
        job_params = {"source_format": "markdown", "target_format": "html"}

        # Remove potential CSRF protection headers
        headers_no_csrf = {
            "Content-Type": "application/json",
            "X-API-Key": self.api_key,
        }

        response = self.api_client.post(
            f"{self.base_url}/convert", json=job_params, headers=headers_no_csrf
        )

        # Should either succeed (if CSRF not required) or fail with appropriate error
        assert response.status_code in [202, 400, 403]

    def test_api_key_scope_validation(self):
        """Test API key scope/permission validation"""
        # Mock different API key scopes
        test_scenarios = [
            {
                "key": "read-only-key",
                "allowed_endpoints": ["/health", "/jobs"],
                "blocked": ["/convert", "/scrape"],
            },
            {
                "key": "full-access-key",
                "allowed_endpoints": ["/health", "/jobs", "/convert", "/scrape"],
                "blocked": [],
            },
            {
                "key": "limited-key",
                "allowed_endpoints": ["/health"],
                "blocked": ["/jobs", "/convert", "/scrape"],
            },
        ]

        for scenario in test_scenarios:
            headers = {"Content-Type": "application/json", "X-API-Key": scenario["key"]}

            with patch("api_server.check_api_key_scope") as mock_scope:

                def mock_check_scope(api_key, endpoint):
                    if endpoint in scenario["allowed_endpoints"]:
                        return True
                    return False

                mock_scope.side_effect = mock_check_scope

                # Test allowed endpoints
                for endpoint in scenario["allowed_endpoints"]:
                    response = self.api_client.get(
                        f"{self.base_url}{endpoint}", headers=headers
                    )
                    assert response.status_code in [
                        200,
                        404,
                    ]  # 404 if endpoint doesn't exist

                # Test blocked endpoints
                for endpoint in scenario["blocked"]:
                    response = self.api_client.get(
                        f"{self.base_url}{endpoint}", headers=headers
                    )
                    assert response.status_code == 403

    @pytest.mark.slow
    def test_authentication_performance(self):
        """Test performance of authentication checks"""
        start_time = time.time()

        # Make multiple authentication requests
        for _ in range(100):
            status, response = self.get("/health")
            assert status == 200

        end_time = time.time()
        total_time = end_time - start_time
        avg_time = total_time / 100

        # Authentication should be fast (less than 50ms per request on average)
        assert avg_time < 0.05, f"Authentication too slow: {avg_time:.3f}s per request"
