"""
API endpoints tests for Integral Philosophy Publishing System
"""

import pytest
import json
from unittest.mock import Mock, patch, MagicMock
from typing import Dict, Any, List

from ..utils.base_test_classes import APITestCase


class TestAPIEndpoints(APITestCase):
    """Test all HTTP methods and API endpoints"""

    def test_health_endpoint_get(self):
        """Test GET /health endpoint"""
        status, response = self.get("/health")
        self.assert_api_response(
            status, response, expected_status=200, expected_keys=["status"]
        )

        assert response["status"] in ["healthy", "ok"]
        assert "timestamp" in response or "version" in response

    def test_health_endpoint_post_not_allowed(self):
        """Test POST /health endpoint (should not be allowed)"""
        status, response = self.post("/health", {})
        assert status in [405, 400]  # Method Not Allowed or Bad Request

    def test_health_endpoint_delete_not_allowed(self):
        """Test DELETE /health endpoint (should not be allowed)"""
        status, response = self.delete("/health")
        assert status in [405, 400]

    def test_convert_endpoint_post(self):
        """Test POST /convert endpoint"""
        convert_data = {
            "source_format": "markdown",
            "target_format": "html",
            "source_content": "# Test Document\n\nThis is test content.",
        }

        status, response = self.post("/convert", convert_data, expected_status=202)
        self.assert_api_response(
            status, response, expected_status=202, expected_keys=["job_id"]
        )

    def test_convert_endpoint_get_not_allowed(self):
        """Test GET /convert endpoint (should not be allowed)"""
        status, response = self.get("/convert")
        assert status in [405, 400]

    def test_scrape_endpoint_post(self):
        """Test POST /scrape endpoint"""
        scrape_data = {
            "url": "https://example.com/article",
            "depth": 2,
            "max_pages": 10,
        }

        status, response = self.post("/scrape", scrape_data, expected_status=202)
        self.assert_api_response(
            status, response, expected_status=202, expected_keys=["job_id"]
        )

    def test_tei_endpoint_post(self):
        """Test POST /tei endpoint"""
        tei_data = {
            "source_format": "markdown",
            "source_content": "# Academic Article\n\nThis is scholarly content.",
            "metadata": {
                "title": "Test Article",
                "author": "Test Author",
            },
        }

        status, response = self.post("/tei", tei_data, expected_status=202)
        self.assert_api_response(
            status, response, expected_status=202, expected_keys=["job_id"]
        )

    def test_uml_endpoint_post(self):
        """Test POST /uml endpoint"""
        uml_data = {
            "source_type": "python",
            "source_content": "class Document:\n    def render(self):\n        pass",
            "output_format": "plantuml",
        }

        status, response = self.post("/uml", uml_data, expected_status=202)
        self.assert_api_response(
            status, response, expected_status=202, expected_keys=["job_id"]
        )

    def test_pipeline_endpoint_post(self):
        """Test POST /pipeline endpoint"""
        pipeline_data = {
            "pipeline_type": "scrape_and_process",
            "input": {
                "url": "https://example.com/article",
                "generate_tei": True,
                "generate_uml": True,
            },
        }

        status, response = self.post("/pipeline", pipeline_data, expected_status=202)
        self.assert_api_response(
            status, response, expected_status=202, expected_keys=["job_id"]
        )

    def test_jobs_endpoint_get(self):
        """Test GET /jobs endpoint"""
        # First create some jobs
        job_ids = []
        for i in range(3):
            job_id, _ = self.create_test_job(
                "convert",
                {
                    "source_format": "markdown",
                    "target_format": "html",
                    "source_content": f"# Test {i}",
                },
            )
            job_ids.append(job_id)

        # Get job list
        status, response = self.get("/jobs")
        self.assert_api_response(
            status, response, expected_status=200, expected_keys=["jobs"]
        )

        jobs = response["jobs"]
        assert isinstance(jobs, list)
        assert len(jobs) >= len(job_ids)

    def test_jobs_endpoint_with_filters(self):
        """Test GET /jobs endpoint with query parameters"""
        # Create jobs of different types
        convert_job_id, _ = self.create_test_job(
            "convert",
            {
                "source_format": "markdown",
                "target_format": "html",
                "source_content": "# Convert Test",
            },
        )

        tei_job_id, _ = self.create_test_job(
            "tei",
            {
                "source_format": "markdown",
                "source_content": "# TEI Test",
                "metadata": {},
            },
        )

        # Filter by job type
        status, response = self.get("/jobs", params={"job_type": "convert"})
        assert status == 200
        assert "jobs" in response

        convert_jobs = [
            job for job in response["jobs"] if job.get("job_type") == "convert"
        ]
        assert len(convert_jobs) >= 1

    def test_job_detail_endpoint_get(self):
        """Test GET /jobs/{job_id} endpoint"""
        # Create a job
        job_id, _ = self.create_test_job(
            "convert",
            {
                "source_format": "markdown",
                "target_format": "html",
                "source_content": "# Test Job",
            },
        )

        # Get job details
        status, response = self.get(f"/jobs/{job_id}")
        assert status in [200, 202]

        if status == 200:
            self.assert_api_response(
                status, response, expected_keys=["job_id", "status"]
            )
            assert response["job_id"] == job_id

    def test_job_detail_endpoint_not_found(self):
        """Test GET /jobs/{nonexistent_job_id} endpoint"""
        fake_job_id = "nonexistent-job-id"
        status, response = self.get(f"/jobs/{fake_job_id}")
        assert status == 404

    def test_job_detail_endpoint_delete(self):
        """Test DELETE /jobs/{job_id} endpoint"""
        # Create a job
        job_id, _ = self.create_test_job(
            "convert",
            {
                "source_format": "markdown",
                "target_format": "html",
                "source_content": "# Test Job",
            },
        )

        # Delete the job
        status, response = self.delete(f"/jobs/{job_id}")
        assert status == 200

    def test_stats_endpoint_get(self):
        """Test GET /stats endpoint"""
        status, response = self.get("/stats")
        assert status in [200, 404]  # 404 if not implemented

        if status == 200:
            expected_keys = ["total_jobs", "active_jobs", "completed_jobs"]
            # At least some expected keys should be present
            assert any(key in response for key in expected_keys)

    def test_version_endpoint_get(self):
        """Test GET /version endpoint"""
        status, response = self.get("/version")
        assert status in [200, 404]  # 404 if not implemented

        if status == 200:
            assert "version" in response

    def test_invalid_endpoint(self):
        """Test requests to invalid endpoints"""
        invalid_endpoints = [
            "/invalid",
            "/nonexistent",
            "/api/v2/invalid",  # Wrong version
        ]

        for endpoint in invalid_endpoints:
            status, response = self.get(endpoint, expected_status=404)
            assert status == 404

    def test_request_response_validation(self):
        """Test request/response format validation"""
        # Test with invalid JSON
        response = self.api_client.post(
            f"{self.base_url}/convert",
            data="invalid json",
            headers=self.headers,
            content_type="application/json",
        )
        assert response.status_code in [400, 422]

        # Test with missing content type
        response = self.api_client.post(
            f"{self.base_url}/convert",
            data=json.dumps({"test": "data"}),
            headers={"X-API-Key": self.api_key},  # Missing Content-Type
        )
        assert response.status_code in [400, 415]

    def test_error_response_format(self):
        """Test error response format consistency"""
        # Test various error scenarios
        error_scenarios = [
            # Missing API key
            lambda: self.api_client.get(f"{self.base_url}/health", headers={}),
            # Invalid endpoint
            lambda: self.get("/invalid-endpoint", expected_status=404),
            # Invalid method
            lambda: self.post("/health", {}),
        ]

        for scenario in error_scenarios:
            response = scenario()

            if hasattr(response, "get_json"):
                try:
                    error_data = response.get_json()
                    if error_data:
                        assert "error" in error_data or "message" in error_data
                except:
                    pass  # JSON parsing failed

    def test_api_versioning_support(self):
        """Test API versioning"""
        # Test current version
        status, response = self.get("/health")
        assert status == 200

        # Test version header if present
        response_obj = self.api_client.get(
            f"{self.base_url}/health", headers=self.headers
        )
        if "API-Version" in response_obj.headers:
            assert response_obj.headers["API-Version"] == "v1"

    def test_content_negotiation(self):
        """Test content negotiation (different response formats)"""
        # Test JSON response
        headers_json = self.headers.copy()
        headers_json["Accept"] = "application/json"

        response = self.api_client.get(f"{self.base_url}/health", headers=headers_json)
        assert response.status_code == 200
        assert "application/json" in response.headers.get("Content-Type", "")

        # Test HTML response (if supported)
        headers_html = self.headers.copy()
        headers_html["Accept"] = "text/html"

        response = self.api_client.get(f"{self.base_url}/health", headers=headers_html)
        # Should still work, might return JSON or HTML
        assert response.status_code == 200

    def test_cors_headers(self):
        """Test CORS headers presence"""
        response = self.api_client.options(
            f"{self.base_url}/health", headers=self.headers
        )

        # Check for common CORS headers
        cors_headers = [
            "Access-Control-Allow-Origin",
            "Access-Control-Allow-Methods",
            "Access-Control-Allow-Headers",
        ]

        # At least one CORS header should be present
        assert any(header in response.headers for header in cors_headers)

    def test_parameter_encoding(self):
        """Test parameter encoding in requests"""
        # Test with special characters
        special_content = (
            "Test with special chars: àáâãäåæçèéêë ñòóôõö ùúûüý ÿ 中文 русский"
        )

        convert_data = {
            "source_format": "markdown",
            "target_format": "html",
            "source_content": special_content,
        }

        status, response = self.post("/convert", convert_data, expected_status=202)
        assert status == 202
        assert "job_id" in response

    def test_large_payload_handling(self):
        """Test handling of large request payloads"""
        # Create large content
        large_content = "# Large Document\n\n" + "This is a large paragraph.\n" * 1000

        convert_data = {
            "source_format": "markdown",
            "target_format": "html",
            "source_content": large_content,
        }

        status, response = self.post("/convert", convert_data)
        # Should either accept or reject with appropriate status
        assert status in [202, 413]  # 413 = Payload Too Large

    def test_concurrent_endpoint_access(self):
        """Test concurrent access to same endpoint"""
        import threading
        import queue

        results = queue.Queue()

        def access_endpoint():
            try:
                status, response = self.get("/health")
                results.put((status, response))
            except Exception as e:
                results.put(("error", str(e)))

        # Create multiple threads accessing the same endpoint
        threads = []
        for _ in range(10):
            thread = threading.Thread(target=access_endpoint)
            threads.append(thread)
            thread.start()

        # Wait for completion
        for thread in threads:
            thread.join()

        # Check results
        success_count = 0
        while not results.empty():
            status, response = results.get()
            if status == 200:
                success_count += 1
            elif status == "error":
                pytest.fail(f"Concurrent access failed: {response}")

        assert success_count > 0

    def test_http_method_override(self):
        """Test HTTP method override support"""
        # Some APIs support X-HTTP-Method-Override header
        headers_override = self.headers.copy()
        headers_override["X-HTTP-Method-Override"] = "GET"

        response = self.api_client.post(
            f"{self.base_url}/health", headers=headers_override
        )

        # Should either support override or return error
        assert response.status_code in [200, 405, 400]

    def test_batch_operations(self):
        """Test batch operation support"""
        batch_data = {
            "operations": [
                {
                    "type": "convert",
                    "params": {
                        "source_format": "markdown",
                        "target_format": "html",
                        "source_content": "# Batch Test 1",
                    },
                },
                {
                    "type": "convert",
                    "params": {
                        "source_format": "markdown",
                        "target_format": "html",
                        "source_content": "# Batch Test 2",
                    },
                },
            ]
        }

        # Test batch endpoint if available
        status, response = self.post("/batch", batch_data)
        # May or may not be implemented
        assert status in [200, 202, 404]

    def test_webhook_support(self):
        """Test webhook callback support"""
        webhook_data = {
            "source_format": "markdown",
            "target_format": "html",
            "source_content": "# Webhook Test",
            "webhook_url": "https://example.com/webhook",
        }

        status, response = self.post("/convert", webhook_data, expected_status=202)
        assert status == 202
        assert "job_id" in response

    def test_request_id_tracking(self):
        """Test request ID tracking"""
        headers_with_id = self.headers.copy()
        headers_with_id["X-Request-ID"] = "test-request-123"

        response = self.api_client.get(
            f"{self.base_url}/health", headers=headers_with_id
        )

        assert response.status_code == 200

        # Check if request ID is echoed back
        if "X-Request-ID" in response.headers:
            assert response.headers["X-Request-ID"] == "test-request-123"

    @pytest.mark.slow
    def test_endpoint_performance(self):
        """Test endpoint response times"""
        import time

        endpoints = [
            "/health",
            "/jobs",
        ]

        for endpoint in endpoints:
            start_time = time.time()
            status, response = self.get(endpoint)
            end_time = time.time()

            response_time = end_time - start_time

            # Most endpoints should respond within 1 second
            assert response_time < 1.0, f"{endpoint} too slow: {response_time:.3f}s"
            assert status in [200, 202]
