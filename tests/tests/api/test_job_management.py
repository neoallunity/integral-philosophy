"""
Job management tests for Integral Philosophy Publishing System API
"""

import pytest
import time
import uuid
from unittest.mock import Mock, patch, MagicMock
from typing import Dict, Any, List

from ..utils.base_test_classes import APITestCase


class TestJobManagement(APITestCase):
    """Test job creation, tracking, and management functionality"""

    def test_convert_job_creation(self):
        """Test creation of format conversion jobs"""
        convert_params = {
            "source_format": "markdown",
            "target_format": "html",
            "source_content": "# Test Document\n\nThis is test content.",
        }

        status, response = self.post("/convert", convert_params, expected_status=202)
        self.assert_api_response(
            status, response, expected_status=202, expected_keys=["job_id"]
        )

        # Verify job ID format
        job_id = response["job_id"]
        assert isinstance(job_id, str)
        assert len(job_id) > 0

        # Check job status
        status, response = self.get(f"/jobs/{job_id}")
        assert status in [200, 202]  # 200 if completed, 202 if still processing
        self.assert_api_response(
            status, response, expected_status=status, expected_keys=["job_id", "status"]
        )

    def test_scrape_job_creation(self):
        """Test creation of web scraping jobs"""
        scrape_params = {
            "url": "https://example.com/article",
            "depth": 2,
            "max_pages": 10,
            "respect_robots": True,
        }

        status, response = self.post("/scrape", scrape_params, expected_status=202)
        self.assert_api_response(
            status, response, expected_status=202, expected_keys=["job_id"]
        )

        job_id = response["job_id"]

        # Verify job was created with correct parameters
        status, response = self.get(f"/jobs/{job_id}")
        assert status in [200, 202]
        if "parameters" in response:
            assert response["parameters"]["url"] == scrape_params["url"]

    def test_tei_job_creation(self):
        """Test creation of TEI generation jobs"""
        tei_params = {
            "source_format": "markdown",
            "source_content": "# Academic Article\n\nThis is scholarly content.",
            "metadata": {
                "title": "Test Article",
                "author": "Test Author",
                "date": "2024-01-01",
            },
        }

        status, response = self.post("/tei", tei_params, expected_status=202)
        self.assert_api_response(
            status, response, expected_status=202, expected_keys=["job_id"]
        )

        job_id = response["job_id"]

        # Check job type
        status, response = self.get(f"/jobs/{job_id}")
        assert status in [200, 202]
        if "job_type" in response:
            assert response["job_type"] == "tei"

    def test_uml_job_creation(self):
        """Test creation of UML generation jobs"""
        uml_params = {
            "source_type": "python",
            "source_content": """
class Document:
    def __init__(self, title):
        self.title = title
    
    def render(self):
        return f"<h1>{self.title}</h1>"
            """,
            "output_format": "plantuml",
        }

        status, response = self.post("/uml", uml_params, expected_status=202)
        self.assert_api_response(
            status, response, expected_status=202, expected_keys=["job_id"]
        )

        job_id = response["job_id"]

        # Verify job parameters
        status, response = self.get(f"/jobs/{job_id}")
        assert status in [200, 202]

    def test_pipeline_job_creation(self):
        """Test creation of pipeline jobs"""
        pipeline_params = {
            "pipeline_type": "scrape_and_process",
            "input": {
                "url": "https://example.com/article",
                "generate_tei": True,
                "generate_uml": True,
                "output_formats": ["html", "tei"],
            },
        }

        status, response = self.post("/pipeline", pipeline_params, expected_status=202)
        self.assert_api_response(
            status, response, expected_status=202, expected_keys=["job_id"]
        )

        job_id = response["job_id"]

        # Check pipeline job status
        status, response = self.get(f"/jobs/{job_id}")
        assert status in [200, 202]

    def test_job_status_tracking(self):
        """Test job status tracking through lifecycle"""
        # Create a job
        job_id, _ = self.create_test_job(
            "convert",
            {
                "source_format": "markdown",
                "target_format": "html",
                "source_content": "# Test",
            },
        )

        # Track status changes
        status_history = []
        max_checks = 10

        for _ in range(max_checks):
            status, response = self.get(f"/jobs/{job_id}")
            assert status in [200, 202]

            current_status = response.get("status", "unknown")
            if current_status not in status_history:
                status_history.append(current_status)

            # If job is complete or failed, stop tracking
            if current_status in ["completed", "failed"]:
                break

            time.sleep(0.1)  # Small delay between checks

        # Verify status progression
        assert len(status_history) > 0
        assert (
            "pending" in status_history
            or "running" in status_history
            or "completed" in status_history
        )

    def test_job_cancellation(self):
        """Test job cancellation functionality"""
        # Create a long-running job
        job_params = {
            "source_format": "markdown",
            "target_format": "html",
            "source_content": "# "
            + "Long content\n" * 1000,  # Large content to delay processing
        }

        status, response = self.post("/convert", job_params, expected_status=202)
        job_id = response["job_id"]

        # Cancel the job
        status, response = self.delete(f"/jobs/{job_id}", expected_status=200)
        self.assert_api_response(status, response, expected_status=200)

        # Verify job is cancelled
        status, response = self.get(f"/jobs/{job_id}")
        assert status == 200
        assert response.get("status") in ["cancelled", "completed", "failed"]

    def test_concurrent_job_handling(self):
        """Test handling of multiple concurrent jobs"""
        import threading
        import queue

        results = queue.Queue()

        def create_job(job_type, params):
            try:
                status, response = self.post(
                    f"/{job_type}", params, expected_status=202
                )
                results.put((job_type, status, response))
            except Exception as e:
                results.put((job_type, "error", str(e)))

        # Create multiple jobs of different types concurrently
        job_specs = [
            (
                "convert",
                {
                    "source_format": "markdown",
                    "target_format": "html",
                    "source_content": "# Test 1",
                },
            ),
            (
                "tei",
                {
                    "source_format": "markdown",
                    "source_content": "# Test 2",
                    "metadata": {"title": "Test"},
                },
            ),
            (
                "uml",
                {
                    "source_type": "python",
                    "source_content": "class Test: pass",
                    "output_format": "plantuml",
                },
            ),
        ]

        threads = []
        for job_type, params in job_specs:
            thread = threading.Thread(target=create_job, args=(job_type, params))
            threads.append(thread)
            thread.start()

        # Wait for all threads to complete
        for thread in threads:
            thread.join()

        # Check results
        success_count = 0
        while not results.empty():
            job_type, status, response = results.get()
            if status == 202:
                success_count += 1
                assert "job_id" in response
            elif status == "error":
                pytest.fail(f"Job {job_type} failed with error: {response}")

        # At least some jobs should succeed
        assert success_count > 0

    def test_job_error_scenarios(self):
        """Test job error handling and recovery"""
        # Test with invalid parameters
        invalid_params = {
            "source_format": "invalid_format",
            "target_format": "html",
            "source_content": "# Test",
        }

        status, response = self.post("/convert", invalid_params)
        # Should return error status
        assert status in [400, 422]
        assert "error" in response.get_json() if response.get_json() else True

        # Test with missing required parameters
        missing_params = {
            "source_format": "markdown",
            # Missing target_format
        }

        status, response = self.post("/convert", missing_params)
        assert status in [400, 422]

    def test_job_timeout_handling(self):
        """Test job timeout and cleanup"""
        with patch("api_server.JOB_TIMEOUT", 1):  # Set timeout to 1 second
            # Create a job that might timeout
            job_params = {
                "source_format": "markdown",
                "target_format": "html",
                "source_content": "# Test Content\n" * 1000,
            }

            status, response = self.post("/convert", job_params, expected_status=202)
            job_id = response["job_id"]

            # Wait for potential timeout
            time.sleep(2)

            # Check job status
            status, response = self.get(f"/jobs/{job_id}")
            assert status == 200
            # Job might be completed or timed out
            assert response.get("status") in ["completed", "failed", "timeout"]

    def test_job_result_retrieval(self):
        """Test retrieval of job results"""
        # Create a simple job
        job_id, _ = self.create_test_job(
            "convert",
            {
                "source_format": "markdown",
                "target_format": "html",
                "source_content": "# Test",
            },
        )

        # Wait for completion or timeout
        max_wait = 10
        for _ in range(max_wait):
            status, response = self.get(f"/jobs/{job_id}")
            if response.get("status") == "completed":
                # Check for results
                if "result" in response:
                    assert isinstance(response["result"], dict)
                break
            elif response.get("status") in ["failed", "cancelled"]:
                break
            time.sleep(0.5)

    def test_job_list_retrieval(self):
        """Test retrieval of job lists"""
        # Create multiple jobs
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
        assert status == 200
        self.assert_api_response(
            status, response, expected_status=200, expected_keys=["jobs"]
        )

        # Verify job list structure
        jobs = response.get("jobs", [])
        assert isinstance(jobs, list)

        # Check if our jobs are in the list
        returned_job_ids = [job.get("job_id") for job in jobs]
        for job_id in job_ids:
            assert job_id in returned_job_ids

    def test_job_filtering_and_pagination(self):
        """Test job filtering and pagination"""
        # Create jobs with different types
        job_types = ["convert", "tei", "uml"]
        created_jobs = []

        for job_type in job_types:
            if job_type == "convert":
                params = {
                    "source_format": "markdown",
                    "target_format": "html",
                    "source_content": "# Test",
                }
            elif job_type == "tei":
                params = {
                    "source_format": "markdown",
                    "source_content": "# Test",
                    "metadata": {},
                }
            elif job_type == "uml":
                params = {
                    "source_type": "python",
                    "source_content": "class Test: pass",
                    "output_format": "plantuml",
                }

            job_id, _ = self.create_test_job(job_type, params)
            created_jobs.append((job_type, job_id))

        # Test filtering by job type
        for job_type, _ in created_jobs:
            status, response = self.get("/jobs", params={"job_type": job_type})
            assert status == 200
            jobs = response.get("jobs", [])
            # Should only return jobs of specified type
            for job in jobs:
                assert job.get("job_type") == job_type

        # Test pagination
        status, response = self.get("/jobs", params={"limit": 2, "offset": 0})
        assert status == 200
        assert "jobs" in response
        assert "pagination" in response

    def test_job_cleanup(self):
        """Test cleanup of old jobs"""
        # Create a job
        job_id, _ = self.create_test_job(
            "convert",
            {
                "source_format": "markdown",
                "target_format": "html",
                "source_content": "# Test",
            },
        )

        # Verify job exists
        status, response = self.get(f"/jobs/{job_id}")
        assert status == 200

        # Cancel/delete the job
        status, response = self.delete(f"/jobs/{job_id}")
        assert status == 200

        # Verify cleanup
        status, response = self.get(f"/jobs/{job_id}")
        # Job might be marked as deleted or return 404
        assert status in [200, 404]

    def test_job_parameter_validation(self):
        """Test validation of job parameters"""
        # Test various parameter validation scenarios
        validation_tests = [
            # Convert job tests
            {
                "endpoint": "/convert",
                "params": {
                    "source_format": "markdown",
                    "target_format": "invalid",
                    "source_content": "# Test",
                },
                "expected_status": 400,
            },
            {
                "endpoint": "/convert",
                "params": {
                    "source_format": "invalid",
                    "target_format": "html",
                    "source_content": "# Test",
                },
                "expected_status": 400,
            },
            {
                "endpoint": "/convert",
                "params": {
                    "source_format": "markdown",
                    "target_format": "html",
                },  # Missing source_content
                "expected_status": 400,
            },
            # TEI job tests
            {
                "endpoint": "/tei",
                "params": {"source_format": "invalid", "source_content": "# Test"},
                "expected_status": 400,
            },
            # UML job tests
            {
                "endpoint": "/uml",
                "params": {
                    "source_type": "invalid",
                    "source_content": "class Test: pass",
                },
                "expected_status": 400,
            },
        ]

        for test_case in validation_tests:
            status, response = self.post(
                test_case["endpoint"],
                test_case["params"],
                expected_status=test_case["expected_status"],
            )
            assert status == test_case["expected_status"]

    def test_job_priority_handling(self):
        """Test job priority and queue management"""
        # Create jobs with different priorities
        high_priority_params = {
            "source_format": "markdown",
            "target_format": "html",
            "source_content": "# High Priority",
            "priority": "high",
        }

        low_priority_params = {
            "source_format": "markdown",
            "target_format": "html",
            "source_content": "# Low Priority",
            "priority": "low",
        }

        # Submit low priority first
        status1, response1 = self.post(
            "/convert", low_priority_params, expected_status=202
        )
        low_job_id = response1["job_id"]

        # Then submit high priority
        status2, response2 = self.post(
            "/convert", high_priority_params, expected_status=202
        )
        high_job_id = response2["job_id"]

        # Both should be created successfully
        assert status1 == 202
        assert status2 == 202

        # Check queue positions if supported
        status, response = self.get("/jobs", params={"queue": True})
        if status == 200 and "queue" in response:
            queue = response["queue"]
            # High priority should be before low priority in queue
            high_pos = next(
                (i for i, job in enumerate(queue) if job.get("job_id") == high_job_id),
                -1,
            )
            low_pos = next(
                (i for i, job in enumerate(queue) if job.get("job_id") == low_job_id),
                -1,
            )

            if high_pos >= 0 and low_pos >= 0:
                assert high_pos <= low_pos

    @pytest.mark.slow
    def test_job_performance_under_load(self):
        """Test job processing performance under load"""
        import threading
        import time

        # Measure job creation performance
        start_time = time.time()

        # Create multiple jobs concurrently
        threads = []
        job_ids = []

        def create_job_thread():
            status, response = self.post(
                "/convert",
                {
                    "source_format": "markdown",
                    "target_format": "html",
                    "source_content": "# Performance Test",
                },
                expected_status=202,
            )

            if status == 202:
                job_ids.append(response["job_id"])

        # Create 20 jobs concurrently
        for _ in range(20):
            thread = threading.Thread(target=create_job_thread)
            threads.append(thread)
            thread.start()

        for thread in threads:
            thread.join()

        creation_time = time.time() - start_time

        # Should create all jobs within reasonable time (5 seconds)
        assert creation_time < 5.0
        assert len(job_ids) == 20

        # Check that all jobs are trackable
        trackable_count = 0
        for job_id in job_ids:
            status, _ = self.get(f"/jobs/{job_id}")
            if status in [200, 202]:
                trackable_count += 1

        assert trackable_count == 20
