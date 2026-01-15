#!/usr/bin/env python3
"""
Stress Testing for Integral Philosophy Publishing System
Tests system limits, resource exhaustion, and recovery scenarios
"""

import pytest
import time
import threading
import concurrent.futures
import tempfile
import json
import statistics
import gc
import random
from pathlib import Path
from typing import Dict, List, Any, Tuple
import psutil
import requests
import weakref

from tests.utils.base_test_classes import PerformanceTestCase


class TestSystemLimits(PerformanceTestCase):
    """Comprehensive system stress testing suite"""

    @pytest.fixture
    def stress_config(self):
        """Configuration for stress testing"""
        return {
            "max_concurrent_users": 200,
            "large_file_sizes": {
                "medium": 50 * 1024 * 1024,  # 50MB
                "large": 100 * 1024 * 1024,  # 100MB
                "xlarge": 200 * 1024 * 1024,  # 200MB
                "xxlarge": 500 * 1024 * 1024,  # 500MB
            },
            "stress_thresholds": {
                "max_memory_mb": 2048,  # 2GB
                "max_cpu_percent": 95,  # 95%
                "max_response_time": 30.0,  # 30 seconds
                "min_success_rate": 0.70,  # 70%
                "max_timeout_rate": 0.20,  # 20%
                "recovery_time_seconds": 60,  # Recovery within 1 minute
            },
            "resource_limits": {
                "max_open_files": 1000,
                "max_threads": 500,
                "max_memory_leak_mb": 100,
            },
        }

    def create_large_test_file(self, size_bytes: int, file_type: str = "md") -> Path:
        """Create a large test file for stress testing"""

        base_content = """# Stress Test Document Section {section_num}

This is section {section_num} of a large stress test document.

## Mathematical Content {section_num}

$$\\int_{{{section_num}}}^{{\\infty}} e^{-x^2} dx = \\frac{{\\sqrt{{\\pi}}}}{{2}}$$

The wave function for section {section_num}: $\\psi_{{{section_num}}}(x,t) = A_{{{section_num}}} e^{{i(k_{{{section_num}}}x - \\omega_{{{section_num}}}t)}}$

## Complex Content {section_num}

### Subsection {subsection_num}.{subsubsection_num}

Content with extensive formatting:
- **Bold text {section_num}**
- *Italic text {section_num}*
- `Code content {section_num}`
- [Link {section_num}](https://example.com/section-{section_num})

#### Table {section_num}

| Column 1 | Column 2 | Column 3 |
|----------|----------|----------|
| Row {section_num}-1 | Data {section_num}-1 | Value {section_num}-1 |
| Row {section_num}-2 | Data {section_num}-2 | Value {section_num}-2 |
| Row {section_num}-3 | Data {section_num}-3 | Value {section_num}-3 |

##### Code Block {section_num}

```python
def process_section_{section_num}():
    data = ["item_{i}" for i in range(1000)]
    for item in data:
        process_item(item)
    return len(data)
```

###### List {section_num}

1. Item {section_num}.1
2. Item {section_num}.2
3. Item {section_num}.3
   - Subitem {section_num}.3.1
   - Subitem {section_num}.3.2
4. Item {section_num}.4

Complex nested content for section {section_num} continues here with additional
text to increase file size and test processing capabilities under stress
conditions. This content is repeated and varied to create realistic large
documents that challenge the system's performance and stability.

External reference: [External Resource {section_num}](https://external-{section_num}.example.com)
Internal reference: [Section {section_num + 1}](#stress-test-document-section-{section_num + 1})

"""

        # Calculate how many sections needed
        base_size = len(
            base_content.format(
                section_num=1, subsection_num=1, subsubsection_num=1
            ).encode("utf-8")
        )
        sections_needed = max(1, size_bytes // base_size)

        content_parts = []
        current_size = 0

        for i in range(sections_needed):
            section_content = base_content.format(
                section_num=i + 1, subsection_num=i + 1, subsubsection_num=i + 1
            )
            content_parts.append(section_content)
            current_size += len(section_content.encode("utf-8"))

            if current_size >= size_bytes:
                break

        final_content = "".join(content_parts)

        # Trim to exact size if needed
        if len(final_content.encode("utf-8")) > size_bytes:
            content_bytes = final_content.encode("utf-8")
            final_content = content_bytes[:size_bytes].decode("utf-8", errors="ignore")

        # Create file
        if file_type == "html":
            final_content = f"<html><body><h1>Stress Test Document</h1>{final_content}</body></html>"
            suffix = ".html"
        elif file_type == "tex":
            final_content = f"\\documentclass{{article}}\\begin{{document}}{final_content}\\end{{document}}"
            suffix = ".tex"
        else:
            suffix = ".md"

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=suffix, delete=False, encoding="utf-8"
        ) as f:
            f.write(final_content)
            return Path(f.name)

    def test_maximum_concurrent_users(self, stress_config):
        """Test system with maximum concurrent user simulation"""
        print(
            f"\n🔍 Testing maximum concurrent users (up to {stress_config['max_concurrent_users']})"
        )

        # Gradually increase concurrent users to find breaking point
        user_counts = [25, 50, 75, 100, 125, 150, 175, 200]
        system_results = {}

        for concurrent_users in user_counts:
            print(f"  Testing {concurrent_users} concurrent users...")

            self.start_performance_monitoring()
            start_time = time.time()

            # Simulate concurrent user activity
            user_results = self.simulate_concurrent_users(
                concurrent_users, duration_seconds=30
            )

            end_time = time.time()
            metrics = self.stop_performance_monitoring()

            # Calculate success metrics
            total_requests = len(user_results)
            successful_requests = sum(
                1 for r in user_results if r.get("success", False)
            )
            timeout_requests = sum(1 for r in user_results if r.get("timeout", False))
            failed_requests = total_requests - successful_requests - timeout_requests

            success_rate = (
                successful_requests / total_requests if total_requests > 0 else 0
            )
            timeout_rate = (
                timeout_requests / total_requests if total_requests > 0 else 0
            )
            error_rate = failed_requests / total_requests if total_requests > 0 else 0

            # Response time statistics
            response_times = [
                r["response_time"] for r in user_results if r.get("success", False)
            ]
            avg_response_time = statistics.mean(response_times) if response_times else 0
            p95_response_time = (
                statistics.quantiles(response_times, n=20)[18]
                if len(response_times) > 20
                else 0
            )

            system_results[concurrent_users] = {
                "total_requests": total_requests,
                "successful_requests": successful_requests,
                "timeout_requests": timeout_requests,
                "failed_requests": failed_requests,
                "success_rate": success_rate,
                "timeout_rate": timeout_rate,
                "error_rate": error_rate,
                "avg_response_time": avg_response_time,
                "p95_response_time": p95_response_time,
                "peak_memory_mb": metrics["peak_memory_mb"],
                "avg_cpu_percent": metrics["average_cpu_percent"],
                "test_duration": end_time - start_time,
            }

            print(f"    Success Rate: {success_rate:.1%}")
            print(f"    Timeout Rate: {timeout_rate:.1%}")
            print(f"    Error Rate: {error_rate:.1%}")
            print(f"    Avg Response: {avg_response_time:.3f}s")
            print(f"    P95 Response: {p95_response_time:.3f}s")
            print(f"    Peak Memory: {metrics['peak_memory_mb']:.1f}MB")

            # Check if system is breaking down
            if success_rate < stress_config["stress_thresholds"]["min_success_rate"]:
                print(
                    f"    ⚠️  System breaking point detected at {concurrent_users} users"
                )
                break
            elif timeout_rate > stress_config["stress_thresholds"]["max_timeout_rate"]:
                print(f"    ⚠️  High timeout rate at {concurrent_users} users")
                break
            elif (
                metrics["peak_memory_mb"]
                > stress_config["stress_thresholds"]["max_memory_mb"]
            ):
                print(f"    ⚠️  Memory limit exceeded at {concurrent_users} users")
                break

        # Analyze concurrent user results
        self.analyze_concurrent_user_results(system_results, stress_config)

    def simulate_concurrent_users(
        self, concurrent_users: int, duration_seconds: int
    ) -> List[Dict[str, Any]]:
        """Simulate multiple concurrent users making requests"""

        def user_simulation(user_id: int) -> List[Dict[str, Any]]:
            """Simulate a single user's activity"""
            results = []
            end_time = time.time() + duration_seconds

            while time.time() < end_time:
                # Random endpoint selection
                endpoints = [
                    "/api/v1/convert",
                    "/api/v1/scrape",
                    "/api/v1/tei",
                    "/api/v1/pipeline",
                ]
                endpoint = random.choice(endpoints)

                # Create test payload
                payload = self.create_test_payload(endpoint)

                request_start = time.time()
                try:
                    # Mock API request (in real implementation, this would be actual HTTP request)
                    success, response_data = self.mock_api_request(endpoint, payload)

                    request_time = time.time() - request_start

                    results.append(
                        {
                            "user_id": user_id,
                            "endpoint": endpoint,
                            "success": success,
                            "response_time": request_time,
                            "timeout": request_time > 30.0,
                            "timestamp": request_start,
                            "response_size": len(str(response_data))
                            if response_data
                            else 0,
                        }
                    )

                except Exception as e:
                    request_time = time.time() - request_start
                    results.append(
                        {
                            "user_id": user_id,
                            "endpoint": endpoint,
                            "success": False,
                            "response_time": request_time,
                            "timeout": request_time > 30.0,
                            "error": str(e),
                            "timestamp": request_start,
                        }
                    )

                # Random delay between requests
                time.sleep(random.uniform(0.5, 2.0))

            return results

        # Run concurrent user simulations
        all_results = []
        with concurrent.futures.ThreadPoolExecutor(
            max_workers=concurrent_users
        ) as executor:
            futures = [
                executor.submit(user_simulation, i) for i in range(concurrent_users)
            ]

            for future in concurrent.futures.as_completed(futures):
                try:
                    user_results = future.result()
                    all_results.extend(user_results)
                except Exception as e:
                    print(f"User simulation failed: {e}")

        return all_results

    def create_test_payload(self, endpoint: str) -> Dict[str, Any]:
        """Create test payload for different endpoints"""
        payloads = {
            "/api/v1/convert": {
                "source_content": "# Test Content\\nSome content here",
                "source_format": "markdown",
                "target_format": "html",
                "options": {"preserve_math": True},
            },
            "/api/v1/scrape": {
                "url": f"https://example.com/test-{random.randint(1, 1000)}",
                "depth": 2,
                "options": {"extract_images": False},
            },
            "/api/v1/tei": {
                "source_content": "<html><body><h1>Test Document</h1><p>Content</p></body></html>",
                "metadata": {"title": "Test", "author": "Test Author"},
            },
            "/api/v1/pipeline": {
                "source_content": "# Pipeline Test\\nContent for pipeline processing",
                "pipeline": ["validate", "convert", "tei"],
                "options": {"quality": "high"},
            },
        }
        return payloads.get(endpoint, {"test": True})

    def mock_api_request(
        self, endpoint: str, payload: Dict[str, Any]
    ) -> Tuple[bool, Any]:
        """Mock API request for testing"""
        # Simulate processing time based on endpoint complexity
        processing_times = {
            "/api/v1/convert": 0.5,
            "/api/v1/scrape": 2.0,
            "/api/v1/tei": 0.8,
            "/api/v1/pipeline": 1.5,
        }

        base_time = processing_times.get(endpoint, 1.0)

        # Add random variation
        actual_time = base_time * random.uniform(0.5, 3.0)

        # Simulate occasional failures
        if random.random() < 0.05:  # 5% failure rate
            raise Exception(f"Simulated API failure for {endpoint}")

        # Simulate processing
        time.sleep(min(actual_time, 5.0))  # Cap at 5s for testing

        # Return mock response
        response_data = {
            "status": "completed",
            "endpoint": endpoint,
            "processed_at": time.time(),
            "result": f"Mock result for {endpoint}",
        }

        return True, response_data

    def analyze_concurrent_user_results(
        self, results: Dict[int, Dict[str, Any]], config: Dict[str, Any]
    ):
        """Analyze concurrent user stress test results"""

        print(f"\n📊 Concurrent User Stress Test Results:")

        if not results:
            pytest.fail("No concurrent user test results available")

        # Find maximum sustainable concurrent users
        sustainable_counts = [
            count
            for count, metrics in results.items()
            if (
                metrics["success_rate"]
                >= config["stress_thresholds"]["min_success_rate"]
                and metrics["timeout_rate"]
                <= config["stress_thresholds"]["max_timeout_rate"]
                and metrics["peak_memory_mb"]
                <= config["stress_thresholds"]["max_memory_mb"]
            )
        ]

        max_sustainable = max(sustainable_counts) if sustainable_counts else 0

        print(f"  Maximum Sustainable Users: {max_sustainable}")
        print(f"  Tested User Counts: {list(results.keys())}")

        # Performance summary
        if max_sustainable > 0:
            best_metrics = results[max_sustainable]
            print(f"\n📈 Performance at {max_sustainable} Users:")
            print(f"  Success Rate: {best_metrics['success_rate']:.1%}")
            print(f"  Timeout Rate: {best_metrics['timeout_rate']:.1%}")
            print(f"  Error Rate: {best_metrics['error_rate']:.1%}")
            print(f"  Avg Response Time: {best_metrics['avg_response_time']:.3f}s")
            print(f"  P95 Response Time: {best_metrics['p95_response_time']:.3f}s")
            print(f"  Peak Memory: {best_metrics['peak_memory_mb']:.1f}MB")
            print(f"  Average CPU: {best_metrics['avg_cpu_percent']:.1f}%")
            print(f"  Requests Processed: {best_metrics['total_requests']}")

        # Performance degradation analysis
        if len(results) > 1:
            sorted_counts = sorted(results.keys())
            first_count = sorted_counts[0]
            last_count = sorted_counts[-1]

            first_response = results[first_count]["avg_response_time"]
            last_response = results[last_count]["avg_response_time"]

            if first_response > 0:
                degradation_factor = last_response / first_response
                print(f"\n📉 Performance Degradation:")
                print(f"  Response time at {first_count} users: {first_response:.3f}s")
                print(f"  Response time at {last_count} users: {last_response:.3f}s")
                print(f"  Degradation factor: {degradation_factor:.2f}x")

        # Assertions
        assert max_sustainable >= 25, (
            f"System should sustain at least 25 concurrent users, only {max_sustainable} sustainable"
        )

        if max_sustainable > 0:
            best_metrics = results[max_sustainable]
            assert (
                best_metrics["p95_response_time"]
                <= config["stress_thresholds"]["max_response_time"]
            ), (
                f"P95 response time {best_metrics['p95_response_time']:.3f}s exceeds threshold "
                f"{config['stress_thresholds']['max_response_time']}s"
            )

    def test_large_file_processing_limits(self, stress_config):
        """Test large file processing limits"""
        print("\n🔍 Testing large file processing limits")

        large_files = stress_config["large_file_sizes"]
        thresholds = stress_config["stress_thresholds"]

        processing_results = {}

        for size_name, size_bytes in large_files.items():
            print(f"  Testing {size_name} file ({size_bytes / (1024 * 1024):.0f}MB)...")

            try:
                # Create large test file
                test_file = self.create_large_test_file(size_bytes, "md")
                file_size_mb = test_file.stat().st_size / (1024 * 1024)

                print(f"    Created test file: {file_size_mb:.1f}MB")

                # Monitor system resources
                self.start_performance_monitoring()
                start_time = time.time()

                # Process the large file
                success, processing_time, output_info = self.mock_large_file_processing(
                    test_file
                )

                end_time = time.time()
                metrics = self.stop_performance_monitoring()

                processing_results[size_name] = {
                    "success": success,
                    "file_size_mb": file_size_mb,
                    "processing_time": end_time - start_time,
                    "throughput_mb_per_sec": file_size_mb / (end_time - start_time)
                    if end_time > start_time
                    else 0,
                    "peak_memory_mb": metrics["peak_memory_mb"],
                    "avg_cpu_percent": metrics["average_cpu_percent"],
                    "output_info": output_info,
                    "memory_per_mb_input": metrics["peak_memory_mb"] / file_size_mb
                    if file_size_mb > 0
                    else 0,
                }

                print(
                    f"    Processing Time: {processing_results[size_name]['processing_time']:.3f}s"
                )
                print(
                    f"    Throughput: {processing_results[size_name]['throughput_mb_per_sec']:.2f} MB/s"
                )
                print(f"    Peak Memory: {metrics['peak_memory_mb']:.1f}MB")
                print(
                    f"    Memory per MB: {processing_results[size_name]['memory_per_mb_input']:.1f}MB/MB"
                )

                # Cleanup
                test_file.unlink(missing_ok=True)

                # Check if we've hit resource limits
                if metrics["peak_memory_mb"] > thresholds["max_memory_mb"]:
                    print(f"    ⚠️  Memory limit exceeded for {size_name} file")
                    break
                elif end_time - start_time > thresholds["max_response_time"]:
                    print(f"    ⚠️  Response time limit exceeded for {size_name} file")
                    break

            except Exception as e:
                processing_results[size_name] = {
                    "success": False,
                    "error": str(e),
                    "file_size_mb": size_bytes / (1024 * 1024),
                }
                print(f"    Failed: {e}")

        # Analyze large file processing results
        self.analyze_large_file_results(processing_results, stress_config)

    def mock_large_file_processing(
        self, input_file: Path
    ) -> Tuple[bool, float, Dict[str, Any]]:
        """Mock large file processing for testing"""
        file_size_mb = input_file.stat().st_size / (1024 * 1024)

        # Simulate processing time proportional to file size
        processing_time = file_size_mb * 0.1  # 0.1s per MB
        actual_processing_time = min(processing_time, 10.0)  # Cap at 10s for testing

        time.sleep(actual_processing_time)

        # Simulate occasional failures for very large files
        if (
            file_size_mb > 300 and random.random() < 0.1
        ):  # 10% failure for files > 300MB
            raise Exception("File too large for processing")

        output_info = {
            "lines_processed": file_size_mb * 100,  # Mock: 100 lines per MB
            "sections_extracted": file_size_mb * 2,  # Mock: 2 sections per MB
            "math_elements_processed": file_size_mb * 5,  # Mock: 5 math elements per MB
            "output_size_bytes": int(
                file_size_mb * 1024 * 1024 * 0.8
            ),  # 80% of input size
        }

        return True, processing_time, output_info

    def analyze_large_file_results(
        self, results: Dict[str, Dict[str, Any]], config: Dict[str, Any]
    ):
        """Analyze large file processing results"""

        print(f"\n📊 Large File Processing Results:")

        successful_results = {
            k: v for k, v in results.items() if v.get("success", False)
        }

        if not successful_results:
            pytest.fail("No large file processing succeeded")

        # Find maximum processable file size
        max_size_name = max(
            successful_results.keys(),
            key=lambda k: successful_results[k]["file_size_mb"],
        )
        max_size_mb = successful_results[max_size_name]["file_size_mb"]

        print(f"  Maximum Processable File: {max_size_name} ({max_size_mb:.0f}MB)")
        print(
            f"  Successfully Processed: {len(successful_results)}/{len(results)} files"
        )

        # Performance analysis
        throughputs = [r["throughput_mb_per_sec"] for r in successful_results.values()]
        memory_per_mb = [r["memory_per_mb_input"] for r in successful_results.values()]

        avg_throughput = statistics.mean(throughputs)
        avg_memory_per_mb = statistics.mean(memory_per_mb)

        print(f"\n📈 Performance Metrics:")
        print(f"  Average Throughput: {avg_throughput:.2f} MB/s")
        print(f"  Average Memory per MB: {avg_memory_per_mb:.1f}MB/MB")

        # Per-file breakdown
        print(f"\n📊 Per-File Performance:")
        for size_name, metrics in successful_results.items():
            print(f"  {size_name}:")
            print(f"    File Size: {metrics['file_size_mb']:.1f}MB")
            print(f"    Processing Time: {metrics['processing_time']:.3f}s")
            print(f"    Throughput: {metrics['throughput_mb_per_sec']:.2f} MB/s")
            print(f"    Memory per MB: {metrics['memory_per_mb_input']:.1f}MB/MB")

        # Assertions
        assert max_size_mb >= 50, (
            f"Should process at least 50MB files, only {max_size_mb:.0f}MB processed"
        )
        assert avg_throughput >= 1.0, (
            f"Average throughput {avg_throughput:.2f} MB/s too low"
        )
        assert avg_memory_per_mb <= 100, (
            f"Average memory per MB {avg_memory_per_mb:.1f}MB/MB too high"
        )

    def test_resource_exhaustion_scenarios(self, stress_config):
        """Test system behavior under resource exhaustion"""
        print("\n🔍 Testing resource exhaustion scenarios")

        scenarios = [
            {"name": "Memory Exhaustion", "type": "memory", "stress_level": 0.9},
            {"name": "CPU Exhaustion", "type": "cpu", "stress_level": 0.95},
            {"name": "File Handle Exhaustion", "type": "files", "stress_level": 0.8},
            {"name": "Thread Exhaustion", "type": "threads", "stress_level": 0.85},
        ]

        scenario_results = {}

        for scenario in scenarios:
            print(f"  Testing {scenario['name']}...")

            try:
                # Monitor system before stress
                initial_state = self.get_system_state()

                # Apply stress
                self.start_performance_monitoring()
                start_time = time.time()

                stress_result = self.apply_resource_stress(
                    scenario["type"], scenario["stress_level"]
                )

                end_time = time.time()
                metrics = self.stop_performance_monitoring()

                # Monitor recovery
                recovery_start = time.time()
                recovery_success = self.monitor_recovery(
                    scenario["type"],
                    stress_config["stress_thresholds"]["recovery_time_seconds"],
                )
                recovery_time = time.time() - recovery_start

                # Monitor system after stress and recovery
                final_state = self.get_system_state()

                scenario_results[scenario["name"]] = {
                    "stress_applied": True,
                    "stress_result": stress_result,
                    "stress_duration": end_time - start_time,
                    "recovery_success": recovery_success,
                    "recovery_time": recovery_time,
                    "initial_state": initial_state,
                    "final_state": final_state,
                    "peak_memory_mb": metrics["peak_memory_mb"],
                    "avg_cpu_percent": metrics["average_cpu_percent"],
                    "system_recovered": recovery_success
                    and recovery_time
                    <= stress_config["stress_thresholds"]["recovery_time_seconds"],
                }

                print(
                    f"    Stress Duration: {scenario_results[scenario['name']]['stress_duration']:.1f}s"
                )
                print(f"    Recovery Time: {recovery_time:.1f}s")
                print(
                    f"    System Recovered: {scenario_results[scenario['name']]['system_recovered']}"
                )

                # Cleanup resources
                self.cleanup_resources(scenario["type"])

            except Exception as e:
                scenario_results[scenario["name"]] = {
                    "stress_applied": False,
                    "error": str(e),
                }
                print(f"    Failed: {e}")

        # Analyze resource exhaustion results
        self.analyze_resource_exhaustion_results(scenario_results, stress_config)

    def get_system_state(self) -> Dict[str, Any]:
        """Get current system state"""
        process = psutil.Process()
        return {
            "memory_mb": process.memory_info().rss / 1024 / 1024,
            "cpu_percent": process.cpu_percent(),
            "open_files": len(process.open_files()),
            "threads": process.num_threads(),
            "system_memory_percent": psutil.virtual_memory().percent,
            "system_cpu_percent": psutil.cpu_percent(),
        }

    def apply_resource_stress(
        self, stress_type: str, stress_level: float
    ) -> Dict[str, Any]:
        """Apply resource stress to the system"""

        if stress_type == "memory":
            # Consume memory
            memory_blocks = []
            target_memory_mb = (
                psutil.virtual_memory().total * stress_level / (1024 * 1024 * 1024)
            )

            try:
                while (
                    psutil.Process().memory_info().rss / 1024 / 1024 < target_memory_mb
                ):
                    # Allocate 10MB blocks
                    block = bytearray(10 * 1024 * 1024)
                    memory_blocks.append(block)

                    if len(memory_blocks) > 200:  # Safety limit
                        break

                return {
                    "memory_allocated_mb": len(memory_blocks) * 10,
                    "blocks_count": len(memory_blocks),
                }

            except MemoryError:
                return {
                    "memory_allocated_mb": len(memory_blocks) * 10,
                    "blocks_count": len(memory_blocks),
                    "error": "MemoryError",
                }

        elif stress_type == "cpu":
            # CPU intensive computation
            def cpu_intensive():
                end_time = time.time() + 10  # Run for 10 seconds
                while time.time() < end_time:
                    # Compute-intensive operation
                    sum(i * i for i in range(10000))

            # Start multiple threads
            threads = []
            for _ in range(psutil.cpu_count() * 2):
                thread = threading.Thread(target=cpu_intensive)
                thread.start()
                threads.append(thread)

            # Wait for completion
            for thread in threads:
                thread.join()

            return {"threads_created": len(threads), "duration_seconds": 10}

        elif stress_type == "files":
            # Open many files
            temp_files = []
            try:
                for i in range(500):  # Try to open 500 files
                    temp_file = tempfile.NamedTemporaryFile(mode="w", delete=False)
                    temp_file.write(f"Test file {i} content\\n" * 100)
                    temp_file.flush()
                    temp_files.append(temp_file)

                    if i >= 400:  # Safety limit
                        break

                return {"files_opened": len(temp_files)}

            except OSError:
                return {"files_opened": len(temp_files), "error": "OSError"}

        elif stress_type == "threads":
            # Create many threads
            def dummy_work():
                time.sleep(1)  # Work for 1 second

            threads = []
            try:
                for i in range(200):  # Try to create 200 threads
                    thread = threading.Thread(target=dummy_work)
                    thread.start()
                    threads.append(thread)

                    if i >= 150:  # Safety limit
                        break

                # Wait for completion
                for thread in threads:
                    thread.join()

                return {"threads_created": len(threads)}

            except Exception:
                return {
                    "threads_created": len(threads),
                    "error": "Thread creation failed",
                }

        return {"error": "Unknown stress type"}

    def monitor_recovery(self, stress_type: str, timeout_seconds: int) -> bool:
        """Monitor system recovery after stress"""

        if stress_type == "memory":
            # Force garbage collection to recover memory
            gc.collect()
            time.sleep(2)  # Allow memory to be reclaimed

        elif stress_type in ["cpu", "threads"]:
            # CPU and threads recover automatically
            time.sleep(1)

        # Check if system has recovered to acceptable state
        final_state = self.get_system_state()

        if stress_type == "memory":
            return final_state["system_memory_percent"] < 90
        elif stress_type == "cpu":
            return final_state["system_cpu_percent"] < 80
        elif stress_type == "files":
            return final_state["open_files"] < 100
        elif stress_type == "threads":
            return final_state["threads"] < 50

        return True

    def cleanup_resources(self, stress_type: str):
        """Clean up resources after stress test"""

        if stress_type == "memory":
            # Force garbage collection
            gc.collect()

        elif stress_type == "files":
            # Close temporary files
            import gc

            gc.collect()  # Help with file handle cleanup

    def analyze_resource_exhaustion_results(
        self, results: Dict[str, Dict[str, Any]], config: Dict[str, Any]
    ):
        """Analyze resource exhaustion test results"""

        print(f"\n📊 Resource Exhaustion Results:")

        successful_scenarios = {
            k: v for k, v in results.items() if v.get("stress_applied", False)
        }

        if not successful_scenarios:
            pytest.fail("No resource exhaustion scenarios could be applied")

        recovered_scenarios = {
            k: v
            for k, v in successful_scenarios.items()
            if v.get("system_recovered", False)
        }

        print(f"  Scenarios Tested: {len(successful_scenarios)}")
        print(f"  Scenarios Recovered: {len(recovered_scenarios)}")
        print(
            f"  Recovery Rate: {len(recovered_scenarios) / len(successful_scenarios):.1%}"
        )

        # Per-scenario analysis
        print(f"\n📈 Scenario Performance:")
        for scenario_name, metrics in successful_scenarios.items():
            recovered = metrics.get("system_recovered", False)
            recovery_time = metrics.get("recovery_time", 0)

            print(f"  {scenario_name}:")
            print(f"    Stress Applied: {metrics.get('stress_applied', False)}")
            print(f"    System Recovered: {recovered}")
            if recovered:
                print(f"    Recovery Time: {recovery_time:.1f}s")

            if "initial_state" in metrics and "final_state" in metrics:
                initial_memory = metrics["initial_state"]["memory_mb"]
                final_memory = metrics["final_state"]["memory_mb"]
                memory_delta = final_memory - initial_memory
                print(f"    Memory Delta: {memory_delta:+.1f}MB")

        # Recovery assertions
        recovery_rate = len(recovered_scenarios) / len(successful_scenarios)
        assert recovery_rate >= 0.5, f"Recovery rate {recovery_rate:.1%} too low"

        # Individual scenario recovery assertions
        for scenario_name, metrics in successful_scenarios.items():
            if scenario_name in ["Memory Exhaustion", "CPU Exhaustion"]:
                # Critical scenarios should recover
                assert metrics.get("system_recovered", False), (
                    f"Critical scenario {scenario_name} did not recover"
                )

    def test_system_recovery_under_stress(self, stress_config):
        """Test system recovery capabilities under continuous stress"""
        print("\n🔍 Testing system recovery under continuous stress")

        recovery_scenarios = [
            {"name": "API Service Recovery", "stress_type": "api_overload"},
            {"name": "Database Recovery", "stress_type": "db_overload"},
            {"name": "File System Recovery", "stress_type": "fs_overload"},
            {"name": "Memory Recovery", "stress_type": "memory_pressure"},
        ]

        recovery_results = {}

        for scenario in recovery_scenarios:
            print(f"  Testing {scenario['name']}...")

            try:
                # Apply continuous stress
                self.start_performance_monitoring()

                # Measure baseline performance
                baseline_metrics = self.measure_baseline_performance()

                # Apply stress and cause failure
                stress_result = self.apply_continuous_stress(
                    scenario["stress_type"], duration_seconds=30
                )

                # Wait for recovery period
                time.sleep(stress_config["stress_thresholds"]["recovery_time_seconds"])

                # Measure recovery performance
                recovery_metrics = self.measure_baseline_performance()

                metrics = self.stop_performance_monitoring()

                # Calculate recovery quality
                recovery_quality = self.calculate_recovery_quality(
                    baseline_metrics, recovery_metrics
                )

                recovery_results[scenario["name"]] = {
                    "baseline_metrics": baseline_metrics,
                    "recovery_metrics": recovery_metrics,
                    "recovery_quality": recovery_quality,
                    "stress_result": stress_result,
                    "peak_memory_mb": metrics["peak_memory_mb"],
                    "recovery_successful": recovery_quality
                    >= 0.8,  # 80% recovery considered successful
                }

                print(f"    Recovery Quality: {recovery_quality:.1%}")
                print(
                    f"    Recovery Successful: {recovery_results[scenario['name']]['recovery_successful']}"
                )

            except Exception as e:
                recovery_results[scenario["name"]] = {
                    "error": str(e),
                    "recovery_successful": False,
                }
                print(f"    Failed: {e}")

        # Analyze recovery results
        self.analyze_recovery_results(recovery_results, stress_config)

    def measure_baseline_performance(self) -> Dict[str, float]:
        """Measure baseline system performance"""

        # Quick performance test
        test_operations = [
            ("memory_test", lambda: self.quick_memory_test()),
            ("cpu_test", lambda: self.quick_cpu_test()),
            ("io_test", lambda: self.quick_io_test()),
        ]

        performance_metrics = {}

        for test_name, test_func in test_operations:
            start_time = time.time()
            try:
                result = test_func()
                end_time = time.time()
                performance_metrics[test_name] = {
                    "duration": end_time - start_time,
                    "result": result,
                    "success": True,
                }
            except Exception as e:
                end_time = time.time()
                performance_metrics[test_name] = {
                    "duration": end_time - start_time,
                    "error": str(e),
                    "success": False,
                }

        return performance_metrics

    def quick_memory_test(self) -> Dict[str, int]:
        """Quick memory performance test"""
        import random

        # Allocate and process some data
        data = [random.randint(1, 1000) for _ in range(10000)]
        result = {
            "sum": sum(data),
            "max": max(data),
            "min": min(data),
            "avg": sum(data) / len(data),
        }
        return result

    def quick_cpu_test(self) -> int:
        """Quick CPU performance test"""
        total = sum(i * i for i in range(1000))
        return total

    def quick_io_test(self) -> int:
        """Quick I/O performance test"""
        with tempfile.NamedTemporaryFile(mode="w", delete=True) as f:
            test_content = "Test content\n" * 1000
            f.write(test_content)
            f.flush()
            return len(test_content)

    def apply_continuous_stress(
        self, stress_type: str, duration_seconds: int
    ) -> Dict[str, Any]:
        """Apply continuous stress to the system"""

        end_time = time.time() + duration_seconds

        if stress_type == "api_overload":
            # Simulate API overload with concurrent requests
            def api_stress():
                while time.time() < end_time:
                    self.mock_api_request("/api/v1/convert", {"test": True})
                    time.sleep(0.1)

            threads = []
            for _ in range(20):  # 20 concurrent threads
                thread = threading.Thread(target=api_stress)
                thread.start()
                threads.append(thread)

            for thread in threads:
                thread.join()

            return {
                "stress_type": "api_overload",
                "threads": 20,
                "duration": duration_seconds,
            }

        elif stress_type == "memory_pressure":
            # Continuous memory allocation and deallocation
            while time.time() < end_time:
                # Allocate memory
                memory_blocks = []
                for _ in range(10):
                    memory_blocks.append(bytearray(1024 * 1024))  # 1MB blocks

                # Hold briefly then release
                time.sleep(0.5)
                del memory_blocks
                gc.collect()

        return {"stress_type": stress_type, "duration": duration_seconds}

    def calculate_recovery_quality(
        self, baseline: Dict[str, Any], recovery: Dict[str, Any]
    ) -> float:
        """Calculate recovery quality as percentage of baseline performance"""

        quality_scores = []

        for test_name in baseline.keys():
            if baseline[test_name]["success"] and recovery.get(test_name, {}).get(
                "success", False
            ):
                baseline_duration = baseline[test_name]["duration"]
                recovery_duration = recovery[test_name]["duration"]

                # Recovery quality based on performance similarity (within 20% considered good)
                if baseline_duration > 0:
                    ratio = recovery_duration / baseline_duration
                    if 0.8 <= ratio <= 1.2:
                        quality_scores.append(1.0)
                    elif 0.5 <= ratio <= 2.0:
                        quality_scores.append(0.8)
                    else:
                        quality_scores.append(0.5)
                else:
                    quality_scores.append(0.0)
            else:
                quality_scores.append(0.0)

        return sum(quality_scores) / len(quality_scores) if quality_scores else 0.0

    def analyze_recovery_results(
        self, results: Dict[str, Dict[str, Any]], config: Dict[str, Any]
    ):
        """Analyze system recovery test results"""

        print(f"\n📊 System Recovery Results:")

        successful_scenarios = {
            k: v for k, v in results.items() if v.get("recovery_successful", False)
        }

        total_scenarios = len(results)
        successful_recoveries = len(successful_scenarios)
        recovery_rate = (
            successful_recoveries / total_scenarios if total_scenarios > 0 else 0
        )

        print(f"  Total Scenarios: {total_scenarios}")
        print(f"  Successful Recoveries: {successful_recoveries}")
        print(f"  Recovery Rate: {recovery_rate:.1%}")

        # Per-scenario breakdown
        print(f"\n📈 Per-Scenario Recovery:")
        for scenario_name, metrics in results.items():
            recovery_quality = metrics.get("recovery_quality", 0)
            recovery_successful = metrics.get("recovery_successful", False)

            print(f"  {scenario_name}:")
            print(f"    Recovery Quality: {recovery_quality:.1%}")
            print(f"    Recovery Successful: {recovery_successful}")

            if "baseline_metrics" in metrics and "recovery_metrics" in metrics:
                baseline = metrics["baseline_metrics"]
                recovery = metrics["recovery_metrics"]

                for test_name in baseline.keys():
                    if baseline[test_name]["success"] and recovery.get(
                        test_name, {}
                    ).get("success", False):
                        baseline_time = baseline[test_name]["duration"]
                        recovery_time = recovery[test_name]["duration"]
                        performance_ratio = (
                            recovery_time / baseline_time if baseline_time > 0 else 0
                        )
                        print(
                            f"    {test_name}: {performance_ratio:.2f}x baseline performance"
                        )

        # Recovery assertions
        assert recovery_rate >= 0.5, f"System recovery rate {recovery_rate:.1%} too low"

        # Critical scenarios must recover
        critical_scenarios = ["API Service Recovery", "Memory Recovery"]
        for scenario in critical_scenarios:
            if scenario in results:
                assert results[scenario].get("recovery_successful", False), (
                    f"Critical scenario {scenario} failed to recover"
                )
