#!/usr/bin/env python3
"""
Load Testing for Integral Philosophy Publishing System
Tests API performance under concurrent load conditions (10-100 concurrent requests)
"""

import pytest
import time
import threading
import concurrent.futures
import json
import statistics
from pathlib import Path
from typing import Dict, List, Any, Tuple
import requests
import psutil
import sys
import os

# Add parent directory to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", ".."))

from tests.utils.base_test_classes import PerformanceTestCase, APITestCase


class TestAPILoadPerformance(PerformanceTestCase, APITestCase):
    """Comprehensive API load testing suite"""

    @pytest.fixture
    def load_test_config(self):
        """Configuration for load testing scenarios"""
        return {
            "concurrent_users": [10, 25, 50, 75, 100],
            "test_duration_seconds": 60,
            "ramp_up_seconds": 10,
            "requests_per_user": 20,
            "endpoints": [
                "/api/v1/convert",
                "/api/v1/scrape",
                "/api/v1/tei",
                "/api/v1/uml",
                "/api/v1/pipeline",
            ],
            "performance_thresholds": {
                "max_response_time_p95": 5.0,  # seconds
                "max_response_time_p99": 10.0,  # seconds
                "min_throughput_rps": 10,  # requests per second
                "max_error_rate": 0.05,  # 5%
                "max_cpu_usage": 80.0,  # percent
                "max_memory_increase": 200.0,  # MB
            },
        }

    def create_test_payload(self, endpoint: str) -> Dict[str, Any]:
        """Create appropriate test payload for each endpoint"""
        payloads = {
            "/api/v1/convert": {
                "source_file": "sample.md",
                "source_format": "markdown",
                "target_format": "html",
                "options": {"preserve_math": True},
            },
            "/api/v1/scrape": {
                "url": "https://example.com/philosophy-article",
                "depth": 2,
                "options": {"extract_images": False},
            },
            "/api/v1/tei": {
                "source_file": "document.html",
                "metadata": {
                    "title": "Test Document",
                    "author": "Test Author",
                    "language": "en",
                },
            },
            "/api/v1/uml": {
                "source_file": "document.md",
                "diagram_type": "structure",
                "format": "plantuml",
            },
            "/api/v1/pipeline": {
                "source_file": "philosophy.tex",
                "pipeline": ["validate", "convert", "tei", "uml"],
                "options": {"quality": "high"},
            },
        }
        return payloads.get(endpoint, {"test": True})

    def single_user_simulation(
        self, user_id: int, config: Dict[str, Any]
    ) -> List[Dict[str, Any]]:
        """Simulate single user making sequential requests"""
        results = []
        session = requests.Session()
        session.headers.update(self.headers)

        for request_num in range(config["requests_per_user"]):
            for endpoint in config["endpoints"]:
                start_time = time.time()

                try:
                    payload = self.create_test_payload(endpoint)
                    response = session.post(
                        f"http://localhost:5000{endpoint}", json=payload, timeout=30
                    )

                    end_time = time.time()
                    response_time = end_time - start_time

                    results.append(
                        {
                            "user_id": user_id,
                            "request_num": request_num,
                            "endpoint": endpoint,
                            "status_code": response.status_code,
                            "response_time": response_time,
                            "success": response.status_code == 200,
                            "content_length": len(response.content)
                            if response.content
                            else 0,
                            "timestamp": start_time,
                        }
                    )

                    # Small delay between requests
                    time.sleep(0.1)

                except Exception as e:
                    end_time = time.time()
                    results.append(
                        {
                            "user_id": user_id,
                            "request_num": request_num,
                            "endpoint": endpoint,
                            "status_code": 0,
                            "response_time": end_time - start_time,
                            "success": False,
                            "error": str(e),
                            "timestamp": start_time,
                        }
                    )

        session.close()
        return results

    @pytest.mark.parametrize("concurrent_users", [10, 25, 50])
    def test_concurrent_api_load(self, load_test_config, concurrent_users):
        """Test API performance under concurrent load"""
        print(f"\n🔍 Testing concurrent API load with {concurrent_users} users")

        # Start performance monitoring
        self.start_performance_monitoring()
        start_time = time.time()

        # Configure test
        test_config = load_test_config.copy()
        test_config["requests_per_user"] = 10  # Reduce for CI testing

        # Execute concurrent user simulation
        all_results = []
        with concurrent.futures.ThreadPoolExecutor(
            max_workers=concurrent_users
        ) as executor:
            # Submit user simulations
            futures = [
                executor.submit(self.single_user_simulation, i, test_config)
                for i in range(concurrent_users)
            ]

            # Collect results
            for future in concurrent.futures.as_completed(futures):
                try:
                    user_results = future.result()
                    all_results.extend(user_results)
                except Exception as e:
                    print(f"User simulation failed: {e}")

        end_time = time.time()
        metrics = self.stop_performance_monitoring()

        # Analyze results
        self.analyze_load_results(
            all_results, start_time, end_time, load_test_config, metrics
        )

    def analyze_load_results(
        self,
        results: List[Dict[str, Any]],
        start_time: float,
        end_time: float,
        config: Dict[str, Any],
        performance_metrics: Dict[str, Any],
    ):
        """Analyze load test results and generate comprehensive metrics"""

        if not results:
            pytest.fail("No results collected from load test")

        # Calculate response time statistics
        response_times = [
            r["response_time"] for r in results if r.get("success", False)
        ]

        if not response_times:
            pytest.fail("No successful requests in load test")

        total_requests = len(results)
        successful_requests = len(response_times)
        failed_requests = total_requests - successful_requests
        error_rate = failed_requests / total_requests

        # Calculate percentiles
        response_times_sorted = sorted(response_times)
        p50 = statistics.median(response_times_sorted)
        p95 = response_times_sorted[int(len(response_times_sorted) * 0.95)]
        p99 = response_times_sorted[int(len(response_times_sorted) * 0.99)]
        avg_response_time = statistics.mean(response_times)

        # Calculate throughput
        test_duration = end_time - start_time
        throughput_rps = successful_requests / test_duration if test_duration > 0 else 0

        # Endpoint-specific analysis
        endpoint_stats = {}
        for result in results:
            endpoint = result["endpoint"]
            if endpoint not in endpoint_stats:
                endpoint_stats[endpoint] = {
                    "total": 0,
                    "successful": 0,
                    "response_times": [],
                }

            endpoint_stats[endpoint]["total"] += 1
            if result.get("success", False):
                endpoint_stats[endpoint]["successful"] += 1
                endpoint_stats[endpoint]["response_times"].append(
                    result["response_time"]
                )

        # Generate detailed report
        print(f"\n📊 Load Test Results Summary:")
        print(f"  Total Requests: {total_requests}")
        print(f"  Successful Requests: {successful_requests}")
        print(f"  Failed Requests: {failed_requests}")
        print(f"  Error Rate: {error_rate:.2%}")
        print(f"  Test Duration: {test_duration:.2f}s")
        print(f"  Throughput: {throughput_rps:.2f} RPS")
        print(f"\n📈 Response Time Statistics:")
        print(f"  Average: {avg_response_time:.3f}s")
        print(f"  50th percentile: {p50:.3f}s")
        print(f"  95th percentile: {p95:.3f}s")
        print(f"  99th percentile: {p99:.3f}s")
        print(f"\n🖥️  System Performance:")
        print(f"  Peak Memory: {performance_metrics['peak_memory_mb']:.1f}MB")
        print(f"  Average CPU: {performance_metrics['average_cpu_percent']:.1f}%")

        print(f"\n🔗 Endpoint Performance:")
        for endpoint, stats in endpoint_stats.items():
            endpoint_success_rate = (
                stats["successful"] / stats["total"] if stats["total"] > 0 else 0
            )
            endpoint_avg_time = (
                statistics.mean(stats["response_times"])
                if stats["response_times"]
                else 0
            )
            print(f"  {endpoint}:")
            print(f"    Success Rate: {endpoint_success_rate:.2%}")
            print(f"    Avg Response Time: {endpoint_avg_time:.3f}s")
            print(f"    Requests: {stats['successful']}/{stats['total']}")

        # Performance assertions
        thresholds = config["performance_thresholds"]

        # Response time assertions
        assert p95 <= thresholds["max_response_time_p95"], (
            f"95th percentile response time {p95:.3f}s exceeds threshold "
            f"{thresholds['max_response_time_p95']}s"
        )

        assert p99 <= thresholds["max_response_time_p99"], (
            f"99th percentile response time {p99:.3f}s exceeds threshold "
            f"{thresholds['max_response_time_p99']}s"
        )

        # Throughput assertions
        assert throughput_rps >= thresholds["min_throughput_rps"], (
            f"Throughput {throughput_rps:.2f} RPS below threshold "
            f"{thresholds['min_throughput_rps']} RPS"
        )

        # Error rate assertions
        assert error_rate <= thresholds["max_error_rate"], (
            f"Error rate {error_rate:.2%} exceeds threshold "
            f"{thresholds['max_error_rate']:.2%}"
        )

        # Resource usage assertions
        assert (
            performance_metrics["average_cpu_percent"] <= thresholds["max_cpu_usage"]
        ), (
            f"Average CPU usage {performance_metrics['average_cpu_percent']:.1f}% "
            f"exceeds threshold {thresholds['max_cpu_usage']}%"
        )

    def test_sustained_load_simulation(self, load_test_config):
        """Test system under sustained load for extended period"""
        print("\n🔍 Testing sustained load simulation (24-hour simulation compressed)")

        # For testing, we'll simulate 5 minutes of sustained load
        simulation_duration = 300  # 5 minutes for CI, represents 24 hours
        concurrent_users = 20
        requests_per_minute = 60

        self.start_performance_monitoring()
        start_time = time.time()

        all_results = []
        end_time = start_time + simulation_duration

        # Run sustained load simulation
        with concurrent.futures.ThreadPoolExecutor(
            max_workers=concurrent_users
        ) as executor:
            user_id = 0
            current_time = start_time

            while current_time < end_time:
                # Submit batch of requests
                futures = []
                for _ in range(requests_per_minute // concurrent_users):
                    for endpoint in load_test_config["endpoints"]:
                        future = executor.submit(
                            self.make_single_request, user_id, endpoint, current_time
                        )
                        futures.append(future)
                        user_id += 1

                # Wait for batch completion
                for future in concurrent.futures.as_completed(futures):
                    try:
                        result = future.result()
                        all_results.append(result)
                    except Exception as e:
                        print(f"Sustained load request failed: {e}")

                # Wait for next minute
                time.sleep(60)
                current_time = time.time()

        metrics = self.stop_performance_monitoring()

        # Analyze sustained load results
        self.analyze_sustained_load_results(all_results, start_time, end_time, metrics)

    def make_single_request(
        self, user_id: int, endpoint: str, timestamp: float
    ) -> Dict[str, Any]:
        """Make a single API request and return metrics"""
        request_start_time = time.time()
        try:
            payload = self.create_test_payload(endpoint)
            start_time = request_start_time

            response = requests.post(
                f"http://localhost:5000{endpoint}",
                json=payload,
                headers=self.headers,
                timeout=30,
            )

            end_time = time.time()

            return {
                "user_id": user_id,
                "endpoint": endpoint,
                "timestamp": timestamp,
                "response_time": end_time - start_time,
                "status_code": response.status_code,
                "success": response.status_code == 200,
                "content_length": len(response.content) if response.content else 0,
            }

        except Exception as e:
            end_time = time.time()
            return {
                "user_id": user_id,
                "endpoint": endpoint,
                "timestamp": timestamp,
                "response_time": end_time - request_start_time,
                "status_code": 0,
                "success": False,
                "error": str(e),
            }

    def analyze_sustained_load_results(
        self,
        results: List[Dict[str, Any]],
        start_time: float,
        end_time: float,
        metrics: Dict[str, Any],
    ):
        """Analyze sustained load test results"""

        if not results:
            pytest.fail("No results from sustained load test")

        total_duration = end_time - start_time
        successful_requests = sum(1 for r in results if r.get("success", False))
        total_requests = len(results)
        error_rate = (total_requests - successful_requests) / total_requests
        throughput_rps = successful_requests / total_duration

        # Time-based analysis
        time_buckets = {}
        bucket_size = 60  # 1 minute buckets

        for result in results:
            bucket_key = int((result["timestamp"] - start_time) // bucket_size)
            if bucket_key not in time_buckets:
                time_buckets[bucket_key] = {
                    "total": 0,
                    "successful": 0,
                    "response_times": [],
                }

            time_buckets[bucket_key]["total"] += 1
            if result.get("success", False):
                time_buckets[bucket_key]["successful"] += 1
                time_buckets[bucket_key]["response_times"].append(
                    result["response_time"]
                )

        # Calculate degradation
        successful_buckets = [
            (k, v) for k, v in time_buckets.items() if v["response_times"]
        ]
        if len(successful_buckets) > 1:
            first_bucket_avg = statistics.mean(
                successful_buckets[0][1]["response_times"]
            )
            last_bucket_avg = statistics.mean(
                successful_buckets[-1][1]["response_times"]
            )
            degradation_percent = (
                (last_bucket_avg - first_bucket_avg) / first_bucket_avg
            ) * 100
        else:
            degradation_percent = 0

        print(f"\n📊 Sustained Load Results:")
        print(f"  Duration: {total_duration:.0f}s ({total_duration / 60:.1f} minutes)")
        print(f"  Total Requests: {total_requests}")
        print(f"  Successful Requests: {successful_requests}")
        print(f"  Error Rate: {error_rate:.2%}")
        print(f"  Throughput: {throughput_rps:.2f} RPS")
        print(f"  Performance Degradation: {degradation_percent:.1f}%")
        print(f"\n🖥️  System Performance:")
        print(f"  Peak Memory: {metrics['peak_memory_mb']:.1f}MB")
        print(f"  Average CPU: {metrics['average_cpu_percent']:.1f}%")

        # Sustained load assertions
        assert error_rate <= 0.10, (
            f"Sustained load error rate {error_rate:.2%} too high"
        )
        assert degradation_percent <= 20, (
            f"Performance degradation {degradation_percent:.1f}% too high"
        )
        assert throughput_rps >= 5, (
            f"Sustained throughput {throughput_rps:.2f} RPS too low"
        )

    def test_memory_usage_during_load(self, load_test_config):
        """Test memory usage patterns during load testing"""
        print("\n🔍 Testing memory usage during concurrent load")

        concurrent_users = 50
        process = psutil.Process()

        # Monitor memory before, during, and after load
        initial_memory = process.memory_info().rss / 1024 / 1024

        self.start_performance_monitoring(process)

        # Generate load
        test_config = load_test_config.copy()
        test_config["requests_per_user"] = 15

        with concurrent.futures.ThreadPoolExecutor(
            max_workers=concurrent_users
        ) as executor:
            futures = [
                executor.submit(self.single_user_simulation, i, test_config)
                for i in range(concurrent_users)
            ]

            all_results = []
            for future in concurrent.futures.as_completed(futures):
                user_results = future.result()
                all_results.extend(user_results)

        metrics = self.stop_performance_monitoring()

        # Monitor memory after cleanup
        time.sleep(5)  # Allow for cleanup
        final_memory = process.memory_info().rss / 1024 / 1024
        memory_leak = final_memory - initial_memory

        print(f"\n💾 Memory Usage Analysis:")
        print(f"  Initial Memory: {initial_memory:.1f}MB")
        print(f"  Peak Memory: {metrics['peak_memory_mb']:.1f}MB")
        print(f"  Final Memory: {final_memory:.1f}MB")
        print(f"  Memory Increase: {memory_leak:.1f}MB")
        print(f"  Memory per Request: {memory_leak / len(all_results) * 1024:.1f}KB")

        # Memory assertions
        assert memory_leak < 100, f"Memory leak detected: {memory_leak:.1f}MB increase"
        assert metrics["peak_memory_mb"] < 500, (
            f"Peak memory too high: {metrics['peak_memory_mb']:.1f}MB"
        )

    def test_api_response_time_validation(self, load_test_config):
        """Validate API response times under different load conditions"""
        print("\n🔍 Testing API response time validation")

        load_scenarios = [
            {"users": 5, "requests_per_user": 5, "name": "Light Load"},
            {"users": 15, "requests_per_user": 10, "name": "Medium Load"},
            {"users": 30, "requests_per_user": 15, "name": "Heavy Load"},
        ]

        scenario_results = {}

        for scenario in load_scenarios:
            print(f"  Testing {scenario['name']} ({scenario['users']} users)...")

            self.start_performance_monitoring()

            test_config = load_test_config.copy()
            test_config["requests_per_user"] = scenario["requests_per_user"]

            with concurrent.futures.ThreadPoolExecutor(
                max_workers=scenario["users"]
            ) as executor:
                futures = [
                    executor.submit(self.single_user_simulation, i, test_config)
                    for i in range(scenario["users"])
                ]

                all_results = []
                for future in concurrent.futures.as_completed(futures):
                    user_results = future.result()
                    all_results.extend(user_results)

            metrics = self.stop_performance_monitoring()

            # Calculate scenario metrics
            response_times = [
                r["response_time"] for r in all_results if r.get("success", False)
            ]
            if response_times:
                scenario_results[scenario["name"]] = {
                    "avg_response_time": statistics.mean(response_times),
                    "p95_response_time": sorted(response_times)[
                        int(len(response_times) * 0.95)
                    ],
                    "p99_response_time": sorted(response_times)[
                        int(len(response_times) * 0.99)
                    ],
                    "throughput_rps": len(response_times) / metrics["duration_seconds"],
                    "error_rate": 1 - (len(response_times) / len(all_results)),
                    "peak_memory_mb": metrics["peak_memory_mb"],
                }

                print(
                    f"    Avg Response: {scenario_results[scenario['name']]['avg_response_time']:.3f}s"
                )
                print(
                    f"    P95 Response: {scenario_results[scenario['name']]['p95_response_time']:.3f}s"
                )
                print(
                    f"    Throughput: {scenario_results[scenario['name']]['throughput_rps']:.2f} RPS"
                )

        # Validate response time scaling
        light_p95 = scenario_results["Light Load"]["p95_response_time"]
        heavy_p95 = scenario_results["Heavy Load"]["p95_response_time"]
        scaling_factor = heavy_p95 / light_p95

        print(f"\n📈 Response Time Scaling Analysis:")
        print(f"  Light Load P95: {light_p95:.3f}s")
        print(f"  Heavy Load P95: {heavy_p95:.3f}s")
        print(f"  Scaling Factor: {scaling_factor:.2f}x")

        # Assertions
        assert scaling_factor < 5, (
            f"Response time scaling too high: {scaling_factor:.2f}x"
        )

        for scenario_name, metrics in scenario_results.items():
            assert metrics["p95_response_time"] < 8.0, (
                f"P95 response time too high for {scenario_name}: {metrics['p95_response_time']:.3f}s"
            )
            assert metrics["error_rate"] < 0.15, (
                f"Error rate too high for {scenario_name}: {metrics['error_rate']:.2%}"
            )
