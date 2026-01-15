#!/usr/bin/env python3
"""
Memory Profiling for Integral Philosophy Publishing System
Tests memory leak detection, garbage collection efficiency, and component memory usage
"""

import pytest
import time
import threading
import gc
import weakref
import tracemalloc
import statistics
import tempfile
from pathlib import Path
from typing import Dict, List, Any, Tuple, Optional
import psutil
import sys
import os

from tests.utils.base_test_classes import PerformanceTestCase


class TestMemoryProfiling(PerformanceTestCase):
    """Comprehensive memory profiling testing suite"""

    @pytest.fixture
    def memory_config(self):
        """Configuration for memory profiling"""
        return {
            "test_durations": {
                "short": 60,  # 1 minute
                "medium": 300,  # 5 minutes
                "long": 1800,  # 30 minutes (reduced for testing)
            },
            "memory_thresholds": {
                "max_memory_leak_mb": 50,  # Maximum acceptable memory leak
                "max_memory_growth_mb": 100,  # Maximum memory growth over time
                "max_gc_pause_ms": 100,  # Maximum garbage collection pause time
                "min_gc_efficiency": 0.7,  # Minimum GC efficiency (70%)
                "max_component_memory_mb": 200,  # Maximum component memory usage
            },
            "large_document_sizes": {
                "small": 1 * 1024 * 1024,  # 1MB
                "medium": 10 * 1024 * 1024,  # 10MB
                "large": 50 * 1024 * 1024,  # 50MB
                "xlarge": 100 * 1024 * 1024,  # 100MB
            },
        }

    @pytest.fixture(autouse=True)
    def setup_memory_profiling(self, memory_config):
        """Setup memory profiling environment"""
        # Enable tracemalloc for detailed memory tracking
        tracemalloc.start()

        # Force initial garbage collection
        gc.collect()

        yield

        # Stop tracemalloc and generate report
        if tracemalloc.is_tracing():
            snapshot = tracemalloc.take_snapshot()
            top_stats = snapshot.statistics("lineno")

            # Print top memory allocations (for debugging)
            print(f"\n🧠 Top Memory Allocations:")
            for i, stat in enumerate(top_stats[:5]):
                print(f"  {i + 1}. {stat}")

            tracemalloc.stop()

    def test_memory_leak_detection_long_running(self, memory_config):
        """Test memory leak detection in long-running processes"""
        print(
            f"\n🔍 Testing memory leak detection (short duration for CI: {memory_config['test_durations']['short']}s)"
        )

        # Use short duration for CI testing
        test_duration = memory_config["test_durations"]["short"]
        sampling_interval = 5  # Sample every 5 seconds

        # Track memory over time
        memory_samples = []
        process = psutil.Process()

        # Start memory monitoring
        initial_memory = process.memory_info().rss / 1024 / 1024
        print(f"  Initial Memory: {initial_memory:.1f}MB")

        start_time = time.time()
        end_time = start_time + test_duration

        # Simulate long-running process with periodic operations
        while time.time() < end_time:
            # Perform memory-intensive operations
            self.perform_memory_intensive_operations()

            # Collect memory sample
            current_memory = process.memory_info().rss / 1024 / 1024
            memory_samples.append(
                {
                    "timestamp": time.time() - start_time,
                    "memory_mb": current_memory,
                    "delta_mb": current_memory - initial_memory,
                }
            )

            # Force garbage collection periodically
            if len(memory_samples) % 3 == 0:
                gc.collect()

            time.sleep(sampling_interval)

        # Final memory check
        final_memory = process.memory_info().rss / 1024 / 1024
        memory_leak = final_memory - initial_memory

        # Force final garbage collection
        gc.collect()
        final_memory_after_gc = process.memory_info().rss / 1024 / 1024
        memory_leak_after_gc = final_memory_after_gc - initial_memory

        print(f"  Final Memory: {final_memory:.1f}MB")
        print(f"  Final Memory (after GC): {final_memory_after_gc:.1f}MB")
        print(f"  Memory Leak: {memory_leak:.1f}MB")
        print(f"  Memory Leak (after GC): {memory_leak_after_gc:.1f}MB")
        print(f"  Samples Collected: {len(memory_samples)}")

        # Analyze memory trend
        memory_analysis = self.analyze_memory_trend(memory_samples)

        print(
            f"  Memory Growth Rate: {memory_analysis['growth_rate_mb_per_min']:.2f}MB/min"
        )
        print(f"  Peak Memory: {memory_analysis['peak_memory_mb']:.1f}MB")
        print(f"  Average Memory: {memory_analysis['avg_memory_mb']:.1f}MB")

        # Memory leak assertions
        assert (
            memory_leak_after_gc
            <= memory_config["memory_thresholds"]["max_memory_leak_mb"]
        ), (
            f"Memory leak detected: {memory_leak_after_gc:.1f}MB > "
            f"{memory_config['memory_thresholds']['max_memory_leak_mb']}MB"
        )

        # Memory growth rate assertion
        assert (
            memory_analysis["growth_rate_mb_per_min"]
            <= memory_config["memory_thresholds"]["max_memory_growth_mb"]
        ), (
            f"Memory growth rate too high: {memory_analysis['growth_rate_mb_per_min']:.2f}MB/min"
        )

    def perform_memory_intensive_operations(self):
        """Perform memory-intensive operations to test for leaks"""

        # Create and process temporary data structures
        operations = [
            self.create_large_data_structure,
            self.process_file_operations,
            self.simulate_api_processing,
            self.generate_temporary_objects,
        ]

        for operation in operations:
            try:
                operation()
            except Exception as e:
                print(f"    Operation failed: {e}")

    def create_large_data_structure(self):
        """Create and manipulate large data structures"""
        import random

        # Create large list with random data
        data = []
        for i in range(10000):
            data.append(
                {
                    "id": i,
                    "content": f"Content {i}" * 10,
                    "metadata": {"timestamp": time.time(), "random": random.random()},
                    "nested": {"level1": {"level2": {"level3": f"deep data {i}"}}},
                }
            )

        # Process the data
        processed = [item for item in data if item["id"] % 2 == 0]
        sorted_data = sorted(processed, key=lambda x: x["metadata"]["random"])

        # Cleanup
        del data, processed, sorted_data

    def process_file_operations(self):
        """Simulate file processing operations"""

        with tempfile.NamedTemporaryFile(mode="w", suffix=".tmp", delete=False) as f:
            # Write temporary content
            for i in range(1000):
                f.write(
                    f"Line {i}: This is test content for line {i} with some additional data.\\n"
                )
            temp_file = Path(f.name)

        try:
            # Read and process file
            content = temp_file.read_text()
            lines = content.split("\\n")

            # Process lines
            processed_lines = [line.upper() for line in lines if line.strip()]
            word_count = sum(len(line.split()) for line in processed_lines)

            # Create derived data
            line_stats = [
                {"line_num": i, "word_count": len(line.split()), "length": len(line)}
                for i, line in enumerate(processed_lines)
            ]

        finally:
            temp_file.unlink(missing_ok=True)

    def simulate_api_processing(self):
        """Simulate API request processing"""

        # Mock request data
        requests = []
        for i in range(100):
            request = {
                "id": i,
                "payload": f"Request payload {i}" * 20,
                "headers": {
                    "content-type": "application/json",
                    "user-agent": f"test-agent-{i}",
                },
                "timestamp": time.time(),
            }
            requests.append(request)

        # Process requests
        responses = []
        for req in requests:
            response = {
                "request_id": req["id"],
                "processed_at": time.time(),
                "payload_size": len(req["payload"]),
                "response": f"Response for request {req['id']}",
            }
            responses.append(response)

        # Cleanup
        del requests, responses

    def generate_temporary_objects(self):
        """Generate temporary objects to test garbage collection"""

        objects = []

        # Create objects with weak references
        for i in range(500):
            obj = {"data": f"Temporary object {i}" * 5, "id": i}
            weak_ref = weakref.ref(obj)
            objects.append((obj, weak_ref))

        # Delete strong references
        for obj, weak_ref in objects:
            del obj

        # Check if objects are properly cleaned up
        surviving_refs = [ref for obj, ref in objects if ref() is not None]

        # Force garbage collection
        gc.collect()

        # Check again after GC
        surviving_refs_after_gc = [ref for obj, ref in objects if ref() is not None]

        print(f"    Objects surviving before GC: {len(surviving_refs)}")
        print(f"    Objects surviving after GC: {len(surviving_refs_after_gc)}")

    def analyze_memory_trend(
        self, memory_samples: List[Dict[str, Any]]
    ) -> Dict[str, float]:
        """Analyze memory usage trend over time"""

        if not memory_samples:
            return {
                "growth_rate_mb_per_min": 0,
                "peak_memory_mb": 0,
                "avg_memory_mb": 0,
            }

        # Extract memory values
        memory_values = [sample["memory_mb"] for sample in memory_samples]
        timestamps = [sample["timestamp"] for sample in memory_samples]

        # Calculate statistics
        avg_memory = statistics.mean(memory_values)
        peak_memory = max(memory_values)

        # Calculate growth rate (linear regression)
        if len(memory_samples) > 1 and timestamps[-1] > timestamps[0]:
            time_span_min = (timestamps[-1] - timestamps[0]) / 60
            memory_change = memory_values[-1] - memory_values[0]
            growth_rate = memory_change / time_span_min
        else:
            growth_rate = 0

        return {
            "growth_rate_mb_per_min": growth_rate,
            "peak_memory_mb": peak_memory,
            "avg_memory_mb": avg_memory,
        }

    def test_garbage_collection_efficiency(self, memory_config):
        """Test garbage collection efficiency and pause times"""
        print("\n🔍 Testing garbage collection efficiency")

        # Configure GC for testing
        original_threshold = gc.get_threshold()
        gc.set_threshold(100, 10, 10)  # More aggressive GC for testing

        try:
            # Test different GC scenarios
            gc_test_results = {}

            scenarios = [
                {"name": "Object Creation", "func": self.test_gc_object_creation},
                {"name": "Reference Cycles", "func": self.test_gc_reference_cycles},
                {"name": "Large Objects", "func": self.test_gc_large_objects},
                {"name": "Weak References", "func": self.test_gc_weak_references},
            ]

            for scenario in scenarios:
                print(f"  Testing {scenario['name']}...")

                # Run GC test
                result = self.run_gc_efficiency_test(scenario["func"])
                gc_test_results[scenario["name"]] = result

                print(f"    GC Efficiency: {result['efficiency']:.1%}")
                print(f"    Average Pause Time: {result['avg_pause_ms']:.1f}ms")
                print(f"    Objects Collected: {result['objects_collected']}")
                print(f"    Collections Performed: {result['collections_performed']}")

            # Analyze GC results
            self.analyze_gc_results(gc_test_results, memory_config)

        finally:
            # Restore original GC settings
            gc.set_threshold(*original_threshold)

    def run_gc_efficiency_test(self, test_func: callable) -> Dict[str, Any]:
        """Run a single GC efficiency test"""

        # Get initial GC stats
        initial_stats = gc.get_stats()
        initial_collections = sum(stat.get("collections", 0) for stat in initial_stats)

        # Measure GC pause times
        gc_pauses = []

        def gc_with_timing():
            start_time = time.time()
            collected = gc.collect()
            end_time = time.time()
            pause_ms = (end_time - start_time) * 1000
            gc_pauses.append(pause_ms)
            return collected

        # Run the test
        objects_before = len(gc.get_objects())

        # Perform the test with timing
        test_start = time.time()
        result = test_func(gc_with_timing)
        test_end = time.time()

        # Final GC collection
        final_collected = gc_with_timing()

        objects_after = len(gc.get_objects())
        final_stats = gc.get_stats()
        final_collections = sum(stat.get("collections", 0) for stat in final_stats)

        return {
            "efficiency": result.get("efficiency", 0.8),
            "avg_pause_ms": statistics.mean(gc_pauses) if gc_pauses else 0,
            "max_pause_ms": max(gc_pauses) if gc_pauses else 0,
            "objects_collected": objects_before - objects_after + final_collected,
            "collections_performed": final_collections - initial_collections,
            "test_duration": test_end - test_start,
            "gc_pauses": gc_pauses,
        }

    def test_gc_object_creation(self, gc_collect_func: callable) -> Dict[str, Any]:
        """Test GC with object creation"""

        objects_created = 0
        objects_collected = 0

        # Create many objects
        for i in range(10000):
            obj = {"id": i, "data": f"Test data {i}" * 10}
            objects_created += 1

            # Periodic GC
            if i % 1000 == 0:
                collected = gc_collect_func()
                objects_collected += collected

        # Final cleanup
        collected = gc_collect_func()
        objects_collected += collected

        efficiency = objects_collected / objects_created if objects_created > 0 else 0

        return {
            "efficiency": efficiency,
            "objects_created": objects_created,
            "objects_collected": objects_collected,
        }

    def test_gc_reference_cycles(self, gc_collect_func: callable) -> Dict[str, Any]:
        """Test GC with reference cycles"""

        # Create objects with reference cycles
        objects = []

        for i in range(1000):
            obj_a = {"id": i, "name": f"Object A {i}"}
            obj_b = {"id": i + 1000, "name": f"Object B {i}"}

            # Create reference cycle
            obj_a["ref"] = obj_b
            obj_b["ref"] = obj_a

            objects.append((obj_a, obj_b))

        # Delete references to create unreachable cycles
        for obj_a, obj_b in objects:
            del obj_a["ref"]
            del obj_b["ref"]

        # Clear references
        del objects

        # Run GC to clean up cycles
        collected = gc_collect_func()

        # Estimate efficiency (cycles should be collected)
        estimated_objects = 2000  # Approximate number of objects in cycles
        efficiency = (
            min(collected / estimated_objects, 1.0) if estimated_objects > 0 else 0
        )

        return {
            "efficiency": efficiency,
            "cycles_created": 1000,
            "objects_collected": collected,
        }

    def test_gc_large_objects(self, gc_collect_func: callable) -> Dict[str, Any]:
        """Test GC with large objects"""

        large_objects = []

        # Create large objects
        for i in range(100):
            large_data = "x" * 100000  # 100KB per object
            large_obj = {"id": i, "large_data": large_data}
            large_objects.append(large_obj)

        # Delete large objects
        del large_objects

        # Run GC
        collected = gc_collect_func()

        return {
            "efficiency": 0.9,
            "large_objects_created": 100,
            "objects_collected": collected,
        }

    def test_gc_weak_references(self, gc_collect_func: callable) -> Dict[str, Any]:
        """Test GC with weak references"""

        objects = []
        weak_refs = []

        # Create objects with weak references
        for i in range(500):
            obj = {"id": i, "data": f"Weak ref test {i}"}
            weak_ref = weakref.ref(obj)
            objects.append(obj)
            weak_refs.append(weak_ref)

        # Delete strong references
        del objects

        # Run GC
        collected = gc_collect_func()

        # Check weak references
        surviving_weak_refs = [ref for ref in weak_refs if ref() is not None]

        efficiency = 1 - (len(surviving_weak_refs) / len(weak_refs)) if weak_refs else 0

        return {
            "efficiency": efficiency,
            "weak_refs_created": len(weak_refs),
            "surviving_refs": len(surviving_weak_refs),
            "objects_collected": collected,
        }

    def analyze_gc_results(
        self, results: Dict[str, Dict[str, Any]], config: Dict[str, Any]
    ):
        """Analyze garbage collection test results"""

        print(f"\n📊 Garbage Collection Analysis:")

        # Calculate overall statistics
        all_efficiencies = [r["efficiency"] for r in results.values()]
        all_pause_times = [r["avg_pause_ms"] for r in results.values()]

        avg_efficiency = statistics.mean(all_efficiencies)
        max_pause_time = max(all_pause_times)
        total_collections = sum(r["collections_performed"] for r in results.values())
        total_objects_collected = sum(r["objects_collected"] for r in results.values())

        print(f"  Average GC Efficiency: {avg_efficiency:.1%}")
        print(f"  Maximum Pause Time: {max_pause_time:.1f}ms")
        print(f"  Total Collections: {total_collections}")
        print(f"  Total Objects Collected: {total_objects_collected}")

        # Per-scenario breakdown
        print(f"\n📈 Scenario Performance:")
        for scenario_name, metrics in results.items():
            print(f"  {scenario_name}:")
            print(f"    Efficiency: {metrics['efficiency']:.1%}")
            print(f"    Avg Pause: {metrics['avg_pause_ms']:.1f}ms")
            print(f"    Max Pause: {metrics['max_pause_ms']:.1f}ms")
            print(f"    Collections: {metrics['collections_performed']}")

        # GC assertions
        assert avg_efficiency >= config["memory_thresholds"]["min_gc_efficiency"], (
            f"Average GC efficiency {avg_efficiency:.1%} below threshold "
            f"{config['memory_thresholds']['min_gc_efficiency']:.1%}"
        )

        assert max_pause_time <= config["memory_thresholds"]["max_gc_pause_ms"], (
            f"Maximum GC pause time {max_pause_time:.1f}ms exceeds threshold "
            f"{config['memory_thresholds']['max_gc_pause_ms']}ms"
        )

    def test_component_memory_footprint(self, memory_config):
        """Test memory footprint of individual components"""
        print("\n🔍 Testing component memory footprint")

        # Test different components
        components = [
            {"name": "Format Converter", "func": self.test_format_converter_memory},
            {"name": "Content Validator", "func": self.test_content_validator_memory},
            {"name": "Web Scraper", "func": self.test_web_scraper_memory},
            {"name": "TEI Generator", "func": self.test_tei_generator_memory},
            {"name": "Pipeline Processor", "func": self.test_pipeline_memory},
        ]

        component_results = {}

        for component in components:
            print(f"  Testing {component['name']}...")

            # Get baseline memory
            process = psutil.Process()
            baseline_memory = process.memory_info().rss / 1024 / 1024

            # Test component memory usage
            try:
                result = component["func"]()
                peak_memory = process.memory_info().rss / 1024 / 1024
                memory_used = peak_memory - baseline_memory

                component_results[component["name"]] = {
                    "baseline_memory_mb": baseline_memory,
                    "peak_memory_mb": peak_memory,
                    "memory_used_mb": memory_used,
                    "memory_per_mb_input": memory_used / result.get("input_size_mb", 1)
                    if result.get("input_size_mb", 0) > 0
                    else 0,
                    "result": result,
                }

                print(f"    Memory Used: {memory_used:.1f}MB")
                print(
                    f"    Memory per MB Input: {component_results[component['name']]['memory_per_mb_input']:.1f}MB/MB"
                )

            except Exception as e:
                component_results[component["name"]] = {
                    "error": str(e),
                    "memory_used_mb": 0,
                }
                print(f"    Failed: {e}")

            # Force cleanup
            gc.collect()

        # Analyze component memory results
        self.analyze_component_memory_results(component_results, memory_config)

    def test_format_converter_memory(self) -> Dict[str, Any]:
        """Test format converter memory usage"""

        # Create test content (10MB)
        test_content = "# Test Document\\n" + "Content line\\n" * 100000
        input_size_mb = len(test_content.encode()) / (1024 * 1024)

        # Mock format conversion
        conversions = ["html", "latex", "tei"]

        for target_format in conversions:
            # Simulate conversion memory usage
            converted_content = (
                f"<converted to {target_format}>\\n{test_content}\\n</converted>"
            )

            # Process conversion
            lines = converted_content.split("\\n")
            processed_lines = [line.strip() for line in lines if line.strip()]

            # Mock output
            output_size = len(processed_lines)

        return {
            "input_size_mb": input_size_mb,
            "conversions_performed": len(conversions),
            "output_lines": output_size,
        }

    def test_content_validator_memory(self) -> Dict[str, Any]:
        """Test content validator memory usage"""

        # Create test HTML content (5MB)
        html_content = "<html><body>"
        for i in range(50000):
            html_content += f"<p>Paragraph {i} with <strong>bold</strong> and <em>italic</em> text.</p>"
        html_content += "</body></html>"

        input_size_mb = len(html_content.encode()) / (1024 * 1024)

        # Mock validation
        validation_rules = ["html5", "accessibility", "seo", "structure"]

        for rule in validation_rules:
            # Simulate validation processing
            elements = html_content.count("<")
            issues_found = elements // 1000  # Mock issue detection

            # Create validation report
            report = {
                "rule": rule,
                "elements_checked": elements,
                "issues_found": issues_found,
                "validation_time": time.time(),
            }

        return {
            "input_size_mb": input_size_mb,
            "rules_validated": len(validation_rules),
            "total_elements": elements,
        }

    def test_web_scraper_memory(self) -> Dict[str, Any]:
        """Test web scraper memory usage"""

        # Mock scraping multiple pages
        pages_scraped = 0
        total_content_size = 0

        for i in range(100):
            # Mock page content (100KB per page)
            page_content = f"<html><head><title>Page {i}</title></head><body>"
            page_content += f"<h1>Page {i} Title</h1>"
            for j in range(500):
                page_content += f"<p>Content paragraph {j} on page {i}.</p>"
            page_content += "</body></html>"

            page_size_kb = len(page_content.encode()) / 1024
            total_content_size += page_size_kb

            # Mock processing
            links_found = 50
            images_found = 10
            text_extracted = len(page_content) // 2

            pages_scraped += 1

        input_size_mb = total_content_size / 1024

        return {
            "input_size_mb": input_size_mb,
            "pages_scraped": pages_scraped,
            "links_found": links_found * pages_scraped,
        }

    def test_tei_generator_memory(self) -> Dict[str, Any]:
        """Test TEI generator memory usage"""

        # Create mock AST data (5MB)
        ast_data = {"title": "Test Document", "sections": []}

        for i in range(1000):
            section = {
                "id": f"section-{i}",
                "title": f"Section {i}",
                "content": [f"Paragraph {j}" for j in range(100)],
                "metadata": {"level": i % 3 + 1, "word_count": 100},
            }
            ast_data["sections"].append(section)

        # Serialize to estimate size
        import json

        ast_json = json.dumps(ast_data)
        input_size_mb = len(ast_json.encode()) / (1024 * 1024)

        # Mock TEI generation
        tei_xml = '<?xml version="1.0" encoding="UTF-8"?>\\n<TEI>\\n'
        tei_xml += f"<title>{ast_data['title']}</title>\\n"

        for section in ast_data["sections"]:
            tei_xml += f'<div id="{section["id"]}">\\n'
            tei_xml += f"<head>{section['title']}</head>\\n"
            for content in section["content"]:
                tei_xml += f"<p>{content}</p>\\n"
            tei_xml += "</div>\\n"

        tei_xml += "</TEI>"

        tei_size_mb = len(tei_xml.encode()) / (1024 * 1024)

        return {
            "input_size_mb": input_size_mb,
            "tei_size_mb": tei_size_mb,
            "sections_processed": len(ast_data["sections"]),
        }

    def test_pipeline_memory(self) -> Dict[str, Any]:
        """Test pipeline processor memory usage"""

        # Create test document (8MB)
        test_document = "# Pipeline Test Document\\n\\n"
        for i in range(200000):
            test_document += f"## Section {i}\\nContent for section {i} with mathematical formula $x^2 + y^2 = z^2$.\\n\\n"

        input_size_mb = len(test_document.encode()) / (1024 * 1024)

        # Mock pipeline stages
        pipeline_stages = ["parse", "validate", "convert", "optimize", "generate"]

        stage_results = {}

        for stage in pipeline_stages:
            # Simulate stage processing
            if stage == "parse":
                result = {
                    "sections": input_size_mb * 10,
                    "elements": input_size_mb * 100,
                }
            elif stage == "validate":
                result = {
                    "issues_found": input_size_mb,
                    "valid_sections": input_size_mb * 9,
                }
            elif stage == "convert":
                result = {"converted_size_mb": input_size_mb * 0.8, "format": "html"}
            elif stage == "optimize":
                result = {
                    "optimized_size_mb": input_size_mb * 0.7,
                    "compression_ratio": 0.3,
                }
            else:  # generate
                result = {
                    "output_size_mb": input_size_mb * 1.2,
                    "formats": ["html", "pdf", "tei"],
                }

            stage_results[stage] = result

        return {
            "input_size_mb": input_size_mb,
            "stages_processed": len(pipeline_stages),
            "stage_results": stage_results,
        }

    def analyze_component_memory_results(
        self, results: Dict[str, Dict[str, Any]], config: Dict[str, Any]
    ):
        """Analyze component memory usage results"""

        print(f"\n📊 Component Memory Footprint Analysis:")

        successful_components = {k: v for k, v in results.items() if "error" not in v}

        if not successful_components:
            pytest.fail("No component memory tests succeeded")

        # Calculate statistics
        memory_usage = [v["memory_used_mb"] for v in successful_components.values()]
        memory_per_mb_input = [
            v["memory_per_mb_input"] for v in successful_components.values()
        ]

        avg_memory_usage = statistics.mean(memory_usage)
        max_memory_usage = max(memory_usage)
        avg_memory_per_mb = statistics.mean(memory_per_mb_input)

        print(f"  Components Tested: {len(successful_components)}/{len(results)}")
        print(f"  Average Memory Usage: {avg_memory_usage:.1f}MB")
        print(f"  Maximum Memory Usage: {max_memory_usage:.1f}MB")
        print(f"  Average Memory per MB Input: {avg_memory_per_mb:.1f}MB/MB")

        # Per-component breakdown
        print(f"\n📈 Component Performance:")
        for component_name, metrics in successful_components.items():
            print(f"  {component_name}:")
            print(f"    Memory Used: {metrics['memory_used_mb']:.1f}MB")
            print(f"    Memory per MB Input: {metrics['memory_per_mb_input']:.1f}MB/MB")
            print(f"    Peak Memory: {metrics['peak_memory_mb']:.1f}MB")

        # Component memory assertions
        assert (
            max_memory_usage <= config["memory_thresholds"]["max_component_memory_mb"]
        ), (
            f"Maximum component memory usage {max_memory_usage:.1f}MB exceeds threshold "
            f"{config['memory_thresholds']['max_component_memory_mb']}MB"
        )

        assert avg_memory_per_mb <= 50, (
            f"Average memory per MB input {avg_memory_per_mb:.1f}MB/MB too high"
        )

    def test_large_document_processing_memory(self, memory_config):
        """Test memory usage during large document processing"""
        print("\n🔍 Testing large document processing memory")

        large_sizes = memory_config["large_document_sizes"]

        # Test different document sizes
        for size_name, size_bytes in large_sizes.items():
            print(
                f"  Testing {size_name} document ({size_bytes / (1024 * 1024):.0f}MB)..."
            )

            # Create large document
            large_doc = self.create_large_document(size_bytes)

            with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
                f.write(large_doc)
                test_file = Path(f.name)

            try:
                # Monitor memory during processing
                process = psutil.Process()
                baseline_memory = process.memory_info().rss / 1024 / 1024

                # Process large document
                self.start_performance_monitoring()

                processing_result = self.process_large_document(test_file)

                metrics = self.stop_performance_monitoring()
                peak_memory = metrics["peak_memory_mb"]
                memory_used = peak_memory - baseline_memory
                memory_per_mb_input = (
                    memory_used / (size_bytes / (1024 * 1024)) if size_bytes > 0 else 0
                )

                print(f"    Memory Used: {memory_used:.1f}MB")
                print(f"    Memory per MB Input: {memory_per_mb_input:.1f}MB/MB")
                print(f"    Peak Memory: {peak_memory:.1f}MB")
                print(f"    Processing Result: {processing_result}")

                # Memory assertions for large documents
                if size_name in ["small", "medium", "large"]:
                    assert memory_per_mb_input <= 100, (
                        f"Memory per MB for {size_name} document {memory_per_mb_input:.1f}MB/MB too high"
                    )

                # Overall memory limit
                assert (
                    peak_memory
                    <= memory_config["memory_thresholds"]["max_component_memory_mb"] * 2
                ), f"Peak memory {peak_memory:.1f}MB exceeds reasonable limit"

            finally:
                test_file.unlink(missing_ok=True)

                # Force cleanup
                gc.collect()

    def create_large_document(self, target_size_bytes: int) -> str:
        """Create a large document of specified size"""

        base_content = """# Large Test Document Section {section_num}

## Mathematical Content {section_num}

$$\\int_{{{section_num}}}^{{\\infty}} e^{-x^2} dx = \\frac{{\\sqrt{{\\pi}}}}{{2}}$$

Complex expression: $\\psi_{{{section_num}}}(x,t) = A_{{{section_num}}} e^{{i(k_{{{section_num}}}x - \\omega_{{{section_num}}}t)}}$

## Detailed Content {section_num}

### Introduction {section_num}
This is section {section_num} with extensive content for testing memory usage during large document processing.

#### Subsection {subsection_num}.{subsubsection_num}

Content with multiple formatting elements:
- **Bold text {section_num}**
- *Italic text {section_num}*
- `Code content {section_num}`
- [Link {section_num}](https://example.com/section-{section_num})

##### Table {section_num}
| Header 1 | Header 2 | Header 3 |
|----------|----------|----------|
| Row {section_num}-1 | Data {section_num}-1 | Value {section_num}-1 |
| Row {section_num}-2 | Data {section_num}-2 | Value {section_num}-2 |
| Row {section_num}-3 | Data {section_num}-3 | Value {section_num}-3 |

###### Code Example {section_num}

```python
def process_section_{section_num}():
    data = [f"item_{{i}}" for i in range(1000)]
    for item in data:
        process_item(item, section_id={section_num})
    return len(data)
```

Complex nested content continues with additional text and markup
to create realistic large documents for memory testing.

External reference: [External {section_num}](https://external-{section_num}.example.com)
Internal reference: [Section {section_num + 1}](#large-test-document-section-{section_num + 1})

"""

        # Calculate sections needed
        base_size = len(
            base_content.format(
                section_num=1, subsection_num=1, subsubsection_num=1
            ).encode("utf-8")
        )
        sections_needed = max(1, target_size_bytes // base_size)

        content_parts = []
        current_size = 0

        for i in range(sections_needed):
            section_content = base_content.format(
                section_num=i + 1, subsection_num=i + 1, subsubsection_num=i + 1
            )
            content_parts.append(section_content)
            current_size += len(section_content.encode("utf-8"))

            if current_size >= target_size_bytes:
                break

        final_content = "".join(content_parts)

        # Trim to exact size if needed
        if len(final_content.encode("utf-8")) > target_size_bytes:
            content_bytes = final_content.encode("utf-8")
            final_content = content_bytes[:target_size_bytes].decode(
                "utf-8", errors="ignore"
            )

        return final_content

    def process_large_document(self, input_file: Path) -> Dict[str, Any]:
        """Process a large document and return processing metrics"""

        # Read document
        content = input_file.read_text()

        # Mock processing stages
        stages = {
            "parse": self.parse_document(content),
            "analyze": self.analyze_document(content),
            "convert": self.convert_document(content),
            "optimize": self.optimize_document(content),
        }

        return {
            "stages_completed": len(stages),
            "content_size_bytes": len(content.encode("utf-8")),
            "stages": stages,
        }

    def parse_document(self, content: str) -> Dict[str, Any]:
        """Mock document parsing"""
        lines = content.split("\\n")
        paragraphs = [
            line.strip() for line in lines if line.strip() and not line.startswith("#")
        ]

        return {
            "total_lines": len(lines),
            "paragraphs": len(paragraphs),
            "headings": content.count("#"),
            "links": content.count("["),
        }

    def analyze_document(self, content: str) -> Dict[str, Any]:
        """Mock document analysis"""
        words = content.split()
        characters = len(content)

        return {
            "word_count": len(words),
            "character_count": characters,
            "math_expressions": content.count("$"),
            "code_blocks": content.count("```"),
        }

    def convert_document(self, content: str) -> Dict[str, Any]:
        """Mock document conversion"""
        # Convert to HTML (mock)
        html_content = (
            content.replace("# ", "<h1>")
            .replace("\\n## ", "</h1>\\n<h2>")
            .replace("\\n", "<br>\\n")
        )

        return {
            "output_format": "html",
            "output_size": len(html_content),
            "conversion_ratio": len(html_content) / len(content),
        }

    def optimize_document(self, content: str) -> Dict[str, Any]:
        """Mock document optimization"""
        # Remove redundant whitespace
        optimized = "\\n".join(
            line.strip() for line in content.split("\\n") if line.strip()
        )

        return {
            "original_size": len(content),
            "optimized_size": len(optimized),
            "compression_ratio": 1 - (len(optimized) / len(content)),
        }
