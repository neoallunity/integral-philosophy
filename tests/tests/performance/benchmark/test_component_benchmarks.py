#!/usr/bin/env python3
"""
Benchmark Testing for Integral Philosophy Publishing System
Tests component performance metrics and speed benchmarks
"""

import pytest
import time
import statistics
import tempfile
import json
from pathlib import Path
from typing import Dict, List, Any, Tuple
import psutil
import threading
from concurrent.futures import ThreadPoolExecutor

from tests.utils.base_test_classes import PerformanceTestCase


class TestComponentBenchmarks(PerformanceTestCase):
    """Comprehensive component benchmark testing suite"""

    @pytest.fixture
    def benchmark_config(self):
        """Configuration for benchmark testing"""
        return {
            "test_file_sizes": {
                "small": 1024,  # 1KB
                "medium": 10240,  # 10KB
                "large": 102400,  # 100KB
                "xlarge": 1024000,  # 1MB
            },
            "performance_baselines": {
                "format_conversion": {
                    "max_time_per_kb": 0.01,  # seconds per KB
                    "max_memory_per_mb": 50,  # MB per MB of input
                    "min_throughput_mb_per_sec": 10,
                },
                "content_processing": {
                    "max_time_per_kb": 0.005,
                    "max_memory_per_mb": 30,
                    "min_throughput_mb_per_sec": 20,
                },
                "web_scraping": {
                    "max_time_per_page": 5.0,
                    "max_memory_per_page": 20,
                    "min_success_rate": 0.8,
                },
                "tei_generation": {
                    "max_time_per_kb": 0.02,
                    "max_memory_per_mb": 40,
                    "min_throughput_mb_per_sec": 5,
                },
            },
        }

    def create_test_content(self, size_name: str, size_bytes: int) -> str:
        """Create test content of specified size"""

        base_content = """# Benchmark Test Document

## Mathematical Content

This document tests performance with mathematical formulas:

$$\\int_{0}^{\\infty} e^{-x^2} dx = \\frac{\\sqrt{\\pi}}{2}$$

The wave function: $\\psi(x,t) = A e^{i(kx - \\omega t)}$

## Structure Test

### Section {section_num}
Content with some **bold** and *italic* text.

#### Subsection {subsection_num}
List of items:
1. Item one
2. Item two  
3. Item three

## Code Test

```python
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)
```

## Complex Structure

| Column 1 | Column 2 | Column 3 |
|----------|----------|----------|
| Row 1    | Data 1   | Value 1  |
| Row 2    | Data 2   | Value 2  |

External link: [Example](https://example.com)
Internal link: [Section 1](#section-1)

"""

        # Calculate how many sections needed
        base_size = len(base_content.encode("utf-8"))
        sections_needed = max(1, size_bytes // base_size)

        content_parts = []
        current_size = 0

        for i in range(sections_needed):
            section_content = base_content.format(
                section_num=i + 1, subsection_num=i + 1
            )
            content_parts.append(section_content)
            current_size += len(section_content.encode("utf-8"))

            if current_size >= size_bytes:
                break

        final_content = "".join(content_parts)

        # Trim to exact size if needed
        if len(final_content.encode("utf-8")) > size_bytes:
            content_bytes = final_content.encode("utf-8")
            return content_bytes[:size_bytes].decode("utf-8", errors="ignore")

        return final_content

    @pytest.mark.parametrize("size_name", ["small", "medium", "large"])
    def test_format_conversion_speed(self, benchmark_config, size_name):
        """Benchmark format conversion speed"""
        print(f"\n🔍 Testing format conversion speed for {size_name} files")

        size_bytes = benchmark_config["test_file_sizes"][size_name]
        baselines = benchmark_config["performance_baselines"]["format_conversion"]

        # Create test content
        test_content = self.create_test_content(size_name, size_bytes)

        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(test_content)
            test_file = Path(f.name)

        try:
            # Test different format conversions
            formats_to_test = ["html", "latex", "org", "asciidoc", "rst"]
            conversion_results = {}

            for target_format in formats_to_test:
                print(f"  Testing conversion to {target_format}...")

                # Monitor performance
                self.start_performance_monitoring()
                start_time = time.time()

                try:
                    # Mock format conversion (in real implementation, this would call actual converter)
                    success, output_path = self.mock_format_conversion(
                        test_file, target_format
                    )

                    end_time = time.time()
                    metrics = self.stop_performance_monitoring()

                    conversion_time = end_time - start_time
                    throughput_mb_per_sec = (
                        (size_bytes / (1024 * 1024)) / conversion_time
                        if conversion_time > 0
                        else 0
                    )
                    time_per_kb = (
                        conversion_time / (size_bytes / 1024) if size_bytes > 0 else 0
                    )
                    memory_per_mb = (
                        metrics["peak_memory_mb"] / (size_bytes / (1024 * 1024))
                        if size_bytes > 0
                        else 0
                    )

                    conversion_results[target_format] = {
                        "success": success,
                        "time_seconds": conversion_time,
                        "throughput_mb_per_sec": throughput_mb_per_sec,
                        "time_per_kb": time_per_kb,
                        "memory_per_mb": memory_per_mb,
                        "peak_memory_mb": metrics["peak_memory_mb"],
                        "output_size_kb": Path(output_path).stat().st_size / 1024
                        if success
                        else 0,
                    }

                    print(f"    Time: {conversion_time:.3f}s")
                    print(f"    Throughput: {throughput_mb_per_sec:.2f} MB/s")
                    print(f"    Memory: {metrics['peak_memory_mb']:.1f}MB")

                except Exception as e:
                    end_time = time.time()
                    conversion_time = end_time - start_time
                    conversion_results[target_format] = {
                        "success": False,
                        "error": str(e),
                        "time_seconds": conversion_time,
                    }
                    print(f"    Failed: {e}")

            # Analyze results
            self.analyze_conversion_results(
                conversion_results, size_name, baselines, size_bytes
            )

        finally:
            test_file.unlink(missing_ok=True)

    def mock_format_conversion(
        self, input_file: Path, target_format: str
    ) -> Tuple[bool, Path]:
        """Mock format conversion for testing"""
        # Simulate conversion work based on file size and format complexity
        input_size = input_file.stat().st_size

        # Different formats have different processing times
        complexity_factors = {
            "html": 1.0,
            "latex": 1.5,
            "org": 0.8,
            "asciidoc": 1.2,
            "rst": 0.9,
        }

        complexity = complexity_factors.get(target_format, 1.0)

        # Simulate processing time (proportional to file size)
        processing_time = (
            (input_size / 1024 / 1024) * 0.1 * complexity
        )  # 0.1s per MB * complexity
        time.sleep(min(processing_time, 0.5))  # Cap at 0.5s for testing

        # Create mock output file
        output_file = input_file.with_suffix(f".{target_format}")
        output_content = (
            f"<!-- Converted to {target_format} -->\\n{input_file.read_text()}"
        )
        output_file.write_text(output_content)

        return True, output_file

    def analyze_conversion_results(
        self,
        results: Dict[str, Dict[str, Any]],
        size_name: str,
        baselines: Dict[str, Any],
        input_size_bytes: int,
    ):
        """Analyze format conversion benchmark results"""

        print(f"\n📊 Format Conversion Results for {size_name} files:")

        successful_conversions = [
            r for r in results.values() if r.get("success", False)
        ]

        if not successful_conversions:
            pytest.fail("No format conversions succeeded")

        # Calculate statistics
        conversion_times = [r["time_seconds"] for r in successful_conversions]
        throughputs = [r["throughput_mb_per_sec"] for r in successful_conversions]
        time_per_kb_values = [r["time_per_kb"] for r in successful_conversions]

        avg_time = statistics.mean(conversion_times)
        min_time = min(conversion_times)
        max_time = max(conversion_times)
        avg_throughput = statistics.mean(throughputs)
        avg_time_per_kb = statistics.mean(time_per_kb_values)

        input_size_mb = input_size_bytes / (1024 * 1024)

        print(f"  Input Size: {input_size_mb:.2f}MB")
        print(f"  Successful Conversions: {len(successful_conversions)}/{len(results)}")
        print(f"  Average Time: {avg_time:.3f}s")
        print(f"  Time Range: {min_time:.3f}s - {max_time:.3f}s")
        print(f"  Average Throughput: {avg_throughput:.2f} MB/s")
        print(f"  Average Time per KB: {avg_time_per_kb:.4f}s")

        # Per-format breakdown
        print(f"\n📈 Per-Format Performance:")
        for format_name, result in results.items():
            if result.get("success", False):
                print(f"  {format_name}:")
                print(f"    Time: {result['time_seconds']:.3f}s")
                print(f"    Throughput: {result['throughput_mb_per_sec']:.2f} MB/s")
                print(f"    Memory: {result['memory_per_mb']:.1f}MB per MB")
                print(f"    Output Size: {result['output_size_kb']:.1f}KB")

        # Performance assertions
        assert avg_time_per_kb <= baselines["max_time_per_kb"], (
            f"Average time per KB {avg_time_per_kb:.4f}s exceeds baseline "
            f"{baselines['max_time_per_kb']:.4f}s"
        )

        assert avg_throughput >= baselines["min_throughput_mb_per_sec"], (
            f"Average throughput {avg_throughput:.2f} MB/s below baseline "
            f"{baselines['min_throughput_mb_per_sec']} MB/s"
        )

        # Memory usage assertions
        for format_name, result in results.items():
            if result.get("success", False):
                assert result["memory_per_mb"] <= baselines["max_memory_per_mb"], (
                    f"Memory usage for {format_name} {result['memory_per_mb']:.1f}MB/MB "
                    f"exceeds baseline {baselines['max_memory_per_mb']}MB/MB"
                )

    @pytest.mark.parametrize("size_name", ["small", "medium", "large"])
    def test_content_processing_throughput(self, benchmark_config, size_name):
        """Test content processing throughput measurement"""
        print(f"\n🔍 Testing content processing throughput for {size_name} files")

        size_bytes = benchmark_config["test_file_sizes"][size_name]
        baselines = benchmark_config["performance_baselines"]["content_processing"]

        # Create test content
        test_content = self.create_test_content(size_name, size_bytes)

        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(test_content)
            test_file = Path(f.name)

        try:
            # Test different processing operations
            operations = [
                "parse_ast",
                "extract_metadata",
                "validate_structure",
                "generate_toc",
                "process_math",
            ]

            processing_results = {}

            for operation in operations:
                print(f"  Testing {operation}...")

                self.start_performance_monitoring()
                start_time = time.time()

                try:
                    # Mock content processing
                    success = self.mock_content_processing(test_file, operation)

                    end_time = time.time()
                    metrics = self.stop_performance_monitoring()

                    processing_time = end_time - start_time
                    throughput_mb_per_sec = (
                        (size_bytes / (1024 * 1024)) / processing_time
                        if processing_time > 0
                        else 0
                    )
                    time_per_kb = (
                        processing_time / (size_bytes / 1024) if size_bytes > 0 else 0
                    )

                    processing_results[operation] = {
                        "success": success,
                        "time_seconds": processing_time,
                        "throughput_mb_per_sec": throughput_mb_per_sec,
                        "time_per_kb": time_per_kb,
                        "peak_memory_mb": metrics["peak_memory_mb"],
                    }

                    print(f"    Time: {processing_time:.3f}s")
                    print(f"    Throughput: {throughput_mb_per_sec:.2f} MB/s")

                except Exception as e:
                    end_time = time.time()
                    processing_time = end_time - start_time
                    processing_results[operation] = {
                        "success": False,
                        "error": str(e),
                        "time_seconds": processing_time,
                    }
                    print(f"    Failed: {e}")

            # Analyze throughput results
            self.analyze_throughput_results(
                processing_results, size_name, baselines, size_bytes
            )

        finally:
            test_file.unlink(missing_ok=True)

    def mock_content_processing(self, input_file: Path, operation: str) -> bool:
        """Mock content processing for testing"""
        input_size = input_file.stat().st_size

        # Different operations have different processing times
        complexity_factors = {
            "parse_ast": 1.0,
            "extract_metadata": 0.3,
            "validate_structure": 0.5,
            "generate_toc": 0.2,
            "process_math": 1.2,
        }

        complexity = complexity_factors.get(operation, 1.0)

        # Simulate processing time
        processing_time = (
            (input_size / 1024 / 1024) * 0.05 * complexity
        )  # 0.05s per MB * complexity
        time.sleep(min(processing_time, 0.3))  # Cap at 0.3s for testing

        # Simulate success/failure based on operation
        success_rates = {
            "parse_ast": 0.95,
            "extract_metadata": 0.98,
            "validate_structure": 0.90,
            "generate_toc": 0.92,
            "process_math": 0.85,
        }

        import random

        return random.random() < success_rates.get(operation, 0.9)

    def analyze_throughput_results(
        self,
        results: Dict[str, Dict[str, Any]],
        size_name: str,
        baselines: Dict[str, Any],
        input_size_bytes: int,
    ):
        """Analyze content processing throughput results"""

        print(f"\n📊 Content Processing Results for {size_name} files:")

        successful_operations = [r for r in results.values() if r.get("success", False)]

        if not successful_operations:
            pytest.fail("No content processing operations succeeded")

        # Calculate statistics
        processing_times = [r["time_seconds"] for r in successful_operations]
        throughputs = [r["throughput_mb_per_sec"] for r in successful_operations]
        time_per_kb_values = [r["time_per_kb"] for r in successful_operations]

        avg_time = statistics.mean(processing_times)
        avg_throughput = statistics.mean(throughputs)
        avg_time_per_kb = statistics.mean(time_per_kb_values)

        input_size_mb = input_size_bytes / (1024 * 1024)

        print(f"  Input Size: {input_size_mb:.2f}MB")
        print(f"  Successful Operations: {len(successful_operations)}/{len(results)}")
        print(f"  Average Time: {avg_time:.3f}s")
        print(f"  Average Throughput: {avg_throughput:.2f} MB/s")
        print(f"  Average Time per KB: {avg_time_per_kb:.4f}s")

        # Per-operation breakdown
        print(f"\n📈 Per-Operation Performance:")
        for operation, result in results.items():
            if result.get("success", False):
                print(f"  {operation}:")
                print(f"    Time: {result['time_seconds']:.3f}s")
                print(f"    Throughput: {result['throughput_mb_per_sec']:.2f} MB/s")
                print(f"    Memory: {result['peak_memory_mb']:.1f}MB")

        # Performance assertions
        assert avg_time_per_kb <= baselines["max_time_per_kb"], (
            f"Average time per KB {avg_time_per_kb:.4f}s exceeds baseline "
            f"{baselines['max_time_per_kb']:.4f}s"
        )

        assert avg_throughput >= baselines["min_throughput_mb_per_sec"], (
            f"Average throughput {avg_throughput:.2f} MB/s below baseline "
            f"{baselines['min_throughput_mb_per_sec']} MB/s"
        )

        # Success rate assertion
        success_rate = len(successful_operations) / len(results)
        assert success_rate >= 0.8, f"Success rate {success_rate:.2%} too low"

    def test_web_scraping_performance(self, benchmark_config):
        """Test web scraping performance metrics"""
        print("\n🔍 Testing web scraping performance")

        baselines = benchmark_config["performance_baselines"]["web_scraping"]

        # Mock test URLs
        test_urls = [
            "https://philosophynow.org/issues/123/test-article",
            "https://plato.stanford.edu/entries/test-entry/",
            "https://iep.utm.edu/test-philosopher/",
            "https://example.com/philosophy/simple",
            "https://example.com/philosophy/complex",
        ]

        scraping_results = {}

        for url in test_urls:
            print(f"  Testing {url}...")

            self.start_performance_monitoring()
            start_time = time.time()

            try:
                # Mock web scraping
                success, pages_scraped, content_size = self.mock_web_scraping(url)

                end_time = time.time()
                metrics = self.stop_performance_monitoring()

                scraping_time = end_time - start_time

                scraping_results[url] = {
                    "success": success,
                    "time_seconds": scraping_time,
                    "pages_scraped": pages_scraped,
                    "content_size_kb": content_size / 1024,
                    "peak_memory_mb": metrics["peak_memory_mb"],
                    "throughput_kb_per_sec": content_size / scraping_time / 1024
                    if scraping_time > 0
                    else 0,
                }

                print(f"    Time: {scraping_time:.3f}s")
                print(f"    Pages: {pages_scraped}")
                print(f"    Content: {scraping_results[url]['content_size_kb']:.1f}KB")

            except Exception as e:
                end_time = time.time()
                scraping_time = end_time - start_time
                scraping_results[url] = {
                    "success": False,
                    "error": str(e),
                    "time_seconds": scraping_time,
                }
                print(f"    Failed: {e}")

        # Analyze scraping results
        self.analyze_scraping_results(scraping_results, baselines)

    def mock_web_scraping(self, url: str) -> Tuple[bool, int, int]:
        """Mock web scraping for testing"""
        import random

        # Simulate network latency and processing time
        base_time = 1.0  # 1 second base time
        if "complex" in url or "stanford" in url:
            base_time *= 2.0
        elif "simple" in url:
            base_time *= 0.5

        # Add random variation
        variation = random.uniform(0.5, 1.5)
        total_time = base_time * variation

        # Simulate scraping work
        time.sleep(min(total_time, 2.0))  # Cap at 2s for testing

        # Simulate success/failure
        success = random.random() < 0.85

        if success:
            pages_scraped = random.randint(1, 5)
            content_size = pages_scraped * random.randint(
                2000, 10000
            )  # 2-10KB per page
            return True, pages_scraped, content_size

        return False, 0, 0

    def analyze_scraping_results(
        self, results: Dict[str, Dict[str, Any]], baselines: Dict[str, Any]
    ):
        """Analyze web scraping performance results"""

        print(f"\n📊 Web Scraping Results:")

        successful_scrapes = [r for r in results.values() if r.get("success", False)]

        if not successful_scrapes:
            pytest.fail("No web scraping operations succeeded")

        # Calculate statistics
        scraping_times = [r["time_seconds"] for r in successful_scrapes]
        pages_scraped = [r["pages_scraped"] for r in successful_scrapes]
        content_sizes = [r["content_size_kb"] for r in successful_scrapes]

        avg_time = statistics.mean(scraping_times)
        avg_pages = statistics.mean(pages_scraped)
        avg_content_size = statistics.mean(content_sizes)
        success_rate = len(successful_scrapes) / len(results)

        total_content_kb = sum(content_sizes)
        total_time = sum(scraping_times)
        overall_throughput = total_content_kb / total_time if total_time > 0 else 0

        print(f"  URLs Tested: {len(results)}")
        print(f"  Successful Scrapes: {len(successful_scrapes)}")
        print(f"  Success Rate: {success_rate:.1%}")
        print(f"  Average Time: {avg_time:.3f}s")
        print(f"  Average Pages: {avg_pages:.1f}")
        print(f"  Average Content: {avg_content_size:.1f}KB")
        print(f"  Overall Throughput: {overall_throughput:.1f}KB/s")

        # Per-URL breakdown
        print(f"\n📈 Per-URL Performance:")
        for url, result in results.items():
            url_short = url.split("/")[-1] if url.endswith("/") else url.split("/")[-1]
            if result.get("success", False):
                print(f"  {url_short}:")
                print(f"    Time: {result['time_seconds']:.3f}s")
                print(f"    Pages: {result['pages_scraped']}")
                print(f"    Content: {result['content_size_kb']:.1f}KB")
                print(f"    Throughput: {result['throughput_kb_per_sec']:.1f}KB/s")

        # Performance assertions
        assert avg_time <= baselines["max_time_per_page"], (
            f"Average scraping time {avg_time:.3f}s exceeds baseline "
            f"{baselines['max_time_per_page']}s"
        )

        assert success_rate >= baselines["min_success_rate"], (
            f"Success rate {success_rate:.1%} below baseline "
            f"{baselines['min_success_rate']:.1%}"
        )

        # Memory usage assertions
        for url, result in results.items():
            if result.get("success", False):
                memory_per_page = result["peak_memory_mb"] / result["pages_scraped"]
                assert memory_per_page <= baselines["max_memory_per_page"], (
                    f"Memory usage for {url} {memory_per_page:.1f}MB/page "
                    f"exceeds baseline {baselines['max_memory_per_page']}MB/page"
                )

    def test_tei_generation_speed(self, benchmark_config):
        """Test TEI XML generation speed"""
        print("\n🔍 Testing TEI XML generation speed")

        baselines = benchmark_config["performance_baselines"]["tei_generation"]

        # Test different document sizes
        test_scenarios = ["small", "medium", "large"]

        for size_name in test_scenarios:
            size_bytes = benchmark_config["test_file_sizes"][size_name]

            print(f"  Testing {size_name} document TEI generation...")

            # Create test content
            test_content = self.create_test_content(size_name, size_bytes)

            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".html", delete=False
            ) as f:
                f.write(f"<html><body>{test_content}</body></html>")
                test_file = Path(f.name)

            try:
                self.start_performance_monitoring()
                start_time = time.time()

                # Mock TEI generation
                success, tei_xml = self.mock_tei_generation(test_file)

                end_time = time.time()
                metrics = self.stop_performance_monitoring()

                generation_time = end_time - start_time
                throughput_mb_per_sec = (
                    (size_bytes / (1024 * 1024)) / generation_time
                    if generation_time > 0
                    else 0
                )
                time_per_kb = (
                    generation_time / (size_bytes / 1024) if size_bytes > 0 else 0
                )
                tei_size_bytes = len(tei_xml.encode("utf-8")) if success else 0

                print(f"    Time: {generation_time:.3f}s")
                print(f"    Throughput: {throughput_mb_per_sec:.2f} MB/s")
                print(f"    TEI Size: {tei_size_bytes / 1024:.1f}KB")
                print(f"    Memory: {metrics['peak_memory_mb']:.1f}MB")

                # Performance assertions for this size
                assert time_per_kb <= baselines["max_time_per_kb"], (
                    f"TEI generation time per KB for {size_name} {time_per_kb:.4f}s "
                    f"exceeds baseline {baselines['max_time_per_kb']:.4f}s"
                )

                if size_name in ["small", "medium"]:
                    assert (
                        throughput_mb_per_sec >= baselines["min_throughput_mb_per_sec"]
                    ), (
                        f"TEI generation throughput for {size_name} {throughput_mb_per_sec:.2f} MB/s "
                        f"below baseline {baselines['min_throughput_mb_per_sec']} MB/s"
                    )

                # Memory usage per MB
                memory_per_mb = (
                    metrics["peak_memory_mb"] / (size_bytes / (1024 * 1024))
                    if size_bytes > 0
                    else 0
                )
                assert memory_per_mb <= baselines["max_memory_per_mb"], (
                    f"Memory usage for {size_name} TEI generation {memory_per_mb:.1f}MB/MB "
                    f"exceeds baseline {baselines['max_memory_per_mb']}MB/MB"
                )

            finally:
                test_file.unlink(missing_ok=True)

    def mock_tei_generation(self, input_file: Path) -> Tuple[bool, str]:
        """Mock TEI XML generation for testing"""
        input_size = input_file.stat().st_size

        # Simulate TEI processing time
        processing_time = (input_size / 1024 / 1024) * 0.2  # 0.2s per MB
        time.sleep(min(processing_time, 0.5))  # Cap at 0.5s for testing

        # Generate mock TEI XML
        tei_xml = f"""<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <teiHeader>
    <fileDesc>
      <titleStmt>
        <title>Generated Test Document</title>
        <author>System Test</author>
      </titleStmt>
      <publicationStmt>
        <publisher>Integral Philosophy Publishing System</publisher>
        <date>2024</date>
      </publicationStmt>
    </fileDesc>
  </teiHeader>
  <text>
    <body>
      <!-- Generated from {input_file.name} -->
      <p>Generated TEI content with size {input_size} bytes</p>
    </body>
  </text>
</TEI>"""

        return True, tei_xml

    def test_memory_usage_profiling(self, benchmark_config):
        """Test memory usage profiling during benchmarks"""
        print("\n🔍 Testing memory usage profiling")

        size_bytes = benchmark_config["test_file_sizes"]["large"]
        process = psutil.Process()

        # Baseline memory
        baseline_memory = process.memory_info().rss / 1024 / 1024

        # Create test content
        test_content = self.create_test_content("large", size_bytes)

        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(test_content)
            test_file = Path(f.name)

        try:
            # Profile memory across different operations
            operations = {
                "format_conversion": self.mock_format_conversion,
                "content_processing": lambda f: self.mock_content_processing(
                    f, "parse_ast"
                ),
                "tei_generation": lambda f: self.mock_tei_generation(f)[0],
            }

            memory_profile = {}

            for operation_name, operation_func in operations.items():
                print(f"  Profiling {operation_name} memory usage...")

                # Memory before operation
                memory_before = process.memory_info().rss / 1024 / 1024

                # Execute operation
                self.start_performance_monitoring(process)
                start_time = time.time()

                try:
                    if operation_name == "format_conversion":
                        success, output_file = operation_func(test_file, "html")
                    else:
                        success = operation_func(test_file)

                    end_time = time.time()
                    metrics = self.stop_performance_monitoring()

                    # Memory after operation
                    memory_after = process.memory_info().rss / 1024 / 1024
                    memory_delta = memory_after - memory_before

                    memory_profile[operation_name] = {
                        "success": success,
                        "memory_before_mb": memory_before,
                        "memory_after_mb": memory_after,
                        "memory_delta_mb": memory_delta,
                        "peak_memory_mb": metrics["peak_memory_mb"],
                        "time_seconds": end_time - start_time,
                        "memory_per_mb_input": memory_delta
                        / (size_bytes / (1024 * 1024))
                        if size_bytes > 0
                        else 0,
                    }

                    print(f"    Memory Delta: {memory_delta:.1f}MB")
                    print(f"    Peak Memory: {metrics['peak_memory_mb']:.1f}MB")
                    print(
                        f"    Memory per MB input: {memory_profile[operation_name]['memory_per_mb_input']:.1f}MB/MB"
                    )

                    # Cleanup if needed
                    if operation_name == "format_conversion" and success:
                        Path(output_file).unlink(missing_ok=True)

                except Exception as e:
                    print(f"    Failed: {e}")
                    memory_profile[operation_name] = {
                        "success": False,
                        "error": str(e),
                        "memory_delta_mb": 0,
                    }

            # Analyze memory profile
            self.analyze_memory_profile(memory_profile, baseline_memory)

        finally:
            test_file.unlink(missing_ok=True)

    def analyze_memory_profile(
        self, profile: Dict[str, Dict[str, Any]], baseline_memory: float
    ):
        """Analyze memory usage profile"""

        print(f"\n📊 Memory Usage Profile:")
        print(f"  Baseline Memory: {baseline_memory:.1f}MB")

        successful_operations = {
            k: v for k, v in profile.items() if v.get("success", False)
        }

        if not successful_operations:
            pytest.fail("No memory profiling operations succeeded")

        # Calculate memory statistics
        memory_deltas = [v["memory_delta_mb"] for v in successful_operations.values()]
        memory_per_mb_inputs = [
            v["memory_per_mb_input"] for v in successful_operations.values()
        ]

        avg_memory_delta = statistics.mean(memory_deltas)
        max_memory_delta = max(memory_deltas)
        avg_memory_per_mb = statistics.mean(memory_per_mb_inputs)

        print(f"  Successful Operations: {len(successful_operations)}/{len(profile)}")
        print(f"  Average Memory Delta: {avg_memory_delta:.1f}MB")
        print(f"  Maximum Memory Delta: {max_memory_delta:.1f}MB")
        print(f"  Average Memory per MB Input: {avg_memory_per_mb:.1f}MB/MB")

        # Per-operation breakdown
        print(f"\n📈 Per-Operation Memory Usage:")
        for operation, metrics in successful_operations.items():
            print(f"  {operation}:")
            print(f"    Memory Delta: {metrics['memory_delta_mb']:.1f}MB")
            print(f"    Peak Memory: {metrics['peak_memory_mb']:.1f}MB")
            print(f"    Memory per MB Input: {metrics['memory_per_mb_input']:.1f}MB/MB")
            print(f"    Time: {metrics['time_seconds']:.3f}s")

        # Memory efficiency assertions
        assert avg_memory_per_mb <= 100, (
            f"Average memory per MB {avg_memory_per_mb:.1f}MB/MB too high"
        )
        assert max_memory_delta <= 200, (
            f"Maximum memory delta {max_memory_delta:.1f}MB too high"
        )

        # Memory leak detection
        final_memory = psutil.Process().memory_info().rss / 1024 / 1024
        memory_leak = final_memory - baseline_memory

        print(f"  Final Memory: {final_memory:.1f}MB")
        print(f"  Memory Leak: {memory_leak:.1f}MB")

        assert memory_leak < 50, f"Memory leak detected: {memory_leak:.1f}MB"
