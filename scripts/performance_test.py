#!/usr/bin/env python3
"""
Performance and Optimization Tests for Integral Philosophy Publishing System
"""

import time
import psutil
import os
import sys
import json
from pathlib import Path
import tempfile
import shutil
from typing import Dict, List, Any
import logging

# Set up logging
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)

# Import system components
sys.path.append(os.path.dirname(os.path.abspath(__file__)))


def benchmark_web_scraper():
    """Benchmark web scraping performance"""
    logger.info("=== Web Scraper Performance Test ===")

    from web_scraper import WebScraper

    config = {"timeout": 30, "rate_limit": 1.0, "max_depth": 2, "max_pages": 5}

    scraper = WebScraper(config)

    # Test URLs
    test_urls = [
        "https://philosophynow.org",
        "https://plato.stanford.edu/entries/descartes-epistemology/",
    ]

    results = {}

    for url in test_urls:
        logger.info(f"Testing scraper with: {url}")

        # Monitor resources
        process = psutil.Process()
        start_memory = process.memory_info().rss / 1024 / 1024  # MB
        start_time = time.time()

        try:
            result = scraper.scrape_url(url, depth=2)

            end_time = time.time()
            end_memory = process.memory_info().rss / 1024 / 1024  # MB

            results[url] = {
                "success": True,
                "pages_scraped": len(result.get("pages", [])),
                "time_seconds": end_time - start_time,
                "memory_used_mb": end_memory - start_memory,
                "memory_peak_mb": end_memory,
            }

        except Exception as e:
            results[url] = {
                "success": False,
                "error": str(e),
                "time_seconds": time.time() - start_time,
            }

    return results


def benchmark_format_converter():
    """Benchmark format conversion performance"""
    logger.info("=== Format Converter Performance Test ===")

    from format_converter import FormatConverter

    converter = FormatConverter(Path("performance_test"))

    # Test content
    test_content = """
# Performance Test Document

## Mathematical Content

This document tests performance with mathematical formulas:

$$\\int_{0}^{\\infty} e^{-x^2} dx = \\frac{\\sqrt{\\pi}}{2}$$

The wave function: $\\psi(x,t) = A e^{i(kx - \\omega t)}$

## Structure Test

### Section 1
Content with some **bold** and *italic* text.

### Section 2
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

    # Create test file
    test_file = Path("performance_test/perf_test.md")
    test_file.parent.mkdir(exist_ok=True)
    test_file.write_text(test_content)

    formats = ["html", "latex", "org", "asciidoc", "rst", "typst", "tei"]
    results = {}

    for format_name in formats:
        logger.info(f"Testing conversion to {format_name}")

        process = psutil.Process()
        start_memory = process.memory_info().rss / 1024 / 1024  # MB
        start_time = time.time()

        try:
            success, output_file = converter.convert(test_file, format_name)

            end_time = time.time()
            end_memory = process.memory_info().rss / 1024 / 1024  # MB

            if success and output_file.exists():
                results[format_name] = {
                    "success": True,
                    "time_seconds": end_time - start_time,
                    "memory_used_mb": end_memory - start_memory,
                    "output_size_kb": output_file.stat().st_size / 1024,
                }
            else:
                results[format_name] = {
                    "success": False,
                    "time_seconds": end_time - start_time,
                }

        except Exception as e:
            results[format_name] = {
                "success": False,
                "error": str(e),
                "time_seconds": time.time() - start_time,
            }

    return results


def benchmark_tei_generation():
    """Benchmark TEI XML generation performance"""
    logger.info("=== TEI Generation Performance Test ===")

    from tei_generator import TEIGenerator
    from format_converter import FormatConverter

    converter = FormatConverter(Path("performance_test"))
    generator = TEIGenerator()

    # Create AST from test content
    test_file = Path("performance_test/perf_test.md")
    if not test_file.exists():
        test_content = "# Test Content\n\nSome content with math: $E = mc^2$"
        test_file.write_text(test_content)

    process = psutil.Process()
    start_memory = process.memory_info().rss / 1024 / 1024  # MB
    start_time = time.time()

    try:
        # Convert to AST first
        ast_data = converter.convert_to_ast(test_file)

        # Generate TEI
        tei_xml = generator.generate_tei(
            ast_data,
            {
                "title": "Performance Test Document",
                "author": "System Test",
                "language": "en",
            },
        )

        end_time = time.time()
        end_memory = process.memory_info().rss / 1024 / 1024  # MB

        results = {
            "success": True,
            "time_seconds": end_time - start_time,
            "memory_used_mb": end_memory - start_memory,
            "tei_size_bytes": len(tei_xml.encode("utf-8")),
        }

    except Exception as e:
        results = {
            "success": False,
            "error": str(e),
            "time_seconds": time.time() - start_time,
        }

    return results


def benchmark_uml_generation():
    """Benchmark UML diagram generation performance"""
    logger.info("=== UML Generation Performance Test ===")

    from ast_to_uml import UMLGenerator
    from format_converter import FormatConverter

    converter = FormatConverter(Path("performance_test"))
    generator = UMLGenerator()

    # Create AST from test content
    test_file = Path("performance_test/perf_test.md")
    if not test_file.exists():
        test_content = """# Test Content
        
## Section 1
Content here.

## Section 2
More content with [link](#section-1).

### Subsection 2.1
Nested content.
        """
        test_file.write_text(test_content)

    try:
        ast_data = converter.convert_to_ast(test_file)

        formats = ["plantuml", "mermaid", "graphviz"]
        results = {}

        for format_name in formats:
            process = psutil.Process()
            start_memory = process.memory_info().rss / 1024 / 1024  # MB
            start_time = time.time()

            try:
                if format_name == "plantuml":
                    uml_output = generator.generate_plantuml(ast_data)
                elif format_name == "mermaid":
                    uml_output = generator.generate_mermaid(ast_data)
                elif format_name == "graphviz":
                    uml_output = generator.generate_dot(ast_data)

                end_time = time.time()
                end_memory = process.memory_info().rss / 1024 / 1024  # MB

                results[format_name] = {
                    "success": True,
                    "time_seconds": end_time - start_time,
                    "memory_used_mb": end_memory - start_memory,
                    "output_size_bytes": len(uml_output.encode("utf-8")),
                }

            except Exception as e:
                results[format_name] = {
                    "success": False,
                    "error": str(e),
                    "time_seconds": time.time() - start_time,
                }

        return results

    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "time_seconds": time.time() - start_time,
        }


def stress_test_conversions():
    """Stress test with multiple concurrent conversions"""
    logger.info("=== Stress Test: Multiple Conversions ===")

    from format_converter import FormatConverter
    import threading
    import queue

    converter = FormatConverter(Path("performance_test"))

    # Create test files
    test_files = []
    for i in range(5):
        content = f"""# Test Document {i}

## Section {i}

This is test content for document {i} with math formula $x^2 + y^2 = z^2$.

### Subsection {i}

More content here.
        """
        test_file = Path(f"performance_test/stress_test_{i}.md")
        test_file.write_text(content)
        test_files.append(test_file)

    def convert_file(file_path, result_queue):
        start_time = time.time()
        try:
            success, output = converter.convert(file_path, "html")
            end_time = time.time()
            result_queue.put(
                {
                    "file": str(file_path),
                    "success": success,
                    "time_seconds": end_time - start_time,
                }
            )
        except Exception as e:
            result_queue.put(
                {
                    "file": str(file_path),
                    "success": False,
                    "error": str(e),
                    "time_seconds": time.time() - start_time,
                }
            )

    # Run conversions concurrently
    start_time = time.time()
    result_queue = queue.Queue()
    threads = []

    for file_path in test_files:
        thread = threading.Thread(target=convert_file, args=(file_path, result_queue))
        threads.append(thread)
        thread.start()

    # Wait for all threads to complete
    for thread in threads:
        thread.join()

    total_time = time.time() - start_time

    # Collect results
    results = []
    while not result_queue.empty():
        results.append(result_queue.get())

    return {
        "total_time_seconds": total_time,
        "concurrent_files": len(test_files),
        "individual_results": results,
        "average_time_per_file": total_time / len(test_files),
    }


def generate_performance_report(results: Dict[str, Any]):
    """Generate comprehensive performance report"""

    report = {
        "test_timestamp": time.strftime("%Y-%m-%d %H:%M:%S"),
        "system_info": {
            "cpu_count": psutil.cpu_count(),
            "memory_gb": psutil.virtual_memory().total / (1024**3),
            "disk_free_gb": psutil.disk_usage(".").free / (1024**3),
        },
        "test_results": results,
    }

    # Calculate summary statistics
    report["summary"] = {
        "web_scraper_success_rate": 0,
        "format_conversion_success_rate": 0,
        "fastest_conversion": None,
        "slowest_conversion": None,
        "memory_usage_peak_mb": 0,
    }

    # Web scraper summary
    if "web_scraper" in results:
        scraper_results = results["web_scraper"]
        successful = sum(1 for r in scraper_results.values() if r.get("success", False))
        report["summary"]["web_scraper_success_rate"] = successful / len(
            scraper_results
        )

    # Format conversion summary
    if "format_converter" in results:
        conv_results = results["format_converter"]
        successful = sum(1 for r in conv_results.values() if r.get("success", False))
        report["summary"]["format_conversion_success_rate"] = successful / len(
            conv_results
        )

        # Find fastest/slowest successful conversions
        successful_conversions = [
            (fmt, r["time_seconds"])
            for fmt, r in conv_results.items()
            if r.get("success", False)
        ]
        if successful_conversions:
            fastest = min(successful_conversions, key=lambda x: x[1])
            slowest = max(successful_conversions, key=lambda x: x[1])
            report["summary"]["fastest_conversion"] = {
                "format": fastest[0],
                "time_seconds": fastest[1],
            }
            report["summary"]["slowest_conversion"] = {
                "format": slowest[0],
                "time_seconds": slowest[1],
            }

    # Memory usage summary
    memory_values = []
    for test_group in results.values():
        if isinstance(test_group, dict):
            for result in test_group.values():
                if isinstance(result, dict) and "memory_used_mb" in result:
                    memory_values.append(result["memory_used_mb"])

    if memory_values:
        report["summary"]["memory_usage_peak_mb"] = max(memory_values)

    return report


def main():
    """Run comprehensive performance tests"""

    logger.info("Starting Performance and Optimization Tests")
    logger.info("=" * 50)

    # Create test directory
    test_dir = Path("performance_test")
    if test_dir.exists():
        shutil.rmtree(test_dir)
    test_dir.mkdir()

    results = {}

    try:
        # Web scraper benchmark
        results["web_scraper"] = benchmark_web_scraper()

        # Format converter benchmark
        results["format_converter"] = benchmark_format_converter()

        # TEI generation benchmark
        results["tei_generation"] = benchmark_tei_generation()

        # UML generation benchmark
        results["uml_generation"] = benchmark_uml_generation()

        # Stress test
        results["stress_test"] = stress_test_conversions()

    except Exception as e:
        logger.error(f"Performance test failed: {e}")
        results["error"] = str(e)

    finally:
        # Clean up test directory
        if test_dir.exists():
            shutil.rmtree(test_dir)

    # Generate report
    report = generate_performance_report(results)

    # Save report
    report_file = Path("performance_report.json")
    with open(report_file, "w", encoding="utf-8") as f:
        json.dump(report, f, indent=2, ensure_ascii=False)

    # Print summary
    logger.info("Performance Test Summary:")
    logger.info(
        f"Web scraper success rate: {report['summary']['web_scraper_success_rate']:.1%}"
    )
    logger.info(
        f"Format conversion success rate: {report['summary']['format_conversion_success_rate']:.1%}"
    )

    if report["summary"]["fastest_conversion"]:
        logger.info(
            f"Fastest conversion: {report['summary']['fastest_conversion']['format']} ({report['summary']['fastest_conversion']['time_seconds']:.3f}s)"
        )
        logger.info(
            f"Slowest conversion: {report['summary']['slowest_conversion']['format']} ({report['summary']['slowest_conversion']['time_seconds']:.3f}s)"
        )

    logger.info(
        f"Peak memory usage: {report['summary']['memory_usage_peak_mb']:.1f} MB"
    )
    logger.info(f"Report saved to: {report_file}")

    return report


if __name__ == "__main__":
    import sys

    sys.path.append(os.path.dirname(os.path.abspath(__file__)))

    main()
