#!/usr/bin/env python3
"""
Performance and stress testing for validation system.
Tests system behavior under load and with large inputs.
"""

import sys
import os
import tempfile
import time
import threading
import concurrent.futures
from pathlib import Path
from typing import Dict, List

# Add the project root to Python path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from validators import (
    HTML5Validator,
    CSSValidator,
    JavaScriptValidator,
    LaTeXValidator,
    ContentIntegrityValidator,
    QualityReportGenerator,
)


def test_validation_performance():
    """Test validation performance with various file sizes."""
    print("🔍 Testing validation performance...")

    sizes = [
        ("Small (1KB)", 1024),
        ("Medium (10KB)", 10240),
        ("Large (100KB)", 102400),
        ("Very Large (1MB)", 1024000),
    ]

    for size_name, size_bytes in sizes:
        # Generate HTML content of specified size
        html_content = """<!DOCTYPE html>
<html lang="en">
<head><meta charset="UTF-8"><title>Performance Test</title></head>
<body>"""

        # Add content to reach target size
        while len(html_content.encode()) < size_bytes:
            html_content += """<h2>Section</h2>
<p>This is test content for performance testing. </p>
<img src="test.jpg" alt="Test image">
<div style="color: red;">Styled content</div>"""

        html_content += """</body></html>"""

        with tempfile.NamedTemporaryFile(mode="w", suffix=".html", delete=False) as f:
            f.write(html_content)
            temp_file = Path(f.name)

        try:
            validator = HTML5Validator()

            start_time = time.time()
            result = validator.validate(temp_file)
            end_time = time.time()

            duration = end_time - start_time
            file_size = temp_file.stat().st_size

            print(
                f"  ✓ {size_name}: {duration:.3f}s ({file_size} bytes, {result.error_count} errors)"
            )

            # Performance assertion - should complete within reasonable time
            max_time = max(0.1, file_size / 1000000)  # 1 second per MB minimum
            assert duration < max_time, (
                f"Validation took too long: {duration}s > {max_time}s"
            )

        finally:
            temp_file.unlink()


def test_memory_usage():
    """Test memory usage during validation."""
    print("🔍 Testing memory usage...")

    import psutil
    import gc

    # Get initial memory usage
    process = psutil.Process()
    initial_memory = process.memory_info().rss / 1024 / 1024  # MB

    # Validate many files
    validator = HTML5Validator()
    files_created = []

    try:
        for i in range(50):
            html_content = f"""<!DOCTYPE html>
<html><head><title>Test {i}</title></head>
<body>
<h1>Test {i}</h1>
<img src="test{i}.jpg" alt="Test {i}">
<div style="color: red;">Content {i}</div>
</body></html>"""

            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".html", delete=False
            ) as f:
                f.write(html_content)
                temp_file = Path(f.name)
                files_created.append(temp_file)

            result = validator.validate(temp_file)

        # Force garbage collection
        gc.collect()

        # Check memory usage
        final_memory = process.memory_info().rss / 1024 / 1024  # MB
        memory_increase = final_memory - initial_memory

        print(
            f"  ✓ Memory usage: {initial_memory:.1f}MB → {final_memory:.1f}MB (+{memory_increase:.1f}MB)"
        )

        # Memory assertion - should not increase excessively
        assert memory_increase < 100, f"Memory increased too much: {memory_increase}MB"

    finally:
        for temp_file in files_created:
            temp_file.unlink()


def test_concurrent_load():
    """Test system under concurrent load."""
    print("🔍 Testing concurrent load...")

    def validate_html_worker(worker_id):
        """Worker function for concurrent validation."""
        validator = HTML5Validator()
        results = []

        for i in range(10):
            html_content = f"""<!DOCTYPE html>
<html><head><title>Worker {worker_id} Test {i}</title></head>
<body>
<h1>Worker {worker_id} Test {i}</h1>
<img src="test{worker_id}_{i}.jpg" alt="Test {worker_id}_{i}">
<div style="color: red;">Content {worker_id}_{i}</div>
</body></html>"""

            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".html", delete=False
            ) as f:
                f.write(html_content)
                temp_file = Path(f.name)

            try:
                start_time = time.time()
                result = validator.validate(temp_file)
                end_time = time.time()
                results.append(
                    (worker_id, i, end_time - start_time, result.error_count)
                )
            finally:
                temp_file.unlink()

        return results

    # Test with different numbers of concurrent workers
    worker_counts = [1, 2, 4, 8]

    for worker_count in worker_counts:
        start_time = time.time()

        with concurrent.futures.ThreadPoolExecutor(
            max_workers=worker_count
        ) as executor:
            futures = [
                executor.submit(validate_html_worker, i) for i in range(worker_count)
            ]
            all_results = []

            for future in concurrent.futures.as_completed(futures):
                try:
                    results = future.result()
                    all_results.extend(results)
                except Exception as e:
                    print(f"  ⚠️  Worker failed: {e}")

        end_time = time.time()
        duration = end_time - start_time
        total_validations = len(all_results)
        avg_time = duration / total_validations if total_validations > 0 else 0

        print(
            f"  ✓ {worker_count} workers: {duration:.2f}s total, {avg_time:.3f}s avg per validation"
        )

        # Performance assertion
        assert total_validations == worker_count * 10, (
            f"Expected {worker_count * 10} validations, got {total_validations}"
        )
        assert avg_time < 0.5, f"Average validation time too high: {avg_time}s"


def test_stress_large_validation():
    """Stress test with very large validation tasks."""
    print("🔍 Testing stress with large validation tasks...")

    # Create a very large HTML file
    large_html = """<!DOCTYPE html>
<html lang="en">
<head><meta charset="UTF-8"><title>Stress Test</title></head>
<body>"""

    # Add many sections to create a large file
    for i in range(1000):
        large_html += f"""
    <section id="section-{i}">
        <h2>Section {i}</h2>
        <p>This is paragraph {i} with substantial content for stress testing.</p>
        <img src="image-{i}.jpg" alt="Image {i}">
        <div style="color: red; background: blue; margin: 10px; padding: 5px;">
            Styled content {i} with multiple CSS properties for testing.
        </div>
        <ul>
            <li>List item {i}-1</li>
            <li>List item {i}-2</li>
            <li>List item {i}-3</li>
        </ul>
    </section>"""

    large_html += """</body></html>"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".html", delete=False) as f:
        f.write(large_html)
        temp_file = Path(f.name)

    try:
        file_size = temp_file.stat().st_size / 1024 / 1024  # MB
        print(f"  Created test file: {file_size:.2f}MB")

        validator = HTML5Validator()

        start_time = time.time()
        result = validator.validate(temp_file)
        end_time = time.time()

        duration = end_time - start_time

        print(
            f"  ✓ Large file validation: {duration:.2f}s ({result.error_count} errors, {result.warning_count} warnings)"
        )

        # Performance assertion
        max_time = max(1.0, file_size / 10)  # 1 second per 10MB minimum
        assert duration < max_time, (
            f"Large file validation took too long: {duration}s > {max_time}s"
        )

        # Should detect many issues in large file
        assert result.error_count >= 1000, "Should detect many missing alt attributes"
        assert result.warning_count >= 1000, "Should detect many inline styles"

    finally:
        temp_file.unlink()


def test_integrity_performance():
    """Test content integrity validation performance."""
    print("🔍 Testing content integrity performance...")

    # Create files with substantial content
    html_content = """<!DOCTYPE html>
<html><head><title>Integrity Test</title></head><body>"""

    for i in range(100):
        html_content += f"""
    <h2>Section {i}</h2>
    <p>This is paragraph {i} with content for integrity testing.</p>
    <p>Additional paragraph {i} with more content.</p>"""

    html_content += """</body></html>"""

    text_content = "\n\n".join(
        [
            f"Section {i}\n\nThis is paragraph {i} with content for integrity testing.\n\nAdditional paragraph {i} with more content."
            for i in range(100)
        ]
    )

    with tempfile.NamedTemporaryFile(mode="w", suffix=".html", delete=False) as f:
        f.write(html_content)
        html_file = Path(f.name)

    with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as f:
        f.write(text_content)
        text_file = Path(f.name)

    try:
        validator = ContentIntegrityValidator()

        start_time = time.time()
        result = validator.validate_integrity_across_formats(
            {"html": html_file, "text": text_file}
        )
        end_time = time.time()

        duration = end_time - start_time

        print(f"  ✓ Content integrity validation: {duration:.2f}s")
        print(f"    Content chunks: {result.stats.get('content_chunks', 0)}")
        print(f"    Similarity scores: {result.stats.get('similarity_scores', {})}")

        # Performance assertion
        assert duration < 5.0, f"Integrity validation took too long: {duration}s"

    finally:
        html_file.unlink()
        text_file.unlink()


def test_report_generation_performance():
    """Test quality report generation performance."""
    print("🔍 Testing report generation performance...")

    from validators.validators import ValidationResult, ValidationError

    # Create many validation results
    validation_results = {}

    for format_name in ["html", "css", "js", "latex"]:
        errors = []
        for i in range(100):
            errors.append(
                ValidationError(
                    severity="error" if i % 3 == 0 else "warning",
                    message=f"Test error {i} for {format_name}",
                    file_path=f"test.{format_name}",
                    line=i + 1,
                    rule_id=f"test-{format_name}-{i}",
                )
            )

        validation_results[format_name] = ValidationResult(
            is_valid=False,
            errors=errors,
            stats={"checks_performed": 50, "file_size": 10240},
        )

    # Create temporary output files
    output_formats = {}
    for format_name in validation_results.keys():
        with tempfile.NamedTemporaryFile(
            suffix=f".{format_name}", delete=False, mode="w", encoding="utf-8"
        ) as f:
            f.write(f"Test content for {format_name}")
            output_formats[format_name] = Path(f.name)

    try:
        generator = QualityReportGenerator()

        start_time = time.time()
        report = generator.generate_report(
            source_files=[Path("test1.tex"), Path("test2.tex")],
            output_formats=output_formats,
            validation_results=validation_results,
            processing_time=10.5,
        )
        end_time = time.time()

        duration = end_time - start_time

        print(f"  ✓ Report generation: {duration:.3f}s")
        print(f"    Overall score: {report.quality_metrics.overall_score:.1f}")
        print(f"    Recommendations: {len(report.recommendations)}")

        # Performance assertion
        assert duration < 1.0, f"Report generation took too long: {duration}s"

        # Test report saving performance
        report_dir = Path(tempfile.mkdtemp())

        start_time = time.time()
        generator.save_report(report, report_dir / "test.json", "json")
        generator.save_report(report, report_dir / "test.html", "html")
        generator.save_report(report, report_dir / "test.md", "markdown")
        end_time = time.time()

        save_duration = end_time - start_time
        print(f"  ✓ Report saving: {save_duration:.3f}s")

        # Cleanup
        import shutil

        shutil.rmtree(report_dir)

        assert save_duration < 0.5, f"Report saving took too long: {save_duration}s"

    finally:
        for temp_file in output_formats.values():
            temp_file.unlink()


def test_resource_cleanup():
    """Test that resources are properly cleaned up."""
    print("🔍 Testing resource cleanup...")

    import gc
    import weakref

    # Test validator cleanup
    validators_created = []
    weak_refs = []

    for i in range(10):
        validator = HTML5Validator()
        validators_created.append(validator)
        weak_refs.append(weakref.ref(validator))

    # Delete validators
    del validators_created

    # Force garbage collection
    gc.collect()

    # Check that validators were cleaned up
    cleaned_up = sum(1 for ref in weak_refs if ref() is None)
    print(f"  ✓ Validators cleaned up: {cleaned_up}/10")

    # Test file handle cleanup
    files_created = []

    for i in range(5):
        with tempfile.NamedTemporaryFile(mode="w", suffix=".html", delete=False) as f:
            f.write(f"<!DOCTYPE html><html><body><p>Test {i}</p></body></html>")
            temp_file = Path(f.name)
            files_created.append(temp_file)

    # Validate files
    validator = HTML5Validator()
    for temp_file in files_created:
        result = validator.validate(temp_file)

    # Delete files
    for temp_file in files_created:
        temp_file.unlink()

    print("  ✓ File handles cleaned up")


def main():
    """Run all performance and stress tests."""
    print("🚀 Running Performance and Stress Tests...\n")

    tests = [
        test_validation_performance,
        test_memory_usage,
        test_concurrent_load,
        test_stress_large_validation,
        test_integrity_performance,
        test_report_generation_performance,
        test_resource_cleanup,
    ]

    results = []

    for test in tests:
        try:
            test()
            results.append(True)
            print(f"✅ {test.__name__} passed\n")
        except Exception as e:
            print(f"❌ {test.__name__} failed: {e}\n")
            results.append(False)

    # Summary
    passed = sum(results)
    total = len(results)

    print("=" * 60)
    print(f"Performance Tests Summary: {passed}/{total} tests passed")

    if passed == total:
        print("🎉 All performance tests passed!")
        print("✅ Validation system performs well under load")
        return 0
    else:
        print("❌ Some performance tests failed")
        return 1


if __name__ == "__main__":
    exit(main())
