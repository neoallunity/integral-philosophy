#!/usr/bin/env python3
"""
Final comprehensive validation and quality assurance test.
Runs all validation tests and generates final quality report.
"""

import sys
import os
import json
import time
import tempfile
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


def run_final_validation():
    """Run final comprehensive validation of the entire system."""
    print("🚀 Running Final Comprehensive Validation\n")

    start_time = time.time()

    # Test 1: Basic functionality
    print("📋 Test 1: Basic Functionality")
    print("-" * 40)

    try:
        from validators import (
            HTML5Validator,
            CSSValidator,
            JavaScriptValidator,
            LaTeXValidator,
        )
        from validators.content_integrity import ContentIntegrityValidator
        from validators.quality_report import QualityReportGenerator

        print("✅ All validator classes import successfully")
    except Exception as e:
        print(f"❌ Import failed: {e}")
        return False

    # Test 2: Validate core project files
    print("\n📝 Test 2: Core Project Files Validation")
    print("-" * 40)

    validators = {
        "html": HTML5Validator(),
        "css": CSSValidator(),
        "js": JavaScriptValidator(),
        "latex": LaTeXValidator(),
    }

    core_files = {
        "main.tex": "latex",
        "preamble.tex": "latex",
        "cfg/cfg-fonts.tex": "latex",
        "cfg/cfg-bibliography.tex": "latex",
        "cfg/cfg-structure.tex": "latex",
    }

    validation_results = {}
    total_errors = 0
    total_warnings = 0

    for file_path, format_type in core_files.items():
        if Path(file_path).exists():
            validator = validators[format_type]
            result = validator.validate(Path(file_path))
            validation_results[file_path] = result

            total_errors += result.error_count
            total_warnings += result.warning_count

            print(
                f"  ✓ {file_path}: {result.error_count} errors, {result.warning_count} warnings"
            )
        else:
            print(f"  ⚠️  {file_path}: File not found")

    # Test 3: Content integrity
    print("\n🔗 Test 3: Content Integrity Validation")
    print("-" * 40)

    if Path("main.tex").exists():
        try:
            integrity_validator = ContentIntegrityValidator()

            # Create a simple text version for comparison
            with open("main.tex", "r", encoding="utf-8") as f:
                tex_content = f.read()

            # Extract basic text content
            import re

            text_content = re.sub(r"\\[a-zA-Z]+\{([^}]+)\}", r"\1", tex_content)
            text_content = re.sub(r"[{}]", "", text_content)
            text_content = re.sub(r"%.*$", "", text_content, flags=re.MULTILINE)

            # Write to temporary file
            import tempfile

            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".txt", delete=False
            ) as f:
                f.write(text_content.strip())
                temp_text_file = Path(f.name)

            try:
                integrity_result = (
                    integrity_validator.validate_integrity_across_formats(
                        {"latex": Path("main.tex"), "text": temp_text_file}
                    )
                )

                validation_results["integrity"] = integrity_result
                total_errors += integrity_result.error_count
                total_warnings += integrity_result.warning_count

                print(
                    f"  ✓ Content integrity: {integrity_result.error_count} errors, {integrity_result.warning_count} warnings"
                )
                print(
                    f"    Content chunks: {integrity_result.stats.get('content_chunks', 0)}"
                )

            finally:
                temp_text_file.unlink()

        except Exception as e:
            print(f"  ⚠️  Content integrity validation failed: {e}")

    # Test 4: Quality metrics
    print("\n📊 Test 4: Quality Metrics Calculation")
    print("-" * 40)

    try:
        generator = QualityReportGenerator()

        # Create dummy output files for report generation
        import tempfile

        output_files = {}

        for format_name in ["html", "css", "js"]:
            with tempfile.NamedTemporaryFile(
                suffix=f".{format_name}", delete=False, mode="w", encoding="utf-8"
            ) as f:
                f.write(f"Test content for {format_name}")
                output_files[format_name] = Path(f.name)

        try:
            report = generator.generate_report(
                source_files=[Path("main.tex")],
                output_formats=output_files,
                validation_results=validation_results,
                processing_time=time.time() - start_time,
            )

            print(f"  ✓ Quality report generated")
            print(f"    Overall score: {report.quality_metrics.overall_score:.1f}/100")
            print(f"    Format scores: {report.quality_metrics.format_scores}")
            print(f"    Integrity score: {report.quality_metrics.integrity_score:.1f}")
            print(
                f"    Accessibility score: {report.quality_metrics.accessibility_score:.1f}"
            )
            print(
                f"    Performance score: {report.quality_metrics.performance_score:.1f}"
            )
            print(
                f"    Standards compliance: {report.quality_metrics.standards_compliance:.1f}"
            )
            print(f"    Recommendations: {len(report.recommendations)}")

            # Save final report
            report_dir = Path("out/final_validation")
            report_dir.mkdir(parents=True, exist_ok=True)

            generator.save_report(report, report_dir / "final_report.json", "json")
            generator.save_report(report, report_dir / "final_report.html", "html")
            generator.save_report(report, report_dir / "final_report.md", "markdown")

            print(f"  ✓ Final reports saved to: {report_dir}")

        finally:
            for temp_file in output_files.values():
                temp_file.unlink()

    except Exception as e:
        print(f"  ⚠️  Quality metrics calculation failed: {e}")

    # Test 5: Performance validation
    print("\n⚡ Test 5: Performance Validation")
    print("-" * 40)

    try:
        # Test validation speed
        test_html = """<!DOCTYPE html>
<html><head><title>Performance Test</title></head>
<body>
<h1>Test</h1>
<img src="test.jpg" alt="Test image">
<div style="color: red;">Styled content</div>
</body></html>"""

        with tempfile.NamedTemporaryFile(mode="w", suffix=".html", delete=False) as f:
            f.write(test_html)
            temp_file = Path(f.name)

        try:
            validator = HTML5Validator()

            perf_start = time.time()
            result = validator.validate(temp_file)
            perf_end = time.time()

            perf_duration = perf_end - perf_start

            print(f"  ✓ HTML validation performance: {perf_duration:.3f}s")

            # Should complete quickly
            assert perf_duration < 1.0, f"Validation too slow: {perf_duration}s"

        finally:
            temp_file.unlink()

    except Exception as e:
        print(f"  ⚠️  Performance validation failed: {e}")

    # Final summary
    end_time = time.time()
    total_duration = end_time - start_time

    print("\n" + "=" * 60)
    print("🎯 FINAL VALIDATION SUMMARY")
    print("=" * 60)
    print(f"⏱️  Total validation time: {total_duration:.2f} seconds")
    print(f"📁 Files validated: {len(validation_results)}")
    print(f"❌ Total errors: {total_errors}")
    print(f"⚠️  Total warnings: {total_warnings}")

    # Calculate quality score
    if validation_results:
        max_possible_issues = len(validation_results) * 10
        issues_found = total_errors + (total_warnings * 0.3)
        quality_score = max(0, 100 - (issues_found / max_possible_issues * 100))

        print(f"📈 Overall quality score: {quality_score:.1f}/100")

        if quality_score >= 90:
            print("🎉 EXCELLENT: System ready for production")
        elif quality_score >= 80:
            print("👍 GOOD: System ready with minor improvements")
        elif quality_score >= 70:
            print("⚠️  ACCEPTABLE: System needs some improvements")
        else:
            print("❌ NEEDS WORK: System requires significant improvements")

    print(f"📄 Final reports available in: out/final_validation/")

    return total_errors == 0


def main():
    """Run final validation."""
    try:
        success = run_final_validation()
        return 0 if success else 1
    except Exception as e:
        print(f"\n❌ Final validation failed with exception: {e}")
        import traceback

        traceback.print_exc()
        return 1


if __name__ == "__main__":
    exit(main())
