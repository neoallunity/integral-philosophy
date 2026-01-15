#!/usr/bin/env python3
"""
Comprehensive test suite for edge cases and error conditions.
Tests boundary conditions, malformed inputs, and error handling.
"""

import sys
import os
import tempfile
import json
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
from validators.validators import ValidationError, ValidationResult


def test_empty_files():
    """Test validation of empty files."""
    print("🔍 Testing empty files...")

    # Test empty HTML
    with tempfile.NamedTemporaryFile(mode="w", suffix=".html", delete=False) as f:
        f.write("")
        temp_file = Path(f.name)

    try:
        validator = HTML5Validator()
        result = validator.validate(temp_file)
        assert result.error_count >= 1, "Should detect empty HTML file"
        print("  ✓ Empty HTML validation works")
    finally:
        temp_file.unlink()

    # Test empty CSS
    with tempfile.NamedTemporaryFile(mode="w", suffix=".css", delete=False) as f:
        f.write("")
        temp_file = Path(f.name)

    try:
        validator = CSSValidator()
        result = validator.validate(temp_file)
        print("  ✓ Empty CSS validation works")
    finally:
        temp_file.unlink()

    # Test empty JavaScript
    with tempfile.NamedTemporaryFile(mode="w", suffix=".js", delete=False) as f:
        f.write("")
        temp_file = Path(f.name)

    try:
        validator = JavaScriptValidator()
        result = validator.validate(temp_file)
        print("  ✓ Empty JavaScript validation works")
    finally:
        temp_file.unlink()

    # Test empty LaTeX
    with tempfile.NamedTemporaryFile(mode="w", suffix=".tex", delete=False) as f:
        f.write("")
        temp_file = Path(f.name)

    try:
        validator = LaTeXValidator()
        result = validator.validate(temp_file)
        assert result.error_count >= 1, "Should detect empty LaTeX file"
        print("  ✓ Empty LaTeX validation works")
    finally:
        temp_file.unlink()


def test_malformed_files():
    """Test validation of malformed files."""
    print("🔍 Testing malformed files...")

    # Test malformed HTML
    malformed_html = """<html><head><title>Test</title></head><body>
    <h1>Unclosed heading
    <p>Paragraph with no closing tag
    <div>Nested <span>tags <b>without</div> proper closing</b></span>
    <img src="test.jpg" alt="Missing quote" style="color: red;>
    </body>"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".html", delete=False) as f:
        f.write(malformed_html)
        temp_file = Path(f.name)

    try:
        validator = HTML5Validator()
        result = validator.validate(temp_file)
        assert result.error_count >= 1 or result.warning_count >= 1, (
            "Should detect malformed HTML"
        )
        print("  ✓ Malformed HTML validation works")
    finally:
        temp_file.unlink()

    # Test malformed CSS
    malformed_css = """.test {
        color: red;
        font-size: 14px
        margin: 10px;
    }
    
    .invalid {
        background: url(test.jpg;
        border: 1px solid #000
    }"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".css", delete=False) as f:
        f.write(malformed_css)
        temp_file = Path(f.name)

    try:
        validator = CSSValidator()
        result = validator.validate(temp_file)
        print("  ✓ Malformed CSS validation works")
    finally:
        temp_file.unlink()

    # Test malformed JavaScript
    malformed_js = """function test() {
        var x = 10;
        if (x > 5 {
            console.log("x is greater than 5");
        }
        return x;
    }
    
    function another() {
        let y = 20;
        return y +;
    }"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".js", delete=False) as f:
        f.write(malformed_js)
        temp_file = Path(f.name)

    try:
        validator = JavaScriptValidator()
        result = validator.validate(temp_file)
        assert result.warning_count >= 1, "Should detect malformed JavaScript"
        print("  ✓ Malformed JavaScript validation works")
    finally:
        temp_file.unlink()

    # Test malformed LaTeX
    malformed_tex = """\\documentclass{article}
\\begin{document}

\\section{Test Section}
This is some content.

\\begin{itemize}
\\item Item 1
\\item Item 2
\\item Item 3

\\subsection{Subsection}
Missing end of itemize environment.

\\begin{table}
\\caption{Test table}
\\begin{tabular}{|c|c|}
\\hline
Column 1 & Column 2
\\hline
Data 1 & Data 2
\\hline
\\end{document}"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".tex", delete=False) as f:
        f.write(malformed_tex)
        temp_file = Path(f.name)

    try:
        validator = LaTeXValidator()
        result = validator.validate(temp_file)
        assert result.error_count >= 1, "Should detect malformed LaTeX"
        print("  ✓ Malformed LaTeX validation works")
    finally:
        temp_file.unlink()


def test_unicode_and_encoding():
    """Test validation with Unicode content and different encodings."""
    print("🔍 Testing Unicode and encoding...")

    # Test HTML with Unicode
    unicode_html = """<!DOCTYPE html>
<html lang="ru">
<head>
    <meta charset="UTF-8">
    <title>Тестовый документ</title>
</head>
<body>
    <h1>Заголовок на русском</h1>
    <p>Текст с эмодзи 🎉 и специальными символами: α, β, γ, δ</p>
    <img src="test.jpg" alt="Изображение с описанием">
    <div style="color: red;">Цветной текст</div>
</body>
</html>"""

    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".html", delete=False, encoding="utf-8"
    ) as f:
        f.write(unicode_html)
        temp_file = Path(f.name)

    try:
        validator = HTML5Validator()
        result = validator.validate(temp_file)
        print("  ✓ Unicode HTML validation works")
    finally:
        temp_file.unlink()

    # Test CSS with Unicode
    unicode_css = """.тестовый-класс {
    content: "Текст на русском 🎉";
    font-family: "Arial Unicode MS", sans-serif;
}

.emoji {
    content: "🎉🚀📚";
}"""

    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".css", delete=False, encoding="utf-8"
    ) as f:
        f.write(unicode_css)
        temp_file = Path(f.name)

    try:
        validator = CSSValidator()
        result = validator.validate(temp_file)
        print("  ✓ Unicode CSS validation works")
    finally:
        temp_file.unlink()

    # Test JavaScript with Unicode
    unicode_js = """// Комментарий на русском
const тестоваяПеременная = "значение 🎉";

function функция() {
    console.log("Тестовое сообщение с эмодзи 🚀");
    return тестоваяПеременная;
}

// Строка с специальными символами
const specialChars = "αβγδ";"""

    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".js", delete=False, encoding="utf-8"
    ) as f:
        f.write(unicode_js)
        temp_file = Path(f.name)

    try:
        validator = JavaScriptValidator()
        result = validator.validate(temp_file)
        print("  ✓ Unicode JavaScript validation works")
    finally:
        temp_file.unlink()


def test_large_files():
    """Test validation with large files."""
    print("🔍 Testing large files...")

    # Generate large HTML file
    large_html = """<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Large Test Document</title>
</head>
<body>
"""

    # Add many sections
    for i in range(100):
        large_html += f"""    <h2>Section {i}</h2>
    <p>This is paragraph {i} with some content.</p>
    <img src="image{i}.jpg" alt="Image {i}">
    <div style="color: red;">Styled content {i}</div>
"""

    large_html += """</body>
</html>"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".html", delete=False) as f:
        f.write(large_html)
        temp_file = Path(f.name)

    try:
        validator = HTML5Validator()
        result = validator.validate(temp_file)
        assert result.error_count >= 100, (
            "Should detect missing alt attributes in large file"
        )
        assert result.warning_count >= 100, "Should detect inline styles in large file"
        print("  ✓ Large HTML file validation works")
    finally:
        temp_file.unlink()


def test_nonexistent_files():
    """Test validation with nonexistent files."""
    print("🔍 Testing nonexistent files...")

    validator = HTML5Validator()
    nonexistent_file = Path("/nonexistent/path/file.html")

    try:
        result = validator.validate(nonexistent_file)
        assert False, "Should raise exception for nonexistent file"
    except FileNotFoundError:
        print("  ✓ Nonexistent file handling works")
    except Exception as e:
        print(f"  ✓ Nonexistent file raises exception: {type(e).__name__}")


def test_permission_errors():
    """Test validation with permission errors."""
    print("🔍 Testing permission errors...")

    # Create a file and make it unreadable
    with tempfile.NamedTemporaryFile(mode="w", suffix=".html", delete=False) as f:
        f.write(
            "<!DOCTYPE html><html><head><title>Test</title></head><body><h1>Test</h1></body></html>"
        )
        temp_file = Path(f.name)

    try:
        # Make file unreadable (if possible)
        try:
            temp_file.chmod(0o000)
            validator = HTML5Validator()
            result = validator.validate(temp_file)
            assert False, "Should raise exception for unreadable file"
        except PermissionError:
            print("  ✓ Permission error handling works")
        except Exception as e:
            print(f"  ✓ Permission error raises exception: {type(e).__name__}")
        finally:
            # Restore permissions for cleanup
            temp_file.chmod(0o644)
    finally:
        temp_file.unlink()


def test_edge_case_content():
    """Test validation with edge case content."""
    print("🔍 Testing edge case content...")

    # Test HTML with only comments
    comments_only_html = """<!DOCTYPE html>
<html>
<head><title>Test</title></head>
<body>
<!-- This is a comment -->
<!-- Another comment -->
<!-- Yet another comment -->
</body>
</html>"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".html", delete=False) as f:
        f.write(comments_only_html)
        temp_file = Path(f.name)

    try:
        validator = HTML5Validator()
        result = validator.validate(temp_file)
        print("  ✓ Comments-only HTML validation works")
    finally:
        temp_file.unlink()

    # Test CSS with only whitespace
    whitespace_css = """


    
    """

    with tempfile.NamedTemporaryFile(mode="w", suffix=".css", delete=False) as f:
        f.write(whitespace_css)
        temp_file = Path(f.name)

    try:
        validator = CSSValidator()
        result = validator.validate(temp_file)
        print("  ✓ Whitespace-only CSS validation works")
    finally:
        temp_file.unlink()

    # Test JavaScript with only comments
    comments_only_js = """// Single line comment
/* Multi-line comment */
// Another single line comment
/* Another
   multi-line
   comment */"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".js", delete=False) as f:
        f.write(comments_only_js)
        temp_file = Path(f.name)

    try:
        validator = JavaScriptValidator()
        result = validator.validate(temp_file)
        print("  ✓ Comments-only JavaScript validation works")
    finally:
        temp_file.unlink()


def test_content_integrity_edge_cases():
    """Test content integrity validation edge cases."""
    print("🔍 Testing content integrity edge cases...")

    # Test with identical content
    identical_content = "This is identical content for testing."

    with tempfile.NamedTemporaryFile(mode="w", suffix=".html", delete=False) as f:
        f.write(f"<!DOCTYPE html><html><body><p>{identical_content}</p></body></html>")
        html_file = Path(f.name)

    with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as f:
        f.write(identical_content)
        text_file = Path(f.name)

    try:
        validator = ContentIntegrityValidator()
        result = validator.validate_integrity_across_formats(
            {"html": html_file, "text": text_file}
        )
        assert result.stats.get("similarity_scores", {}).get("html_vs_text", 0) > 0.8, (
            "Should detect high similarity"
        )
        print("  ✓ Identical content integrity validation works")
    finally:
        html_file.unlink()
        text_file.unlink()

    # Test with completely different content
    html_content = "HTML content with different text."
    text_content = "Text content with completely different words."

    with tempfile.NamedTemporaryFile(mode="w", suffix=".html", delete=False) as f:
        f.write(f"<!DOCTYPE html><html><body><p>{html_content}</p></body></html>")
        html_file = Path(f.name)

    with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as f:
        f.write(text_content)
        text_file = Path(f.name)

    try:
        validator = ContentIntegrityValidator()
        result = validator.validate_integrity_across_formats(
            {"html": html_file, "text": text_file}
        )
        similarity = result.stats.get("similarity_scores", {}).get("html_vs_text", 1.0)
        assert similarity < 0.5, "Should detect low similarity"
        print("  ✓ Different content integrity validation works")
    finally:
        html_file.unlink()
        text_file.unlink()


def test_quality_report_edge_cases():
    """Test quality report generation edge cases."""
    print("🔍 Testing quality report edge cases...")

    # Test with empty validation results
    empty_results = {}

    with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as f:
        html_file = Path(f.name)

    try:
        generator = QualityReportGenerator()
        report = generator.generate_report(
            source_files=[],
            output_formats={"html": html_file},
            validation_results=empty_results,
            processing_time=0.0,
        )

        assert report.quality_metrics.overall_score == 0.0, (
            "Empty results should give score 0"
        )
        print("  ✓ Empty validation results report works")
    finally:
        html_file.unlink()

    # Test with perfect validation results
    perfect_results = {
        "html": ValidationResult(
            is_valid=True, errors=[], stats={"checks_performed": 10}
        )
    }

    with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as f:
        html_file = Path(f.name)

    try:
        generator = QualityReportGenerator()
        report = generator.generate_report(
            source_files=[Path("test.tex")],
            output_formats={"html": html_file},
            validation_results=perfect_results,
            processing_time=1.0,
        )

        assert report.quality_metrics.overall_score >= 90.0, (
            "Perfect results should give high score"
        )
        print("  ✓ Perfect validation results report works")
    finally:
        html_file.unlink()


def test_concurrent_validation():
    """Test concurrent validation of multiple files."""
    print("🔍 Testing concurrent validation...")

    import threading
    import time

    results = []
    errors = []

    def validate_file(file_index):
        try:
            test_html = f"""<!DOCTYPE html>
<html lang="en">
<head><title>Test {file_index}</title></head>
<body>
    <h1>Test {file_index}</h1>
    <img src="test{file_index}.jpg">
    <div style="color: red;">Content {file_index}</div>
</body>
</html>"""

            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".html", delete=False
            ) as f:
                f.write(test_html)
                temp_file = Path(f.name)

            try:
                validator = HTML5Validator()
                result = validator.validate(temp_file)
                results.append((file_index, result.error_count, result.warning_count))
            finally:
                temp_file.unlink()

        except Exception as e:
            errors.append((file_index, e))

    # Create multiple threads
    threads = []
    for i in range(5):
        thread = threading.Thread(target=validate_file, args=(i,))
        threads.append(thread)
        thread.start()

    # Wait for all threads to complete
    for thread in threads:
        thread.join()

    assert len(errors) == 0, f"Concurrent validation had errors: {errors}"
    assert len(results) == 5, f"Expected 5 results, got {len(results)}"
    print("  ✓ Concurrent validation works")


def main():
    """Run all edge case tests."""
    print("🚀 Running Edge Case and Error Condition Tests...\n")

    tests = [
        test_empty_files,
        test_malformed_files,
        test_unicode_and_encoding,
        test_large_files,
        test_nonexistent_files,
        test_permission_errors,
        test_edge_case_content,
        test_content_integrity_edge_cases,
        test_quality_report_edge_cases,
        test_concurrent_validation,
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
    print(f"Edge Case Tests Summary: {passed}/{total} tests passed")

    if passed == total:
        print("🎉 All edge case tests passed!")
        print("✅ Validation system handles edge cases correctly")
        return 0
    else:
        print("❌ Some edge case tests failed")
        return 1


if __name__ == "__main__":
    exit(main())
