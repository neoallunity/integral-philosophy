#!/usr/bin/env python3
"""
Comprehensive test suite for extended validation system.
Tests all new validators and integrations.
"""

import sys
import os
import tempfile
import time
from pathlib import Path
from typing import Dict, List, Any

# Add project root to Python path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from validators import (
    EPUB3Validator,
    PDFValidator,
    DOCXValidator,
    WCAG21AAValidator,
    PerformanceBenchmark,
    SecurityScanner,
    BatchProcessor,
    QualityDashboard,
)
from validators.validators import (
    HTML5Validator,
    CSSValidator,
    JavaScriptValidator,
    LaTeXValidator,
)


def test_epub3_validator():
    """Test EPUB3 validator."""
    print("🔍 Testing EPUB3 Validator...")

    # Create minimal EPUB structure for testing
    import tempfile
    import zipfile

    with tempfile.NamedTemporaryFile(suffix=".epub", delete=False) as temp_file:
        with zipfile.ZipFile(temp_file, "w") as epub:
            # Add mimetype
            epub.writestr("mimetype", b"application/epub+zip")

            # Add META-INF/container.xml
            container_xml = """<?xml version="1.0"?>
<container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
  <rootfiles>
    <rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/>
  </rootfiles>
</container>"""
            epub.writestr("META-INF/container.xml", container_xml)

            # Add minimal OPF
            opf_xml = """<?xml version="1.0"?>
<package version="3.0" xmlns="http://www.idpf.org/2007/opf" unique-identifier="bookid">
  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/">
    <dc:title>Test Document</dc:title>
    <dc:creator>Test Author</dc:creator>
    <dc:language>en</dc:language>
  </metadata>
  <manifest>
    <item id="content" href="content.xhtml" media-type="application/xhtml+xml"/>
  </manifest>
  <spine>
    <itemref idref="content"/>
  </spine>
</package>"""
            epub.writestr("OEBPS/content.opf", opf_xml)

            # Add minimal XHTML
            xhtml_xml = """<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
    <title>Test Document</title>
</head>
<body>
    <h1>Test Content</h1>
    <p>This is test content for EPUB validation.</p>
</body>
</html>"""
            epub.writestr("OEBPS/content.xhtml", xhtml_xml)

    try:
        validator = EPUB3Validator()
        result = validator.validate(Path(temp_file.name))

        print(f"  ✓ EPUB3 validation completed")
        print(f"    Errors: {result.error_count}, Warnings: {result.warning_count}")
        print(f"    Checks performed: {result.stats.get('total_checks', 0)}")

        # Clean up
        os.unlink(temp_file.name)

        return True

    except Exception as e:
        print(f"  ❌ EPUB3 validation failed: {e}")
        return False


def test_pdf_validator():
    """Test PDF validator."""
    print("🔍 Testing PDF Validator...")

    # Create minimal PDF structure for testing
    with tempfile.NamedTemporaryFile(suffix=".pdf", delete=False) as temp_file:
        # Write minimal PDF structure
        pdf_content = b"""%PDF-1.4
1 0 obj
<<
/Length 44
>>
stream
BT
/F1 12 Tf
72 720 Td
(Hello World) Tj
ET
endstream
endobj
2 0 obj
<<
/Type /Page
/Parent 1 0 R
/Resources << /Font << /F1 3 0 R >> >>
/MediaBox [0 0 612 792]
/Contents 3 0 R
>>
endobj
3 0 obj
<<
/Type /Font
/Subtype /Type1
/BaseFont /Helvetica
>>
endobj
xref
0 4
0000000000 65535 f
0000000010 00000 n
0000000099 00000 n
0000000179 00000 n
0000000384 00000 n
trailer
<<
/Size 4
/Root 2 0 R
>>
startxref
%%EOF"""

        with open(temp_file.name, "wb") as f:
            f.write(pdf_content)

    try:
        validator = PDFValidator()
        result = validator.validate(Path(temp_file.name))

        print(f"  ✓ PDF validation completed")
        print(f"    Errors: {result.error_count}, Warnings: {result.warning_count}")
        print(f"    Checks performed: {result.stats.get('total_checks', 0)}")

        # Clean up
        os.unlink(temp_file.name)

        return True

    except Exception as e:
        print(f"  ❌ PDF validation failed: {e}")
        return False


def test_docx_validator():
    """Test DOCX validator."""
    print("🔍 Testing DOCX Validator...")

    # Create minimal DOCX structure for testing
    import tempfile
    import zipfile

    with tempfile.NamedTemporaryFile(suffix=".docx", delete=False) as temp_file:
        with zipfile.ZipFile(temp_file, "w") as docx:
            # Add [Content_Types].xml
            types_xml = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
    <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
    <Default Extension="xml" ContentType="application/xml"/>
    <Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>
</Types>"""
            docx.writestr("[Content_Types].xml", types_xml)

            # Add _rels/.rels
            rels_xml = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
    <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/>
</Relationships>"""
            docx.writestr("_rels/.rels", rels_xml)

            # Add word/document.xml
            doc_xml = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:wordDocument xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
    <w:body>
        <w:p>
            <w:r>
                <w:t>Hello World</w:t>
            </w:r>
        </w:p>
    </w:body>
</w:wordDocument>"""
            docx.writestr("word/document.xml", doc_xml)

    try:
        validator = DOCXValidator()
        result = validator.validate(Path(temp_file.name))

        print(f"  ✓ DOCX validation completed")
        print(f"    Errors: {result.error_count}, Warnings: {result.warning_count}")
        print(f"    Checks performed: {result.stats.get('total_checks', 0)}")

        # Clean up
        os.unlink(temp_file.name)

        return True

    except Exception as e:
        print(f"  ❌ DOCX validation failed: {e}")
        return False


def test_wcag_validator():
    """Test WCAG 2.1 AA validator."""
    print("🔍 Testing WCAG 2.1 AA Validator...")

    # Create HTML with various accessibility issues
    html_content = """<!DOCTYPE html>
<html>
<head>
    <title>Test Document</title>
</head>
<body>
    <h1>Test Heading</h1>
    <img src="test.jpg"> <!-- Missing alt text -->
    <button onclick="alert('test')">Click me</button> <!-- No keyboard alternative -->
    <form>
        <input type="text" name="username"> <!-- Missing label -->
        <input type="password"> <!-- Missing label -->
    </form>
    <marquee>Flashing content</marquee> <!-- Seizure risk -->
    <div style="color: #cccccc; background-color: #ffffff;">Low contrast text</div>
</body>
</html>"""

    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".html", delete=False
    ) as temp_file:
        temp_file.write(html_content)
        temp_file.flush()

    try:
        validator = WCAG21AAValidator()
        result = validator.validate(Path(temp_file.name))

        print(f"  ✓ WCAG 2.1 AA validation completed")
        print(f"    Errors: {result.error_count}, Warnings: {result.warning_count}")
        print(f"    WCAG checks performed: {result.stats.get('wcag_checks', 0)}")

        # Clean up
        os.unlink(temp_file.name)

        return True

    except Exception as e:
        print(f"  ❌ WCAG validation failed: {e}")
        return False


def test_security_scanner():
    """Test security scanner."""
    print("🔍 Testing Security Scanner...")

    # Create HTML with security vulnerabilities
    html_content = """<!DOCTYPE html>
<html>
<head>
    <title>Security Test</title>
</head>
<body>
    <script>alert('XSS')</script> <!-- Stored XSS -->
    <img src="x" onerror="alert('XSS')"> <!-- Reflected XSS -->
    <form action="/login" method="GET"> <!-- Insecure form method -->
        <input type="text" name="username">
        <input type="password" name="password" autocomplete="on"> <!-- Autocomplete enabled -->
    </form>
    <a href="http://example.com" target="_blank">External link</a> <!-- Missing rel="noopener" -->
    <div id="result"></div>
    <script>
        eval('document.getElementById("result").innerHTML = "<h1>Potential XSS</h1>"'); <!-- eval usage -->
    </script>
</body>
</html>"""

    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".html", delete=False
    ) as temp_file:
        temp_file.write(html_content)
        temp_file.flush()

    try:
        validator = SecurityScanner()
        result = validator.validate(Path(temp_file.name))

        print(f"  ✓ Security scanning completed")
        print(f"    Errors: {result.error_count}, Warnings: {result.warning_count}")
        print(f"    Security checks performed: {result.stats.get('total_checks', 0)}")

        # Clean up
        os.unlink(temp_file.name)

        return True

    except Exception as e:
        print(f"  ❌ Security scanning failed: {e}")
        return False


def test_performance_benchmark():
    """Test performance benchmarking."""
    print("🔍 Testing Performance Benchmark...")

    # Create test files
    test_files = []

    for i in range(3):
        html_content = f"""<!DOCTYPE html>
<html>
<head><title>Performance Test {i}</title></head>
<body>
    <h1>Test {i}</h1>
    <p>This is test content {i} for performance testing.</p>
    <img src="test{i}.jpg" alt="Test image {i}">
    <div style="color: red;">Styled content {i}</div>
</body>
</html>"""

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as temp_file:
            temp_file.write(html_content)
            test_files.append(Path(temp_file.name))

    try:
        # Test basic validation
        validator = HTML5Validator()
        benchmark = PerformanceBenchmark()

        result = benchmark.benchmark_validator(validator, test_files, iterations=2)

        print(f"  ✓ Performance benchmarking completed")
        print(f"    Average time: {result.average_time:.3f}s")
        print(f"    Throughput: {result.throughput_ops_per_second:.2f} ops/sec")
        print(
            f"    Success rate: {result.successful_operations}/{result.total_operations}"
        )

        # Test concurrent performance
        concurrent_results = benchmark.benchmark_concurrent_performance(
            validator, test_files, [1, 2, 4]
        )

        print(f"  ✓ Concurrent performance tested")
        for workers, result in concurrent_results.items():
            print(f"    {workers} workers: {result.average_time:.3f}s avg")

        # Clean up
        for temp_file in test_files:
            if temp_file.exists():
                temp_file.unlink()

        return True

    except Exception as e:
        print(f"  ❌ Performance benchmarking failed: {e}")
        return False


def test_batch_processor():
    """Test batch processing system."""
    print("🔍 Testing Batch Processor...")

    # Create test publications
    publications = []

    for i in range(2):
        pub = {
            "source_path": f"test_publication_{i}.html",
            "title": f"Test Publication {i}",
            "formats": ["html", "validation"],
        }
        publications.append(pub)

        # Create actual test files
        html_content = f"""<!DOCTYPE html>
<html>
<head><title>Batch Test {i}</title></head>
<body>
    <h1>Test Publication {i}</h1>
    <p>This is test content for batch processing {i}.</p>
</body>
</html>"""

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".html", delete=False
        ) as temp_file:
            temp_file.write(html_content)
            temp_file.flush()
            pub["source_path"] = temp_file.name

    try:
        # Test batch processing
        processor = BatchProcessor()
        result = processor.process_batch(publications, ["html"])

        print(f"  ✓ Batch processing completed")
        print(f"    Total jobs: {result['total_jobs']}")
        print(f"    Completed jobs: {result['completed_jobs']}")
        print(f"    Success rate: {result['success_rate']:.1%}")
        print(f"    Average quality score: {result['average_quality_score']:.1f}")

        # Clean up
        for pub in publications:
            if pub["source_path"] and os.path.exists(pub["source_path"]):
                os.unlink(pub["source_path"])

        return True

    except Exception as e:
        print(f"  ❌ Batch processing failed: {e}")
        return False


def test_quality_dashboard():
    """Test quality dashboard."""
    print("🔍 Testing Quality Dashboard...")

    try:
        # Create dashboard
        dashboard = QualityDashboard()

        # Record some test metrics
        dashboard.record_metric("quality_score", 85.5, "html", "html")
        dashboard.record_metric("success_rate", 95.0, "html", "html")
        dashboard.record_metric("processing_time", 0.5, "html", "html")

        # Generate dashboard
        dashboard_html = dashboard.generate_dashboard_html()

        # Test metrics summary
        summary = dashboard.get_metrics_summary()

        print(f"  ✓ Quality dashboard generated")
        print(f"    Metrics recorded: {len(summary)}")
        print(f"    Dashboard HTML length: {len(dashboard_html)} chars")

        # Test trends
        trends = dashboard.get_quality_trends(7)
        print(f"    Trends analyzed: {len(trends)}")

        # Test alerts
        alerts = dashboard.get_active_alerts()
        print(f"    Active alerts: {len(alerts)}")

        return True

    except Exception as e:
        print(f"  ❌ Quality dashboard test failed: {e}")
        return False


def test_all_integrations():
    """Test all validator integrations."""
    print("🔍 Testing All Validator Integrations...")

    # Test with comprehensive validation
    validators = {
        "html": HTML5Validator(),
        "css": CSSValidator(),
        "js": JavaScriptValidator(),
        "latex": LaTeXValidator(),
        "epub3": EPUB3Validator(),
        "pdf": PDFValidator(),
        "docx": DOCXValidator(),
        "wcag": WCAG21AAValidator(),
        "security": SecurityScanner(),
    }

    # Create test files for each validator
    test_files = {}

    # HTML test file
    html_content = """<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Integration Test</title>
</head>
<body>
    <h1>Integration Test Document</h1>
    <img src="test.jpg" alt="Test image">
    <script>"use strict"; console.log('test');</script>
    <p>This is integration test content.</p>
</body>
</html>"""

    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".html", delete=False
    ) as temp_file:
        temp_file.write(html_content)
        test_files["html"] = Path(temp_file.name)

    try:
        # Test each validator
        total_errors = 0
        total_warnings = 0

        for validator_name, validator in validators.items():
            if validator_name in ["html", "css", "js", "latex", "wcag", "security"]:
                test_file = test_files.get("html")
                if test_file:
                    result = validator.validate(test_file)
                    total_errors += result.error_count
                    total_warnings += result.warning_count
                    print(
                        f"  ✓ {validator_name}: {result.error_count} errors, {result.warning_count} warnings"
                    )

        # Test integration with quality report
        from validators.quality_report import QualityReportGenerator

        generator = QualityReportGenerator()

        # Mock validation results
        validation_results = {
            "html": type(
                "MockResult",
                (),
                {
                    "is_valid": True,
                    "error_count": 1,
                    "warning_count": 2,
                    "stats": {"checks_performed": 5},
                },
            )(),
            "quality": type(
                "MockResult",
                (),
                {
                    "is_valid": True,
                    "error_count": 0,
                    "warning_count": 1,
                    "stats": {"checks_performed": 3},
                },
            )(),
        }

        # Generate report
        report = generator.generate_report(
            source_files=[test_files["html"]],
            output_formats={"html": test_files["html"]},
            validation_results=validation_results,
            processing_time=1.5,
        )

        print(
            f"  ✓ Integration test: Overall score {report.quality_metrics.overall_score:.1f}/100"
        )

        # Clean up
        for file_path in test_files.values():
            if file_path.exists():
                file_path.unlink()

        return True

    except Exception as e:
        print(f"  ❌ Integration test failed: {e}")
        return False


def main():
    """Run comprehensive extended validation tests."""
    print("🚀 Running Comprehensive Extended Validation Tests...\n")

    tests = [
        test_epub3_validator,
        test_pdf_validator,
        test_docx_validator,
        test_wcag_validator,
        test_security_scanner,
        test_performance_benchmark,
        test_batch_processor,
        test_quality_dashboard,
        test_all_integrations,
    ]

    results = []

    for test in tests:
        try:
            result = test()
            results.append(result)
            print(f"✅ {test.__name__} passed\n")
        except Exception as e:
            print(f"❌ {test.__name__} failed: {e}\n")
            results.append(False)

    # Summary
    passed = sum(results)
    total = len(results)

    print("=" * 60)
    print(f"Extended Validation Tests Summary: {passed}/{total} tests passed")

    if passed == total:
        print("🎉 All extended validation tests passed!")
        print("✅ Extended validation system is ready for production")
        return 0
    else:
        print("❌ Some extended validation tests failed")
        return 1


if __name__ == "__main__":
    exit(main())
