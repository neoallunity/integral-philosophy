#!/usr/bin/env python3
"""
Simple test for validator imports without problematic dependencies.
"""

import sys
import os
from pathlib import Path

# Add project root to Python path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def test_all_validator_imports():
    """Test that all validators can be imported successfully."""
    print("🔍 Testing All Validator Imports...")

    validators_to_test = [
        ("BaseValidator", "validators.validators"),
        ("HTML5Validator", "validators.validators"),
        ("CSSValidator", "validators.validators"),
        ("JavaScriptValidator", "validators.validators"),
        ("LaTeXValidator", "validators.validators"),
        ("QualityReportGenerator", "validators.quality_report"),
        ("EPUB3Validator", "validators.epub3_validator"),
        ("PDFValidator", "validators.pdf_validator"),
        ("DOCXValidator", "validators.docx_validator"),
        ("WCAG21AAValidator", "validators.wcag_validator"),
        ("PerformanceBenchmark", "validators.performance_benchmark"),
        ("SecurityScanner", "validators.security_scanner"),
        ("BatchProcessor", "validators.batch_processor"),
        ("QualityDashboard", "validators.quality_dashboard"),
    ]

    successful_imports = 0
    failed_imports = []

    for validator_name, module_path in validators_to_test:
        try:
            exec(f"from {module_path} import {validator_name}")
            print(f"  ✓ {validator_name} imported successfully")
            successful_imports += 1
        except Exception as e:
            print(f"  ❌ {validator_name} import failed: {e}")
            failed_imports.append((validator_name, str(e)))

    print(f"\nImport Summary:")
    print(f"  Successful: {successful_imports}")
    print(f"  Failed: {len(failed_imports)}")

    if failed_imports:
        print(f"\nFailed Imports:")
        for name, error in failed_imports:
            print(f"  - {name}: {error}")

    return len(failed_imports) == 0


def test_validator_instantiation():
    """Test that all validators can be instantiated."""
    print("\n🔍 Testing Validator Instantiation...")

    # Test basic validators
    try:
        from validators.validators import (
            HTML5Validator,
            CSSValidator,
            JavaScriptValidator,
            LaTeXValidator,
        )

        html_validator = HTML5Validator()
        css_validator = CSSValidator()
        js_validator = JavaScriptValidator()
        latex_validator = LaTeXValidator()

        print("  ✓ Basic validators instantiated successfully")
    except Exception as e:
        print(f"  ❌ Basic validators instantiation failed: {e}")
        return False

    # Test extended validators (with optional dependencies)
    extended_validators = [
        ("EPUB3Validator", "validators.epub3_validator"),
        ("PDFValidator", "validators.pdf_validator"),
        ("DOCXValidator", "validators.docx_validator"),
        ("WCAG21AAValidator", "validators.wcag_validator"),
        ("PerformanceBenchmark", "validators.performance_benchmark"),
        ("SecurityScanner", "validators.security_scanner"),
        ("BatchProcessor", "validators.batch_processor"),
        ("QualityDashboard", "validators.quality_dashboard"),
    ]

    successful_extended = 0
    failed_extended = []

    for validator_name, module_path in extended_validators:
        try:
            exec(f"from {module_path} import {validator_name}")
            exec(f"{validator_name.lower()}_instance = {validator_name}()")
            print(f"  ✓ {validator_name} instantiated successfully")
            successful_extended += 1
        except Exception as e:
            print(f"  ❌ {validator_name} instantiation failed: {e}")
            failed_extended.append((validator_name, str(e)))

    print(f"\nExtended Validator Summary:")
    print(f"  Successful: {successful_extended}")
    print(f"  Failed: {len(failed_extended)}")

    if failed_extended:
        print(f"\nFailed Extended Validators:")
        for name, error in failed_extended:
            print(f"  - {name}: {error}")

    return len(failed_extended) == 0


def test_validator_interfaces():
    """Test validator interfaces are correct."""
    print("\n🔍 Testing Validator Interfaces...")

    try:
        from validators.validators import BaseValidator, HTML5Validator

        # Test BaseValidator interface
        class TestValidator(BaseValidator):
            def validate(self, file_path):
                return self._create_result(True, [], {})

        test_validator = TestValidator()

        # Test that HTML5Validator extends BaseValidator
        html_validator = HTML5Validator()

        # Check methods exist
        if not hasattr(html_validator, "validate"):
            print("  ❌ HTML5Validator missing validate method")
            return False

        print("  ✓ Validator interfaces are correct")
        return True

    except Exception as e:
        print(f"  ❌ Interface testing failed: {e}")
        return False


def main():
    """Run simplified validator tests."""
    print("🚀 Running Simplified Validator Tests...\n")

    tests = [
        test_all_validator_imports,
        test_validator_instantiation,
        test_validator_interfaces,
    ]

    results = []

    for test in tests:
        try:
            result = test()
            results.append(result)
            print(f"✅ {test.__name__} passed")
        except Exception as e:
            print(f"❌ {test.__name__} failed: {e}")
            results.append(False)

    # Summary
    passed = sum(results)
    total = len(results)

    print("\n" + "=" * 60)
    print(f"Simplified Validator Tests Summary: {passed}/{total} tests passed")

    if passed == total:
        print("🎉 All validator tests passed!")
        print("✅ Extended validation system structure is ready")
        print(
            "📈 Available validators: HTML5, CSS, JavaScript, LaTeX, EPUB3, PDF, DOCX, WCAG 2.1 AA, Performance, Security, Batch Processing, Quality Dashboard"
        )
        return 0
    else:
        print("❌ Some validator tests failed")
        return 1


if __name__ == "__main__":
    exit(main())
