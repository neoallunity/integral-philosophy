#!/usr/bin/env python3
"""
Validators module package for Integral Philosophy publishing system.
"""

from .validators import (
    BaseValidator,
    HTML5Validator,
    CSSValidator,
    JavaScriptValidator,
    LaTeXValidator,
)

# Import core validators
from .epub3_validator import EPUB3Validator
from .pdf_validator import PDFValidator
from .docx_validator import DOCXValidator
from .wcag_validator import WCAGValidator as WCAG21AAValidator
from .security_scanner import SecurityScanner

# Import with error handling for optional dependencies
try:
    from .content_integrity import ContentIntegrityValidator, CrossReferenceValidator
except ImportError as e:
    ContentIntegrityValidator = None
    CrossReferenceValidator = None
    print(f"Warning: content_integrity module unavailable: {e}")

try:
    from .quality_report import QualityReportGenerator, TransformationReport
except ImportError as e:
    QualityReportGenerator = None
    TransformationReport = None
    print(f"Warning: quality_report module unavailable: {e}")

try:
    from .performance_benchmark import PerformanceBenchmark, BenchmarkResult
except ImportError as e:
    PerformanceBenchmark = None
    BenchmarkResult = None
    print(f"Warning: performance_benchmark module unavailable: {e}")

try:
    from .batch_processor import BatchProcessor, BatchJob
except ImportError as e:
    BatchProcessor = None
    BatchJob = None
    print(f"Warning: batch_processor module unavailable: {e}")

try:
    from .quality_dashboard import QualityDashboard, QualityAlert
except ImportError as e:
    QualityDashboard = None
    QualityAlert = None
    print(f"Warning: quality_dashboard module unavailable: {e}")

__all__ = [
    "BaseValidator",
    "HTML5Validator",
    "CSSValidator",
    "JavaScriptValidator",
    "LaTeXValidator",
    "EPUB3Validator",
    "PDFValidator",
    "DOCXValidator",
    "WCAG21AAValidator",
    "SecurityScanner",
    "ContentIntegrityValidator",
    "CrossReferenceValidator",
    "QualityReportGenerator",
    "TransformationReport",
    "PerformanceBenchmark",
    "BenchmarkResult",
    "BatchProcessor",
    "BatchJob",
    "QualityDashboard",
    "QualityAlert",
]
