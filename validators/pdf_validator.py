#!/usr/bin/env python3
"""
PDF Validation Engine for Integral Philosophy publishing system.
Implements comprehensive PDF validation for academic publishing standards.
"""

import re
import json
import struct
import subprocess
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
import logging

from .validators import BaseValidator, ValidationResult, ValidationError

logger = logging.getLogger(__name__)

PDF_SIGNATURE = b"%PDF"
PDF_MIN_SIZE = 100  # Minimum PDF file size in bytes
PDF_MAX_SIZE = 100 * 1024 * 1024  # Maximum PDF file size (100MB)


@dataclass
class PDFInfo:
    """Information extracted from PDF file."""

    version: str
    page_count: int
    title: Optional[str]
    author: Optional[str]
    subject: Optional[str]
    creator: Optional[str]
    producer: Optional[str]
    creation_date: Optional[str]
    modification_date: Optional[str]
    is_encrypted: bool
    has_forms: bool
    has_annotations: bool
    file_size: int
    is_tagged: bool = False


class PDFValidator(BaseValidator):
    """Comprehensive PDF validation for academic publishing."""

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        super().__init__(config)
        self.required_fonts = [
            "Times-Roman",
            "Times-Italic",
            "Times-Bold",
            "Times-BoldItalic",
            "Helvetica",
            "Helvetica-Oblique",
            "Helvetica-Bold",
            "Helvetica-BoldOblique",
            "Courier",
            "Courier-Oblique",
            "Courier-Bold",
            "Courier-BoldOblique",
        ]
        self.academic_requirements = {
            "min_dpi": 300,
            "max_file_size": PDF_MAX_SIZE,
            "required_metadata": ["title", "author"],
            "allowed_formats": ["PDF-1.4", "PDF-1.5", "PDF-1.6", "PDF-1.7", "PDF-2.0"],
        }

    def validate(self, file_path: Path) -> ValidationResult:
        """Validate PDF file comprehensively."""
        errors = []
        stats = {"file_size": 0, "checks_performed": 0}

        try:
            # Extract PDF information
            pdf_info = self._extract_pdf_info(file_path)
            stats.update(
                {
                    "version": pdf_info.version,
                    "page_count": pdf_info.page_count,
                    "is_encrypted": pdf_info.is_encrypted,
                    "is_tagged": pdf_info.is_tagged,
                    "file_size": pdf_info.file_size,
                }
            )

            # Basic file validation
            basic_errors = self._validate_basic_structure(file_path, pdf_info)
            errors.extend(basic_errors)
            stats["basic_checks"] = len(basic_errors)

            # Metadata validation
            metadata_errors = self._validate_metadata(pdf_info)
            errors.extend(metadata_errors)
            stats["metadata_checks"] = len(metadata_errors)

            # Academic publishing standards
            academic_errors = self._validate_academic_standards(pdf_info)
            errors.extend(academic_errors)
            stats["academic_checks"] = len(academic_errors)

            # Accessibility validation
            accessibility_errors = self._validate_accessibility(pdf_info)
            errors.extend(accessibility_errors)
            stats["accessibility_checks"] = len(accessibility_errors)

            # Security validation
            security_errors = self._validate_security(pdf_info)
            errors.extend(security_errors)
            stats["security_checks"] = len(security_errors)

            # Performance validation
            performance_errors = self._validate_performance(file_path, pdf_info)
            errors.extend(performance_errors)
            stats["performance_checks"] = len(performance_errors)

            stats["total_checks"] = sum(
                [
                    stats["basic_checks"],
                    stats["metadata_checks"],
                    stats["academic_checks"],
                    stats["accessibility_checks"],
                    stats["security_checks"],
                    stats["performance_checks"],
                ]
            )

        except Exception as e:
            errors.append(
                ValidationError(
                    severity="error",
                    message=f"PDF validation failed: {str(e)}",
                    file_path=str(file_path),
                    rule_id="pdf-validation-error",
                )
            )

        return self._create_result(
            is_valid=len([e for e in errors if e.severity == "error"]) == 0,
            errors=errors,
            stats=stats,
        )

    def _extract_pdf_info(self, file_path: Path) -> PDFInfo:
        """Extract basic information from PDF file."""
        with open(file_path, "rb") as f:
            content = f.read()

        # Check PDF signature
        if not content.startswith(PDF_SIGNATURE):
            raise ValueError("Invalid PDF file signature")

        # Extract version
        version_match = re.search(rb"%PDF-(\d\.\d)", content)
        version = version_match.group(1).decode("utf-8") if version_match else "Unknown"

        # Look for basic metadata in PDF content
        title = self._extract_metadata_field(content, b"/Title")
        author = self._extract_metadata_field(content, b"/Author")
        subject = self._extract_metadata_field(content, b"/Subject")
        creator = self._extract_metadata_field(content, b"/Creator")
        producer = self._extract_metadata_field(content, b"/Producer")
        creation_date = self._extract_metadata_field(content, b"/CreationDate")
        modification_date = self._extract_metadata_field(content, b"/ModDate")

        # Check for encryption
        is_encrypted = b"/Encrypt" in content

        # Check for forms
        has_forms = b"/AcroForm" in content

        # Check for annotations
        has_annotations = b"/Annots" in content

        # Check if tagged (for accessibility)
        is_tagged = b"/StructTreeRoot" in content

        # Count pages (simplified - would need proper PDF parsing)
        page_count = len(re.findall(rb"/Type\s*/Page", content))

        return PDFInfo(
            version=version,
            page_count=page_count,
            title=title,
            author=author,
            subject=subject,
            creator=creator,
            producer=producer,
            creation_date=creation_date,
            modification_date=modification_date,
            is_encrypted=is_encrypted,
            has_forms=has_forms,
            has_annotations=has_annotations,
            file_size=len(content),
            is_tagged=is_tagged,
        )

    def _extract_metadata_field(self, content: bytes, field: bytes) -> Optional[str]:
        """Extract metadata field from PDF content."""
        pattern = field + rb"\s*\(([^)]+)\)"
        match = re.search(pattern, content)
        return match.group(1).decode("utf-8", errors="ignore") if match else None

    def _validate_basic_structure(
        self, file_path: Path, pdf_info: PDFInfo
    ) -> List[ValidationError]:
        """Validate basic PDF structure."""
        errors = []

        # Check file size
        if pdf_info.file_size < PDF_MIN_SIZE:
            errors.append(
                ValidationError(
                    severity="error",
                    message=f"PDF file too small: {pdf_info.file_size} bytes (minimum: {PDF_MIN_SIZE})",
                    file_path=str(file_path),
                    rule_id="pdf-file-too-small",
                )
            )

        if pdf_info.file_size > self.academic_requirements["max_file_size"]:
            errors.append(
                ValidationError(
                    severity="warning",
                    message=f"PDF file large: {pdf_info.file_size / (1024 * 1024):.1f}MB (consider optimization)",
                    file_path=str(file_path),
                    rule_id="pdf-file-large",
                )
            )

        # Check PDF version
        if pdf_info.version not in self.academic_requirements["allowed_formats"]:
            errors.append(
                ValidationError(
                    severity="warning",
                    message=f"PDF version {pdf_info.version} may not be compatible with all readers",
                    file_path=str(file_path),
                    rule_id="pdf-version-compatibility",
                )
            )

        # Check for pages
        if pdf_info.page_count == 0:
            errors.append(
                ValidationError(
                    severity="error",
                    message="PDF appears to have no pages",
                    file_path=str(file_path),
                    rule_id="pdf-no-pages",
                )
            )

        # Check for encryption (academic papers should generally be unencrypted)
        if pdf_info.is_encrypted:
            errors.append(
                ValidationError(
                    severity="warning",
                    message="PDF is encrypted - may affect accessibility and processing",
                    file_path=str(file_path),
                    rule_id="pdf-encrypted",
                )
            )

        return errors

    def _validate_metadata(self, pdf_info: PDFInfo) -> List[ValidationError]:
        """Validate PDF metadata for academic publishing."""
        errors = []

        # Check required metadata
        if not pdf_info.title:
            errors.append(
                ValidationError(
                    severity="error",
                    message="Missing title in PDF metadata",
                    file_path="metadata",
                    rule_id="pdf-missing-title",
                )
            )

        if not pdf_info.author:
            errors.append(
                ValidationError(
                    severity="error",
                    message="Missing author in PDF metadata",
                    file_path="metadata",
                    rule_id="pdf-missing-author",
                )
            )

        # Check for subject (important for academic papers)
        if not pdf_info.subject:
            errors.append(
                ValidationError(
                    severity="warning",
                    message="Missing subject in PDF metadata (recommended for academic papers)",
                    file_path="metadata",
                    rule_id="pdf-missing-subject",
                )
            )

        # Check creation date
        if not pdf_info.creation_date:
            errors.append(
                ValidationError(
                    severity="warning",
                    message="Missing creation date in PDF metadata",
                    file_path="metadata",
                    rule_id="pdf-missing-creation-date",
                )
            )

        return errors

    def _validate_academic_standards(self, pdf_info: PDFInfo) -> List[ValidationError]:
        """Validate academic publishing standards."""
        errors = []

        # Check page count (academic papers typically have minimum pages)
        if pdf_info.page_count < 2:
            errors.append(
                ValidationError(
                    severity="warning",
                    message="Very short document (may not be suitable for academic publishing)",
                    file_path="",
                    rule_id="pdf-too-short",
                )
            )

        # Check for appropriate producer/creator
        if pdf_info.producer and "scanner" in pdf_info.producer.lower():
            errors.append(
                ValidationError(
                    severity="warning",
                    message="Document appears to be scanned (check OCR quality)",
                    file_path="metadata",
                    rule_id="pdf-scanned-document",
                )
            )

        return errors

    def _validate_accessibility(self, pdf_info: PDFInfo) -> List[ValidationError]:
        """Validate PDF accessibility features."""
        errors = []

        # Check if tagged (essential for accessibility)
        if not pdf_info.is_tagged:
            errors.append(
                ValidationError(
                    severity="error",
                    message="PDF is not tagged (required for accessibility compliance)",
                    file_path="",
                    rule_id="pdf-not-tagged",
                )
            )

        # Check for forms (may affect accessibility)
        if pdf_info.has_forms:
            errors.append(
                ValidationError(
                    severity="warning",
                    message="PDF contains forms (ensure proper accessibility tags)",
                    file_path="",
                    rule_id="pdf-has-forms",
                )
            )

        return errors

    def _validate_security(self, pdf_info: PDFInfo) -> List[ValidationError]:
        """Validate PDF security aspects."""
        errors = []

        # Check for potentially dangerous features
        if pdf_info.has_forms:
            errors.append(
                ValidationError(
                    severity="info",
                    message="PDF contains forms (review for security implications)",
                    file_path="",
                    rule_id="pdf-forms-security",
                )
            )

        if pdf_info.has_annotations:
            errors.append(
                ValidationError(
                    severity="info",
                    message="PDF contains annotations (review content for sensitive information)",
                    file_path="",
                    rule_id="pdf-annotations-security",
                )
            )

        return errors

    def _validate_performance(
        self, file_path: Path, pdf_info: PDFInfo
    ) -> List[ValidationError]:
        """Validate PDF performance characteristics."""
        errors = []

        # Check file size vs page count ratio
        if pdf_info.page_count > 0:
            size_per_page = pdf_info.file_size / pdf_info.page_count
            if size_per_page > 1024 * 1024:  # > 1MB per page
                errors.append(
                    ValidationError(
                        severity="warning",
                        message=f"Large file size per page ({size_per_page / 1024:.1f}KB/page) - consider optimization",
                        file_path=str(file_path),
                        rule_id="pdf-size-per-page",
                    )
                )

        # Check for potential optimization opportunities
        if pdf_info.file_size > 10 * 1024 * 1024:  # > 10MB
            errors.append(
                ValidationError(
                    severity="info",
                    message="Large PDF file - consider image optimization and compression",
                    file_path=str(file_path),
                    rule_id="pdf-optimize-size",
                )
            )

        return errors

    def get_validation_summary(self, result: ValidationResult) -> str:
        """Get a human-readable summary of validation results."""
        if result.is_valid:
            return f"✅ PDF validation passed - {result.stats.get('page_count', 'Unknown')} pages, {result.stats.get('version', 'Unknown')} format"

        errors_by_severity = {"error": [], "warning": [], "info": []}
        for error in result.errors:
            errors_by_severity[error.severity].append(error)

        summary_parts = []
        if errors_by_severity["error"]:
            summary_parts.append(
                f"❌ {len(errors_by_severity['error'])} critical issues"
            )
        if errors_by_severity["warning"]:
            summary_parts.append(f"⚠️ {len(errors_by_severity['warning'])} warnings")
        if errors_by_severity["info"]:
            summary_parts.append(f"ℹ️ {len(errors_by_severity['info'])} suggestions")

        return " | ".join(summary_parts)
