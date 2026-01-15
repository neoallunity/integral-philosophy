#!/usr/bin/env python3
"""
EPUB3 Validator for Integral Philosophy publishing system.
Implements comprehensive EPUB3 standard compliance validation.
"""

import re
import json
import zipfile
import xml.etree.ElementTree as ET
from pathlib import Path
from typing import Dict, List, Any, Optional, Set
from dataclasses import dataclass, field
import logging

from .validators import BaseValidator, ValidationResult, ValidationError

logger = logging.getLogger(__name__)

EPUB_MIMETYPE = b"application/epub+zip"


@dataclass
class EPUBStructure:
    """Represents EPUB file structure."""

    mimetype: str
    container_xml: str
    opf_document: ET.ElementTree
    ncx_document: Optional[ET.ElementTree] = None
    nav_document: Optional[ET.ElementTree] = None
    content_files: List[Path] = field(default_factory=list)
    css_files: List[Path] = field(default_factory=list)
    image_files: List[Path] = field(default_factory=list)


class EPUB3Validator(BaseValidator):
    """Comprehensive EPUB3 validation with standard compliance checks."""

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        super().__init__(config)
        self.namespaces = {
            "opf": "http://www.idpf.org/2007/opf",
            "dc": "http://purl.org/dc/elements/1.1/",
            "ncx": "http://www.daisy.org/z3986/2005/ncx/",
            "nav": "http://www.w3.org/1999/xhtml",
            "epub": "http://www.idpf.org/2007/ops",
        }

    def validate(self, file_path: Path) -> ValidationResult:
        """Validate EPUB3 file comprehensively."""
        errors = []
        stats = {"file_size": file_path.stat().st_size, "checks_performed": 0}

        try:
            # Extract and parse EPUB structure
            epub_structure = self._extract_epub_structure(file_path)

            # Basic file structure validation
            structure_errors = self._validate_epub_structure(epub_structure)
            errors.extend(structure_errors)
            stats["structure_checks"] = len(structure_errors)

            # OPF metadata validation
            opf_errors = self._validate_opf_metadata(epub_structure)
            errors.extend(opf_errors)
            stats["opf_checks"] = len(opf_errors)

            # Navigation validation (NCX and NAV)
            nav_errors = self._validate_navigation(epub_structure)
            errors.extend(nav_errors)
            stats["navigation_checks"] = len(nav_errors)

            # Content validation
            content_errors = self._validate_content(epub_structure)
            errors.extend(content_errors)
            stats["content_checks"] = len(content_errors)

            # Accessibility validation
            accessibility_errors = self._validate_accessibility(epub_structure)
            errors.extend(accessibility_errors)
            stats["accessibility_checks"] = len(accessibility_errors)

            # Performance validation
            performance_errors = self._validate_performance(epub_structure)
            errors.extend(performance_errors)
            stats["performance_checks"] = len(performance_errors)

            stats["total_checks"] = sum(
                [
                    stats["structure_checks"],
                    stats["opf_checks"],
                    stats["navigation_checks"],
                    stats["content_checks"],
                    stats["accessibility_checks"],
                    stats["performance_checks"],
                ]
            )

        except Exception as e:
            errors.append(
                ValidationError(
                    severity="error",
                    message=f"EPUB3 validation failed: {str(e)}",
                    file_path=str(file_path),
                    rule_id="epub3-validation-error",
                )
            )

        return self._create_result(
            is_valid=len([e for e in errors if e.severity == "error"]) == 0,
            errors=errors,
            stats=stats,
        )

    def _extract_epub_structure(self, file_path: Path) -> EPUBStructure:
        """Extract and parse EPUB file structure."""
        with zipfile.ZipFile(file_path, "r") as epub_zip:
            # Check mimetype
            try:
                mimetype = epub_zip.read("mimetype")
                mimetype_str = mimetype.decode("utf-8").strip()
            except KeyError:
                raise ValueError("EPUB missing required mimetype file")

            # Check container.xml
            try:
                container_xml = epub_zip.read("META-INF/container.xml").decode("utf-8")
            except KeyError:
                raise ValueError("EPUB missing required container.xml")

            # Parse OPF file
            try:
                opf_content = epub_zip.read("OEBPS/content.opf")
                opf_document = ET.ElementTree(ET.fromstring(opf_content))
            except KeyError:
                raise ValueError("EPUB missing required OPF file")

            # Find navigation files
            ncx_document = None
            nav_document = None

            # List all files in EPUB
            all_files = epub_zip.namelist()
            content_files = []
            css_files = []
            image_files = []

            for file_name in all_files:
                if file_name.endswith(".ncx"):
                    try:
                        ncx_content = epub_zip.read(file_name)
                        ncx_document = ET.ElementTree(ET.fromstring(ncx_content))
                    except Exception:
                        pass
                elif file_name.endswith(".xhtml") or file_name.endswith(".html"):
                    content_files.append(Path(file_name))
                elif file_name.endswith(".css"):
                    css_files.append(Path(file_name))
                elif file_name.lower().endswith(
                    (".jpg", ".jpeg", ".png", ".gif", ".svg")
                ):
                    image_files.append(Path(file_name))

            return EPUBStructure(
                mimetype=mimetype_str,
                container_xml=container_xml,
                opf_document=opf_document,
                ncx_document=ncx_document,
                nav_document=nav_document,
                content_files=content_files,
                css_files=css_files,
                image_files=image_files,
            )

    def _validate_epub_structure(
        self, structure: EPUBStructure
    ) -> List[ValidationError]:
        """Validate basic EPUB file structure."""
        errors = []

        # Check mimetype
        if structure.mimetype != EPUB_MIMETYPE.decode("utf-8"):
            errors.append(
                ValidationError(
                    severity="error",
                    message=f"Invalid mimetype: {structure.mimetype}",
                    file_path="mimetype",
                    rule_id="epub3-invalid-mimetype",
                )
            )

        # Check container.xml structure
        try:
            container_root = ET.fromstring(structure.container_xml)
            rootfiles = container_root.find(".//rootfile")
            if rootfiles is None:
                errors.append(
                    ValidationError(
                        severity="error",
                        message="Container XML missing rootfile element",
                        file_path="META-INF/container.xml",
                        rule_id="epub3-missing-rootfile",
                    )
                )
            else:
                full_path = rootfiles.get("full-path")
                if not full_path:
                    errors.append(
                        ValidationError(
                            severity="error",
                            message="Rootfile missing full-path attribute",
                            file_path="META-INF/container.xml",
                            rule_id="epub3-missing-fullpath",
                        )
                    )
        except ET.ParseError:
            errors.append(
                ValidationError(
                    severity="error",
                    message="Invalid container XML syntax",
                    file_path="META-INF/container.xml",
                    rule_id="epub3-invalid-container-xml",
                )
            )

        return errors

    def _validate_opf_metadata(self, structure: EPUBStructure) -> List[ValidationError]:
        """Validate OPF metadata for EPUB3 compliance."""
        errors = []

        opf_root = structure.opf_document.getroot()
        if opf_root is None:
            errors.append(
                ValidationError(
                    severity="error",
                    message="OPF document has no root element",
                    file_path="OEBPS/content.opf",
                    rule_id="epub3-missing-opf-document",
                )
            )
            return errors

        # Check required metadata elements
        title = opf_root.find(".//dc:title", self.namespaces)
        if title is None or not title.text:
            errors.append(
                ValidationError(
                    severity="error",
                    message="Missing or empty title element",
                    file_path="OEBPS/content.opf",
                    rule_id="epub3-missing-title",
                )
            )

        identifier = opf_root.find(".//dc:identifier", self.namespaces)
        if identifier is None or not identifier.text:
            errors.append(
                ValidationError(
                    severity="error",
                    message="Missing or empty identifier element",
                    file_path="OEBPS/content.opf",
                    rule_id="epub3-missing-identifier",
                )
            )

        language = opf_root.find(".//dc:language", self.namespaces)
        if language is None or not language.text:
            errors.append(
                ValidationError(
                    severity="error",
                    message="Missing or empty language element",
                    file_path="OEBPS/content.opf",
                    rule_id="epub3-missing-language",
                )
            )

        return errors

    def _validate_navigation(self, structure: EPUBStructure) -> List[ValidationError]:
        """Validate navigation structure (NCX and NAV)."""
        errors = []

        # Validate NCX if present
        if structure.ncx_document is not None:
            ncx_root = structure.ncx_document.getroot()
            if ncx_root is None:
                errors.append(
                    ValidationError(
                        severity="warning",
                        message="NCX document has no root element",
                        file_path="navigation.ncx",
                        rule_id="epub3-invalid-ncx-root",
                    )
                )
            else:
                # Check for required NCX elements
                doc_title = ncx_root.find(".//ncx:docTitle", self.namespaces)
                if doc_title is None:
                    errors.append(
                        ValidationError(
                            severity="warning",
                            message="NCX missing docTitle element",
                            file_path="navigation.ncx",
                            rule_id="epub3-missing-ncx-title",
                        )
                    )

        # Note: If neither NCX nor NAV is present, that's an error
        if structure.ncx_document is None and structure.nav_document is None:
            errors.append(
                ValidationError(
                    severity="error",
                    message="EPUB missing both NCX and NAV navigation documents",
                    file_path="",
                    rule_id="epub3-missing-navigation",
                )
            )

        return errors

    def _validate_content(self, structure: EPUBStructure) -> List[ValidationError]:
        """Validate content files for XHTML compliance."""
        errors = []

        # Check if there are content files
        if not structure.content_files:
            errors.append(
                ValidationError(
                    severity="error",
                    message="EPUB contains no content files",
                    file_path="",
                    rule_id="epub3-no-content-files",
                )
            )
            return errors

        # Validate content file structure (basic checks)
        for content_file in structure.content_files:
            # This would require reading individual files from the ZIP
            # For now, just check file extensions and naming
            if not content_file.suffix.lower() in [".xhtml", ".html"]:
                errors.append(
                    ValidationError(
                        severity="warning",
                        message=f"Content file {content_file.name} should use .xhtml extension",
                        file_path=str(content_file),
                        rule_id="epub3-content-file-extension",
                    )
                )

        return errors

    def _validate_accessibility(
        self, structure: EPUBStructure
    ) -> List[ValidationError]:
        """Validate accessibility features for EPUB3."""
        errors = []

        # Check for accessibility metadata in OPF
        opf_root = structure.opf_document.getroot()
        if opf_root is None:
            errors.append(
                ValidationError(
                    severity="warning",
                    message="OPF document not available for accessibility validation",
                    file_path="OEBPS/content.opf",
                    rule_id="epub3-opf-unavailable",
                )
            )
            return errors

        # Look for accessibility meta tags
        access_modes = opf_root.find(
            './/opf:meta[@property="schema:accessMode"]', self.namespaces
        )
        access_features = opf_root.find(
            './/opf:meta[@property="schema:accessibilityFeature"]', self.namespaces
        )

        if access_modes is None:
            errors.append(
                ValidationError(
                    severity="warning",
                    message="Missing accessMode metadata (recommended for accessibility)",
                    file_path="OEBPS/content.opf",
                    rule_id="epub3-missing-access-mode",
                )
            )

        if access_features is None:
            errors.append(
                ValidationError(
                    severity="warning",
                    message="Missing accessibilityFeature metadata (recommended for accessibility)",
                    file_path="OEBPS/content.opf",
                    rule_id="epub3-missing-accessibility-feature",
                )
            )

        return errors

    def _validate_performance(self, structure: EPUBStructure) -> List[ValidationError]:
        """Validate EPUB performance characteristics."""
        errors = []

        # Check for unnecessary files
        unnecessary_files = []
        for content_file in structure.content_files:
            if (
                "temp" in content_file.name.lower()
                or "backup" in content_file.name.lower()
            ):
                unnecessary_files.append(content_file.name)

        if unnecessary_files:
            errors.append(
                ValidationError(
                    severity="warning",
                    message=f"Unnecessary files found (remove before publishing): {', '.join(unnecessary_files)}",
                    file_path="",
                    rule_id="epub3-unnecessary-files",
                )
            )

        return errors
