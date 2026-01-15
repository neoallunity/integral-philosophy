#!/usr/bin/env python3
"""
DOCX Validator for Integral Philosophy publishing system.
Implements comprehensive DOCX validation with Microsoft Word compatibility checks.
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

DOCX_CONTENT_TYPES = {
    "word/document.xml": "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml",
    "word/styles.xml": "application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml",
    "word/numbering.xml": "application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml",
}

WORD_COMPATIBILITY_VERSIONS = ["2007", "2010", "2013", "2016", "2019", "365"]


@dataclass
class DOCXStructure:
    """Represents DOCX file structure."""

    document_xml: ET.ElementTree
    styles_xml: Optional[ET.ElementTree] = None
    numbering_xml: Optional[ET.ElementTree] = None
    content_types: Dict[str, str] = field(default_factory=dict)
    relationships: Dict[str, str] = field(default_factory=dict)
    core_properties: Dict[str, str] = field(default_factory=dict)
    app_properties: Dict[str, str] = field(default_factory=dict)
    media_files: List[str] = field(default_factory=list)
    font_table: List[Dict[str, str]] = field(default_factory=list)


class DOCXValidator(BaseValidator):
    """Comprehensive DOCX validation with Word compatibility checks."""

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        super().__init__(config)
        self.namespaces = {
            "w": "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
            "r": "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
            "ct": "http://schemas.openxmlformats.org/package/2006/content-types",
            "cp": "http://schemas.openxmlformats.org/package/2006/metadata/core-properties",
            "ap": "http://schemas.openxmlformats.org/officeDocument/2006/extended-properties",
            "dc": "http://purl.org/dc/elements/1.1/",
            "dcterms": "http://purl.org/dc/terms/",
            "dcmitype": "http://purl.org/dc/dcmitype/",
            "xsi": "http://www.w3.org/2001/XMLSchema-instance",
        }

        self.word_requirements = {
            "max_file_size": 50 * 1024 * 1024,  # 50MB
            "min_content_length": 100,  # Minimum characters
            "required_properties": ["title", "author"],
            "recommended_styles": ["Normal", "Heading 1", "Heading 2", "Heading 3"],
        }

    def validate(self, file_path: Path) -> ValidationResult:
        """Validate DOCX file comprehensively."""
        errors = []
        stats = {"file_size": file_path.stat().st_size, "checks_performed": 0}

        try:
            # Extract DOCX structure
            docx_structure = self._extract_docx_structure(file_path)

            # Basic file structure validation
            structure_errors = self._validate_docx_structure(docx_structure)
            errors.extend(structure_errors)
            stats["structure_checks"] = len(structure_errors)

            # Word compatibility validation
            compatibility_errors = self._validate_word_compatibility(docx_structure)
            errors.extend(compatibility_errors)
            stats["compatibility_checks"] = len(compatibility_errors)

            # Content validation
            content_errors = self._validate_content(docx_structure)
            errors.extend(content_errors)
            stats["content_checks"] = len(content_errors)

            # Style validation
            style_errors = self._validate_styles(docx_structure)
            errors.extend(style_errors)
            stats["style_checks"] = len(style_errors)

            # Metadata validation
            metadata_errors = self._validate_metadata(docx_structure)
            errors.extend(metadata_errors)
            stats["metadata_checks"] = len(metadata_errors)

            # Accessibility validation
            accessibility_errors = self._validate_accessibility(docx_structure)
            errors.extend(accessibility_errors)
            stats["accessibility_checks"] = len(accessibility_errors)

            # Performance validation
            performance_errors = self._validate_performance(file_path, docx_structure)
            errors.extend(performance_errors)
            stats["performance_checks"] = len(performance_errors)

            stats["total_checks"] = sum(
                [
                    stats["structure_checks"],
                    stats["compatibility_checks"],
                    stats["content_checks"],
                    stats["style_checks"],
                    stats["metadata_checks"],
                    stats["accessibility_checks"],
                    stats["performance_checks"],
                ]
            )

        except Exception as e:
            errors.append(
                ValidationError(
                    severity="error",
                    message=f"DOCX validation failed: {str(e)}",
                    file_path=str(file_path),
                    rule_id="docx-validation-error",
                )
            )

        return self._create_result(
            is_valid=len([e for e in errors if e.severity == "error"]) == 0,
            errors=errors,
            stats=stats,
        )

    def _extract_docx_structure(self, file_path: Path) -> DOCXStructure:
        """Extract and parse DOCX file structure."""
        with zipfile.ZipFile(file_path, "r") as docx_zip:
            # Parse main document
            try:
                document_content = docx_zip.read("word/document.xml")
                document_xml = ET.ElementTree(ET.fromstring(document_content))
            except KeyError:
                raise ValueError("DOCX missing required document.xml")

            # Parse styles if available
            styles_xml = None
            try:
                styles_content = docx_zip.read("word/styles.xml")
                styles_xml = ET.ElementTree(ET.fromstring(styles_content))
            except KeyError:
                pass  # Styles are optional

            # Parse numbering if available
            numbering_xml = None
            try:
                numbering_content = docx_zip.read("word/numbering.xml")
                numbering_xml = ET.ElementTree(ET.fromstring(numbering_content))
            except KeyError:
                pass  # Numbering is optional

            # Parse content types
            content_types = {}
            try:
                types_content = docx_zip.read("[Content_Types].xml")
                types_root = ET.fromstring(types_content)
                for override in types_root.findall(".//ct:Override", self.namespaces):
                    part_name = override.get("PartName")
                    content_type = override.get("ContentType")
                    if part_name and content_type:
                        content_types[part_name] = content_type
            except KeyError:
                pass

            # Parse relationships
            relationships = {}
            try:
                rels_content = docx_zip.read("word/_rels/document.xml.rels")
                rels_root = ET.fromstring(rels_content)
                for relationship in rels_root.findall(
                    ".//r:Relationship", self.namespaces
                ):
                    rel_id = relationship.get("Id")
                    target = relationship.get("Target")
                    if rel_id and target:
                        relationships[rel_id] = target
            except KeyError:
                pass

            # Parse core properties
            core_properties = {}
            try:
                core_content = docx_zip.read("docProps/core.xml")
                core_root = ET.fromstring(core_content)
                for elem in core_root:
                    tag_str = str(elem.tag)
                    if tag_str.startswith(f"{{{self.namespaces['dc']}}}"):
                        core_properties[tag_str.split("}")[-1]] = elem.text
                    elif tag_str.startswith(f"{{{self.namespaces['dcterms']}}}"):
                        core_properties[tag_str.split("}")[-1]] = elem.text
            except KeyError:
                pass

            # Parse app properties
            app_properties = {}
            try:
                app_content = docx_zip.read("docProps/app.xml")
                app_root = ET.fromstring(app_content)
                for elem in app_root:
                    tag_str = str(elem.tag)
                    if tag_str.startswith(f"{{{self.namespaces['ap']}}}"):
                        app_properties[tag_str.split("}")[-1]] = elem.text
            except KeyError:
                pass

            # Find media files
            media_files = []
            for file_info in docx_zip.infolist():
                if file_info.filename.startswith("word/media/"):
                    media_files.append(file_info.filename)

            # Extract font table
            font_table = []
            try:
                font_content = docx_zip.read("word/fontTable.xml")
                font_root = ET.fromstring(font_content)
                for font in font_root.findall(".//w:font", self.namespaces):
                    font_info = {
                        "name": font.get(
                            "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}name"
                        ),
                        "charset": font.get(
                            "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}charset"
                        ),
                        "family": font.get(
                            "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}family"
                        ),
                    }
                    font_table.append(font_info)
            except KeyError:
                pass

            return DOCXStructure(
                document_xml=document_xml,
                styles_xml=styles_xml,
                numbering_xml=numbering_xml,
                content_types=content_types,
                relationships=relationships,
                core_properties=core_properties,
                app_properties=app_properties,
                media_files=media_files,
                font_table=font_table,
            )

    def _validate_docx_structure(
        self, structure: DOCXStructure
    ) -> List[ValidationError]:
        """Validate basic DOCX file structure."""
        errors = []

        # Check document XML exists
        if structure.document_xml is None:
            errors.append(
                ValidationError(
                    severity="error",
                    message="Missing document.xml",
                    file_path="word/document.xml",
                    rule_id="docx-missing-document",
                )
            )
            return errors

        # Check content types
        if not structure.content_types:
            errors.append(
                ValidationError(
                    severity="warning",
                    message="Missing [Content_Types].xml",
                    file_path="[Content_Types].xml",
                    rule_id="docx-missing-content-types",
                )
            )

        return errors

    def _validate_word_compatibility(
        self, structure: DOCXStructure
    ) -> List[ValidationError]:
        """Validate Word compatibility across versions."""
        errors = []

        if structure.document_xml is None:
            return errors

        doc_root = structure.document_xml.getroot()

        # Check for Word version compatibility
        compatibility = doc_root.find(".//w:compat", self.namespaces)
        if compatibility is not None:
            compatibility_setting = compatibility.find(
                ".//w:compatSetting", self.namespaces
            )
            if compatibility_setting is not None:
                name = compatibility_setting.get(
                    "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}name"
                )
                val = compatibility_setting.get(
                    "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}val"
                )
                if name == "compatibilityMode" and val:
                    try:
                        version_num = int(val)
                        if version_num < 12:  # Before Word 2007
                            errors.append(
                                ValidationError(
                                    severity="warning",
                                    message=f"Document uses old compatibility mode: {val}",
                                    file_path="word/document.xml",
                                    rule_id="docx-old-compatibility-mode",
                                )
                            )
                    except (ValueError, TypeError):
                        pass

        return errors

    def _validate_content(self, structure: DOCXStructure) -> List[ValidationError]:
        """Validate document content."""
        errors = []

        if structure.document_xml is None:
            return errors

        doc_root = structure.document_xml.getroot()

        # Check document has content
        paragraphs = doc_root.findall(".//w:p", self.namespaces)
        if not paragraphs:
            errors.append(
                ValidationError(
                    severity="error",
                    message="Document contains no paragraphs",
                    file_path="word/document.xml",
                    rule_id="docx-no-content",
                )
            )

        # Check content length
        text_content = []
        for p in paragraphs:
            text_elems = p.findall(".//w:t", self.namespaces)
            for text_elem in text_elems:
                if text_elem.text:
                    text_content.append(text_elem.text)

        total_text = "".join(text_content)
        if len(total_text) < self.word_requirements["min_content_length"]:
            errors.append(
                ValidationError(
                    severity="warning",
                    message=f"Document content very short: {len(total_text)} characters",
                    file_path="word/document.xml",
                    rule_id="docx-short-content",
                )
            )

        return errors

    def _validate_styles(self, structure: DOCXStructure) -> List[ValidationError]:
        """Validate document styles."""
        errors = []

        if structure.styles_xml is None:
            errors.append(
                ValidationError(
                    severity="warning",
                    message="Missing styles.xml - using default styles",
                    file_path="word/styles.xml",
                    rule_id="docx-missing-styles",
                )
            )
            return errors

        styles_root = structure.styles_xml.getroot()
        if styles_root is None:
            return errors

        # Check for recommended styles
        defined_styles = {}
        for style in styles_root.findall(".//w:style", self.namespaces):
            style_id = style.get(
                "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}styleId"
            )
            style_name_elem = style.find(".//w:name", self.namespaces)
            style_name = (
                style_name_elem.get(
                    "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}val"
                )
                if style_name_elem is not None
                else style_id
            )
            if style_id:
                defined_styles[style_id] = style_name

        missing_styles = []
        for recommended_style in self.word_requirements["recommended_styles"]:
            if recommended_style not in defined_styles.values():
                missing_styles.append(recommended_style)

        if missing_styles:
            errors.append(
                ValidationError(
                    severity="info",
                    message=f"Recommended styles missing: {', '.join(missing_styles)}",
                    file_path="word/styles.xml",
                    rule_id="docx-missing-recommended-styles",
                )
            )

        return errors

    def _validate_metadata(self, structure: DOCXStructure) -> List[ValidationError]:
        """Validate document metadata."""
        errors = []

        # Check core properties
        for required_prop in self.word_requirements["required_properties"]:
            if (
                required_prop not in structure.core_properties
                or not structure.core_properties[required_prop]
            ):
                errors.append(
                    ValidationError(
                        severity="error",
                        message=f"Missing required property: {required_prop}",
                        file_path="docProps/core.xml",
                        rule_id="docx-missing-required-property",
                    )
                )

        # Check for title
        if "title" not in structure.core_properties:
            errors.append(
                ValidationError(
                    severity="warning",
                    message="Missing document title",
                    file_path="docProps/core.xml",
                    rule_id="docx-missing-title",
                )
            )

        # Check for author
        if "creator" not in structure.core_properties:
            errors.append(
                ValidationError(
                    severity="warning",
                    message="Missing document author",
                    file_path="docProps/core.xml",
                    rule_id="docx-missing-author",
                )
            )

        return errors

    def _validate_accessibility(
        self, structure: DOCXStructure
    ) -> List[ValidationError]:
        """Validate document accessibility."""
        errors = []

        if structure.document_xml is None:
            return errors

        doc_root = structure.document_xml.getroot()

        # Check for alt text on images
        drawings = doc_root.findall(".//w:drawing", self.namespaces)
        images_without_alt = 0
        for drawing in drawings:
            alt_text = drawing.find(".//w:altText", self.namespaces)
            if alt_text is None or not alt_text.get(
                "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}val"
            ):
                images_without_alt += 1

        if images_without_alt > 0:
            errors.append(
                ValidationError(
                    severity="warning",
                    message=f"Images without alternative text: {images_without_alt}",
                    file_path="word/document.xml",
                    rule_id="docx-images-no-alt-text",
                )
            )

        return errors

    def _validate_performance(
        self, file_path: Path, structure: DOCXStructure
    ) -> List[ValidationError]:
        """Validate document performance characteristics."""
        errors = []

        # Check file size
        file_size = file_path.stat().st_size
        if file_size > self.word_requirements["max_file_size"]:
            errors.append(
                ValidationError(
                    severity="warning",
                    message=f"Large file size: {file_size / (1024 * 1024):.1f}MB - consider optimization",
                    file_path=str(file_path),
                    rule_id="docx-large-file",
                )
            )

        # Check media files
        if structure.media_files:
            if len(structure.media_files) > 50:
                errors.append(
                    ValidationError(
                        severity="info",
                        message=f"Many media files: {len(structure.media_files)} - may affect performance",
                        file_path="word/media/",
                        rule_id="docx-many-media-files",
                    )
                )

        # Check font usage
        if len(structure.font_table) > 20:
            errors.append(
                ValidationError(
                    severity="info",
                    message=f"Many custom fonts: {len(structure.font_table)} - may affect portability",
                    file_path="word/fontTable.xml",
                    rule_id="docx-many-fonts",
                )
            )

        return errors

    def get_validation_summary(self, result: ValidationResult) -> str:
        """Get a human-readable summary of validation results."""
        if result.is_valid:
            return f"✅ DOCX validation passed - {result.stats.get('file_size', 0) / 1024:.1f}KB"

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
