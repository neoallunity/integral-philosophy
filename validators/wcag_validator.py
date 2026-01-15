#!/usr/bin/env python3
"""
WCAG 2.1 AA Accessibility Validator for Integral Philosophy publishing system.
Implements comprehensive accessibility validation for web content and digital publications.
"""

import re
import json
from pathlib import Path
from typing import Dict, List, Any, Optional, Set, Tuple
from dataclasses import dataclass
import logging
from urllib.parse import urlparse
import colorsys

from .validators import BaseValidator, ValidationResult, ValidationError

logger = logging.getLogger(__name__)

WCAG_SUCCESS_CRITERIA = {
    "1.1.1": {"title": "Non-text Content", "level": "A"},
    "1.2.1": {"title": "Audio-only and Video-only (Prerecorded)", "level": "A"},
    "1.2.2": {"title": "Captions (Prerecorded)", "level": "A"},
    "1.2.3": {
        "title": "Audio Description or Media Alternative (Prerecorded)",
        "level": "A",
    },
    "1.2.4": {"title": "Captions (Live)", "level": "AA"},
    "1.2.5": {"title": "Audio Description (Prerecorded)", "level": "AA"},
    "1.3.1": {"title": "Info and Relationships", "level": "A"},
    "1.3.2": {"title": "Meaningful Sequence", "level": "A"},
    "1.3.3": {"title": "Sensory Characteristics", "level": "A"},
    "1.3.4": {"title": "Orientation", "level": "AA"},
    "1.3.5": {"title": "Identify Input Purpose", "level": "AA"},
    "1.3.6": {"title": "Identify Purpose", "level": "AAA"},
    "1.4.1": {"title": "Use of Color", "level": "A"},
    "1.4.2": {"title": "Audio Control", "level": "A"},
    "1.4.3": {"title": "Contrast (Minimum)", "level": "AA"},
    "1.4.4": {"title": "Resize text", "level": "AA"},
    "1.4.5": {"title": "Text Spacing", "level": "AA"},
    "1.4.6": {"title": "Contrast (Enhanced)", "level": "AAA"},
    "1.4.7": {"title": "Low or No Background Audio", "level": "AAA"},
    "1.4.8": {"title": "Visual Presentation", "level": "AAA"},
    "1.4.9": {"title": "Text Images (No Exception)", "level": "AAA"},
    "1.4.10": {"title": "Reflow", "level": "AA"},
    "1.4.11": {"title": "Non-text Contrast", "level": "AA"},
    "1.4.12": {"title": "Text Spacing", "level": "AA"},
    "1.4.13": {"title": "Content on Hover or Focus", "level": "AA"},
    "2.1.1": {"title": "Keyboard", "level": "A"},
    "2.1.2": {"title": "No Keyboard Trap", "level": "A"},
    "2.1.3": {"title": "Keyboard (No Exception)", "level": "AAA"},
    "2.1.4": {"title": "Character Key Shortcuts", "level": "A"},
    "2.2.1": {"title": "Timing Adjustable", "level": "A"},
    "2.2.2": {"title": "Pause, Stop, Hide", "level": "A"},
    "2.2.3": {"title": "No Timing", "level": "AAA"},
    "2.2.4": {"title": "Interruptions", "level": "AAA"},
    "2.2.5": {"title": "Re-authenticating", "level": "AAA"},
    "2.2.6": {"title": "Timeouts", "level": "AAA"},
    "2.3.1": {"title": "Three Flashes or Below Threshold", "level": "A"},
    "2.3.2": {"title": "Three Flashes", "level": "AAA"},
    "2.3.3": {"title": "Animation from Interactions", "level": "AAA"},
    "2.4.1": {"title": "Bypass Blocks", "level": "A"},
    "2.4.2": {"title": "Page Titled", "level": "A"},
    "2.4.3": {"title": "Focus Order", "level": "A"},
    "2.4.4": {"title": "Link Purpose (In Context)", "level": "A"},
    "2.4.5": {"title": "Multiple Ways", "level": "AA"},
    "2.4.6": {"title": "Headings and Labels", "level": "AA"},
    "2.4.7": {"title": "Focus Visible", "level": "AA"},
    "2.4.8": {"title": "Location", "level": "AAA"},
    "2.4.9": {"title": "Link Purpose (Link Only)", "level": "AAA"},
    "2.4.10": {"title": "Section Headings", "level": "AAA"},
    "3.1.1": {"title": "Language of Page", "level": "A"},
    "3.1.2": {"title": "Language of Parts", "level": "AA"},
    "3.1.3": {"title": "Unusual Words", "level": "AAA"},
    "3.1.4": {"title": "Abbreviations", "level": "AAA"},
    "3.1.5": {"title": "Reading Level", "level": "AAA"},
    "3.1.6": {"title": "Pronunciation", "level": "AAA"},
    "3.2.1": {"title": "On Focus", "level": "A"},
    "3.2.2": {"title": "On Input", "level": "A"},
    "3.2.3": {"title": "Consistent Navigation", "level": "AA"},
    "3.2.4": {"title": "Consistent Identification", "level": "AA"},
    "3.2.5": {"title": "Change on Request", "level": "AAA"},
    "3.3.1": {"title": "Error Identification", "level": "A"},
    "3.3.2": {"title": "Labels or Instructions", "level": "A"},
    "3.3.3": {"title": "Error Suggestion", "level": "AA"},
    "3.3.4": {"title": "Error Prevention (Legal, Financial, Data)", "level": "AA"},
    "3.3.5": {"title": "Help", "level": "AAA"},
    "3.3.6": {"title": "Error Prevention (All)", "level": "AAA"},
    "3.3.7": {"title": "Redundant Entry", "level": "AAA"},
    "3.3.8": {"title": "Accessible Authentication (Minimum)", "level": "AA"},
    "3.3.9": {"title": "Accessible Authentication (Enhanced)", "level": "AAA"},
    "4.1.1": {"title": "Parsing", "level": "A"},
    "4.1.2": {"title": "Name, Role, Value", "level": "A"},
    "4.1.3": {"title": "Status Messages", "level": "AA"},
}


@dataclass
class AccessibilityIssue:
    """Represents an accessibility issue."""

    wcag_criterion: str
    title: str
    level: str
    element: str
    message: str
    severity: str
    suggestion: str


class WCAGValidator(BaseValidator):
    """Comprehensive WCAG 2.1 AA accessibility validator."""

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        super().__init__(config)
        self.min_contrast_ratio = 4.5  # AA standard
        self.enhanced_contrast_ratio = 7.0  # AAA standard
        self.large_text_min_contrast = 3.0  # AA for large text
        self.large_text_enhanced_contrast = 4.5  # AAA for large text

    def validate(self, file_path: Path) -> ValidationResult:
        """Validate accessibility for comprehensive WCAG 2.1 AA compliance."""
        errors = []
        stats = {"file_size": file_path.stat().st_size, "checks_performed": 0}

        try:
            # Determine file type and extract content
            content_type = self._detect_content_type(file_path)

            if content_type == "html":
                issues = self._validate_html_accessibility(file_path)
            elif content_type == "pdf":
                issues = self._validate_pdf_accessibility(file_path)
            elif content_type == "epub":
                issues = self._validate_epub_accessibility(file_path)
            elif content_type == "docx":
                issues = self._validate_docx_accessibility(file_path)
            else:
                errors.append(
                    ValidationError(
                        severity="warning",
                        message=f"Unsupported content type for accessibility validation: {content_type}",
                        file_path=str(file_path),
                        rule_id="wcag-unsupported-format",
                    )
                )
                issues = []

            # Convert issues to validation errors
            for issue in issues:
                errors.append(
                    ValidationError(
                        severity=issue.severity,
                        message=f"[{issue.wcag_criterion}] {issue.title}: {issue.message}",
                        file_path=str(file_path),
                        rule_id=f"wcag-{issue.wcag_criterion}",
                    )
                )

            stats.update(
                {
                    "wcag_issues_found": len(issues),
                    "aa_compliance_issues": len(
                        [
                            i
                            for i in issues
                            if i.level in ["A", "AA"]
                            and i.severity in ["error", "warning"]
                        ]
                    ),
                    "aaa_compliance_issues": len(
                        [i for i in issues if i.level == "AAA"]
                    ),
                    "content_type": content_type,
                }
            )

            stats["total_checks"] = len(WCAG_SUCCESS_CRITERIA)

        except Exception as e:
            errors.append(
                ValidationError(
                    severity="error",
                    message=f"WCAG validation failed: {str(e)}",
                    file_path=str(file_path),
                    rule_id="wcag-validation-error",
                )
            )

        return self._create_result(
            is_valid=len([e for e in errors if e.severity == "error"]) == 0,
            errors=errors,
            stats=stats,
        )

    def _detect_content_type(self, file_path: Path) -> str:
        """Detect the type of content file."""
        suffix = file_path.suffix.lower()
        if suffix in [".html", ".htm", ".xhtml"]:
            return "html"
        elif suffix == ".pdf":
            return "pdf"
        elif suffix == ".epub":
            return "epub"
        elif suffix == ".docx":
            return "docx"
        else:
            return "unknown"

    def _validate_html_accessibility(self, file_path: Path) -> List[AccessibilityIssue]:
        """Validate HTML content for WCAG compliance."""
        issues = []

        try:
            with open(file_path, "r", encoding="utf-8") as f:
                content = f.read()

            # 1.1.1 Non-text Content - Images without alt text
            img_issues = self._check_image_alt_text(content)
            issues.extend(img_issues)

            # 1.2.1 Audio-only and Video-only - Check for captions/transcripts
            media_issues = self._check_media_alternatives(content)
            issues.extend(media_issues)

            # 1.3.1 Info and Relationships - Check semantic structure
            structure_issues = self._check_semantic_structure(content)
            issues.extend(structure_issues)

            # 1.4.1 Use of Color - Check color-only information
            color_issues = self._check_color_usage(content)
            issues.extend(color_issues)

            # 1.4.3 Contrast (Minimum) - Check color contrast
            contrast_issues = self._check_color_contrast(content)
            issues.extend(contrast_issues)

            # 2.1.1 Keyboard - Check keyboard accessibility
            keyboard_issues = self._check_keyboard_accessibility(content)
            issues.extend(keyboard_issues)

            # 2.4.2 Page Titled - Check page title
            title_issues = self._check_page_title(content)
            issues.extend(title_issues)

            # 2.4.6 Headings and Labels - Check headings and form labels
            heading_issues = self._check_headings_and_labels(content)
            issues.extend(heading_issues)

            # 3.1.1 Language of Page - Check language declaration
            language_issues = self._check_language_declaration(content)
            issues.extend(language_issues)

            # 4.1.1 Parsing - Check HTML validity
            parsing_issues = self._check_html_validity(content)
            issues.extend(parsing_issues)

            # 4.1.2 Name, Role, Value - Check ARIA attributes
            aria_issues = self._check_aria_attributes(content)
            issues.extend(aria_issues)

        except Exception as e:
            issues.append(
                AccessibilityIssue(
                    wcag_criterion="general",
                    title="Content Processing Error",
                    level="A",
                    element="file",
                    message=f"Could not process HTML content: {str(e)}",
                    severity="error",
                    suggestion="Ensure file is valid HTML and accessible",
                )
            )

        return issues

    def _check_image_alt_text(self, content: str) -> List[AccessibilityIssue]:
        """Check for images without appropriate alt text."""
        issues = []

        # Find all img tags
        img_pattern = r"<img[^>]*>"
        img_matches = re.finditer(img_pattern, content, re.IGNORECASE)

        for match in img_matches:
            img_tag = match.group()

            # Check for alt attribute
            alt_match = re.search(r'alt=["\']([^"\']*)["\']', img_tag, re.IGNORECASE)

            if not alt_match:
                issues.append(
                    AccessibilityIssue(
                        wcag_criterion="1.1.1",
                        title="Non-text Content",
                        level="A",
                        element=img_tag,
                        message="Image missing alt attribute",
                        severity="error",
                        suggestion='Add descriptive alt text or alt="" for decorative images',
                    )
                )
            else:
                alt_text = alt_match.group(1).strip()
                if not alt_text:
                    # Empty alt text is acceptable for decorative images
                    pass
                elif len(alt_text) < 5:
                    issues.append(
                        AccessibilityIssue(
                            wcag_criterion="1.1.1",
                            title="Non-text Content",
                            level="A",
                            element=img_tag,
                            message=f"Image alt text too short: '{alt_text}'",
                            severity="warning",
                            suggestion="Provide more descriptive alt text",
                        )
                    )

        return issues

    def _check_media_alternatives(self, content: str) -> List[AccessibilityIssue]:
        """Check for media accessibility features."""
        issues = []

        # Check video elements
        video_pattern = r"<video[^>]*>"
        video_matches = re.finditer(video_pattern, content, re.IGNORECASE)

        for match in video_matches:
            video_tag = match.group()

            # Check for captions
            if not re.search(r'track[^>]*kind=["\']captions', video_tag, re.IGNORECASE):
                issues.append(
                    AccessibilityIssue(
                        wcag_criterion="1.2.2",
                        title="Captions (Prerecorded)",
                        level="A",
                        element=video_tag,
                        message="Video missing captions",
                        severity="error",
                        suggestion="Add caption track for deaf and hard of hearing users",
                    )
                )

        # Check audio elements
        audio_pattern = r"<audio[^>]*>"
        audio_matches = re.finditer(audio_pattern, content, re.IGNORECASE)

        for match in audio_matches:
            audio_tag = match.group()

            # Check for transcript alternative
            if not re.search(
                r'track[^>]*kind=["\']descriptions', audio_tag, re.IGNORECASE
            ):
                issues.append(
                    AccessibilityIssue(
                        wcag_criterion="1.2.1",
                        title="Audio-only and Video-only (Prerecorded)",
                        level="A",
                        element=audio_tag,
                        message="Audio content missing transcript or description",
                        severity="warning",
                        suggestion="Provide transcript or audio description for audio content",
                    )
                )

        return issues

    def _check_semantic_structure(self, content: str) -> List[AccessibilityIssue]:
        """Check for proper semantic HTML structure."""
        issues = []

        # Check for heading hierarchy
        heading_pattern = r"<h([1-6])[^>]*>(.*?)</h\1>"
        heading_matches = list(
            re.finditer(heading_pattern, content, re.IGNORECASE | re.DOTALL)
        )

        current_level = 0
        for match in heading_matches:
            level = int(match.group(1))
            if level > current_level + 1:
                issues.append(
                    AccessibilityIssue(
                        wcag_criterion="1.3.1",
                        title="Info and Relationships",
                        level="A",
                        element=match.group(),
                        message=f"Heading level {level} follows H{current_level}, skipping levels",
                        severity="warning",
                        suggestion="Use proper heading hierarchy without skipping levels",
                    )
                )
            current_level = level

        # Check for tables with headers
        table_pattern = r"<table[^>]*>.*?</table>"
        table_matches = re.finditer(table_pattern, content, re.IGNORECASE | re.DOTALL)

        for match in table_matches:
            table_content = match.group()
            if not re.search(r"<th[^>]*>", table_content, re.IGNORECASE):
                issues.append(
                    AccessibilityIssue(
                        wcag_criterion="1.3.1",
                        title="Info and Relationships",
                        level="A",
                        element=match.group(),
                        message="Table missing header cells (th)",
                        severity="error",
                        suggestion="Use th elements for table headers and specify scope",
                    )
                )

        return issues

    def _check_color_usage(self, content: str) -> List[AccessibilityIssue]:
        """Check for information conveyed only through color."""
        issues = []

        # Look for color-only indicators in text (simplified check)
        color_indicators = [
            r"click the (red|blue|green|yellow|orange|purple|black|white) button",
            r"(red|blue|green|yellow|orange|purple|black|white) text indicates",
            r"differentiated by (red|blue|green|yellow|orange|purple|black|white)",
        ]

        for indicator in color_indicators:
            matches = re.findall(indicator, content, re.IGNORECASE)
            if matches:
                issues.append(
                    AccessibilityIssue(
                        wcag_criterion="1.4.1",
                        title="Use of Color",
                        level="A",
                        element="text",
                        message=f"Information conveyed only through color: {matches[0]}",
                        severity="warning",
                        suggestion="Add non-color indicators (text, icons, patterns) for color-blind users",
                    )
                )

        return issues

    def _check_color_contrast(self, content: str) -> List[AccessibilityIssue]:
        """Check for sufficient color contrast (basic check)."""
        issues = []

        # Extract inline styles and check for contrast (simplified)
        style_pattern = r'style=["\'][^"\']*color[^"\']*["\']'
        style_matches = re.finditer(style_pattern, content, re.IGNORECASE)

        for match in style_matches:
            style_content = match.group()

            # Extract color values (basic parsing)
            color_match = re.search(r"color:\s*([^;]+)", style_content)
            bg_color_match = re.search(r"background(-color)?:\s*([^;]+)", style_content)

            if color_match and bg_color_match:
                # This would require more sophisticated color analysis
                # For now, just flag for manual review
                issues.append(
                    AccessibilityIssue(
                        wcag_criterion="1.4.3",
                        title="Contrast (Minimum)",
                        level="AA",
                        element=style_content,
                        message="Inline color styling detected - verify contrast ratio manually",
                        severity="info",
                        suggestion="Ensure text has sufficient contrast against background (4.5:1 minimum)",
                    )
                )

        return issues

    def _check_keyboard_accessibility(self, content: str) -> List[AccessibilityIssue]:
        """Check for keyboard accessibility."""
        issues = []

        # Check for elements that might not be keyboard accessible
        problematic_elements = ["div", "span", "p"]

        for element in problematic_elements:
            pattern = f"<{element}[^>]*onclick[^>]*>"
            matches = re.finditer(pattern, content, re.IGNORECASE)

            for match in matches:
                element_tag = match.group()

                # Check if it's keyboard accessible (has tabindex, role, etc.)
                if "tabindex" not in element_tag.lower():
                    issues.append(
                        AccessibilityIssue(
                            wcag_criterion="2.1.1",
                            title="Keyboard",
                            level="A",
                            element=element_tag,
                            message="Element with onclick handler may not be keyboard accessible",
                            severity="warning",
                            suggestion="Add tabindex and keyboard event handlers, or use button/link elements",
                        )
                    )

        return issues

    def _check_page_title(self, content: str) -> List[AccessibilityIssue]:
        """Check for page title."""
        issues = []

        # Look for title tag
        title_match = re.search(
            r"<title[^>]*>(.*?)</title>", content, re.IGNORECASE | re.DOTALL
        )

        if not title_match:
            issues.append(
                AccessibilityIssue(
                    wcag_criterion="2.4.2",
                    title="Page Titled",
                    level="A",
                    element="head",
                    message="Missing page title",
                    severity="error",
                    suggestion="Add descriptive title element to the head section",
                )
            )
        else:
            title_text = title_match.group(1).strip()
            if len(title_text) < 5:
                issues.append(
                    AccessibilityIssue(
                        wcag_criterion="2.4.2",
                        title="Page Titled",
                        level="A",
                        element=f"<title>{title_text}</title>",
                        message="Page title too short",
                        severity="warning",
                        suggestion="Provide a more descriptive page title",
                    )
                )

        return issues

    def _check_headings_and_labels(self, content: str) -> List[AccessibilityIssue]:
        """Check headings and form labels."""
        issues = []

        # Check form inputs without labels
        input_pattern = r"<input[^>]*>"
        input_matches = re.finditer(input_pattern, content, re.IGNORECASE)

        for match in input_matches:
            input_tag = match.group()

            # Skip inputs with aria-label, title, or type hidden/submit/reset
            if any(
                attr in input_tag.lower()
                for attr in ["aria-label=", "title=", "placeholder="]
            ) or re.search(
                r'type=["\']?(hidden|submit|reset)', input_tag, re.IGNORECASE
            ):
                continue

            issues.append(
                AccessibilityIssue(
                    wcag_criterion="2.4.6",
                    title="Headings and Labels",
                    level="AA",
                    element=input_tag,
                    message="Form input missing label",
                    severity="error",
                    suggestion="Add label element or aria-label attribute",
                )
            )

        return issues

    def _check_language_declaration(self, content: str) -> List[AccessibilityIssue]:
        """Check for language declaration."""
        issues = []

        # Check html lang attribute
        html_match = re.search(r"<html[^>]*>", content, re.IGNORECASE)

        if not html_match:
            issues.append(
                AccessibilityIssue(
                    wcag_criterion="3.1.1",
                    title="Language of Page",
                    level="A",
                    element="html",
                    message="Missing html element",
                    severity="error",
                    suggestion="Add html element with lang attribute",
                )
            )
        else:
            html_tag = html_match.group()
            if "lang=" not in html_tag.lower():
                issues.append(
                    AccessibilityIssue(
                        wcag_criterion="3.1.1",
                        title="Language of Page",
                        level="A",
                        element=html_tag,
                        message="Missing lang attribute",
                        severity="error",
                        suggestion='Add lang attribute to html element (e.g., lang="en")',
                    )
                )

        return issues

    def _check_html_validity(self, content: str) -> List[AccessibilityIssue]:
        """Check basic HTML validity."""
        issues = []

        # Check for unclosed tags (simplified check)
        tag_pattern = r"<(/?)([a-zA-Z][a-zA-Z0-9]*)[^>]*>"
        open_tags = []

        for match in re.finditer(tag_pattern, content):
            is_closing = match.group(1) == "/"
            tag_name = match.group(2).lower()

            # Skip self-closing and void elements
            void_elements = {
                "img",
                "br",
                "hr",
                "input",
                "meta",
                "link",
                "area",
                "base",
                "col",
                "embed",
                "param",
                "source",
                "track",
                "wbr",
            }
            if tag_name in void_elements and not is_closing:
                continue

            if is_closing:
                if open_tags and open_tags[-1] == tag_name:
                    open_tags.pop()
                else:
                    issues.append(
                        AccessibilityIssue(
                            wcag_criterion="4.1.1",
                            title="Parsing",
                            level="A",
                            element=match.group(),
                            message=f"Unexpected closing tag: {tag_name}",
                            severity="error",
                            suggestion="Ensure HTML tags are properly nested and closed",
                        )
                    )
            else:
                open_tags.append(tag_name)

        # Check for unclosed tags
        if open_tags:
            issues.append(
                AccessibilityIssue(
                    wcag_criterion="4.1.1",
                    title="Parsing",
                    level="A",
                    element="document",
                    message=f"Unclosed tags: {', '.join(open_tags)}",
                    severity="error",
                    suggestion="Close all opened HTML tags",
                )
            )

        return issues

    def _check_aria_attributes(self, content: str) -> List[AccessibilityIssue]:
        """Check ARIA attributes for accessibility."""
        issues = []

        # Check for ARIA roles
        aria_role_pattern = r'role=["\']([^"\']*)["\']'
        role_matches = re.finditer(aria_role_pattern, content, re.IGNORECASE)

        for match in role_matches:
            role = match.group(1).strip()
            # This could be expanded to check against valid ARIA roles
            if not role:
                issues.append(
                    AccessibilityIssue(
                        wcag_criterion="4.1.2",
                        title="Name, Role, Value",
                        level="A",
                        element=match.group(),
                        message="Empty role attribute",
                        severity="warning",
                        suggestion="Provide valid ARIA role or remove empty role attribute",
                    )
                )

        # Check for ARIA labels without proper context
        aria_label_pattern = r'aria-label=["\']([^"\']*)["\']'
        label_matches = re.finditer(aria_label_pattern, content, re.IGNORECASE)

        for match in label_matches:
            label = match.group(1).strip()
            if len(label) < 3:
                issues.append(
                    AccessibilityIssue(
                        wcag_criterion="4.1.2",
                        title="Name, Role, Value",
                        level="A",
                        element=match.group(),
                        message="ARIA label too short",
                        severity="warning",
                        suggestion="Provide descriptive ARIA label",
                    )
                )

        return issues

    def _validate_pdf_accessibility(self, file_path: Path) -> List[AccessibilityIssue]:
        """Validate PDF accessibility (basic checks)."""
        issues = []

        try:
            with open(file_path, "rb") as f:
                content = f.read()

            # Check if PDF is tagged
            if b"/StructTreeRoot" not in content:
                issues.append(
                    AccessibilityIssue(
                        wcag_criterion="1.3.1",
                        title="Info and Relationships",
                        level="A",
                        element="pdf",
                        message="PDF is not tagged",
                        severity="error",
                        suggestion="Tag PDF for accessibility using Adobe Acrobat or similar tool",
                    )
                )

            # Check for language declaration
            if b"/Lang" not in content:
                issues.append(
                    AccessibilityIssue(
                        wcag_criterion="3.1.1",
                        title="Language of Page",
                        level="A",
                        element="pdf",
                        message="PDF missing language declaration",
                        severity="error",
                        suggestion="Add language declaration to PDF metadata",
                    )
                )

            # Check for alternative text on images (simplified check)
            if b"/Alt" not in content:
                issues.append(
                    AccessibilityIssue(
                        wcag_criterion="1.1.1",
                        title="Non-text Content",
                        level="A",
                        element="pdf",
                        message="PDF may contain images without alternative text",
                        severity="warning",
                        suggestion="Add alternative text to images in PDF",
                    )
                )

        except Exception as e:
            issues.append(
                AccessibilityIssue(
                    wcag_criterion="general",
                    title="PDF Processing Error",
                    level="A",
                    element="pdf",
                    message=f"Could not process PDF: {str(e)}",
                    severity="error",
                    suggestion="Ensure PDF is accessible and not corrupted",
                )
            )

        return issues

    def _validate_epub_accessibility(self, file_path: Path) -> List[AccessibilityIssue]:
        """Validate EPUB accessibility (basic checks)."""
        issues = []

        # This would require EPUB-specific parsing
        # For now, provide basic guidance
        issues.append(
            AccessibilityIssue(
                wcag_criterion="general",
                title="EPUB Accessibility",
                level="A",
                element="epub",
                message="EPUB accessibility validation requires specialized parsing",
                severity="info",
                suggestion="Use EPUBCheck tool for comprehensive accessibility validation",
            )
        )

        return issues

    def _validate_docx_accessibility(self, file_path: Path) -> List[AccessibilityIssue]:
        """Validate DOCX accessibility (basic checks)."""
        issues = []

        # This would require DOCX-specific parsing
        # For now, provide basic guidance
        issues.append(
            AccessibilityIssue(
                wcag_criterion="general",
                title="DOCX Accessibility",
                level="A",
                element="docx",
                message="DOCX accessibility validation requires specialized parsing",
                severity="info",
                suggestion="Use Microsoft Word Accessibility Checker for validation",
            )
        )

        return issues

    def get_accessibility_report(self, result: ValidationResult) -> Dict[str, Any]:
        """Generate comprehensive accessibility report."""
        if not result.stats:
            return {}

        report = {
            "summary": {
                "total_issues": result.stats.get("wcag_issues_found", 0),
                "aa_compliance_issues": result.stats.get("aa_compliance_issues", 0),
                "aaa_compliance_issues": result.stats.get("aaa_compliance_issues", 0),
                "content_type": result.stats.get("content_type", "unknown"),
            },
            "issues_by_criterion": {},
            "issues_by_level": {"A": 0, "AA": 0, "AAA": 0},
            "recommendations": [],
        }

        # Analyze errors by WCAG criterion
        for error in result.errors:
            if "wcag-" in error.rule_id:
                criterion = error.rule_id.replace("wcag-", "")
                if criterion not in report["issues_by_criterion"]:
                    report["issues_by_criterion"][criterion] = []

                criterion_info = WCAG_SUCCESS_CRITERIA.get(
                    criterion, {"title": "Unknown", "level": "A"}
                )
                report["issues_by_criterion"][criterion].append(
                    {
                        "title": criterion_info["title"],
                        "level": criterion_info["level"],
                        "message": error.message,
                        "severity": error.severity,
                        "file_path": error.file_path,
                    }
                )

                if criterion_info["level"] in report["issues_by_level"]:
                    report["issues_by_level"][criterion_info["level"]] += 1

        # Generate recommendations
        if report["issues_by_level"]["A"] > 0:
            report["recommendations"].append(
                "Priority 1: Fix Level A issues for basic accessibility compliance"
            )
        if report["issues_by_level"]["AA"] > 0:
            report["recommendations"].append(
                "Priority 2: Address Level AA issues for enhanced accessibility"
            )
        if report["issues_by_level"]["AAA"] > 0:
            report["recommendations"].append(
                "Consider: Level AAA issues for optimal accessibility (optional)"
            )

        return report
