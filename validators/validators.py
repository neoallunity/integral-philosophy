#!/usr/bin/env python3
"""
Validation module for Integral Philosophy publishing system.
Implements comprehensive validation of all output formats.
"""

import re
import json
import subprocess
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
from abc import ABC, abstractmethod
import logging

logger = logging.getLogger(__name__)


@dataclass
class ValidationError:
    """Represents a validation error."""

    severity: str  # 'error', 'warning', 'info'
    message: str
    file_path: str
    line: Optional[int] = None
    column: Optional[int] = None
    rule_id: Optional[str] = None


@dataclass
class ValidationResult:
    """Result of validation."""

    is_valid: bool
    errors: List[ValidationError]
    stats: Dict[str, Any]

    def get_errors_by_severity(self, severity: str) -> List[ValidationError]:
        """Get errors filtered by severity."""
        return [e for e in self.errors if e.severity == severity]

    @property
    def error_count(self) -> int:
        return len(self.get_errors_by_severity("error"))

    @property
    def warning_count(self) -> int:
        return len(self.get_errors_by_severity("warning"))

    @property
    def info_count(self) -> int:
        return len(self.get_errors_by_severity("info"))


class BaseValidator(ABC):
    """Abstract base class for validators."""

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        self.config = config or {}

    @abstractmethod
    def validate(self, file_path: Path) -> ValidationResult:
        """Validate a file."""
        pass

    def _create_result(
        self,
        is_valid: bool,
        errors: List[ValidationError],
        stats: Optional[Dict[str, Any]] = None,
    ) -> ValidationResult:
        """Create validation result."""
        return ValidationResult(is_valid=is_valid, errors=errors, stats=stats or {})


class HTML5Validator(BaseValidator):
    """HTML5 validation using W3C validator and custom checks."""

    def validate(self, file_path: Path) -> ValidationResult:
        """Validate HTML5 file."""
        errors = []
        stats = {"file_size": file_path.stat().st_size}

        try:
            content = file_path.read_text(encoding="utf-8")
            stats["lines"] = len(content.splitlines())

            # Basic HTML5 structure checks
            doctype_match = re.search(r"<!DOCTYPE\s+html>", content, re.IGNORECASE)
            if not doctype_match:
                errors.append(
                    ValidationError(
                        severity="error",
                        message="Missing or invalid HTML5 DOCTYPE",
                        file_path=str(file_path),
                        line=1,
                        rule_id="html5-doctype",
                    )
                )

            # Check for proper HTML5 semantic structure
            html_tag_match = re.search(r"<html[^>]*lang=", content, re.IGNORECASE)
            if not html_tag_match:
                errors.append(
                    ValidationError(
                        severity="warning",
                        message="HTML tag missing lang attribute",
                        file_path=str(file_path),
                        line=1,
                        rule_id="html5-lang",
                    )
                )

            # Check for viewport meta tag (responsive design)
            viewport_match = re.search(
                r'<meta[^>]*name=["\']viewport["\']', content, re.IGNORECASE
            )
            if not viewport_match:
                errors.append(
                    ValidationError(
                        severity="warning",
                        message="Missing viewport meta tag for responsive design",
                        file_path=str(file_path),
                        rule_id="html5-viewport",
                    )
                )

            # Check for inline styles (should be avoided)
            style_matches = re.findall(r'style=["\'][^"\']*["\']', content)
            if style_matches:
                errors.append(
                    ValidationError(
                        severity="warning",
                        message=f"Found {len(style_matches)} inline styles (should use external CSS)",
                        file_path=str(file_path),
                        rule_id="html5-no-inline-styles",
                    )
                )

            # Check accessibility basics
            if not re.search(r"<main[^>]*>", content):
                errors.append(
                    ValidationError(
                        severity="warning",
                        message="Missing <main> element for accessibility",
                        file_path=str(file_path),
                        rule_id="html5-accessibility-main",
                    )
                )

            # Check alt attributes for images
            img_matches = re.finditer(r"<img[^>]*>", content, re.IGNORECASE)
            missing_alt = 0
            for match in img_matches:
                if not re.search(r'alt=["\'][^"\']*["\']', match.group()):
                    missing_alt += 1

            if missing_alt > 0:
                errors.append(
                    ValidationError(
                        severity="error",
                        message=f"{missing_alt} image(s) missing alt attribute",
                        file_path=str(file_path),
                        rule_id="html5-img-alt",
                    )
                )

            # Try to use vnu.jar if available for W3C validation
            if self._can_run_vnu():
                vnu_errors = self._run_vnu_validation(file_path)
                errors.extend(vnu_errors)

            stats["checks_performed"] = 7

        except Exception as e:
            errors.append(
                ValidationError(
                    severity="error",
                    message=f"Validation failed: {str(e)}",
                    file_path=str(file_path),
                    rule_id="html5-validation-error",
                )
            )

        return self._create_result(
            is_valid=len([e for e in errors if e.severity == "error"]) == 0,
            errors=errors,
            stats=stats,
        )

    def _can_run_vnu(self) -> bool:
        """Check if W3C validator (vnu.jar) is available."""
        try:
            result = subprocess.run(
                ["java", "-version"], capture_output=True, timeout=5
            )
            return result.returncode == 0
        except:
            return False

    def _run_vnu_validation(self, file_path: Path) -> List[ValidationError]:
        """Run W3C Nu Validator if available."""
        errors = []
        try:
            # Try to find vnu.jar in common locations
            vnu_paths = [
                "/usr/share/java/vnu.jar",
                "/usr/local/bin/vnu.jar",
                "./vnu.jar",
            ]

            vnu_jar = None
            for path in vnu_paths:
                if Path(path).exists():
                    vnu_jar = path
                    break

            if vnu_jar:
                result = subprocess.run(
                    ["java", "-jar", vnu_jar, "--format", "json", str(file_path)],
                    capture_output=True,
                    text=True,
                    timeout=30,
                )

                if result.stdout:
                    vnu_data = json.loads(result.stdout)
                    for message in vnu_data.get("messages", []):
                        errors.append(
                            ValidationError(
                                severity="error"
                                if message.get("type") == "error"
                                else "warning",
                                message=message.get("message", ""),
                                file_path=str(file_path),
                                line=message.get("lastLine"),
                                column=message.get("firstColumn"),
                                rule_id="w3c-validator",
                            )
                        )

        except Exception as e:
            logger.warning(f"W3C validation failed: {e}")

        return errors


class CSSValidator(BaseValidator):
    """CSS validation using stylelint and custom checks."""

    def validate(self, file_path: Path) -> ValidationResult:
        """Validate CSS file."""
        errors = []
        stats = {"file_size": file_path.stat().st_size}

        try:
            content = file_path.read_text(encoding="utf-8")
            stats["lines"] = len(content.splitlines())
            stats["rules"] = len(re.findall(r"\{", content))

            # Check for CSS syntax basics
            if not content.strip().endswith("}"):
                errors.append(
                    ValidationError(
                        severity="error",
                        message="CSS file should end with closing brace",
                        file_path=str(file_path),
                        line=stats["lines"],
                        rule_id="css-syntax-ending",
                    )
                )

            # Check for empty rules
            empty_rules = re.findall(r"[^{}]+\{\s*\}", content)
            if empty_rules:
                errors.append(
                    ValidationError(
                        severity="warning",
                        message=f"Found {len(empty_rules)} empty CSS rules",
                        file_path=str(file_path),
                        rule_id="css-empty-rules",
                    )
                )

            # Check for !important usage
            important_count = len(re.findall(r"!\s*important", content))
            if important_count > 0:
                errors.append(
                    ValidationError(
                        severity="warning",
                        message=f"Found {important_count} uses of !important",
                        file_path=str(file_path),
                        rule_id="css-important",
                    )
                )

            # Check for universal selector
            if "*" in content and not content.strip().startswith("*"):
                errors.append(
                    ValidationError(
                        severity="info",
                        message="Universal selector (*) detected - consider specificity",
                        file_path=str(file_path),
                        rule_id="css-universal",
                    )
                )

            # Try stylelint if available
            if self._can_run_stylelint():
                stylelint_errors = self._run_stylelint(file_path)
                errors.extend(stylelint_errors)

            stats["checks_performed"] = 5

        except Exception as e:
            errors.append(
                ValidationError(
                    severity="error",
                    message=f"CSS validation failed: {str(e)}",
                    file_path=str(file_path),
                    rule_id="css-validation-error",
                )
            )

        return self._create_result(
            is_valid=len([e for e in errors if e.severity == "error"]) == 0,
            errors=errors,
            stats=stats,
        )

    def _can_run_stylelint(self) -> bool:
        """Check if stylelint is available."""
        try:
            result = subprocess.run(
                ["npx", "stylelint", "--version"], capture_output=True, timeout=5
            )
            return result.returncode == 0
        except:
            return False

    def _run_stylelint(self, file_path: Path) -> List[ValidationError]:
        """Run stylelint validation if available."""
        errors = []
        try:
            result = subprocess.run(
                ["npx", "stylelint", "--format", "json", str(file_path)],
                capture_output=True,
                text=True,
                timeout=30,
            )

            if result.stdout:
                stylelint_data = json.loads(result.stdout)
                for file_result in stylelint_data:
                    for warning in file_result.get("warnings", []):
                        errors.append(
                            ValidationError(
                                severity="warning"
                                if warning.get("severity") == "warning"
                                else "error",
                                message=warning.get("text", ""),
                                file_path=str(file_path),
                                line=warning.get("line"),
                                column=warning.get("column"),
                                rule_id=warning.get("rule", "stylelint"),
                            )
                        )

        except Exception as e:
            logger.warning(f"Stylelint validation failed: {e}")

        return errors


class JavaScriptValidator(BaseValidator):
    """JavaScript validation using ESLint and custom checks."""

    def validate(self, file_path: Path) -> ValidationResult:
        """Validate JavaScript file."""
        errors = []
        stats = {"file_size": file_path.stat().st_size}

        try:
            content = file_path.read_text(encoding="utf-8")
            stats["lines"] = len(content.splitlines())
            stats["functions"] = len(re.findall(r"function\s+\w+", content))

            # Check for use strict
            if not re.search(r'"use strict"|\'use strict\'', content):
                errors.append(
                    ValidationError(
                        severity="warning",
                        message='Missing "use strict" directive',
                        file_path=str(file_path),
                        line=1,
                        rule_id="js-use-strict",
                    )
                )

            # Check for console.log statements
            console_logs = len(re.findall(r"console\.log", content))
            if console_logs > 0:
                errors.append(
                    ValidationError(
                        severity="warning",
                        message=f"Found {console_logs} console.log statements",
                        file_path=str(file_path),
                        rule_id="js-console-log",
                    )
                )

            # Check for var usage (should use const/let)
            var_count = len(re.findall(r"\bvar\s+\w+", content))
            if var_count > 0:
                errors.append(
                    ValidationError(
                        severity="warning",
                        message=f"Found {var_count} var declarations (prefer const/let)",
                        file_path=str(file_path),
                        rule_id="js-var-usage",
                    )
                )

            # Check for eval usage
            if re.search(r"\beval\s*\(", content):
                errors.append(
                    ValidationError(
                        severity="error",
                        message="Found eval() usage (security risk)",
                        file_path=str(file_path),
                        rule_id="js-eval",
                    )
                )

            # Try ESLint if available
            if self._can_run_eslint():
                eslint_errors = self._run_eslint(file_path)
                errors.extend(eslint_errors)

            stats["checks_performed"] = 5

        except Exception as e:
            errors.append(
                ValidationError(
                    severity="error",
                    message=f"JavaScript validation failed: {str(e)}",
                    file_path=str(file_path),
                    rule_id="js-validation-error",
                )
            )

        return self._create_result(
            is_valid=len([e for e in errors if e.severity == "error"]) == 0,
            errors=errors,
            stats=stats,
        )

    def _can_run_eslint(self) -> bool:
        """Check if ESLint is available."""
        try:
            result = subprocess.run(
                ["npx", "eslint", "--version"], capture_output=True, timeout=5
            )
            return result.returncode == 0
        except:
            return False

    def _run_eslint(self, file_path: Path) -> List[ValidationError]:
        """Run ESLint validation if available."""
        errors = []
        try:
            result = subprocess.run(
                ["npx", "eslint", "--format", "json", str(file_path)],
                capture_output=True,
                text=True,
                timeout=30,
            )

            if result.stdout:
                eslint_data = json.loads(result.stdout)
                for file_result in eslint_data:
                    for message in file_result.get("messages", []):
                        errors.append(
                            ValidationError(
                                severity="warning"
                                if message.get("severity") == 1
                                else "error",
                                message=message.get("message", ""),
                                file_path=str(file_path),
                                line=message.get("line"),
                                column=message.get("column"),
                                rule_id=message.get("ruleId", "eslint"),
                            )
                        )

        except Exception as e:
            logger.warning(f"ESLint validation failed: {e}")

        return errors


class LaTeXValidator(BaseValidator):
    """LaTeX validation using lacheck and custom checks."""

    def validate(self, file_path: Path) -> ValidationResult:
        """Validate LaTeX file."""
        errors = []
        stats = {"file_size": file_path.stat().st_size}

        try:
            content = file_path.read_text(encoding="utf-8")
            stats["lines"] = len(content.splitlines())
            stats["environments"] = len(re.findall(r"\\begin\{[^}]+\}", content))

            # Check for document class
            if not re.search(r"\\documentclass", content):
                errors.append(
                    ValidationError(
                        severity="error",
                        message="Missing \\documentclass declaration",
                        file_path=str(file_path),
                        line=1,
                        rule_id="latex-documentclass",
                    )
                )

            # Check for matching begin/end pairs
            begin_matches = re.findall(r"\\begin\{([^}]+)\}", content)
            end_matches = re.findall(r"\\end\{([^}]+)\}", content)

            # Count occurrences of each environment
            begin_counts = {}
            end_counts = {}

            for env in begin_matches:
                begin_counts[env] = begin_counts.get(env, 0) + 1

            for env in end_matches:
                end_counts[env] = end_counts.get(env, 0) + 1

            # Check for unmatched environments
            unmatched = []
            for env in begin_counts:
                if begin_counts[env] > end_counts.get(env, 0):
                    unmatched.extend(
                        [env] * (begin_counts[env] - end_counts.get(env, 0))
                    )

            for env in end_counts:
                if end_counts[env] > begin_counts.get(env, 0):
                    unmatched.extend(
                        [f"extra end{{{env}}}"]
                        * (end_counts[env] - begin_counts.get(env, 0))
                    )

            if unmatched:
                errors.append(
                    ValidationError(
                        severity="error",
                        message=f"Unmatched environments: {', '.join(unmatched)}",
                        file_path=str(file_path),
                        rule_id="latex-unmatched-environments",
                    )
                )

            # Check for bracket balance
            open_braces = content.count("{")
            close_braces = content.count("}")
            if open_braces != close_braces:
                errors.append(
                    ValidationError(
                        severity="error",
                        message=f"Unbalanced braces: {open_braces} open, {close_braces} close",
                        file_path=str(file_path),
                        rule_id="latex-unbalanced-braces",
                    )
                )

            # Check for proper inputenc
            if (
                "\\usepackage[utf8]{inputenc}" not in content
                and "\\usepackage{fontspec}" not in content
            ):
                errors.append(
                    ValidationError(
                        severity="warning",
                        message="Missing input encoding package (inputenc or fontspec)",
                        file_path=str(file_path),
                        rule_id="latex-encoding",
                    )
                )

            # Try lacheck if available
            if self._can_run_lacheck():
                lacheck_errors = self._run_lacheck(file_path)
                errors.extend(lacheck_errors)

            stats["checks_performed"] = 5

        except Exception as e:
            errors.append(
                ValidationError(
                    severity="error",
                    message=f"LaTeX validation failed: {str(e)}",
                    file_path=str(file_path),
                    rule_id="latex-validation-error",
                )
            )

        return self._create_result(
            is_valid=len([e for e in errors if e.severity == "error"]) == 0,
            errors=errors,
            stats=stats,
        )

    def _can_run_lacheck(self) -> bool:
        """Check if lacheck is available."""
        try:
            result = subprocess.run(
                ["lacheck", "--version"], capture_output=True, timeout=5
            )
            return result.returncode == 0
        except:
            return False

    def _run_lacheck(self, file_path: Path) -> List[ValidationError]:
        """Run lacheck validation if available."""
        errors = []
        try:
            result = subprocess.run(
                ["lacheck", str(file_path)], capture_output=True, text=True, timeout=30
            )

            if result.stderr:
                for line in result.stderr.splitlines():
                    if line.strip():
                        # Parse lacheck output format
                        match = re.match(r'"([^"]+)", line (\d+):\s*(.*)', line)
                        if match:
                            errors.append(
                                ValidationError(
                                    severity="warning",
                                    message=match.group(3),
                                    file_path=match.group(1),
                                    line=int(match.group(2)),
                                    rule_id="lacheck",
                                )
                            )

        except Exception as e:
            logger.warning(f"LaTeX check failed: {e}")

        return errors
