#!/usr/bin/env python3
"""
Security Vulnerability Scanner for Integral Philosophy publishing system.
Implements comprehensive security scanning for web outputs and publications.
"""

import re
import json
import hashlib
import base64
from pathlib import Path
from typing import Dict, List, Any, Optional, Set, Tuple
from dataclasses import dataclass
import logging
from urllib.parse import urlparse, unquote
import binascii

from .validators import BaseValidator, ValidationResult, ValidationError

logger = logging.getLogger(__name__)

# Security patterns for vulnerability detection
XSS_PATTERNS = [
    r"<script[^>]*>.*?</script>",
    r"javascript\s*:",
    r'on\w+\s*=\s*["\'][^"\']*["\']',
    r"<iframe[^>]*>",
    r"<object[^>]*>",
    r"<embed[^>]*>",
    r"eval\s*\(",
    r"document\.write\s*\(",
    r"innerHTML\s*=",
    r"outerHTML\s*=",
]

SQL_INJECTION_PATTERNS = [
    r"(['\"]*;?\s*(drop|delete|insert|update|create|alter|exec|execute)\s+",
    r"(union\s+select)",
    r"(select\s+.+\s+from\s+.+)",
    r"(or\s+1\s*=\s*1)",
    r"(and\s+1\s*=\s*1)",
    r"(['\"]\s*(or|and)\s*['\"]?\s*['\"]?\s*=\s*['\"]?['\"]?)",
]

PATH_TRAVERSAL_PATTERNS = [
    r"\.\.[/\\]",
    r"%2e%2e%2f",
    r"%2e%2e\\",
    r"\.\.%2f",
    r"\.\.\\",
    r"[/\\]*etc[/\\]*passwd",
    r"[/\\]*windows[/\\]*system32",
]

COMMAND_INJECTION_PATTERNS = [
    r"[;&|`$()]",
    r"(?i)(whoami|id|uname|hostname|pwd|ls|dir|cat|type)",
    r"(?i)(wget|curl|nc|netcat|telnet)",
    r"(?i)(python|perl|ruby|bash|sh|cmd)",
]

CSRF_PATTERNS = [
    r'<form[^>]*method=["\']post["\'][^>]*>',
    r'<input[^>]*type=["\']hidden["\'][^>]*>',
    r'<input[^>]*name=["\'](?:csrf|authenticity|_token)["\']',
    r'meta\s+name=["\']csrf-token["\']',
]

SENSITIVE_DATA_PATTERNS = [
    r'(?i)(password|passwd|pwd)\s*[:=]\s*["\']?([^"\'\s]{4,})',
    r'(?i)(api[_-]?key|apikey)\s*[:=]\s*["\']?([^"\'\s]{16,})',
    r'(?i)(secret|token)\s*[:=]\s*["\']?([^"\'\s]{16,})',
    r'(?i)(database[_-]?url|db[_-]?connection)\s*[:=]\s*["\']?([^"\'\s]{8,})',
    r"-----BEGIN[A-Z\s]+KEY-----",
    r"-----BEGIN[A-Z\s]+CERTIFICATE-----",
]


@dataclass
class SecurityVulnerability:
    """Represents a security vulnerability."""

    vulnerability_type: str
    severity: str
    confidence: str
    description: str
    location: str
    pattern_matched: str
    recommendation: str
    cwe_id: Optional[str] = None
    owasp_category: Optional[str] = None


class SecurityScanner(BaseValidator):
    """Comprehensive security vulnerability scanner."""

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        super().__init__(config)
        self.severity_levels = {
            "critical": 4,
            "high": 3,
            "medium": 2,
            "low": 1,
            "info": 0,
        }

        self.security_rules = {
            "xss": {
                "patterns": XSS_PATTERNS,
                "cwe": "CWE-79",
                "owasp": "A03:2021 – Injection",
                "severity": "high",
            },
            "sql_injection": {
                "patterns": SQL_INJECTION_PATTERNS,
                "cwe": "CWE-89",
                "owasp": "A03:2021 – Injection",
                "severity": "critical",
            },
            "path_traversal": {
                "patterns": PATH_TRAVERSAL_PATTERNS,
                "cwe": "CWE-22",
                "owasp": "A01:2021 – Broken Access Control",
                "severity": "high",
            },
            "command_injection": {
                "patterns": COMMAND_INJECTION_PATTERNS,
                "cwe": "CWE-78",
                "owasp": "A03:2021 – Injection",
                "severity": "critical",
            },
            "csrf": {
                "patterns": CSRF_PATTERNS,
                "cwe": "CWE-352",
                "owasp": "A01:2021 – Broken Access Control",
                "severity": "medium",
            },
            "sensitive_data": {
                "patterns": SENSITIVE_DATA_PATTERNS,
                "cwe": "CWE-200",
                "owasp": "A02:2021 – Cryptographic Failures",
                "severity": "critical",
            },
        }

    def validate(self, file_path: Path) -> ValidationResult:
        """Scan file for security vulnerabilities."""
        errors = []
        stats = {"file_size": file_path.stat().st_size, "checks_performed": 0}

        try:
            # Read file content
            content = self._read_file_content(file_path)

            # Detect file type
            content_type = self._detect_content_type(file_path)

            vulnerabilities = []

            if content_type in ["html", "javascript", "css"]:
                # Scan web content
                vulnerabilities.extend(self._scan_web_content(content, file_path))
            elif content_type == "json":
                # Scan JSON configuration files
                vulnerabilities.extend(self._scan_json_content(content, file_path))
            elif content_type in ["xml", "svg"]:
                # Scan XML/SVG files
                vulnerabilities.extend(self._scan_xml_content(content, file_path))
            else:
                # Generic scanning
                vulnerabilities.extend(self._scan_generic_content(content, file_path))

            # Convert vulnerabilities to validation errors
            for vuln in vulnerabilities:
                errors.append(
                    ValidationError(
                        severity=self._map_severity(vuln.severity),
                        message=f"[{vuln.vulnerability_type.upper()}] {vuln.description}",
                        file_path=str(file_path),
                        rule_id=f"security-{vuln.vulnerability_type}",
                        line_number=None,
                        column_number=None,
                    )
                )

            # Update statistics
            stats.update(
                {
                    "vulnerabilities_found": len(vulnerabilities),
                    "critical_vulns": len(
                        [v for v in vulnerabilities if v.severity == "critical"]
                    ),
                    "high_vulns": len(
                        [v for v in vulnerabilities if v.severity == "high"]
                    ),
                    "medium_vulns": len(
                        [v for v in vulnerabilities if v.severity == "medium"]
                    ),
                    "low_vulns": len(
                        [v for v in vulnerabilities if v.severity == "low"]
                    ),
                    "content_type": content_type,
                    "security_score": self._calculate_security_score(vulnerabilities),
                }
            )

            stats["total_checks"] = len(self.security_rules)

        except Exception as e:
            errors.append(
                ValidationError(
                    severity="error",
                    message=f"Security scan failed: {str(e)}",
                    file_path=str(file_path),
                    rule_id="security-scan-error",
                )
            )

        return self._create_result(
            is_valid=len([e for e in errors if e.severity == "error"]) == 0,
            errors=errors,
            stats=stats,
        )

    def _read_file_content(self, file_path: Path) -> str:
        """Read file content with appropriate encoding."""
        try:
            # Try UTF-8 first
            with open(file_path, "r", encoding="utf-8") as f:
                return f.read()
        except UnicodeDecodeError:
            try:
                # Try with error handling
                with open(file_path, "r", encoding="utf-8", errors="replace") as f:
                    return f.read()
            except Exception:
                # For binary files, return base64 representation
                with open(file_path, "rb") as f:
                    return base64.b64encode(f.read()).decode("ascii")

    def _detect_content_type(self, file_path: Path) -> str:
        """Detect content type based on file extension and content."""
        suffix = file_path.suffix.lower()

        content_type_map = {
            ".html": "html",
            ".htm": "html",
            ".xhtml": "html",
            ".js": "javascript",
            ".mjs": "javascript",
            ".css": "css",
            ".json": "json",
            ".xml": "xml",
            ".svg": "xml",
            ".pdf": "pdf",
            ".docx": "docx",
            ".epub": "epub",
            ".txt": "text",
            ".md": "markdown",
        }

        return content_type_map.get(suffix, "unknown")

    def _scan_web_content(
        self, content: str, file_path: Path
    ) -> List[SecurityVulnerability]:
        """Scan web content for vulnerabilities."""
        vulnerabilities = []

        # Scan for all vulnerability types
        for vuln_type, config in self.security_rules.items():
            if vuln_type == "sensitive_data":
                continue  # Scan separately to avoid false positives in web content

            patterns = config["patterns"]
            for pattern in patterns:
                matches = re.finditer(
                    pattern, content, re.IGNORECASE | re.MULTILINE | re.DOTALL
                )

                for match in matches:
                    vulnerability = SecurityVulnerability(
                        vulnerability_type=vuln_type,
                        severity=config["severity"],
                        confidence="high",
                        description=f"Potential {vuln_type.replace('_', ' ')} vulnerability detected",
                        location=f"Line {self._get_line_number(content, match.start())}",
                        pattern_matched=match.group(),
                        recommendation=self._get_recommendation(vuln_type),
                        cwe_id=config["cwe"],
                        owasp_category=config["owasp"],
                    )
                    vulnerabilities.append(vulnerability)

        # Additional web-specific checks
        vulnerabilities.extend(self._check_web_specific_issues(content, file_path))

        return vulnerabilities

    def _scan_json_content(
        self, content: str, file_path: Path
    ) -> List[SecurityVulnerability]:
        """Scan JSON content for vulnerabilities."""
        vulnerabilities = []

        try:
            # Parse JSON to ensure it's valid
            json_data = json.loads(content)

            # Look for sensitive data in JSON
            json_str = json.dumps(json_data, ensure_ascii=False)
            for pattern in SENSITIVE_DATA_PATTERNS:
                matches = re.finditer(pattern, json_str, re.IGNORECASE)

                for match in matches:
                    vulnerability = SecurityVulnerability(
                        vulnerability_type="sensitive_data",
                        severity="critical",
                        confidence="high",
                        description="Sensitive data exposure in JSON configuration",
                        location=f"Key: {self._extract_json_key(json_data, match.group())}",
                        pattern_matched=match.group(),
                        recommendation="Remove sensitive data from configuration files or encrypt properly",
                        cwe_id="CWE-200",
                        owasp_category="A02:2021 – Cryptographic Failures",
                    )
                    vulnerabilities.append(vulnerability)

        except json.JSONDecodeError:
            # Invalid JSON - report as issue
            vulnerability = SecurityVulnerability(
                vulnerability_type="invalid_json",
                severity="medium",
                confidence="high",
                description="Invalid JSON syntax detected",
                location="entire file",
                pattern_matched="JSON parsing error",
                recommendation="Fix JSON syntax to prevent parsing errors",
                cwe_id="CWE-787",
                owasp_category="A05:2021 – Security Misconfiguration",
            )
            vulnerabilities.append(vulnerability)

        return vulnerabilities

    def _scan_xml_content(
        self, content: str, file_path: Path
    ) -> List[SecurityVulnerability]:
        """Scan XML/SVG content for vulnerabilities."""
        vulnerabilities = []

        # Check for XXE vulnerabilities
        xxe_patterns = [
            r"<!DOCTYPE[^>]*\[<!ENTITY[^>]*>",
            r"&\w+;",
            r"(?i)system\s*\(",
            r"(?i)file\s*://",
        ]

        for pattern in xxe_patterns:
            matches = re.finditer(pattern, content, re.IGNORECASE)

            for match in matches:
                vulnerability = SecurityVulnerability(
                    vulnerability_type="xxe_injection",
                    severity="critical",
                    confidence="medium",
                    description="Potential XML External Entity (XXE) vulnerability",
                    location=f"Line {self._get_line_number(content, match.start())}",
                    pattern_matched=match.group(),
                    recommendation="Disable external entity processing and use secure XML parsers",
                    cwe_id="CWE-611",
                    owasp_category="A03:2021 – Injection",
                )
                vulnerabilities.append(vulnerability)

        # SVG-specific checks
        if file_path.suffix.lower() == ".svg":
            vulnerabilities.extend(self._scan_svg_security(content, file_path))

        return vulnerabilities

    def _scan_generic_content(
        self, content: str, file_path: Path
    ) -> List[SecurityVulnerability]:
        """Generic content scanning for all file types."""
        vulnerabilities = []

        # Check for sensitive data
        for pattern in SENSITIVE_DATA_PATTERNS:
            matches = re.finditer(pattern, content, re.IGNORECASE | re.MULTILINE)

            for match in matches:
                vulnerability = SecurityVulnerability(
                    vulnerability_type="sensitive_data",
                    severity="critical",
                    confidence="medium",
                    description="Potential sensitive data exposure",
                    location=f"Line {self._get_line_number(content, match.start())}",
                    pattern_matched=match.group()[:50] + "..."
                    if len(match.group()) > 50
                    else match.group(),
                    recommendation="Remove or properly encrypt sensitive data",
                    cwe_id="CWE-200",
                    owasp_category="A02:2021 – Cryptographic Failures",
                )
                vulnerabilities.append(vulnerability)

        return vulnerabilities

    def _check_web_specific_issues(
        self, content: str, file_path: Path
    ) -> List[SecurityVulnerability]:
        """Check for web-specific security issues."""
        vulnerabilities = []

        # Check for mixed content
        mixed_content_pattern = r'https?://[^\s"\'<>]+'
        https_matches = re.findall(r'https://[^\s"\'<>]+', content)
        http_matches = re.findall(r'http://[^\s"\'<>]+', content)

        if https_matches and http_matches:
            vulnerability = SecurityVulnerability(
                vulnerability_type="mixed_content",
                severity="medium",
                confidence="medium",
                description="Mixed HTTP/HTTPS content detected",
                location="multiple locations",
                pattern_matched="http://",
                recommendation="Use HTTPS for all resources to prevent mixed content attacks",
                cwe_id="CWE-311",
                owasp_category="A02:2021 – Cryptographic Failures",
            )
            vulnerabilities.append(vulnerability)

        # Check for insecure headers (would need HTTP context - simplified check)
        insecure_headers = [
            "Content-Security-Policy",
            "X-Content-Type-Options",
            "X-Frame-Options",
            "Strict-Transport-Security",
        ]

        # This would typically check actual HTTP headers
        # For now, check if security headers are mentioned in comments or meta tags
        if not any(header.lower() in content.lower() for header in insecure_headers):
            vulnerability = SecurityVulnerability(
                vulnerability_type="missing_security_headers",
                severity="low",
                confidence="low",
                description="Missing security headers (detected at content level only)",
                location="HTTP headers",
                pattern_matched="Security headers",
                recommendation="Implement security headers like CSP, X-Frame-Options, HSTS",
                cwe_id="CWE-693",
                owasp_category="A05:2021 – Security Misconfiguration",
            )
            vulnerabilities.append(vulnerability)

        return vulnerabilities

    def _scan_svg_security(
        self, content: str, file_path: Path
    ) -> List[SecurityVulnerability]:
        """SVG-specific security scanning."""
        vulnerabilities = []

        # Check for SVG script execution
        svg_script_patterns = [
            r"<script[^>]*>.*?</script>",
            r'on\w+\s*=\s*["\'][^"\']*["\']',
            r"javascript\s*:",
        ]

        for pattern in svg_script_patterns:
            matches = re.finditer(pattern, content, re.IGNORECASE | re.DOTALL)

            for match in matches:
                vulnerability = SecurityVulnerability(
                    vulnerability_type="svg_script_injection",
                    severity="high",
                    confidence="high",
                    description="Script execution in SVG file detected",
                    location=f"Line {self._get_line_number(content, match.start())}",
                    pattern_matched=match.group(),
                    recommendation="Remove scripts from SVG files or sanitize properly",
                    cwe_id="CWE-79",
                    owasp_category="A03:2021 – Injection",
                )
                vulnerabilities.append(vulnerability)

        return vulnerabilities

    def _get_line_number(self, content: str, position: int) -> int:
        """Get line number for a given position in content."""
        return content[:position].count("\n") + 1

    def _extract_json_key(self, json_data: dict, pattern: str) -> str:
        """Extract JSON key where pattern was found."""
        json_str = json.dumps(json_data, ensure_ascii=False)
        # This is a simplified approach
        match_pos = json_str.find(pattern)
        if match_pos == -1:
            return "unknown"

        # Find the closest key before the pattern
        before_pattern = json_str[:match_pos]
        key_matches = re.findall(r'"([^"]+)"\s*:', before_pattern)
        return key_matches[-1] if key_matches else "unknown"

    def _get_recommendation(self, vulnerability_type: str) -> str:
        """Get security recommendation for vulnerability type."""
        recommendations = {
            "xss": "Implement proper input validation and output encoding. Use CSP headers.",
            "sql_injection": "Use parameterized queries/prepared statements. Validate and sanitize input.",
            "path_traversal": "Validate and sanitize file paths. Use allowlists for file access.",
            "command_injection": "Avoid system calls with user input. Use safe APIs.",
            "csrf": "Implement CSRF tokens. Use SameSite cookies.",
            "sensitive_data": "Remove sensitive data from code/config. Use secure storage.",
        }
        return recommendations.get(
            vulnerability_type, "Review and fix security vulnerability"
        )

    def _map_severity(self, security_severity: str) -> str:
        """Map security severity to validation severity."""
        mapping = {
            "critical": "error",
            "high": "error",
            "medium": "warning",
            "low": "info",
        }
        return mapping.get(security_severity, "info")

    def _calculate_security_score(
        self, vulnerabilities: List[SecurityVulnerability]
    ) -> int:
        """Calculate security score (0-100, higher is better)."""
        if not vulnerabilities:
            return 100

        total_penalty = 0
        for vuln in vulnerabilities:
            penalty = self.severity_levels.get(vuln.severity, 0)
            confidence_penalty = 1.0 if vuln.confidence == "high" else 0.5
            total_penalty += penalty * confidence_penalty

        # Calculate score (inverse of penalty)
        max_penalty = len(vulnerabilities) * 4  # Maximum possible penalty
        score = max(0, 100 - (total_penalty / max_penalty * 100))

        return int(score)

    def generate_security_report(self, result: ValidationResult) -> Dict[str, Any]:
        """Generate comprehensive security report."""
        if not result.stats:
            return {}

        report = {
            "security_summary": {
                "security_score": result.stats.get("security_score", 0),
                "total_vulnerabilities": result.stats.get("vulnerabilities_found", 0),
                "critical_issues": result.stats.get("critical_vulns", 0),
                "high_issues": result.stats.get("high_vulns", 0),
                "medium_issues": result.stats.get("medium_vulns", 0),
                "low_issues": result.stats.get("low_vulns", 0),
            },
            "vulnerability_details": [],
            "risk_assessment": "",
            "recommendations": [],
        }

        # Process vulnerability details
        vuln_by_type = {}
        for error in result.errors:
            if "security-" in error.rule_id:
                vuln_type = error.rule_id.replace("security-", "")
                if vuln_type not in vuln_by_type:
                    vuln_by_type[vuln_type] = []

                vuln_by_type[vuln_type].append(
                    {
                        "message": error.message,
                        "file_path": error.file_path,
                        "line_number": error.line_number,
                        "severity": error.severity,
                    }
                )

        for vuln_type, details in vuln_by_type.items():
            config = self.security_rules.get(vuln_type, {})
            report["vulnerability_details"].append(
                {
                    "type": vuln_type,
                    "cwe_id": config.get("cwe"),
                    "owasp_category": config.get("owasp"),
                    "count": len(details),
                    "severity": config.get("severity", "unknown"),
                    "instances": details,
                }
            )

        # Risk assessment
        critical_count = report["security_summary"]["critical_issues"]
        high_count = report["security_summary"]["high_issues"]

        if critical_count > 0:
            report["risk_assessment"] = "CRITICAL - Immediate action required"
        elif high_count > 2:
            report["risk_assessment"] = "HIGH - Prompt attention needed"
        elif high_count > 0 or critical_count > 0:
            report["risk_assessment"] = "MEDIUM - Security review recommended"
        else:
            report["risk_assessment"] = "LOW - Maintain security practices"

        # Recommendations
        if critical_count > 0:
            report["recommendations"].append(
                "Address critical vulnerabilities immediately"
            )
        if high_count > 0:
            report["recommendations"].append("Review and fix high-severity issues")
        if report["security_summary"]["total_vulnerabilities"] > 10:
            report["recommendations"].append(
                "Consider implementing automated security testing"
            )

        report["recommendations"].extend(
            [
                "Regular security scanning and code reviews",
                "Keep dependencies updated",
                "Implement security headers",
                "Train development team on security best practices",
            ]
        )

        return report
