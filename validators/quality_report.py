#!/usr/bin/env python3
"""
Quality report generator for Integral Philosophy publishing system.
Generates comprehensive reports on transformation quality and validation results.
"""

import json
import statistics
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, asdict
import logging

from .validators import ValidationResult, ValidationError
from .content_integrity import ContentIntegrityValidator

logger = logging.getLogger(__name__)


@dataclass
class QualityMetrics:
    """Quality metrics for a transformation."""

    overall_score: float  # 0-100
    format_scores: Dict[str, float]
    integrity_score: float
    accessibility_score: float
    performance_score: float
    standards_compliance: float


@dataclass
class TransformationReport:
    """Comprehensive transformation quality report."""

    timestamp: str
    source_files: List[str]
    output_formats: List[str]
    validation_results: Dict[str, ValidationResult]
    quality_metrics: QualityMetrics
    errors_by_severity: Dict[str, int]
    recommendations: List[str]
    processing_time: float


class QualityReportGenerator:
    """Generates quality reports for transformation processes."""

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        self.config = config or {}
        self.integrity_validator = ContentIntegrityValidator(config)

    def generate_report(
        self,
        source_files: List[Path],
        output_formats: Dict[str, Path],
        validation_results: Dict[str, ValidationResult],
        processing_time: float = 0.0,
    ) -> TransformationReport:
        """
        Generate comprehensive quality report.

        Args:
            source_files: List of source file paths
            output_formats: Dictionary mapping format names to output file paths
            validation_results: Validation results for each format
            processing_time: Total processing time in seconds
        """
        timestamp = datetime.now().isoformat()

        # Calculate quality metrics
        quality_metrics = self._calculate_quality_metrics(
            output_formats, validation_results
        )

        # Analyze errors
        errors_by_severity = self._analyze_errors(validation_results)

        # Generate recommendations
        recommendations = self._generate_recommendations(validation_results)

        return TransformationReport(
            timestamp=timestamp,
            source_files=[str(f) for f in source_files],
            output_formats=list(output_formats.keys()),
            validation_results=validation_results,
            quality_metrics=quality_metrics,
            errors_by_severity=errors_by_severity,
            recommendations=recommendations,
            processing_time=processing_time,
        )

    def _calculate_quality_metrics(
        self,
        output_formats: Dict[str, Path],
        validation_results: Dict[str, ValidationResult],
    ) -> QualityMetrics:
        """Calculate comprehensive quality metrics."""

        # Individual format scores
        format_scores = {}
        for format_name, result in validation_results.items():
            score = self._calculate_format_score(result)
            format_scores[format_name] = score

        # Overall score (average of format scores)
        overall_score = (
            statistics.mean(format_scores.values()) if format_scores else 0.0
        )

        # Integrity score (if multiple formats)
        integrity_score = 0.0
        if len(output_formats) > 1:
            try:
                integrity_result = (
                    self.integrity_validator.validate_integrity_across_formats(
                        output_formats
                    )
                )
                integrity_score = self._calculate_integrity_score(integrity_result)
            except Exception as e:
                logger.warning(f"Failed to calculate integrity score: {e}")
                integrity_score = 50.0  # Default score

        # Accessibility score (based on HTML validation)
        accessibility_score = self._calculate_accessibility_score(validation_results)

        # Performance score (based on file sizes and processing)
        performance_score = self._calculate_performance_score(output_formats)

        # Standards compliance score
        standards_compliance = self._calculate_standards_compliance(validation_results)

        return QualityMetrics(
            overall_score=overall_score,
            format_scores=format_scores,
            integrity_score=integrity_score,
            accessibility_score=accessibility_score,
            performance_score=performance_score,
            standards_compliance=standards_compliance,
        )

    def _calculate_format_score(self, result: ValidationResult) -> float:
        """Calculate quality score for a single format."""
        if not result.errors:
            return 100.0

        total_errors = len(result.errors)
        error_count = result.error_count
        warning_count = result.warning_count
        info_count = result.info_count

        # Weight different severities
        weighted_score = error_count * 10 + warning_count * 3 + info_count * 1

        # Calculate score (0-100)
        score = max(0, 100 - weighted_score)

        # Bonus points for good stats
        if "checks_performed" in result.stats:
            bonus = min(10, result.stats["checks_performed"] * 0.5)
            score = min(100, score + bonus)

        return score

    def _calculate_integrity_score(self, integrity_result: ValidationResult) -> float:
        """Calculate content integrity score."""
        if integrity_result.is_valid and not integrity_result.errors:
            return 100.0

        # Deduct points for integrity issues
        error_count = integrity_result.error_count
        warning_count = integrity_result.warning_count

        score = max(0, 100 - (error_count * 15 + warning_count * 5))

        # Consider similarity scores if available
        if "similarity_scores" in integrity_result.stats:
            similarities = integrity_result.stats["similarity_scores"].values()
            if similarities:
                avg_similarity = statistics.mean(similarities)
                score = max(score, avg_similarity * 100)

        return score

    def _calculate_accessibility_score(
        self, validation_results: Dict[str, ValidationResult]
    ) -> float:
        """Calculate accessibility compliance score."""
        html_score = 0.0

        if "html" in validation_results:
            html_result = validation_results["html"]

            # Check for accessibility-related errors
            accessibility_errors = [
                e
                for e in html_result.errors
                if e.rule_id and ("accessibility" in e.rule_id or "alt" in e.rule_id)
            ]

            if not accessibility_errors:
                html_score = 100.0
            else:
                # Deduct points for accessibility issues
                critical_issues = len(
                    [e for e in accessibility_errors if e.severity == "error"]
                )
                minor_issues = len(
                    [e for e in accessibility_errors if e.severity == "warning"]
                )

                html_score = max(0, 100 - (critical_issues * 20 + minor_issues * 10))
        else:
            # No HTML format to check
            html_score = 50.0

        return html_score

    def _calculate_performance_score(self, output_formats: Dict[str, Path]) -> float:
        """Calculate performance score based on output files."""
        if not output_formats:
            return 0.0

        scores = []

        for format_name, file_path in output_formats.items():
            if file_path.exists():
                file_size = file_path.stat().st_size

                # Score based on file size (smaller is generally better for web formats)
                if format_name == "html":
                    # HTML should be reasonably sized
                    if file_size < 100 * 1024:  # < 100KB
                        score = 100.0
                    elif file_size < 500 * 1024:  # < 500KB
                        score = 80.0
                    elif file_size < 1024 * 1024:  # < 1MB
                        score = 60.0
                    else:
                        score = 40.0
                elif format_name == "pdf":
                    # PDF can be larger
                    if file_size < 1024 * 1024:  # < 1MB
                        score = 100.0
                    elif file_size < 5 * 1024 * 1024:  # < 5MB
                        score = 80.0
                    elif file_size < 10 * 1024 * 1024:  # < 10MB
                        score = 60.0
                    else:
                        score = 40.0
                else:
                    # Other formats
                    score = 80.0  # Default good score

                scores.append(score)
            else:
                scores.append(0.0)  # File doesn't exist

        return statistics.mean(scores) if scores else 0.0

    def _calculate_standards_compliance(
        self, validation_results: Dict[str, ValidationResult]
    ) -> float:
        """Calculate standards compliance score."""
        if not validation_results:
            return 0.0

        scores = []

        for format_name, result in validation_results.items():
            # Check for standards-related errors
            standards_errors = [
                e
                for e in result.errors
                if e.rule_id
                and any(
                    keyword in e.rule_id.lower()
                    for keyword in ["w3c", "validator", "standard"]
                )
            ]

            if not standards_errors:
                scores.append(100.0)
            else:
                critical_issues = len(
                    [e for e in standards_errors if e.severity == "error"]
                )
                minor_issues = len(
                    [e for e in standards_errors if e.severity == "warning"]
                )

                score = max(0, 100 - (critical_issues * 25 + minor_issues * 10))
                scores.append(score)

        return statistics.mean(scores) if scores else 0.0

    def _analyze_errors(
        self, validation_results: Dict[str, ValidationResult]
    ) -> Dict[str, int]:
        """Analyze errors by severity across all formats."""
        error_counts = {"error": 0, "warning": 0, "info": 0}

        for result in validation_results.values():
            error_counts["error"] += result.error_count
            error_counts["warning"] += result.warning_count
            error_counts["info"] += result.info_count

        return error_counts

    def _generate_recommendations(
        self, validation_results: Dict[str, ValidationResult]
    ) -> List[str]:
        """Generate actionable recommendations based on validation results."""
        recommendations = []

        for format_name, result in validation_results.items():
            # Analyze common issues and generate recommendations
            error_types = {}

            for error in result.errors:
                rule_id = error.rule_id
                if rule_id not in error_types:
                    error_types[rule_id] = []
                error_types[rule_id].append(error)

            # Generate recommendations based on error patterns
            for rule_id, errors in error_types.items():
                if rule_id == "html5-img-alt":
                    recommendations.append(
                        f"Add alt attributes to {len(errors)} images in HTML output for accessibility compliance"
                    )
                elif rule_id == "html5-no-inline-styles":
                    recommendations.append(
                        f"Move {len(errors)} inline styles to external CSS files in HTML output"
                    )
                elif rule_id == "css-important":
                    recommendations.append(
                        f"Replace {len(errors)} !important declarations with more specific CSS selectors"
                    )
                elif rule_id == "js-console-log":
                    recommendations.append(
                        f"Remove {len(errors)} console.log statements from JavaScript output"
                    )
                elif rule_id == "latex-unmatched-environments":
                    recommendations.append(
                        f"Fix {len(errors)} unmatched LaTeX environments"
                    )
                elif rule_id == "integrity-low-similarity":
                    recommendations.append(
                        f"Review content transformation to improve consistency between formats"
                    )

        # Remove duplicates
        unique_recommendations = list(set(recommendations))

        # Sort by priority (errors first, then warnings)
        return sorted(unique_recommendations, key=lambda x: ("Fix" in x, x.lower()))

    def save_report(
        self, report: TransformationReport, output_path: Path, format_type: str = "json"
    ):
        """Save report to file in specified format."""
        output_path = Path(output_path)

        if format_type.lower() == "json":
            with open(output_path, "w", encoding="utf-8") as f:
                json.dump(asdict(report), f, indent=2, ensure_ascii=False)

        elif format_type.lower() == "html":
            html_content = self._generate_html_report(report)
            with open(output_path, "w", encoding="utf-8") as f:
                f.write(html_content)

        elif format_type.lower() == "markdown":
            md_content = self._generate_markdown_report(report)
            with open(output_path, "w", encoding="utf-8") as f:
                f.write(md_content)

        else:
            raise ValueError(f"Unsupported report format: {format_type}")

    def _generate_html_report(self, report: TransformationReport) -> str:
        """Generate HTML version of the report."""
        # Simplified HTML report generation
        html = f"""
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Transformation Quality Report</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 40px; }}
        .score {{ font-size: 24px; font-weight: bold; }}
        .good {{ color: green; }}
        .warning {{ color: orange; }}
        .error {{ color: red; }}
        table {{ border-collapse: collapse; width: 100%; }}
        th, td {{ border: 1px solid #ddd; padding: 8px; text-align: left; }}
        th {{ background-color: #f2f2f2; }}
    </style>
</head>
<body>
    <h1>Transformation Quality Report</h1>
    <p><strong>Generated:</strong> {report.timestamp}</p>
    <p><strong>Processing Time:</strong> {report.processing_time:.2f} seconds</p>
    
    <h2>Overall Quality Score</h2>
    <div class="score {"good" if report.quality_metrics.overall_score >= 80 else "warning" if report.quality_metrics.overall_score >= 60 else "error"}">
        {report.quality_metrics.overall_score:.1f}/100
    </div>
    
    <h2>Format Scores</h2>
    <table>
        <tr><th>Format</th><th>Score</th></tr>
        {"".join([f"<tr><td>{fmt}</td><td>{score:.1f}</td></tr>" for fmt, score in report.quality_metrics.format_scores.items()])}
    </table>
    
    <h2>Quality Metrics</h2>
    <table>
        <tr><th>Metric</th><th>Score</th></tr>
        <tr><td>Content Integrity</td><td>{report.quality_metrics.integrity_score:.1f}</td></tr>
        <tr><td>Accessibility</td><td>{report.quality_metrics.accessibility_score:.1f}</td></tr>
        <tr><td>Performance</td><td>{report.quality_metrics.performance_score:.1f}</td></tr>
        <tr><td>Standards Compliance</td><td>{report.quality_metrics.standards_compliance:.1f}</td></tr>
    </table>
    
    <h2>Error Summary</h2>
    <table>
        <tr><th>Severity</th><th>Count</th></tr>
        <tr><td>Errors</td><td>{report.errors_by_severity.get("error", 0)}</td></tr>
        <tr><td>Warnings</td><td>{report.errors_by_severity.get("warning", 0)}</td></tr>
        <tr><td>Info</td><td>{report.errors_by_severity.get("info", 0)}</td></tr>
    </table>
    
    <h2>Recommendations</h2>
    <ul>
        {"".join([f"<li>{rec}</li>" for rec in report.recommendations])}
    </ul>
</body>
</html>
        """
        return html

    def _generate_markdown_report(self, report: TransformationReport) -> str:
        """Generate Markdown version of the report."""
        md = f"""# Transformation Quality Report

**Generated:** {report.timestamp}  
**Processing Time:** {report.processing_time:.2f} seconds

## Overall Quality Score

**{report.quality_metrics.overall_score:.1f}/100**

## Format Scores

| Format | Score |
|--------|-------|
{chr(10).join([f"| {fmt} | {score:.1f} |" for fmt, score in report.quality_metrics.format_scores.items()])}

## Quality Metrics

| Metric | Score |
|--------|-------|
| Content Integrity | {report.quality_metrics.integrity_score:.1f} |
| Accessibility | {report.quality_metrics.accessibility_score:.1f} |
| Performance | {report.quality_metrics.performance_score:.1f} |
| Standards Compliance | {report.quality_metrics.standards_compliance:.1f} |

## Error Summary

| Severity | Count |
|----------|-------|
| Errors | {report.errors_by_severity.get("error", 0)} |
| Warnings | {report.errors_by_severity.get("warning", 0)} |
| Info | {report.errors_by_severity.get("info", 0)} |

## Recommendations

{chr(10).join([f"- {rec}" for rec in report.recommendations])}
        """
        return md
