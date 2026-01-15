#!/usr/bin/env python3
"""
Continuous Quality Monitoring Dashboard for Integral Philosophy publishing system.
Implements real-time quality monitoring, alerting, and analytics.
"""

import json
import time
import threading
import queue
from pathlib import Path
from typing import Dict, List, Any, Optional, Callable, Union
from dataclasses import dataclass, field
from datetime import datetime, timedelta
import logging
from collections import defaultdict, deque
import statistics
import smtplib
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart

from .validators import BaseValidator, ValidationResult, ValidationError

# Import optional components
try:
    from .content_integrity import ContentIntegrityValidator, CrossReferenceValidator
except ImportError:
    ContentIntegrityValidator = None
    CrossReferenceValidator = None

try:
    from .quality_report import QualityReportGenerator, TransformationReport
except ImportError:
    QualityReportGenerator = None
    TransformationReport = None

try:
    from .performance_benchmark import PerformanceBenchmark, BenchmarkResult
except ImportError:
    PerformanceBenchmark = None
    BenchmarkResult = None

logger = logging.getLogger(__name__)


@dataclass
class QualityAlert:
    """Represents a quality alert."""

    alert_id: str
    severity: str  # "critical", "high", "medium", "low", "info"
    category: str  # "validation", "performance", "security", "accessibility"
    title: str
    message: str
    validator_name: str
    file_path: str
    timestamp: datetime = field(default_factory=datetime.now)
    resolved: bool = False
    resolution_notes: Optional[str] = None


@dataclass
class QualityMetrics:
    """Real-time quality metrics."""

    timestamp: datetime
    total_validations: int
    successful_validations: int
    failed_validations: int
    error_count: int
    warning_count: int
    security_vulnerabilities: int
    accessibility_issues: int
    performance_score: float
    validator_performance: Dict[str, float]  # validator -> avg score
    trend_data: Dict[str, deque]  # metric -> last N values


@dataclass
class MonitoringConfig:
    """Configuration for quality monitoring."""

    alert_thresholds: Dict[str, Any] = field(default_factory=dict)
    notification_settings: Dict[str, Any] = field(default_factory=dict)
    retention_period_hours: int = 24
    trend_analysis_window: int = 100
    auto_escalation: bool = True
    integration_endpoints: List[str] = field(default_factory=list)


class QualityDashboard:
    """Comprehensive continuous quality monitoring system."""

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        if config:
            self.config = MonitoringConfig(**config)
        else:
            self.config = MonitoringConfig()

        # Monitoring state
        self.alerts = []
        self.quality_history = deque(maxlen=self.config.trend_analysis_window)
        self.active_monitors = {}
        self.alert_queue = queue.Queue()

        # Background monitoring thread
        self.monitoring_thread = None
        self.shutdown_event = threading.Event()

        # Alert system
        self.alert_handlers = []
        self.alert_escalation_levels = {
            "critical": {"threshold": 1, "escalation_time": 300},  # 5 minutes
            "high": {"threshold": 3, "escalation_time": 900},  # 15 minutes
            "medium": {"threshold": 5, "escalation_time": 1800},  # 30 minutes
            "low": {"threshold": 10, "escalation_time": 3600},  # 1 hour
        }

        # Initialize default alert handlers
        self._init_alert_handlers()

        # Start monitoring
        self._start_monitoring()

    def _init_alert_handlers(self):
        """Initialize default alert handlers."""

        def log_alert(alert: QualityAlert):
            """Log alert to file."""
            log_message = f"[{alert.timestamp.isoformat()}] {alert.severity.upper()} {alert.category}: {alert.title}"
            logger.warning(log_message)

        def console_alert(alert: QualityAlert):
            """Print alert to console."""
            severity_emoji = {
                "critical": "🚨",
                "high": "⚠️",
                "medium": "📊",
                "low": "ℹ️",
                "info": "📋",
            }

            emoji = severity_emoji.get(alert.severity, "📊")
            print(f"{emoji} {alert.severity.upper()} {alert.category}: {alert.title}")
            print(f"   {alert.message}")
            print(f"   Validator: {alert.validator_name}")
            print(f"   File: {alert.file_path}")
            print()

        self.alert_handlers = [log_alert, console_alert]

    def _start_monitoring(self):
        """Start background monitoring thread."""
        self.monitoring_thread = threading.Thread(
            target=self._monitoring_loop, name="QualityMonitor", daemon=True
        )
        self.monitoring_thread.start()
        logger.info("Quality monitoring started")

    def _monitoring_loop(self):
        """Main monitoring loop."""
        while not self.shutdown_event.is_set():
            try:
                # Process alerts from queue
                try:
                    alert = self.alert_queue.get(timeout=1.0)
                    self._process_alert(alert)
                except queue.Empty:
                    pass

                # Cleanup old alerts and metrics
                self._cleanup_old_data()

                # Update quality metrics
                self._update_quality_metrics()

            except Exception as e:
                logger.error(f"Monitoring loop error: {e}")

            time.sleep(10)  # Check every 10 seconds

    def add_validation_result(
        self, result: ValidationResult, validator_name: str, file_path: str
    ):
        """Add validation result to monitoring system."""

        # Create quality metrics
        metrics = self._create_metrics_from_result(result, validator_name, file_path)

        # Add to history
        self.quality_history.append(metrics)

        # Check for alerts
        self._check_for_alerts(metrics)

        # Log to monitoring system
        logger.info(f"Validation result added: {validator_name} - {result.is_valid}")

    def _create_metrics_from_result(
        self, result: ValidationResult, validator_name: str, file_path: str
    ) -> QualityMetrics:
        """Create quality metrics from validation result."""

        error_count = len([e for e in result.errors if e.severity == "error"])
        warning_count = len([e for e in result.errors if e.severity == "warning"])

        # Calculate performance score from stats if available
        performance_score = 0.0
        if result.stats:
            # Use validation statistics to calculate score
            total_checks = result.stats.get("total_checks", 1)
            successful_checks = total_checks - error_count
            performance_score = (
                (successful_checks / total_checks) * 100 if total_checks > 0 else 0
            )

        return QualityMetrics(
            timestamp=datetime.now(),
            total_validations=1,
            successful_validations=1 if result.is_valid else 0,
            failed_validations=0 if result.is_valid else 1,
            error_count=error_count,
            warning_count=warning_count,
            security_vulnerabilities=0,  # Would be extracted from security validator
            accessibility_issues=0,  # Would be extracted from accessibility validator
            performance_score=performance_score,
            validator_performance={validator_name: performance_score},
            trend_data=defaultdict(lambda: deque(maxlen=100)),
        )

    def _check_for_alerts(self, metrics: QualityMetrics):
        """Check if metrics trigger any alerts."""

        # Error count alert
        if metrics.error_count > 0:
            alert = QualityAlert(
                alert_id=f"error_{metrics.timestamp.timestamp()}",
                severity="critical",
                category="validation",
                title="Validation Failed",
                message=f"Validation completed with {metrics.error_count} errors",
                validator_name=list(metrics.validator_performance.keys())[0],
                file_path="monitoring",
            )
            self.alert_queue.put(alert)

        # Performance score alert
        if metrics.performance_score < 50:
            alert = QualityAlert(
                alert_id=f"performance_{metrics.timestamp.timestamp()}",
                severity="medium",
                category="performance",
                title="Low Performance Score",
                message=f"Performance score: {metrics.performance_score:.1f}% (threshold: 50%)",
                validator_name=list(metrics.validator_performance.keys())[0],
                file_path="monitoring",
            )
            self.alert_queue.put(alert)

        # Warning count alert
        if metrics.warning_count > 10:
            alert = QualityAlert(
                alert_id=f"warnings_{metrics.timestamp.timestamp()}",
                severity="high",
                category="validation",
                title="High Warning Count",
                message=f"Validation generated {metrics.warning_count} warnings",
                validator_name=list(metrics.validator_performance.keys())[0],
                file_path="monitoring",
            )
            self.alert_queue.put(alert)

        # Check for trend alerts
        self._check_trend_alerts(metrics)

    def _check_trend_alerts(self, metrics: QualityMetrics):
        """Check for trends that need alerting."""

        if len(self.quality_history) < 10:
            return  # Not enough data for trend analysis

        recent_scores = [m.performance_score for m in list(self.quality_history)[-10:]]

        # Declining performance trend
        if len(recent_scores) >= 5:
            recent_avg = statistics.mean(recent_scores[-5:])
            older_avg = (
                statistics.mean(recent_scores[-10:-5])
                if len(recent_scores) >= 10
                else recent_avg
            )

            if older_avg > 0 and recent_avg < (older_avg * 0.8):  # 20% decline
                alert = QualityAlert(
                    alert_id=f"trend_decline_{metrics.timestamp.timestamp()}",
                    severity="medium",
                    category="performance",
                    title="Performance Decline Detected",
                    message=f"Performance declined from {older_avg:.1f}% to {recent_avg:.1f}%",
                    validator_name=list(metrics.validator_performance.keys())[0],
                    file_path="monitoring",
                )
                self.alert_queue.put(alert)

    def _process_alert(self, alert: QualityAlert):
        """Process and distribute alert."""

        # Check if alert already exists
        if self._is_duplicate_alert(alert):
            return

        # Add to alerts list
        self.alerts.append(alert)

        # Distribute to handlers
        for handler in self.alert_handlers:
            try:
                handler(alert)
            except Exception as e:
                logger.error(f"Alert handler failed: {e}")

        # Check for escalation
        if self.config.auto_escalation:
            self._check_escalation(alert)

    def _is_duplicate_alert(self, alert: QualityAlert) -> bool:
        """Check if alert is duplicate of recent alert."""

        # Check recent alerts for same type and file
        cutoff_time = datetime.now() - timedelta(minutes=5)
        recent_alerts = [a for a in self.alerts if a.timestamp > cutoff_time]

        for recent_alert in recent_alerts:
            if (
                recent_alert.validator_name == alert.validator_name
                and recent_alert.category == alert.category
                and recent_alert.title == alert.title
            ):
                return True

        return False

    def _check_escalation(self, alert: QualityAlert):
        """Check if alert should be escalated."""

        escalation_config = self.alert_escalation_levels.get(alert.severity)
        if not escalation_config:
            return

        # Count similar alerts
        similar_alerts = [
            a
            for a in self.alerts
            if (
                a.severity == alert.severity
                and a.category == alert.category
                and a.timestamp
                > datetime.now()
                - timedelta(seconds=escalation_config["escalation_time"])
            )
        ]

        if len(similar_alerts) >= escalation_config["threshold"]:
            self._escalate_alert(alert, len(similar_alerts))

    def _escalate_alert(self, alert: QualityAlert, count: int):
        """Escalate alert to higher priority."""

        escalation_alert = QualityAlert(
            alert_id=f"escalation_{alert.alert_id}",
            severity="critical",
            category="escalation",
            title=f"ESCALATION: {alert.title}",
            message=f"Alert escalated after {count} occurrences: {alert.message}",
            validator_name=alert.validator_name,
            file_path=alert.file_path,
        )

        self.alert_queue.put(escalation_alert)

    def _cleanup_old_data(self):
        """Clean up old alerts and metrics."""

        cutoff_time = datetime.now() - timedelta(
            hours=self.config.retention_period_hours
        )

        # Clean old alerts
        original_count = len(self.alerts)
        self.alerts = [a for a in self.alerts if a.timestamp > cutoff_time]
        cleaned_count = original_count - len(self.alerts)

        if cleaned_count > 0:
            logger.info(f"Cleaned up {cleaned_count} old alerts")

    def _update_quality_metrics(self):
        """Update aggregate quality metrics."""

        if not self.quality_history:
            return

        current_metrics = self.quality_history[-1]

        # Update trend data
        current_metrics.trend_data["performance_score"].append(
            current_metrics.performance_score
        )
        current_metrics.trend_data["error_count"].append(current_metrics.error_count)
        current_metrics.trend_data["warning_count"].append(
            current_metrics.warning_count
        )

    def get_quality_summary(self, hours: int = 24) -> Dict[str, Any]:
        """Get quality summary for specified time period."""

        cutoff_time = datetime.now() - timedelta(hours=hours)
        recent_metrics = [m for m in self.quality_history if m.timestamp > cutoff_time]

        if not recent_metrics:
            return {
                "period_hours": hours,
                "total_validations": 0,
                "success_rate": 0,
                "average_performance_score": 0,
                "total_alerts": 0,
                "trend": "no_data",
            }

        # Calculate summary statistics
        total_validations = len(recent_metrics)
        successful_validations = sum(m.successful_validations for m in recent_metrics)
        error_count = sum(m.error_count for m in recent_metrics)
        warning_count = sum(m.warning_count for m in recent_metrics)
        performance_scores = [m.performance_score for m in recent_metrics]

        # Calculate trends
        if len(performance_scores) >= 10:
            recent_avg = statistics.mean(performance_scores[-5:])
            older_avg = statistics.mean(performance_scores[-10:-5])
            trend = (
                "improving"
                if recent_avg > older_avg
                else "declining"
                if recent_avg < older_avg
                else "stable"
            )
        else:
            trend = "insufficient_data"

        # Recent alerts
        recent_alerts = [a for a in self.alerts if a.timestamp > cutoff_time]
        alerts_by_severity = defaultdict(int)
        for alert in recent_alerts:
            alerts_by_severity[alert.severity] += 1

        return {
            "period_hours": hours,
            "total_validations": total_validations,
            "success_rate": (successful_validations / total_validations * 100)
            if total_validations > 0
            else 0,
            "average_performance_score": statistics.mean(performance_scores)
            if performance_scores
            else 0,
            "total_errors": error_count,
            "total_warnings": warning_count,
            "trend": trend,
            "total_alerts": len(recent_alerts),
            "alerts_by_severity": dict(alerts_by_severity),
            "validator_performance": self._calculate_validator_averages(recent_metrics),
            "quality_health": self._assess_quality_health(
                statistics.mean(performance_scores) if performance_scores else 0
            ),
        }

    def _calculate_validator_averages(
        self, metrics: List[QualityMetrics]
    ) -> Dict[str, float]:
        """Calculate average performance by validator."""

        validator_scores = defaultdict(list)
        for metric in metrics:
            for validator_name, score in metric.validator_performance.items():
                validator_scores[validator_name].append(score)

        return {
            validator: statistics.mean(scores) if scores else 0
            for validator, scores in validator_scores.items()
        }

    def _assess_quality_health(self, avg_score: float) -> str:
        """Assess overall quality health."""

        if avg_score >= 90:
            return "excellent"
        elif avg_score >= 75:
            return "good"
        elif avg_score >= 60:
            return "fair"
        elif avg_score >= 40:
            return "poor"
        else:
            return "critical"

    def get_active_alerts(self, severity: Optional[str] = None) -> List[QualityAlert]:
        """Get active alerts, optionally filtered by severity."""

        alerts = self.alerts.copy()
        if severity:
            alerts = [a for a in alerts if a.severity == severity]

        # Sort by timestamp (most recent first)
        alerts.sort(key=lambda a: a.timestamp, reverse=True)
        return alerts

    def resolve_alert(self, alert_id: str, resolution_notes: Optional[str] = None):
        """Mark alert as resolved."""

        for alert in self.alerts:
            if alert.alert_id == alert_id:
                alert.resolved = True
                alert.resolution_notes = resolution_notes
                logger.info(f"Alert {alert_id} resolved: {resolution_notes}")
                return

        logger.warning(f"Alert {alert_id} not found for resolution")

    def generate_dashboard_report(self) -> Dict[str, Any]:
        """Generate comprehensive dashboard report."""

        summary_24h = self.get_quality_summary(24)
        summary_7d = self.get_quality_summary(24 * 7)
        summary_30d = self.get_quality_summary(24 * 30)

        active_alerts = self.get_active_alerts()
        critical_alerts = self.get_active_alerts("critical")
        high_alerts = self.get_active_alerts("high")

        # Calculate quality trends
        current_health = summary_24h["quality_health"]
        recent_trend = summary_24h["trend"]

        return {
            "dashboard_overview": {
                "last_updated": datetime.now().isoformat(),
                "monitoring_status": "active"
                if self.monitoring_thread and self.monitoring_thread.is_alive()
                else "inactive",
                "total_monitored_validations": summary_24h["total_validations"],
                "current_health": current_health,
                "recent_trend": recent_trend,
            },
            "quality_metrics": {
                "last_24_hours": summary_24h,
                "last_7_days": summary_7d,
                "last_30_days": summary_30d,
            },
            "alerts_summary": {
                "total_active_alerts": len(active_alerts),
                "critical_alerts": len(critical_alerts),
                "high_alerts": len(high_alerts),
                "alerts_by_severity": summary_24h["alerts_by_severity"],
                "recent_alerts": active_alerts[:10],  # Last 10 alerts
            },
            "validator_performance": summary_24h["validator_performance"],
            "recommendations": self._generate_dashboard_recommendations(summary_24h),
        }

    def _generate_dashboard_recommendations(self, summary: Dict[str, Any]) -> List[str]:
        """Generate dashboard recommendations based on quality metrics."""

        recommendations = []

        if summary["total_errors"] > 5:
            recommendations.append(
                "High error rate detected - review validation rules and content quality"
            )

        if summary["total_warnings"] > 20:
            recommendations.append(
                "Many warnings generated - consider tightening quality standards"
            )

        if summary["average_performance_score"] < 70:
            recommendations.append(
                "Below target performance score - investigate validation bottlenecks"
            )

        if summary["trend"] == "declining":
            recommendations.append(
                "Quality trend declining - implement quality improvement measures"
            )

        if len(summary.get("alerts_by_severity", {})) > 10:
            recommendations.append(
                "High alert volume - review monitoring thresholds and processes"
            )

        # Validator-specific recommendations
        for validator, avg_score in summary.get("validator_performance", {}).items():
            if avg_score < 60:
                recommendations.append(
                    f"{validator} validator performing poorly - review optimization opportunities"
                )

        return recommendations

    def add_custom_alert_handler(self, handler):
        """Add custom alert handler."""
        self.alert_handlers.append(handler)

    def export_alerts(
        self, format: str = "json", output_file: Optional[Path] = None
    ) -> str:
        """Export alerts in specified format."""

        alert_data = []
        for alert in self.alerts:
            alert_data.append(
                {
                    "alert_id": alert.alert_id,
                    "severity": alert.severity,
                    "category": alert.category,
                    "title": alert.title,
                    "message": alert.message,
                    "validator_name": alert.validator_name,
                    "file_path": alert.file_path,
                    "timestamp": alert.timestamp.isoformat(),
                    "resolved": alert.resolved,
                    "resolution_notes": alert.resolution_notes,
                }
            )

        if format.lower() == "json":
            export_data = json.dumps(alert_data, indent=2)
        else:
            # Simple text format
            export_data = "\n".join(
                [
                    f"{a['timestamp']} [{a['severity'].upper()}] {a['category']}: {a['title']}"
                    f"  {a['message']}"
                    f"  Validator: {a['validator_name']}"
                    f"  File: {a['file_path']}"
                    f"  Resolved: {a['resolved']}"
                    ""
                    for a in alert_data
                ]
            )

        if output_file:
            with open(output_file, "w", encoding="utf-8") as f:
                f.write(export_data)

        return export_data

    def shutdown(self):
        """Shutdown monitoring system gracefully."""
        logger.info("Shutting down quality monitoring...")

        # Signal shutdown
        self.shutdown_event.set()

        # Wait for monitoring thread to finish
        if self.monitoring_thread:
            self.monitoring_thread.join(timeout=5)

        logger.info("Quality monitoring shutdown complete")
