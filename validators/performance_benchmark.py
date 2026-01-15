#!/usr/bin/env python3
"""
Performance Benchmarking and Optimization System for Integral Philosophy publishing system.
Implements comprehensive performance measurement, benchmarking, and optimization recommendations.
"""

import time
import psutil
import sys
import threading
import statistics
from pathlib import Path
from typing import Dict, List, Any, Optional, Callable, Tuple
from dataclasses import dataclass, field
from datetime import datetime, timedelta
import logging
import json
import gc
from concurrent.futures import ThreadPoolExecutor
import hashlib
import os

from .validators import BaseValidator, ValidationResult, ValidationError

logger = logging.getLogger(__name__)


@dataclass
class PerformanceMetrics:
    """Performance metrics for validation operations."""

    execution_time: float
    memory_usage_mb: float
    cpu_usage_percent: float
    file_size_bytes: int
    throughput_mb_per_sec: float
    validator_name: str
    timestamp: datetime = field(default_factory=datetime.now)
    errors_found: int = 0
    warnings_found: int = 0


@dataclass
class BenchmarkResult:
    """Results of performance benchmarking."""

    validator_name: str
    file_path: str
    metrics: PerformanceMetrics
    optimization_recommendations: List[str]
    performance_score: int  # 0-100
    comparison_percentile: Optional[float] = None


class PerformanceBenchmark(BaseValidator):
    """Comprehensive performance benchmarking and optimization system."""

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        super().__init__(config)
        self.baseline_metrics = {}
        self.performance_history = []
        self.optimization_rules = {
            "slow_validation_threshold": 5.0,  # seconds
            "high_memory_threshold": 100.0,  # MB
            "high_cpu_threshold": 80.0,  # percent
            "low_throughput_threshold": 1.0,  # MB/sec
        }

        # Import validators for benchmarking
        try:
            from .epub3_validator import EPUB3Validator
            from .pdf_validator import PDFValidator
            from .docx_validator import DOCXValidator
            from .wcag_validator import WCAGValidator
            from .security_scanner import SecurityScanner

            self.available_validators = {
                "epub3": EPUB3Validator,
                "pdf": PDFValidator,
                "docx": DOCXValidator,
                "wcag": WCAGValidator,
                "security": SecurityScanner,
            }
        except ImportError as e:
            logger.warning(f"Some validators not available for benchmarking: {e}")
            self.available_validators = {}

    def validate(self, file_path: Path) -> ValidationResult:
        """Run performance benchmarking on file."""
        errors = []
        stats = {"file_size": file_path.stat().st_size, "checks_performed": 0}

        try:
            # Determine file type and select appropriate validator
            validator_name = self._determine_validator_type(file_path)

            if validator_name not in self.available_validators:
                errors.append(
                    ValidationError(
                        severity="warning",
                        message=f"No validator available for benchmarking: {validator_name}",
                        file_path=str(file_path),
                        rule_id="benchmark-validator-unavailable",
                    )
                )
                return self._create_result(
                    is_valid=len([e for e in errors if e.severity == "error"]) == 0,
                    errors=errors,
                    stats=stats,
                )

            # Get validator and run benchmark
            validator_class = self.available_validators[validator_name]
            validator = validator_class()

            # Benchmark the validation
            metrics = self._benchmark_validation(validator, file_path, validator_name)

            # Generate optimization recommendations
            recommendations = self._generate_optimization_recommendations(metrics)

            # Calculate performance score
            performance_score = self._calculate_performance_score(metrics)

            # Create benchmark result
            benchmark_result = BenchmarkResult(
                validator_name=validator_name,
                file_path=str(file_path),
                metrics=metrics,
                optimization_recommendations=recommendations,
                performance_score=performance_score,
            )

            # Update stats
            stats_dict = {
                "performance_score": performance_score,
                "execution_time": metrics.execution_time,
                "memory_usage_mb": metrics.memory_usage_mb,
                "cpu_usage_percent": metrics.cpu_usage_percent,
                "throughput_mb_per_sec": metrics.throughput_mb_per_sec,
                "recommendations_count": len(recommendations),
                "validator_benchmarked": validator_name,
            }
            stats.update(stats_dict)

            stats["total_checks"] = 1  # One benchmark check performed

        except Exception as e:
            errors.append(
                ValidationError(
                    severity="error",
                    message=f"Performance benchmarking failed: {str(e)}",
                    file_path=str(file_path),
                    rule_id="benchmark-validation-error",
                )
            )

        return self._create_result(
            is_valid=len([e for e in errors if e.severity == "error"]) == 0,
            errors=errors,
            stats=stats,
        )

    def _determine_validator_type(self, file_path: Path) -> str:
        """Determine appropriate validator based on file type."""
        suffix = file_path.suffix.lower()

        type_mapping = {
            ".epub": "epub3",
            ".pdf": "pdf",
            ".docx": "docx",
            ".html": "wcag",
            ".htm": "wcag",
            ".xhtml": "wcag",
            ".css": "wcag",
            ".js": "wcag",
        }

        return type_mapping.get(suffix, "unknown")

    def _benchmark_validation(
        self, validator: BaseValidator, file_path: Path, validator_name: str
    ) -> PerformanceMetrics:
        """Benchmark a validation operation with comprehensive metrics."""

        # Get initial system state
        process = psutil.Process()
        initial_memory = process.memory_info().rss / 1024 / 1024  # MB
        initial_cpu = process.cpu_percent()

        # Start monitoring
        start_time = time.time()
        peak_memory = initial_memory
        peak_cpu = initial_cpu

        def monitor_performance():
            """Monitor system performance during validation."""
            nonlocal peak_memory, peak_cpu
            try:
                current_memory = process.memory_info().rss / 1024 / 1024
                current_cpu = process.cpu_percent()
                peak_memory = max(peak_memory, current_memory)
                peak_cpu = max(peak_cpu, current_cpu)
            except Exception:
                pass

        # Run validation with monitoring
        with ThreadPoolExecutor(max_workers=2) as executor:
            # Start validation
            validation_future = executor.submit(validator.validate, file_path)

            # Start monitoring in background
            monitor_future = executor.submit(monitor_performance)

            # Wait for validation to complete
            validation_result = validation_future.result()

            # Stop monitoring
            monitor_future.cancel()

        end_time = time.time()

        # Calculate final metrics
        execution_time = end_time - start_time
        final_memory = process.memory_info().rss / 1024 / 1024
        file_size = file_path.stat().st_size
        throughput = (
            file_size / (1024 * 1024) / execution_time if execution_time > 0 else 0
        )

        # Count errors and warnings
        errors_found = len(
            [e for e in validation_result.errors if e.severity == "error"]
        )
        warnings_found = len(
            [e for e in validation_result.errors if e.severity == "warning"]
        )

        metrics = PerformanceMetrics(
            execution_time=execution_time,
            memory_usage_mb=peak_memory,
            cpu_usage_percent=peak_cpu,
            file_size_bytes=file_size,
            throughput_mb_per_sec=throughput,
            validator_name=validator_name,
            errors_found=errors_found,
            warnings_found=warnings_found,
        )

        # Store metrics for historical comparison
        self.performance_history.append(metrics)

        return metrics

    def _generate_optimization_recommendations(
        self, metrics: PerformanceMetrics
    ) -> List[str]:
        """Generate optimization recommendations based on performance metrics."""
        recommendations = []

        # Execution time recommendations
        if (
            metrics.execution_time
            > self.optimization_rules["slow_validation_threshold"]
        ):
            recommendations.append(
                f"Slow validation detected ({metrics.execution_time:.2f}s). "
                "Consider optimizing validation logic or file preprocessing."
            )

        # Memory usage recommendations
        if metrics.memory_usage_mb > self.optimization_rules["high_memory_threshold"]:
            recommendations.append(
                f"High memory usage ({metrics.memory_usage_mb:.1f}MB). "
                "Consider implementing streaming or chunked processing."
            )

        # CPU usage recommendations
        if metrics.cpu_usage_percent > self.optimization_rules["high_cpu_threshold"]:
            recommendations.append(
                f"High CPU usage ({metrics.cpu_usage_percent:.1f}%). "
                "Consider optimizing algorithms or adding caching."
            )

        # Throughput recommendations
        if (
            metrics.throughput_mb_per_sec
            < self.optimization_rules["low_throughput_threshold"]
        ):
            recommendations.append(
                f"Low throughput ({metrics.throughput_mb_per_sec:.2f}MB/s). "
                "Consider parallel processing or I/O optimization."
            )

        # File size specific recommendations
        if metrics.file_size_bytes > 50 * 1024 * 1024:  # > 50MB
            recommendations.append(
                "Large file detected. Consider file compression or pre-processing optimization."
            )

        # Validator-specific recommendations
        if metrics.validator_name == "epub3":
            if metrics.execution_time > 10:
                recommendations.append(
                    "EPUB3 validation is slow. Check for efficient ZIP parsing."
                )
        elif metrics.validator_name == "pdf":
            if metrics.memory_usage_mb > 200:
                recommendations.append(
                    "PDF validation using high memory. Consider streaming PDF parser."
                )
        elif metrics.validator_name == "wcag":
            if metrics.errors_found > 10:
                recommendations.append(
                    "Many accessibility issues found. Consider automated fixing tools."
                )

        return recommendations

    def _calculate_performance_score(self, metrics: PerformanceMetrics) -> int:
        """Calculate overall performance score (0-100)."""

        # Define scoring factors
        time_score = self._score_time_performance(metrics.execution_time)
        memory_score = self._score_memory_performance(metrics.memory_usage_mb)
        cpu_score = self._score_cpu_performance(metrics.cpu_usage_percent)
        throughput_score = self._score_throughput_performance(
            metrics.throughput_mb_per_sec
        )

        # Weight the scores
        weights = {
            "time": 0.3,
            "memory": 0.25,
            "cpu": 0.2,
            "throughput": 0.25,
        }

        total_score = (
            time_score * weights["time"]
            + memory_score * weights["memory"]
            + cpu_score * weights["cpu"]
            + throughput_score * weights["throughput"]
        )

        return int(total_score)

    def _score_time_performance(self, execution_time: float) -> float:
        """Score execution time (0-100, higher is better)."""
        # Lower time = higher score
        optimal_time = 1.0  # 1 second is optimal
        if execution_time <= optimal_time:
            return 100.0

        # Score decreases exponentially after optimal time
        score = max(0, 100 * (optimal_time / execution_time) ** 0.5)
        return score

    def _score_memory_performance(self, memory_mb: float) -> float:
        """Score memory usage (0-100, higher is better)."""
        # Lower memory = higher score
        optimal_memory = 50.0  # 50MB is optimal
        if memory_mb <= optimal_memory:
            return 100.0

        score = max(0, 100 * (optimal_memory / memory_mb) ** 0.7)
        return score

    def _score_cpu_performance(self, cpu_percent: float) -> float:
        """Score CPU usage (0-100, higher is better)."""
        # Lower CPU = higher score
        optimal_cpu = 25.0  # 25% is optimal
        if cpu_percent <= optimal_cpu:
            return 100.0

        score = max(0, 100 * (optimal_cpu / cpu_percent) ** 0.8)
        return score

    def _score_throughput_performance(self, throughput_mb_per_sec: float) -> float:
        """Score throughput (0-100, higher is better)."""
        # Higher throughput = higher score
        optimal_throughput = 10.0  # 10MB/s is optimal
        if throughput_mb_per_sec >= optimal_throughput:
            return 100.0

        score = min(100, 100 * (throughput_mb_per_sec / optimal_throughput) ** 1.2)
        return score

    def run_comprehensive_benchmark(
        self, file_paths: List[Path]
    ) -> Dict[str, List[BenchmarkResult]]:
        """Run comprehensive benchmarks across multiple files."""
        results = {}

        for validator_name in self.available_validators.keys():
            validator_results = []

            for file_path in file_paths:
                if self._determine_validator_type(file_path) == validator_name:
                    try:
                        validator_class = self.available_validators[validator_name]
                        validator = validator_class()

                        # Benchmark this file
                        metrics = self._benchmark_validation(
                            validator, file_path, validator_name
                        )
                        recommendations = self._generate_optimization_recommendations(
                            metrics
                        )
                        performance_score = self._calculate_performance_score(metrics)

                        result = BenchmarkResult(
                            validator_name=validator_name,
                            file_path=str(file_path),
                            metrics=metrics,
                            optimization_recommendations=recommendations,
                            performance_score=performance_score,
                        )

                        validator_results.append(result)

                    except Exception as e:
                        logger.error(f"Benchmark failed for {file_path}: {e}")

            if validator_results:
                results[validator_name] = validator_results

        return results

    def generate_performance_report(
        self, benchmark_results: Dict[str, List[BenchmarkResult]]
    ) -> Dict[str, Any]:
        """Generate comprehensive performance report."""

        report = {
            "summary": {
                "total_files_benchmarked": 0,
                "total_validators_used": len(benchmark_results),
                "average_performance_score": 0,
                "best_performing_validator": None,
                "worst_performing_validator": None,
                "overall_assessment": "",
            },
            "validator_performance": {},
            "optimization_recommendations": [],
            "trending_analysis": {},
            "resource_utilization": {
                "average_execution_time": 0,
                "average_memory_usage": 0,
                "average_cpu_usage": 0,
                "average_throughput": 0,
            },
        }

        all_results = []
        for validator_name, results in benchmark_results.items():
            all_results.extend(results)

            # Calculate validator-specific metrics
            if results:
                avg_score = sum(r.performance_score for r in results) / len(results)
                avg_time = sum(r.metrics.execution_time for r in results) / len(results)
                avg_memory = sum(r.metrics.memory_usage_mb for r in results) / len(
                    results
                )

                report["validator_performance"][validator_name] = {
                    "files_count": len(results),
                    "average_score": avg_score,
                    "average_time": avg_time,
                    "average_memory": avg_memory,
                    "best_score": max(r.performance_score for r in results),
                    "worst_score": min(r.performance_score for r in results),
                }

        # Calculate overall summary
        if all_results:
            report["summary"]["total_files_benchmarked"] = len(all_results)
            report["summary"]["average_performance_score"] = sum(
                r.performance_score for r in all_results
            ) / len(all_results)
            report["resource_utilization"]["average_execution_time"] = sum(
                r.metrics.execution_time for r in all_results
            ) / len(all_results)
            report["resource_utilization"]["average_memory_usage"] = sum(
                r.metrics.memory_usage_mb for r in all_results
            ) / len(all_results)
            report["resource_utilization"]["average_cpu_usage"] = sum(
                r.metrics.cpu_usage_percent for r in all_results
            ) / len(all_results)
            report["resource_utilization"]["average_throughput"] = sum(
                r.metrics.throughput_mb_per_sec for r in all_results
            ) / len(all_results)

            # Find best/worst performers
            validator_scores = [
                (v["average_score"], v)
                for v in report["validator_performance"].values()
            ]
            if validator_scores:
                report["summary"]["best_performing_validator"] = max(validator_scores)[
                    1
                ]
                report["summary"]["worst_performing_validator"] = min(validator_scores)[
                    1
                ]

            # Overall assessment
            avg_score = report["summary"]["average_performance_score"]
            if avg_score >= 80:
                report["summary"]["overall_assessment"] = "Excellent performance"
            elif avg_score >= 60:
                report["summary"]["overall_assessment"] = "Good performance"
            elif avg_score >= 40:
                report["summary"]["overall_assessment"] = "Fair performance"
            else:
                report["summary"]["overall_assessment"] = (
                    "Poor performance - optimization needed"
                )

        # Collect all recommendations
        all_recommendations = []
        for results in benchmark_results.values():
            for result in results:
                all_recommendations.extend(result.optimization_recommendations)

        # Deduplicate and prioritize recommendations
        report["optimization_recommendations"] = list(set(all_recommendations))

        return report

    def get_system_baseline(self) -> Dict[str, Any]:
        """Get current system performance baseline."""

        # Test system capabilities
        test_file = Path("/tmp/performance_test.txt")
        test_file.write_text("Performance test data" * 1000)

        baseline_metrics = []

        # Test different validators
        for validator_name in ["epub3", "pdf", "wcag"]:
            if validator_name in self.available_validators:
                validator_class = self.available_validators[validator_name]
                validator = validator_class()

                try:
                    # Simple benchmark
                    start_time = time.time()
                    process = psutil.Process()
                    initial_memory = process.memory_info().rss / 1024 / 1024

                    # Run validation
                    validator.validate(test_file)

                    end_time = time.time()
                    final_memory = process.memory_info().rss / 1024 / 1024

                    baseline_metrics.append(
                        {
                            "validator": validator_name,
                            "time": end_time - start_time,
                            "memory_delta": final_memory - initial_memory,
                        }
                    )

                except Exception as e:
                    logger.warning(f"Baseline test failed for {validator_name}: {e}")

        # Cleanup
        test_file.unlink(missing_ok=True)

        return {
            "system_capabilities": baseline_metrics,
            "system_info": {
                "cpu_count": psutil.cpu_count(),
                "memory_total_gb": psutil.virtual_memory().total / (1024**3),
                "disk_free_gb": psutil.disk_usage("/").free / (1024**3),
                "python_version": f"{sys.version_info[0]}.{sys.version_info[1]}"
                if hasattr(sys, "version_info")
                else "unknown",
            },
            "optimization_thresholds": self.optimization_rules,
        }

    def optimize_validator_performance(self, validator_name: str) -> Dict[str, Any]:
        """Generate optimization recommendations for specific validator."""

        optimization_plan = {
            "validator": validator_name,
            "current_performance": {},
            "optimization_opportunities": [],
            "estimated_improvements": {},
            "implementation_priority": [],
        }

        # Analyze historical performance
        validator_history = [
            m for m in self.performance_history if m.validator_name == validator_name
        ]

        if validator_history:
            avg_time = sum(m.execution_time for m in validator_history) / len(
                validator_history
            )
            avg_memory = sum(m.memory_usage_mb for m in validator_history) / len(
                validator_history
            )

            optimization_plan["current_performance"] = {
                "average_execution_time": avg_time,
                "average_memory_usage": avg_memory,
                "sample_count": len(validator_history),
            }

            # Generate optimization opportunities
            if avg_time > 5.0:
                optimization_plan["optimization_opportunities"].append(
                    {
                        "area": "Execution Time",
                        "issue": "Slow validation processing",
                        "recommendation": "Implement caching and optimize algorithms",
                    }
                )
                optimization_plan["estimated_improvements"]["time_reduction"] = "30-50%"
                optimization_plan["implementation_priority"].append("High")

            if avg_memory > 100:
                optimization_plan["optimization_opportunities"].append(
                    {
                        "area": "Memory Usage",
                        "issue": "High memory consumption",
                        "recommendation": "Implement streaming processing and memory pooling",
                    }
                )
                optimization_plan["estimated_improvements"]["memory_reduction"] = (
                    "40-60%"
                )
                optimization_plan["implementation_priority"].append("Medium")

        return optimization_plan
