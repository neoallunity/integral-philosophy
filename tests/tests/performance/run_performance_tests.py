#!/usr/bin/env python3
"""
Performance Test Runner for Integral Philosophy Publishing System
Comprehensive execution and reporting for Phase 2 Performance Tests
"""

import sys
import os
import time
import json
import traceback
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
import subprocess
import psutil
from datetime import datetime

# Add project root to path
sys.path.insert(
    0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
)

from tests.utils.base_test_classes import PerformanceTestCase


class PerformanceTestRunner:
    """Comprehensive performance test runner and report generator"""

    def __init__(self, test_dir: Path = None):
        self.test_dir = test_dir or Path(__file__).parent
        self.project_root = self.test_dir.parent.parent
        self.results_dir = self.project_root / "test_results" / "performance"
        self.results_dir.mkdir(parents=True, exist_ok=True)

        # Test categories and their corresponding files
        self.test_categories = {
            "load": {
                "name": "Load Testing",
                "description": "API performance under concurrent load",
                "files": [self.test_dir / "load" / "test_api_load.py"],
                "priority": "high",
            },
            "benchmark": {
                "name": "Benchmark Testing",
                "description": "Component performance metrics and speed benchmarks",
                "files": [self.test_dir / "benchmark" / "test_component_benchmarks.py"],
                "priority": "high",
            },
            "stress": {
                "name": "Stress Testing",
                "description": "System limits and resource exhaustion scenarios",
                "files": [self.test_dir / "stress" / "test_system_limits.py"],
                "priority": "high",
            },
            "memory": {
                "name": "Memory Profiling",
                "description": "Memory leak detection and component memory usage",
                "files": [self.test_dir / "memory" / "test_memory_profiling.py"],
                "priority": "high",
            },
        }

        self.performance_metrics = {
            "start_time": None,
            "end_time": None,
            "total_duration": 0,
            "categories_executed": 0,
            "tests_executed": 0,
            "tests_passed": 0,
            "tests_failed": 0,
            "errors": [],
        }

    def run_performance_tests(self, categories: List[str] = None) -> Dict[str, Any]:
        """Run comprehensive performance tests"""

        print(
            "🚀 Starting Phase 2 Performance Tests for Integral Philosophy Publishing System"
        )
        print("=" * 80)

        self.performance_metrics["start_time"] = time.time()

        # Determine which categories to run
        if categories is None:
            categories = list(self.test_categories.keys())

        results = {}

        for category in categories:
            if category not in self.test_categories:
                print(f"⚠️  Unknown test category: {category}")
                continue

            print(f"\\n📊 Running {self.test_categories[category]['name']}...")
            print(f"   {self.test_categories[category]['description']}")
            print("-" * 60)

            try:
                category_result = self.run_test_category(category)
                results[category] = category_result
                self.performance_metrics["categories_executed"] += 1

                if category_result["success"]:
                    print(
                        f"✅ {self.test_categories[category]['name']} completed successfully"
                    )
                else:
                    print(
                        f"❌ {self.test_categories[category]['name']} completed with errors"
                    )

            except Exception as e:
                error_msg = f"Failed to run {category} tests: {str(e)}"
                print(f"❌ {error_msg}")
                traceback.print_exc()

                results[category] = {
                    "success": False,
                    "error": error_msg,
                    "traceback": traceback.format_exc(),
                    "duration": 0,
                    "test_results": {},
                }

                self.performance_metrics["errors"].append(error_msg)

        self.performance_metrics["end_time"] = time.time()
        self.performance_metrics["total_duration"] = (
            self.performance_metrics["end_time"]
            - self.performance_metrics["start_time"]
        )

        # Generate comprehensive report
        report = self.generate_performance_report(results)

        # Save report
        report_file = self.save_performance_report(report)

        # Print summary
        self.print_execution_summary(report)

        return {
            "report": report,
            "report_file": report_file,
            "metrics": self.performance_metrics,
        }

    def run_test_category(self, category: str) -> Dict[str, Any]:
        """Run all tests in a specific category"""

        category_config = self.test_categories[category]
        category_results = {
            "success": True,
            "duration": 0,
            "test_results": {},
            "summary": {"total_tests": 0, "passed": 0, "failed": 0, "errors": 0},
        }

        start_time = time.time()

        for test_file in category_config["files"]:
            if not test_file.exists():
                error_msg = f"Test file not found: {test_file}"
                print(f"   ⚠️  {error_msg}")
                category_results["test_results"][test_file.name] = {
                    "success": False,
                    "error": error_msg,
                }
                category_results["success"] = False
                continue

            print(f"   🧪 Running {test_file.name}...")

            try:
                test_result = self.run_single_test(test_file)
                category_results["test_results"][test_file.name] = test_result

                # Update summary
                category_results["summary"]["total_tests"] += test_result.get(
                    "tests_run", 0
                )
                category_results["summary"]["passed"] += test_result.get("passed", 0)
                category_results["summary"]["failed"] += test_result.get("failed", 0)
                category_results["summary"]["errors"] += test_result.get("errors", 0)

                if not test_result.get("success", False):
                    category_results["success"] = False

                # Update global metrics
                self.performance_metrics["tests_executed"] += test_result.get(
                    "tests_run", 0
                )
                self.performance_metrics["tests_passed"] += test_result.get("passed", 0)
                self.performance_metrics["tests_failed"] += test_result.get("failed", 0)

                print(
                    f"      ✅ {test_result.get('passed', 0)} passed, "
                    f"❌ {test_result.get('failed', 0)} failed, "
                    f"⚠️  {test_result.get('errors', 0)} errors"
                )

            except Exception as e:
                error_msg = f"Error running {test_file.name}: {str(e)}"
                print(f"      ❌ {error_msg}")
                traceback.print_exc()

                category_results["test_results"][test_file.name] = {
                    "success": False,
                    "error": error_msg,
                    "traceback": traceback.format_exc(),
                }
                category_results["success"] = False
                self.performance_metrics["errors"].append(error_msg)

        category_results["duration"] = time.time() - start_time

        return category_results

    def run_single_test(self, test_file: Path) -> Dict[str, Any]:
        """Run a single test file and capture results"""

        # Change to project root directory
        original_cwd = os.getcwd()
        os.chdir(self.project_root)

        try:
            # Prepare pytest command
            cmd = [
                sys.executable,
                "-m",
                "pytest",
                str(test_file.relative_to(self.project_root)),
                "-v",  # Verbose output
                "--tb=short",  # Short traceback format
                "--json-report",  # JSON report
                f"--json-report-file={self.results_dir / f'{test_file.stem}_results.json'}",
                "-x",  # Stop on first failure for faster feedback
                "--disable-warnings",
            ]

            # Set environment variables for performance testing
            env = os.environ.copy()
            env.update(
                {
                    "PYTHONPATH": str(self.project_root),
                    "PERFORMANCE_TEST": "1",
                    "CI": os.environ.get("CI", "0"),  # Pass through CI flag
                }
            )

            # Run pytest
            start_time = time.time()
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                cwd=self.project_root,
                env=env,
                timeout=1800,  # 30 minute timeout per test file
            )
            end_time = time.time()

            # Parse results
            test_result = {
                "success": result.returncode == 0,
                "duration": end_time - start_time,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "returncode": result.returncode,
                "tests_run": 0,
                "passed": 0,
                "failed": 0,
                "errors": 0,
                "details": {},
            }

            # Try to parse JSON report if available
            json_report_file = self.results_dir / f"{test_file.stem}_results.json"
            if json_report_file.exists():
                try:
                    with open(json_report_file, "r") as f:
                        json_report = json.load(f)

                    # Extract test statistics
                    summary = json_report.get("summary", {})
                    test_result.update(
                        {
                            "tests_run": summary.get("total", 0),
                            "passed": summary.get("passed", 0),
                            "failed": summary.get("failed", 0),
                            "errors": summary.get("error", 0),
                            "details": json_report.get("tests", {}),
                        }
                    )

                except Exception as e:
                    print(f"      ⚠️  Could not parse JSON report: {e}")

            # Try to extract metrics from stdout
            test_result["performance_metrics"] = self.extract_performance_metrics(
                result.stdout
            )

            return test_result

        finally:
            os.chdir(original_cwd)

    def extract_performance_metrics(self, stdout: str) -> Dict[str, Any]:
        """Extract performance metrics from test output"""

        metrics = {
            "response_times": [],
            "memory_usage": [],
            "cpu_usage": [],
            "throughput": [],
            "errors": [],
        }

        # Parse common performance patterns from output
        lines = stdout.split("\\n")

        for line in lines:
            line = line.strip()

            # Extract response time metrics
            if "Response Time:" in line:
                try:
                    time_str = line.split("Response Time:")[1].strip().split("s")[0]
                    metrics["response_times"].append(float(time_str))
                except:
                    pass

            # Extract memory usage
            if "Memory:" in line or "Peak Memory:" in line:
                try:
                    memory_str = line.split("Memory:")[-1].split("MB")[0].strip()
                    metrics["memory_usage"].append(float(memory_str))
                except:
                    pass

            # Extract CPU usage
            if "CPU:" in line:
                try:
                    cpu_str = line.split("CPU:")[-1].split("%")[0].strip()
                    metrics["cpu_usage"].append(float(cpu_str))
                except:
                    pass

            # Extract throughput
            if "Throughput:" in line:
                try:
                    throughput_str = line.split("Throughput:")[1].strip().split()[0]
                    metrics["throughput"].append(float(throughput_str))
                except:
                    pass

        # Calculate statistics
        for key in ["response_times", "memory_usage", "cpu_usage", "throughput"]:
            if metrics[key]:
                metrics[f"{key}_avg"] = sum(metrics[key]) / len(metrics[key])
                metrics[f"{key}_max"] = max(metrics[key])
                metrics[f"{key}_min"] = min(metrics[key])

        return metrics

    def generate_performance_report(self, results: Dict[str, Any]) -> Dict[str, Any]:
        """Generate comprehensive performance report"""

        report = {
            "metadata": {
                "report_title": "Phase 2 Performance Tests - Integral Philosophy Publishing System",
                "generated_at": datetime.now().isoformat(),
                "test_duration": self.performance_metrics["total_duration"],
                "categories_tested": self.performance_metrics["categories_executed"],
                "total_tests": self.performance_metrics["tests_executed"],
            },
            "summary": {
                "overall_success": all(
                    category_result.get("success", False)
                    for category_result in results.values()
                ),
                "categories_passed": sum(
                    1
                    for category_result in results.values()
                    if category_result.get("success", False)
                ),
                "categories_failed": sum(
                    1
                    for category_result in results.values()
                    if not category_result.get("success", False)
                ),
                "tests_passed": self.performance_metrics["tests_passed"],
                "tests_failed": self.performance_metrics["tests_failed"],
                "total_errors": len(self.performance_metrics["errors"]),
            },
            "categories": {},
            "performance_analysis": {},
            "recommendations": [],
            "system_info": self.get_system_info(),
        }

        # Process each category
        for category_name, category_result in results.items():
            if category_name not in self.test_categories:
                continue

            category_config = self.test_categories[category_name]

            report["categories"][category_name] = {
                "name": category_config["name"],
                "description": category_config["description"],
                "priority": category_config["priority"],
                "success": category_result.get("success", False),
                "duration": category_result.get("duration", 0),
                "summary": category_result.get("summary", {}),
                "test_files": category_result.get("test_results", {}),
                "key_metrics": self.extract_category_metrics(category_result),
            }

        # Generate performance analysis
        report["performance_analysis"] = self.generate_performance_analysis(results)

        # Generate recommendations
        report["recommendations"] = self.generate_recommendations(report)

        return report

    def extract_category_metrics(
        self, category_result: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Extract key performance metrics for a category"""

        metrics = {
            "overall_success_rate": 0,
            "response_times": [],
            "memory_usage": [],
            "cpu_usage": [],
            "throughput": [],
        }

        test_files = category_result.get("test_results", {})

        for test_file_name, test_result in test_files.items():
            if isinstance(test_result, dict) and "performance_metrics" in test_result:
                perf_metrics = test_result["performance_metrics"]

                # Collect metrics across all test files
                for key in [
                    "response_times_avg",
                    "memory_usage_avg",
                    "cpu_usage_avg",
                    "throughput_avg",
                ]:
                    if f"{key}" in perf_metrics:
                        target_key = key.replace("_avg", "")
                        metrics[target_key].append(perf_metrics[key])

        # Calculate averages and statistics
        for key in ["response_times", "memory_usage", "cpu_usage", "throughput"]:
            if metrics[key]:
                metrics[f"{key}_average"] = sum(metrics[key]) / len(metrics[key])
                metrics[f"{key}_maximum"] = max(metrics[key])
                metrics[f"{key}_minimum"] = min(metrics[key])

        # Calculate overall success rate
        summary = category_result.get("summary", {})
        total_tests = summary.get("total_tests", 0)
        passed_tests = summary.get("passed", 0)

        if total_tests > 0:
            metrics["overall_success_rate"] = passed_tests / total_tests

        return metrics

    def generate_performance_analysis(self, results: Dict[str, Any]) -> Dict[str, Any]:
        """Generate overall performance analysis"""

        analysis = {
            "performance_trends": {},
            "bottlenecks": [],
            "strengths": [],
            "comparative_analysis": {},
        }

        # Analyze response times across categories
        all_response_times = []
        all_memory_usage = []
        all_cpu_usage = []

        for category_name, category_result in results.items():
            if category_name not in self.test_categories:
                continue

            test_files = category_result.get("test_results", {})

            for test_file_name, test_result in test_files.items():
                if (
                    isinstance(test_result, dict)
                    and "performance_metrics" in test_result
                ):
                    perf_metrics = test_result["performance_metrics"]

                    if "response_times_avg" in perf_metrics:
                        all_response_times.append(perf_metrics["response_times_avg"])
                    if "memory_usage_avg" in perf_metrics:
                        all_memory_usage.append(perf_metrics["memory_usage_avg"])
                    if "cpu_usage_avg" in perf_metrics:
                        all_cpu_usage.append(perf_metrics["cpu_usage_avg"])

        # Calculate overall averages
        if all_response_times:
            analysis["performance_trends"]["average_response_time"] = sum(
                all_response_times
            ) / len(all_response_times)
            analysis["performance_trends"]["max_response_time"] = max(
                all_response_times
            )

        if all_memory_usage:
            analysis["performance_trends"]["average_memory_usage"] = sum(
                all_memory_usage
            ) / len(all_memory_usage)
            analysis["performance_trends"]["max_memory_usage"] = max(all_memory_usage)

        if all_cpu_usage:
            analysis["performance_trends"]["average_cpu_usage"] = sum(
                all_cpu_usage
            ) / len(all_cpu_usage)
            analysis["performance_trends"]["max_cpu_usage"] = max(all_cpu_usage)

        # Identify potential bottlenecks
        if all_response_times and max(all_response_times) > 5.0:
            analysis["bottlenecks"].append(
                {
                    "type": "response_time",
                    "description": f"High response time detected: {max(all_response_times):.2f}s",
                    "severity": "high" if max(all_response_times) > 10.0 else "medium",
                }
            )

        if all_memory_usage and max(all_memory_usage) > 500:
            analysis["bottlenecks"].append(
                {
                    "type": "memory_usage",
                    "description": f"High memory usage detected: {max(all_memory_usage):.1f}MB",
                    "severity": "high" if max(all_memory_usage) > 1000 else "medium",
                }
            )

        # Identify strengths
        if all_response_times and min(all_response_times) < 1.0:
            analysis["strengths"].append(
                {
                    "type": "response_time",
                    "description": f"Good response time achieved: {min(all_response_times):.2f}s",
                }
            )

        if all_memory_usage and max(all_memory_usage) < 200:
            analysis["strengths"].append(
                {
                    "type": "memory_usage",
                    "description": f"Efficient memory usage: max {max(all_memory_usage):.1f}MB",
                }
            )

        return analysis

    def generate_recommendations(self, report: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Generate performance optimization recommendations"""

        recommendations = []

        # Analyze test failures
        if report["summary"]["total_errors"] > 0:
            recommendations.append(
                {
                    "priority": "high",
                    "category": "stability",
                    "title": "Fix Test Failures",
                    "description": f"{report['summary']['total_errors']} test failures detected. Review and fix failing tests.",
                    "action_items": [
                        "Review test failure logs",
                        "Fix identified issues",
                        "Improve error handling",
                        "Add better test coverage",
                    ],
                }
            )

        # Analyze performance from analysis section
        analysis = report.get("performance_analysis", {})

        for bottleneck in analysis.get("bottlenecks", []):
            if bottleneck["type"] == "response_time":
                recommendations.append(
                    {
                        "priority": bottleneck["severity"],
                        "category": "performance",
                        "title": "Optimize Response Times",
                        "description": bottleneck["description"],
                        "action_items": [
                            "Profile slow operations",
                            "Implement caching mechanisms",
                            "Optimize database queries",
                            "Consider async processing",
                        ],
                    }
                )

            elif bottleneck["type"] == "memory_usage":
                recommendations.append(
                    {
                        "priority": bottleneck["severity"],
                        "category": "memory",
                        "title": "Optimize Memory Usage",
                        "description": bottleneck["description"],
                        "action_items": [
                            "Implement memory pooling",
                            "Review memory leaks",
                            "Optimize data structures",
                            "Add memory cleanup procedures",
                        ],
                    }
                )

        # Category-specific recommendations
        for category_name, category_data in report["categories"].items():
            if not category_data["success"]:
                recommendations.append(
                    {
                        "priority": "medium",
                        "category": category_name,
                        "title": f"Improve {category_data['name']} Results",
                        "description": f"{category_data['name']} tests need attention",
                        "action_items": [
                            f"Review {category_name} test failures",
                            f"Check {category_name} system configuration",
                            f"Optimize {category_name} performance",
                        ],
                    }
                )

        return recommendations

    def get_system_info(self) -> Dict[str, Any]:
        """Get system information for the report"""

        return {
            "python_version": f"{sys.version_info.major}.{sys.version_info.minor}.{sys.version_info.micro}",
            "platform": sys.platform,
            "cpu_count": psutil.cpu_count(),
            "memory_total_gb": psutil.virtual_memory().total / (1024**3),
            "disk_free_gb": psutil.disk_usage(self.project_root).free / (1024**3),
            "project_directory": str(self.project_root),
            "test_directory": str(self.test_dir),
            "results_directory": str(self.results_dir),
        }

    def save_performance_report(self, report: Dict[str, Any]) -> Path:
        """Save performance report to file"""

        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        report_file = self.results_dir / f"performance_report_{timestamp}.json"

        with open(report_file, "w", encoding="utf-8") as f:
            json.dump(report, f, indent=2, ensure_ascii=False)

        # Also save as markdown for readability
        markdown_file = self.results_dir / f"performance_report_{timestamp}.md"
        self.save_markdown_report(report, markdown_file)

        print(f"\\n📄 Performance report saved to:")
        print(f"   JSON: {report_file}")
        print(f"   Markdown: {markdown_file}")

        return report_file

    def save_markdown_report(self, report: Dict[str, Any], output_file: Path):
        """Save performance report as markdown"""

        lines = []

        # Title and metadata
        lines.append(f"# {report['metadata']['report_title']}")
        lines.append("")
        lines.append(f"**Generated:** {report['metadata']['generated_at']}")
        lines.append(
            f"**Test Duration:** {report['metadata']['test_duration']:.1f} seconds"
        )
        lines.append(
            f"**Categories Tested:** {report['metadata']['categories_tested']}"
        )
        lines.append(f"**Total Tests:** {report['metadata']['total_tests']}")
        lines.append("")

        # Executive Summary
        lines.append("## Executive Summary")
        lines.append("")
        lines.append(
            f"**Overall Status:** {'✅ PASS' if report['summary']['overall_success'] else '❌ FAIL'}"
        )
        lines.append(
            f"**Categories Passed:** {report['summary']['categories_passed']}/{report['metadata']['categories_tested']}"
        )
        lines.append(
            f"**Tests Passed:** {report['summary']['tests_passed']}/{report['metadata']['total_tests']}"
        )
        lines.append(f"**Total Errors:** {report['summary']['total_errors']}")
        lines.append("")

        # Category Results
        lines.append("## Category Results")
        lines.append("")

        for category_name, category_data in report["categories"].items():
            status = "✅ PASS" if category_data["success"] else "❌ FAIL"
            lines.append(f"### {category_data['name']} - {status}")
            lines.append("")
            lines.append(f"**Description:** {category_data['description']}")
            lines.append(f"**Priority:** {category_data['priority']}")
            lines.append(f"**Duration:** {category_data['duration']:.1f}s")
            lines.append("")

            if category_data["summary"]["total_tests"] > 0:
                lines.append("**Test Summary:**")
                lines.append(
                    f"- Total Tests: {category_data['summary']['total_tests']}"
                )
                lines.append(f"- Passed: {category_data['summary']['passed']}")
                lines.append(f"- Failed: {category_data['summary']['failed']}")
                lines.append(f"- Errors: {category_data['summary']['errors']}")
                lines.append("")

            # Key metrics
            key_metrics = category_data.get("key_metrics", {})
            if key_metrics:
                lines.append("**Performance Metrics:**")
                if "response_times_average" in key_metrics:
                    lines.append(
                        f"- Average Response Time: {key_metrics['response_times_average']:.3f}s"
                    )
                if "memory_usage_average" in key_metrics:
                    lines.append(
                        f"- Average Memory Usage: {key_metrics['memory_usage_average']:.1f}MB"
                    )
                if "cpu_usage_average" in key_metrics:
                    lines.append(
                        f"- Average CPU Usage: {key_metrics['cpu_usage_average']:.1f}%"
                    )
                lines.append("")

        # Performance Analysis
        analysis = report.get("performance_analysis", {})
        if analysis:
            lines.append("## Performance Analysis")
            lines.append("")

            trends = analysis.get("performance_trends", {})
            if trends:
                lines.append("**Performance Trends:**")
                if "average_response_time" in trends:
                    lines.append(
                        f"- Average Response Time: {trends['average_response_time']:.3f}s"
                    )
                if "average_memory_usage" in trends:
                    lines.append(
                        f"- Average Memory Usage: {trends['average_memory_usage']:.1f}MB"
                    )
                if "average_cpu_usage" in trends:
                    lines.append(
                        f"- Average CPU Usage: {trends['average_cpu_usage']:.1f}%"
                    )
                lines.append("")

            bottlenecks = analysis.get("bottlenecks", [])
            if bottlenecks:
                lines.append("**Bottlenecks Identified:**")
                for bottleneck in bottlenecks:
                    lines.append(
                        f"- **{bottleneck['type'].title()}** ({bottleneck['severity']}): {bottleneck['description']}"
                    )
                lines.append("")

            strengths = analysis.get("strengths", [])
            if strengths:
                lines.append("**Performance Strengths:**")
                for strength in strengths:
                    lines.append(
                        f"- **{strength['type'].title()}**: {strength['description']}"
                    )
                lines.append("")

        # Recommendations
        recommendations = report.get("recommendations", [])
        if recommendations:
            lines.append("## Recommendations")
            lines.append("")

            for rec in recommendations:
                lines.append(f"### {rec['title']} ({rec['priority']} priority)")
                lines.append("")
                lines.append(f"**Category:** {rec['category']}")
                lines.append(f"**Description:** {rec['description']}")
                lines.append("")
                lines.append("**Action Items:**")
                for action in rec["action_items"]:
                    lines.append(f"- {action}")
                lines.append("")

        # System Information
        lines.append("## System Information")
        lines.append("")
        sys_info = report.get("system_info", {})
        for key, value in sys_info.items():
            lines.append(f"**{key.replace('_', ' ').title()}:** {value}")

        # Write to file
        with open(output_file, "w", encoding="utf-8") as f:
            f.write("\\n".join(lines))

    def print_execution_summary(self, report: Dict[str, Any]):
        """Print execution summary to console"""

        print("\\n" + "=" * 80)
        print("📊 PERFORMANCE TEST EXECUTION SUMMARY")
        print("=" * 80)

        summary = report["summary"]

        print(
            f"\\n🎯 Overall Status: {'✅ PASS' if summary['overall_success'] else '❌ FAIL'}"
        )
        print(
            f"📂 Categories: {summary['categories_passed']}/{report['metadata']['categories_tested']} passed"
        )
        print(
            f"🧪 Tests: {summary['tests_passed']}/{report['metadata']['total_tests']} passed"
        )
        print(f"⚠️  Errors: {summary['total_errors']}")
        print(f"⏱️  Duration: {report['metadata']['test_duration']:.1f}s")

        # Category breakdown
        print("\\n📋 Category Results:")
        for category_name, category_data in report["categories"].items():
            status = "✅" if category_data["success"] else "❌"
            print(
                f"  {status} {category_data['name']}: {category_data['summary']['passed']}/{category_data['summary']['total_tests']} tests passed"
            )

        # Top recommendations
        recommendations = report.get("recommendations", [])
        if recommendations:
            print("\\n💡 Top Recommendations:")
            for i, rec in enumerate(recommendations[:3], 1):
                priority_icon = (
                    "🔴"
                    if rec["priority"] == "high"
                    else "🟡"
                    if rec["priority"] == "medium"
                    else "🟢"
                )
                print(f"  {i}. {priority_icon} {rec['title']}")

        print("\\n" + "=" * 80)


def main():
    """Main entry point for performance test runner"""

    import argparse

    parser = argparse.ArgumentParser(description="Run Phase 2 Performance Tests")
    parser.add_argument(
        "--categories",
        nargs="+",
        choices=["load", "benchmark", "stress", "memory"],
        help="Specific categories to run (default: all)",
    )
    parser.add_argument("--test-dir", type=Path, help="Test directory path")

    args = parser.parse_args()

    # Initialize runner
    runner = PerformanceTestRunner(args.test_dir)

    # Run tests
    result = runner.run_performance_tests(args.categories)

    # Return appropriate exit code
    exit_code = 0 if result["report"]["summary"]["overall_success"] else 1
    return exit_code


if __name__ == "__main__":
    sys.exit(main())
