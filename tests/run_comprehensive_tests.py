#!/usr/bin/env python3
"""
Comprehensive Test Runner for Integral Philosophy Publishing System

This module provides a complete test runner that orchestrates all test phases
and provides comprehensive reporting for the entire testing framework.
"""

import pytest
import sys
import os
import time
import json
import yaml
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
from datetime import datetime
import subprocess

# Add project root to path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root))
sys.path.insert(0, str(project_root / "scripts"))


@dataclass
class TestResults:
    """Test results data structure"""

    phase: str
    total_tests: int
    passed: int
    failed: int
    skipped: int
    errors: int
    duration: float
    success_rate: float


class ComprehensiveTestRunner:
    """Comprehensive test runner for all phases"""

    def __init__(self):
        self.base_path = Path(__file__).parent.parent
        self.results_dir = self.base_path / "test_results"
        self.results_dir.mkdir(exist_ok=True)

        # Define test phases with their directories and configurations
        self.test_phases = {
            "unit": {
                "directory": "tests/unit",
                "description": "Unit Tests - Individual Component Testing",
                "required": True,
                "timeout": 300,
            },
            "integration": {
                "directory": "tests/integration",
                "description": "Integration Tests - Component Interaction Testing",
                "required": True,
                "timeout": 600,
            },
            "api": {
                "directory": "tests/api",
                "description": "API Tests - REST Interface Testing",
                "required": True,
                "timeout": 300,
            },
            "functional": {
                "directory": "tests/functional",
                "description": "Functional Tests - Business Logic Testing",
                "required": True,
                "timeout": 600,
            },
            "security": {
                "directory": "tests/security",
                "description": "Security Tests - Vulnerability and Access Control",
                "required": True,
                "timeout": 450,
            },
            "performance": {
                "directory": "tests/performance",
                "description": "Performance Tests - Load and Stress Testing",
                "required": False,
                "timeout": 900,
            },
            "ui": {
                "directory": "tests/ui",
                "description": "UI Tests - Web Interface Testing",
                "required": False,
                "timeout": 600,
            },
            "e2e": {
                "directory": "tests/e2e",
                "description": "End-to-End Tests - Full Workflow Testing",
                "required": False,
                "timeout": 900,
            },
            "docker": {
                "directory": "tests/docker",
                "description": "Docker Tests - Container and Orchestration Testing",
                "required": True,
                "timeout": 600,
            },
            "compliance": {
                "directory": "tests/compliance",
                "description": "Compliance Tests - Standards and Regulations",
                "required": True,
                "timeout": 600,
            },
            "infrastructure": {
                "directory": "tests/infrastructure",
                "description": "Infrastructure Tests - CI/CD and Monitoring",
                "required": True,
                "timeout": 600,
            },
        }

        self.all_results: List[TestResults] = []
        self.start_time = None
        self.end_time = None

    def run_all_tests(self, phases: Optional[List[str]] = None) -> bool:
        """Run all specified test phases"""
        self.start_time = datetime.now()

        if phases is None:
            phases = list(self.test_phases.keys())

        print(f"🚀 Starting Integral Philosophy Publishing System Test Suite")
        print(f"📅 Started at: {self.start_time.strftime('%Y-%m-%d %H:%M:%S')}")
        print(f"📋 Phases to run: {', '.join(phases)}")
        print("=" * 80)

        all_passed = True

        for phase_name in phases:
            if phase_name not in self.test_phases:
                print(f"❌ Unknown test phase: {phase_name}")
                all_passed = False
                continue

            phase_config = self.test_phases[phase_name]
            print(f"\n🧪 Running {phase_name.upper()} Tests")
            print(f"📝 {phase_config['description']}")
            print("-" * 60)

            # Check if directory exists
            test_dir = self.base_path / phase_config["directory"]
            if not test_dir.exists():
                print(f"⚠️  Test directory not found: {test_dir}")
                if phase_config["required"]:
                    all_passed = False
                continue

            # Run the phase tests
            phase_results = self._run_test_phase(phase_name, phase_config)
            self.all_results.append(phase_results)

            # Print phase summary
            self._print_phase_summary(phase_name, phase_results)

            # Determine if phase passed
            phase_passed = phase_results.failed == 0 and phase_results.errors == 0
            if not phase_passed and phase_config["required"]:
                all_passed = False

        self.end_time = datetime.now()

        # Generate comprehensive report
        if self.start_time and self.end_time:
            self._generate_comprehensive_report()

        print("\n" + "=" * 80)
        if all_passed:
            print("✅ ALL TESTS PASSED! System is ready for deployment.")
        else:
            print("❌ SOME TESTS FAILED! Please review the failures before deployment.")

        print(f"📅 Completed at: {self.end_time.strftime('%Y-%m-%d %H:%M:%S')}")
        print(
            f"⏱️  Total duration: {(self.end_time - self.start_time).total_seconds():.2f}s"
        )

        return all_passed

    def _run_test_phase(
        self, phase_name: str, phase_config: Dict[str, Any]
    ) -> TestResults:
        """Run a single test phase"""
        test_dir = self.base_path / phase_config["directory"]

        # Build pytest command
        pytest_args = [
            str(test_dir),
            "-v",
            "--tb=short",
            "--json-report",
            f"--json-report-file={self.results_dir / f'{phase_name}_results.json'}",
            f"--timeout={phase_config['timeout']}",
        ]

        # Add coverage for unit tests
        if phase_name == "unit":
            pytest_args.extend(
                [
                    "--cov=src",
                    "--cov-report=html",
                    f"--cov-report=html:{self.results_dir / 'coverage_html'}",
                    "--cov-report=xml",
                    f"--cov-report=xml:{self.results_dir / 'coverage.xml'}",
                ]
            )

        # Run pytest
        start_time = time.time()

        try:
            result = subprocess.run(
                [sys.executable, "-m", "pytest"] + pytest_args,
                cwd=self.base_path,
                capture_output=True,
                text=True,
                timeout=phase_config["timeout"] + 60,  # Extra time for setup
            )

            duration = time.time() - start_time

            # Parse results from JSON report
            json_report_path = self.results_dir / f"{phase_name}_results.json"
            if json_report_path.exists():
                with open(json_report_path, "r") as f:
                    json_results = json.load(f)

                return TestResults(
                    phase=phase_name,
                    total_tests=json_results.get("summary", {}).get("total", 0),
                    passed=json_results.get("summary", {}).get("passed", 0),
                    failed=json_results.get("summary", {}).get("failed", 0),
                    skipped=json_results.get("summary", {}).get("skipped", 0),
                    errors=json_results.get("summary", {}).get("error", 0),
                    duration=duration,
                    success_rate=self._calculate_success_rate(json_results),
                )
            else:
                # Fallback to parsing pytest output
                return self._parse_pytest_output(result.stdout, phase_name, duration)

        except subprocess.TimeoutExpired:
            print(
                f"⏰ Phase {phase_name} timed out after {phase_config['timeout']} seconds"
            )
            return TestResults(
                phase=phase_name,
                total_tests=0,
                passed=0,
                failed=0,
                skipped=0,
                errors=1,
                duration=phase_config["timeout"],
                success_rate=0.0,
            )

        except Exception as e:
            print(f"💥 Error running phase {phase_name}: {e}")
            return TestResults(
                phase=phase_name,
                total_tests=0,
                passed=0,
                failed=0,
                skipped=0,
                errors=1,
                duration=0,
                success_rate=0.0,
            )

    def _parse_pytest_output(
        self, output: str, phase_name: str, duration: float
    ) -> TestResults:
        """Parse pytest output as fallback"""
        lines = output.split("\n")

        # Look for summary line
        summary_line = None
        for line in reversed(lines):
            if "passed" in line and ("failed" in line or "error" in line):
                summary_line = line
                break

        if summary_line:
            # Parse summary line like "5 passed, 2 failed, 1 skipped in 10.5s"
            import re

            pattern = r"(\d+) passed, (\d+) failed, (\d+) skipped, (\d+) error"
            match = re.search(pattern, summary_line)

            if match:
                passed = int(match.group(1))
                failed = int(match.group(2))
                skipped = int(match.group(3))
                errors = int(match.group(4))

                return TestResults(
                    phase=phase_name,
                    total_tests=passed + failed + skipped + errors,
                    passed=passed,
                    failed=failed,
                    skipped=skipped,
                    errors=errors,
                    duration=duration,
                    success_rate=(passed / (passed + failed + errors) * 100)
                    if (passed + failed + errors) > 0
                    else 0,
                )

        return TestResults(
            phase=phase_name,
            total_tests=0,
            passed=0,
            failed=0,
            skipped=0,
            errors=1,
            duration=duration,
            success_rate=0.0,
        )

    def _calculate_success_rate(self, json_results: Dict[str, Any]) -> float:
        """Calculate success rate from JSON results"""
        summary = json_results.get("summary", {})
        total = summary.get("total", 0)
        passed = summary.get("passed", 0)

        if total == 0:
            return 0.0

        return (passed / total) * 100

    def _print_phase_summary(self, phase_name: str, results: TestResults):
        """Print summary for a test phase"""
        status = (
            "✅ PASSED"
            if (results.failed == 0 and results.errors == 0)
            else "❌ FAILED"
        )

        print(f"Status: {status}")
        print(
            f"Tests: {results.total_tests} total, {results.passed} passed, {results.failed} failed, {results.skipped} skipped, {results.errors} errors"
        )
        print(f"Success Rate: {results.success_rate:.1f}%")
        print(f"Duration: {results.duration:.2f}s")

        if results.failed > 0 or results.errors > 0:
            print(f"⚠️  Phase {phase_name} requires attention!")

    def _generate_comprehensive_report(self):
        """Generate comprehensive HTML report with trend analysis"""
        report_data = {
            "metadata": {
                "system": "Integral Philosophy Publishing System",
                "test_run_date": self.start_time.isoformat()
                if self.start_time
                else datetime.now().isoformat(),
                "total_duration": (self.end_time - self.start_time).total_seconds()
                if self.start_time and self.end_time
                else 0,
                "test_framework_version": "1.0.0",
            },
            "summary": self._calculate_overall_summary(),
            "phases": [
                {
                    "name": result.phase,
                    "description": self.test_phases[result.phase]["description"],
                    "required": self.test_phases[result.phase]["required"],
                    "total_tests": result.total_tests,
                    "passed": result.passed,
                    "failed": result.failed,
                    "skipped": result.skipped,
                    "errors": result.errors,
                    "duration": result.duration,
                    "success_rate": result.success_rate,
                }
                for result in self.all_results
            ],
        }

        # Add performance trend analysis
        report_data["trend_analysis"] = self._analyze_performance_trends()

        # Add compliance status dashboard
        report_data["compliance_status"] = self._assess_compliance_status()

        # Add bottleneck detection
        report_data["bottlenecks"] = self._detect_bottlenecks()

        # Generate JSON report
        json_report_path = self.results_dir / "comprehensive_report.json"
        with open(json_report_path, "w") as f:
            json.dump(report_data, f, indent=2)

        # Generate HTML report
        html_report = self._generate_html_report(report_data)
        html_report_path = self.results_dir / "comprehensive_report.html"
        with open(html_report_path, "w") as f:
            f.write(html_report)

        # Generate summary text report
        summary_report = self._generate_text_summary(report_data)
        summary_report_path = self.results_dir / "summary.txt"
        with open(summary_report_path, "w") as f:
            f.write(summary_report)

        # Update dashboard data
        self._update_dashboard_data(report_data)

        print(f"\n📊 Reports generated in: {self.results_dir}")
        print(f"   📋 JSON Report: {json_report_path}")
        print(f"   🌐 HTML Report: {html_report_path}")
        print(f"   📝 Summary: {summary_report_path}")

    def _calculate_overall_summary(self) -> Dict[str, Any]:
        """Calculate overall summary statistics"""
        total_tests = sum(r.total_tests for r in self.all_results)
        total_passed = sum(r.passed for r in self.all_results)
        total_failed = sum(r.failed for r in self.all_results)
        total_skipped = sum(r.skipped for r in self.all_results)
        total_errors = sum(r.errors for r in self.all_results)
        total_duration = sum(r.duration for r in self.all_results)

        overall_success_rate = (
            (total_passed / total_tests * 100) if total_tests > 0 else 0
        )

        required_phases_passed = sum(
            1
            for r in self.all_results
            if self.test_phases[r.phase]["required"] and r.failed == 0 and r.errors == 0
        )

        total_required_phases = sum(
            1 for phase in self.test_phases.values() if phase["required"]
        )

        return {
            "total_tests": total_tests,
            "passed": total_passed,
            "failed": total_failed,
            "skipped": total_skipped,
            "errors": total_errors,
            "success_rate": overall_success_rate,
            "duration": total_duration,
            "required_phases_passed": required_phases_passed,
            "total_required_phases": total_required_phases,
            "all_required_passed": required_phases_passed == total_required_phases,
        }

    def _generate_text_summary(self, report_data: Dict[str, Any]) -> str:
        """Generate text summary report"""
        summary = report_data["summary"]

        output = f"""
INTEGRAL PHILOSOPHY PUBLISHING SYSTEM - TEST SUMMARY
==================================================

Test Run Date: {report_data["metadata"]["test_run_date"]}
Total Duration: {summary["duration"]:.2f} seconds

OVERALL RESULTS
---------------
Total Tests: {summary["total_tests"]}
Passed: {summary["passed"]}
Failed: {summary["failed"]}
Skipped: {summary["skipped"]}
Errors: {summary["errors"]}
Success Rate: {summary["success_rate"]:.1f}%

REQUIRED PHASES
---------------
Required Phases Passed: {summary["required_phases_passed"]}/{summary["total_required_phases"]}
All Required Passed: {"Yes" if summary["all_required_passed"] else "No"}

PHASE BREAKDOWN
---------------
"""

        for phase in report_data["phases"]:
            required = " (Required)" if phase["required"] else " (Optional)"
            status = (
                "PASSED" if phase["failed"] == 0 and phase["errors"] == 0 else "FAILED"
            )

            output += f"""
{phase["name"].upper()}{required}
Description: {phase["description"]}
Tests: {phase["total_tests"]} total, {phase["passed"]} passed, {phase["failed"]} failed, {phase["skipped"]} skipped, {phase["errors"]} errors
Success Rate: {phase["success_rate"]:.1f}%
Duration: {phase["duration"]:.2f}s
Status: {status}
{"-" * 50}
"""

        output += f"""
DEPLOYMENT STATUS
----------------
Ready for Deployment: {"Yes" if summary["all_required_passed"] else "No"}

Generated: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}
        """

        return output


def main():
    """Main entry point for the comprehensive test runner"""
    import argparse

    parser = argparse.ArgumentParser(
        description="Comprehensive Test Runner for Integral Philosophy Publishing System"
    )
    parser.add_argument(
        "--phases",
        nargs="+",
        choices=[
            "unit",
            "integration",
            "api",
            "functional",
            "security",
            "performance",
            "ui",
            "e2e",
            "docker",
            "compliance",
            "infrastructure",
        ],
        help="Specific test phases to run (default: all required phases)",
    )
    parser.add_argument(
        "--required-only", action="store_true", help="Run only required test phases"
    )
    parser.add_argument(
        "--list-phases", action="store_true", help="List all available test phases"
    )

    args = parser.parse_args()

    runner = ComprehensiveTestRunner()

    if args.list_phases:
        print("Available Test Phases:")
        print("=" * 50)
        for phase_name, phase_config in runner.test_phases.items():
            required = " (Required)" if phase_config["required"] else " (Optional)"
            print(f"{phase_name}: {phase_config['description']}{required}")
        return 0

    # Determine which phases to run
    if args.required_only:
        phases = [
            name for name, config in runner.test_phases.items() if config["required"]
        ]
    elif args.phases:
        phases = args.phases
    else:
        # Default to all phases
        phases = list(runner.test_phases.keys())

    # Run the tests
    success = runner.run_all_tests(phases)

    return 0 if success else 1


if __name__ == "__main__":
    sys.exit(main())
