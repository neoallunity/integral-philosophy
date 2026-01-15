#!/usr/bin/env python3
"""
Phase 2 UI Test Runner for Integral Philosophy Publishing System
Comprehensive UI testing execution with detailed reporting
"""

import os
import sys
import time
import json
import argparse
import subprocess
from pathlib import Path
from typing import Dict, List, Any, Optional

# Add project root to Python path
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))


class UITestRunner:
    """Comprehensive UI test runner for Phase 2 testing"""

    def __init__(
        self, base_url: str = "http://localhost:5000", output_dir: str = "test_reports"
    ):
        self.base_url = base_url
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(exist_ok=True)

        self.test_categories = {
            "web_interface": {
                "path": "tests/ui/web_interface/test_user_interface.py",
                "description": "Web Interface Testing - Page rendering, forms, navigation, dynamic content",
                "pytest_args": [
                    "-v",
                    "--tb=short",
                    "--html=reports/web_interface_report.html",
                    "--self-contained-html",
                ],
            },
            "accessibility": {
                "path": "tests/ui/accessibility/test_wcag_compliance.py",
                "description": "WCAG 2.1 AA Compliance Testing - Screen readers, keyboard navigation, color contrast",
                "pytest_args": [
                    "-v",
                    "--tb=short",
                    "--html=reports/accessibility_report.html",
                    "--self-contained-html",
                ],
            },
            "visual_regression": {
                "path": "tests/ui/visual/test_visual_regression.py",
                "description": "Visual Regression Testing - Screenshot comparison, layout consistency, responsive design",
                "pytest_args": [
                    "-v",
                    "--tb=short",
                    "--html=reports/visual_report.html",
                    "--self-contained-html",
                ],
            },
        }

        self.results = {
            "timestamp": time.strftime("%Y-%m-%d %H:%M:%S"),
            "base_url": base_url,
            "categories": {},
            "summary": {
                "total_tests": 0,
                "passed": 0,
                "failed": 0,
                "errors": 0,
                "skipped": 0,
                "duration": 0,
            },
        }

    def setup_test_environment(self):
        """Setup test environment"""
        print("Setting up UI test environment...")

        # Create necessary directories
        directories = [
            "test_screenshots/baseline",
            "test_screenshots/current",
            "test_screenshots/diffs",
            "reports",
            "logs",
        ]

        for directory in directories:
            Path(directory).mkdir(parents=True, exist_ok=True)

        # Check if test server is running
        print(f"Checking if test server is running at {self.base_url}...")
        try:
            import requests

            response = requests.get(self.base_url, timeout=5)
            if response.status_code == 200:
                print("✓ Test server is running")
            else:
                print(f"⚠ Test server returned status {response.status_code}")
        except Exception as e:
            print(f"✗ Cannot connect to test server: {e}")
            print("Please ensure test server is running before executing UI tests")
            return False

        return True

    def run_test_category(
        self, category_name: str, test_config: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Run a specific test category"""
        print(f"\n{'=' * 60}")
        print(f"Running {category_name.upper()} Tests")
        print(f"Description: {test_config['description']}")
        print(f"{'=' * 60}")

        start_time = time.time()
        test_file = test_config["path"]

        # Check if test file exists
        if not Path(test_file).exists():
            return {
                "status": "error",
                "message": f"Test file not found: {test_file}",
                "duration": 0,
                "output": "",
            }

        # Prepare pytest command
        pytest_cmd = [
            sys.executable,
            "-m",
            "pytest",
            test_file,
            f"--base-url={self.base_url}",
            f"--junit-xml=reports/{category_name}_junit.xml",
        ]

        # Add category-specific arguments
        if "pytest_args" in test_config:
            pytest_cmd.extend(test_config["pytest_args"])

        try:
            # Run tests
            print(f"Executing: {' '.join(pytest_cmd)}")
            result = subprocess.run(
                pytest_cmd,
                capture_output=True,
                text=True,
                timeout=600,  # 10 minute timeout
            )

            duration = time.time() - start_time

            # Parse results
            test_results = self._parse_pytest_output(result.stdout, result.stderr)
            test_results["command"] = " ".join(pytest_cmd)
            test_results["return_code"] = result.returncode
            test_results["stdout"] = result.stdout
            test_results["stderr"] = result.stderr
            test_results["duration"] = duration

            # Print summary
            print(f"\n{category_name.title()} Test Results:")
            print(f"  Passed: {test_results['passed']}")
            print(f"  Failed: {test_results['failed']}")
            print(f"  Errors: {test_results['errors']}")
            print(f"  Skipped: {test_results['skipped']}")
            print(f"  Duration: {duration:.2f}s")

            if result.returncode != 0:
                print(f"⚠ {category_name} tests completed with issues")
            else:
                print(f"✓ {category_name} tests completed successfully")

            return test_results

        except subprocess.TimeoutExpired:
            return {
                "status": "timeout",
                "message": f"Tests timed out after 10 minutes",
                "duration": time.time() - start_time,
                "output": "",
            }

        except Exception as e:
            return {
                "status": "error",
                "message": f"Failed to run tests: {e}",
                "duration": time.time() - start_time,
                "output": "",
            }

    def _parse_pytest_output(self, stdout: str, stderr: str) -> Dict[str, Any]:
        """Parse pytest output to extract test counts"""
        results = {
            "status": "completed",
            "passed": 0,
            "failed": 0,
            "errors": 0,
            "skipped": 0,
            "total": 0,
        }

        # Parse from stdout
        for line in stdout.split("\n"):
            if "=" in line and "passed" in line.lower():
                # Example: "5 passed, 2 failed, 1 skipped in 10.5s"
                parts = line.split("=")[1].strip() if "=" in line else line
                for part in parts.split(","):
                    part = part.strip()
                    if "passed" in part:
                        results["passed"] = int(part.split()[0])
                    elif "failed" in part:
                        results["failed"] = int(part.split()[0])
                    elif "error" in part:
                        results["errors"] = int(part.split()[0])
                    elif "skipped" in part:
                        results["skipped"] = int(part.split()[0])

        results["total"] = (
            results["passed"]
            + results["failed"]
            + results["errors"]
            + results["skipped"]
        )

        return results

    def run_all_tests(self):
        """Run all UI test categories"""
        print("\n" + "=" * 80)
        print("PHASE 2 UI TESTING - INTEGRAL PHILOSOPHY PUBLISHING SYSTEM")
        print("=" * 80)

        if not self.setup_test_environment():
            return False

        overall_start = time.time()

        for category_name, test_config in self.test_categories.items():
            category_results = self.run_test_category(category_name, test_config)
            self.results["categories"][category_name] = category_results

            # Update overall summary
            self.results["summary"]["total_tests"] += category_results.get("total", 0)
            self.results["summary"]["passed"] += category_results.get("passed", 0)
            self.results["summary"]["failed"] += category_results.get("failed", 0)
            self.results["summary"]["errors"] += category_results.get("errors", 0)
            self.results["summary"]["skipped"] += category_results.get("skipped", 0)

        self.results["summary"]["duration"] = time.time() - overall_start

        # Generate comprehensive report
        self.generate_report()

        # Print final summary
        self.print_summary()

        return (
            self.results["summary"]["failed"] == 0
            and self.results["summary"]["errors"] == 0
        )

    def run_category(self, category_name: str):
        """Run a specific test category"""
        if category_name not in self.test_categories:
            print(f"Unknown test category: {category_name}")
            print(f"Available categories: {list(self.test_categories.keys())}")
            return False

        test_config = self.test_categories[category_name]
        category_results = self.run_test_category(category_name, test_config)
        self.results["categories"][category_name] = category_results

        self.generate_report()

        return category_results.get("status") != "error"

    def generate_report(self):
        """Generate comprehensive HTML and JSON reports"""
        print("\nGenerating comprehensive reports...")

        # Generate JSON report
        json_report_path = self.output_dir / "ui_test_report.json"
        with open(json_report_path, "w") as f:
            json.dump(self.results, f, indent=2)

        # Generate HTML report
        html_report_path = self.output_dir / "ui_test_report.html"
        self._generate_html_report(html_report_path)

        print(f"✓ Reports generated:")
        print(f"  JSON: {json_report_path}")
        print(f"  HTML: {html_report_path}")

    def _generate_html_report(self, output_path: Path):
        """Generate HTML report"""
        html_content = f"""
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Phase 2 UI Test Report - Integral Philosophy Publishing System</title>
    <style>
        body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; margin: 0; padding: 20px; background: #f5f5f5; }}
        .container {{ max-width: 1200px; margin: 0 auto; background: white; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }}
        .header {{ background: #2c3e50; color: white; padding: 30px; border-radius: 8px 8px 0 0; }}
        .header h1 {{ margin: 0; font-size: 28px; }}
        .header p {{ margin: 10px 0 0 0; opacity: 0.9; }}
        .summary {{ padding: 30px; display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; }}
        .metric {{ background: #f8f9fa; padding: 20px; border-radius: 6px; text-align: center; border-left: 4px solid #007bff; }}
        .metric h3 {{ margin: 0; font-size: 32px; color: #2c3e50; }}
        .metric p {{ margin: 5px 0 0 0; color: #6c757d; text-transform: uppercase; font-size: 12px; letter-spacing: 1px; }}
        .metric.failed {{ border-left-color: #dc3545; }}
        .metric.passed {{ border-left-color: #28a745; }}
        .metric.errors {{ border-left-color: #ffc107; }}
        .categories {{ padding: 0 30px 30px; }}
        .category {{ margin-bottom: 30px; border: 1px solid #dee2e6; border-radius: 6px; overflow: hidden; }}
        .category-header {{ background: #f8f9fa; padding: 20px; border-bottom: 1px solid #dee2e6; }}
        .category-header h2 {{ margin: 0; color: #2c3e50; }}
        .category-header p {{ margin: 5px 0 0 0; color: #6c757d; }}
        .category-content {{ padding: 20px; }}
        .status-badge {{ display: inline-block; padding: 4px 8px; border-radius: 4px; font-size: 12px; font-weight: bold; text-transform: uppercase; }}
        .status-passed {{ background: #d4edda; color: #155724; }}
        .status-failed {{ background: #f8d7da; color: #721c24; }}
        .status-error {{ background: #fff3cd; color: #856404; }}
        .status-timeout {{ background: #f8d7da; color: #721c24; }}
        pre {{ background: #f8f9fa; padding: 15px; border-radius: 4px; overflow-x: auto; font-size: 12px; }}
        .footer {{ padding: 20px; text-align: center; color: #6c757d; font-size: 14px; }}
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>Phase 2 UI Test Report</h1>
            <p>Integral Philosophy Publishing System - {self.results["timestamp"]}</p>
            <p>Base URL: {self.results["base_url"]}</p>
        </div>
        
        <div class="summary">
            <div class="metric">
                <h3>{self.results["summary"]["total_tests"]}</h3>
                <p>Total Tests</p>
            </div>
            <div class="metric passed">
                <h3>{self.results["summary"]["passed"]}</h3>
                <p>Passed</p>
            </div>
            <div class="metric failed">
                <h3>{self.results["summary"]["failed"]}</h3>
                <p>Failed</p>
            </div>
            <div class="metric errors">
                <h3>{self.results["summary"]["errors"]}</h3>
                <p>Errors</p>
            </div>
            <div class="metric">
                <h3>{self.results["summary"]["duration"]:.2f}s</h3>
                <p>Duration</p>
            </div>
        </div>
        
        <div class="categories">
"""

        for category_name, category_results in self.results["categories"].items():
            status = category_results.get("status", "unknown")
            status_class = f"status-{status}"

            html_content += f"""
            <div class="category">
                <div class="category-header">
                    <h2>{category_name.title().replace("_", " ")}
                        <span class="status-badge {status_class}">{status}</span>
                    </h2>
                    <p>{self.test_categories[category_name]["description"]}</p>
                </div>
                <div class="category-content">
"""

            if status in ["completed", "timeout"]:
                if "passed" in category_results:
                    html_content += f"""
                    <p><strong>Results:</strong> {category_results.get("passed", 0)} passed, 
                    {category_results.get("failed", 0)} failed, 
                    {category_results.get("errors", 0)} errors, 
                    {category_results.get("skipped", 0)} skipped</p>
                    <p><strong>Duration:</strong> {category_results.get("duration", 0):.2f} seconds</p>
"""

            if status == "error":
                html_content += f"""
                    <p><strong>Error:</strong> {category_results.get("message", "Unknown error")}</p>
"""

            if category_results.get("stdout"):
                html_content += f"""
                    <h4>Output:</h4>
                    <pre>{category_results["stdout"]}</pre>
"""

            if category_results.get("stderr"):
                html_content += f"""
                    <h4>Errors:</h4>
                    <pre>{category_results["stderr"]}</pre>
"""

            html_content += """
                </div>
            </div>
"""

        html_content += f"""
        </div>
        
        <div class="footer">
            <p>Generated by Phase 2 UI Test Runner for Integral Philosophy Publishing System</p>
        </div>
    </div>
</body>
</html>
"""

        with open(output_path, "w") as f:
            f.write(html_content)

    def print_summary(self):
        """Print final test summary"""
        print("\n" + "=" * 80)
        print("FINAL TEST SUMMARY")
        print("=" * 80)

        summary = self.results["summary"]
        print(f"Total Tests: {summary['total_tests']}")
        print(f"Passed: {summary['passed']}")
        print(f"Failed: {summary['failed']}")
        print(f"Errors: {summary['errors']}")
        print(f"Skipped: {summary['skipped']}")
        print(f"Duration: {summary['duration']:.2f}s")

        success_rate = (
            (summary["passed"] / summary["total_tests"] * 100)
            if summary["total_tests"] > 0
            else 0
        )
        print(f"Success Rate: {success_rate:.1f}%")

        if summary["failed"] == 0 and summary["errors"] == 0:
            print("\n✓ All UI tests passed!")
        else:
            print(
                f"\n⚠ {summary['failed'] + summary['errors']} tests failed or had errors"
            )

        print("=" * 80)


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(description="Phase 2 UI Test Runner")
    parser.add_argument(
        "--base-url",
        default="http://localhost:5000",
        help="Base URL for testing (default: http://localhost:5000)",
    )
    parser.add_argument(
        "--category",
        choices=["web_interface", "accessibility", "visual_regression"],
        help="Run specific test category",
    )
    parser.add_argument(
        "--output-dir",
        default="test_reports",
        help="Output directory for reports (default: test_reports)",
    )
    parser.add_argument(
        "--list-categories", action="store_true", help="List available test categories"
    )

    args = parser.parse_args()

    runner = UITestRunner(args.base_url, args.output_dir)

    if args.list_categories:
        print("Available test categories:")
        for name, config in runner.test_categories.items():
            print(f"  {name}: {config['description']}")
        return

    if args.category:
        success = runner.run_category(args.category)
    else:
        success = runner.run_all_tests()

    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
