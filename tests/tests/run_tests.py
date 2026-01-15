#!/usr/bin/env python3
"""
Test runner script for Integral Philosophy Publishing System tests
Provides convenient command-line interface for running different test categories
"""

import sys
import os
import subprocess
import argparse
import time
from pathlib import Path


def run_command(cmd: list, cwd: str = None) -> tuple:
    """Run command and return (return_code, stdout, stderr)"""
    try:
        result = subprocess.run(
            cmd,
            cwd=cwd or Path(__file__).parent.parent,
            capture_output=True,
            text=True,
            timeout=1800,  # 30 minutes timeout
        )
        return result.returncode, result.stdout, result.stderr
    except subprocess.TimeoutExpired:
        return -1, "", "Command timed out after 30 minutes"


def run_tests(test_type: str, args: list, verbose: bool = False) -> bool:
    """Run specific type of tests"""
    project_root = Path(__file__).parent.parent

    # Base pytest command
    cmd = ["python", "-m", "pytest", "tests/"]

    # Add verbosity
    if verbose:
        cmd.append("-v")

    # Test type mappings
    test_mappings = {
        "unit": ["-m", "unit"],
        "integration": ["-m", "integration"],
        "functional": ["-m", "functional"],
        "e2e": ["-m", "e2e"],
        "performance": ["-m", "performance"],
        "security": ["-m", "security"],
        "selenium": ["-m", "selenium"],
        "slow": ["-m", "slow"],
        "components": ["-m", "requires_components"],
        "all": [],
        "quick": ["-m", "not slow and not performance and not selenium"],
        "ci": ["-m", "not selenium", "--tb=short", "--disable-warnings"],
    }

    if test_type not in test_mappings:
        print(f"Unknown test type: {test_type}")
        print(f"Available types: {list(test_mappings.keys())}")
        return False

    # Add test markers
    cmd.extend(test_mappings[test_type])

    # Add additional arguments
    cmd.extend(args)

    print(f"Running: {' '.join(cmd)}")
    print("=" * 80)

    start_time = time.time()
    returncode, stdout, stderr = run_command(cmd)
    duration = time.time() - start_time

    print(stdout)
    if stderr:
        print("STDERR:", stderr)

    print("=" * 80)
    print(f"Tests completed in {duration:.2f} seconds with return code {returncode}")

    return returncode == 0


def run_coverage(args: list) -> bool:
    """Run tests with coverage"""
    cmd = [
        "python",
        "-m",
        "pytest",
        "tests/",
        "--cov=.",
        "--cov-report=html:tests/reports/htmlcov",
        "--cov-report=xml:tests/reports/coverage.xml",
        "--cov-report=term-missing",
        "--cov-fail-under=80",
    ]
    cmd.extend(args)

    print("Running tests with coverage analysis...")
    print("=" * 80)

    start_time = time.time()
    returncode, stdout, stderr = run_command(cmd)
    duration = time.time() - start_time

    print(stdout)
    if stderr:
        print("STDERR:", stderr)

    print("=" * 80)
    print(f"Coverage analysis completed in {duration:.2f} seconds")
    print("HTML coverage report available at: tests/reports/htmlcov/index.html")

    return returncode == 0


def run_parallel(args: list) -> bool:
    """Run tests in parallel"""
    cmd = ["python", "-m", "pytest", "tests/", "-n", "auto", "--dist=loadscope"]
    cmd.extend(args)

    print("Running tests in parallel...")
    print("=" * 80)

    start_time = time.time()
    returncode, stdout, stderr = run_command(cmd)
    duration = time.time() - start_time

    print(stdout)
    if stderr:
        print("STDERR:", stderr)

    print("=" * 80)
    print(f"Parallel tests completed in {duration:.2f} seconds")

    return returncode == 0


def setup_test_environment() -> bool:
    """Setup test environment"""
    project_root = Path(__file__).parent.parent

    print("Setting up test environment...")

    # Install test dependencies
    print("Installing test dependencies...")
    returncode, _, _ = run_command(
        ["python", "-m", "pip", "install", "-r", "tests/requirements.txt"]
    )

    if returncode != 0:
        print("Failed to install test dependencies")
        return False

    # Create necessary directories
    directories = [
        "tests/reports",
        "tests/reports/htmlcov",
        "tests/reports/screenshots",
        "tests/reports/logs",
        "api_jobs",
        "web_jobs",
    ]

    for directory in directories:
        Path(project_root / directory).mkdir(parents=True, exist_ok=True)

    print("Test environment setup complete!")
    return True


def cleanup_test_environment() -> bool:
    """Cleanup test environment"""
    project_root = Path(__file__).parent.parent

    print("Cleaning up test environment...")

    # Remove test artifacts
    artifacts = [
        "api_jobs",
        "web_jobs",
        "tests/reports",
        ".coverage",
        "coverage.xml",
        "pytest.xml",
        ".pytest_cache",
    ]

    for artifact in artifacts:
        artifact_path = project_root / artifact
        if artifact_path.exists():
            if artifact_path.is_dir():
                import shutil

                shutil.rmtree(artifact_path)
            else:
                artifact_path.unlink()
            print(f"Removed {artifact}")

    print("Test environment cleanup complete!")
    return True


def main():
    parser = argparse.ArgumentParser(
        description="Test runner for Integral Philosophy Publishing System"
    )
    parser.add_argument(
        "test_type",
        choices=[
            "unit",
            "integration",
            "functional",
            "e2e",
            "performance",
            "security",
            "selenium",
            "slow",
            "components",
            "all",
            "quick",
            "ci",
        ],
        help="Type of tests to run",
    )
    parser.add_argument(
        "--coverage", "-c", action="store_true", help="Run tests with coverage analysis"
    )
    parser.add_argument(
        "--parallel", "-p", action="store_true", help="Run tests in parallel"
    )
    parser.add_argument("--verbose", "-v", action="store_true", help="Verbose output")
    parser.add_argument(
        "--setup",
        "-s",
        action="store_true",
        help="Setup test environment before running tests",
    )
    parser.add_argument(
        "--cleanup",
        "-k",
        action="store_true",
        help="Cleanup test environment after running tests",
    )
    parser.add_argument(
        "--args",
        nargs=argparse.REMAINDER,
        help="Additional arguments to pass to pytest",
    )

    args = parser.parse_args()

    # Setup if requested
    if args.setup:
        if not setup_test_environment():
            sys.exit(1)

    # Build pytest arguments
    pytest_args = args.args or []

    try:
        success = True

        # Run with coverage if requested
        if args.coverage:
            success = run_coverage(pytest_args)
        # Run in parallel if requested
        elif args.parallel:
            success = run_parallel(pytest_args)
        # Run normal tests
        else:
            success = run_tests(args.test_type, pytest_args, args.verbose)

        # Cleanup if requested
        if args.cleanup:
            cleanup_test_environment()

        sys.exit(0 if success else 1)

    except KeyboardInterrupt:
        print("\nTest run interrupted by user")
        sys.exit(130)
    except Exception as e:
        print(f"Error running tests: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
