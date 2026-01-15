#!/usr/bin/env python3
# SCons utilities and helper functions
# ===================================

import os
import sys
import subprocess
import shutil
import json
from pathlib import Path


class Color:
    """ANSI color codes for output formatting."""

    RED = "\033[0;31m"
    GREEN = "\033[0;32m"
    YELLOW = "\033[1;33m"
    BLUE = "\033[0;34m"
    MAGENTA = "\033[0;35m"
    CYAN = "\033[0;36m"
    WHITE = "\033[0;37m"
    BOLD = "\033[1m"
    UNDERLINE = "\033[4m"
    NC = "\033[0m"  # No Color


class Logger:
    """Logging utility with colored output."""

    @staticmethod
    def info(message):
        print(f"{Color.BLUE}ℹ️  {message}{Color.NC}")

    @staticmethod
    def success(message):
        print(f"{Color.GREEN}✅ {message}{Color.NC}")

    @staticmethod
    def warning(message):
        print(f"{Color.YELLOW}⚠️  {message}{Color.NC}")

    @staticmethod
    def error(message):
        print(f"{Color.RED}❌ {message}{Color.NC}")

    @staticmethod
    def status(message):
        print(f"{Color.CYAN}🔧 {message}{Color.NC}")

    @staticmethod
    def debug(message):
        if os.environ.get("SCONS_DEBUG"):
            print(f"{Color.MAGENTA}🐛 {message}{Color.NC}")


class ToolChecker:
    """Utility to check for required tools."""

    def __init__(self, env):
        self.env = env
        self.logger = Logger()

    def check_tool(self, tool_name):
        """Check if a tool is available."""
        if not self.env.WhereIs(tool_name):
            self.logger.error(f"{tool_name} not found in PATH")
            return False
        return True

    def check_required_tools(self, tools):
        """Check multiple required tools."""
        missing_tools = []
        for tool in tools:
            if not self.check_tool(tool):
                missing_tools.append(tool)

        if missing_tools:
            self.logger.error(f"Missing required tools: {', '.join(missing_tools)}")
            self.logger.info(
                "Please install missing tools and ensure they're in your PATH"
            )
            return False

        self.logger.success("All required tools found")
        return True

    def get_tool_version(self, tool_name):
        """Get version of a tool."""
        try:
            result = subprocess.run(
                [tool_name, "--version"], capture_output=True, text=True, timeout=10
            )
            if result.returncode == 0:
                return result.stdout.strip()
        except (subprocess.TimeoutExpired, FileNotFoundError):
            pass
        return "Unknown"


class FileHelper:
    """File and directory utilities."""

    def __init__(self, logger=None):
        self.logger = logger or Logger()

    def ensure_dir(self, path):
        """Ensure directory exists."""
        try:
            os.makedirs(path, exist_ok=True)
            return True
        except Exception as e:
            self.logger.error(f"Failed to create directory {path}: {e}")
            return False

    def copy_file(self, src, dst):
        """Copy file with error handling."""
        try:
            shutil.copy2(src, dst)
            return True
        except Exception as e:
            self.logger.error(f"Failed to copy {src} to {dst}: {e}")
            return False

    def find_files(self, directory, pattern="*"):
        """Find files matching pattern in directory."""
        try:
            path = Path(directory)
            return list(path.rglob(pattern))
        except Exception as e:
            self.logger.error(f"Failed to find files in {directory}: {e}")
            return []

    def get_file_size(self, file_path):
        """Get file size in bytes."""
        try:
            return os.path.getsize(file_path)
        except Exception:
            return 0


class ProjectHelper:
    """Project-specific utilities."""

    def __init__(self, env, logger=None):
        self.env = env
        self.logger = logger or Logger()
        self.file_helper = FileHelper(logger)

    def verify_haskell_project(self):
        """Verify Haskell project structure."""
        haskell_dir = self.env["HASKELL_PROJECT_DIR"]

        if not os.path.exists(haskell_dir):
            self.logger.error(f"Haskell project directory not found: {haskell_dir}")
            return False

        required_files = ["package.yaml", "stack.yaml"]
        missing_files = []

        for file_name in required_files:
            file_path = os.path.join(haskell_dir, file_name)
            if not os.path.exists(file_path):
                missing_files.append(file_path)

        if missing_files:
            self.logger.error(f"Missing required files: {', '.join(missing_files)}")
            return False

        self.logger.success("Haskell project structure verified")
        return True

    def get_haskell_sources(self):
        """Get list of Haskell source files."""
        haskell_dir = self.env["HASKELL_PROJECT_DIR"]
        src_dir = os.path.join(haskell_dir, "src")

        if not os.path.exists(src_dir):
            self.logger.warning(f"Source directory not found: {src_dir}")
            return []

        sources = self.file_helper.find_files(src_dir, "*.hs")
        self.logger.info(f"Found {len(sources)} Haskell source files")
        return sources

    def create_test_file(self, test_file):
        """Create a test LaTeX file if it doesn't exist."""
        if os.path.exists(test_file):
            return True

        try:
            content = r"""\documentclass{article}
\title{Test Document}
\author{Test Author}
\begin{document}
\maketitle
This is a test LaTeX document for the Integral Philosophy Publishing System.
\end{document}
"""

            with open(test_file, "w") as f:
                f.write(content)

            self.logger.success(f"Created test file: {test_file}")
            return True
        except Exception as e:
            self.logger.error(f"Failed to create test file {test_file}: {e}")
            return False


class BuildHelper:
    """Build-related utilities."""

    def __init__(self, env, logger=None):
        self.env = env
        self.logger = logger or Logger()
        self.tool_checker = ToolChecker(env)
        self.project_helper = ProjectHelper(env, logger)

    def prepare_build(self):
        """Prepare for build."""
        self.logger.status("Preparing build environment...")

        # Check tools
        if not self.tool_checker.check_required_tools(
            [self.env["GHC"], self.env["STACK"]]
        ):
            return False

        # Verify project structure
        if not self.project_helper.verify_haskell_project():
            return False

        # Create directories
        file_helper = FileHelper(self.logger)
        if not file_helper.ensure_dir(self.env["BUILD_DIR"]):
            return False

        if not file_helper.ensure_dir(self.env["BIN_DIR"]):
            return False

        # Create test file
        if not self.project_helper.create_test_file(self.env["TEST_FILE"]):
            return False

        self.logger.success("Build preparation completed")
        return True

    def execute_command(self, command, description=""):
        """Execute shell command with error handling."""
        if description:
            self.logger.status(description)

        try:
            result = subprocess.run(command, shell=True, capture_output=True, text=True)

            if result.returncode == 0:
                if result.stdout and self.env["VERBOSE"]:
                    print(result.stdout)
                return True
            else:
                self.logger.error(f"Command failed: {command}")
                if result.stderr:
                    self.logger.error(f"Error output: {result.stderr}")
                return False
        except Exception as e:
            self.logger.error(f"Exception executing command: {e}")
            return False

    def build_with_stack(self):
        """Build project using Stack."""
        haskell_dir = self.env["HASKELL_PROJECT_DIR"]
        stack_flags = self.env["STACK_FLAGS"]

        build_cmd = f"cd {haskell_dir} && {self.env['STACK']} build {stack_flags}"
        return self.execute_command(build_cmd, "Building with Stack")

    def copy_executable(self):
        """Copy built executable to target location."""
        haskell_dir = self.env["HASKELL_PROJECT_DIR"]
        target_path = f"{self.env['BIN_DIR']}/integral-philosophy"
        file_helper = FileHelper(self.logger)

        # Try different possible locations of the built executable
        possible_paths = [
            f"{haskell_dir}/.stack-work/install/x86_64-linux-tinfo6/*/9.4.8/bin/integral-philosophy",
            f"{haskell_dir}/.stack-work/dist/x86_64-linux-tinfo6/*/build/integral-philosophy/integral-philosophy",
        ]

        for path_pattern in possible_paths:
            try:
                import glob

                matches = glob.glob(path_pattern)
                if matches:
                    src_path = matches[0]
                    if file_helper.copy_file(src_path, target_path):
                        self.logger.success(f"Executable copied to {target_path}")
                        return True
            except Exception:
                continue

        self.logger.error("Could not find built executable")
        return False


class TestHelper:
    """Testing utilities."""

    def __init__(self, env, logger=None):
        self.env = env
        self.logger = logger or Logger()
        self.build_helper = BuildHelper(env, logger)

    def run_basic_test(self):
        """Run basic functionality test."""
        executable = f"{self.env['BIN_DIR']}/integral-philosophy"

        if not os.path.exists(executable):
            self.logger.error("Executable not found")
            return False

        test_cmd = f"{executable} help"
        return self.build_helper.execute_command(test_cmd, "Running basic test")

    def run_validation_test(self):
        """Run LaTeX validation test."""
        executable = f"{self.env['BIN_DIR']}/integral-philosophy"
        test_file = self.env["TEST_FILE"]

        if not os.path.exists(test_file):
            self.logger.error(f"Test file not found: {test_file}")
            return False

        test_cmd = f"{executable} validate {test_file}"
        return self.build_helper.execute_command(test_cmd, "Running validation test")

    def run_all_tests(self):
        """Run all available tests."""
        self.logger.status("Running all tests...")

        tests = [
            ("Basic functionality", self.run_basic_test),
            ("LaTeX validation", self.run_validation_test),
        ]

        passed = 0
        total = len(tests)

        for test_name, test_func in tests:
            self.logger.status(f"Running {test_name}...")
            if test_func():
                self.logger.success(f"{test_name} passed")
                passed += 1
            else:
                self.logger.error(f"{test_name} failed")

        self.logger.info(f"Tests completed: {passed}/{total} passed")
        return passed == total


# Export utilities for use in SConscript files
def get_logger():
    """Get logger instance."""
    return Logger()


def get_tool_checker(env):
    """Get tool checker instance."""
    return ToolChecker(env)


def get_file_helper(logger=None):
    """Get file helper instance."""
    return FileHelper(logger)


def get_project_helper(env, logger=None):
    """Get project helper instance."""
    return ProjectHelper(env, logger)


def get_build_helper(env, logger=None):
    """Get build helper instance."""
    return BuildHelper(env, logger)


def get_test_helper(env, logger=None):
    """Get test helper instance."""
    return TestHelper(env, logger)


# Export all utility classes and functions
__all__ = [
    "Color",
    "Logger",
    "ToolChecker",
    "FileHelper",
    "ProjectHelper",
    "BuildHelper",
    "TestHelper",
    "get_logger",
    "get_tool_checker",
    "get_file_helper",
    "get_project_helper",
    "get_build_helper",
    "get_test_helper",
]
