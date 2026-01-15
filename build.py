#!/usr/bin/env python3
# Master Build Script for Integral Philosophy Publishing System
# ========================================================

import os
import sys
import argparse
import subprocess
from pathlib import Path


def run_command(cmd, description=""):
    """Run a command with error handling."""
    print(f"🔧 {description}...")
    try:
        result = subprocess.run(
            cmd, shell=True, check=True, capture_output=True, text=True
        )
        if result.stdout:
            print(result.stdout)
        return True
    except subprocess.CalledProcessError as e:
        print(f"❌ Error: {e}")
        if e.stderr:
            print(f"Error output: {e.stderr}")
        return False


def build_with_scons(args):
    """Build using SCons."""
    cmd = "cd build-systems/scons && python3 scons-helper"
    if args.debug:
        cmd += " build --debug"
    else:
        cmd += " build"
    return run_command(cmd, "Building with SCons")


def build_with_cmake(args):
    """Build using CMake."""
    run_command("mkdir -p cmake-build", "Creating build directory")
    cmd = "cd cmake-build && cmake ../build-systems/cmake/ && make"
    return run_command(cmd, "Building with CMake")


def build_with_autotools(args):
    """Build using Autotools."""
    cmd = "cd build-systems/autotools && ./bootstrap && ./configure && make"
    return run_command(cmd, "Building with Autotools")


def build_with_make(args):
    """Build using traditional Make."""
    cmd = "make -C build-systems/make/"
    return run_command(cmd, "Building with Make")


def main():
    """Main function."""
    parser = argparse.ArgumentParser(
        description="Master build script for Integral Philosophy Publishing System",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    parser.add_argument(
        "--system",
        choices=["scons", "cmake", "autotools", "make"],
        default="scons",
        help="Build system to use (default: scons)",
    )
    parser.add_argument("--debug", action="store_true", help="Enable debug build")

    args = parser.parse_args()

    print("🚀 Integral Philosophy Publishing System - Master Build")

    success = False
    if args.system == "scons":
        success = build_with_scons(args)
    elif args.system == "cmake":
        success = build_with_cmake(args)
    elif args.system == "autotools":
        success = build_with_autotools(args)
    elif args.system == "make":
        success = build_with_make(args)

    if success:
        print("✅ Build completed successfully!")
    else:
        print("❌ Build failed!")
        sys.exit(1)


if __name__ == "__main__":
    main()
