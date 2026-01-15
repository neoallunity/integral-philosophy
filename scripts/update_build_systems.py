#!/usr/bin/env python3
# Update build systems for new directory structure
# ===========================================

import os
import sys
from pathlib import Path


def update_scons():
    """Update SCons build system paths."""
    print("🔧 Updating SCons build system...")

    scons_dir = Path("build-systems/scons")
    if not scons_dir.exists():
        print("❌ SCons directory not found")
        return False

    # Update SConstruct paths
    sconstruct_file = scons_dir / "SConstruct"
    if sconstruct_file.exists():
        content = sconstruct_file.read_text()

        # Update Haskell project path to work from build-systems/scons directory
        content = content.replace("'../haskell-project'", "'../../haskell-project'")

        # Update copy commands
        content = content.replace(
            'cp {env["HASKELL_PROJECT_DIR"]}/.stack-work/',
            'cp {env["HASKELL_PROJECT_DIR"]}/.stack-work/',
        )

        sconstruct_file.write_text(content)
        print("✅ Updated SConstruct")

    return True


def update_cmake():
    """Update CMake build system paths."""
    print("🔧 Updating CMake build system...")

    cmake_dir = Path("build-systems/cmake")
    if not cmake_dir.exists():
        print("❌ CMake directory not found")
        return False

    # Update CMakeLists.txt paths
    cmake_file = cmake_dir / "CMakeLists.txt"
    if cmake_file.exists():
        content = cmake_file.read_text()

        # Update Haskell project path
        content = content.replace(
            "${CMAKE_SOURCE_DIR}/haskell-project",
            "${CMAKE_SOURCE_DIR}/../../haskell-project",
        )

        cmake_file.write_text(content)
        print("✅ Updated CMakeLists.txt")

    return True


def update_autotools():
    """Update Autotools build system paths."""
    print("🔧 Updating Autotools build system...")

    autotools_dir = Path("build-systems/autotools")
    if not autotools_dir.exists():
        print("❌ Autotools directory not found")
        return False

    # Update configure.ac
    config_file = autotools_dir / "configure.ac"
    if config_file.exists():
        content = config_file.read_text()

        # Update Haskell project path
        content = content.replace("./haskell-project", "../../haskell-project")

        config_file.write_text(content)
        print("✅ Updated configure.ac")

    # Update Makefile.am
    makefile = autotools_dir / "Makefile.am"
    if makefile.exists():
        content = makefile.read_text()

        # Update Haskell project path
        content = content.replace("$(haskelldir)", "$(HASKELL_PROJECT_DIR)")

        makefile.write_text(content)
        print("✅ Updated Makefile.am")

    return True


def main():
    """Main update function."""
    print("🚀 Updating build systems for new directory structure...")

    success = True
    success &= update_scons()
    success &= update_cmake()
    success &= update_autotools()

    if success:
        print("\n✅ All build systems updated successfully!")
    else:
        print("\n❌ Some updates failed!")
        sys.exit(1)


if __name__ == "__main__":
    main()
