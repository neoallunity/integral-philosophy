#!/usr/bin/env python3
# API Documentation Generator for Integral Philosophy Publishing System
# =================================================================

import os
import sys
import subprocess
import argparse
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


def generate_api_docs():
    """Generate API documentation using Doxygen."""
    print("🚀 Generating API documentation...")

    # Check if Doxygen is installed
    if not run_command("doxygen --version", "Checking Doxygen installation"):
        print("❌ Doxygen not found. Please install Doxygen:")
        print("   Ubuntu/Debian: sudo apt-get install doxygen")
        print("   macOS: brew install doxygen")
        print("   pip: pip install doxygen")
        return False

    # Run Doxygen
    return run_command("doxygen Doxyfile", "Generating API documentation")


def generate_user_docs():
    """Generate user documentation from markdown files."""
    print("📚 Generating user documentation...")

    docs_dir = Path("docs/user")
    if not docs_dir.exists():
        docs_dir.mkdir(parents=True, exist_ok=True)

    # Copy README to user docs
    readme_src = Path("docs/README.md")
    if readme_src.exists():
        readme_dst = docs_dir / "README.md"
        import shutil

        shutil.copy2(readme_src, readme_dst)
        print(f"✅ Copied README to {readme_dst}")

    return True


def generate_dev_docs():
    """Generate development documentation."""
    print("🛠️ Generating development documentation...")

    dev_docs_dir = Path("docs/development")
    if not dev_docs_dir.exists():
        dev_docs_dir.mkdir(parents=True, exist_ok=True)

    # Create development guide
    dev_guide = dev_docs_dir / "DEVELOPMENT.md"
    dev_content = """# Development Guide

## Build Systems

This project supports multiple build systems:

### SCons (Recommended)
```bash
./build-systems/scons/scons-helper init
./build-systems/scons/scons-helper build
```

### CMake
```bash
mkdir -p cmake-build && cd cmake-build
cmake ../build-systems/cmake/
make
```

### Autotools
```bash
cd build-systems/autotools/
./bootstrap
./configure
make
```

### Make
```bash
make -C build-systems/make/
```

## Project Structure

```
Integral Philosophy Publishing System/
├── build-systems/          # Build system configurations
│   ├── scons/             # SCons build system
│   ├── cmake/             # CMake build system
│   ├── autotools/         # Autotools build system
│   └── make/              # Traditional Make
├── haskell-project/       # Haskell source code
├── scripts/               # Utility scripts
├── config/                # Configuration files
├── examples/              # Example outputs
├── docs/                 # Documentation
│   ├── api/               # API documentation (Doxygen)
│   ├── user/              # User documentation
│   └── development/       # Development docs
├── tests/                 # Test files
└── legacy/                # Old/deprecated files
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test with all build systems
5. Submit a pull request

## Testing

```bash
# Run basic tests
./scripts/test_basic.py

# Run validation tests
./scripts/test_validator_structure.py

# Run with specific build system
./build-systems/scons/scons-helper test
```
"""

    with open(dev_guide, "w") as f:
        f.write(dev_content)

    print(f"✅ Created development guide: {dev_guide}")
    return True


def main():
    """Main function."""
    parser = argparse.ArgumentParser(
        description="Generate documentation for Integral Philosophy Publishing System",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    parser.add_argument(
        "--api", action="store_true", help="Generate API documentation (Doxygen)"
    )
    parser.add_argument(
        "--user", action="store_true", help="Generate user documentation"
    )
    parser.add_argument(
        "--dev", action="store_true", help="Generate development documentation"
    )
    parser.add_argument("--all", action="store_true", help="Generate all documentation")

    args = parser.parse_args()

    # Default to all if no specific option given
    if not any([args.api, args.user, args.dev]):
        args.all = True

    success = True

    if args.all or args.api:
        success &= generate_api_docs()

    if args.all or args.user:
        success &= generate_user_docs()

    if args.all or args.dev:
        success &= generate_dev_docs()

    if success:
        print("\n✅ Documentation generation completed!")
        print("📖 API docs: docs/api/html/index.html")
        print("📚 User docs: docs/user/")
        print("🛠️ Development docs: docs/development/")
    else:
        print("\n❌ Documentation generation failed!")
        sys.exit(1)


if __name__ == "__main__":
    main()
