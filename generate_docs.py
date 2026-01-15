#!/usr/bin/env python3
# Master Documentation Generator
# =============================

import os
import sys
import subprocess
from pathlib import Path


def main():
    """Main function."""
    print("📚 Generating documentation...")

    # Generate API docs
    api_cmd = "cd docs && python3 ../scripts/generate_docs.py --api"
    result = subprocess.run(api_cmd, shell=True)

    # Generate user docs
    user_cmd = "cd docs && python3 ../scripts/generate_docs.py --user"
    result = subprocess.run(user_cmd, shell=True)

    # Generate dev docs
    dev_cmd = "cd docs && python3 ../scripts/generate_docs.py --dev"
    result = subprocess.run(dev_cmd, shell=True)

    print("✅ Documentation generation completed!")
    print("📖 API docs: docs/api/html/index.html")
    print("📚 User docs: docs/user/")
    print("🛠️ Development docs: docs/development/")


if __name__ == "__main__":
    main()
