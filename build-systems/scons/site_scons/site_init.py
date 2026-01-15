#!/usr/bin/env python3
# SCons site_init.py
# ==================

# This file is automatically loaded by SCons and can be used
# to configure the SCons environment for this project.

import os
import sys

# Add site_scons directory to Python path
current_dir = os.path.dirname(os.path.abspath(__file__))
project_dir = os.path.dirname(current_dir)
site_scons_dir = os.path.join(project_dir, "site_scons")
if site_scons_dir not in sys.path:
    sys.path.insert(0, site_scons_dir)

# Try to import utilities
try:
    import utils

    print("ℹ️  SCons utilities loaded successfully")
except ImportError as e:
    print(f"Warning: Could not load SCons utilities: {e}")


def setup_environment(env):
    """Setup SCons environment with custom configurations."""

    # Configure default SCons behavior
    env.SetOption("implicit_cache", 1)  # Cache implicit dependencies
    env.SetOption("max_drift", 60)  # Maximum time drift for files

    # Add simple logging methods to environment
    env.AddMethod(lambda msg: print(f"ℹ️  {msg}"), "log_info")
    env.AddMethod(lambda msg: print(f"✅ {msg}"), "log_success")
    env.AddMethod(lambda msg: print(f"❌ {msg}"), "log_error")
    env.AddMethod(lambda msg: print(f"⚠️  {msg}"), "log_warning")

    return env


print("✅ SCons site_init.py loaded")
