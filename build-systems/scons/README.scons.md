# SCons README for Integral Philosophy Publishing System
# =================================================

This directory contains the SCons-based build system for the Integral Philosophy Publishing System.

## Quick Start

```bash
# Initialize the build system
./scons-helper init

# Configure the project
./scons-helper configure

# Build the project
./scons-helper build

# Run tests
./scons-helper test

# Show help
./scons-helper help
```

## Available Commands

### Using the Helper Script

The `scons-helper` script provides a convenient interface:

```bash
# Initialize build system
./scons-helper init

# Configure with options
./scons-helper configure --debug --verbose

# Build project
./scons-helper build
./scons-helper build --debug
./scons-helper build --dependencies

# Run tests
./scons-helper test
./scons-helper test --type validation
./scons-helper test --type analysis

# Clean build
./scons-helper clean
./scons-helper clean --all

# Show information
./scons-helper info
```

### Using SCons Directly

You can also use SCons directly:

```bash
# Basic build
scons

# Debug build
scons DEBUG=1

# Verbose output
scons VERBOSE=1

# Enable profiling
scons PROFILE=1

# Custom test file
scons TEST_FILE=custom.tex

# Show help
scons help

# Run specific targets
scons build
scons test
scons validate
scons analyze
scons reconstruct
scons demo
scons check

# Build information
scons info
scons version
scons syntax-check

# Dependencies
scons deps
scons check-deps
scons update-deps

# Documentation
scons docs

# Performance analysis
scons perf

# Package creation
scons package

# CI build
scons ci
```

## Build Options

### Configuration Variables

- `BUILD_DIR`: Build directory (default: `build`)
- `BIN_DIR`: Binary directory (default: `bin`)
- `HASKELL_PROJECT_DIR`: Haskell project directory (default: `haskell-project`)
- `GHC`: GHC compiler (default: `ghc`)
- `STACK`: Stack build tool (default: `stack`)
- `GHC_FLAGS`: GHC compilation flags (default: `-Wall -O2 -threaded`)
- `STACK_FLAGS`: Stack build flags (default: ``)
- `TEST_FILE`: Test LaTeX file (default: `test.tex`)

### Boolean Options

- `DEBUG`: Enable debug build (default: `False`)
- `OPTIMIZE`: Enable optimization (default: `True`)
- `VERBOSE`: Verbose output (default: `False`)
- `PROFILE`: Enable profiling (default: `False`)

## Available Targets

### Core Targets

- `build`: Build the integral-philosophy executable
- `test`: Run basic functionality tests
- `validate`: Validate LaTeX files
- `analyze`: Analyze LaTeX files
- `reconstruct`: Reconstruct LaTeX files
- `run`: Run the executable
- `demo`: Run system demo
- `check`: Run comprehensive checks

### Development Targets

- `deps`: Install Haskell dependencies
- `syntax-check`: Check Haskell syntax
- `stack-test`: Run Stack tests
- `docs`: Build Haddock documentation
- `profile`: Build profiling version
- `benchmark`: Run benchmarks

### Maintenance Targets

- `clean`: Clean build artifacts
- `clean-stack`: Clean Stack build artifacts
- `info`: Show build information
- `version`: Show GHC version
- `help`: Show available commands

### Advanced Targets

- `perf`: Performance analysis
- `package`: Create installation package
- `ci`: Continuous integration build
- `summary`: Create build summary
- `verify`: Verify build configuration

## File Structure

```
.
├── SConstruct                  # Main SCons build file
├── SConscript                  # Haskell sources configuration
├── SConscript-config           # Build configuration utilities
├── scons-helper                # Helper script
├── site_scons/                # Site-specific SCons modules
│   ├── site_init.py           # SCons initialization
│   └── utils.py              # Utility functions
└── haskell-project/           # Haskell project source
    ├── package.yaml
    ├── stack.yaml
    └── src/
```

## Integration with CI/CD

### GitHub Actions Example

```yaml
name: Build with SCons
on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    
    - name: Install dependencies
      run: |
        sudo apt-get update
        sudo apt-get install -y scons ghc stack
    
    - name: Build with SCons
      run: |
        ./scons-helper init
        ./scons-helper configure
        ./scons-helper build
    
    - name: Run tests
      run: ./scons-helper test
    
    - name: CI build
      run: scons ci
```

### Custom Build Scripts

Create custom build scripts using the SCons API:

```python
# my_build_script.py
import SCons.Environment

def custom_build():
    env = SCons.Environment.Environment()
    
    # Load project configuration
    env.SConscript('SConscript')
    
    # Custom build steps
    env.Command('my_target', [], 'echo "Custom build step"')
    
    return env

if __name__ == '__main__':
    custom_build()
```

## Troubleshooting

### Common Issues

1. **SCons not found**
   ```bash
   # Install SCons
   pip install scons
   # or
   sudo apt-get install scons
   ```

2. **Haskell tools not found**
   ```bash
   # Install Stack
   curl -sSL https://get.haskellstack.org/ | sh
   
   # Install GHC
   stack setup
   ```

3. **Build failures**
   ```bash
   # Clean and rebuild
   scons clean
   scons deps
   scons build
   ```

4. **Missing test file**
   ```bash
   # Create test file
   scons TEST_FILE=my_test.tex validate
   ```

### Debug Mode

Enable debug mode for detailed output:

```bash
scons DEBUG=1 VERBOSE=1 build
```

### Verbose Stack Output

```bash
scons STACK_FLAGS=--verbose build
```

## Performance Optimization

### Parallel Builds

SCons automatically detects parallel build capabilities:

```bash
# Explicit parallel build
scons -j4  # Use 4 parallel jobs
```

### Incremental Builds

SCons tracks dependencies and only rebuilds what's necessary:

```bash
# First build
scons build

# Incremental build (only rebuilds changed files)
scons build
```

### Profiling

Enable profiling for performance analysis:

```bash
scons PROFILE=1 build
scons perf
```

## Extension Points

### Custom Builders

Add custom builders in `site_scons/utils.py`:

```python
def custom_builder(env, target, source):
    # Custom build logic
    return target

# Register in SConscript
env['BUILDERS']['Custom'] = Builder(action=custom_builder)
```

### Custom Commands

Add custom commands to the helper script:

```python
def custom_command(args):
    print("Custom command logic")
    return True

# Add to argument parser in scons-helper
custom_parser = subparsers.add_parser('custom', help='Custom command')
```

## Contributing

When modifying the SCons build system:

1. Test with `./scons-helper init`
2. Verify configuration with `./scons-helper configure`
3. Build with `./scons-helper build`
4. Run tests with `./scons-helper test`
5. Check CI with `scons ci`

## Comparison with Other Build Systems

| Feature | SCons | Make | CMake | Autotools |
|---------|-------|------|-------|-----------|
| Auto-dependencies | ✅ | ❌ | ✅ | ✅ |
| Cross-platform | ✅ | ⚠️ | ✅ | ✅ |
| Python scripting | ✅ | ❌ | ⚠️ | ❌ |
| Incremental builds | ✅ | ✅ | ✅ | ✅ |
| Configuration system | ✅ | ❌ | ✅ | ✅ |
| Package management | ⚠️ | ❌ | ⚠️ | ✅ |

SCons provides the best balance of flexibility, cross-platform support, and Python integration for this Haskell project.