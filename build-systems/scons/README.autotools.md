# Autotools README for Integral Philosophy Publishing System
# ========================================================

This directory contains the Autotools-based build system for the Integral Philosophy Publishing System.

## Quick Start

```bash
# Initialize the build system
./bootstrap

# Configure the project
./configure

# Build the project
make

# Install (optional)
sudo make install
```

## Available Commands

### Bootstrap
```bash
./bootstrap
```
Initializes the autotools build system. Run this after cloning the repository or after modifying:
- `configure.ac`
- Any `Makefile.am` file

### Configure
```bash
./configure [OPTIONS]
```
Configures the build system. Common options:

```bash
./configure --help                    # Show all options
./configure --enable-debug             # Enable debug build
./configure --with-test-file=FILE      # Set custom test file
./configure --prefix=/usr/local        # Set installation prefix
```

### Build Targets
```bash
make                # Build everything (default)
make build          # Build the executable
make test           # Run tests
make validate       # Validate LaTeX files
make reconstruct    # Reconstruct LaTeX files
make analyze        # Analyze LaTeX files
make run            # Run the executable
make demo           # Run demo
make dev            # Development workflow (build + test)
```

### Maintenance
```bash
make clean          # Clean build artifacts
make distclean      # Clean all generated files
make deps           # Install Haskell dependencies
make syntax-check   # Check Haskell syntax
make info           # Show build information
make version        # Show GHC version
```

### Installation
```bash
make install        # Install the executable
make uninstall      # Uninstall the executable
make release        # Build release version
```

### Testing
```bash
make check          # Run comprehensive checks
make help           # Show all available targets
```

## Using the Autotools Helper Script

The `autotools` script provides convenient shortcuts:

```bash
# Initialize build system
./autotools bootstrap

# Configure with options
./autotools configure --enable-debug

# Clean build
./autotools clean

# Create distribution
./autotools dist

# Install
./autotools install

# Run tests
./autotools check
```

## File Structure

```
.
├── configure.ac          # Autoconf configuration
├── Makefile.am           # Main Automake makefile
├── src/
│   └── Makefile.am       # Haskell source makefile
├── build-aux/
│   └── Makefile.am       # Auxiliary files makefile
├── m4/                   # Autoconf macros (created by bootstrap)
├── bootstrap             # Bootstrap script
├── autotools             # Helper script
└── haskell-project/      # Haskell project source
```

## Dependencies

### Build Dependencies
- `autoconf` (>= 2.69)
- `automake` (>= 1.15)
- `libtool` (optional)

### Runtime Dependencies
- `ghc` (Glasgow Haskell Compiler)
- `stack` (Haskell build tool)

### Installing Dependencies

#### Ubuntu/Debian
```bash
sudo apt-get install autotools-dev autoconf automake libtool
```

#### CentOS/RHEL/Fedora
```bash
sudo yum install autoconf automake libtool
# or
sudo dnf install autoconf automake libtool
```

#### macOS
```bash
brew install autoconf automake libtool
```

#### Haskell Tools
```bash
# Install Stack
curl -sSL https://get.haskellstack.org/ | sh

# Or install GHC and Stack via package manager
# Ubuntu/Debian
sudo apt-get install ghc stack
# macOS
brew install ghc stack
```

## Configuration Options

### Debug Build
```bash
./configure --enable-debug
```
Enables debug information and disables optimizations.

### Optimization
```bash
./configure --enable-optimization
```
Enables optimizations (default: enabled).

### Custom Test File
```bash
./configure --with-test-file=custom-test.tex
```
Sets a custom LaTeX file for testing validation functions.

### Installation Prefix
```bash
./configure --prefix=/opt/integral-philosophy
```
Sets the installation directory (default: `/usr/local`).

## Troubleshooting

### Bootstrap Fails
```bash
# Install missing autotools
sudo apt-get install autotools-dev autoconf automake

# Or on macOS
brew install autoconf automake libtool
```

### Configure Fails
```bash
# Check for required tools
which ghc stack

# Install Haskell tools
curl -sSL https://get.haskellstack.org/ | sh
```

### Build Fails
```bash
# Clean and rebuild
make clean
make deps
make
```

### Tests Fail
```bash
# Check test file exists
ls -la test.tex

# Or create custom test file
./autotools configure --with-test-file=my-test.tex
```

## Distribution

Create a source distribution:

```bash
./autotools bootstrap
./autotools configure
./autotools dist
```

This creates a tarball: `integral-philosophy-0.1.0.0.tar.gz`

## Integration with CI/CD

The autotools build system integrates well with CI/CD pipelines:

```bash
#!/bin/bash
set -e

# Initialize
./bootstrap

# Configure
./configure --enable-debug

# Build
make

# Test
make check

# Install (if needed)
make install DESTDIR=$PWD/install
```

## Contributing

When modifying the build system:

1. Update `configure.ac` for new configuration options
2. Update `Makefile.am` for new build targets
3. Run `./bootstrap` to regenerate files
4. Test with `./autotools configure && make && make check`
5. Commit both source files and generated files