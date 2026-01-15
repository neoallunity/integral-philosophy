# Development Guide

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
