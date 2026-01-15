cmake_minimum_required(VERSION 3.16)
project(IntegralPhilosophy VERSION 0.1.0.0 LANGUAGES NONE)

# Set C/C++ standard (not used for Haskell but required by CMake)
set(CMAKE_CXX_STANDARD 17)

# Custom variables
set(BUILD_DIR ${CMAKE_BINARY_DIR}/build)
set(BIN_DIR ${CMAKE_BINARY_DIR}/bin)
set(HASKELL_PROJECT_DIR ${CMAKE_SOURCE_DIR}/../../haskell-project)

# Find required tools
find_program(GHC_EXECUTABLE ghc)
find_program(STACK_EXECUTABLE stack)

if(NOT GHC_EXECUTABLE)
    message(FATAL_ERROR "GHC not found. Please install GHC.")
endif()

if(NOT STACK_EXECUTABLE)
    message(FATAL_ERROR "Stack not found. Please install Stack.")
endif()

# Directory setup
file(MAKE_DIRECTORY ${BUILD_DIR})
file(MAKE_DIRECTORY ${BIN_DIR})

# Main target: build the integral-philosophy executable
add_custom_target(build-all
    DEPENDS integral-philosophy
    COMMENT "Building Integral Philosophy Publishing System"
)

add_custom_target(build-target
    DEPENDS integral-philosophy
    COMMENT "Building integral-philosophy"
)

# Core build target
add_custom_command(
    OUTPUT ${BIN_DIR}/integral-philosophy
    COMMAND ${CMAKE_COMMAND} -E echo "🔨 Building Haskell Integral Philosophy Publishing System..."
    COMMAND ${CMAKE_COMMAND} -E chdir ${HASKELL_PROJECT_DIR} ${STACK_EXECUTABLE} build --fast
    COMMAND ${CMAKE_COMMAND} -E copy_if_different 
        ${HASKELL_PROJECT_DIR}/.stack-work/install/x86_64-linux-tinfo6/*/9.4.8/bin/integral-philosophy
        ${BIN_DIR}/integral-philosophy
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${HASKELL_PROJECT_DIR}/.stack-work/dist/x86_64-linux-tinfo6/*/build/integral-philosophy/integral-philosophy
        ${BIN_DIR}/integral-philosophy
    COMMENT "Building integral-philosophy executable"
    VERBATIM
)

add_custom_target(integral-philosophy
    DEPENDS ${BIN_DIR}/integral-philosophy
    COMMENT "Integral Philosophy executable built"
)

# Testing targets
add_custom_target(test
    COMMAND ${CMAKE_COMMAND} -E echo "🧪 Running tests..."
    COMMAND ${BIN_DIR}/integral-philosophy help
    DEPENDS integral-philosophy
    COMMENT "Running tests"
)

add_custom_target(validate
    COMMAND ${CMAKE_COMMAND} -E echo "✅ Validating LaTeX files..."
    COMMAND ${BIN_DIR}/integral-philosophy validate test.tex
    DEPENDS integral-philosophy
    COMMENT "Validating LaTeX files"
)

add_custom_target(reconstruct
    COMMAND ${CMAKE_COMMAND} -E echo "🔧 Reconstructing LaTeX files..."
    COMMAND ${BIN_DIR}/integral-philosophy reconstruct test.tex
    DEPENDS integral-philosophy
    COMMENT "Reconstructing LaTeX files"
)

add_custom_target(analyze
    COMMAND ${CMAKE_COMMAND} -E echo "📊 Analyzing LaTeX files..."
    COMMAND ${BIN_DIR}/integral-philosophy analyze test.tex
    DEPENDS integral-philosophy
    COMMENT "Analyzing LaTeX files"
)

# Demo and run targets
add_custom_target(run
    COMMAND ${BIN_DIR}/integral-philosophy help
    DEPENDS integral-philosophy
    COMMENT "Running integral-philosophy"
)

add_custom_target(demo
    COMMAND ${CMAKE_COMMAND} -E echo "🎉 Haskell Integral Philosophy Publishing System Demo"
    COMMAND ${BIN_DIR}/integral-philosophy help
    DEPENDS integral-philosophy
    COMMENT "Running demo"
)

# Development workflow
add_custom_target(dev-workflow
    DEPENDS build-target test
    COMMENT "Development workflow: build and test"
)

# Maintenance targets
add_custom_target(clean-build
    COMMAND ${CMAKE_COMMAND} -E remove_directory ${BUILD_DIR}
    COMMAND ${CMAKE_COMMAND} -E remove_directory ${BIN_DIR}
    COMMENT "Cleaning build artifacts"
)

add_custom_target(deps
    COMMAND ${CMAKE_COMMAND} -E echo "📦 Installing dependencies..."
    COMMAND ${CMAKE_COMMAND} -E chdir ${HASKELL_PROJECT_DIR} ${STACK_EXECUTABLE} install --dependencies-only
    COMMENT "Installing dependencies"
)

# Information targets
add_custom_target(version
    COMMAND ${GHC_EXECUTABLE} --version
    COMMENT "Showing GHC version"
)

add_custom_target(info
    COMMAND ${CMAKE_COMMAND} -E echo "GHC: ${GHC_EXECUTABLE}"
    COMMAND ${CMAKE_COMMAND} -E echo "Stack: ${STACK_EXECUTABLE}"
    COMMAND ${CMAKE_COMMAND} -E echo "Build Dir: ${BUILD_DIR}"
    COMMAND ${CMAKE_COMMAND} -E echo "Bin Dir: ${BIN_DIR}"
    COMMENT "Showing build information"
)

# Comprehensive check
add_custom_target(check-build
    DEPENDS build-target validate reconstruct analyze
    COMMENT "Running comprehensive checks"
)

# Installation and release
add_custom_target(install-target
    DEPENDS build-target
    COMMENT "Installing integral-philosophy"
)

add_custom_target(release-build
    DEPENDS clean-build deps build-target
    COMMENT "Building release version"
)

# Syntax check
add_custom_target(syntax-check
    COMMAND ${CMAKE_COMMAND} -E echo "🔍 Checking Haskell syntax..."
    COMMAND ${CMAKE_COMMAND} -E chdir ${HASKELL_PROJECT_DIR} 
        ${CMAKE_COMMAND} -P -E "execute_process(COMMAND find . -name \"*.hs\" -exec ${GHC_EXECUTABLE} -Wall -c {} +)"
    COMMENT "Checking Haskell syntax"
)

# Default target
add_custom_target(default-target
    DEPENDS build-all
)

# Print help information
add_custom_target(help-targets
    COMMAND ${CMAKE_COMMAND} -E echo "Available targets:"
    COMMAND ${CMAKE_COMMAND} -E echo "  build-all    - Build everything"
    COMMAND ${CMAKE_COMMAND} -E echo "  build-target - Build the executable"
    COMMAND ${CMAKE_COMMAND} -E echo "  test         - Run tests"
    COMMAND ${CMAKE_COMMAND} -E echo "  validate     - Validate LaTeX files"
    COMMAND ${CMAKE_COMMAND} -E echo "  reconstruct  - Reconstruct LaTeX files"
    COMMAND ${CMAKE_COMMAND} -E echo "  analyze      - Analyze LaTeX files"
    COMMAND ${CMAKE_COMMAND} -E echo "  run          - Run the executable"
    COMMAND ${CMAKE_COMMAND} -E echo "  demo         - Run demo"
    COMMAND ${CMAKE_COMMAND} -E echo "  dev-workflow - Development workflow"
    COMMAND ${CMAKE_COMMAND} -E echo "  clean-build  - Clean build artifacts"
    COMMAND ${CMAKE_COMMAND} -E echo "  deps         - Install dependencies"
    COMMAND ${CMAKE_COMMAND} -E echo "  version      - Show GHC version"
    COMMAND ${CMAKE_COMMAND} -E echo "  info         - Show build information"
    COMMAND ${CMAKE_COMMAND} -E echo "  check-build  - Run comprehensive checks"
    COMMAND ${CMAKE_COMMAND} -E echo "  install-target - Install the executable"
    COMMAND ${CMAKE_COMMAND} -E echo "  release-build - Build release version"
    COMMAND ${CMAKE_COMMAND} -E echo "  syntax-check - Check Haskell syntax"
    COMMAND ${CMAKE_COMMAND} -E echo "  help-targets - Show this help"
    COMMAND ${CMAKE_COMMAND} -E echo "  default-target - Default build target"
    COMMENT "Showing available targets"
)