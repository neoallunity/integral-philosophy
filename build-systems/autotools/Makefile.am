# Automake Makefile for Integral Philosophy Publishing System
# ============================================================

# Global variables
AUTOMAKE_OPTIONS = subdir-objects -Wall -Werror
ACLOCAL_AMFLAGS = ${ACLOCAL_FLAGS}

# Directories
builddir = $(BUILD_DIR)
bindir = $(BIN_DIR)
haskelldir = $(HASKELL_PROJECT_DIR)

# Build directories (order-only prerequisites)
$(builddir) $(bindir):
	$(MKDIR_P) $@

# Primary target
all: $(bindir)/integral-philosophy

# Main executable target
$(bindir)/integral-philosophy: | $(builddir) $(bindir)
	@echo "🔨 Building Haskell Integral Philosophy Publishing System..."
	cd $(HASKELL_PROJECT_DIR) && $(STACK) build $(STACK_FLAGS)
	@if [ -f $(HASKELL_PROJECT_DIR)/.stack-work/install/x86_64-linux-tinfo6/*/9.4.8/bin/integral-philosophy ]; then \
		cp $(HASKELL_PROJECT_DIR)/.stack-work/install/x86_64-linux-tinfo6/*/9.4.8/bin/integral-philosophy $(bindir)/integral-philosophy; \
	elif [ -f $(HASKELL_PROJECT_DIR)/.stack-work/dist/x86_64-linux-tinfo6/*/build/integral-philosophy/integral-philosophy ]; then \
		cp $(HASKELL_PROJECT_DIR)/.stack-work/dist/x86_64-linux-tinfo6/*/build/integral-philosophy/integral-philosophy $(bindir)/integral-philosophy; \
	fi
	@echo "✅ Build complete!"

# Build target
build: $(bindir)/integral-philosophy

# Testing targets
test: $(bindir)/integral-philosophy
	@echo "🧪 Running tests..."
	$(bindir)/integral-philosophy help

validate: $(bindir)/integral-philosophy
	@echo "✅ Validating LaTeX files..."
	$(bindir)/integral-philosophy validate $(TEST_FILE)

reconstruct: $(bindir)/integral-philosophy
	@echo "🔧 Reconstructing LaTeX files..."
	$(bindir)/integral-philosophy reconstruct $(TEST_FILE)

analyze: $(bindir)/integral-philosophy
	@echo "📊 Analyzing LaTeX files..."
	$(bindir)/integral-philosophy analyze $(TEST_FILE)

# Demo and run targets
run: $(bindir)/integral-philosophy
	$(bindir)/integral-philosophy help

demo: $(bindir)/integral-philosophy
	@echo "🎉 Haskell Integral Philosophy Publishing System Demo"
	$(bindir)/integral-philosophy help

# Development workflow
dev: build test

# Maintenance targets
clean-local:
	@echo "🧹 Cleaning build artifacts..."
	rm -rf $(builddir) $(bindir)
	cd $(HASKELL_PROJECT_DIR) && $(STACK) clean

distclean-local:
	@echo "🧹 Cleaning all generated files..."
	rm -rf $(builddir) $(bindir)
	rm -f config.log config.status
	cd $(HASKELL_PROJECT_DIR) && $(STACK) clean

# Dependencies
deps:
	@echo "📦 Installing dependencies..."
	cd $(HASKELL_PROJECT_DIR) && $(STACK) install --dependencies-only

# Information targets
version:
	@echo "GHC Version: $(GHC_VERSION)"
	$(GHC) --version

info-local:
	@echo "GHC: $(GHC)"
	@echo "GHC Version: $(GHC_VERSION)"
	@echo "Stack: $(STACK)"
	@echo "Build Dir: $(builddir)"
	@echo "Bin Dir: $(bindir)"
	@echo "Haskell Project: $(HASKELL_PROJECT_DIR)"
	@echo "GHC Flags: $(GHC_FLAGS)"
	@echo "Stack Flags: $(STACK_FLAGS)"
	@echo "Test File: $(TEST_FILE)"

# Comprehensive check
check: build validate reconstruct analyze

# Installation
install-exec-local: $(bindir)/integral-philosophy
	@echo "📦 Installing integral-philosophy..."
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) $(bindir)/integral-philosophy $(DESTDIR)$(bindir)/integral-philosophy

uninstall-local:
	@echo "🗑️ Uninstalling integral-philosophy..."
	rm -f $(DESTDIR)$(bindir)/integral-philosophy

# Release build
release: clean deps build

# Syntax check
syntax-check:
	@echo "🔍 Checking Haskell syntax..."
	cd $(HASKELL_PROJECT_DIR) && find . -name "*.hs" -exec $(GHC) -Wall $(GHC_FLAGS) -c {} +

# Help target
help:
	@echo "Available targets:"
	@echo "  all            - Build everything (default)"
	@echo "  build          - Build the executable"
	@echo "  test           - Run tests"
	@echo "  validate       - Validate LaTeX files"
	@echo "  reconstruct    - Reconstruct LaTeX files"
	@echo "  analyze        - Analyze LaTeX files"
	@echo "  run            - Run the executable"
	@echo "  demo           - Run demo"
	@echo "  dev            - Development workflow"
	@echo "  clean          - Clean build artifacts"
	@echo "  distclean      - Clean all generated files"
	@echo "  deps           - Install dependencies"
	@echo "  version        - Show GHC version"
	@echo "  info           - Show build information"
	@echo "  check          - Run comprehensive checks"
	@echo "  install        - Install the executable"
	@echo "  uninstall      - Uninstall the executable"
	@echo "  release        - Build release version"
	@echo "  syntax-check   - Check Haskell syntax"
	@echo "  help           - Show this help"
	@echo ""
	@echo "Configure options:"
	@echo "  --enable-debug           Enable debug build"
	@echo "  --enable-optimization    Enable optimization (default)"
	@echo "  --with-test-file=FILE    Set test file (default: test.tex)"

# Distribution
EXTRA_DIST = \
    configure.ac \
    Makefile.am \
    src/Makefile.am \
    build-aux/Makefile.am \
    haskell-project/package.yaml \
    haskell-project/stack.yaml \
    haskell-project/src/*.hs \
    haskell-project/src/**/*.hs

# PHONY targets
.PHONY: all build test validate reconstruct analyze run demo dev clean-local distclean-local deps version info check install-exec-local uninstall-local release syntax-check help

# Ensure make runs in parallel by default
.NOTPARALLEL: