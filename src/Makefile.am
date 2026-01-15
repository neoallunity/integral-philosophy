# src/Makefile.am for Integral Philosophy Publishing System
# =======================================================

# This Makefile handles Haskell source files and compilation

# Source directories
HS_SRC_DIR = $(HASKELL_PROJECT_DIR)/src
HS_BUILD_DIR = $(HASKELL_PROJECT_DIR)/.stack-work

# Include all Haskell files in distribution
EXTRA_DIST = \
    $(HS_SRC_DIR)/Main.hs \
    $(HS_SRC_DIR)/IntegralPhilosophy/Prelude.hs \
    $(HS_SRC_DIR)/IntegralPhilosophy/Types.hs \
    $(HS_SRC_DIR)/IntegralPhilosophy/Validation.hs \
    $(HS_SRC_DIR)/IntegralPhilosophy/Validation/LaTeX.hs \
    $(HS_SRC_DIR)/IntegralPhilosophy/Reconstruction/LaTeX.hs \
    $(HS_SRC_DIR)/IntegralPhilosophy/Utils/Time.hs \
    $(HS_SRC_DIR)/IntegralPhilosophy/Utils/Regex.hs \
    $(HS_SRC_DIR)/Tools/LaTeXAnalyzer.hs

# Target for syntax checking individual Haskell files
syntax-check-local:
	@echo "🔍 Checking syntax for Haskell source files..."
	@for file in $(HS_SOURCES); do \
		if [ -f "$$file" ]; then \
			echo "Checking: $$file"; \
			$(GHC) -Wall $(GHC_FLAGS) -c "$$file" || exit 1; \
		fi; \
	done

# Clean Stack-generated files
clean-local:
	@if [ -d "$(HS_BUILD_DIR)" ]; then \
		echo "Cleaning Stack build artifacts..."; \
		cd $(HASKELL_PROJECT_DIR) && $(STACK) clean; \
	fi

# Target for building with specific GHC flags
build-with-flags:
	@echo "🔨 Building with custom GHC flags: $(GHC_FLAGS)"
	cd $(HASKELL_PROJECT_DIR) && \
		$(STACK) build $(STACK_FLAGS) --ghc-options "$(GHC_FLAGS)"

# Target for profiling build
profile:
	@echo "📊 Building profiling version..."
	cd $(HASKELL_PROJECT_DIR) && \
		$(STACK) build --profile --library-profiling --executable-profiling

# Target for documentation generation
docs:
	@echo "📚 Generating documentation..."
	cd $(HASKELL_PROJECT_DIR) && \
		$(STACK) haddock

# Target for testing with Stack
stack-test:
	@echo "🧪 Running Stack tests..."
	cd $(HASKELL_PROJECT_DIR) && \
		$(STACK) test

# Target for benchmarking
benchmark:
	@echo "⏱️ Running benchmarks..."
	cd $(HASKELL_PROJECT_DIR) && \
		$(STACK) bench

# Target for checking dependencies
check-deps:
	@echo "🔍 Checking Haskell dependencies..."
	cd $(HASKELL_PROJECT_DIR) && \
		$(STACK) solver

# Target for updating dependencies
update-deps:
	@echo "⬆️ Updating dependencies..."
	cd $(HASKELL_PROJECT_DIR) && \
		$(STACK) update

# PHONY targets
.PHONY: syntax-check-local clean-local build-with-flags profile docs stack-test benchmark check-deps update-deps