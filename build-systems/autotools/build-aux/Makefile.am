# build-aux/Makefile.am for Integral Philosophy Publishing System
# ==============================================================

# This Makefile handles auxiliary build files and scripts

# Install auxiliary scripts
install-data-local:
	@echo "📦 Installing auxiliary scripts..."
	$(MKDIR_P) $(DESTDIR)$(pkgdatadir)/build-aux
	for script in bootstrap test-driver install-sh missing compile depcomp; do \
		if [ -f "$(srcdir)/$$script" ]; then \
			$(INSTALL_DATA) "$(srcdir)/$$script" "$(DESTDIR)$(pkgdatadir)/build-aux/$$script"; \
		fi; \
	done

# Clean auxiliary files
clean-local:
	@echo "🧹 Cleaning auxiliary files..."
	rm -f *.tmp *.log *.bak *~

# Distribution files for auxiliary build
EXTRA_DIST = \
    bootstrap \
    test-driver \
    install-sh \
    missing \
    compile \
    depcomp

# PHONY targets
.PHONY: install-data-local clean-local