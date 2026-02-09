# Minimal makefile for Sphinx documentation

# You can set these variables from the command line, and also
# from the environment for the first two.
SPHINXOPTS    ?=
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = source
BUILDDIR      = build

# Put it first so that "make" without argument is like "make help".
.PHONY: help
help:
	@echo "Entelequia Documentation Build Targets:"
	@echo "  make html      - Build HTML documentation"
	@echo "  make info      - Build Info manual"
	@echo "  make texinfo   - Build Texinfo sources"
	@echo "  make clean     - Remove build artifacts"
	@echo "  make cleanall  - Remove all build artifacts including CLAUDE.md"

.PHONY: html
html:
	$(SPHINXBUILD) -b html "$(SOURCEDIR)" "$(BUILDDIR)/html" $(SPHINXOPTS)
	@echo
	@echo "Build finished. The HTML pages are in $(BUILDDIR)/html."

.PHONY: texinfo
texinfo:
	$(SPHINXBUILD) -b texinfo "$(SOURCEDIR)" "$(BUILDDIR)/texinfo" $(SPHINXOPTS)
	@echo
	@echo "Build finished. The Texinfo files are in $(BUILDDIR)/texinfo."

.PHONY: info
info: texinfo
	@echo "Running makeinfo to generate Info manual..."
	cd "$(BUILDDIR)/texinfo" && makeinfo --no-split entelequia.texi
	@echo
	@echo "Build finished. The Info manual is $(BUILDDIR)/texinfo/entelequia.info"

.PHONY: clean
clean:
	rm -rf "$(BUILDDIR)"

.PHONY: cleanall
cleanall: clean
	rm -f ../CLAUDE.md

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
