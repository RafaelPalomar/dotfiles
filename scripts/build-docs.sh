#!/usr/bin/env bash
# Build Entelequia documentation in all formats

set -e

# Get the directory of this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOTFILES_DIR="$(dirname "$SCRIPT_DIR")"
DOCS_DIR="$DOTFILES_DIR/docs"

echo "==> Building Entelequia Documentation"
echo "    Dotfiles: $DOTFILES_DIR"
echo "    Docs:     $DOCS_DIR"
echo

# Build HTML documentation
echo "==> Building HTML documentation..."
make -C "$DOCS_DIR" html
echo "    ✓ HTML build complete"
echo

# Build Info manual
echo "==> Building Info manual..."
make -C "$DOCS_DIR" info
echo "    ✓ Info build complete"
echo

# Generate CLAUDE.md from RST sources (for AI compatibility)
echo "==> Generating CLAUDE.md for AI compatibility..."

# Check if pandoc is available
if ! command -v pandoc &> /dev/null; then
    echo "    ⚠ Warning: pandoc not found, skipping CLAUDE.md generation"
    echo "    Install pandoc with: guix shell pandoc -- bash scripts/build-docs.sh"
else
    # Combine all RST files in order
    pandoc \
        "$DOCS_DIR/source/index.rst" \
        "$DOCS_DIR/source/overview.rst" \
        "$DOCS_DIR/source/architecture.rst" \
        "$DOCS_DIR/source/configuration.rst" \
        "$DOCS_DIR/source/commands.rst" \
        "$DOCS_DIR/source/packages.rst" \
        "$DOCS_DIR/source/security.rst" \
        "$DOCS_DIR/source/gpg.rst" \
        "$DOCS_DIR/source/testing.rst" \
        "$DOCS_DIR/source/troubleshooting.rst" \
        -f rst -t markdown \
        -o "$DOTFILES_DIR/CLAUDE.md" \
        --standalone \
        --metadata title="Entelequia Dotfiles System" \
        --metadata subtitle="GNU Guix-based declarative system configuration"

    echo "    ✓ CLAUDE.md generated"
fi

echo
echo "==> Documentation build complete!"
echo "    HTML:     $DOCS_DIR/build/html/index.html"
echo "    Info:     $DOCS_DIR/build/texinfo/entelequia.info"
echo "    Markdown: $DOTFILES_DIR/CLAUDE.md"
echo
echo "View Info manual:"
echo "    info $DOCS_DIR/build/texinfo/entelequia.info"
echo
echo "View HTML documentation:"
echo "    firefox $DOCS_DIR/build/html/index.html"
