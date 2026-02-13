#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
DOCS_DIR="docs"

echo "Building documentation..."
cd "$DOCS_DIR"
make clean
make html
make texinfo
make info

echo ""
echo "Documentation built successfully!"
echo "  HTML: $DOCS_DIR/build/html/index.html"
echo "  Info: $DOCS_DIR/build/texinfo/entelequia.info"
echo ""
echo "To install system-wide:"
echo "  guix home reconfigure ~/systems/einstein.scm"
