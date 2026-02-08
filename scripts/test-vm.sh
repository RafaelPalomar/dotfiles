#!/usr/bin/env bash
# Test VM configuration script
#
# Usage: ./scripts/test-vm.sh [test-desktop|test-server]
#
# Launches a QEMU VM to test system configurations before deploying
# to physical machines.

set -e

CONFIG=${1:-"test-desktop"}
DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

echo "Testing configuration: $CONFIG"
echo "Dotfiles directory: $DOTFILES_DIR"

cd "$DOTFILES_DIR"

# Build and launch VM
guix time-machine -C channels.scm -- \
  system vm "entelequia/system/vms/$CONFIG.scm" \
  --share=/tmp/.X11-unix \
  -m 4G

echo ""
echo "VM launched successfully!"
echo "To test the other configuration, run:"
echo "  ./scripts/test-vm.sh test-server"
