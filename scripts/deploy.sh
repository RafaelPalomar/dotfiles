#!/usr/bin/env bash
# Deployment script for entelequia system configurations
#
# Usage: ./scripts/deploy.sh [einstein|curie] [--dry-run]
#
# Deploys system configuration to physical machines using guix system reconfigure.
# Always test in VM first using test-vm.sh before deploying to production!

set -e

MACHINE=${1:-"einstein"}
DRY_RUN=$2
DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

if [[ "$MACHINE" != "einstein" && "$MACHINE" != "curie" ]]; then
    echo "Error: Unknown machine '$MACHINE'"
    echo "Usage: $0 [einstein|curie] [--dry-run]"
    exit 1
fi

echo "Deploying configuration for: $MACHINE"
echo "Dotfiles directory: $DOTFILES_DIR"

cd "$DOTFILES_DIR"

if [[ "$DRY_RUN" == "--dry-run" ]]; then
    echo "Running dry-run (no changes will be made)..."
    sudo guix time-machine -C channels.scm -- \
      system reconfigure "entelequia/system/machines/$MACHINE.scm" \
      --dry-run
    echo ""
    echo "Dry-run completed successfully!"
    echo "To deploy for real, run without --dry-run:"
    echo "  ./scripts/deploy.sh $MACHINE"
else
    echo "WARNING: This will reconfigure your system!"
    echo "Press Ctrl+C to cancel, or Enter to continue..."
    read

    sudo guix time-machine -C channels.scm -- \
      system reconfigure "entelequia/system/machines/$MACHINE.scm"

    echo ""
    echo "Deployment completed successfully!"
    echo "You may need to restart services or reboot for changes to take effect."
fi
