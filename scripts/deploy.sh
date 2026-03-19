#!/usr/bin/env bash
# Deployment script for entelequia system configurations
#
# Usage: ./scripts/deploy.sh [einstein|curie|alucard] [--dry-run]
#
# einstein/curie : local guix system reconfigure
# alucard        : remote guix deploy (requires root SSH on port 2222)
#
# Before deploying alucard for the first time, fill in the host-key in
# entelequia/deploy/alucard.scm (see instructions in that file).

set -e

MACHINE=${1:-"einstein"}
DRY_RUN=$2
DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

case "$MACHINE" in
    einstein|curie) ;;
    alucard|lovelace) ;;
    *)
        echo "Error: Unknown machine '$MACHINE'"
        echo "Usage: $0 [einstein|curie|alucard|lovelace] [--dry-run]"
        exit 1
        ;;
esac

echo "Deploying configuration for: $MACHINE"
cd "$DOTFILES_DIR"

# ── Local deployment (einstein / curie) ──────────────────────────────────────

if [[ "$MACHINE" == "einstein" || "$MACHINE" == "curie" ]]; then
    CMD=(sudo guix time-machine -C channels.scm --
         system reconfigure "entelequia/system/machines/$MACHINE.scm")
    [[ "$DRY_RUN" == "--dry-run" ]] && CMD+=(--dry-run)

    if [[ "$DRY_RUN" != "--dry-run" ]]; then
        echo "WARNING: This will reconfigure your system!"
        echo "Press Ctrl+C to cancel, or Enter to continue..."
        read
    fi

    "${CMD[@]}"
fi

# ── Remote deployment (alucard via guix deploy) ───────────────────────────────

if [[ "$MACHINE" == "alucard" ]]; then
    CMD=(guix time-machine -C channels.scm --
         deploy -L "$(realpath .)" entelequia/deploy/alucard.scm)
    [[ "$DRY_RUN" == "--dry-run" ]] && CMD+=(--dry-run)

    if [[ "$DRY_RUN" != "--dry-run" ]]; then
        echo "WARNING: This will reconfigure alucard.local!"
        echo "Press Ctrl+C to cancel, or Enter to continue..."
        read
    fi

    "${CMD[@]}"
fi

# ── Remote deployment (lovelace via guix deploy) ──────────────────────────────

if [[ "$MACHINE" == "lovelace" ]]; then
    CMD=(guix time-machine -C channels.scm --
         deploy -L "$(realpath .)" entelequia/deploy/lovelace.scm)
    [[ "$DRY_RUN" == "--dry-run" ]] && CMD+=(--dry-run)

    if [[ "$DRY_RUN" != "--dry-run" ]]; then
        echo "WARNING: This will reconfigure lovelace (192.168.88.46)!"
        echo "Press Ctrl+C to cancel, or Enter to continue..."
        read
    fi

    "${CMD[@]}"
fi
