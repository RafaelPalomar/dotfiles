#!/usr/bin/env bash
# Deployment script for entelequia system configurations
#
# Usage: ./scripts/deploy.sh [einstein|curie|alucard|lovelace|edison|hopper] [--dry-run]
#
# einstein/curie         : local guix system reconfigure
# alucard|lovelace|edison|hopper : remote guix deploy
#
# Before deploying a remote target for the first time, fill in the host-key in
# entelequia/deploy/<target>.scm (see instructions in that file).

set -e

MACHINE=${1:-"einstein"}
DRY_RUN=$2
DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

case "$MACHINE" in
    einstein|curie) ;;
    alucard|lovelace|edison|hopper) ;;
    *)
        echo "Error: Unknown machine '$MACHINE'"
        echo "Usage: $0 [einstein|curie|alucard|lovelace|edison|hopper] [--dry-run]"
        exit 1
        ;;
esac

echo "Deploying configuration for: $MACHINE"
cd "$DOTFILES_DIR"

# ── Local deployment (einstein / curie) ──────────────────────────────────────

if [[ "$MACHINE" == "einstein" || "$MACHINE" == "curie" ]]; then
    CMD=(sudo guix time-machine -C channels-lock.scm --
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
    CMD=(guix time-machine -C channels-lock.scm --
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
    CMD=(guix time-machine -C channels-lock.scm --
         deploy -L "$(realpath .)" entelequia/deploy/lovelace.scm)
    [[ "$DRY_RUN" == "--dry-run" ]] && CMD+=(--dry-run)

    if [[ "$DRY_RUN" != "--dry-run" ]]; then
        echo "WARNING: This will reconfigure lovelace (192.168.88.46)!"
        echo "Press Ctrl+C to cancel, or Enter to continue..."
        read
    fi

    "${CMD[@]}"
fi

# ── Remote deployment (edison via guix deploy) ────────────────────────────────

if [[ "$MACHINE" == "edison" ]]; then
    CMD=(guix time-machine -C channels-lock.scm --
         deploy -L "$(realpath .)" entelequia/deploy/edison.scm)
    [[ "$DRY_RUN" == "--dry-run" ]] && CMD+=(--dry-run)

    if [[ "$DRY_RUN" != "--dry-run" ]]; then
        echo "WARNING: This will reconfigure edison (192.168.88.14)!"
        echo "Press Ctrl+C to cancel, or Enter to continue..."
        read
    fi

    "${CMD[@]}"
fi

# ── Remote deployment (hopper via guix deploy) ────────────────────────────────

if [[ "$MACHINE" == "hopper" ]]; then
    CMD=(guix time-machine -C channels-lock.scm --
         deploy -L "$(realpath .)" entelequia/deploy/hopper.scm)
    [[ "$DRY_RUN" == "--dry-run" ]] && CMD+=(--dry-run)

    if [[ "$DRY_RUN" != "--dry-run" ]]; then
        echo "WARNING: This will reconfigure hopper!"
        echo "Press Ctrl+C to cancel, or Enter to continue..."
        read
    fi

    "${CMD[@]}"
fi
