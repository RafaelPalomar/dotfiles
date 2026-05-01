#!/bin/sh
# Install + launch CoQ Windows build under wine64-staging on Guix.
# Usage: coq-wine-setup.sh /path/to/cavesofqud-windows-*.zip
#
# Creates a dedicated wine prefix at ~/.wine-coq (isolated from other
# wine apps) and extracts CoQ into it.  Second invocation (no args or
# any arg) just launches the installed game.

set -e

SRC="${1:-}"
PREFIX="$HOME/.wine-coq"
GAMEDIR="$PREFIX/drive_c/CavesOfQud"

need() {
    command -v "$1" >/dev/null 2>&1 || {
        echo "!! '$1' not on PATH; wrap call with guix shell"
        exit 1
    }
}

# --- First-run install path ---------------------------------------------------
if [ -n "$SRC" ] && [ -f "$SRC" ]; then
    echo "=== installer: $SRC ==="

    if [ -d "$PREFIX" ]; then
        echo "=== wine prefix already exists at $PREFIX ==="
        echo "    ( rm -rf $PREFIX  to start fresh )"
    else
        echo "=== creating wine prefix $PREFIX ==="
        WINEPREFIX="$PREFIX" WINEARCH=win64 \
            guix shell wine64-staging -- wineboot --init
    fi

    if [ -d "$GAMEDIR" ]; then
        echo "=== removing old $GAMEDIR ==="
        rm -rf "$GAMEDIR"
    fi
    mkdir -p "$GAMEDIR"

    echo "=== extracting to $GAMEDIR ==="
    case "$SRC" in
        *.zip) guix shell unzip -- unzip -q "$SRC" -d "$GAMEDIR" ;;
        *.exe) echo "!! .exe installers unsupported; re-download the plain zip if available"; exit 1 ;;
        *)     echo "!! unknown file type $SRC"; exit 1 ;;
    esac

    echo "=== top-level in $GAMEDIR ==="
    ls -la "$GAMEDIR" | head -12
fi

# --- Launch -------------------------------------------------------------------
EXE=$(find "$GAMEDIR" -maxdepth 2 -name 'CoQ.exe' -o -name 'CavesOfQud.exe' 2>/dev/null | head -1)
if [ -z "$EXE" ]; then
    echo "!! no CoQ.exe / CavesOfQud.exe found under $GAMEDIR"
    ls "$GAMEDIR" | head -20
    exit 1
fi

echo "=== launching $EXE ==="
cd "$(dirname "$EXE")"
exec env WINEPREFIX="$PREFIX" \
         guix shell wine64-staging -- wine "$EXE"
