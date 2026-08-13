#!/usr/bin/env bash
#
# screenshot.sh — capture the screen with maim, save it, and copy it to
# the clipboard.  Bound to the Print key (and modifier variants) in sxhkd.
#
# Usage: screenshot.sh [full|region|window]
#   full    capture every monitor          (default; bare Print)
#   region  drag-select a region via slop  (super + Print)
#   window  capture the active window       (alt + Print)
#
# Saves a timestamped PNG under ~/Pictures/Screenshots and also puts the
# image on the X clipboard so it can be pasted straight into chat/editors.

set -euo pipefail

mode="${1:-full}"

dir="${HOME}/Pictures/Screenshots"
mkdir -p "$dir"
file="${dir}/$(date +%Y-%m-%d_%H-%M-%S).png"

# maim honours the compositor (picom), so shadows/transparency come out
# correct.  --hidecursor keeps the pointer out of the shot.
maim_opts=(--hidecursor --format=png)

case "$mode" in
    full)
        ;;
    region)
        # -s uses slop for an interactive drag-select; a light border makes
        # the selection visible against any wallpaper.
        maim_opts+=(--select --bordersize=2 --color=0.9,0.9,0.9,0.4)
        ;;
    window)
        maim_opts+=(--window "$(xdotool getactivewindow)")
        ;;
    *)
        echo "Usage: screenshot.sh [full|region|window]" >&2
        exit 1
        ;;
esac

# slop returns non-zero if the user hits Escape — treat that as a quiet abort.
if ! maim "${maim_opts[@]}" "$file"; then
    rm -f "$file"
    exit 0
fi

# Put the PNG on the clipboard for immediate pasting.
xclip -selection clipboard -target image/png -in "$file" >/dev/null 2>&1 || true

# Notify (dunst).  Fall back silently if no notifier is running.
notify-send -i "$file" "Screenshot saved" "$(basename "$file") — copied to clipboard" \
    2>/dev/null || true
