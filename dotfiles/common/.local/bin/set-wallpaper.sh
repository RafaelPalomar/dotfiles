#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Usage: set-wallpaper <image-path>"
    exit 1
fi

# Set wallpaper and generate colorscheme
wal -n -i "$1"
feh --bg-scale "$1"

# Copy rofi colors to the expected location
mkdir -p ~/.cache/wal
if [ -f ~/.cache/wal/colors-rofi.rasi ]; then
    # Wal already generated it
    :
elif [ -f ~/.cache/wal/colors-rofi-dark.rasi ]; then
    # Some versions use -dark suffix
    cp ~/.cache/wal/colors-rofi-dark.rasi ~/.cache/wal/colors-rofi.rasi
fi

# Restart polybar to apply new colors
killall -q polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
${HOME}/.config/polybar.local/launch.sh

# Restart dunst to apply new colors
killall -q dunst
dunst &
