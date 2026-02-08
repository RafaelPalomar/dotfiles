#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Usage: set-wallpaper <image-path>"
    exit 1
fi

# Set wallpaper and generate colorscheme
wal -n -i "$1"
feh --bg-scale "$1"

# Restart polybar to apply new colors
killall -q polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
${HOME}/.config/polybar.local/launch.sh

# Restart dunst to apply new colors
killall -q dunst
dunst &
