#!/usr/bin/env bash

# Power menu using rofi
# Options: lock, logout, suspend, restart, shutdown

LOCK="󰌾  Lock"
LOGOUT="󰍃  Logout"
SUSPEND="󰤄  Suspend"
RESTART="󰑓  Restart"
SHUTDOWN="󰐥  Shutdown"

CHOICE=$(printf '%s\n' "$LOCK" "$LOGOUT" "$SUSPEND" "$RESTART" "$SHUTDOWN" \
    | rofi -dmenu -p "Power" -i)

case "$CHOICE" in
    "$LOCK")     /run/setuid-programs/slock ;;
    "$LOGOUT")   bspc quit ;;
    "$SUSPEND")  loginctl suspend ;;
    "$RESTART")  loginctl reboot ;;
    "$SHUTDOWN") loginctl poweroff ;;
esac
