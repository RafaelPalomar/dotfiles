#!/usr/bin/env bash
# Rofi menu for org-clock control

OPTIONS="Clock In\nClock Out\nGo to Current\nResume Last\nReport Today"

CHOICE=$(echo -e "$OPTIONS" | rofi -dmenu -p "Clock Control" -i)

case "$CHOICE" in
    "Clock In")
        # Get recent tasks from clock history
        TASKS=$(emacsclient -e '(mapconcat
            (lambda (m) (with-current-buffer (marker-buffer m)
                         (goto-char (marker-position m))
                         (org-get-heading t t t t)))
            org-clock-history "\n")' | sed 's/"//g')

        TASK=$(echo -e "$TASKS" | rofi -dmenu -p "Clock In")
        if [[ -n "$TASK" ]]; then
            emacsclient -e "(org-clock-in)" &>/dev/null
            notify-send "Org Clock" "Clocked in: $TASK" -i clock
        fi
        ;;
    "Clock Out")
        emacsclient -e '(org-clock-out)' &>/dev/null
        notify-send "Org Clock" "Clocked out" -i clock
        ;;
    "Go to Current")
        emacsclient -c -e '(org-clock-goto)'
        ;;
    "Resume Last")
        emacsclient -e '(org-clock-in-last)' &>/dev/null
        notify-send "Org Clock" "Resumed last task" -i clock
        ;;
    "Report Today")
        emacsclient -c -e '(org-clock-report)'
        ;;
esac
