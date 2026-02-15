#!/usr/bin/env bash
# Advanced capture menu

OPTIONS="Quick Task\nDetailed Task\nProject Idea\nMeeting Note\nAI Task\nGitHub Issue\nQuick Note"

CHOICE=$(echo -e "$OPTIONS" | rofi -dmenu -p "Capture" -i)

case "$CHOICE" in
    "Quick Task")
        ~/.local/bin/org-quick-capture.sh
        ;;
    "Detailed Task")
        emacsclient -c -F '((name . "org-capture") (window-system . x))' -e '(org-capture nil "tt")'
        ;;
    "Project Idea")
        emacsclient -c -F '((name . "org-capture") (window-system . x))' -e '(org-capture nil "p")'
        ;;
    "Meeting Note")
        emacsclient -c -F '((name . "org-capture") (window-system . x))' -e '(org-capture nil "n")'
        ;;
    "AI Task")
        emacsclient -c -F '((name . "org-capture") (window-system . x))' -e '(org-capture nil "a")'
        ;;
    "GitHub Issue")
        ~/.local/bin/org-github-capture.sh
        ;;
    "Quick Note")
        emacsclient -c -F '((name . "denote") (window-system . x))' -e '(denote-create-note)'
        ;;
esac
