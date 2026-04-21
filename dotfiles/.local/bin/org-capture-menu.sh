#!/usr/bin/env bash
# Rofi-driven capture menu.  Routes to either org-capture (for GTD
# inbox entries) or the PKS denote capture helpers (for silo-routed
# Zettelkasten notes).  All denote captures open a floating frame
# named "denote" so bspwm pins them.

OPTIONS="\
Quick Task\n\
Detailed Task\n\
Personal Task\n\
Meeting Note\n\
AI Task\n\
GitHub Issue\n\
---\n\
Fleeting Note (denote)\n\
Literature Note (denote)\n\
New Project (denote)\n\
Hub / MOC (denote)"

CHOICE=$(echo -e "$OPTIONS" | rofi -dmenu -p "Capture" -i)
[[ -z "$CHOICE" || "$CHOICE" == "---" ]] && exit 0

# Floating Emacs frames for interactive captures.  `denote` frame
# name is pinned by bspwm / picom rules; `org-capture` similarly.
frame() { echo "((name . \"$1\") (window-system . x))"; }

case "$CHOICE" in
    "Quick Task")
        ~/.local/bin/org-quick-capture.sh
        ;;
    "Detailed Task")
        emacsclient -c -F "$(frame org-capture)" -e '(org-capture nil "tt")'
        ;;
    "Personal Task")
        emacsclient -c -F "$(frame org-capture)" -e '(org-capture nil "tp")'
        ;;
    "Meeting Note")
        emacsclient -c -F "$(frame org-capture)" -e '(org-capture nil "n")'
        ;;
    "AI Task")
        emacsclient -c -F "$(frame org-capture)" -e '(org-capture nil "a")'
        ;;
    "GitHub Issue")
        ~/.local/bin/org-github-capture.sh
        ;;
    "Fleeting Note (denote)")
        emacsclient -c -F "$(frame denote)" -e '(my-pks-capture-fleeting)'
        ;;
    "Literature Note (denote)")
        emacsclient -c -F "$(frame denote)" -e '(my-pks-capture-literature)'
        ;;
    "New Project (denote)")
        emacsclient -c -F "$(frame denote)" -e '(my-pks-capture-project)'
        ;;
    "Hub / MOC (denote)")
        emacsclient -c -F "$(frame denote)" -e '(my-pks-capture-hub)'
        ;;
esac
