#!/usr/bin/env bash
# Open dired as a floating file manager

# Get starting directory (default to home if not provided)
START_DIR="${1:-$HOME}"

# Open dired in a floating frame
emacsclient -c -F '((name . "dired-manager") (window-system . x))' \
            -e "(progn
                  (dired \"$START_DIR\")
                  (delete-other-windows))"
