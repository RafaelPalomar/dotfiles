#!/usr/bin/env bash
# Display current org-clock status for Polybar

# Query Emacs for current clock
CLOCK_INFO=$(emacsclient -e '(if (org-clock-is-active)
    (format "%s (%s)"
            (substring-no-properties (org-clock-get-clock-string))
            (org-duration-from-minutes (org-clock-get-clocked-time)))
  "")' 2>/dev/null | sed 's/"//g')

if [[ -n "$CLOCK_INFO" && "$CLOCK_INFO" != "" ]]; then
    echo " $CLOCK_INFO"
else
    echo ""
fi
