#!/bin/sh
# rofi front-end for autorandr — bound to super + shift + d (sxhkdrc).
#
#   • saved profiles  → `autorandr --load <name>`
#   • [detect & apply]→ `autorandr --change` (auto-pick the profile whose
#                        output fingerprint matches what's connected now)
#   • [save current…] → prompt a name, snapshot the live layout
#
# After any switch the global postswitch hook
# (~/.config/autorandr/postswitch) restores the wallpaper and respawns
# polybar, so the bars never go missing.

# `autorandr` (list) prints one profile per line, marking the matching /
# active one with " (detected)" / " (current)".  Strip the markers so the
# bare name is usable as a --load argument.
profiles=$(autorandr 2>/dev/null | sed -E 's/[[:space:]]*\((detected|current)\)[[:space:]]*//g')

choice=$(printf '%s\n[detect & apply]\n[save current as…]\n' "$profiles" \
           | grep -v '^[[:space:]]*$' \
           | rofi -dmenu -i -p "display")

case "$choice" in
    "" )                  exit 0 ;;
    "[detect & apply]" )  autorandr --change ;;
    "[save current as…]" )
        name=$(printf '' | rofi -dmenu -p "profile name")
        [ -n "$name" ] && autorandr --save "$name" --force \
            && notify-send "autorandr" "saved profile: $name"
        ;;
    * )                   autorandr --load "$choice" ;;
esac
