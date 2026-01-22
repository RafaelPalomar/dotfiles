#!/usr/bin/env sh
killall -q polybar
while pgrep -x polybar >/dev/null; do sleep 0.2; done

mon="$(bspc query -M --names | head -n1)"
bspc config -m "$mon" top_padding 36

for m in $(polybar -m | cut -d: -f1); do
  MONITOR="$m" polybar -r -c "$HOME/.config/polybar.local/config.ini" mymain &
done
