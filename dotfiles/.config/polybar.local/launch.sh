#!/usr/bin/env sh
killall -q polybar
while pgrep -x polybar >/dev/null; do sleep 0.2; done

mon="$(bspc query -M --names | head -n1)"
# top_padding + window_gap (12) = effective gap from screen top.
# Bar height 56 + 4 breathing → window starts at y=60 → top_padding 48.
bspc config -m "$mon" top_padding 48

for m in $(polybar -m | cut -d: -f1); do
  MONITOR="$m" polybar -r -c "$HOME/.config/polybar.local/config.ini" mymain &
done

# Restart tray clients so they re-register with the (new) tray host.
# Order matters: nm-applet et al. must start AFTER polybar's tray module
# has claimed the _NET_SYSTEM_TRAY_S0 selection — otherwise the icons
# fail to appear.  Wait briefly for polybar then reset the clients.
( sleep 1
  pkill -x nm-applet 2>/dev/null
  nm-applet --indicator >/dev/null 2>&1 &
) &
