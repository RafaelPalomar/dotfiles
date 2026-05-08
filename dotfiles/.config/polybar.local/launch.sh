#!/usr/bin/env sh
killall -q polybar
while pgrep -x polybar >/dev/null; do sleep 0.2; done

mon="$(bspc query -M --names | head -n1)"
# top_padding + window_gap (12) = effective gap from screen top.
# Bar height 56 + 4 breathing → window starts at y=60 → top_padding 48.
bspc config -m "$mon" top_padding 48

# ── Per-machine polybar parameters ────────────────────────────────────────
# Polybar's INI substitution reliably resolves cross-section refs like
# ${machine.name}, but env-var substitution inside quoted format strings is
# fragile across releases.  So we render a tiny [machine] snippet here and
# config.ini includes it — declarative on the bar side, host-aware here.
machine_ini="$HOME/.cache/polybar/machine.ini"
mkdir -p "$(dirname "$machine_ini")"

host="$(hostname)"
case "$host" in
  curie)
    cat > "$machine_ini" <<'EOF'
[machine]
name = CURIE
net-type = wireless
right-modules = my-pulseaudio sep my-memory sep mynetwork sep my-backlight sep mybattery sep my-datalocker sep my-keyboard-layout sep my-powermenu mysystray
EOF
    ;;
  einstein)
    cat > "$machine_ini" <<'EOF'
[machine]
name = EINSTEIN
net-type = wired
; Desktop has no battery and no backlight; CPU + filesystem fill the gap
; so the right register still reads as a complete status line.
right-modules = my-pulseaudio sep my-memory sep my-cpu sep my-filesystem sep mynetwork sep my-datalocker sep my-keyboard-layout sep my-powermenu mysystray
EOF
    ;;
  *)
    upper="$(printf '%s' "$host" | tr '[:lower:]' '[:upper:]')"
    cat > "$machine_ini" <<EOF
[machine]
name = ${upper}
net-type = wireless
right-modules = my-pulseaudio sep my-memory sep mynetwork sep my-keyboard-layout sep my-powermenu mysystray
EOF
    ;;
esac

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
