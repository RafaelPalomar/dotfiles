#!/usr/bin/env sh
killall -q polybar
while pgrep -x polybar >/dev/null; do sleep 0.2; done

# top_padding is set per-monitor in the launch loop below, since it depends
# on which bar (masthead 56 / compact 28) that monitor gets.
# Relation: top_padding = bar_height + 4 breathing − window_gap (12).

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
backlight-card = amdgpu_bl0
right-modules = my-pulseaudio sep my-memory sep mynetwork sep my-backlight sep mybattery sep my-datalocker sep my-keyboard-layout sep my-powermenu mysystray
EOF
    ;;
  baroja)
    cat > "$machine_ini" <<'EOF'
[machine]
name = BAROJA
net-type = wireless
backlight-card = intel_backlight
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

# Per-monitor bar choice by vertical resolution: small/low-res panels
# (≤800px tall, e.g. baroja's 1366×768) get the compact bar; everything
# else keeps the full masthead.  `polybar -m` lines look like
# "eDP-1: 1920x1200+0+0 (primary)" — name before the colon, then the first
# WxH+X+Y token; pull H from it.  Read line-by-line (the geometry line has
# spaces, so word-splitting $(...) would mangle it).
polybar -m | while IFS= read -r line; do
  m="${line%%:*}"
  geom="${line#*:}"; geom="${geom# }"   # strip name + leading space
  res="${geom%% *}"                     # first token: WxH+X+Y
  h="${res#*x}"; h="${h%%+*}"           # vertical resolution
  if [ "${h:-9999}" -le 800 ]; then
    bar=mycompact; top_pad=20    # 28 + 4 − 12
  else
    bar=mymain;    top_pad=48    # 56 + 4 − 12
  fi
  bspc config -m "$m" top_padding "$top_pad"
  MONITOR="$m" polybar -r -c "$HOME/.config/polybar.local/config.ini" "$bar" &
done

# Restart tray clients so they re-register with the (new) tray host.
# Order matters: nm-applet et al. must start AFTER polybar's tray module
# has claimed the _NET_SYSTEM_TRAY_S0 selection — otherwise the icons
# fail to appear.  Wait briefly for polybar then reset the clients.
( sleep 1
  pkill -x nm-applet 2>/dev/null
  nm-applet --indicator >/dev/null 2>&1 &
) &
