if [ "$(hostname)" = "einstein" ]; then
    picom -b --config ~/.config/picom/picom-einstein.conf &
else
    picom -b --config ~/.config/picom/picom-curie.conf &
fi

xsetroot -cursor_name left_ptr &
sxhkd &
dunst&
nextcloud-client&
exec bspwm
