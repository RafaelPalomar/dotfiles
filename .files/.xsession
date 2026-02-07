if [ "$(hostname)" = "einstein" ]; then
    picom -b --config ~/.config/picom/picom-einstein.conf &
else
    picom -b --config ~/.config/picom/picom-curie.conf &
fi

#emacs --daemon &
sxhkd &
xsetroot -cursor_name left_ptr &
dunst&
nextcloud-client&
exec bspwm
