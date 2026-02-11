# Picom is now managed by shepherd home service (see entelequia/home/services/desktop.scm)
# with auto-restart enabled for stability

sxhkd &
xsetroot -cursor_name left_ptr &
dunst&
nextcloud-client&
exec bspwm
