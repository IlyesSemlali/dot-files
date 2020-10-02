xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr
eval $(dbus-launch --sh-syntax --exit-with-session xmonad)
