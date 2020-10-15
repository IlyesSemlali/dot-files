xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr
export QT_STYLE_OVERRIDE=kvantum
eval $(dbus-launch --sh-syntax --exit-with-session xmonad)
