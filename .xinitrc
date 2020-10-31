xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr
export QT_STYLE_OVERRIDE=kvantum
redshift &
xss-lock xlock &
greenclip daemon &
dunst &
picom --experimental-backends -b
feh --bg-fill ~/.config/wallpapers/pirogues.jpg &
eval $(dbus-launch --sh-syntax --exit-with-session xmonad)
quit
