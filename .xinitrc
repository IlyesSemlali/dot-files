xhost +localhost
xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr
export QT_STYLE_OVERRIDE=kvantum
redshift &
xss-lock xlock &
greenclip daemon &
dunst &
feh --bg-fill ~/.wallpapers/li-esteban-toro.jpg &
[ -f ~/.xinitrc.local ] && . ~/.xinitrc.local
eval $(dbus-launch --sh-syntax --exit-with-session xmonad)
quit
