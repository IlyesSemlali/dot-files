xhost +localhost
xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr
export QT_STYLE_OVERRIDE=kvantum
export CM_LAUNCHER=rofi-script
redshift &
xss-lock lock &
dunst &
feh --bg-fill ~/.wallpaper &
[ -f ~/.xinitrc.local ] && . ~/.xinitrc.local
eval $(dbus-launch --sh-syntax --exit-with-session xmonad)
quit
