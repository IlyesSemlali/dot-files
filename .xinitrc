xhost +localhost
setxkbmap fr
xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr
export QT_STYLE_OVERRIDE=kvantum
export QT_QPA_PLATFORMTHEME=qt5ct
export CM_LAUNCHER=rofi-script
redshift &
xss-lock lock &
feh --bg-fill ~/.wallpaper &
[ -f ~/.xinitrc.local ] && . ~/.xinitrc.local
eval $(dbus-launch --sh-syntax --exit-with-session xmonad)
quit
