module Config where

import XMonad

modMask :: KeyMask
modMask = mod4Mask

homeDir = "/home/user/"
wallpaper = homeDir ++ ".config/wallpapers/pirogues.jpg"

-- apps
term = "alacritty"
browser = "qutebrowser "
editor = term ++ " -e vim "

-- commands
menuManager = "rofi -terminal alacritty -drun-show-actions -show drun -display-drun launch"
wallpaperCommand = "feh --bg-fill " ++ wallpaper ++ " &"
xmobarCommand = "xmobar -i " ++ homeDir ++ ".xmonad/xpm_stdres " ++ homeDir ++ ".xmonad/xmobar.hs"
vmCommand = "vboxmanage startvm \"Awesome Linux VM (of course)\""
printScreenCommand = "scrot '%Y-%m-%d_%H%M%S.png' -e 'mkdir -p " ++ homeDir ++ "/Images/Screenshots/%Y-%m-%d/ && mv $f " ++ homeDir ++ "/Images/Screenshots/%Y-%m-%d/'"
compositorCommand = "picom --experimental-backends -b"
xbacklightOptions = "-fps 60"
mainScratchpad = "netflix"
