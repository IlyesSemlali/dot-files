module Config where

import XMonad

modMask :: KeyMask
modMask = mod4Mask

homeDir = "/home/user/"
wallpaper = homeDir ++ ".config/wallpapers/pirogues.jpg"

-- apps
term = "alacritty"   -- Sets default terminal
browser = "qutebrowser "               -- Sets firefox as browser for tree select
editor = term ++ " -e vim "    -- Sets vim as editor for tree select

-- commands
menuManager = "rofi -terminal alacritty -combi-modi window,drun,ssh -drun-show-actions -font 'hack 10' -show combi"
wallpaperCommand = "feh --bg-fill " ++ wallpaper ++ " &"
xmobarCommand = "xmobar -i " ++ homeDir ++ ".xmonad/xpm/ " ++ homeDir ++ ".xmonad/xmobar.hs"
vmCommand = "vboxmanage startvm \"Awesome Linux VM (of course)\""
