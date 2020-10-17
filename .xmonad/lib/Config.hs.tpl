module Config where

homeDir = "/home/user/"

font = "xft:SourceCodePro:bold:size=9:antialias=true:hinting=true"

menuManager = "rofi -terminal alacritty -combi-modi window,drun,ssh -drun-show-actions -font 'hack 10' -show combi"

terminalEmulator = "alacritty"

browser = "qutebrowser "

editor = terminalEmulator ++ " -e vim "

wallpaper = ".wallpaper.jpg"
