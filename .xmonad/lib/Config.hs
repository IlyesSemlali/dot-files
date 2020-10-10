module Config where

homeDir = "/home/ilyes/"

font = "xft:SourceCodePro:bold:size=9:antialias=true:hinting=true"

menuManager = "rofi -terminal alacritty -combi-modi window,drun,ssh -drun-show-actions -font 'hack 10' -show combi"

terminalEmulator = "alacritty"   -- Sets default terminal

browser = "qutebrowser "               -- Sets firefox as browser for tree select

editor = terminalEmulator ++ " -e vim "    -- Sets vim as editor for tree select
