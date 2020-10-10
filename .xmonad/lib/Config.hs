module Config where

import XMonad (Dimension)

configHomeDir = "/home/ilyes"

configFont = "xft:SourceCodePro:bold:size=9:antialias=true:hinting=true"

configMenuManager = "rofi -terminal alacritty -combi-modi window,drun,ssh -drun-show-actions -font 'hack 10' -show combi"

configTerminal = "alacritty"   -- Sets default terminal

configBrowser = "qutebrowser "               -- Sets firefox as browser for tree select

configEditor = configTerminal ++ " -e vim "    -- Sets vim as editor for tree select

configBorderWidth :: Dimension
configBorderWidth = 1          -- Sets border width for windows
