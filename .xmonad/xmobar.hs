-- http://projects.haskell.org/xmobar/
-- install xmobar with these flags: --flags="with_alsa" --flags="with_mpd" --flags="with_xft"  OR --flags="all_extensions"
-- you can find weather location codes here: http://weather.noaa.gov/index.html

Config { font    = "xft:SourceCodePro Light:pixelsize=14:antialias=true:hinting=true"
       , additionalFonts = [ "xft:SourceCodePro:pixelsize=11:antialias=true:hinting=true"
                           , "xft:SourceCodePro:pixelsize=16:antialias=true:hinting=true"
                           , "xft:Font Awesome 5 Free:pixelsize=13"
                           , "xft:Font Awesome 5 Brands:pixelsize=13"
                           ]
       , bgColor = "#292d3e"
       , fgColor = "#f07178"
       , position = Static { xpos = 0 , ypos = 0, width = 1920, height = 24 }
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , iconRoot = "/home/ilyes/.xmonad/xpm/"  -- default: "."
       , commands = [ 
                      -- Time and date
                      Run Date "<fn=3>\xf133</fn> %b %d %Y (%H:%M)" "date" 50
                      -- Network up and down
                    , Run Network "wlp59s0" ["-t", "down: <rx>kb  up: <tx>kb"] 20
                      -- Cpu usage in percent
                    , Run Cpu ["-t", "<fn=3>\xf133</fn> cpu: (<total>%)","-H","50","--high","red"] 20
                      -- Ram used number and percent
                    , Run Memory ["-t", "<fn=3>\xf063</fn> mem: <used>M (<usedratio>%)"] 20
                      -- Disk space free
                    , Run DiskU [("/", "<fn=3>\xf0c7</fn> hdd: <free> free")] [] 60
                      -- Runs a standard shell command 'uname -r' to get kernel version
                    , Run Com "uname" ["-r"] "" 3600
                      -- Prints out the left side items such as workspaces, layout, etc.
                      -- The workspaces are 'clickable' in my configs.
                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "<action=`xdotool key control+alt+g`><icon=haskell_20.xpm/></action><fc=#666666>|</fc> %UnsafeStdinReader% }{ <fc=#666666><fn=1>|</fn> </fc><fc=#b3afc2><fn=4></fn>  %uname% </fc><fc=#666666><fn=1>|</fn></fc><fc=#FFB86C> %cpu% </fc><fc=#666666><fn=1>|</fn></fc><fc=#FF5555> %memory% </fc><fc=#666666><fn=1>|</fn></fc><fc=#82AAFF> %disku% </fc><fc=#666666><fn=1>|</fn></fc><fc=#c3e88d> %wlp59s0% </fc><fc=#666666><fn=1>|</fn></fc><fc=#8BE9FD> %date%  </fc>"
       }
