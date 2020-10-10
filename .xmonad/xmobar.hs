--  -- http://projects.haskell.org/xmobar/
--  -- install xmobar with these flags: --flags="with_alsa" --flags="with_mpd" --flags="with_xft"  OR --flags="all_extensions"
--  -- you can find weather location codes here: http://weather.noaa.gov/index.html

Config {

 -- appearance
    font =         "xft:SourceCodePro Light:pixelsize=14:antialias=true:hinting=true"
  , additionalFonts = [ "xft:SourceCodePro:pixelsize=11:antialias=true:hinting=true"
                           , "xft:SourceCodePro:pixelsize=16:antialias=true:hinting=true"
                           , "xft:Font Awesome 5 Free:pixelsize=13"
                           , "xft:Font Awesome 5 Brands:pixelsize=13"   ]
  , bgColor = "#292d3e"
  , fgColor = "#ABABAB"
  , iconRoot = "/home/ilyes/.xmonad/xpm/"  -- default: "."
  , position =     Top

  -- layout
  , sepChar =  "%"   -- delineator between plugin names and straight text
  , alignSep = "}{"  -- separator between left-right alignment
  , template = "<icon=haskell_20.xpm/> %UnsafeStdinReader% }{ <fn=4></fn> %uname% | %battery% | %cpu% | %LFLL% | %date% "

  -- general behavior
  , lowerOnStart =     True    -- send to bottom of window stack on start
  , hideOnStart =      False   -- start with window unmapped (hidden)
  , allDesktops =      True    -- show on all desktops
  , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
  , pickBroadest =     False   -- choose widest display (multi-monitor)
  , persistent =       True    -- enable/disable hiding (True = disabled)

  -- plugins
  --   Numbers can be automatically colored according to their value. xmobar
  --   decides color based on a three-tier/two-cutoff system, controlled by
  --   command options:
  --     --Low sets the low cutoff
  --     --High sets the high cutoff
  --
  --     --low sets the color below --Low cutoff
  --     --normal sets the color between --Low and --High cutoffs
  --     --High sets the color above --High cutoff
  --
  --   The --template option controls how the plugin is displayed. Text
  --   color can be set by enclosing in <fc></fc> tags. For more details
  --   see http://projects.haskell.org/xmobar/#system-monitor-plugins.
  , commands =

       -- weather monitor
       [ Run Weather "LFLL" [ "--template", "<tempC>°C"
                            ] 36000

       -- network activity monitor (dynamic interface resolution)
       , Run DynNetwork     [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s"
                            , "--Low"      , "1000"       -- units: B/s
                            , "--High"     , "5000"       -- units: B/s
                            , "--low"      , "darkgreen"
                            , "--normal"   , "darkorange"
                            , "--high"     , "darkred"
                            ] 10

       -- Cpu usage in percent
       , Run Cpu            ["--template", "Cpu: (<total>%)"
                            , "--High"     , "85"         -- units: %
                            , "--high"     , "darkorange"
                            ] 10

       -- cpu activity monitor
       , Run MultiCpu       [ "--template" , "Cpu: <total0>%|<total1>%"
                            , "--High"     , "85"         -- units: %
                            , "--high"     , "darkorange"
                            ] 10

       -- cpu core temperature monitor
       , Run CoreTemp       [ "--template" , "Temp: <core0>°C|<core1>°C"
                            , "--Low"      , "70"        -- units: °C
                            , "--High"     , "80"        -- units: °C
                            , "--low"      , "darkgreen"
                            , "--normal"   , "darkorange"
                            , "--high"     , "darkred"
                            ] 50

       -- Disk space free
       , Run DiskU [("/", "<free> free")] [] 60

       -- memory usage monitor
       , Run Memory         [ "--template" ,"Mem: <usedratio>%"
                            , "--Low"      , "20"        -- units: %
                            , "--High"     , "90"        -- units: %
                            , "--low"      , "darkgreen"
                            , "--normal"   , "darkorange"
                            , "--high"     , "darkred"
                            ] 10

       -- battery monitor
       , Run Battery        [ "--template" , "Batt: <acstatus>"
                            , "--Low"      , "15"        -- units: %
                            , "--low"      , "darkorange"

                            , "--" -- battery specific options
                                      -- discharging status
                                      , "-o"	, "<left>% (<timeleft>)"
                                      -- AC "on" status
                                      , "-O"	, "<fc=#dAA520>Charging</fc>"
                                      -- charged status
                                      , "-i"	, "<fc=#006000>Charged</fc>"
                            ] 50

       -- Runs a standard shell command 'uname -r' to get kernel version
       , Run Com "uname" ["-r"] "" 3600

       -- time and date indicator
       --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
       -- , Run Date           "<fc=#ABABAB>%F (%a) %T</fc>" "date" 10
       , Run Date "%d %b. %Y - %H:%M" "date" 50

       -- Prints out the left side items such as workspaces, layout, etc.
       -- The workspaces are 'clickable' in my configs.
       , Run UnsafeStdinReader
       ]
  }
