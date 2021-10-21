-- http://projects.haskell.org/xmobar/
-- you can find weather location codes here: http://weather.noaa.gov/index.html

Config {

  -- general behavior
    lowerOnStart     = True    -- send to bottom of window stack on start
  , hideOnStart      = False   -- start with window unmapped (hidden)
  , allDesktops      = True    -- show on all desktops
  , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
  , pickBroadest     = False   -- choose widest display (multi-monitor)
  , persistent       = True    -- enable/disable hiding (True = disabled)

 -- appearance
  , font            = "xft:Comfortaa Light:pixelsize=16:antialias=true:hinting=true"
  , additionalFonts = [ "xft:SourceCodePro:pixelsize=14:antialias=true:hinting=true"
                           , "xft:SourceCodePro:pixelsize=16:antialias=true:hinting=true"
                           , "xft:Font Awesome 5 Free:pixelsize=13"
                           , "xft:Font Awesome 5 Brands:pixelsize=13"   ]
  , bgColor  = "#292d3e"
  , fgColor  = "#ABABAB"
  , position =     Top

  -- layout
  , sepChar  =  "%"   -- delineator between plugin names and straight text
  , alignSep = "}{"  -- separator between left-right alignment
  , template = " %UnsafeStdinReader% }{ %id% | <icon=linux.xpm/> %uname% | %cpu% | %wlp59s0wi% | %default:Master% | %battery% | %date% "

  , commands = [
         Run Volume "default" "Master" [ "--template", " <volumeipat> <volume> % (<status>)"
                                           , "--"
				               , "--volume-icon-pattern", "<icon=volume/volume_%%.xpm/>"
				               , "-C", "#ABABAB"
				               , "-c", "#ABABAB"
				               , "-O", "on"
				               , "-o", "off"
                                       ] 1

       , Run Com "id" ["-u", "--name"] "" 3600

       , Run Com "uname" ["-r"] "" 3600

       , Run Wireless "wlp59s0" [ "--template", "<qualityipat> <quality> %"
                                    , "--"
                                        , "--quality-icon-pattern", "<icon=wireless/wireless_%%.xpm/>"
                                ] 30

       , Run Date "%d %b. %Y - %H:%M" "date" 50

       -- Prints out the left side items such as workspaces, layout, etc.
       , Run UnsafeStdinReader

       , Run Cpu [ "--template" , "<ipat> <total> %"
                 , "--Low"      , "55"      -- units: %
                 , "--High"     , "77"      -- units: %
                 , "--low"      , "#b5bd68"
                 , "--normal"   , "#de935f"
                 , "--high"     , "#a54242"
                     , "--"
                         , "--load-icon-pattern" , "<icon=cpu/cpu_%%.xpm/>"
                 ] 10

       , Run Battery [ "--template"  , "<leftipat> <timeleft>"
                     , "--maxtwidth" , "10"
                         , "--"
                             , "--on-icon-pattern"   , "<icon=battery/on/battery_on_%%.xpm/>"
                             , "--off-icon-pattern"  , "<icon=battery/off/battery_off_%%.xpm/>"
                             , "--idle-icon-pattern" , "<icon=battery/idle/battery_idle_%%.xpm/>"
                             , "-o" , "<left><fc=#c5c8c6>%</fc> <timeleft>" -- discharging status
                             , "-O" , "<left><fc=#c5c8c6>% <timeleft></fc>" -- plugged in status
                             , "-i" , "<fc=#707880>IDLE</fc>"               -- charged status
                     ] 50
     ]
}
