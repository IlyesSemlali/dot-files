from libqtile.config import Key
from libqtile.command import lazy

import settings

keys = [
         Key([], "XF86AudioMute", lazy.spawn("amixer -q set Master toggle")),
         Key([], "XF86AudioLowerVolume", lazy.spawn("amixer -c 0 sset Master 5%- unmute")),
         Key([], "XF86AudioRaiseVolume", lazy.spawn("amixer -c 0 sset Master 5%+ unmute")),
         Key([], "XF86MonBrightnessUp", lazy.spawn("backlight.sh up")),
         Key([], "XF86MonBrightnessDown", lazy.spawn("backlight.sh down")),

         ### The essentials
         Key([settings.mod], "Return",
             lazy.spawn(settings.myTerm),
             desc='Launches My Terminal'
             ),
         Key([settings.mod], "v",
             lazy.spawn("vivaldi --new-window"),
             desc='Launches Vivaldi'
             ),
         Key([], "F12",
             lazy.spawn("rofi -terminal alacritty -combi-modi window,drun,ssh -drun-show-actions -font 'hack 10' -show combi"),

             desc='Dmenu Run Launcher'
             ),
         Key([settings.mod], "Tab",
             lazy.next_layout(),
             desc='Toggle through layouts'
             ),
         Key([settings.mod], "c",
             lazy.window.kill(),
             desc='Kill active window'
             ),
         Key([settings.mod, "shift"], "r",
             lazy.restart(),
             desc='Restart Qtile'
             ),
         Key([settings.mod], "Escape",
             lazy.shutdown(),
             desc='Shutdown Qtile'
             ),
         ### Switch focus to specific monitor (out of three)
#         Key([settings.mod], "w",
#             lazy.to_screen(0),
#             desc='Keyboard focus to monitor 1'
#             ),
#         Keysettings.([mod], "e",
#             lazy.to_screen(1),
#             desc='Keyboard focus to monitor 2'
#             ),
#         Key([settings.mod], "r",
#             lazy.to_screen(2),
#             desc='Keyboard focus to monitor 3'
#             ),
         ### Switch focus of monitors
         Key([settings.mod], "period",
             lazy.next_screen(),
             desc='Move focus to next monitor'
             ),
         Key([settings.mod], "comma",
             lazy.prev_screen(),
             desc='Move focus to prev monitor'
             ),
         ### Window controls
         Key([settings.mod], "k",
             lazy.layout.down(),
             desc='Move focus down in current stack pane'
             ),
         Key([settings.mod], "j",
             lazy.layout.up(),
             desc='Move focus up in current stack pane'
             ),
         Key([settings.mod, "shift"], "k",
             lazy.layout.shuffle_down(),
             desc='Move windows down in current stack'
             ),
         Key([settings.mod, "shift"], "j",
             lazy.layout.shuffle_up(),
             desc='Move windows up in current stack'
             ),
         Key([settings.mod], "l",
             lazy.layout.grow(),
             lazy.layout.increase_nmaster(),
             desc='Expand window (MonadTall), increase number in master pane (Tile)'
             ),
         Key([settings.mod], "h",
             lazy.layout.shrink(),
             lazy.layout.decrease_nmaster(),
             desc='Shrink window (MonadTall), decrease number in master pane (Tile)'
             ),
         Key([settings.mod], "n",
             lazy.layout.normalize(),
             desc='normalize window size ratios'
             ),
         Key([settings.mod], "m",
             lazy.layout.maximize(),
             desc='toggle window between minimum and maximum sizes'
             ),
         Key([settings.mod, "shift"], "f",
             lazy.window.toggle_floating(),
             desc='toggle floating'
             ),
         Key([settings.mod, "shift"], "m",
             lazy.window.toggle_fullscreen(),
             desc='toggle fullscreen'
             ),
         ### Stack controls
         Key([settings.mod, "shift"], "space",
             lazy.layout.rotate(),
             lazy.layout.flip(),
             desc='Switch which side main pane occupies (XmonadTall)'
             ),
         Key([settings.mod], "space",
             lazy.layout.next(),
             desc='Switch window focus to other pane(s) of stack'
             ),
         Key([settings.mod, "shift"], "Return",
             lazy.layout.toggle_split(),
             desc='Toggle between split and unsplit sides of stack'
             ),
         ### Dmenu scripts launched with ALT + CTRL + KEY
]
