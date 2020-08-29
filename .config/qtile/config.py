# -*- coding: utf-8 -*-
import os
import re
import socket
import subprocess
from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook
from typing import List  # noqa: F401

import keys
import settings

keys = keys.keys

modifier_keys = {
   'M': 'mod4',
   'A': 'mod1',
}

colors = [["#182828", "#182828"], # 0
          ["#394760", "#394760"], # 1
          ["#81608D", "#81608D"], # 2
          ["#B9678A", "#B9678A"], # 3
          ["#E17774", "#E17774"], # 4 secondary color
          ["#ED9857", "#ED9857"], # 5 main color
          ["#E36267", "#E36267"], # 6 urgent color
          ["#FFFFFF", "#FFFFFF"]] # 7 window name

group_names = [('a',"TERM", {'layout': 'monadtall'}),
               ('q', "WWW", {'layout': 'stack'}),
               ('w', "SYS", {'layout': 'monadtall'}),
               ('x', "RAND", {'layout': 'monadtall'})]

groups = [Group(name, **kwargs) for shortcut, name, kwargs in group_names]

for i, (shortcut, name, kwargs) in enumerate(group_names, 1):
    keys.append(Key([settings.mod], shortcut, lazy.group[name].toscreen()))        # Switch to another group
    keys.append(Key([settings.mod, "shift"], shortcut, lazy.window.togroup(name))) # Send current window to another group

layout_theme = {"border_width": 0,
                "margin": 3
                }

layouts = [
    #layout.MonadWide(**layout_theme),
    #layout.Bsp(**layout_theme),
    layout.Stack(num_stacks=1, **layout_theme),
    #layout.Columns(**layout_theme),
    #layout.RatioTile(**layout_theme),
    #layout.VerticalTile(**layout_theme),
    #layout.Matrix(**layout_theme),
    #layout.Zoomy(**layout_theme),
    layout.MonadTall(**layout_theme),
    #layout.Max(**layout_theme),
    #layout.Tile(shift_windows=True, **layout_theme),
    #layout.Stack(num_stacks=2, **layout_theme),
    layout.TreeTab(
         font = "Ubuntu",
         fontsize = 10,
         sections = ["Windows: "],
         section_fontsize = 11,
         bg_color = "141414",
         active_bg = "90C435",
         active_fg = "000000",
         inactive_bg = "384323",
         inactive_fg = "a0a0a0",
         padding_y = 5,
         section_top = 10,
         panel_width = 1
         )
    #layout.Floating(**layout_theme)
]


prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())
#
###### DEFAULT WIDGET SETTINGS #####
#widget_defaults = dict(
#    font="Ubuntu Mono",
#    fontsize = 12,
#    padding = 2,
#    background=colors[2]
#)
#extension_defaults = widget_defaults.copy()
#
def init_widgets_list():
    widgets_list = [
#              widget.Sep(
#                       linewidth = 0,
#                       padding = 6,
#                       foreground = colors[5],
#                       background = colors[0]
#                       ),
#              widget.Image(
#                       filename = "~/.config/qtile/icons/python.png",
#                       mouse_callbacks = {'Button1': lambda qtile: qtile.cmd_spawn('dmenu_run')}
#                       ),
              widget.CurrentLayoutIcon(
                       custom_icon_paths = [os.path.expanduser("~/.config/qtile/icons")],
                       foreground = colors[0],
                       background = colors[5],
                       padding = 0,
                       scale = 0.7
                       ),
              widget.GroupBox(
                       font = "Liberation Sans",
                       fontsize = 10,
                       margin_y = 0,
                       margin_x = 0,
                       padding_y = 5,
                       padding_x = 3,
                       borderwidth = 0,
                       active = colors[4],
                       inactive = colors[4],
                       hide_unused = True,
                       rounded = False,
                       highlight_color = colors[1],
                       highlight_method = "line",
                       this_current_screen_border = colors[3],
                       this_screen_border = colors[4],
                       other_current_screen_border = colors[0],
                       other_screen_border = colors[0],
                       foreground = colors[4],
                       background = colors[0]
                       ),
#              widget.Prompt(
#                       prompt = prompt,
#                       font = "Ubuntu Mono",
#                       padding = 10,
#                       foreground = colors[3],
#                       background = colors[1]
#                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 10,
                       background = colors[0]
                       ),
              widget.WindowName(
                       foreground = colors[6],
                       background = colors[0],
                       padding = 0
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 10,
                       foreground = colors[2],
                       background = colors[0]
                       ),
              widget.Battery(
                       fontsize = 12,
                       foreground = colors[6],
                       background = colors[0],
                       update_interval = 10,
                       format = "{percent:2.0%} {watt:.2f}W ({hour:d}h{min:02d})"
                       ),
              widget.TextBox(
                      text = " Vol:",
                       foreground = colors[6],
                       background = colors[0],
                       padding = 0
                       ),
              widget.Volume(
                       foreground = colors[6],
                       background = colors[0],
                       padding = 5
                       ),
              widget.Clock(
                       foreground = colors[6],
                       background = colors[0],
                       format = "%A %d %B  -  %H:%M"
                       ),
              widget.Sep(
                       linewidth = 0,
                       padding = 10,
                       foreground = colors[6],
                       background = colors[0],
                       ),
#              widget.Systray(
#                       background = colors[0],
#                       padding = 5
#                       ),
              ]
    return widgets_list

def init_widgets_screen1():
    widgets_screen1 = init_widgets_list()
    return widgets_screen1                       # Slicing removes unwanted widgets on Monitors 1,3
#
def init_widgets_screen2():
    widgets_screen2 = init_widgets_list()
    return widgets_screen2                       # Monitor 2 will display all widgets in widgets_list

def init_screens():
    return [Screen(top=bar.Bar(widgets=init_widgets_screen1(), opacity=0.85, size=20)),
            Screen(top=bar.Bar(widgets=init_widgets_screen2(), opacity=1.0, size=20)),
            Screen(top=bar.Bar(widgets=init_widgets_screen1(), opacity=1.0, size=20))]

if __name__ in ["config", "__main__"]:
    screens = init_screens()
    widgets_list = init_widgets_list()
    widgets_screen1 = init_widgets_screen1()
#    widgets_screen2 = init_widgets_screen2()

mouse = [
    Drag([settings.mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([settings.mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([settings.mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = False
bring_front_click = False
cursor_warp = False

floating_layout = layout.Floating(float_rules=[
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
#    {'wmwindowrole': 'pop-up'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'keepassxc'},
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
])
auto_fullscreen = True
focus_on_window_activation = "smart"

@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser('~')
    subprocess.call([home + '/.config/qtile/autostart.sh'])

# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
