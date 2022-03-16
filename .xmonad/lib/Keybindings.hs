--   You can find key symbols here: https://pastebin.com/zHxgcrKD

module Keybindings where

import Data.Maybe (isJust)
import Graphics.X11.ExtraTypes.XF86
import System.Exit (exitSuccess)

import XMonad
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (WSType(..))
import XMonad.Actions.CycleWS (nextScreen, prevScreen, WSType(..))
import XMonad.Actions.Minimize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Layout.Minimize
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad


import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import qualified XMonad.Layout.BoringWindows as BW

import Config
import Workspaces
import Scratchpads

bindings :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
bindings conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
-- Workspaces
        [ ((m .|. Config.modMask, k), windows $ f i)                                   --Switch to n workspaces and send client to n workspaces
          | (i, k) <- zip (XMonad.workspaces conf) (map fst Workspaces.spaces)
          , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
        ] ++
    -- Xmonad
        [ ((Config.modMask .|. controlMask, xK_r),                      spawn "sh -c 'cd ~/.xmonad/lib && ghc --make *.hs'" >> spawn "xmonad --recompile")
        , ((Config.modMask .|. shiftMask, xK_r),                        spawn "xmonad --restart")
        , ((Config.modMask .|. shiftMask, xK_Escape),                   spawn "sh -c 'pkill -f \"xinit.*$DISPLAY\"' -U $(id -u)" >>
                                                                        spawn "sh -c 'pkill -f \"clipmenud\"' -U $(id -u)")
        , ((Config.modMask, xK_Escape),                                 spawn "lock -f")

    -- Windows
        , ((Config.modMask, xK_c),                                      kill1)
        , ((Config.modMask .|. shiftMask, xK_c),                        killAll)

    -- Floating windows
        , ((Config.modMask, xK_f),                                      sendMessage (T.Toggle "floats"))
        , ((Config.modMask, xK_Delete),                                 withFocused $ windows . W.sink)
        , ((Config.modMask .|. shiftMask, xK_Delete),                   sinkAll)

    -- Windows navigation
        , ((Config.modMask, xK_o),                                      prevScreen)
        , ((Config.modMask, xK_p),                                      nextScreen)
        , ((Config.modMask, xK_k),                                      BW.focusUp)
        , ((Config.modMask, xK_j),                                      BW.focusDown)
        , ((Config.modMask .|. shiftMask, xK_h),                        windows W.swapMaster)
        , ((Config.modMask .|. shiftMask, xK_k),                        windows W.swapUp)
        , ((Config.modMask .|. shiftMask, xK_j),                        windows W.swapDown)
        , ((Config.modMask .|. shiftMask, xK_l),                        windows W.swapMaster >> windows W.swapDown)
        , ((Config.modMask, xK_BackSpace),                              promote)
        , ((Config.modMask .|. shiftMask, xK_BackSpace),                rotSlavesDown)

     -- Notifications
        , ((mod5Mask, xK_Delete),                                       spawn "dunstctl close")
        , ((mod5Mask .|. shiftMask, xK_Delete),                         spawn "notifications enable" >> spawn "dunstctl history-pop")
        , ((mod5Mask, xK_BackSpace),                                    spawn "notifications disable")
        , ((mod5Mask .|. shiftMask, xK_BackSpace),                      spawn "notifications enable")

     -- Layouts
        , ((Config.modMask, xK_Tab),                                    sendMessage NextLayout)
        , ((Config.modMask .|. controlMask .|. mod1Mask, xK_Up),        sendMessage Arrange)
        , ((Config.modMask .|. controlMask .|. mod1Mask, xK_Down),      sendMessage DeArrange)
        , ((Config.modMask, xK_space),                                  sendMessage (MT.Toggle NBFULL))
        , ((Config.modMask .|. shiftMask, xK_space),                    sendMessage ToggleStruts)
        , ((Config.modMask .|. shiftMask, xK_n),                        sendMessage $ MT.Toggle NOBORDERS)
        , ((Config.modMask, xK_exclam),                                 sendMessage (IncMasterN 1))
        , ((Config.modMask .|. shiftMask, xK_exclam),                   sendMessage (IncMasterN (-1)))

        , ((Config.modMask, xK_h),                                      BW.focusMaster)
        , ((Config.modMask, xK_l),                                      BW.focusMaster >> BW.focusDown)
        , ((Config.modMask .|. controlMask, xK_h),                      sendMessage Shrink)
        , ((Config.modMask .|. controlMask, xK_l),                      sendMessage Expand)
        , ((Config.modMask .|. controlMask, xK_k),                      sendMessage MirrorShrink)
        , ((Config.modMask .|. controlMask, xK_j),                      sendMessage MirrorExpand)
        , ((Config.modMask, xK_m),                                      withFocused minimizeWindow)
        , ((Config.modMask .|. shiftMask, xK_m),                        withLastMinimized maximizeWindowAndFocus)

    -- Scratchpads
        , ((Config.modMask, xK_F1),                                     namedScratchpadAction Scratchpads.pads "terminal")
        , ((Config.modMask, xK_F2),                                     namedScratchpadAction Scratchpads.pads "keepass")
        , ((Config.modMask, xK_F3),                                     namedScratchpadAction Scratchpads.pads Config.comScratchpad)
        , ((Config.modMask, xK_F4),                                     namedScratchpadAction Scratchpads.pads Config.confScratchpad)
        , ((Config.modMask, xK_F5),                                     namedScratchpadAction Scratchpads.pads Config.mainScratchpad)
        , ((Config.modMask, xK_F6),                                     namedScratchpadAction Scratchpads.pads "music")
        , ((Config.modMask, xK_F7),                                     namedScratchpadAction Scratchpads.pads "screencast")
        , ((Config.modMask, xK_F8),                                     spawn ("monitor-selector"))
        , ((controlMask .|. Config.modMask, xK_w),                      namedScratchpadAction Scratchpads.pads "virtualmachine")
        , ((controlMask .|. Config.modMask .|. shiftMask, xK_w),        spawn "killall -9 VirtualBoxVM")

    -- Applications
        , ((Config.modMask, xK_Return),                                 spawn Config.term)
        , ((controlMask, xK_Return),                                    spawn Config.menuManager)
        , ((Config.modMask .|. mod1Mask, xK_v),                         spawn ("LANG=en_US vivaldi --force-dark-mode --new-window"))
        , ((Config.modMask .|. mod1Mask, xK_b),                         spawn ("bluetooth-selector"))
        , ((Config.modMask .|. mod1Mask, xK_c),                         namedScratchpadAction Scratchpads.pads "fileExplorer")


    -- Multimedia Keys
        , ((0, xF86XK_AudioPlay),                                       spawn "playerctl play-pause")
        , ((shiftMask, xF86XK_AudioPlay),                               spawn "playerctl --all-players stop")
        , ((0, xF86XK_AudioPrev),                                       spawn "playerctl previous")
        , ((shiftMask, xF86XK_AudioPrev),                               spawn "playerctl position 0")
        , ((0, xF86XK_AudioNext),                                       spawn "playerctl next")

        , ((0, xF86XK_AudioMute),                                       spawn "volume mute")
        , ((0, xF86XK_AudioLowerVolume),                                spawn "volume min")
        , ((0, xF86XK_AudioRaiseVolume),                                spawn "volume max")

        , ((shiftMask, xF86XK_AudioMute),                               spawn "microphone mute")
        , ((shiftMask, xF86XK_AudioLowerVolume),                        spawn "microphone min")
        , ((shiftMask, xF86XK_AudioRaiseVolume),                        spawn "microphone max")

        , ((controlMask, xF86XK_AudioMute),                             namedScratchpadAction Scratchpads.pads "pavucontrol")
        , ((controlMask, xF86XK_AudioLowerVolume),                      spawn "volume decrease")
        , ((controlMask, xF86XK_AudioRaiseVolume),                      spawn "volume increase")

        , ((0, xF86XK_MonBrightnessUp),                                 spawn "backlight increase")
        , ((0, xF86XK_MonBrightnessDown),                               spawn "backlight decrease")
        , ((shiftMask, xF86XK_MonBrightnessUp),                         spawn "backlight max")
        , ((shiftMask, xF86XK_MonBrightnessDown),                       spawn "backlight min")

        , ((Config.modMask, xK_Control_R),                              spawn "rofi -modi \"clipboard:clipmenu\" -show clipboard -run-command '{cmd}'")
        , ((controlMask .|. Config.modMask, xK_Return),                 spawn "ambiances")
        , ((0, xK_Print),                                               spawn Config.printScreenCommand)
        ]
        -- The following lines are needed for named scratchpads.
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

