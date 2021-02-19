--   You can find key symbols here: https://pastebin.com/zHxgcrKD

module Keybindings where

import Data.Maybe (isJust)
import Graphics.X11.ExtraTypes.XF86
import System.Exit (exitSuccess)

import XMonad
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (WSType(..))
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad

import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

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
        , ((Config.modMask .|. shiftMask, xK_Escape),                   io exitSuccess)
        , ((Config.modMask, xK_Escape),                                 spawn "xlock -lockdelay 5")

    -- Windows
        , ((Config.modMask, xK_c),                                      kill1)
        , ((Config.modMask .|. shiftMask, xK_c),                        killAll)

    -- Floating windows
        , ((Config.modMask, xK_f),                                      sendMessage (T.Toggle "floats"))
        , ((Config.modMask, xK_Delete),                                 withFocused $ windows . W.sink)
        , ((Config.modMask .|. shiftMask, xK_Delete),                   sinkAll)

    -- Windows navigation
        , ((Config.modMask, xK_k),                                      windows W.focusUp)
        , ((Config.modMask, xK_j),                                      windows W.focusDown)
        , ((Config.modMask .|. shiftMask, xK_h),                        windows W.swapMaster)
        , ((Config.modMask .|. shiftMask, xK_k),                        windows W.swapUp)
        , ((Config.modMask .|. shiftMask, xK_j),                        windows W.swapDown)
        , ((Config.modMask .|. shiftMask, xK_l),                        windows W.swapMaster >> windows W.swapDown)
        , ((Config.modMask, xK_BackSpace),                              promote)
        , ((Config.modMask .|. shiftMask, xK_BackSpace),                rotSlavesDown)

     -- Notifications
        , ((mod1Mask .|. shiftMask, xK_Delete),                         spawn "dunstctl close")
        , ((mod1Mask, xK_Delete),                                       spawn "dunstctl history-pop")
        , ((mod1Mask .|. controlMask, xK_Return),                       spawn "dunstctl context")
        , ((mod1Mask, xK_BackSpace),                                    spawn "dunstctl set-paused true")
        , ((mod1Mask .|. shiftMask, xK_BackSpace),                      spawn "dunstctl set-paused false")

     -- Layouts
        , ((Config.modMask, xK_Tab),                                    sendMessage NextLayout)
        , ((Config.modMask .|. controlMask .|. mod1Mask, xK_Up),        sendMessage Arrange)
        , ((Config.modMask .|. controlMask .|. mod1Mask, xK_Down),      sendMessage DeArrange)
        , ((Config.modMask, xK_space),                                  sendMessage (MT.Toggle NBFULL))
        , ((Config.modMask .|. shiftMask, xK_space),                    sendMessage ToggleStruts)
        , ((Config.modMask .|. shiftMask, xK_n),                        sendMessage $ MT.Toggle NOBORDERS)
        , ((Config.modMask, xK_exclam),                                 sendMessage (IncMasterN 1))
        , ((Config.modMask .|. shiftMask, xK_exclam),                   sendMessage (IncMasterN (-1)))

        , ((Config.modMask, xK_h),                                      windows W.focusMaster)
        , ((Config.modMask, xK_l),                                      windows W.focusMaster >> windows W.focusDown)
        , ((Config.modMask .|. controlMask, xK_h),                      sendMessage Shrink)
        , ((Config.modMask .|. controlMask, xK_l),                      sendMessage Expand)
        , ((Config.modMask .|. controlMask, xK_k),                      sendMessage MirrorShrink)
        , ((Config.modMask .|. controlMask, xK_j),                      sendMessage MirrorExpand)

    -- Scratchpads
        , ((Config.modMask, xK_F1),                                     namedScratchpadAction Scratchpads.pads "terminal")
        , ((Config.modMask, xK_F2),                                     namedScratchpadAction Scratchpads.pads "keepass")
        , ((Config.modMask, xK_F3),                                     namedScratchpadAction Scratchpads.pads "weechat")
        , ((Config.modMask, xK_F5),                                     namedScratchpadAction Scratchpads.pads "netflix")
        , ((Config.modMask, xK_F7),                                     namedScratchpadAction Scratchpads.pads "screencast")
        , ((Config.modMask, xK_F8),                                     namedScratchpadAction Scratchpads.pads "youtube-music")
        , ((Config.modMask, xK_F9),                                     namedScratchpadAction Scratchpads.pads "kdeconnect-sms")
        , ((controlMask .|. Config.modMask, xK_w),                      namedScratchpadAction Scratchpads.pads "virtualmachine")
        , ((controlMask .|. Config.modMask .|. shiftMask, xK_w),        spawn "killall -9 VirtualBoxVM")

    -- Applications
        , ((Config.modMask, xK_Return),                                 spawn Config.term)
        , ((controlMask, xK_Return),                                    spawn Config.menuManager)
        , ((Config.modMask .|. mod1Mask, xK_Return),                    spawn (Config.browser))
        , ((Config.modMask .|. mod1Mask, xK_v),                         spawn ("vivaldi --new-window"))
        , ((Config.modMask .|. mod1Mask, xK_c),                         spawn ("dolphin"))


    -- Multimedia Keys
        , ((0, xF86XK_AudioPlay),                                       spawn "playerctl play-pause")
        , ((shiftMask, xF86XK_AudioPlay),                               spawn "playerctl --all-players stop")
        , ((0, xF86XK_AudioPrev),                                       spawn "playerctl previous")
        , ((shiftMask, xF86XK_AudioPrev),                               spawn "playerctl position 0")
        , ((0, xF86XK_AudioNext),                                       spawn "playerctl next")

        , ((0, xF86XK_AudioMute),                                       spawn "amixer set Master mute")
        , ((shiftMask, xF86XK_AudioMute),                               spawn "amixer set Master unmute")
        , ((0, xF86XK_AudioLowerVolume),                                spawn "amixer set Master 5%-")
        , ((shiftMask, xF86XK_AudioLowerVolume),                        spawn "amixer set Master 20%")
        , ((0, xF86XK_AudioRaiseVolume),                                spawn "amixer set Master 5%+")
        , ((shiftMask, xF86XK_AudioRaiseVolume),                        spawn "amixer set Master 50%")

        , ((0, xF86XK_MonBrightnessUp),                                 spawn "xbacklight -inc 2 -perceived -fps 60")
        , ((0, xF86XK_MonBrightnessDown),                               spawn "xbacklight -dec 2 -perceived -fps 60")
        , ((shiftMask, xF86XK_MonBrightnessUp),                         spawn "xbacklight -set 80 -fps 60")
        , ((shiftMask, xF86XK_MonBrightnessDown),                       spawn "xbacklight -set 7 -fps 60")

        , ((Config.modMask, xK_Control_R),                              spawn "rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command '{cmd}'")
        , ((controlMask .|. Config.modMask, xK_Return),                 spawn "ambiances")
        , ((0, xK_Print),                                               spawn Config.printScreenCommand)
        ]
        -- The following lines are needed for named scratchpads.
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

