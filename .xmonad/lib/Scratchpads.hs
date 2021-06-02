module Scratchpads where

import System.Exit (exitSuccess)
import System.IO (hPutStrLn)

import XMonad

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doRectFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import Config

smallNSP = customFloating $ W.RationalRect l t w h
               where
                 h = 0.6
                 w = 0.6
                 t = 0.8 -h
                 l = 0.8 -w

mediumNSP = customFloating $ W.RationalRect l t w h
               where
                 h = 0.8
                 w = 0.8
                 t = 0.9 -h
                 l = 0.9 -w

fullNSP = customFloating $ W.RationalRect l t w h
               where
                 h = 1
                 w = 1
                 t = 1 -h
                 l = 1 -w


pads :: [NamedScratchpad]
pads = [ NS "terminal" spawnTerm findTerm mediumNSP
                , NS "keepass" spawnKeepass findKeepass smallNSP
                , NS "weechat" spawnWeechat findWeechat mediumNSP
                , NS "slack" spawnSlack findSlack mediumNSP
                , NS "youtube-music" spawnMocp findMocp fullNSP
                , NS "netflix" spawnNetflix findNetflix fullNSP
                , NS "kdeconnect-sms" spawnKSMS findKSMS smallNSP
                , NS "virtualmachine" spawnVM findVM fullNSP
                , NS "screencast" spawnScreencast findScreencast fullNSP
                ]
  where
    spawnTerm  = Config.term ++ " -t scratchpad"
    findTerm   = title=? "scratchpad"

    spawnSlack  = "slack"
    findSlack   = className=? "Slack"

    spawnWeechat  = Config.term ++ " -t weechat -e weechat"
    findWeechat   = title=? "weechat"

    spawnKeepass  = "keepassxc"
    findKeepass   = className=? "KeePassXC"

    spawnMocp  = Config.browser ++ " --qt-arg name ytmusic --basedir .cache/qutebrowser-ytmusic music.youtube.com"
    findMocp   = resource =? "ytmusic"

    spawnKSMS = "kdeconnect-sms"
    findKSMS = className =? "kdeconnect.sms"

    spawnVM = Config.vmCommand
    findVM = className =? "VirtualBox Machine"

    spawnNetflix = "firefox-bin --kiosk https://netflix.com"
    findNetflix   = className =? "Firefox"

    spawnScreencast = "screencast"
    findScreencast = className =? "ffplay"
