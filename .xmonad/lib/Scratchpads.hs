module Scratchpads where

import System.Exit (exitSuccess)
import System.IO (hPutStrLn)

import Data.List as L
import Data.List (isSuffixOf)

import XMonad

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doRectFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import Config

(~?) :: Eq a => Query [a] -> [a] -> Query Bool
q ~? x = fmap (x `L.isInfixOf`) q

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

bigNSP = customFloating $ W.RationalRect l t w h
               where
                 h = 0.95
                 w = 0.95
                 t = 0.975 -h
                 l = 0.975 -w

fullNSP = customFloating $ W.RationalRect l t w h
               where
                 h = 1
                 w = 1
                 t = 1 -h
                 l = 1 -w


pads :: [NamedScratchpad]
pads = [
                  NS "pavucontrol"      spawnPavu          findPavu          smallNSP
                , NS "kdeconnect-sms"   spawnKSMS          findKSMS          smallNSP
                , NS "keepass"          spawnKeepass       findKeepass       mediumNSP
                , NS "terminal"         spawnTerm          findTerm          mediumNSP
                , NS "slack"            spawnSlack         findSlack         mediumNSP
                , NS "fileExplorer"     spawnFileExplorer  findFileExplorer  mediumNSP
                , NS "meet"             spawnMeet          findMeet          fullNSP
                , NS "music"            spawnMocp          findMocp          smallNSP
                , NS "nomacs"           spawnNomacs        findNomacs        fullNSP
                , NS "netflix"          spawnNetflix       findNetflix       fullNSP
                , NS "virtualmachine"   spawnVM            findVM            fullNSP
                , NS "screencast"       spawnScreencast    findScreencast    fullNSP
        ]
  where
    spawnTerm  = Config.term ++ " -t scratchpad"
    findTerm   = title=? "scratchpad"

    spawnFileExplorer  = "dolphin"
    findFileExplorer   = ( className=? "dolphin" <&&> role =? "Dolphin#1" )

    spawnNomacs  = "nomacs"
    findNomacs   = ( className=? "Image Lounge" <&&> name =? "Nomacs" )

    spawnPavu  = "pavucontrol --tab=4"
    findPavu   = className=? "Pavucontrol"

    spawnKeepass  = "keepassxc-wrapper"
    findKeepass   = role =? "KeepassMainWindow"

    spawnSlack  = "slack"
    findSlack   = ( title ~? "Slack | " <||> title =? "Slack" )

    spawnMeet  = "wwwwrap meet https://apps.google.com/meet/"
    findMeet   = className =? "meet"

    spawnMocp  = "wwwwrap deezer https://deezer.com/fr"
    findMocp   = className =? "deezer"

    spawnKSMS = "kdeconnect-sms"
    findKSMS = className =? "kdeconnect.sms"

    spawnVM = Config.vmCommand
    findVM = className =? "VirtualBox Machine"

    spawnNetflix = "wwwwrap netflix https://www.disneyplus.com/home"
    findNetflix   = className =? "netflix"

    spawnScreencast = "screencast"
    findScreencast = className =? "ffplay"

    role = stringProperty "WM_WINDOW_ROLE"
    name = stringProperty "WM_NAME"
