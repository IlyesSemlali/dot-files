-- Before runing this, you need to add a config file and compile it:
--   cd lib/
--   cp Config.hs{.tpl,} && vim $_
--   ghc --make *.hs

import Data.Maybe (isJust)
import Data.Monoid
import Data.Ratio
import Data.Tuple
import Graphics.X11.ExtraTypes.XF86
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import qualified Data.Map as M

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO

import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doRectFloat)
import XMonad.Hooks.Minimize
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.ServerMode
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Minimize
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.BoringWindows as BW
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W

   -- Config
import Config
import Scratchpads
import Workspaces
import Keybindings

myBorderWidth :: Dimension
myBorderWidth = 1

myNormColor :: String
myNormColor   = "#292d3e"

myFocusColor :: String
myFocusColor  = "#75512b"

-- Functions
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Layouts
tall     = renamed [Replace "tall"]
           $ limitWindows 5
           $ mySpacing 2
           $ ResizableTall 1 (3/100) (1/2) []

monocle  = renamed [Replace "monocle"]
           $ mySpacing 2
           $ limitWindows 20 Full

floats   = renamed [Replace "floats"]
           $ limitWindows 20 simplestFloat

-- Hooks
myStartupHook :: X ()
myStartupHook = do
          spawn "clipmenud"
          spawn "dunst"
          spawn "battery-check"
          spawnPipe "xrdb -nocpp -merge .Xresources"
          spawnPipe "kdeconnect-cli --refresh"
          spawnPipe "NOTIFY=false audio-output auto"
          spawnPipe "NOTIFY=false volume pause"
          spawnPipe "isync"
          spawnPipe Config.compositorCommand
          setWMName "LG3D"




myLayoutHook =  lessBorders OnlyScreenFloat
                $ BW.boringWindows
                $ minimize
                $ avoidStruts
                $ mouseResize
                $ windowArrange
                $ T.toggleLayouts floats
                $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
                $ onWorkspaces ["www", "alt", "com"] (monocle ||| tall)
                myDefaultLayout
             where
               myDefaultLayout = tall ||| monocle




myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
-- (composition works from right to left, so the first written rule takes it)
myManageHook = composeAll [
-- Define where new windows are created relative to current window
       className        =? "Alacritty"
       <||> className   =? "Google-chrome"
       <||> className   =? "Preview"
       <||> className   =? "Vivaldi-stable"
                                                                                   --> insertPosition Below Newer
     , insertPosition Above Newer

-- Place windows on the right workspace
     , className        =? "Vivaldi-stable"
       <||> className   =? "Google-chrome"
                                                                                   --> doShift "www"

     , ( className      =? "Gimp.bin"       <&&> role =? "gimp-image-window-1" )   --> doShift "edit"

-- Floating Windows
     -- Handle file dialogs
     , ( className      =? "Vivaldi-stable" <&&> role =? "pop-up" )
       <||> role        =? "GtkFileChooserDialog"
                                                                                   --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
     -- "bubble" is for the "google cast" popup window
     , isDialog
     <||> role          =? "bubble"
     <||> className     =? "VirtualBoxVM"
                                                                                   --> doFloat
     , role =? "bubble"                                                            --> hasBorder False


     , className        =? "VirtualBox Manager"
       <||> className   =? "Kvantum Manager"
       <||> className   =? "F5 VPN"
        -- Vivaldi Profile Selector (has no specific title)
       <||> name        =? "Vivaldi"
       <||> className   =? "dolphin"
       <||> className   =? "KeePassXC"
       <||> className   =? "Xmessage"
                                                                                   --> doCenterFloat

     , isFullscreen                                                                --> doFullFloat

-- Scratchpad related rules
     , namedScratchpadManageHook Scratchpads.pads
     ]
       where
             role = stringProperty "WM_WINDOW_ROLE"
             name = stringProperty "WM_NAME"




main :: IO ()
main = do
    -- Connect to DBus Session
    dbus <- D.connectSession
    D.requestName dbus (D.busName_ "org.xmonad.Log")
      [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
    -- Launching one instance of xmobar on one monitor
    xmobarproc <- spawnPipe Config.xmobarCommand
    -- Start xmonad
    xmonad $ ewmhFullscreen $ docks $ ewmh def
        { manageHook = myManageHook
        , handleEventHook    = minimizeEventHook
                               <+> serverModeEventHookCmd
                               <+> serverModeEventHook
                               <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
        , XMonad.modMask            = Config.modMask
        , terminal           = Config.term
        , keys               = Keybindings.bindings
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = map snd Workspaces.spaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , focusFollowsMouse  = False
        , clickJustFocuses   = False
        , logHook = workspaceHistoryHook
                <+> refocusLastLogHook
                <+> dynamicLogWithPP (myLogHook dbus)
        }

current = "#F7C42a"
visible = "#C7A02a"
hidden = "#CCCCCC"
hiddenNoWindows = "#828A8F"
title = "#b3afc2"
urgent = "#C45500"

myLogHook :: D.Client -> PP
myLogHook dbus = def
  { ppOutput = dbusOutput dbus
    -- , ppCurrent = xmobarColor "#F7C42a" ""                -- Current workspace in xmobar
    -- , ppVisible = xmobarColor "#C7A02a" ""                -- Visible but not current workspace
    -- , ppHidden = xmobarColor "#CCCCCC" ""                 -- Hidden workspaces in xmobar
    -- , ppHiddenNoWindows = xmobarColor "#828A8F" ""        -- Hidden workspaces (no windows)
    -- , ppTitle = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
    -- , ppSep =  "<fc=#666666> <fn=2>|</fn> </fc>"          -- Separators in xmobar
    -- , ppUrgent = xmobarColor "#C45500" ""                 -- Urgent workspace
    , ppExtras  = [windowCount]                           -- # of windows current workspace
    -- , ppLayout = wrap "<icon=layouts/" ".xpm/>"
    , ppOrder  = \(ws:l:t:ex) -> [l]++ex++[ws,t]
    , ppSort   = fmap (filterOutWs [scratchpadWorkspaceTag].) DO.getSortByOrder
  }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"
