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
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doRectFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
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
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
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
myFocusColor  = "#cbc5df"

-- Functions
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Layouts
tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing 2
           $ ResizableTall 1 (3/100) (1/2) []

monocle  = renamed [Replace "monocle"]
           $ mySpacing 4
           $ limitWindows 20 Full

floats   = renamed [Replace "floats"]
           $ limitWindows 20 simplestFloat

-- Hooks
myStartupHook :: X ()
myStartupHook = do
          spawnPipe "xrdb -nocpp -merge .Xresources"
          spawnPipe "kdeconnect-cli --refresh &"
          spawnPipe Config.compositorCommand
          setWMName "LG3D"

myLayoutHook =  smartBorders
                $ avoidStruts
                $ mouseResize
                $ windowArrange
                $ T.toggleLayouts floats
                $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
                $ onWorkspaces ["www", "edit"] (monocle ||| tall)
                $ onWorkspace "vm" (floats ||| tall)
                myDefaultLayout
             where
               myDefaultLayout = tall ||| monocle

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ className   =? "Vivaldi-stable"           --> doShift "www"
     , className   =? "Gimp"                     --> doShift "edit"
     , (className  =? "Vivaldi-stable"
                  <&&> role =? "pop-up")         --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
     , (role       =? "GtkFileChooserDialog")    --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
     , (role       =? "bubble")                  --> doFloat
     , className   =? "VirtualBox Manager"       --> doFloat
     , className   =? "VirtualBoxVM"             --> doFloat
     , className   =? "Image Lounge"             --> doRectFloat (W.RationalRect 0 0 1 1)
     ]
     <+> ( isFullscreen --> doFullFloat )
     <+> ( isDialog     --> doF W.swapUp )
     <+> insertPosition Below Newer
     <+> namedScratchpadManageHook Scratchpads.pads
       where
             role = stringProperty "WM_WINDOW_ROLE"

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.0

main :: IO ()
main = do
    -- Launching one instance of xmobar on one monitor
    xmobarproc <- spawnPipe Config.xmobarCommand
    -- Start xmonad
    xmonad $ ewmh def
        { manageHook = myManageHook <+> manageDocks
        , handleEventHook    = serverModeEventHookCmd
                               <+> serverModeEventHook
                               <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                               <+> docksEventHook
                               <+> fullscreenEventHook
        , XMonad.modMask            = Config.modMask
        , terminal           = Config.term
        , keys               = Keybindings.bindings
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = map snd Workspaces.spaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
                        { ppOutput = \x -> hPutStrLn xmobarproc x
                        , ppCurrent = xmobarColor "#F7C42a" ""                -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#C7A02a" ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor "#CCCCCC" ""                 -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#828A8F" ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
                        , ppSep =  "<fc=#666666> <fn=2>|</fn> </fc>"          -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" ""                 -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppLayout = wrap "<icon=layouts/" ".xpm/>"
                        , ppOrder  = \(ws:l:t:ex) -> [l]++ex++[ws,t]
                        , ppSort   = fmap (namedScratchpadFilterOutWorkspace.) DO.getSortByOrder
                        }
        }
