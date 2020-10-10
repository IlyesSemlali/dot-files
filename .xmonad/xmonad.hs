-- Run xmonad commands from command line with "xmonadctl command". Commands include:
-- shrink, expand, next-layout, default-layout, restart-wm, xterm, kill, refresh, run,
-- focus-up, focus-down, swap-up, swap-down, swap-master, sink, quit-wm. You can run
-- "xmonadctl 0" to generate full list of commands written to ~/.xsession-errors.

    -- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.OnScreen
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO

    -- Data
import Data.Char (isSpace)
import Data.Monoid
import Data.Ratio
import Data.Maybe (isJust)
import Data.Tree
import Data.Tuple
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doRectFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.PerWorkspace

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

    -- Utilities
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

   -- Config
import Config as C

myHomeDir :: String
myHomeDir = C.homeDir

myFont :: String
myFont = C.font

myMenuManager :: String
myMenuManager = C.menuManager

myModMask :: KeyMask
myModMask = mod4Mask       -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = C.terminalEmulator

myBrowser :: String
myBrowser = C.browser

myEditor :: String
myEditor = C.editor

myBorderWidth :: Dimension
myBorderWidth = 1

myXmobarCommand :: String
myXmobarCommand = "xmobar -x 0 " ++ myHomeDir ++ ".xmonad/xmobar.hs"

myNormColor :: String
myNormColor   = "#292d3e"  -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#bbc5ff"  -- Border color of focused windows

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
          spawnPipe "nitrogen --restore &"
          spawnPipe "picom --experimental-backends -b"
          spawnPipe "unclutter --timeout 1 &"
          spawnPipe "dunst &"
          spawnPipe "copyq &"
          spawnPipe "pkill redshift && sleep 5; redshift"
          spawnPipe "nm-applet &"
          spawnPipe "volumeicon &"
          -- spawnPipe "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x292d3e --height 22 &"
          setWMName "LG3D"

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x29,0x2d,0x3e) -- lowest inactive bg
                  (0x29,0x2d,0x3e) -- highest inactive bg
                  (0xc7,0x92,0xea) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0x29,0x2d,0x3e) -- active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 4
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 200
                   , gs_cellpadding  = 4
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }

myAppGrid = [ ("Audacity", "audacity")
                 , ("Deadbeef", "deadbeef")
                 , ("Firefox", "firefox")
                 , ("Geany", "geany")
                 , ("Geary", "geary")
                 , ("Gimp", "gimp")
                 , ("Kdenlive", "kdenlive")
                 , ("LibreOffice Impress", "loimpress")
                 , ("LibreOffice Writer", "lowriter")
                 , ("OBS", "obs")
                 , ("PCManFM", "pcmanfm")
                 ]

smallNSP = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w

mediumNSP = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w

fullNSP = customFloating $ W.RationalRect l t w h
               where
                 h = 1
                 w = 1
                 t = 1 -h
                 l = 1 -w

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm mediumNSP
                , NS "youtube-music" spawnMocp findMocp fullNSP
                , NS "netflix" spawnNetflix findNetflix fullNSP
                ]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title=? "scratchpad"

    spawnMocp  = myBrowser ++ " --qt-arg name ytmusic --basedir .cache/qutebrowser-ytmusic music.youtube.com"
    findMocp   = resource =? "ytmusic"

    spawnNetflix = "netflix"
    findNetflix   = title =? "netflix"

-- Layouts definitions
tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing 4
           $ ResizableTall 1 (3/100) (1/2) []

magnify  = renamed [Replace "magnify"]
           $ magnifier
           $ limitWindows 12
           $ mySpacing 4
           $ ResizableTall 1 (3/100) (1/2) []

monocle  = renamed [Replace "monocle"]
           $ mySpacing 6
           $ limitWindows 20 Full

floats   = renamed [Replace "floats"]
           $ limitWindows 20 simplestFloat

grid     = renamed [Replace "grid"]
           $ limitWindows 12
           $ mySpacing 4
           $ mkToggle (single MIRROR)
           $ Grid (16/10)

-- The layout hook
myLayoutHook =  smartBorders
                $ avoidStruts
                $ mouseResize
                $ windowArrange
                $ T.toggleLayouts floats
                $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
                $ onWorkspaces ["www", "edit"] (monocle ||| tall)
                myDefaultLayout
             where
               myDefaultLayout = tall ||| monocle ||| magnify

myWorkspaceMap :: [(KeySym, String)]
myWorkspaceMap =
               [ (xK_a, "term")
               , (xK_z, "www")
               , (xK_q, "sys")
               , (xK_s, "edit")
               , (xK_x, "rand")]

myWorkspaces :: [String]
myWorkspaces = ["term", "www", "sys", "edit", "rand"]

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ className =? "Vivaldi-stable"     --> doShift "www"
     , className =? "Gimp"               --> doShift "edit"
     , (className =? "Vivaldi-stable" <&&> role =? "pop-up") --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
     , (role =? "GtkFileChooserDialog") --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
     ]
     <+> ( isFullscreen --> doFullFloat )
     <+> namedScratchpadManageHook myScratchPads
     <+> insertPosition Below Newer
       where
             role = stringProperty "WM_WINDOW_ROLE"

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.0

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- Workspaces
        [ ((m .|. modMask, k), windows $ f i)                                                        --Switch to n workspaces and send client to n workspaces
          | (i, k) <- zip (XMonad.workspaces conf) (map fst myWorkspaceMap)
          , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
        ] ++
    -- Xmonad
        [ ((modMask .|. controlMask, xK_r),                                    spawn "xmonad --recompile")      -- Recompiles xmonad
        , ((modMask .|. shiftMask, xK_r),                                      spawn "xmonad --restart")        -- Restarts xmonad
        -- , ((modMask .|. shiftMask, xK_Escape),                                 io exitSuccess)           -- Quits xmonad
        -- , ((modMask, xK_Escape),                                               io exitSuccess)             -- TODO: lock sreen

    -- Open my preferred terminal
        , ((modMask, xK_Return),                                               spawn myTerminal)
        , ((modMask .|. shiftMask, xK_Return),                                 spawn myMenuManager)

    -- Windows
        , ((modMask, xK_c),                                                    kill1)                           -- Kill the currently focused client
        , ((modMask .|. shiftMask, xK_c),                                      killAll)                         -- Kill all windows on current workspace

    -- Floating windows
        , ((modMask, xK_f),                                                    sendMessage (T.Toggle "floats"))       -- Toggles my 'floats' layout
        , ((modMask, xK_Delete),                                               withFocused $ windows . W.sink) -- Push floating window back to tile
        , ((modMask .|. shiftMask, xK_Delete),                                 sinkAll)                      -- Push ALL floating windows to tile

--    -- Grid Select (CTRL-g followed by a key)
--        , ((controlMask .|. xK_g xK_g), spawnSelected' myAppGrid)                 -- grid select favorite apps
--        , ((controlMask .|. xK_g xK_t), goToSelected $ mygridConfig myColorizer)  -- goto selected window
--        , ((controlMask .|. xK_g xK_b), bringSelected $ mygridConfig myColorizer) -- bring selected window

    -- Windows navigation
        , ((modMask, xK_k),                                                    windows W.focusUp)            -- Move focus to the next window
        , ((modMask, xK_j),                                                    windows W.focusDown)          -- Move focus to the prev window
        , ((modMask .|. shiftMask, xK_m),                                      windows W.swapMaster)       -- Swap the focused window and the master window
        , ((modMask .|. shiftMask, xK_k),                                      windows W.swapUp)           -- Swap focused window with next window
        , ((modMask .|. shiftMask, xK_j),                                      windows W.swapDown)         -- Swap focused window with prev window
        , ((modMask, xK_BackSpace),                                            promote)            -- Moves focused window to master, others maintain order
        , ((mod1Mask .|. shiftMask, xK_Tab),                                   rotSlavesDown)         -- Rotate all windows except master and keep focus in place
        , ((mod1Mask .|. controlMask, xK_Tab),                                 rotAllDown)            -- Rotate all the windows in the current stack
        -- , ((modMask .|. shiftMask, xK_s), windows copyToAll)
        -- , ((modMask .|. controlMask, xK_s), killAllOtherCopies)

     -- Layouts
        , ((modMask, xK_Tab),                                                  sendMessage NextLayout)                -- Switch to next layout
        , ((modMask .|. controlMask .|. mod1Mask, xK_Up),                      sendMessage Arrange)
        , ((modMask .|. controlMask .|. mod1Mask, xK_Down),                    sendMessage DeArrange)
        , ((modMask, xK_space),                                                sendMessage (MT.Toggle NBFULL)) -- Toggles noborder/full
        , ((modMask .|. shiftMask, xK_space),                                  sendMessage ToggleStruts)         -- Toggles struts
        , ((modMask .|. shiftMask, xK_n),                                      sendMessage $ MT.Toggle NOBORDERS)      -- Toggles noborder
        , ((modMask, xK_exclam),                                               sendMessage (IncMasterN 1))   -- Increase number of clients in master pane
        , ((modMask .|. shiftMask, xK_exclam),                                 sendMessage (IncMasterN (-1)))  -- Decrease number of clients in master pane
        -- , ((modMask .|. shiftMask <KP_Multiply>"), increaseLimit)              -- Increase number of windows
        -- , ((modMask .|. shiftMask <KP_Divide>"), decreaseLimit)                -- Decrease number of windows

        , ((modMask, xK_h),                                                    windows W.focusMaster)                    -- Move focus to the master window
        , ((modMask, xK_l),                                                    windows W.focusMaster >> windows W.focusDown)  -- Move focus to the stack
        , ((modMask .|. controlMask, xK_h),                                    sendMessage Shrink)                     -- Shrink horiz window width
        , ((modMask .|. controlMask, xK_l),                                    sendMessage Expand)                     -- Expand horiz window width
        , ((modMask .|. controlMask, xK_k),                                    sendMessage MirrorShrink)               -- Shrink vert window width
        , ((modMask .|. controlMask, xK_j),                                    sendMessage MirrorExpand)               -- Exoand vert window width

    -- Scratchpads
        , ((modMask, xK_F1),                                                   namedScratchpadAction myScratchPads "terminal")
        , ((modMask, xK_F8),                                                   namedScratchpadAction myScratchPads "youtube-music")
        , ((modMask, xK_F5),                                                   namedScratchpadAction myScratchPads "netflix")

    -- Applications
        , ((modMask .|. mod1Mask, xK_Return),                                  spawn (myBrowser))
        , ((modMask .|. mod1Mask, xK_v),                                       spawn ("vivaldi --new-window"))

    -- Multimedia Keys
        -- , ((0, xF86XK_AudioPlay), spawn "cmus toggle")
        -- , ((0, xF86XK_AudioPrev), spawn "cmus prev")
        -- , ((0, xF86XK_AudioNext), spawn "cmus next")
        , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
        , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-")
        , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+ unmute")
        , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5 -time 300")
        , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5 -time 300")
        , ((0, xK_Print), spawn "scrotd")
        ]
        -- The following lines are needed for named scratchpads.
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))


main :: IO ()
main = do
    -- Launching one instance of xmobar on one monitor
    xmproc0 <- spawnPipe myXmobarCommand
    -- Start xmonad
    xmonad $ ewmh def
        { manageHook = myManageHook <+> manageDocks
        , handleEventHook    = serverModeEventHookCmd
                               <+> serverModeEventHook
                               <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                               <+> docksEventHook
                               <+> fullscreenEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , keys               = myKeys
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc0 x
                        , ppCurrent = xmobarColor "#c3e88d" ""                -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" ""                 -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#c792ea" ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
                        , ppSep =  "<fc=#666666> <fn=2>|</fn> </fc>"                     -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" ""                 -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        , ppSort   = fmap (namedScratchpadFilterOutWorkspace.) DO.getSortByOrder
                        }
        }
