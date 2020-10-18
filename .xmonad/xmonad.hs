-- Before runing this, you need to add a config file and compile it:
--   cd lib/
--   cp Config.hs{.tpl,} && vim $_
--   ghc --make Config.hs

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
import Config as C

myHomeDir :: String
myHomeDir = C.homeDir

myFont :: String
myFont = C.font

myMenuManager :: String
myMenuManager = C.menuManager

myModMask :: KeyMask
myModMask = mod4Mask

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

myFehCommand :: String
myFehCommand = "feh --bg-fill " ++ C.wallpaper ++ "&"

myNormColor :: String
myNormColor   = "#292d3e"

myFocusColor :: String
myFocusColor  = "#bbc5ff"

-- Functions
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Workspaces
myWorkspaceMap :: [(KeySym, String)]
myWorkspaceMap =
               [ (xK_a, "term")
               , (xK_z, "www")
               , (xK_q, "sys")
               , (xK_s, "edit")
               , (xK_x, "rand")]

myWorkspaces :: [String]
myWorkspaces = map snd myWorkspaceMap

-- Layouts
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

-- Scrathpads
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

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm mediumNSP
                , NS "keepass" spawnKeepass findKeepass smallNSP
                , NS "weechat" spawnWeechat findWeechat mediumNSP
                , NS "youtube-music" spawnMocp findMocp fullNSP
                , NS "netflix" spawnNetflix findNetflix fullNSP
                ]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title=? "scratchpad"

    spawnWeechat  = myTerminal ++ " -t weechat -e weechat"
    findWeechat   = title=? "weechat"

    spawnKeepass  = "keepassxc"
    findKeepass   = className=? "KeePassXC"

    spawnMocp  = myBrowser ++ " --qt-arg name ytmusic --basedir .cache/qutebrowser-ytmusic music.youtube.com"
    findMocp   = resource =? "ytmusic"

    spawnNetflix = "firefox --kiosk https://netflix.com"
    findNetflix   = className =? "Firefox"

-- Hooks
myStartupHook :: X ()
myStartupHook = do
          spawnPipe myFehCommand
          spawnPipe "picom --experimental-backends -b"
          spawnPipe "unclutter --timeout 1 &"
          spawnPipe "dunst &"
          spawnPipe "greenclip daemon &"
          spawnPipe "pkill redshift && sleep 5; redshift"
          setWMName "LG3D"

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

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ className =? "Vivaldi-stable"     --> doShift "www"
     , className =? "Gimp"               --> doShift "edit"
     , (className =? "Vivaldi-stable" <&&> role =? "pop-up") --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
     , (role =? "GtkFileChooserDialog") --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
     , className =? "VirtualBox Manager" --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
     ]
     <+> ( isFullscreen --> doFullFloat )
     <+> namedScratchpadManageHook myScratchPads
     <+> insertPosition Below Newer
     <+> ( isDialog --> doF W.swapUp )
       where
             role = stringProperty "WM_WINDOW_ROLE"

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.0


myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- Workspaces
        [ ((m .|. modMask, k), windows $ f i)                                   --Switch to n workspaces and send client to n workspaces
          | (i, k) <- zip (XMonad.workspaces conf) (map fst myWorkspaceMap)
          , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
        ] ++
    -- Xmonad
        [ ((modMask .|. controlMask, xK_r),                      spawn "xmonad --recompile")
        , ((modMask .|. shiftMask, xK_r),                        spawn "xmonad --restart")
        , ((modMask .|. shiftMask, xK_Escape),                   io exitSuccess)
        -- , ((modMask, xK_Escape),                                 io exitSuccess)             -- TODO: lock sreen

    -- Open my preferred terminal
        , ((modMask, xK_Return),                                 spawn myTerminal)
        , ((modMask .|. shiftMask, xK_Return),                   spawn myMenuManager)

    -- Windows
        , ((modMask, xK_c),                                      kill1)
        , ((modMask .|. shiftMask, xK_c),                        killAll)

    -- Floating windows
        , ((modMask, xK_f),                                      sendMessage (T.Toggle "floats"))
        , ((modMask, xK_Delete),                                 withFocused $ windows . W.sink)
        , ((modMask .|. shiftMask, xK_Delete),                   sinkAll)

    -- Windows navigation
        , ((modMask, xK_k),                                      windows W.focusUp)
        , ((modMask, xK_j),                                      windows W.focusDown)
        , ((modMask .|. shiftMask, xK_m),                        windows W.swapMaster)
        , ((modMask .|. shiftMask, xK_k),                        windows W.swapUp)
        , ((modMask .|. shiftMask, xK_j),                        windows W.swapDown)
        , ((modMask, xK_BackSpace),                              promote)
        , ((modMask .|. shiftMask, xK_BackSpace),                rotSlavesDown)
        , ((mod1Mask .|. controlMask, xK_Tab),                   rotAllDown)

     -- Layouts
        , ((modMask, xK_Tab),                                    sendMessage NextLayout)
        , ((modMask .|. controlMask .|. mod1Mask, xK_Up),        sendMessage Arrange)
        , ((modMask .|. controlMask .|. mod1Mask, xK_Down),      sendMessage DeArrange)
        , ((modMask, xK_space),                                  sendMessage (MT.Toggle NBFULL))
        , ((modMask .|. shiftMask, xK_space),                    sendMessage ToggleStruts)
        , ((modMask .|. shiftMask, xK_n),                        sendMessage $ MT.Toggle NOBORDERS)
        , ((modMask, xK_exclam),                                 sendMessage (IncMasterN 1))
        , ((modMask .|. shiftMask, xK_exclam),                   sendMessage (IncMasterN (-1)))

        , ((modMask, xK_h),                                      windows W.focusMaster)
        , ((modMask, xK_l),                                      windows W.focusMaster >> windows W.focusDown)
        , ((modMask .|. controlMask, xK_h),                      sendMessage Shrink)
        , ((modMask .|. controlMask, xK_l),                      sendMessage Expand)
        , ((modMask .|. controlMask, xK_k),                      sendMessage MirrorShrink)
        , ((modMask .|. controlMask, xK_j),                      sendMessage MirrorExpand)

    -- Scratchpads
        , ((modMask, xK_F1),                                     namedScratchpadAction myScratchPads "terminal")
        , ((modMask, xK_F2),                                     namedScratchpadAction myScratchPads "keepass")
        , ((modMask, xK_F3),                                     namedScratchpadAction myScratchPads "weechat")
        , ((modMask, xK_F5),                                     namedScratchpadAction myScratchPads "netflix")
        , ((modMask, xK_F8),                                     namedScratchpadAction myScratchPads "youtube-music")

    -- Applications
        , ((modMask .|. mod1Mask, xK_Return),                    spawn (myBrowser))
        , ((modMask .|. mod1Mask, xK_v),                         spawn ("vivaldi --new-window"))

    -- Multimedia Keys
        , ((0, xF86XK_AudioPlay),                                spawn "playerctl play-pause")
        , ((shiftMask, xF86XK_AudioPlay),                        spawn "playerctl --all-players stop")
        , ((0, xF86XK_AudioPrev),                                spawn "playerctl previous")
        , ((shiftMask, xF86XK_AudioPrev),                        spawn "playerctl position 0")
        , ((0, xF86XK_AudioNext),                                spawn "playerctl next")
        , ((0, xF86XK_AudioMute),                                spawn "amixer set Master toggle")

        , ((0, xF86XK_AudioLowerVolume),                         spawn "amixer set Master 5%-")
        , ((0, xF86XK_AudioRaiseVolume),                         spawn "amixer set Master 5%+ unmute")

        , ((0, xF86XK_MonBrightnessUp),                          spawn "xbacklight -inc 5 -time 300")
        , ((0, xF86XK_MonBrightnessDown),                        spawn "xbacklight -dec 5 -time 300")

        , ((modMask, xK_Control_R),                              spawn "rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command '{cmd}'")
        , ((0, xK_Print), spawn "scrotd")
        ]
        -- The following lines are needed for named scratchpads.
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))


main :: IO ()
main = do
    -- Launching one instance of xmobar on one monitor
    xmobarproc <- spawnPipe myXmobarCommand
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
                        { ppOutput = \x -> hPutStrLn xmobarproc x
                        , ppCurrent = xmobarColor "#c3e88d" ""                -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor "#82AAFF" ""                 -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#c792ea" ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
                        , ppSep =  "<fc=#666666> <fn=2>|</fn> </fc>"          -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" ""                 -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        , ppSort   = fmap (namedScratchpadFilterOutWorkspace.) DO.getSortByOrder
                        }
        }
