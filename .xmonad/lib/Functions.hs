module Functions where

import Data.List as L
import Data.Maybe (isJust)
import Data.Monoid
import Data.Ratio
import Data.Tuple
import Graphics.X11.ExtraTypes.XF86
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import qualified Data.Map as M

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W



import Data.List as L

import Data.List as L

(~?) :: Eq a => Query [a] -> [a] -> Query Bool
q ~? x = fmap (x `L.isInfixOf`) q

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

