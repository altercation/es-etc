{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.WindowSwitcherDecoration
-- Copyright   :  (c) Jan Vornberger 2009
--                    Alejandro Serrano 2010
-- License     :  BSD3-style (see LICENSE)
--
-- Modified    :  es@ethanschoonover.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- A decoration that allows to switch the position of windows by dragging
-- them onto each other.
--
-- Modded from :  XMonad.Layout.WindowSwitcherDecoration
--                by jan.vornberger@informatik.uni-oldenburg.de
--
-----------------------------------------------------------------------------

module XMonad.Layout.TabbedWindowSwitcherDecoration
    ( -- * Usage:
      -- $usage
      windowSwitcherDecorationWithButtons,
      windowSwitcherDecorationWithImageButtons,
      tabbedWindowSwitcherDecorationWithImageButtons,
      WindowSwitcherDecoration, ImageWindowSwitcherDecoration,
      TabbedImageWindowSwitcherDecoration,
    ) where

import XMonad
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.ResizeScreen
import XMonad.Prompt ( XPPosition (..) )
import qualified XMonad.StackSet as S
import Control.Monad
import Data.List
import Foreign.C.Types(CInt)


-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.WindowSwitcherDecoration
-- > import XMonad.Layout.DraggingVisualizer
--
-- Then edit your @layoutHook@ by adding the WindowSwitcherDecoration to
-- your layout:
--
-- > myL = windowSwitcherDecoration shrinkText defaultTheme (draggingVisualizer $ layoutHook defaultConfig)
-- > main = xmonad defaultConfig { layoutHook = myL }
--
-- There is also a version of the decoration that contains buttons like
-- "XMonad.Layout.ButtonDecoration". To use that version, you will need to
-- import "XMonad.Layout.DecorationAddons" as well and modify your @layoutHook@
-- in the following way:
--
-- > import XMonad.Layout.DecorationAddons
-- >
-- > myL = windowSwitcherDecorationWithButtons shrinkText defaultThemeWithButtons (draggingVisualizer $ layoutHook defaultConfig)
-- > main = xmonad defaultConfig { layoutHook = myL }
--
-- Additionaly, there is a version of the decoration that contains image buttons like
-- "XMonad.Layout.ImageButtonDecoration". To use that version, you will need to
-- import "XMonad.Layout.ImageButtonDecoration" as well and modify your @layoutHook@
-- in the following way:
--
-- > import XMonad.Layout.ImageButtonDecoration
-- >
-- > myL = windowSwitcherDecorationWithImageButtons shrinkText defaultThemeWithImageButtons (draggingVisualizer $ layoutHook defaultConfig)
-- > main = xmonad defaultConfig { layoutHook = myL }
--

windowSwitcherDecoration :: (Eq a, Shrinker s) => s -> Theme
           -> l a -> ModifiedLayout (Decoration WindowSwitcherDecoration s) l a
windowSwitcherDecoration s c = decoration s c $ WSD False

windowSwitcherDecorationWithButtons :: (Eq a, Shrinker s) => s -> Theme
           -> l a -> ModifiedLayout (Decoration WindowSwitcherDecoration s) l a
windowSwitcherDecorationWithButtons s c = decoration s c $ WSD True

data WindowSwitcherDecoration a = WSD Bool deriving (Show, Read)

instance Eq a => DecorationStyle WindowSwitcherDecoration a where
    describeDeco _ = "WindowSwitcherDeco"

    decorationCatchClicksHook (WSD withButtons) mainw dFL dFR = if withButtons
                                                                    then titleBarButtonHandler mainw dFL dFR
                                                                    else return False
    decorationWhileDraggingHook _ ex ey (mainw, r) x y = handleTiledDraggingInProgress ex ey (mainw, r) x y
    decorationAfterDraggingHook _ (mainw, _) decoWin = do focus mainw
                                                          hasCrossed <- handleScreenCrossing mainw decoWin
                                                          unless hasCrossed $ do sendMessage $ DraggingStopped
                                                                                 performWindowSwitching mainw

-- Note: the image button code is duplicated from the above
-- because the title bar handle is different

windowSwitcherDecorationWithImageButtons :: (Eq a, Shrinker s) => s -> Theme
           -> l a -> ModifiedLayout (Decoration ImageWindowSwitcherDecoration s) l a
windowSwitcherDecorationWithImageButtons s c = decoration s c $ IWSD True

data ImageWindowSwitcherDecoration a = IWSD Bool deriving (Show, Read)

instance Eq a => DecorationStyle ImageWindowSwitcherDecoration a where
    describeDeco _ = "ImageWindowSwitcherDeco"

    decorationCatchClicksHook (IWSD withButtons) mainw dFL dFR = if withButtons
                                                                    then imageTitleBarButtonHandler mainw dFL dFR
                                                                    else return False
    decorationWhileDraggingHook _ ex ey (mainw, r) x y = handleTiledDraggingInProgress ex ey (mainw, r) x y
    decorationAfterDraggingHook _ (mainw, _) decoWin = do focus mainw
                                                          hasCrossed <- handleScreenCrossing mainw decoWin
                                                          unless hasCrossed $ do sendMessage $ DraggingStopped
                                                                                 performWindowSwitching mainw


tabbedWindowSwitcherDecorationWithImageButtons :: (Eq a, Shrinker s) => s -> Theme
           -> l a -> ModifiedLayout (Decoration TabbedImageWindowSwitcherDecoration s) l a
           -- -> l a -> ModifiedLayout (Decoration TabbedImageWindowSwitcherDecoration s) l a
tabbedWindowSwitcherDecorationWithImageButtons s c = decoration s c $ TIWSD True


data TabbedImageWindowSwitcherDecoration a = TIWSD Bool deriving (Show, Read)

instance Eq a => DecorationStyle TabbedImageWindowSwitcherDecoration a where
    describeDeco _ = "TabbedImageWindowSwitcherDeco"

    decorationCatchClicksHook (TIWSD withButtons) mainw dFL dFR = if withButtons
                                                                    then imageTitleBarButtonHandler mainw dFL dFR
                                                                    else return False
    decorationWhileDraggingHook _ ex ey (mainw, r) x y = handleTiledDraggingInProgress ex ey (mainw, r) x y
    decorationAfterDraggingHook _ (mainw, _) decoWin = do focus mainw
                                                          hasCrossed <- handleScreenCrossing mainw decoWin
                                                          unless hasCrossed $ do sendMessage $ DraggingStopped
                                                                                 performWindowSwitching mainw
    pureDecoration (TIWSD p) _ dht (Rectangle x y wh ht) s _ (w,_) =
        if (isInStack s w && numWindows > 1)
          then Just $ Rectangle nx ny wid (fi dht) else Nothing
        where wrs = S.integrate s
              loc i = (wh * fi i) `div` max 1 (fi $ length wrs)
              wid = maybe (fi x) (\i -> loc (i+1) - loc i) $ w `elemIndex` wrs
--              ny  = case p of
--                     Top    -> y
--                     Bottom -> y + fi ht - fi dht
              ny  = y
              nx  = (x +) $ maybe 0 (fi . loc) $ w `elemIndex` wrs
              numWindows = length wrs

handleTiledDraggingInProgress :: CInt -> CInt -> (Window, Rectangle) -> Position -> Position -> X ()
handleTiledDraggingInProgress ex ey (mainw, r) x y = do
    let rect = Rectangle (x - (fi ex - rect_x r))
                         (y - (fi ey - rect_y r))
                         (rect_width  r)
                         (rect_height r)
    sendMessage $ DraggingWindow mainw rect

performWindowSwitching :: Window -> X ()
performWindowSwitching win =
    withDisplay $ \d -> do
       root <- asks theRoot
       (_, _, selWin, _, _, _, _, _) <- io $ queryPointer d root
       ws <- gets windowset
       let allWindows = S.index ws
       -- do a little double check to be sure
       if (win `elem` allWindows) && (selWin `elem` allWindows)
            then do
                let allWindowsSwitched = map (switchEntries win selWin) allWindows
                let (ls, t:rs) = break (win ==) allWindowsSwitched
                let newStack = S.Stack t (reverse ls) rs
                windows $ S.modify' $ \_ -> newStack
            else return ()
    where
        switchEntries a b x
            | x == a    = b
            | x == b    = a
            | otherwise = x

