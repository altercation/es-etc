{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.SideSpacing
-- License     :  BSD-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  portable
--
-- Add a configurable amount of space on side of windows.
-- Modified from B.Yorgey's XMonad.Layout.Spacing module
-----------------------------------------------------------------------------

module XMonad.Layout.SideSpacing (
                               -- * Usage
                               -- $usage

                               spacing, Spacing,

                             ) where

import Graphics.X11 (Rectangle(..))
import Control.Arrow (second)
import XMonad.Util.Font (fi)

import XMonad.Layout.LayoutModifier

-- $usage
-- You can use this module by importing it into your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Layout.Spacing
--
-- and modifying your layoutHook as follows (for example):
--
-- > layoutHook = spacing 2 $ Tall 1 (3/100) (1/2)
-- >                      -- put a 2px space around every window
--

-- | Surround all windows by a certain number of pixels of blank space.
spacing :: Int -> l a -> ModifiedLayout Spacing l a
spacing p = ModifiedLayout (Spacing p)

data Spacing a = Spacing Int deriving (Show, Read)

instance LayoutModifier Spacing a where

    pureModifier (Spacing p) _ _ wrs = (map (second $ shrinkRect p) wrs, Nothing)

    modifierDescription (Spacing p) = "Spacing " ++ show p

shrinkRect :: Int -> Rectangle -> Rectangle
--shrinkRect p (Rectangle x y w h) = Rectangle (x+fi p) (y+fi p) (w-2*fi p) (h-2*fi p)
shrinkRect p (Rectangle x y w h) = Rectangle (x) (y+fi p) (w-fi p) (h-fi p)
