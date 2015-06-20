{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget VScale
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Copyright (C) 1999-2005 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A vertical slider widget for selecting a value from a range
--
module Graphics.UI.Gtk.Entry.VScale (
-- * Detail
--
-- | The 'VScale' widget is used to allow the user to select a value using a
-- vertical slider. To create one, use 'vScaleNewWithRange'.
--
-- The position to show the current value, and the number of decimal places
-- shown can be set using the parent 'Scale' class's functions.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Range'
-- |                     +----'Scale'
-- |                           +----VScale
-- @

-- * Types
  VScale,
  VScaleClass,
  castToVScale, gTypeVScale,
  toVScale,

-- * Constructors
  vScaleNew,
  vScaleNewWithRange,
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'VScale'.
--
vScaleNew ::
    Adjustment -- ^ @adjustment@ - the 'Adjustment' which sets the range of
               -- the scale.
 -> IO VScale
vScaleNew adjustment =
  makeNewObject mkVScale $
  liftM (castPtr :: Ptr Widget -> Ptr VScale) $
  {# call unsafe vscale_new #}
    adjustment

-- | Creates a new vertical scale widget that lets the user input a number
-- between @min@ and @max@ (including @min@ and @max@) with the increment
-- @step@. @step@ must be nonzero; it's the distance the slider moves when
-- using the arrow keys to adjust the scale value.
--
-- Note that the way in which the precision is derived works best if @step@
-- is a power of ten. If the resulting precision is not suitable for your
-- needs, use 'Graphics.UI.Gtk.Abstract.Scale.scaleSetDigits' to correct it.
--
vScaleNewWithRange ::
    Double    -- ^ @min@ - minimum value
 -> Double    -- ^ @max@ - maximum value
 -> Double    -- ^ @step@ - step increment (tick size) used with keyboard
              -- shortcuts
 -> IO VScale
vScaleNewWithRange min max step =
  makeNewObject mkVScale $
  liftM (castPtr :: Ptr Widget -> Ptr VScale) $
  {# call unsafe vscale_new_with_range #}
    (realToFrac min)
    (realToFrac max)
    (realToFrac step)
