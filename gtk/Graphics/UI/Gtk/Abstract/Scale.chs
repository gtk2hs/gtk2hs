{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Scale
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
-- Base class for 'HScale' and 'VScale'
--
module Graphics.UI.Gtk.Abstract.Scale (
-- * Detail
--
-- | A 'Scale' is a slider control used to select a numeric value. To use it,
-- you'll probably want to investigate the methods on its base class, 'Range',
-- in addition to the methods for 'Scale' itself. To set the value of a scale,
-- you would normally use 'Graphics.UI.Gtk.Abstract.Range.rangeSetValue'.
-- To detect changes to the value, you would normally use the
-- 'Graphics.UI.Gtk.Abstract.Range.onRangeValueChanged' signal.
--
-- The 'Scale' widget is an abstract class, used only for deriving the
-- subclasses 'HScale' and 'VScale'. To create a scale widget, call
-- 'Graphics.UI.Gtk.Entry.HScale.hScaleNewWithRange' or
-- 'Graphics.UI.Gtk.Entry.VScale.vScaleNewWithRange'.
--

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Range'
-- |                     +----Scale
-- |                           +----'HScale'
-- |                           +----'VScale'
-- @

-- * Types
  Scale,
  ScaleClass,
  castToScale, gTypeScale,
  toScale,

-- * Methods
  scaleSetDigits,
  scaleGetDigits,
  scaleSetDrawValue,
  scaleGetDrawValue,
  PositionType(..),
  scaleSetValuePos,
  scaleGetValuePos,

-- * Attributes
  scaleDigits,
  scaleDrawValue,
  scaleValuePos,
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.General.Enums    (PositionType(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Sets the number of decimal places that are displayed in the value. Also
-- causes the value of the adjustment to be rounded off to this number of
-- digits, so the retrieved value matches the value the user saw.
--
scaleSetDigits :: ScaleClass self => self
 -> Int   -- ^ @digits@ - the number of decimal places to display, e.g. use 1
          -- to display 1.0, 2 to display 1.00 etc.
 -> IO ()
scaleSetDigits self digits =
  {# call scale_set_digits #}
    (toScale self)
    (fromIntegral digits)

-- | Gets the number of decimal places that are displayed in the value.
--
scaleGetDigits :: ScaleClass self => self
 -> IO Int -- ^ returns the number of decimal places that are displayed.
scaleGetDigits self =
  liftM fromIntegral $
  {# call unsafe scale_get_digits #}
    (toScale self)

-- | Specifies whether the current value is displayed as a string next to the
-- slider.
--
scaleSetDrawValue :: ScaleClass self => self
 -> Bool  -- ^ @drawValue@ - a boolean.
 -> IO ()
scaleSetDrawValue self drawValue =
  {# call scale_set_draw_value #}
    (toScale self)
    (fromBool drawValue)

-- | Returns whether the current value is displayed as a string next to the
-- slider.
--
scaleGetDrawValue :: ScaleClass self => self
 -> IO Bool -- ^ returns whether the current value is displayed as a string.
scaleGetDrawValue self =
  liftM toBool $
  {# call unsafe scale_get_draw_value #}
    (toScale self)

-- | Sets the position in which the current value is displayed.
--
scaleSetValuePos :: ScaleClass self => self
 -> PositionType -- ^ @pos@ - the position in which the current value is
                 -- displayed.
 -> IO ()
scaleSetValuePos self pos =
  {# call scale_set_value_pos #}
    (toScale self)
    ((fromIntegral . fromEnum) pos)

-- | Gets the position in which the current value is displayed.
--
scaleGetValuePos :: ScaleClass self => self
 -> IO PositionType -- ^ returns the position in which the current value is
                    -- displayed.
scaleGetValuePos self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe scale_get_value_pos #}
    (toScale self)

--------------------
-- Attributes

-- | The number of decimal places that are displayed in the value.
--
-- Allowed values: [-1,64]
--
-- Default value: 1
--
scaleDigits :: ScaleClass self => Attr self Int
scaleDigits = newAttr
  scaleGetDigits
  scaleSetDigits

-- | Whether the current value is displayed as a string next to the slider.
--
-- Default value: @True@
--
scaleDrawValue :: ScaleClass self => Attr self Bool
scaleDrawValue = newAttr
  scaleGetDrawValue
  scaleSetDrawValue

-- | The position in which the current value is displayed.
--
-- Default value: 'PosTop'
--
scaleValuePos :: ScaleClass self => Attr self PositionType
scaleValuePos = newAttr
  scaleGetValuePos
  scaleSetValuePos
