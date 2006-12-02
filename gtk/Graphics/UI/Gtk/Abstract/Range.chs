-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Range
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.11 $ from $Date: 2005/10/19 12:57:36 $
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
-- Base class for widgets which visualize an adjustment
--
module Graphics.UI.Gtk.Abstract.Range (
-- * Description
--
-- | For signals regarding a change in the range or increments, refer to
-- 'Adjustment' which is contained in the 'Range' object.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----Range
-- |                     +----'Scale'
-- |                     +----'Scrollbar'
-- @

-- * Types
  Range,
  RangeClass,
  castToRange,
  toRange,

-- * Methods
  rangeGetAdjustment,
  rangeSetAdjustment,
  UpdateType(..),
  rangeGetUpdatePolicy,
  rangeSetUpdatePolicy,
  rangeGetInverted,
  rangeSetInverted,
  rangeGetValue,
  rangeSetValue,
  rangeSetIncrements,
  rangeSetRange,
  ScrollType(..),

-- * Attributes
  rangeUpdatePolicy,
  rangeAdjustment,
  rangeInverted,
  rangeValue,

-- * Signals
  onMoveSlider,
  afterMoveSlider,
  onAdjustBounds,
  afterAdjustBounds,
  onRangeValueChanged,
  afterRangeValueChanged
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(UpdateType(..), ScrollType(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Get the 'Adjustment' which is the \"model\" object for 'Range'. See
-- 'rangeSetAdjustment' for details.
--
rangeGetAdjustment :: RangeClass self => self
 -> IO Adjustment -- ^ returns a 'Adjustment'
rangeGetAdjustment self =
  makeNewObject mkAdjustment $
  {# call unsafe range_get_adjustment #}
    (toRange self)

-- | Sets the adjustment to be used as the \"model\" object for this range
-- widget. The adjustment indicates the current range value, the minimum and
-- maximum range values, the step\/page increments used for keybindings and
-- scrolling, and the page size. The page size is normally 0 for 'Scale' and
-- nonzero for 'Scrollbar', and indicates the size of the visible area of the
-- widget being scrolled. The page size affects the size of the scrollbar
-- slider.
--
rangeSetAdjustment :: RangeClass self => self
 -> Adjustment -- ^ @adjustment@ - a 'Adjustment'
 -> IO ()
rangeSetAdjustment self adjustment =
  {# call range_set_adjustment #}
    (toRange self)
    adjustment

-- | Gets the update policy of @range@. See 'rangeSetUpdatePolicy'.
--
rangeGetUpdatePolicy :: RangeClass self => self
 -> IO UpdateType -- ^ returns the current update policy
rangeGetUpdatePolicy self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe range_get_update_policy #}
    (toRange self)

-- | Sets the update policy for the range. 'UpdateContinuous' means that
-- anytime the range slider is moved, the range value will change and the
-- value_changed signal will be emitted. 'UpdateDelayed' means that the value
-- will be updated after a brief timeout where no slider motion occurs, so
-- updates are spaced by a short time rather than continuous.
-- 'UpdateDiscontinuous' means that the value will only be updated when the
-- user releases the button and ends the slider drag operation.
--
rangeSetUpdatePolicy :: RangeClass self => self
 -> UpdateType -- ^ @policy@ - update policy
 -> IO ()
rangeSetUpdatePolicy self policy =
  {# call range_set_update_policy #}
    (toRange self)
    ((fromIntegral . fromEnum) policy)

-- | Gets the value set by 'rangeSetInverted'.
--
rangeGetInverted :: RangeClass self => self
 -> IO Bool -- ^ returns @True@ if the range is inverted
rangeGetInverted self =
  liftM toBool $
  {# call unsafe range_get_inverted #}
    (toRange self)

-- | Ranges normally move from lower to higher values as the slider moves from
-- top to bottom or left to right. Inverted ranges have higher values at the
-- top or on the right rather than on the bottom or left.
--
rangeSetInverted :: RangeClass self => self
 -> Bool  -- ^ @setting@ - @True@ to invert the range
 -> IO ()
rangeSetInverted self setting =
  {# call range_set_inverted #}
    (toRange self)
    (fromBool setting)

-- | Gets the current value of the range.
--
rangeGetValue :: RangeClass self => self
 -> IO Double -- ^ returns current value of the range.
rangeGetValue self =
  liftM realToFrac $
  {# call unsafe range_get_value #}
    (toRange self)

-- | Sets the current value of the range; if the value is outside the minimum
-- or maximum range values, it will be clamped to fit inside them. The range
-- emits the \"value_changed\" signal if the value changes.
--
rangeSetValue :: RangeClass self => self
 -> Double -- ^ @value@ - new value of the range
 -> IO ()
rangeSetValue self value =
  {# call range_set_value #}
    (toRange self)
    (realToFrac value)

-- | Sets the step and page sizes for the range. The step size is used when
-- the user clicks the 'Scrollbar' arrows or moves 'Scale' via arrow keys. The
-- page size is used for example when moving via Page Up or Page Down keys.
--
rangeSetIncrements :: RangeClass self => self
 -> Double -- ^ @step@ - step size
 -> Double -- ^ @page@ - page size
 -> IO ()
rangeSetIncrements self step page =
  {# call range_set_increments #}
    (toRange self)
    (realToFrac step)
    (realToFrac page)

-- | Sets the allowable values in the 'Range', and clamps the range value to
-- be between @min@ and @max@. (If the range has a non-zero page size, it is
-- clamped between @min@ and @max@ - page-size.)
--
rangeSetRange :: RangeClass self => self
 -> Double -- ^ @min@ - minimum range value
 -> Double -- ^ @max@ - maximum range value
 -> IO ()
rangeSetRange self min max =
  {# call range_set_range #}
    (toRange self)
    (realToFrac min)
    (realToFrac max)

--------------------
-- Attributes

-- | How the range should be updated on the screen.
--
-- Default value: 'UpdateContinuous'
--
rangeUpdatePolicy :: RangeClass self => Attr self UpdateType
rangeUpdatePolicy = newAttr
  rangeGetUpdatePolicy
  rangeSetUpdatePolicy

-- | The 'Adjustment' that contains the current value of this range object.
--
rangeAdjustment :: RangeClass self => Attr self Adjustment
rangeAdjustment = newAttr
  rangeGetAdjustment
  rangeSetAdjustment

-- | Invert direction slider moves to increase range value.
--
-- Default value: @False@
--
rangeInverted :: RangeClass self => Attr self Bool
rangeInverted = newAttr
  rangeGetInverted
  rangeSetInverted

-- | \'value\' property. See 'rangeGetValue' and 'rangeSetValue'
--
rangeValue :: RangeClass self => Attr self Double
rangeValue = newAttr
  rangeGetValue
  rangeSetValue

--------------------
-- Signals

-- | Emitted when the range value is changed either programmatically or by
-- user action.
--
onRangeValueChanged, afterRangeValueChanged :: RangeClass self => self
 -> IO ()
 -> IO (ConnectId self)
onRangeValueChanged = connect_NONE__NONE "value_changed" False
afterRangeValueChanged = connect_NONE__NONE "value_changed" True

-- | Emitted when the range is adjusted by user action. Note the value can be
-- outside the bounds of the range since it depends on the mouse position.
--
-- Usually you should use 'onRangeValueChanged' \/ 'afterRangeValueChanged'
-- instead.
--
onAdjustBounds, afterAdjustBounds :: RangeClass self => self
 -> (Double -> IO ())
 -> IO (ConnectId self)
onAdjustBounds = connect_DOUBLE__NONE "adjust_bounds" False
afterAdjustBounds = connect_DOUBLE__NONE "adjust_bounds" True

-- | Emitted when the user presses a key (e.g. Page Up, Home, Right Arrow) to
-- move the slider. The 'ScrollType' parameter gives the key that was pressed.
--
-- Usually you should use 'onRangeValueChanged' \/
-- 'afterRangeValueChanged' instead.
--
onMoveSlider, afterMoveSlider :: RangeClass self => self
 -> (ScrollType -> IO ())
 -> IO (ConnectId self)
onMoveSlider = connect_ENUM__NONE "move_slider" False
afterMoveSlider = connect_ENUM__NONE "move_slider" True
