-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Range
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.4 $ from $Date: 2005/02/25 22:53:41 $
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
-- Base class for widgets which visualize an 'Adjustment'
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
  rangeSetIncrements,
  rangeSetRange,
  rangeSetValue,
  rangeGetValue,

-- * Signals
  onMoveSlider,
  afterMoveSlider
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(UpdateType(..), ScrollType(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Extract the 'Adjustment' object.
--
rangeGetAdjustment :: RangeClass r => r -> IO Adjustment
rangeGetAdjustment r = makeNewObject mkAdjustment $
  {#call unsafe range_get_adjustment#} (toRange r)

-- | Insert a new 'Adjustment' object.
--
rangeSetAdjustment :: RangeClass r => r -> Adjustment -> IO ()
rangeSetAdjustment r adj = {#call range_set_adjustment#} (toRange r) adj

-- | Get the update policy for the range widget.
--
rangeGetUpdatePolicy :: RangeClass r => r -> IO UpdateType
rangeGetUpdatePolicy r = liftM (toEnum.fromIntegral) $
  {#call unsafe range_get_update_policy#} (toRange r)

-- | Set how the internal 'Adjustment' object is updated.
--
-- * The value of 'UpdateType' determines how frequently value-changed 
--   signals are emitted on the internal 'Adjustment' object.
--
rangeSetUpdatePolicy :: RangeClass r => r -> UpdateType -> IO ()
rangeSetUpdatePolicy r up = {#call range_set_update_policy#}
  (toRange r) ((fromIntegral.fromEnum) up)

-- | Get the inverted flag (determines if the range is reversed).
--
rangeGetInverted :: RangeClass r => r -> IO Bool
rangeGetInverted r = 
  liftM toBool $ {#call unsafe range_get_inverted#} (toRange r)

-- | Set the inverted flag.
--
rangeSetInverted :: RangeClass r => r -> Bool -> IO ()
rangeSetInverted r inv = {#call range_set_inverted#} (toRange r) (fromBool inv)

-- | Gets the current value of the range.
--
rangeGetValue :: RangeClass r => r -> IO Double
rangeGetValue r = liftM realToFrac $
  {#call unsafe range_get_value#} (toRange r)

-- | Sets the current value of the range. The range emits the \"value_changed\"
-- signal if the value changes.
--
-- * If the value is outside the minimum or maximum range values, it will be
-- clamped to fit inside them.
--
rangeSetValue :: RangeClass r => r -> Double -> IO ()
rangeSetValue r value =
  {#call range_set_value#} (toRange r) (realToFrac value)

-- | Sets the step and page sizes for the range. 
-- The step size is used when the
-- user clicks the "Scrollbar" arrows or moves "Scale" via arrow keys. The
-- page size is used for example when moving via Page Up or Page Down keys.
--
rangeSetIncrements :: RangeClass r => r
                   -> Double  -- ^ step size
                   -> Double  -- ^ page size
                   -> IO ()
rangeSetIncrements r step page =
 {#call range_set_increments#} (toRange r) (realToFrac step) (realToFrac page)

-- | Sets the allowable values in the 'Range', and clamps the range value to be
-- between min and max.
--
rangeSetRange :: RangeClass r => r
              -> Double  -- ^ min
              -> Double  -- ^ max
              -> IO ()
rangeSetRange r min max =
 {#call range_set_range#} (toRange r) (realToFrac min) (realToFrac max)

--------------------
-- Signals

-- | The slide has moved. The arguments give
-- detailed information what happend.
--
onMoveSlider, afterMoveSlider :: RangeClass r => r -> (ScrollType -> IO ()) ->
                                 IO (ConnectId r)
onMoveSlider = connect_ENUM__NONE "move-slider" False
afterMoveSlider = connect_ENUM__NONE "move-slider" True
