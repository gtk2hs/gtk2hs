-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Range
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.5 $ from $Date: 2004/05/23 15:46:02 $
--
--  Copyright (c) 1999..2002 Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- |
--
-- An abstract base class to handle widgets that represent some value range.
--

module Range(
  Range,
  RangeClass,
  castToRange,
  rangeGetAdjustment,
  UpdateType(..),
  rangeSetUpdatePolicy,
  rangeSetAdjustment,
  rangeGetInverted,
  rangeSetInverted,
  ScrollType(..),
  onMoveSlider,
  afterMoveSlider
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(UpdateType(..), ScrollType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Extract the 'Adjustment' object.
--
rangeGetAdjustment :: RangeClass r => r -> IO Adjustment
rangeGetAdjustment r = makeNewObject mkAdjustment $
  {#call unsafe range_get_adjustment#} (toRange r)

-- | Set how the internal 'Adjustment'
-- object is updated.
--
rangeSetUpdatePolicy :: RangeClass r => r -> UpdateType -> IO ()
rangeSetUpdatePolicy r up = {#call range_set_update_policy#}
  (toRange r) ((fromIntegral.fromEnum) up)

-- | Insert a new 'Adjustment' object.
--
rangeSetAdjustment :: RangeClass r => r -> Adjustment -> IO ()
rangeSetAdjustment r adj = {#call range_set_adjustment#} (toRange r) adj

-- | Get the inverted flag (determines if the range is
-- reversed).
--
rangeGetInverted :: RangeClass r => r -> IO Bool
rangeGetInverted r = 
  liftM toBool $ {#call unsafe range_get_inverted#} (toRange r)

-- | Set the inverted flag.
--
rangeSetInverted :: RangeClass r => r -> Bool -> IO ()
rangeSetInverted r inv = {#call range_set_inverted#} (toRange r) (fromBool inv)

-- signals

-- | The slide has moved. The arguments give
-- detailed information what happend.
--
onMoveSlider, afterMoveSlider :: RangeClass r => r -> (ScrollType -> IO ()) ->
                                 IO (ConnectId r)
onMoveSlider = connect_ENUM__NONE "move-slider" False
afterMoveSlider = connect_ENUM__NONE "move-slider" True
