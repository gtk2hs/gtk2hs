-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Range@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/24 09:43:24 $
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
-- @description@ --------------------------------------------------------------
--
-- * An abstract base class to handle widgets that represent some value range.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

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
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(UpdateType(..), ScrollType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @method rangeGetAdjustment@ Extract the @ref arg Adjustment@ object.
--
rangeGetAdjustment :: RangeClass r => r -> IO Adjustment
rangeGetAdjustment r = makeNewObject mkAdjustment $
  {#call unsafe range_get_adjustment#} (toRange r)

-- @method rangeSetUpdatePolicy@ Set how the internal @ref arg Adjustment@
-- object is updated.
--
rangeSetUpdatePolicy :: RangeClass r => r -> UpdateType -> IO ()
rangeSetUpdatePolicy r up = {#call range_set_update_policy#}
  (toRange r) ((fromIntegral.fromEnum) up)

-- @method rangeSetAdjustment@ Insert a new @ref arg Adjustment@ object.
--
rangeSetAdjustment :: RangeClass r => r -> Adjustment -> IO ()
rangeSetAdjustment r adj = {#call range_set_adjustment#} (toRange r) adj

-- @method rangeGetInverted@ Get the inverted flag (determines if the range is
-- reversed).
--
rangeGetInverted :: RangeClass r => r -> IO Bool
rangeGetInverted r = 
  liftM toBool $ {#call unsafe range_get_inverted#} (toRange r)

-- @method rangeSetInverted@ Set the inverted flag.
--
rangeSetInverted :: RangeClass r => r -> Bool -> IO ()
rangeSetInverted r inv = {#call range_set_inverted#} (toRange r) (fromBool inv)

-- signals

-- @signal connectToMoveSlider@ The slide has moved. The arguments give
-- detailed information what happend.
--
onMoveSlider, afterMoveSlider :: RangeClass r => r -> (ScrollType -> IO ()) ->
                                 IO (ConnectId r)
onMoveSlider = connect_ENUM__NONE "move-slider" False
afterMoveSlider = connect_ENUM__NONE "move-slider" True
