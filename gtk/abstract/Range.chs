-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Range
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:19 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
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
--- DESCRIPTION ---------------------------------------------------------------
--
-- * An abstract base class to handle widgets that represent some value range.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

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
  connectToMoveSlider
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

-- Extract the @Adjustment object. (EXPORTED)
--
rangeGetAdjustment :: RangeClass r => r -> IO Adjustment
rangeGetAdjustment r = makeNewObject mkAdjustment $
  {#call unsafe range_get_adjustment#} (toRange r)

-- Set how the internal @Adjustment object is updated. (EXPORTED)
--
rangeSetUpdatePolicy :: RangeClass r => UpdateType -> r -> IO ()
rangeSetUpdatePolicy up r = {#call range_set_update_policy#}
  (toRange r) ((fromIntegral.fromEnum) up)

-- Insert a new @Adjustment object. (EXPORTED)
--
rangeSetAdjustment :: RangeClass r => Adjustment -> r -> IO ()
rangeSetAdjustment adj r = {#call range_set_adjustment#} (toRange r) adj

-- Get the inverted flag (determines if the range is reversed). (EXPORTED)
--
rangeGetInverted :: RangeClass r => r -> IO Bool
rangeGetInverted r = 
  liftM toBool $ {#call unsafe range_get_inverted#} (toRange r)

-- Set the inverted flag. (EXPORTED)
--
rangeSetInverted :: RangeClass r => Bool -> r -> IO ()
rangeSetInverted inv r = {#call range_set_inverted#} (toRange r) (fromBool inv)

-- signals

-- The slide has moved. The arguments give detailed information what happend.
-- (EXPORTED)
--
connectToMoveSlider :: RangeClass r => 
  (ScrollType -> IO ()) -> ConnectAfter -> r -> IO (ConnectId r)
connectToMoveSlider = connect_ENUM__NONE "move-slider"
