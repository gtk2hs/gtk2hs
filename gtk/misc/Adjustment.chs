-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Adjustment
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
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
-- * An Adjustment object contains a value with maximum bounds and a step size.
--   It is used to represent the value of a scoll bar and similar widgets.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module Adjustment(
  Adjustment,
  AdjustmentClass,
  castToAdjustment,
  adjustmentNew,
  adjustmentSetValue,
  adjustmentClampPage,
  connectToAdjChanged,
  connectToValueChanged
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a new Adjustment object. (EXPORTED)
--
-- * The creation function take every value that is contained in the object:
--   @value is the initial value and should be between the @upper and @lower
--   bounds of the slider. Clicking on the arrows increases this value by
--   @stepIncrement. Clicking in the slider advances by @pageIncrement. The
--   @pageSize is needed to determine if the end of the slider is still in
--   the range.
--
adjustmentNew :: Double -> Double -> Double -> Double -> Double -> Double -> 
		 IO Adjustment
adjustmentNew value lower upper stepIncrement pageIncrement pageSize =
  makeNewObject mkAdjustment $ liftM castPtr $ {#call unsafe adjustment_new#}
  (realToFrac value) (realToFrac lower) (realToFrac upper) 
  (realToFrac stepIncrement) (realToFrac pageIncrement) 
  (realToFrac pageSize)

-- Set the current value of the Adjustment object. (EXPORTED)
--
adjustmentSetValue :: Double -> Adjustment -> IO ()
adjustmentSetValue value a = 
  {#call adjustment_set_value#} (toAdjustment a) (realToFrac value)

-- Ensure that the alignment is within these bounds. (EXPORTED)
--
-- * Updates the Adjustment value to ensure that the range between lower and 
--   upper is in the current page (i.e. between value and value + page_size).
--   If the range is larger than the page size, then only the start of it will
--   be in the current page. A "changed" signal will be emitted if the value
--   is changed.
--
adjustmentClampPage :: Double -> Double -> Adjustment -> IO ()
adjustmentClampPage lower upper a = {#call adjustment_clamp_page#}
  a (realToFrac lower) (realToFrac upper)

-- signals

-- This signal is emitted if some value of Adjustment except @value itself
-- changes. (EXPORTED)
--
connectToAdjChanged :: 
  IO () -> ConnectAfter -> Adjustment -> IO (ConnectId Adjustment)
connectToAdjChanged = connect_NONE__NONE "changed"

-- This signal is emitted if the value of the Alignment object changed.
-- (EXPORTED)
--
connectToValueChanged ::
  IO () -> ConnectAfter -> Adjustment -> IO (ConnectId Adjustment)
connectToValueChanged = connect_NONE__NONE "value-changed"
