-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Adjustment@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2003/07/09 22:42:44 $
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
-- * An Adjustment object contains a value with maximum bounds and a step size.
--   It is used to represent the value of a scoll bar and similar widgets.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

module Adjustment(
  Adjustment,
  AdjustmentClass,
  castToAdjustment,
  adjustmentNew,
  adjustmentSetValue,
  adjustmentClampPage,
  onAdjChanged,
  afterAdjChanged,
  onValueChanged,
  afterValueChanged
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor adjustmentNew@ Create a new Adjustment object.
--
-- * The creation function take every value that is contained in the object:
--   @ref arg value@ is the initial value and should be between the
--   @ref arg upper@ and @ref arg lower@ bounds of the slider. Clicking on the
--   arrows increases this value by @ref arg stepIncrement@. Clicking in the
--   slider advances by @ref arg pageIncrement@. The @ref arg pageSize@ is
--   needed to determine if the end of the slider is still in the range.
--
adjustmentNew :: Double -> Double -> Double -> Double -> Double -> Double ->
                 IO Adjustment
adjustmentNew pageSize value lower upper stepIncrement pageIncrement =
  makeNewObject mkAdjustment $ liftM castPtr $ {#call unsafe adjustment_new#}
  (realToFrac value) (realToFrac lower) (realToFrac upper) 
  (realToFrac stepIncrement) (realToFrac pageIncrement) 
  (realToFrac pageSize)

-- @method adjustmentSetValue@ Set the current value of the Adjustment object.
--
adjustmentSetValue :: Adjustment -> Double -> IO ()
adjustmentSetValue a value = 
  {#call adjustment_set_value#} (toAdjustment a) (realToFrac value)

-- @method adjustmentClampPage@ Ensure that the alignment is within these
-- bounds.
--
-- * Updates the Adjustment value to ensure that the range between lower and
--   upper is in the current page (i.e. between value and value + page_size).
--   If the range is larger than the page size, then only the start of it will
--   be in the current page. A "changed" signal will be emitted if the value
--   is changed.
--
adjustmentClampPage :: Adjustment -> Double -> Double -> IO ()
adjustmentClampPage a lower upper = {#call adjustment_clamp_page#}
  a (realToFrac lower) (realToFrac upper)

-- signals

-- @signal connectToAdjChanged@ This signal is emitted if some value of
-- Adjustment except @ref arg value@ itself changes.
--
onAdjChanged, afterAdjChanged :: Adjustment -> IO () ->
                                 IO (ConnectId Adjustment)
onAdjChanged = connect_NONE__NONE "changed" False
afterAdjChanged = connect_NONE__NONE "changed" True

-- @signal connectToValueChanged@ This signal is emitted if the value of the
-- Alignment object changed.
--
onValueChanged, afterValueChanged :: Adjustment -> IO () ->
                                     IO (ConnectId Adjustment)
onValueChanged = connect_NONE__NONE "value-changed" False
afterValueChanged = connect_NONE__NONE "value-changed" True
