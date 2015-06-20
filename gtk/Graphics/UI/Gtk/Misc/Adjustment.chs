{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Adjustment
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
-- A 'Object' representing an adjustable bounded value
--
module Graphics.UI.Gtk.Misc.Adjustment (
-- * Detail
--
-- | The 'Adjustment' object represents a value which has an associated lower
-- and upper bound, together with step and page increments, and a page size. It
-- is used within several Gtk+ widgets, including 'SpinButton', 'Viewport', and
-- 'Range' (which is a base class for 'HScrollbar', 'VScrollbar', 'HScale', and
-- 'VScale').
--
-- The 'Adjustment' object does not update the value itself. Instead it is
-- left up to the owner of the 'Adjustment' to control the value.
--
-- The owner of the 'Adjustment' typically calls the
-- 'adjustmentValueChanged' and 'adjustmentChanged' functions after changing
-- the value and its bounds. This results in the emission of the
-- \"value_changed\" or \"changed\" signal respectively.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----Adjustment
-- @

-- * Types
  Adjustment,
  AdjustmentClass,
  castToAdjustment, gTypeAdjustment,
  toAdjustment,

-- * Constructors
  adjustmentNew,

-- * Methods
  adjustmentSetLower,
  adjustmentGetLower,
  adjustmentSetPageIncrement,
  adjustmentGetPageIncrement,
  adjustmentSetPageSize,
  adjustmentGetPageSize,
  adjustmentSetStepIncrement,
  adjustmentGetStepIncrement,
  adjustmentSetUpper,
  adjustmentGetUpper,
  adjustmentSetValue,
  adjustmentGetValue,
  adjustmentClampPage,
  adjustmentAdjChanged,
  adjustmentValueChanged,

-- * Attributes
#if GTK_CHECK_VERSION(2,4,0)
  adjustmentValue,
  adjustmentLower,
  adjustmentUpper,
  adjustmentStepIncrement,
  adjustmentPageIncrement,
  adjustmentPageSize,
#endif

-- * Signals
  onAdjChanged,
  afterAdjChanged,
  onValueChanged,
  afterValueChanged,
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'Adjustment'.
--
-- The creation function take every value that is contained in the object:
-- @value@ is the initial value and should be between the @upper@ and @lower@
-- bounds of the slider. Clicking on the arrows increases this value by
-- @stepIncrement@. Clicking in the slider advances by @pageIncrement@. The
-- @pageSize@ is needed to determine if the end of the slider is still in the
-- range.
--
adjustmentNew ::
    Double        -- ^ @value@ - the initial value.
 -> Double        -- ^ @lower@ - the minimum value.
 -> Double        -- ^ @upper@ - the maximum value.
 -> Double        -- ^ @stepIncrement@ - the step increment.
 -> Double        -- ^ @pageIncrement@ - the page increment.
 -> Double        -- ^ @pageSize@ - the page size.
 -> IO Adjustment
adjustmentNew value lower upper stepIncrement pageIncrement pageSize =
  makeNewObject mkAdjustment $ liftM castPtr $
  {# call unsafe adjustment_new #}
    (realToFrac value)
    (realToFrac lower)
    (realToFrac upper)
    (realToFrac stepIncrement)
    (realToFrac pageIncrement)
    (realToFrac pageSize)

--------------------
-- Methods

-- | Set the lower value.
adjustmentSetLower :: Adjustment -> Double -> IO ()
adjustmentSetLower = objectSetPropertyDouble "lower"

-- | Retrieve the lower value.
adjustmentGetLower :: Adjustment -> IO Double
adjustmentGetLower = objectGetPropertyDouble "lower"

-- | Set the page increment value.
adjustmentSetPageIncrement :: Adjustment -> Double -> IO ()
adjustmentSetPageIncrement = objectSetPropertyDouble "page-increment"

-- | Retrieve the pageincrement value.
adjustmentGetPageIncrement :: Adjustment -> IO Double
adjustmentGetPageIncrement = objectGetPropertyDouble "page-increment"

-- | Set the page size value.
adjustmentSetPageSize :: Adjustment -> Double -> IO ()
adjustmentSetPageSize = objectSetPropertyDouble "page_size"

-- | Retrieve the page size value.
adjustmentGetPageSize :: Adjustment -> IO Double
adjustmentGetPageSize = objectGetPropertyDouble "page_size"

-- | Set the step-increment value.
adjustmentSetStepIncrement :: Adjustment -> Double -> IO ()
adjustmentSetStepIncrement = objectSetPropertyDouble "step-increment"

-- | Retrieve the step-increment value.
adjustmentGetStepIncrement :: Adjustment -> IO Double
adjustmentGetStepIncrement = objectGetPropertyDouble "step-increment"

-- | Set the upper value.
adjustmentSetUpper :: Adjustment -> Double -> IO ()
adjustmentSetUpper = objectSetPropertyDouble "upper"

-- | Retrieve the upper value.
adjustmentGetUpper :: Adjustment -> IO Double
adjustmentGetUpper = objectGetPropertyDouble "upper"

-- | Sets the current value of the Adjustment object. The value is clamped to
-- lie between the adjustment's @lower@ and @upper@ values. See 'adjustmentNew'
-- for details of these properties.
--
-- Note that for adjustments which are used in a 'Scrollbar', the effective
-- range of allowed values goes from @lower@ to @upper - page_size@.
--
adjustmentSetValue :: Adjustment -> Double -> IO ()
adjustmentSetValue self value =
  {# call adjustment_set_value #}
    self
    (realToFrac value)

-- | Gets the current value of the adjustment. See 'adjustmentSetValue'.
--
adjustmentGetValue :: Adjustment -> IO Double
adjustmentGetValue self =
  liftM realToFrac $
  {# call adjustment_get_value #}
    self

-- | Updates the 'Adjustment' @value@ to ensure that the range between @lower@
-- and @upper@ is in the current page (i.e. between @value@ and @value +
-- pageSize@). If the range is larger than the page size, then only the start
-- of it will be in the current page. A \"changed\" signal will be emitted if
-- the value is changed.
--
adjustmentClampPage :: Adjustment
 -> Double     -- ^ @lower@ - the lower value.
 -> Double     -- ^ @upper@ - the upper value.
 -> IO ()
adjustmentClampPage self lower upper =
  {# call adjustment_clamp_page #}
    self
    (realToFrac lower)
    (realToFrac upper)

-- | Emit the 'onAdjChanged' signal.
--
adjustmentAdjChanged :: Adjustment -> IO ()
adjustmentAdjChanged = {#call adjustment_changed#}

-- | Emit the 'onValueChanged' signal.
--
-- * When adjusting the or bounds, this function can be called to enforce a
--   visual update of the containing widget.
--
adjustmentValueChanged :: Adjustment -> IO ()
adjustmentValueChanged = {#call adjustment_value_changed#}

--------------------
-- Attributes

#if GTK_CHECK_VERSION(2,4,0)
-- | The value of the adjustment.
--
-- Default value: 0
--
adjustmentValue :: Attr Adjustment Double
adjustmentValue = newAttr
  adjustmentGetValue
  adjustmentSetValue

-- | The minimum value of the adjustment.
--
-- Default value: 0
--
adjustmentLower :: Attr Adjustment Double
adjustmentLower = newAttrFromDoubleProperty "lower"

-- | The maximum value of the adjustment. Note that values will be restricted
-- by @upper - page-size@ if the page-size property is nonzero.
--
-- Default value: 0
--
adjustmentUpper :: Attr Adjustment Double
adjustmentUpper = newAttrFromDoubleProperty "upper"

-- | The step increment of the adjustment.
--
-- Default value: 0
--
adjustmentStepIncrement :: Attr Adjustment Double
adjustmentStepIncrement = newAttrFromDoubleProperty "step-increment"

-- | The page increment of the adjustment.
--
-- Default value: 0
--
adjustmentPageIncrement :: Attr Adjustment Double
adjustmentPageIncrement = newAttrFromDoubleProperty "page-increment"

-- | The page size of the adjustment. Note that the page-size is irrelevant
-- and should be set to zero if the adjustment is used for a simple scalar
-- value, e.g. in a 'SpinButton'.
--
-- Default value: 0
--
adjustmentPageSize :: Attr Adjustment Double
adjustmentPageSize = newAttrFromDoubleProperty "page-size"
#endif

--------------------
-- Signals

-- | Emitted when one or more of the 'Adjustment' fields have been changed,
-- other than the value field.
--
onAdjChanged, afterAdjChanged :: Adjustment
 -> IO ()
 -> IO (ConnectId Adjustment)
onAdjChanged = connect_NONE__NONE "changed" False
afterAdjChanged = connect_NONE__NONE "changed" True

-- | Emitted when the 'Adjustment' value field has been changed.
--
onValueChanged, afterValueChanged :: Adjustment
 -> IO ()
 -> IO (ConnectId Adjustment)
onValueChanged = connect_NONE__NONE "value-changed" False
afterValueChanged = connect_NONE__NONE "value-changed" True
