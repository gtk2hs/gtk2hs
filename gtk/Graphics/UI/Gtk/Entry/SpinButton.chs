{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SpinButton
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
-- Retrieve an integer or floating-point number from the user
--
module Graphics.UI.Gtk.Entry.SpinButton (
-- * Detail
--
-- | A 'SpinButton' is an ideal way to allow the user to set the value of some
-- attribute. Rather than having to directly type a number into a 'Entry',
-- 'SpinButton' allows the user to click on one of two arrows to increment or
-- decrement the displayed value. A value can still be typed in, with the bonus
-- that it can be checked to ensure it is in a given range.
--
-- The main properties of a 'SpinButton' are through a 'Adjustment'. See the
-- 'Adjustment' section for more details about an adjustment's properties.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Entry'
-- |                     +----SpinButton
-- @

-- * Types
  SpinButton,
  SpinButtonClass,
  castToSpinButton, gTypeSpinButton,
  toSpinButton,

-- * Constructors
  spinButtonNew,
  spinButtonNewWithRange,

-- * Methods
  spinButtonConfigure,
  spinButtonSetAdjustment,
  spinButtonGetAdjustment,
  spinButtonSetDigits,
  spinButtonGetDigits,
  spinButtonSetIncrements,
  spinButtonGetIncrements,
  spinButtonSetRange,
  spinButtonGetRange,
  spinButtonGetValue,
  spinButtonGetValueAsInt,
  spinButtonSetValue,
  SpinButtonUpdatePolicy(..),
  spinButtonSetUpdatePolicy,
  spinButtonGetUpdatePolicy,
  spinButtonSetNumeric,
  spinButtonGetNumeric,
  SpinType(..),
  spinButtonSpin,
  spinButtonSetWrap,
  spinButtonGetWrap,
  spinButtonSetSnapToTicks,
  spinButtonGetSnapToTicks,
  spinButtonUpdate,

-- * Attributes
  spinButtonAdjustment,
  spinButtonClimbRate,
  spinButtonDigits,
  spinButtonSnapToTicks,
  spinButtonNumeric,
  spinButtonWrap,
  spinButtonUpdatePolicy,
  spinButtonValue,

-- * Signals
  onInput,
  afterInput,
  onOutput,
  afterOutput,
  onValueSpinned,
  afterValueSpinned
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Structs  (inputError)
import Graphics.UI.Gtk.General.Enums    (SpinButtonUpdatePolicy(..), SpinType(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Interfaces

instance EditableClass SpinButton

--------------------
-- Constructors

-- | Creates a new 'SpinButton'.
--
spinButtonNew ::
    Adjustment    -- ^ @adjustment@ - the 'Adjustment' object that this spin
                  -- button should use.
 -> Double        -- ^ @climbRate@ - specifies how much the spin button
                  -- changes when an arrow is clicked on.
 -> Int           -- ^ @digits@ - the number of decimal places to display.
 -> IO SpinButton
spinButtonNew adjustment climbRate digits =
  makeNewObject mkSpinButton $
  liftM (castPtr :: Ptr Widget -> Ptr SpinButton) $
  {# call spin_button_new #}
    adjustment
    (realToFrac climbRate)
    (fromIntegral digits)

-- | This is a convenience constructor that allows creation of a numeric
-- 'SpinButton' without manually creating an adjustment. The value is initially
-- set to the minimum value and a page increment of 10 * @step@ is the default.
-- The precision of the spin button is equivalent to the precision of @step@.
--
-- Note that the way in which the precision is derived works best if @step@
-- is a power of ten. If the resulting precision is not suitable for your
-- needs, use 'spinButtonSetDigits' to correct it.
--
spinButtonNewWithRange ::
    Double        -- ^ @min@ - Minimum allowable value
 -> Double        -- ^ @max@ - Maximum allowable value
 -> Double        -- ^ @step@ - Increment added or subtracted by spinning the
                  -- widget
 -> IO SpinButton
spinButtonNewWithRange min max step =
  makeNewObject mkSpinButton $
  liftM (castPtr :: Ptr Widget -> Ptr SpinButton) $
  {# call unsafe spin_button_new_with_range #}
    (realToFrac min)
    (realToFrac max)
    (realToFrac step)

--------------------
-- Methods

-- | Changes the properties of an existing spin button. The adjustment, climb
-- rate, and number of decimal places are all changed accordingly, after this
-- function call.
--
spinButtonConfigure :: SpinButtonClass self => self
 -> Adjustment -- ^ @adjustment@ - a 'Adjustment'.
 -> Double     -- ^ @climbRate@ - the new climb rate.
 -> Int        -- ^ @digits@ - the number of decimal places to display in the
               -- spin button.
 -> IO ()
spinButtonConfigure self adjustment climbRate digits =
  {# call spin_button_configure #}
    (toSpinButton self)
    adjustment
    (realToFrac climbRate)
    (fromIntegral digits)

-- | Replaces the 'Adjustment' associated with the spin button.
--
spinButtonSetAdjustment :: SpinButtonClass self => self
 -> Adjustment -- ^ @adjustment@ - a 'Adjustment' to replace the existing
               -- adjustment
 -> IO ()
spinButtonSetAdjustment self adjustment =
  {# call spin_button_set_adjustment #}
    (toSpinButton self)
    adjustment

-- | Get the adjustment associated with a 'SpinButton'
--
spinButtonGetAdjustment :: SpinButtonClass self => self
 -> IO Adjustment -- ^ returns the 'Adjustment' of @spinButton@
spinButtonGetAdjustment self =
  makeNewObject mkAdjustment $
  {# call unsafe spin_button_get_adjustment #}
    (toSpinButton self)

-- | Set the precision to be displayed by @spinButton@. Up to 20 digit
-- precision is allowed.
--
spinButtonSetDigits :: SpinButtonClass self => self
 -> Int   -- ^ @digits@ - the number of digits after the decimal point to be
          -- displayed for the spin button's value
 -> IO ()
spinButtonSetDigits self digits =
  {# call spin_button_set_digits #}
    (toSpinButton self)
    (fromIntegral digits)

-- | Fetches the precision of @spinButton@. See 'spinButtonSetDigits'.
--
spinButtonGetDigits :: SpinButtonClass self => self
 -> IO Int -- ^ returns the current precision
spinButtonGetDigits self =
  liftM fromIntegral $
  {# call spin_button_get_digits #}
    (toSpinButton self)

-- | Sets the step and page increments for the spin button. This affects how
-- quickly the value changes when the spin button's arrows are activated.
--
spinButtonSetIncrements :: SpinButtonClass self => self
 -> Double -- ^ @step@ - increment applied for a button 1 press.
 -> Double -- ^ @page@ - increment applied for a button 2 press.
 -> IO ()
spinButtonSetIncrements self step page =
  {# call spin_button_set_increments #}
    (toSpinButton self)
    (realToFrac step)
    (realToFrac page)

-- | Gets the current step and page the increments used by the spin button. See
-- 'spinButtonSetIncrements'.
--
spinButtonGetIncrements :: SpinButtonClass self => self
 -> IO (Double, Double) -- ^ @(step, page)@ - step increment and page increment
spinButtonGetIncrements self =
  alloca $ \stepPtr ->
  alloca $ \pagePtr -> do
  {# call unsafe spin_button_get_increments #}
    (toSpinButton self)
    stepPtr
    pagePtr
  step <- peek stepPtr
  page <- peek pagePtr
  return (realToFrac step, realToFrac page)

-- | Sets the minimum and maximum allowable values for the spin button
--
spinButtonSetRange :: SpinButtonClass self => self
 -> Double -- ^ @min@ - minimum allowable value
 -> Double -- ^ @max@ - maximum allowable value
 -> IO ()
spinButtonSetRange self min max =
  {# call spin_button_set_range #}
    (toSpinButton self)
    (realToFrac min)
    (realToFrac max)

-- | Gets the range allowed for the spin button. See 'spinButtonSetRange'.
--
spinButtonGetRange :: SpinButtonClass self => self
 -> IO (Double, Double) -- ^ @(min, max)@ - minimum and maximum allowed value
spinButtonGetRange self =
  alloca $ \minPtr ->
  alloca $ \maxPtr -> do
  {# call unsafe spin_button_get_range #}
    (toSpinButton self)
    minPtr
    maxPtr
  min <- peek minPtr
  max <- peek maxPtr
  return (realToFrac min, realToFrac max)

-- | Get the value of the spin button as a floating point value.
--
spinButtonGetValue :: SpinButtonClass self => self -> IO Double
spinButtonGetValue self =
  liftM realToFrac $
  {# call unsafe spin_button_get_value #}
    (toSpinButton self)

-- | Get the value of the spin button as an integral value.
--
spinButtonGetValueAsInt :: SpinButtonClass self => self -> IO Int
spinButtonGetValueAsInt self =
  liftM fromIntegral $
  {# call unsafe spin_button_get_value_as_int #}
    (toSpinButton self)

-- | Set the value of the spin button.
--
spinButtonSetValue :: SpinButtonClass self => self -> Double -> IO ()
spinButtonSetValue self value =
  {# call spin_button_set_value #}
    (toSpinButton self)
    (realToFrac value)

-- | Sets the update behavior of a spin button. This determines whether the
-- spin button is always updated or only when a valid value is set.
--
spinButtonSetUpdatePolicy :: SpinButtonClass self => self
 -> SpinButtonUpdatePolicy -- ^ @policy@ - a 'SpinButtonUpdatePolicy' value
 -> IO ()
spinButtonSetUpdatePolicy self policy =
  {# call spin_button_set_update_policy #}
    (toSpinButton self)
    ((fromIntegral . fromEnum) policy)

-- | Gets the update behavior of a spin button. See
-- 'spinButtonSetUpdatePolicy'.
--
spinButtonGetUpdatePolicy :: SpinButtonClass self => self
 -> IO SpinButtonUpdatePolicy -- ^ returns the current update policy
spinButtonGetUpdatePolicy self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe spin_button_get_update_policy #}
    (toSpinButton self)

-- | Sets the flag that determines if non-numeric text can be typed into the
-- spin button.
--
spinButtonSetNumeric :: SpinButtonClass self => self
 -> Bool  -- ^ @numeric@ - flag indicating if only numeric entry is allowed.
 -> IO ()
spinButtonSetNumeric self numeric =
  {# call spin_button_set_numeric #}
    (toSpinButton self)
    (fromBool numeric)

-- | Returns whether non-numeric text can be typed into the spin button. See
-- 'spinButtonSetNumeric'.
--
spinButtonGetNumeric :: SpinButtonClass self => self
 -> IO Bool -- ^ returns @True@ if only numeric text can be entered
spinButtonGetNumeric self =
  liftM toBool $
  {# call unsafe spin_button_get_numeric #}
    (toSpinButton self)

-- | Increment or decrement a spin button's value in a specified direction by
-- a specified amount.
--
spinButtonSpin :: SpinButtonClass self => self
 -> SpinType -- ^ @direction@ - a 'SpinType' indicating the direction to spin.
 -> Double   -- ^ @increment@ - step increment to apply in the specified
             -- direction.
 -> IO ()
spinButtonSpin self direction increment =
  {# call spin_button_spin #}
    (toSpinButton self)
    ((fromIntegral . fromEnum) direction)
    (realToFrac increment)

-- | Sets the flag that determines if a spin button value wraps around to the
-- opposite limit when the upper or lower limit of the range is exceeded.
--
spinButtonSetWrap :: SpinButtonClass self => self
 -> Bool  -- ^ @wrap@ - a flag indicating if wrapping behavior is performed.
 -> IO ()
spinButtonSetWrap self wrap =
  {# call spin_button_set_wrap #}
    (toSpinButton self)
    (fromBool wrap)

-- | Returns whether the spin button's value wraps around to the opposite
-- limit when the upper or lower limit of the range is exceeded. See
-- 'spinButtonSetWrap'.
--
spinButtonGetWrap :: SpinButtonClass self => self
 -> IO Bool -- ^ returns @True@ if the spin button wraps around
spinButtonGetWrap self =
  liftM toBool $
  {# call spin_button_get_wrap #}
    (toSpinButton self)

-- | Sets the policy as to whether values are corrected to the nearest step
-- increment when a spin button is activated after providing an invalid value.
--
spinButtonSetSnapToTicks :: SpinButtonClass self => self
 -> Bool  -- ^ @snapToTicks@ - a flag indicating if invalid values should be
          -- corrected.
 -> IO ()
spinButtonSetSnapToTicks self snapToTicks =
  {# call spin_button_set_snap_to_ticks #}
    (toSpinButton self)
    (fromBool snapToTicks)

-- | Returns whether the values are corrected to the nearest step. See
-- 'spinButtonSetSnapToTicks'.
--
spinButtonGetSnapToTicks :: SpinButtonClass self => self
 -> IO Bool -- ^ returns @True@ if values are snapped to the nearest step.
spinButtonGetSnapToTicks self =
  liftM toBool $
  {# call unsafe spin_button_get_snap_to_ticks #}
    (toSpinButton self)

-- | Manually force an update of the spin button.
--
spinButtonUpdate :: SpinButtonClass self => self -> IO ()
spinButtonUpdate self =
  {# call spin_button_update #}
    (toSpinButton self)

--------------------
-- Attributes

-- | The adjustment that holds the value of the spinbutton.
--
spinButtonAdjustment :: SpinButtonClass self => Attr self Adjustment
spinButtonAdjustment = newAttr
  spinButtonGetAdjustment
  spinButtonSetAdjustment

-- | The acceleration rate when you hold down a button.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
spinButtonClimbRate :: SpinButtonClass self => Attr self Double
spinButtonClimbRate = newAttrFromDoubleProperty "climb-rate"

-- | The number of decimal places to display.
--
-- Allowed values: \<= 20
--
-- Default value: 0
--
spinButtonDigits :: SpinButtonClass self => Attr self Int
spinButtonDigits = newAttr
  spinButtonGetDigits
  spinButtonSetDigits

-- | Whether erroneous values are automatically changed to a spin button's
-- nearest step increment.
--
-- Default value: @False@
--
spinButtonSnapToTicks :: SpinButtonClass self => Attr self Bool
spinButtonSnapToTicks = newAttr
  spinButtonGetSnapToTicks
  spinButtonSetSnapToTicks

-- | Whether non-numeric characters should be ignored.
--
-- Default value: @False@
--
spinButtonNumeric :: SpinButtonClass self => Attr self Bool
spinButtonNumeric = newAttr
  spinButtonGetNumeric
  spinButtonSetNumeric

-- | Whether a spin button should wrap upon reaching its limits.
--
-- Default value: @False@
--
spinButtonWrap :: SpinButtonClass self => Attr self Bool
spinButtonWrap = newAttr
  spinButtonGetWrap
  spinButtonSetWrap

-- | Whether the spin button should update always, or only when the value is
-- legal.
--
-- Default value: 'UpdateAlways'
--
spinButtonUpdatePolicy :: SpinButtonClass self => Attr self SpinButtonUpdatePolicy
spinButtonUpdatePolicy = newAttr
  spinButtonGetUpdatePolicy
  spinButtonSetUpdatePolicy

-- | Reads the current value, or sets a new value.
--
-- Default value: 0
--
spinButtonValue :: SpinButtonClass self => Attr self Double
spinButtonValue = newAttr
  spinButtonGetValue
  spinButtonSetValue

--------------------
-- Signals

-- | Install a custom input handler.
--
-- * This signal is called upon each time the value of the SpinButton is set
--   by spinButtonSetValue. The function can return Nothing if the value is no
--   good.
--
onInput, afterInput :: SpinButtonClass sb => sb -> (IO (Maybe Double)) ->
                       IO (ConnectId sb)
onInput sb user = connect_PTR__INT "input" False sb $ \dPtr -> do
  mVal <- user
  case mVal of
    (Just val) -> do
      poke dPtr ((realToFrac val)::{#type gdouble#})
      return 0
    Nothing -> return (fromIntegral inputError)
afterInput sb user = connect_PTR__INT "input" True sb $ \dPtr -> do
  mVal <- user
  case mVal of
    (Just val) -> do
      poke dPtr ((realToFrac val)::{#type gdouble#})
      return 0
    Nothing -> return (fromIntegral inputError)

-- | Install a custom output handler.
--
-- * This handler makes it possible to query the current value and to render
--   something completely different to the screen using entrySetText. The
--   return value must be False in order to let the default output routine run
--   after this signal returns.
--
onOutput, afterOutput :: SpinButtonClass sb => sb -> IO Bool ->
                         IO (ConnectId sb)
onOutput = connect_NONE__BOOL "output" False
afterOutput = connect_NONE__BOOL "output" True

-- | The value of the spin button has changed.
--
onValueSpinned, afterValueSpinned :: SpinButtonClass sb => sb -> IO () ->
                                     IO (ConnectId sb)
onValueSpinned = connect_NONE__NONE "value-changed" False
afterValueSpinned = connect_NONE__NONE "value-changed" True

