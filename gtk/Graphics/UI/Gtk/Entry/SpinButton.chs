-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SpinButton
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.5 $ from $Date: 2005/03/13 19:34:33 $
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
-- A spin button provides the possiblity to enter a numeric value without using
-- the keyboard.
--
module Graphics.UI.Gtk.Entry.SpinButton (
-- * Description
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
  castToSpinButton,

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

-- * Properties
  spinButtonAdjustment,
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

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes		(Attr(..))
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Structs	(inputError)
import Graphics.UI.Gtk.General.Enums	(SpinButtonUpdatePolicy(..), SpinType(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Interfaces

instance EditableClass SpinButton

--------------------
-- Constructors

-- | Create a new SpinButton.
--
-- * @climbRate@ is the amount by which the value is changed each time
--   the up\/down buttons are pressed.
--
-- * @digits@ is the number of shown digits. Set to 0 to work with
--   integer values.
--
spinButtonNew :: Adjustment -> Double -> Int -> IO SpinButton
spinButtonNew adj climbRate digits = makeNewObject mkSpinButton $ 
  liftM castPtr $ {#call unsafe spin_button_new#} adj (realToFrac climbRate)
  (fromIntegral digits)

-- | Create a new SpinButton with a restricted
-- range.
--
-- * This is a convenience function because the user does not have to create
--   an Adjustment first. Page increments are set to 10 * @step@.
--
spinButtonNewWithRange :: Double -> Double -> Double -> IO SpinButton
spinButtonNewWithRange min max step = makeNewObject mkSpinButton $
  liftM castPtr $ {#call unsafe spin_button_new_with_range#}
  (realToFrac min) (realToFrac max) (realToFrac step)

--------------------
-- Methods

-- | Change the settings of a SpinButton.
--
spinButtonConfigure :: SpinButtonClass sb => sb -> Adjustment -> Double ->
                       Int -> IO ()
spinButtonConfigure sb adj climbRate digits = {#call spin_button_configure#}
  (toSpinButton sb) adj (realToFrac climbRate) (fromIntegral digits)

-- | Attach a new Adjustment object to the
-- SpinButton.
--
spinButtonSetAdjustment :: SpinButtonClass sb => sb -> Adjustment -> IO ()
spinButtonSetAdjustment sb adj = {#call spin_button_set_adjustment#}
  (toSpinButton sb) adj

-- | Retrieve the Adjustment object that is
-- currently controlling the SpinButton.
--
spinButtonGetAdjustment :: SpinButtonClass sb => sb -> IO Adjustment
spinButtonGetAdjustment sb = makeNewObject mkAdjustment $
  {#call unsafe spin_button_get_adjustment#} (toSpinButton sb)

-- | Sets the number of shown digits.
--
spinButtonSetDigits :: SpinButtonClass sb => sb -> Int -> IO ()
spinButtonSetDigits sb digits = {#call spin_button_set_digits#} 
  (toSpinButton sb) (fromIntegral digits)

-- | Gets the number of digits shown.
--
spinButtonGetDigits :: SpinButtonClass sb => sb -> IO Int
spinButtonGetDigits sb = liftM fromIntegral $
  {#call spin_button_get_digits#} (toSpinButton sb)

-- | Sets the increment for up\/down buttons.
--
spinButtonSetIncrements :: SpinButtonClass sb => sb -> Double -> Double ->
                           IO ()
spinButtonSetIncrements sb step page = {#call spin_button_set_increments#}
  (toSpinButton sb) (realToFrac step) (realToFrac page)

-- | Sets the increment for up\/down buttons.
--
spinButtonGetIncrements :: SpinButtonClass sb => sb -> IO (Double, Double)
spinButtonGetIncrements sb =
  alloca $ \stepPtr -> alloca $ \pagePtr -> do
  {#call unsafe spin_button_get_increments#} (toSpinButton sb) stepPtr pagePtr
  step <- peek stepPtr
  page <- peek pagePtr
  return (realToFrac step, realToFrac page)

-- | Set the maximal allowable range for the spinbutton.
--
spinButtonSetRange :: SpinButtonClass sb => sb -> Double -> Double -> IO ()
spinButtonSetRange sb min max = {#call spin_button_set_range#}
  (toSpinButton sb) (realToFrac min) (realToFrac max)

-- | Get the maximal allowable range for the spinbutton.
--
spinButtonGetRange :: SpinButtonClass sb => sb -> IO (Double, Double)
spinButtonGetRange sb =
  alloca $ \minPtr -> alloca $ \maxPtr -> do
  {#call unsafe spin_button_get_range#} (toSpinButton sb) minPtr maxPtr
  min <- peek minPtr
  max <- peek maxPtr
  return (realToFrac min, realToFrac max)

-- | Retrieve the current value as a floating point
-- value.
--
spinButtonGetValue :: SpinButtonClass sb => sb -> IO Double
spinButtonGetValue sb = liftM realToFrac $
  {#call unsafe spin_button_get_value#} (toSpinButton sb)

-- | Retrieve the current value as integral
-- value.
--
spinButtonGetValueAsInt :: SpinButtonClass sb => sb -> IO Int
spinButtonGetValueAsInt sb = liftM fromIntegral $
  {#call unsafe spin_button_get_value_as_int#} (toSpinButton sb)

-- | Set the value of the SpinButton.
--
spinButtonSetValue :: SpinButtonClass sb => sb -> Double -> IO ()
spinButtonSetValue sb value = {#call spin_button_set_value#}
  (toSpinButton sb) (realToFrac value)

-- | Whether the an out-of-range value set by 'spinButtonSetValue' is clamped to
-- the limits or simply ignored.
--
spinButtonSetUpdatePolicy :: SpinButtonClass sb => sb ->
                             SpinButtonUpdatePolicy -> IO ()
spinButtonSetUpdatePolicy sb up = {#call spin_button_set_update_policy#}
  (toSpinButton sb) ((fromIntegral.fromEnum) up)

-- | Gets the update behavior of a spin button. See 'spinButtonSetUpdatePolicy'.
--
spinButtonGetUpdatePolicy :: SpinButtonClass sb => sb
                          -> IO SpinButtonUpdatePolicy
spinButtonGetUpdatePolicy sb = liftM (toEnum.fromIntegral) $
  {#call unsafe spin_button_get_update_policy#} (toSpinButton sb)

-- | Sets the flag that determines if non-numeric text can be typed into the
-- spin button.
--
spinButtonSetNumeric :: SpinButtonClass sb => sb -> Bool -> IO ()
spinButtonSetNumeric sb numeric = {#call spin_button_set_numeric#}
  (toSpinButton sb) (fromBool numeric)

-- | Returns whether non-numeric text can be typed into the spin button.
--
spinButtonGetNumeric :: SpinButtonClass sb => sb -> IO Bool
spinButtonGetNumeric sb =
  liftM toBool $ {#call unsafe spin_button_get_numeric#} (toSpinButton sb)

-- | Increment or decrement the current value of the SpinButton.
--
spinButtonSpin :: SpinButtonClass sb => sb -> SpinType -> Double -> IO ()
spinButtonSpin sb st offset = {#call spin_button_spin#} (toSpinButton sb)
  ((fromIntegral.fromEnum) st) (realToFrac offset)

-- | Sets the flag that determines if a spin button value wraps around to the
-- opposite limit when the upper or lower limit of the range is exceeded.
--
spinButtonSetWrap :: SpinButtonClass sb => sb -> Bool -> IO ()
spinButtonSetWrap sb wrap = {#call spin_button_set_wrap#} (toSpinButton sb)
  (fromBool wrap)

-- | Returns whether the spin button's value wraps around to the opposite limit
-- when the upper or lower limit of the range is exceeded.
--
spinButtonGetWrap :: SpinButtonClass sb => sb -> IO Bool
spinButtonGetWrap sb =
  liftM toBool $ {#call spin_button_get_wrap#} (toSpinButton sb)

-- | Sets the policy as to whether values are corrected to the nearest step
-- increment when a spin button is activated after providing an invalid value.
--
spinButtonSetSnapToTicks :: SpinButtonClass sb => sb -> Bool -> IO ()
spinButtonSetSnapToTicks sb snapToTicks = 
  {#call spin_button_set_snap_to_ticks#} (toSpinButton sb) 
  (fromBool snapToTicks)

-- | Returns whether the values are corrected to the nearest step.
--
spinButtonGetSnapToTicks :: SpinButtonClass sb => sb -> IO Bool
spinButtonGetSnapToTicks sb = liftM toBool $
  {#call unsafe spin_button_get_snap_to_ticks#} (toSpinButton sb)

-- | Force an update of the SpinButton.
--
spinButtonUpdate :: SpinButtonClass sb => sb -> IO ()
spinButtonUpdate sb = {#call spin_button_update#} (toSpinButton sb)

--------------------
-- Properties

-- | The adjustment that holds the value of the spinbutton.
--
spinButtonAdjustment :: Attr SpinButton Adjustment
spinButtonAdjustment = Attr 
  spinButtonGetAdjustment
  spinButtonSetAdjustment

-- | The number of decimal places to display.
--
-- Allowed values: \<= 20
--
-- Default value: 0
--
spinButtonDigits :: Attr SpinButton Int
spinButtonDigits = Attr 
  spinButtonGetDigits
  spinButtonSetDigits

-- | Whether erroneous values are automatically changed to a spin button's
-- nearest step increment.
--
-- Default value: @False@
--
spinButtonSnapToTicks :: Attr SpinButton Bool
spinButtonSnapToTicks = Attr 
  spinButtonGetSnapToTicks
  spinButtonSetSnapToTicks

-- | Whether non-numeric characters should be ignored.
--
-- Default value: @False@
--
spinButtonNumeric :: Attr SpinButton Bool
spinButtonNumeric = Attr 
  spinButtonGetNumeric
  spinButtonSetNumeric

-- | Whether a spin button should wrap upon reaching its limits.
--
-- Default value: @False@
--
spinButtonWrap :: Attr SpinButton Bool
spinButtonWrap = Attr 
  spinButtonGetWrap
  spinButtonSetWrap

-- | Whether the spin button should update always, or only when the value is
-- legal.
--
-- Default value: 'UpdateAlways'
--
spinButtonUpdatePolicy :: Attr SpinButton SpinButtonUpdatePolicy
spinButtonUpdatePolicy = Attr 
  spinButtonGetUpdatePolicy
  spinButtonSetUpdatePolicy

-- | Reads the current value, or sets a new value.
--
-- Default value: 0
--
spinButtonValue :: Attr SpinButton Double
spinButtonValue = Attr 
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
    Nothing -> return (toInteger inputError)
afterInput sb user = connect_PTR__INT "input" True sb $ \dPtr -> do
  mVal <- user
  case mVal of
    (Just val) -> do
      poke dPtr ((realToFrac val)::{#type gdouble#})
      return 0
    Nothing -> return (toInteger inputError)

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

