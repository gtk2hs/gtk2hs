-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SpinButton
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.6 $ from $Date: 2004/07/30 16:32:01 $
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
-- * A spin button provides the possiblity to enter a numeric value without
--   using the keyboard.
--
--
--
-- * TODO

module SpinButton(
  SpinButton,
  SpinButtonClass,
  castToSpinButton,
  spinButtonNew,
  spinButtonNewWithRange,
  spinButtonConfigure,
  spinButtonSetAdjustment,
  spinButtonGetAdjustment,
  spinButtonSetDigits,
  spinButtonSetIncrements,
  spinButtonSetRange,
  spinButtonGetValue,
  spinButtonGetValueAsInt,
  spinButtonSetValue,
  SpinButtonUpdatePolicy(..),
  spinButtonSetUpdatePolicy,
  spinButtonSetNumeric,
  SpinType(..),
  spinButtonSpin,
  spinButtonSetWrap,
  spinButtonSetSnapToTicks,
  spinButtonUpdate,
  onInput,
  afterInput,
  onOutput,
  afterOutput,
  onValueSpinned,
  afterValueSpinned
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Structs	(inputError)
import Enums	(SpinButtonUpdatePolicy(..), SpinType(..))

{# context lib="gtk" prefix="gtk" #}

-- GtkSpinbutton implements the GtkEditable interface
instance EditableClass SpinButton

-- methods

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

-- | Sets the increment for up\/down buttons.
--
spinButtonSetIncrements :: SpinButtonClass sb => sb -> Double -> Double ->
                           IO ()
spinButtonSetIncrements sb step page = {#call spin_button_set_increments#}
  (toSpinButton sb) (realToFrac step) (realToFrac page)

-- | Set the maximal allowable range for the
-- spinbutton.
--
spinButtonSetRange :: SpinButtonClass sb => sb -> Double -> Double -> IO ()
spinButtonSetRange sb min max = {#call spin_button_set_range#}
  (toSpinButton sb) (realToFrac min) (realToFrac max)

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

-- | Whether the an out-of-range value set by
-- 'spinButtonSetValue' is clamped to the limits or simply ignored.
--
spinButtonSetUpdatePolicy :: SpinButtonClass sb => sb ->
                             SpinButtonUpdatePolicy -> IO ()
spinButtonSetUpdatePolicy sb up = {#call spin_button_set_update_policy#}
  (toSpinButton sb) ((fromIntegral.fromEnum) up)

-- | Sets the flag that determines if non-numeric
-- text can be typed into the spin button.
--
spinButtonSetNumeric :: SpinButtonClass sb => sb -> Bool -> IO ()
spinButtonSetNumeric sb numeric = {#call spin_button_set_numeric#}
  (toSpinButton sb) (fromBool numeric)

-- | Increment or decrement the current value of the
-- SpinButton.
--
spinButtonSpin :: SpinButtonClass sb => sb -> SpinType -> Double -> IO ()
spinButtonSpin sb st offset = {#call spin_button_spin#} (toSpinButton sb)
  ((fromIntegral.fromEnum) st) (realToFrac offset)

-- | Sets the flag that determines if a spin button
-- value wraps around to the opposite limit when the upper or lower limit of
-- the range is exceeded.
--
spinButtonSetWrap :: SpinButtonClass sb => sb -> Bool -> IO ()
spinButtonSetWrap sb wrap = {#call spin_button_set_wrap#} (toSpinButton sb)
  (fromBool wrap)

-- | Sets the policy as to whether values are
-- corrected to the nearest step increment when a spin button is activated
-- after providing an invalid value.
--
spinButtonSetSnapToTicks :: SpinButtonClass sb => sb -> Bool -> IO ()
spinButtonSetSnapToTicks sb snapToTicks = 
  {#call spin_button_set_snap_to_ticks#} (toSpinButton sb) 
  (fromBool snapToTicks)

-- | Force an update of the SpinButton.
--
spinButtonUpdate :: SpinButtonClass sb => sb -> IO ()
spinButtonUpdate sb = {#call spin_button_update#} (toSpinButton sb)

-- signals

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

