-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget SpinButton
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
-- * A spin button provides the possiblity to enter a numeric value without
--   using the keyboard.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

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
  connectToInput,
  connectToOutput,
  connectToValueSpinned
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Structs	(inputError)
import Enums	(SpinButtonUpdatePolicy(..), SpinType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a new SpinButton. (EXPORTED)
--
-- * @climbRate is the amount by which the value is changed each time the
--   up/down buttons are pressed.
--
-- * @digits is the number of shown digits. Set to 0 to work with integer
--   values.
--
spinButtonNew :: Adjustment -> Double -> Int -> IO SpinButton
spinButtonNew adj climbRate digits = makeNewObject mkSpinButton $ 
  liftM castPtr $ {#call unsafe spin_button_new#} adj (realToFrac climbRate)
  (fromIntegral digits)

-- Create a new SpinButton with a restricted range. (EXPORTED)
--
-- * This is a convenience function because the user does not have to
--   create an Adjustment first. Page increments are set to 10*@step.
--
spinButtonNewWithRange :: Double -> Double -> Double -> IO SpinButton
spinButtonNewWithRange min max step = makeNewObject mkSpinButton $
  liftM castPtr $ {#call unsafe spin_button_new_with_range#}
  (realToFrac min) (realToFrac max) (realToFrac step)

-- Change the settings of a SpinButton. (EXPORTED)
--
spinButtonConfigure :: SpinButtonClass sb => 
  Adjustment -> Double -> Int -> sb -> IO ()
spinButtonConfigure adj climbRate digits sb = {#call spin_button_configure#}
  (toSpinButton sb) adj (realToFrac climbRate) (fromIntegral digits)

-- Attach a new Adjustment object to the SpinButton. (EXPORTED)
--
spinButtonSetAdjustment :: SpinButtonClass sb => Adjustment -> sb -> IO ()
spinButtonSetAdjustment adj sb = {#call spin_button_set_adjustment#}
  (toSpinButton sb) adj

-- Retrieve the Adjustment object that is currently controlling the SpinButton.
-- (EXPORTED)
--
spinButtonGetAdjustment :: SpinButtonClass sb => sb -> IO Adjustment
spinButtonGetAdjustment sb = makeNewObject mkAdjustment $
  {#call unsafe spin_button_get_adjustment#} (toSpinButton sb)

-- Sets the number of shown digits. (EXPORTED)
--
spinButtonSetDigits :: SpinButtonClass sb => Int -> sb -> IO ()
spinButtonSetDigits digits sb = {#call spin_button_set_digits#} 
  (toSpinButton sb) (fromIntegral digits)

-- Sets the increment for up/down buttons. (EXPORTED)
--
spinButtonSetIncrements :: SpinButtonClass sb => 
  Double -> Double -> sb -> IO ()
spinButtonSetIncrements step page sb = {#call spin_button_set_increments#}
  (toSpinButton sb) (realToFrac step) (realToFrac page)

-- Set the maximal allowable range for the spinbutton. (EXPORTED)
--
spinButtonSetRange :: SpinButtonClass sb => Double -> Double -> sb -> IO ()
spinButtonSetRange min max sb = {#call spin_button_set_range#}
  (toSpinButton sb) (realToFrac min) (realToFrac max)

-- Retrieve the current value as a floating point value. (EXPORTED)
--
spinButtonGetValue :: SpinButtonClass sb => sb -> IO Double
spinButtonGetValue sb = liftM realToFrac $
  {#call unsafe spin_button_get_value#} (toSpinButton sb)

-- Retrieve the current value as integral value. (EXPORTED)
--
spinButtonGetValueAsInt :: SpinButtonClass sb => sb -> IO Int
spinButtonGetValueAsInt sb = liftM fromIntegral $
  {#call unsafe spin_button_get_value_as_int#} (toSpinButton sb)

-- Set the value of the SpinButton. (EXPORTED)
--
spinButtonSetValue :: SpinButtonClass sb => Double -> sb -> IO ()
spinButtonSetValue value sb = {#call spin_button_set_value#}
  (toSpinButton sb) (realToFrac value)

-- Whether the an out-of-range value set by @spinButtonSetValue is
-- clamped to the limits or simply ignored.
-- (EXPORTED)
--
spinButtonSetUpdatePolicy :: SpinButtonClass sb => 
  SpinButtonUpdatePolicy -> sb -> IO ()
spinButtonSetUpdatePolicy up sb = {#call spin_button_set_update_policy#}
  (toSpinButton sb) ((fromIntegral.fromEnum) up)

-- Sets the flag that determines if non-numeric text can be typed into the
-- spin button. (EXPORTED)
--
spinButtonSetNumeric :: SpinButtonClass sb => Bool -> sb -> IO ()
spinButtonSetNumeric numeric sb = {#call spin_button_set_numeric#}
  (toSpinButton sb) (fromBool numeric)

-- Increment or decrement the current value of the SpinButton. (EXPORTED)
--
spinButtonSpin :: SpinButtonClass sb => SpinType -> Double -> sb -> IO ()
spinButtonSpin st offset sb = {#call spin_button_spin#} (toSpinButton sb)
  ((fromIntegral.fromEnum) st) (realToFrac offset)

-- Sets the flag that determines if a spin button value wraps around to the
-- opposite limit when the upper or lower limit of the range is exceeded.
-- (EXPORTED)
--
spinButtonSetWrap :: SpinButtonClass sb => Bool -> sb -> IO ()
spinButtonSetWrap wrap sb = {#call spin_button_set_wrap#} (toSpinButton sb)
  (fromBool wrap)

-- Sets the policy as to whether values are corrected to the nearest step 
-- increment when a spin button is activated after providing an invalid value.
-- (EXPORTED)
--
spinButtonSetSnapToTicks :: SpinButtonClass sb => Bool -> sb -> IO ()
spinButtonSetSnapToTicks snapToTicks sb = 
  {#call spin_button_set_snap_to_ticks#} (toSpinButton sb) 
  (fromBool snapToTicks)

-- Force an update of the SpinButton. (EXPORTED)
--
spinButtonUpdate :: SpinButtonClass sb => sb -> IO ()
spinButtonUpdate sb = {#call spin_button_update#} (toSpinButton sb)

-- signals

-- Install a custom input handler. (EXPORTED)
--
-- * This signal is called upon each time the value of the SpinButton is set
--   by spinButtonSetValue. The function can return Nothing if the value is
--   no good.
--
connectToInput :: SpinButtonClass sb => 
  (IO (Maybe Double)) -> ConnectAfter -> sb -> IO (ConnectId sb)
connectToInput fun = connect_PTR__INT "input" (\dPtr -> do
  mVal <- fun
  case mVal of
    (Just val) -> do
      poke dPtr ((realToFrac val)::{#type gdouble#})
      return 0
    Nothing -> return (toInteger inputError))

-- Install a custom output handler. (EXPORTED)
--
-- * This handler makes it possible to query the current value and to
--   render something completely different to the screen using entrySetText.
--   The return value must be False in order to let the default output
--   routine run after this signal returns.
--
connectToOutput :: SpinButtonClass sb => 
  IO Bool -> ConnectAfter -> sb -> IO (ConnectId sb)
connectToOutput = connect_NONE__BOOL "output"

-- The value of the spin button has changed. (EXPORTED)
--
connectToValueSpinned :: SpinButtonClass sb =>
  IO () -> ConnectAfter -> sb -> IO (ConnectId sb)
connectToValueSpinned = connect_NONE__NONE "value-changed"

