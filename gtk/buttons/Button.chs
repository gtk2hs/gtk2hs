-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Button
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
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
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module Button(
  Button,
  ButtonClass,
  castToButton,
  buttonNew,
  buttonNewWithLabel,
  buttonNewWithMnemonic,
  buttonNewFromStock,
  buttonPressed,
  buttonReleased,
  buttonClicked,
  buttonEnter,
  buttonLeave,
  ReliefStyle(..),
  buttonSetRelief,
  buttonGetRelief,
  connectToButtonActivate,
  connectToClicked,
  connectToEnter,
  connectToLeave,
  connectToPressed,
  connectToReleased
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(ReliefStyle(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a new Button widget. (EXPORTED)
--
buttonNew :: IO Button
buttonNew = makeNewObject mkButton $ liftM castPtr {#call unsafe button_new#}


-- Create a button with a label in it. (EXPORTED)
--
buttonNewWithLabel :: String -> IO Button
buttonNewWithLabel lbl = withCString lbl (\strPtr ->
  makeNewObject mkButton $ liftM castPtr $
  {#call unsafe button_new_with_label#} strPtr)

-- Create a button with an accelerator key. (EXPORTED)
--
-- * Like @buttonNewWithLabel but turns every underscore in the label
--   to a underlined character.
buttonNewWithMnemonic :: String -> IO Button
buttonNewWithMnemonic lbl = withCString lbl (\strPtr ->
  makeNewObject mkButton $ liftM castPtr $ 
  {#call unsafe button_new_with_mnemonic#} strPtr)

-- Create a stock (predefined appearance) button. (EXPORTED)
--
buttonNewFromStock :: String -> IO Button
buttonNewFromStock stockId = withCString stockId (\strPtr -> 
  makeNewObject mkButton $ liftM castPtr $
  throwIfNull "buttonNewFromStock: Invalid stock identifier." $ 
  {#call unsafe button_new_from_stock#} strPtr)

-- Depress the button, i.e. emit the pressed signal. (EXPORTED)
--
buttonPressed :: ButtonClass b => b -> IO ()
buttonPressed b = {#call button_pressed#} (toButton b)

-- Release the button, i.e. emit the released signal. (EXPORTED)
--
buttonReleased :: ButtonClass b => b -> IO ()
buttonReleased b = {#call button_released#} (toButton b)

-- Emit the clicked signal on the button. (EXPORTED)
--
-- * This is similar to calling @buttonPressed and @buttonReleased in sequence.
buttonClicked :: ButtonClass b => b -> IO ()
buttonClicked b = {#call button_clicked#} (toButton b)

-- Emit the cursor enters signal to the button. (EXPORTED) 
--
buttonEnter :: ButtonClass b => b -> IO ()
buttonEnter b = {#call button_enter#} (toButton b)

-- Emit the cursor leaves signal to the button. (EXPORTED) 
--
buttonLeave :: ButtonClass b => b -> IO ()
buttonLeave b = {#call button_leave#} (toButton b)

-- Set the style of the button edges. (EXPORTED)
--
buttonSetRelief :: ButtonClass b => ReliefStyle -> b -> IO ()
buttonSetRelief rs b = 
  {#call button_set_relief#} (toButton b) ((fromIntegral.fromEnum) rs)

-- Get the current relief style. (EXPORTED)
--
buttonGetRelief :: ButtonClass b => b -> IO ReliefStyle
buttonGetRelief b = liftM (toEnum.fromIntegral) $
  {#call unsafe button_get_relief#} (toButton b)

-- signals

-- The button has been depressed (but not necessarily released yet). 
-- See @clicked signal. (EXPORTED)
--
connectToButtonActivate :: ButtonClass b => 
  IO () -> ConnectAfter -> b -> IO (ConnectId b)
connectToButtonActivate = connect_NONE__NONE "activate" 

-- The button was clicked. This is only emitted if the mouse cursor was
-- over the button when it was released. (EXPORTED)
--
connectToClicked :: ButtonClass b => IO () -> ConnectAfter -> b -> IO (ConnectId b)
connectToClicked = connect_NONE__NONE "clicked"

-- The cursor enters the button box. (EXPORTED)
--
connectToEnter :: ButtonClass b => IO () -> ConnectAfter -> b -> IO (ConnectId b)
connectToEnter = connect_NONE__NONE "enter"

-- The cursor leaves the button box. (EXPORTED)
--
connectToLeave :: ButtonClass b => IO () -> ConnectAfter -> b -> IO (ConnectId b)
connectToLeave = connect_NONE__NONE "leave"

-- The button is pressed. (EXPORTED)
--
connectToPressed :: ButtonClass b => IO () -> ConnectAfter -> b -> IO (ConnectId b)
connectToPressed = connect_NONE__NONE "pressed"


-- The button is released. (EXPORTED)
--
connectToReleased :: ButtonClass b => IO () -> ConnectAfter -> b -> IO (ConnectId b)
connectToReleased = connect_NONE__NONE "released"









