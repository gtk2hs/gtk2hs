-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Button@
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2002/11/08 10:39:21 $
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
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

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
  buttonSetLabel,
  buttonGetLabel,
  buttonSetUseStock,
  buttonGetUseStock,
  buttonSetUseUnderline,
  buttonGetUseUnderline,
  onButtonActivate,
  afterButtonActivate,
  onClicked,
  afterClicked,
  onEnter,
  afterEnter,
  onLeave,
  afterLeave,
  onPressed,
  afterPressed,
  onReleased,
  afterReleased
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

-- @constructor buttonNew@ Create a new Button widget.
--
buttonNew :: IO Button
buttonNew  = makeNewObject mkButton $ liftM castPtr {#call unsafe button_new#}


-- @constructor buttonNewWithLabel@ Create a button with a label in it.
--
buttonNewWithLabel :: String -> IO Button
buttonNewWithLabel lbl = withCString lbl (\strPtr ->
  makeNewObject mkButton $ liftM castPtr $
  {#call unsafe button_new_with_label#} strPtr)

-- @constructor buttonNewWithMnemonic@ Create a button with an accelerator key.
--
-- * Like @ref constructor buttonNewWithLabel@ but turns every underscore in the
--   label to a underlined character which then acts as a mnemonic (keyboard
--   shortcut).
--
buttonNewWithMnemonic :: String -> IO Button
buttonNewWithMnemonic lbl = withCString lbl (\strPtr ->
  makeNewObject mkButton $ liftM castPtr $ 
  {#call unsafe button_new_with_mnemonic#} strPtr)

-- @constructor buttonNewFromStock@ Create a stock (predefined appearance) button.
--
buttonNewFromStock :: String -> IO Button
buttonNewFromStock stockId = withCString stockId (\strPtr -> 
  makeNewObject mkButton $ liftM castPtr $
  throwIfNull "buttonNewFromStock: Invalid stock identifier." $ 
  {#call unsafe button_new_from_stock#} strPtr)

-- @method buttonPressed@ Depress the button, i.e. emit the pressed signal.
--
buttonPressed :: ButtonClass b => b -> IO ()
buttonPressed b = {#call button_pressed#} (toButton b)

-- @method buttonReleased@ Release the button, i.e. emit the released signal.
--
buttonReleased :: ButtonClass b => b -> IO ()
buttonReleased b = {#call button_released#} (toButton b)

-- @method buttonClicked@ Emit the clicked signal on the button.
--
-- * This is similar to calling @ref method buttonPressed@ and
--   @ref method buttonReleased@ in sequence.
--
buttonClicked :: ButtonClass b => b -> IO ()
buttonClicked b = {#call button_clicked#} (toButton b)

-- @method buttonEnter@ Emit the cursor enters signal to the button.
--
buttonEnter :: ButtonClass b => b -> IO ()
buttonEnter b = {#call button_enter#} (toButton b)

-- @method buttonLeave@ Emit the cursor leaves signal to the button.
--
buttonLeave :: ButtonClass b => b -> IO ()
buttonLeave b = {#call button_leave#} (toButton b)

-- @method buttonSetRelief@ Set the style of the button edges.
--
buttonSetRelief :: ButtonClass b => b -> ReliefStyle -> IO ()
buttonSetRelief b rs = 
  {#call button_set_relief#} (toButton b) ((fromIntegral.fromEnum) rs)

-- @method buttonGetRelief@ Get the current relief style.
--
buttonGetRelief :: ButtonClass b => b -> IO ReliefStyle
buttonGetRelief b = liftM (toEnum.fromIntegral) $
  {#call unsafe button_get_relief#} (toButton b)

-- @method buttonSetLabel@ Set the text of the button.
--
buttonSetLabel :: ButtonClass b => b -> String -> IO ()
buttonSetLabel b lbl = withCString lbl $ \strPtr ->
  {#call button_set_label#} (toButton b) strPtr

-- @method buttonGetLabel@ Get the current text on the button.
--
-- * The method returns the empty string in case the button does not have
--   a label (e.g. it was created with @ref method buttonNew@.
--
buttonGetLabel :: ButtonClass b => b -> IO String
buttonGetLabel b = do
  strPtr <- {#call unsafe button_get_label#} (toButton b)
  if strPtr==nullPtr then return "" else peekCString strPtr

-- @method buttonSetUseStock@ Set if the label is a stock identifier.
--
-- * Setting this property to @literal True@ will make the button lookup
--   its label in the table of stock items. If there is a match, the button
--   will use the stock item instead of the label.  You need to set this
--   flag before you change the label.
--
buttonSetUseStock :: ButtonClass b => b -> Bool -> IO ()
buttonSetUseStock b flag = 
  {#call button_set_use_stock#} (toButton b) (fromBool flag)

-- @method buttonGetUseStock@ Get the current flag for stock lookups.
--
buttonGetUseStock :: ButtonClass b => b -> IO Bool
buttonGetUseStock b = liftM toBool $
  {#call unsafe button_get_use_stock#} (toButton b)

-- @method buttonSetUseUnderline@ Set if the label has accelerators.
--
-- * Setting this property will make the button join any underline character
--   into the following letter and inserting this letter as a keyboard
--   shortcut. You need to set this flag before you change the label.
--
buttonSetUseUnderline :: ButtonClass b => b -> Bool -> IO ()
buttonSetUseUnderline b flag = 
  {#call button_set_use_underline#} (toButton b) (fromBool flag)

-- @method buttonGetUseUnderline@ Query if the underlines are mnemonics.
--
buttonGetUseUnderline :: ButtonClass b => b -> IO Bool
buttonGetUseUnderline b = liftM toBool $
  {#call unsafe button_get_use_underline#} (toButton b)


-- signals

-- @signal connectToButtonActivate@ The button has been depressed (but not
-- necessarily released yet). See @ref arg clicked@ signal.
--
onButtonActivate, afterButtonActivate :: ButtonClass b => b -> IO () ->
                                         IO (ConnectId b)
onButtonActivate = connect_NONE__NONE "activate"  False
afterButtonActivate = connect_NONE__NONE "activate"  True

-- @signal connectToClicked@ The button was clicked. This is only emitted if
-- the mouse cursor was over the button when it was released.
--
onClicked, afterClicked :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onClicked = connect_NONE__NONE "clicked" False
afterClicked = connect_NONE__NONE "clicked" True

-- @signal connectToEnter@ The cursor enters the button box.
--
onEnter, afterEnter :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onEnter = connect_NONE__NONE "enter" False
afterEnter = connect_NONE__NONE "enter" True

-- @signal connectToLeave@ The cursor leaves the button box.
--
onLeave, afterLeave :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onLeave = connect_NONE__NONE "leave" False
afterLeave = connect_NONE__NONE "leave" True

-- @signal connectToPressed@ The button is pressed.
--
onPressed, afterPressed :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onPressed = connect_NONE__NONE "pressed" False
afterPressed = connect_NONE__NONE "pressed" True


-- @signal connectToReleased@ The button is released.
--
onReleased, afterReleased :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onReleased = connect_NONE__NONE "released" False
afterReleased = connect_NONE__NONE "released" True









