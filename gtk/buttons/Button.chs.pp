-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Button
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2004/11/21 15:06:13 $
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
#if GTK_CHECK_VERSION(2,4,0)
  buttonSetFocusOnClick,
  buttonGetFocusOnClick,
  buttonSetAlignment,
  buttonGetAlignment,
#endif
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
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(ReliefStyle(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new Button widget.
--
buttonNew :: IO Button
buttonNew  = makeNewObject mkButton $ liftM castPtr {#call unsafe button_new#}


-- | Create a button with a label in it.
--
buttonNewWithLabel :: String -> IO Button
buttonNewWithLabel lbl = withUTFString lbl (\strPtr ->
  makeNewObject mkButton $ liftM castPtr $
  {#call unsafe button_new_with_label#} strPtr)

-- | Create a button with an accelerator key.
--
-- * Like 'buttonNewWithLabel' but turns every underscore in the
--   label to a underlined character which then acts as a mnemonic (keyboard
--   shortcut).
--
buttonNewWithMnemonic :: String -> IO Button
buttonNewWithMnemonic lbl = withUTFString lbl (\strPtr ->
  makeNewObject mkButton $ liftM castPtr $ 
  {#call unsafe button_new_with_mnemonic#} strPtr)

-- | Create a stock (predefined appearance) button.
--
buttonNewFromStock :: String -> IO Button
buttonNewFromStock stockId = withUTFString stockId (\strPtr -> 
  makeNewObject mkButton $ liftM castPtr $
  throwIfNull "buttonNewFromStock: Invalid stock identifier." $ 
  {#call unsafe button_new_from_stock#} strPtr)

-- | Depress the button, i.e. emit the pressed signal.
--
buttonPressed :: ButtonClass b => b -> IO ()
buttonPressed b = {#call button_pressed#} (toButton b)

-- | Release the button, i.e. emit the released signal.
--
buttonReleased :: ButtonClass b => b -> IO ()
buttonReleased b = {#call button_released#} (toButton b)

-- | Emit the clicked signal on the button.
--
-- * This is similar to calling 'buttonPressed' and
--   'buttonReleased' in sequence.
--
buttonClicked :: ButtonClass b => b -> IO ()
buttonClicked b = {#call button_clicked#} (toButton b)

-- | Emit the cursor enters signal to the button.
--
buttonEnter :: ButtonClass b => b -> IO ()
buttonEnter b = {#call button_enter#} (toButton b)

-- | Emit the cursor leaves signal to the button.
--
buttonLeave :: ButtonClass b => b -> IO ()
buttonLeave b = {#call button_leave#} (toButton b)

-- | Set the style of the button edges.
--
buttonSetRelief :: ButtonClass b => b -> ReliefStyle -> IO ()
buttonSetRelief b rs = 
  {#call button_set_relief#} (toButton b) ((fromIntegral.fromEnum) rs)

-- | Get the current relief style.
--
buttonGetRelief :: ButtonClass b => b -> IO ReliefStyle
buttonGetRelief b = liftM (toEnum.fromIntegral) $
  {#call unsafe button_get_relief#} (toButton b)

-- | Set the text of the button.
--
buttonSetLabel :: ButtonClass b => b -> String -> IO ()
buttonSetLabel b lbl = withUTFString lbl $ \strPtr ->
  {#call button_set_label#} (toButton b) strPtr

-- | Get the current text on the button.
--
-- * The method returns the empty string in case the button does not have
--   a label (e.g. it was created with 'buttonNew'.
--
buttonGetLabel :: ButtonClass b => b -> IO String
buttonGetLabel b = do
  strPtr <- {#call unsafe button_get_label#} (toButton b)
  if strPtr==nullPtr then return "" else peekUTFString strPtr

-- | Set if the label is a stock identifier.
--
-- * Setting this property to @True@ will make the button lookup
--   its label in the table of stock items. If there is a match, the button
--   will use the stock item instead of the label.  You need to set this
--   flag before you change the label.
--
buttonSetUseStock :: ButtonClass b => b -> Bool -> IO ()
buttonSetUseStock b flag = 
  {#call button_set_use_stock#} (toButton b) (fromBool flag)

-- | Get the current flag for stock lookups.
--
buttonGetUseStock :: ButtonClass b => b -> IO Bool
buttonGetUseStock b = liftM toBool $
  {#call unsafe button_get_use_stock#} (toButton b)

-- | Set if the label has accelerators.
--
-- * Setting this property will make the button join any underline character
--   into the following letter and inserting this letter as a keyboard
--   shortcut. You need to set this flag before you change the label.
--
buttonSetUseUnderline :: ButtonClass b => b -> Bool -> IO ()
buttonSetUseUnderline b flag = 
  {#call button_set_use_underline#} (toButton b) (fromBool flag)

-- | Query if the underlines are mnemonics.
--
buttonGetUseUnderline :: ButtonClass b => b -> IO Bool
buttonGetUseUnderline b = liftM toBool $
  {#call unsafe button_get_use_underline#} (toButton b)

#if GTK_CHECK_VERSION(2,4,0)
-- | Sets whether the button will grab focus when it is clicked with the mouse.
--
buttonSetFocusOnClick :: ButtonClass b => b -> Bool -> IO ()
buttonSetFocusOnClick b focus =
  {#call unsafe button_set_focus_on_click#} (toButton b) (fromBool focus)

-- | Gets whether the button grabs focus when it is clicked with the mouse.
--
buttonGetFocusOnClick :: ButtonClass b => b -> IO Bool
buttonGetFocusOnClick b = liftM toBool $
  {#call unsafe button_get_focus_on_click#} (toButton b)

-- | Sets the alignment of the child. This has no effect unless the child
--   derives from "Misc" "Aligment".
--
buttonSetAlignment :: ButtonClass b => b -> (Float, Float) -> IO ()
buttonSetAlignment b (xalign, yalign) =
  {#call unsafe button_set_alignment#} (toButton b)
    (realToFrac xalign) (realToFrac yalign)

-- | Gets the alignment of the child in the button.
--
buttonGetAlignment :: ButtonClass b => b -> IO (Float, Float)
buttonGetAlignment b =
  alloca $ \xalignPtr -> alloca $ \yalignPtr -> do
  {#call unsafe button_get_alignment#} (toButton b) xalignPtr yalignPtr
  xalign <- peek xalignPtr
  yalign <- peek yalignPtr
  return (realToFrac xalign, realToFrac yalign)
#endif


-- signals

-- | The button has been depressed (but not
-- necessarily released yet). See @clicked@ signal.
--
onButtonActivate, afterButtonActivate :: ButtonClass b => b -> IO () ->
                                         IO (ConnectId b)
onButtonActivate = connect_NONE__NONE "activate"  False
afterButtonActivate = connect_NONE__NONE "activate"  True

-- | The button was clicked. This is only emitted if
-- the mouse cursor was over the button when it was released.
--
onClicked, afterClicked :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onClicked = connect_NONE__NONE "clicked" False
afterClicked = connect_NONE__NONE "clicked" True

-- | The cursor enters the button box.
--
onEnter, afterEnter :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onEnter = connect_NONE__NONE "enter" False
afterEnter = connect_NONE__NONE "enter" True

-- | The cursor leaves the button box.
--
onLeave, afterLeave :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onLeave = connect_NONE__NONE "leave" False
afterLeave = connect_NONE__NONE "leave" True

-- | The button is pressed.
--
onPressed, afterPressed :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onPressed = connect_NONE__NONE "pressed" False
afterPressed = connect_NONE__NONE "pressed" True


-- | The button is released.
--
onReleased, afterReleased :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onReleased = connect_NONE__NONE "released" False
afterReleased = connect_NONE__NONE "released" True









