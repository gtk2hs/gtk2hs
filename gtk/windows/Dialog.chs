-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Dialog
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
-- * A dialog is a smaller window that is used to ask the use for input.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module Dialog(
  Dialog,
  DialogClass,
  castToDialog,
  dialogNew,
  dialogGetUpper,
  dialogGetActionArea,
  dialogRun,
  dialogResponse,
  ResponseId,
  responseNone,
  responseReject,
  responseAccept,
  responseDeleteEvent,
  responseOk,
  responseCancel,
  responseClose,
  responseYes,
  responseNo,
  responseApply,
  responseHelp,
  dialogAddButton,
  dialogAddActionWidget,
  dialogGetHasSeparator,
  dialogSetDefaultResponse,
  dialogSetHasSeparator,
  dialogSetResponseSensitive,
  connectToResponse
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Structs	(dialogGetUpper, dialogGetActionArea, ResponseId, responseNone,
		 responseReject, responseAccept, responseDeleteEvent,
		 responseOk, responseCancel, responseClose, responseYes,
		 responseNo, responseApply, responseHelp)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a new Dialog. (EXPORTED)
--
dialogNew :: IO Dialog
dialogNew = makeNewObject mkDialog $ liftM castPtr {#call unsafe dialog_new#}

-- Run the dialog by entering a new main loop. (EXPORTED)
--
-- * The dialog is run until it is either forced to quit (-1 will be returned)
--   or until the user clicks a button (or other widget) in the action
--   area that makes the dialog emit the @response signal (the response
--   id of the pressed button will be returned).
-- * To force a dialog to quit, call @dialogResponse on it.
-- * If this function returns the dialog still needs to be destroyed.
--
dialogRun :: DialogClass dc => dc -> IO ResponseId
dialogRun dc = liftM fromIntegral $ {#call dialog_run#} (toDialog dc)

-- Emit the @response signal on the dialog. (EXPORTED)
--
-- * This function can be used to add a custom widget to the action area
--   that should close the dialog when activated or to close the dialog
--   otherwise.
--
dialogResponse :: DialogClass dc => ResponseId -> dc -> IO ()
dialogResponse resId dc = 
  {#call dialog_response#} (toDialog dc) (fromIntegral resId)

-- Add a button with a label to the action area. (EXPORTED)
--
-- * The text may as well refer to a stock object. If such an object exists
--   it is taken as widget.
-- * The function returns the Button that resulted from the call.
--
dialogAddButton :: DialogClass dc => String -> ResponseId -> dc -> IO Button
dialogAddButton button resId dc = withCString button $ \strPtr -> 
  makeNewObject mkButton $ liftM castPtr $ {#call dialog_add_button#} 
  (toDialog dc) strPtr (fromIntegral resId)

-- Add a widget to the action area. If the widget is put into the activated
-- state @resId will be transmitted by the @response signal. (EXPORTED)
--
-- * A widget that cannot be activated and therefore has to emit the response
--   signal manually must be added by packing it into the action area.
--
dialogAddActionWidget :: (DialogClass dc, WidgetClass w) => 
  w -> ResponseId -> dc -> IO ()
dialogAddActionWidget child resId dc = {#call dialog_add_action_widget#}
  (toDialog dc) (toWidget child) (fromIntegral resId)

-- Query if the dialog has a visible horizontal separator. (EXPORTED)
--
dialogGetHasSeparator :: DialogClass dc => dc -> IO Bool
dialogGetHasSeparator dc = liftM toBool $ 
  {#call unsafe dialog_get_has_separator#} (toDialog dc)

-- Set the default widget that is to be activated if the user pressed enter.
-- The object is specified by the ResponseId. (EXPORTED)
--
dialogSetDefaultResponse :: DialogClass dc => ResponseId -> dc -> IO ()
dialogSetDefaultResponse resId dc = {#call dialog_set_default_response#}
  (toDialog dc) (fromIntegral resId)

-- Set the visibility of the horizontal separator. (EXPORTED)
--
dialogSetHasSeparator :: DialogClass dc => Bool -> dc -> IO ()
dialogSetHasSeparator set dc = {#call dialog_set_has_separator#}
  (toDialog dc) (fromBool set)

-- Set widgets in the action are to be sensitive or not. (EXPORTED)
--
dialogSetResponseSensitive :: DialogClass dc => 
  ResponseId -> Bool -> dc -> IO ()
dialogSetResponseSensitive resId sensitive dc = 
  {#call dialog_set_response_sensitive#} (toDialog dc) (fromIntegral resId)
  (fromBool sensitive)

-- signals

-- This signal is sent when a widget in the action area was activated, the
-- dialog is received a destory event or the user calls dialogResponse.
-- It is usually used to terminate the dialog (by dialogRun for example).
--
connectToResponse :: DialogClass dc => 
  (ResponseId -> IO ()) -> ConnectAfter -> dc -> IO (ConnectId dc)
connectToResponse = connect_INT__NONE "response"
