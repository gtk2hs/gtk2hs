-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Dialog
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:37:33 $
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
-- A dialog is a smaller window that is used to ask the user for input.
--

module Graphics.UI.Gtk.Windows.Dialog (
  Dialog,
  DialogClass,
  castToDialog,
  dialogNew,
  dialogGetUpper,
  dialogGetActionArea,
  dialogRun,
  dialogResponse,
  ResponseId(..),
  dialogAddButton,
  dialogAddActionWidget,
  dialogGetHasSeparator,
  dialogSetDefaultResponse,
  dialogSetHasSeparator,
  dialogSetResponseSensitive,
  onResponse,
  afterResponse
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Structs	(dialogGetUpper, dialogGetActionArea, ResponseId(..), fromResponse, toResponse)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new Dialog.
--
dialogNew :: IO Dialog
dialogNew  = makeNewObject mkDialog $ liftM castPtr {#call unsafe dialog_new#}

-- | Run the dialog by entering a new main loop.
--
-- * The dialog is run until it is either forced to quit (-1 will be returned)
--   or until the user clicks a button (or other widget) in the action area
--   that makes the dialog emit the @response@ signal (the response id
--   of the pressed button will be returned).
--
-- * To force a dialog to quit, call 'dialogResponse' on it.
--
-- * If this function returns the dialog still needs to be destroyed.
--
dialogRun :: DialogClass dc => dc -> IO ResponseId
dialogRun dc = liftM toResponse $ {#call dialog_run#} (toDialog dc)

-- | Emit the @response@ signal on the dialog.
--
-- * This function can be used to add a custom widget to the action area that
--   should close the dialog when activated or to close the dialog otherwise.
--
dialogResponse :: DialogClass dc => dc -> ResponseId -> IO ()
dialogResponse dc resId = 
  {#call dialog_response#} (toDialog dc) (fromResponse resId)

-- | Add a button with a label to the action area.
--
-- * The text may as well refer to a stock object. If such an object exists it
--   is taken as widget.
--
-- * The function returns the Button that resulted from the call.
--
dialogAddButton :: DialogClass dc => dc -> String -> ResponseId -> IO Button
dialogAddButton dc button resId = withUTFString button $ \strPtr -> 
  makeNewObject mkButton $ liftM castPtr $ {#call dialog_add_button#} 
  (toDialog dc) strPtr (fromResponse resId)

-- | Add a widget to the action area. If the
-- widget is put into the activated state @resId@ will be transmitted
-- by the @response@ signal.
--
-- * A widget that cannot be activated and therefore has to emit the response
--   signal manually must be added by packing it into the action area.
--
dialogAddActionWidget :: (DialogClass dc, WidgetClass w) => dc -> w ->
                         ResponseId -> IO ()
dialogAddActionWidget dc child resId = {#call dialog_add_action_widget#}
  (toDialog dc) (toWidget child) (fromResponse resId)

-- | Query if the dialog has a visible horizontal
-- separator.
--
dialogGetHasSeparator :: DialogClass dc => dc -> IO Bool
dialogGetHasSeparator dc = liftM toBool $ 
  {#call unsafe dialog_get_has_separator#} (toDialog dc)

-- | Set the default widget that is to be
-- activated if the user pressed enter. The object is specified by the
-- ResponseId.
--
dialogSetDefaultResponse :: DialogClass dc => dc -> ResponseId -> IO ()
dialogSetDefaultResponse dc resId = {#call dialog_set_default_response#}
  (toDialog dc) (fromResponse resId)

-- | Set the visibility of the horizontal
-- separator.
--
dialogSetHasSeparator :: DialogClass dc => dc -> Bool -> IO ()
dialogSetHasSeparator dc set = {#call dialog_set_has_separator#}
  (toDialog dc) (fromBool set)

-- | Set widgets in the action are to be
-- sensitive or not.
--
dialogSetResponseSensitive :: DialogClass dc => dc -> ResponseId -> Bool ->
                              IO ()
dialogSetResponseSensitive dc resId sensitive = 
  {#call dialog_set_response_sensitive#} (toDialog dc) (fromResponse resId)
  (fromBool sensitive)

-- signals

-- | This signal is sent when a widget in the action
-- area was activated, the dialog is received a destory event or the user
-- calls dialogResponse. It is usually used to terminate the dialog (by
-- dialogRun for example).
--
onResponse, afterResponse :: DialogClass dc => dc -> (ResponseId -> IO ()) ->
                             IO (ConnectId dc)
onResponse dia act = connect_INT__NONE "response" False dia (act . toResponse)
afterResponse dia act = connect_INT__NONE "response" True dia (act . toResponse)
