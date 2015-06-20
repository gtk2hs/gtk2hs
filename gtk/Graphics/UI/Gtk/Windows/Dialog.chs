{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Dialog
--
--  Author : Axel Simon, Andy Stewart
--
--  Created: 23 May 2001
--
--  Copyright (C) 1999-2005 Axel Simon
--  Copyright (C) 2009-2010 Andy Stewart
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
-- Create popup windows
--
-- NOTE:
--     Now FFI haven't support variadic function `gtk_dialog_set_alternative_button_order`
--
module Graphics.UI.Gtk.Windows.Dialog (
-- * Detail
--
-- | Dialog boxes are a convenient way to prompt the user for a small amount
-- of input, e.g. to display a message, ask a question, or anything else that
-- does not require extensive effort on the user's part.
--
-- Gtk+ treats a dialog as a window split vertically. The top section is a
-- 'VBox', and is where widgets such as a 'Label' or a 'Entry' should be
-- packed. The bottom area is known as the action_area. This is generally used
-- for packing buttons into the dialog which may perform functions such as
-- cancel, ok, or apply. The two areas are separated by a 'HSeparator'.
--
-- 'Dialog' boxes are created with a call to 'dialogNew' or
-- 'dialogNewWithButtons'. 'dialogNewWithButtons' is recommended; it allows you
-- to set the dialog title, some convenient flags, and add simple buttons.
--
-- If \'dialog\' is a newly created dialog, the two primary areas of the
-- window can be accessed using 'dialogGetUpper' and
-- 'dialogGetActionArea'.
--
-- A \'modal\' dialog (that is, one which freezes the rest of the
-- application from user input), can be created by calling 'windowSetModal' on
-- the dialog. When using 'dialogNewWithButtons' you can also
-- pass the 'DialogModal' flag to make a dialog modal.
--
-- If you add buttons to 'Dialog' using 'dialogNewWithButtons',
-- 'dialogAddButton', or 'dialogAddActionWidget', clicking
-- the button will emit a signal called \"response\" with a response ID that
-- you specified. Gtk+ will never assign a meaning to positive response IDs;
-- these are entirely user-defined. But for convenience, you can use the
-- response IDs in the 'ResponseType' enumeration (these all have values less
-- than zero). If a dialog receives a delete event, the \"response\" signal
-- will be emitted with a response ID of 'ResponseNone'.
--
-- If you want to block waiting for a dialog to return before returning
-- control flow to your code, you can call 'dialogRun'. This function enters a
-- recursive main loop and waits for the user to respond to the dialog,
-- returning the response ID corresponding to the button the user clicked.
--
-- For a simple message box, you probably want to use
-- 'Graphics.UI.Gtk.Windows.MessageDialog.MessageDialog' which provides
-- convenience functions
-- for creating standard dialogs containing simple messages to inform
-- or ask the user.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Window'
-- |                                 +----Dialog
-- |                                       +----'AboutDialog'
-- |                                       +----'ColorSelectionDialog'
-- |                                       +----'FileChooserDialog'
-- |                                       +----'FileSelection'
-- |                                       +----'FontSelectionDialog'
-- |                                       +----'InputDialog'
-- |                                       +----'MessageDialog'
-- @

-- * Types
  Dialog,
  DialogClass,
  castToDialog, gTypeDialog,
  toDialog,

-- * Enums
  ResponseId(..),

-- * Constructors
  dialogNew,

-- * Methods
#if GTK_MAJOR_VERSION < 3
  dialogGetUpper,
#endif
  dialogGetContentArea,
  dialogGetActionArea,
  dialogRun,
  dialogResponse,
  dialogAddButton,
  dialogAddActionWidget,
  dialogSetDefaultResponse,
#if GTK_MAJOR_VERSION < 3
  dialogGetHasSeparator,
  dialogSetHasSeparator,
#endif
  dialogSetResponseSensitive,
  dialogGetResponseForWidget,
  dialogAlternativeDialogButtonOrder,
  dialogSetAlternativeButtonOrderFromArray,
#if GTK_CHECK_VERSION(2,20,0)
  dialogGetWidgetForResponse,
#endif

-- * Attributes
#if GTK_MAJOR_VERSION < 3
  dialogHasSeparator,
#endif
  dialogActionAreaBorder,
  dialogButtonSpacing,
  dialogContentAreaBorder,
  dialogContentAreaSpacing,

-- * Signals
  response,

-- * Deprecated
#ifndef DISABLE_DEPRECATED
  onResponse,
  afterResponse,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Structs (
#if GTK_MAJOR_VERSION < 3
                    dialogGetUpper, dialogGetActionArea,
#endif
                                        ResponseId(..), fromResponse, toResponse)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new dialog box. Widgets should not be packed into this 'Window'
-- directly, but into the \"upper\" and \"action area\", which are obtained
-- using 'dialogGetUpper' and 'dialogGetActionArea'.
--
dialogNew :: IO Dialog
dialogNew =
  makeNewObject mkDialog $
  liftM (castPtr :: Ptr Widget -> Ptr Dialog) $
  {# call unsafe dialog_new #}

--------------------
-- Methods

-- | Blocks in a recursive main loop until the dialog either emits the
-- response signal, or is destroyed. If the dialog is destroyed during the call
-- to 'dialogRun', it returns 'ResponseNone'. Otherwise, it returns the
-- response ID from the \"response\" signal emission. Before entering the
-- recursive main loop, 'dialogRun' calls 'widgetShow' on the dialog for you.
-- Note that you still need to show any children of the dialog yourself.
--
-- During 'dialogRun', the default behavior of \"delete_event\" is disabled;
-- if the dialog receives \"delete_event\", it will not be destroyed as windows
-- usually are, and 'dialogRun' will return 'ResponseDeleteEvent'. Also, during
-- 'dialogRun' the dialog will be modal. You can force 'dialogRun' to return at
-- any time by calling 'dialogResponse' to emit the \"response\" signal.
-- Destroying the dialog during 'dialogRun' is a very bad idea, because your
-- post-run code won't know whether the dialog was destroyed or not.
-- Hence, you should not call 'Graphics.UI.Gtk.Abstract.widgetDestroy'
-- before 'dialogRun' has returned.
--
-- After 'dialogRun' returns, you are responsible for hiding or destroying
-- the dialog if you wish to do so.
--
-- Note that even though the recursive main loop gives the effect of a modal
-- dialog (it prevents the user from interacting with other windows while the
-- dialog is run), callbacks such as timeouts, IO channel watches, DND drops,
-- etc, /will/ be triggered during a 'dialogRun' call.
--
dialogRun :: DialogClass self => self
 -> IO ResponseId
dialogRun self =
  liftM toResponse $
  {# call dialog_run #}
    (toDialog self)

-- | Emits the \"response\" signal with the given response ID. Used to
-- indicate that the user has responded to the dialog in some way; typically
-- either you or 'dialogRun' will be monitoring the \"response\" signal and
-- take appropriate action.
--
-- This function can be used to add a custom widget to the action area that
-- should close the dialog when activated or to close the dialog otherwise.
--
dialogResponse :: DialogClass self => self
 -> ResponseId
 -> IO ()
dialogResponse self responseId =
  {# call dialog_response #}
    (toDialog self)
    (fromResponse responseId)

-- | Adds a button with the given text (or a stock button, if @buttonText@ is
-- a stock ID) and sets things up so that clicking the button will emit the
-- \"response\" signal with the given @responseId@. The button is appended to
-- the end of the dialog's action area. The button widget is returned, but
-- usually you don't need it.
--
dialogAddButton :: (DialogClass self, GlibString string) => self
 -> string     -- ^ @buttonText@ - text of button, or stock ID
 -> ResponseId -- ^ @responseId@ - response ID for the button
 -> IO Button  -- ^ returns the button widget that was added
dialogAddButton self buttonText responseId =
  makeNewObject mkButton $ liftM castPtr $
  withUTFString buttonText $ \buttonTextPtr ->
  {# call dialog_add_button #}
    (toDialog self)
    buttonTextPtr
    (fromResponse responseId)

-- | Adds an activatable widget to the action area of a 'Dialog', connecting a
-- signal handler that will emit the \"response\" signal on the dialog when the
-- widget is activated. The widget is appended to the end of the dialog's
-- action area. If you want to add a non-activatable widget, simply pack it
-- into the action area.
--
dialogAddActionWidget :: (DialogClass self, WidgetClass child) => self
 -> child      -- ^ @child@ - an activatable widget
 -> ResponseId -- ^ @responseId@ - response ID for @child@
 -> IO ()
dialogAddActionWidget self child responseId =
  {# call dialog_add_action_widget #}
    (toDialog self)
    (toWidget child)
    (fromResponse responseId)

#if GTK_MAJOR_VERSION < 3
-- | Query if the dialog has a visible horizontal separator.
--
-- Removed in Gtk3.
dialogGetHasSeparator :: DialogClass self => self -> IO Bool
dialogGetHasSeparator self =
  liftM toBool $
  {# call unsafe dialog_get_has_separator #}
    (toDialog self)

-- | Sets whether the dialog has a separator above the buttons. @True@ by
-- default.
--
-- Removed in Gtk3.
dialogSetHasSeparator :: DialogClass self => self -> Bool -> IO ()
dialogSetHasSeparator self setting =
  {# call dialog_set_has_separator #}
    (toDialog self)
    (fromBool setting)
#endif

-- | Sets the last widget in the dialog's action area with the given
-- 'ResponseId' as the default widget for the dialog. Pressing \"Enter\"
-- normally activates the default widget.
--
-- * The default response is reset once it is triggered. Hence, if you
--   hide the dialog (rather than closing it) and re-display it later,
--   you need to call this function again.
--
dialogSetDefaultResponse :: DialogClass self => self
 -> ResponseId
 -> IO ()
dialogSetDefaultResponse self responseId =
  {# call dialog_set_default_response #}
    (toDialog self)
    (fromResponse responseId)

-- | Calls @'widgetSetSensitive' widget setting@ for each widget in the
-- dialog's action area with the given @responseId@. A convenient way to
-- sensitize\/desensitize dialog buttons.
--
dialogSetResponseSensitive :: DialogClass self => self
 -> ResponseId -- ^ @responseId@ - a response ID
 -> Bool       -- ^ @setting@ - @True@ for sensitive
 -> IO ()
dialogSetResponseSensitive self responseId setting =
  {# call dialog_set_response_sensitive #}
    (toDialog self)
    (fromResponse responseId)
    (fromBool setting)

-- | Gets the response id of a widget in the action area of a dialog.
dialogGetResponseForWidget :: (DialogClass self, WidgetClass widget) => self
 -> widget  -- ^ @widget@ - a widget in the action area of dialog
 -> IO ResponseId  -- ^ return the response id of widget, or 'ResponseNone' if widget doesn't have a response id set.
dialogGetResponseForWidget self widget = liftM toResponse $
  {# call dialog_get_response_for_widget #}
    (toDialog self)
    (toWidget widget)

-- | Returns @True@ if dialogs are expected to use an alternative button order on the screen screen.
-- See 'dialogSetAlternativeButtonOrder' for more details about alternative button order.
--
-- If you need to use this function, you should probably connect to the 'alternativeButtonOrder' signal on the GtkSettings object associated to  screen, in order to be notified if the button order setting changes.
--
-- * Available since Gtk+ version 2.6
--
dialogAlternativeDialogButtonOrder ::
   Maybe Screen  -- ^ @screen@ - a 'Screen', or @Nothing@ to use the default screen
 -> IO Bool   -- ^ returns whether the alternative button order should be used
dialogAlternativeDialogButtonOrder (Just screen) = liftM toBool $
  {# call alternative_dialog_button_order #} screen
dialogAlternativeDialogButtonOrder Nothing = liftM toBool $
  {# call alternative_dialog_button_order #} (Screen nullForeignPtr)

-- | Sets an alternative button order.
--
-- If the 'alternativeButtonOrder' setting is set to @True@, the dialog
-- buttons are reordered according to the order of the response ids in
-- @newOrder@.
--
-- See 'dialogSetAlternativeButtonOrder' for more information.
--
-- This function is for use by language bindings.
--
-- * Available since Gtk+ version 2.6
--
dialogSetAlternativeButtonOrderFromArray :: DialogClass self => self
 -> [ResponseId]  -- ^ @newOrder@ - an array of response ids of dialog's buttons
 -> IO ()
dialogSetAlternativeButtonOrderFromArray self newOrder =
  withArray (map fromResponse newOrder) $ \newOrderPtr ->
  {# call dialog_set_alternative_button_order_from_array #}
    (toDialog self)
    (fromIntegral (length newOrder))
    newOrderPtr

#if GTK_CHECK_VERSION(2,20,0)
-- | Gets the widget button that uses the given response ID in the action area of a dialog.
dialogGetWidgetForResponse :: DialogClass self => self
                           -> ResponseId -- ^ @responseId@ the response ID used by the dialog widget
                           -> IO (Maybe Widget) -- ^ returns     the widget button that uses the given @responseId@, or 'Nothing'.
dialogGetWidgetForResponse self responseId =
    maybeNull (makeNewObject mkWidget) $
    {#call gtk_dialog_get_widget_for_response #}
      (toDialog self)
      (fromResponse responseId)
#endif

#if GTK_MAJOR_VERSION >= 3
-- | Returns the content area of dialog.
dialogGetContentArea :: DialogClass self => self -> IO Widget
dialogGetContentArea self =
    makeNewObject mkWidget $
    {#call gtk_dialog_get_content_area #}
      (toDialog self)

-- | Returns the action area of dialog.
--
-- * This is useful to add some special widgets that cannot be added with
-- dialogAddActionWidget.
--
dialogGetActionArea :: DialogClass self => self -> IO Widget
dialogGetActionArea self =
    makeNewObject mkWidget $
    {#call gtk_dialog_get_content_area #}
      (toDialog self)
#else
dialogGetContentArea self = liftM toWidget $ dialogGetUpper self
#endif

--------------------
-- Attributes
#if GTK_MAJOR_VERSION < 3
-- | The dialog has a separator bar above its buttons.
--
-- Default value: @True@
--
-- Removed in Gtk3.
dialogHasSeparator :: DialogClass self => Attr self Bool
dialogHasSeparator = newAttr
  dialogGetHasSeparator
  dialogSetHasSeparator
#endif

-- | Width of border around the button area at the bottom of the dialog.
--
-- Allowed values: >= 0
--
-- Default value: 5
--
dialogActionAreaBorder :: DialogClass self => ReadAttr self Int
dialogActionAreaBorder = readAttrFromIntProperty "action-area-border"

-- | Spacing between buttons.
--
-- Allowed values: >= 0
--
-- Default value: 6
--
dialogButtonSpacing :: DialogClass self => ReadAttr self Int
dialogButtonSpacing = readAttrFromIntProperty "button-spacing"

-- | Width of border around the main dialog area.
--
-- Allowed values: >= 0
--
-- Default value: 2
--
dialogContentAreaBorder :: DialogClass self => ReadAttr self Int
dialogContentAreaBorder = readAttrFromIntProperty "content-area-border"

-- | The default spacing used between elements of the content area of the dialog,
-- as returned by 'dialogSetContentArea', unless 'boxSetSpacing' was called on that widget directly.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
-- * Available since Gtk+ version 2.16
--
dialogContentAreaSpacing :: DialogClass self => ReadAttr self Int
dialogContentAreaSpacing = readAttrFromIntProperty "content-area-spacing"

--------------------
-- Signals

-- | Emitted when an action widget is clicked, the dialog receives a delete
-- event, or the application programmer calls 'dialogResponse'. On a delete
-- event, the response ID is 'ResponseNone'. Otherwise, it depends on which
-- action widget was clicked.
--
response :: DialogClass self => Signal self (ResponseId -> IO ())
response = Signal (\after obj fun ->
                   connect_INT__NONE "response" after obj (\i -> fun (toResponse i)))

-- * Deprecated
#ifndef DISABLE_DEPRECATED
-- | Emitted when an action widget is clicked, the dialog receives a delete
-- event, or the application programmer calls 'dialogResponse'. On a delete
-- event, the response ID is 'ResponseNone'. Otherwise, it depends on which
-- action widget was clicked.
--
onResponse, afterResponse :: DialogClass self => self
 -> (ResponseId -> IO ())
 -> IO (ConnectId self)
onResponse dia act = connect_INT__NONE "response" False dia (act . toResponse)
afterResponse dia act = connect_INT__NONE "response" True dia (act . toResponse)
#endif

