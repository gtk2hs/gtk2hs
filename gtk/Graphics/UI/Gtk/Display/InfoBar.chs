{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget InfoBar
--
--  Author : Andy Stewart
--
--  Created: 27 Mar 2010
--
--  Copyright (C) 2010 Andy Stewart
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
-- The following varargs functions can't bound:
--
--   gtk_info_bar_new_with_buttons
--   gtk_info_bar_add_buttons
--
-- Use 'infoBarAddButton' replace.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Report important messages to the user
--
module Graphics.UI.Gtk.Display.InfoBar (

-- * Detail
--
-- | 'InfoBar' is a widget that can be used to show messages to the user
-- without showing a dialog. It is often temporarily shown at the top or bottom
-- of a document. In contrast to 'Dialog', which has a horizontal action area
-- at the bottom, 'InfoBar' has a vertical action area at the side.
--
-- The API of 'InfoBar' is very similar to 'Dialog', allowing you to add
-- buttons to the action area with 'infoBarAddButton'.
-- The sensitivity of action widgets can be controlled
-- with 'infoBarSetResponseSensitive'. To add widgets to the main content area
-- of a 'InfoBar', use 'infoBarGetContentArea' and add your widgets to the
-- container.
--
-- Similar to 'MessageDialog', the contents of a 'InfoBar' can by classified
-- as error message, warning, informational message, etc, by using
-- 'infoBarSetMessageType'. Gtk+ uses the message type to determine the
-- background color of the message area.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Box'
-- |                           +----'HBox'
-- |                                 +----InfoBar
-- @

#if GTK_CHECK_VERSION(2,18,0)
-- * Types
  InfoBar,
  InfoBarClass,
  castToInfoBar,
  toInfoBar,

-- * Constructors
  infoBarNew,

-- * Methods
  infoBarAddActionWidget,
  infoBarAddButton,
  infoBarSetResponseSensitive,
  infoBarSetDefaultResponse,
  infoBarEmitResponse,
  infoBarGetActionArea,
  infoBarGetContentArea,

-- * Attributes
  infoBarMessageType,

-- * Signals
  infoBarResponse,
  infoBarClose,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
{#import Graphics.UI.Gtk.Abstract.Object#}      (makeNewObject)
{#import Graphics.UI.Gtk.Windows.MessageDialog#} (MessageType)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Interfaces

-- instance BuildableClass InfoBar

--------------------
-- Constructors

#if GTK_CHECK_VERSION(2,18,0)
-- | Creates a new 'InfoBar' object.
--
-- * Available since Gtk+ version 2.18
--
infoBarNew :: IO InfoBar
infoBarNew =
  makeNewObject mkInfoBar $
  liftM (castPtr :: Ptr Widget -> Ptr InfoBar) $
  {# call gtk_info_bar_new #}

--------------------
-- Methods

-- | Add an activatable widget to the action area of a 'InfoBar', connecting a signal handler that will
-- emit the "response" signal on the message area when the widget is activated. The widget is appended
-- to the end of the message areas action area.
--
-- * Available since Gtk+ version 2.18
--
infoBarAddActionWidget :: (InfoBarClass self, WidgetClass child) => self
 -> child -- ^ @child@ - an activatable widget
 -> Int   -- ^ @responseId@ - response ID for @child@
 -> IO ()
infoBarAddActionWidget self child responseId =
  {# call gtk_info_bar_add_action_widget #}
    (toInfoBar self)
    (toWidget child)
    (fromIntegral responseId)

-- | Adds a button with the given text (or a stock button, if buttonText is a
-- stock ID) and sets things up so that clicking the button will emit the
-- \"response\" signal with the given responseId. The button is appended to
-- the end of the info bars's action area. The button widget is returned, but
-- usually you don't need it.
--
-- * Available since Gtk+ version 2.18
--
infoBarAddButton :: (InfoBarClass self, GlibString string) => self
 -> string    -- ^ @buttonText@ - text of button, or stock ID
 -> Int       -- ^ @responseId@ - response ID for the button
 -> IO Button -- ^ returns the button widget that was added
infoBarAddButton self buttonText responseId =
  makeNewObject mkButton $
  withUTFString buttonText $ \buttonTextPtr ->
  liftM (castPtr :: Ptr Widget -> Ptr Button) $
     {# call gtk_info_bar_add_button #}
       (toInfoBar self)
       buttonTextPtr
       (fromIntegral responseId)

-- | Calls 'widgetSetSensitive' for each widget in the
-- info bars's action area with the given responseId. A convenient way to
-- sensitize\/desensitize dialog buttons.
--
-- * Available since Gtk+ version 2.18
--
infoBarSetResponseSensitive :: InfoBarClass self => self
 -> Int  -- ^ @responseId@ - a response ID
 -> Bool -- ^ @setting@ - @True@ for sensitive
 -> IO ()
infoBarSetResponseSensitive self responseId setting =
  {# call gtk_info_bar_set_response_sensitive #}
    (toInfoBar self)
    (fromIntegral responseId)
    (fromBool setting)

-- | Sets the last widget in the info bar's action area with the given
-- responseId as the default widget for the dialog. Pressing \"Enter\"
-- normally activates the default widget.
--
-- * Available since Gtk+ version 2.18
--
infoBarSetDefaultResponse :: InfoBarClass self => self
 -> Int -- ^ @responseId@ - a response ID
 -> IO ()
infoBarSetDefaultResponse self responseId =
  {# call gtk_info_bar_set_default_response #}
    (toInfoBar self)
    (fromIntegral responseId)

-- | Emits the \'response\' signal with the given @responseId@.
--
-- * Available since Gtk+ version 2.18
--
infoBarEmitResponse :: InfoBarClass self => self
 -> Int -- ^ @responseId@ - a response ID
 -> IO ()
infoBarEmitResponse self responseId =
  {# call gtk_info_bar_response #}
    (toInfoBar self)
    (fromIntegral responseId)

-- | Returns the action area of @infoBar@.
--
-- * Available since Gtk+ version 2.18
--
infoBarGetActionArea :: InfoBarClass self => self
 -> IO Widget -- ^ returns the action area.
infoBarGetActionArea self =
  makeNewObject mkWidget $
  {# call gtk_info_bar_get_action_area #}
    (toInfoBar self)

-- | Returns the content area of @infoBar@.
--
-- * Available since Gtk+ version 2.18
--
infoBarGetContentArea :: InfoBarClass self => self
 -> IO Widget -- ^ returns the content area.
infoBarGetContentArea self =
  makeNewObject mkWidget $
  {# call gtk_info_bar_get_content_area #}
    (toInfoBar self)

--------------------
-- Attributes

-- | The type of the message.
--
-- The type is used to determine the colors to use in the info bar.
--
-- If the type is 'MessageOther', no info bar is painted but the colors are still set.
--
-- Default value: 'MessageInfo'
--
-- * Available since Gtk+ version 2.18
--
infoBarMessageType :: InfoBarClass self => Attr self MessageType
infoBarMessageType = newAttrFromEnumProperty "message-type"
                       {# call pure unsafe gtk_message_type_get_type #}

--------------------
-- Signals

-- | The 'close' signal is a keybinding signal which gets emitted when the user uses a keybinding to
-- dismiss the info bar.
--
-- The default binding for this signal is the Escape key.
--
-- Since 2.18
infoBarClose :: InfoBarClass self => Signal self (IO ())
infoBarClose = Signal (connect_NONE__NONE "close")


-- | Emitted when an action widget is clicked or the application programmer
-- calls 'dialogResponse'. The @responseId@ depends on which action widget was
-- clicked.
--
-- * Available since Gtk+ version 2.18
--
infoBarResponse :: InfoBarClass self => Signal self (Int -> IO ())
infoBarResponse = Signal (connect_INT__NONE "response")
#endif

