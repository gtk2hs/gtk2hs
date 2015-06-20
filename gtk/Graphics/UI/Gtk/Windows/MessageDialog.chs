{-# LANGUAGE CPP, OverloadedStrings #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget MessageDialog
--
--  Author : Axel Simon
--
--  Created: 20 October 2006
--
--  Copyright (C) 2006 Axel Simon
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
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A convenient message window
--
module Graphics.UI.Gtk.Windows.MessageDialog (
-- * Detail
--
-- | 'MessageDialog' presents a dialog with an image representing the type of
-- message (Error, Question, etc.) alongside some message text. It's simply a
-- convenience widget; you could construct the equivalent of 'MessageDialog'
-- from 'Dialog' without too much effort, but 'MessageDialog' saves typing.
--
-- The easiest way to do a modal message dialog is to use 'dialogRun',
-- though you can also pass in the 'DialogModal' flag, 'dialogRun'
-- automatically makes the dialog modal and waits for the user to respond to
-- it. 'dialogRun' returns when any dialog button is clicked.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Window'
-- |                                 +----'Dialog'
-- |                                       +----MessageDialog
-- @

-- * Types
  MessageDialog,
  MessageDialogClass,
  castToMessageDialog, gTypeMessageDialog,
  toMessageDialog,
  MessageType(..),
  ButtonsType(..),
  DialogFlags(..),

-- * Constructors
  messageDialogNew,
#if GTK_CHECK_VERSION(2,4,0)
  messageDialogNewWithMarkup,
#endif

-- * Methods
#if GTK_CHECK_VERSION(2,4,0)
  messageDialogSetMarkup,
#endif
#if GTK_CHECK_VERSION(2,10,0)
  messageDialogSetImage,
#endif
#if GTK_CHECK_VERSION(2,6,0)
  messageDialogSetSecondaryMarkup,
  messageDialogSetSecondaryText,
#endif

-- * Attributes
  messageDialogMessageType,
#if GTK_CHECK_VERSION(2,10,0)
  messageDialogText,
  messageDialogUseMarkup,
  messageDialogSecondaryText,
  messageDialogSecondaryUseMarkup,
  messageDialogImage,
#endif
  messageDialogButtons,
#if GTK_CHECK_VERSION(2,22,0)
  messageDialogMessageArea,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
{#import Graphics.UI.Gtk.Types#}
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.Flags        (Flags, fromFlags)
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Types

-- | Specify what message icon this dialog should show.
--
#if GTK_CHECK_VERSION(2,10,0)
--
-- * From Gtk 2.10 onwards, you can pass 'MessageOther' and supply your
--   own image using 'messageDialogSetImage'.
--
#endif
{#enum MessageType {underscoreToCase} deriving(Show,Eq)#}

-- | Specify what buttons this dialog should show.
--
-- * Prebuilt sets of buttons for the dialog. If none of these choices
--   are appropriate, simply use 'ButtonsNone' then call 'dialogAddButton'.
--
{#enum ButtonsType {underscoreToCase} deriving(Show,Eq)#}

-- | Flags used to influence dialog construction.
--
-- * Marking a dialog as model will call 'widgetSetModal' on the dialog
--   window, the 'DialogDestroyWithParent' will call
--   'windowSetDestroyWithParent' on the dialog window. Note that in
--   case the dialog is simply destroyed, no response signal is ever
--   emitted. Finally, 'DialogNoSeparator' omits the separator between
--   the action area and the dialog content which is preferable for
--   very simple messages, i.e. those that only contain one button.
--
{#enum DialogFlags {underscoreToCase} deriving (Show,Eq,Bounded)#}

instance Flags DialogFlags

--------------------
-- Constructors

-- | Create a new message dialog, which is a simple dialog with an icon
--   indicating the dialog type (error, warning, etc.) and some text the
--   user may want to see. When the user clicks a button a \"response\" signal
--   is emitted with response IDs from 'ResponseType'. See 'Dialog' for more
--   details.
--
messageDialogNew
  :: GlibString string
  => Maybe Window  -- ^ Transient parent of the dialog (or none)
  -> [DialogFlags]
  -> MessageType
  -> ButtonsType
  -> string        -- ^ The text of the message
  -> IO MessageDialog
messageDialogNew mWindow flags mType bType msg =
  withUTFString (unPrintf msg) $ \msgPtr ->
  makeNewObject mkMessageDialog $
  liftM (castPtr :: Ptr Widget -> Ptr MessageDialog) $
  call_message_dialog_new mWindow flags mType bType msgPtr
                                
                                
call_message_dialog_new :: Maybe Window -> [DialogFlags] ->
                           MessageType -> ButtonsType -> Ptr CChar ->
                           IO (Ptr Widget)
call_message_dialog_new (Just (Window fPtr)) flags mType bType msgPtr =
  withForeignPtr fPtr $ \ptr ->
    message_dialog_new ptr (fromIntegral (fromFlags flags))
      (fromIntegral (fromEnum mType))
      (fromIntegral (fromEnum bType)) msgPtr
call_message_dialog_new Nothing flags mType bType msgPtr =
    message_dialog_new nullPtr (fromIntegral (fromFlags flags))
      (fromIntegral (fromEnum mType))
      (fromIntegral (fromEnum bType)) msgPtr

foreign import ccall unsafe "gtk_message_dialog_new"
  message_dialog_new :: Ptr Window -> CInt -> CInt -> CInt ->
                        Ptr CChar -> IO (Ptr Widget)

#if GTK_CHECK_VERSION(2,4,0)
-- | Creates a new message dialog, which is a simple dialog with an icon
--   indicating the dialog type (error, warning, etc.) and some text which
--   is marked up with the Pango text markup language. When the user clicks
--   a button a \"response\" signal is emitted with response IDs from
--   'ResponseType'. See 'Dialog' and 'PangoMarkup' for more details.
--
-- * Available since Gtk+ version 2.4
--
messageDialogNewWithMarkup
  :: GlibString string
  => Maybe Window  -- ^ Transient parent of the dialog (or none)
  -> [DialogFlags]
  -> MessageType
  -> ButtonsType
  -> string        -- ^ The text of the message
  -> IO MessageDialog
messageDialogNewWithMarkup mWindow flags mType bType msg = do
  md <- makeNewObject mkMessageDialog $
    liftM (castPtr :: Ptr Widget -> Ptr MessageDialog) $
    call_message_dialog_new mWindow flags mType bType nullPtr
  messageDialogSetMarkup md msg
  return md
#endif

--------------------
-- Methods

#if GTK_CHECK_VERSION(2,4,0)
-- | Sets the text of the message dialog to be @str@, which is marked up with
-- the Pango text markup language.
--
-- * Available since Gtk+ version 2.4
--
messageDialogSetMarkup :: (MessageDialogClass self, GlibString string) => self
 -> string -- ^ @str@ - markup string (see Pango markup format)
 -> IO ()
messageDialogSetMarkup self str =
  withUTFString (unPrintf str) $ \strPtr ->
  {# call gtk_message_dialog_set_markup #}
    (toMessageDialog self)
    strPtr
#endif

#if GTK_CHECK_VERSION(2,6,0)
messageDialogSetSecondaryMarkup :: (MessageDialogClass self, GlibString string) => self
 -> string -- ^ @str@ - markup string (see Pango markup format)
 -> IO ()
messageDialogSetSecondaryMarkup self str =
  withUTFString (unPrintf str) $ \strPtr ->
  let (MessageDialog fPtr) = toMessageDialog self in
  withForeignPtr fPtr $ \ptr ->
  message_dialog_format_secondary_markup ptr strPtr

foreign import ccall unsafe "gtk_message_dialog_format_secondary_markup"
  message_dialog_format_secondary_markup :: Ptr MessageDialog ->
                                           Ptr CChar -> IO ()
                                        
messageDialogSetSecondaryText :: (MessageDialogClass self, GlibString string) => self
 -> string -- ^ @str@ - text to be shown as second line
 -> IO ()
messageDialogSetSecondaryText self str =
  withUTFString str $ \strPtr ->
  let (MessageDialog fPtr) = toMessageDialog self in
  withForeignPtr fPtr $ \ptr ->
  message_dialog_format_secondary_text ptr strPtr

foreign import ccall unsafe "gtk_message_dialog_format_secondary_text"
  message_dialog_format_secondary_text :: Ptr MessageDialog ->
                                         Ptr CChar -> IO ()

#if GTK_CHECK_VERSION(2,10,0)
-- %hash c:6cb7 d:ebdd
-- | Sets the dialog's image to @image@.
--
-- * Available since Gtk+ version 2.10
--
messageDialogSetImage :: (MessageDialogClass self, WidgetClass image) => self
 -> image -- ^ @image@ - the image
 -> IO ()
messageDialogSetImage self image =
  {# call gtk_message_dialog_set_image #}
    (toMessageDialog self)
    (toWidget image)
#endif
#endif


--------------------
-- Attributes

-- | The type of message.
--
-- Default value: 'MessageInfo'
--
messageDialogMessageType :: MessageDialogClass self => Attr self MessageType
messageDialogMessageType = newAttrFromEnumProperty "message-type"
  {#call pure unsafe gtk_message_type_get_type #}

#if GTK_CHECK_VERSION(2,10,0)
-- %hash c:a2fe d:e4a2
-- | The primary text of the message dialog. If the dialog has a secondary
-- text, this will appear as the title.
--
-- Default value: @Nothing@
--
-- * Available since Gtk+ version 2.10
--
messageDialogText :: (MessageDialogClass self, GlibString string) => Attr self (Maybe string)
messageDialogText = newAttrFromMaybeStringProperty "text"

-- %hash c:e1dd d:ca3
-- | Interpret the string 'messageDialogText' as markup.
--
-- Default value: @False@
--
-- * Available since Gtk+ version 2.10
--
messageDialogUseMarkup :: MessageDialogClass self => Attr self Bool
messageDialogUseMarkup = newAttrFromBoolProperty "use-markup"

-- %hash c:9623 d:1fbe
-- | The secondary text of the message dialog.
--
-- Default value: @Nothing@
--
-- * Available since Gtk+ version 2.10
--
messageDialogSecondaryText :: (MessageDialogClass self, GlibString string) => Attr self (Maybe string)
messageDialogSecondaryText = newAttrFromMaybeStringProperty "secondary-text"

-- %hash c:1ce2 d:ca3
-- | Default value: @False@
--
-- * Available since Gtk+ version 2.10
--
messageDialogSecondaryUseMarkup :: MessageDialogClass self => Attr self Bool
messageDialogSecondaryUseMarkup = newAttrFromBoolProperty "secondary-use-markup"

-- %hash c:da36 d:b7dd
-- | The image for this dialog.
--
-- * Available since Gtk+ version 2.10
--
messageDialogImage :: (MessageDialogClass self, WidgetClass widget) => ReadWriteAttr self Widget widget
messageDialogImage = newAttrFromObjectProperty "image"
                       {# call pure unsafe gtk_widget_get_type #}
#endif

-- | The buttons shown in the message dialog.
--
-- Default value: 'ButtonsNone'
--
messageDialogButtons :: MessageDialogClass self => WriteAttr self ButtonsType
messageDialogButtons = writeAttrFromEnumProperty "buttons"
  {#call pure unsafe gtk_buttons_type_get_type #}

#if GTK_CHECK_VERSION(2,22,0)
-- | The 'VBox' that corresponds to the message area of this dialog.
--
-- * Available since Gtk+ version 2.22
--
messageDialogMessageArea :: MessageDialogClass self => ReadAttr self VBox
messageDialogMessageArea = readAttrFromObjectProperty "message-area"
  {# call pure unsafe gtk_vbox_get_type #}
#endif

