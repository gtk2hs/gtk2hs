{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget FileChooserButton
--
--  Author : Duncan Coutts
--
--  Created: 5 April 2005
--
--  Copyright (C) 2005 Duncan Coutts
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
-- A button to launch a file selection dialog
--
-- * Module available since Gtk+ version 2.6
--
module Graphics.UI.Gtk.Selectors.FileChooserButton (
-- * Detail
--
-- | The 'FileChooserButton' is a widget that lets the user select a file. It
-- implements the 'FileChooser' interface. Visually, it is a file name with a
-- button to bring up a 'FileChooserDialog'. The user can then use that dialog
-- to change the file associated with that button. This widget does not support
-- setting the \"select-multiple\" property to @True@.
--
-- The 'FileChooserButton' supports the 'FileChooserAction's
-- 'FileChooserActionOpen' and 'FileChooserActionSelectFolder'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Box'
-- |                           +----'HBox'
-- |                                 +----FileChooserButton
-- @

#if GTK_CHECK_VERSION(2,6,0)
-- * Types
  FileChooserButton,
  FileChooserButtonClass,
  castToFileChooserButton, gTypeFileChooserButton,
  toFileChooserButton,

-- * Constructors
  fileChooserButtonNew,
#if GTK_MAJOR_VERSION < 3
  fileChooserButtonNewWithBackend,
#endif
  fileChooserButtonNewWithDialog,

-- * Methods
  fileChooserButtonGetTitle,
  fileChooserButtonSetTitle,
  fileChooserButtonGetWidthChars,
  fileChooserButtonSetWidthChars,

-- * Attributes
  fileChooserButtonDialog,
  fileChooserButtonTitle,
  fileChooserButtonWidthChars,

-- * Signals
  fileChooserButtonFileSet 
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

#if GTK_CHECK_VERSION(2,6,0)
{#import Graphics.UI.Gtk.Selectors.FileChooser#} (FileChooserAction)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Interfaces

instance FileChooserClass FileChooserButton

--------------------
-- Constructors

-- | Creates a new file-selecting button widget.
--
fileChooserButtonNew :: GlibString string
 => string               -- ^ @title@ - the title of the browse dialog.
 -> FileChooserAction    -- ^ @action@ - the open mode for the widget.
 -> IO FileChooserButton
fileChooserButtonNew title action =
  makeNewObject mkFileChooserButton $
  liftM (castPtr :: Ptr Widget -> Ptr FileChooserButton) $
  withUTFString title $ \titlePtr ->
  {# call gtk_file_chooser_button_new #}
    titlePtr
    ((fromIntegral . fromEnum) action)

#if GTK_MAJOR_VERSION < 3
-- | Creates a new file-selecting button widget using @backend@.
--
-- Removed in Gtk3.
fileChooserButtonNewWithBackend :: GlibString string
 => string               -- ^ @title@ - the title of the browse dialog.
 -> FileChooserAction    -- ^ @action@ - the open mode for the widget.
 -> string               -- ^ @backend@ - the name of the file system backend
                         -- to use.
 -> IO FileChooserButton
fileChooserButtonNewWithBackend title action backend =
  makeNewObject mkFileChooserButton $
  liftM (castPtr :: Ptr Widget -> Ptr FileChooserButton) $
  withUTFString backend $ \backendPtr ->
  withUTFString title $ \titlePtr ->
  {# call gtk_file_chooser_button_new_with_backend #}
    titlePtr
    ((fromIntegral . fromEnum) action)
    backendPtr
#endif

-- | Creates a 'FileChooserButton' widget which uses @dialog@ as it's
-- file-picking window.
--
fileChooserButtonNewWithDialog :: FileChooserDialogClass dialog =>
    dialog               -- ^ @dialog@ - the 'FileChooserDialog' widget to
                         -- use.
 -> IO FileChooserButton
fileChooserButtonNewWithDialog dialog =
  makeNewObject mkFileChooserButton $
  liftM (castPtr :: Ptr Widget -> Ptr FileChooserButton) $
  {# call gtk_file_chooser_button_new_with_dialog #}
    (toWidget dialog)

--------------------
-- Methods

-- | Retrieves the title of the browse dialog used by the button.
--
fileChooserButtonGetTitle :: (FileChooserButtonClass self, GlibString string) => self
 -> IO string -- ^ returns a pointer to the browse dialog's title.
fileChooserButtonGetTitle self =
  {# call gtk_file_chooser_button_get_title #}
    (toFileChooserButton self)
  >>= peekUTFString

-- | Modifies the @title@ of the browse dialog used by the button.
--
fileChooserButtonSetTitle :: (FileChooserButtonClass self, GlibString string) => self
 -> string -- ^ @title@ - the new browse dialog title.
 -> IO ()
fileChooserButtonSetTitle self title =
  withUTFString title $ \titlePtr ->
  {# call gtk_file_chooser_button_set_title #}
    (toFileChooserButton self)
    titlePtr

-- | Retrieves the width in characters of the @button@ widget's entry and\/or
-- label.
--
fileChooserButtonGetWidthChars :: FileChooserButtonClass self => self
 -> IO Int -- ^ returns an integer width (in characters) that the button will
           -- use to size itself.
fileChooserButtonGetWidthChars self =
  liftM fromIntegral $
  {# call gtk_file_chooser_button_get_width_chars #}
    (toFileChooserButton self)

-- | Sets the width (in characters) that the button will use to @nChars@.
--
fileChooserButtonSetWidthChars :: FileChooserButtonClass self => self
 -> Int   -- ^ @nChars@ - the new width, in characters.
 -> IO ()
fileChooserButtonSetWidthChars self nChars =
  {# call gtk_file_chooser_button_set_width_chars #}
    (toFileChooserButton self)
    (fromIntegral nChars)

--------------------
-- Attributes

-- | Instance of the 'FileChooserDialog' associated with the button.
--
fileChooserButtonDialog :: (FileChooserButtonClass self, FileChooserDialogClass fileChooserDialog) => WriteAttr self fileChooserDialog
fileChooserButtonDialog = writeAttrFromObjectProperty "dialog"
  {# call pure unsafe gtk_file_chooser_dialog_get_type #}

-- | Title to put on the 'FileChooserDialog' associated with the button.
--
-- Default value: \"Select A File\"
--
fileChooserButtonTitle :: (FileChooserButtonClass self, GlibString string) => Attr self string
fileChooserButtonTitle = newAttr
  fileChooserButtonGetTitle
  fileChooserButtonSetTitle

-- |
--
fileChooserButtonWidthChars :: FileChooserButtonClass self => Attr self Int
fileChooserButtonWidthChars = newAttr
  fileChooserButtonGetWidthChars
  fileChooserButtonSetWidthChars

--------------------
-- Signals

-- %hash c:b660 d:ab72
-- | Emitted when the user selects a file.
--
fileChooserButtonFileSet :: FileChooserButtonClass self => Signal self (IO ())
fileChooserButtonFileSet = Signal (connect_NONE__NONE "file-set")

#endif
