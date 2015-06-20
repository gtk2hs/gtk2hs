{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget FileChooserDialog
--
--  Author : Duncan Coutts
--
--  Created: 24 April 2004
--
--  Copyright (C) 2004-2005 Duncan Coutts
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
-- A file chooser dialog, suitable for \"File\/Open\" or \"File\/Save\"
-- commands
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.Selectors.FileChooserDialog (
-- * Detail
--
-- | 'FileChooserDialog' is a dialog box suitable for use with \"File\/Open\"
-- or \"File\/Save as\" commands. This widget works by putting a
-- 'FileChooserWidget' inside a 'Dialog'. It exposes the 'FileChooser',
-- interface, so you can use all of the
-- 'FileChooser' functions on the file chooser dialog as well as those for
-- 'Dialog'.
--
-- Note that 'FileChooserDialog' does not have any methods of its own.
-- Instead, you should use the functions that work on a 'FileChooser'.

-- ** Response Codes
--
-- | 'FileChooserDialog' inherits from 'Dialog', so buttons that go in its
-- action area have response codes such as 'ResponseAccept' and
-- 'ResponseCancel'.

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
-- |                                       +----FileChooserDialog
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  FileChooserDialog,
  FileChooserDialogClass,
  castToFileChooserDialog, gTypeFileChooserDialog,
  toFileChooserDialog,

  -- * Constructors
  fileChooserDialogNew,
  fileChooserDialogNewWithBackend
#endif
  ) where

import Control.Monad (liftM, when)
import Data.Maybe (isJust, fromJust)

import System.Glib.FFI
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Selectors.FileChooser#}
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Windows.Window
import Graphics.UI.Gtk.Windows.Dialog
import System.Glib.GValue               (allocaGValue)
import System.Glib.GValueTypes          (valueSetMaybeString)
import System.Glib.Attributes

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,4,0)
--------------------
-- Interfaces

instance FileChooserClass FileChooserDialog

--------------------
-- Constructors

-- | Creates a new 'FileChooserDialog'.
--
fileChooserDialogNew
  :: GlibString string
  => Maybe string            -- ^ Title of the dialog (or default)
  -> Maybe Window            -- ^ Transient parent of the dialog (or none)
  -> FileChooserAction       -- ^ Open or save mode for the dialog
  -> [(string, ResponseId)]  -- ^ Buttons and their response codes
  -> IO FileChooserDialog
fileChooserDialogNew title parent action buttons =
  internalFileChooserDialogNew title parent action buttons Nothing

-- | Creates a new 'FileChooserDialog' with a specified backend. This is
-- especially useful if you use 'fileChooserSetLocalOnly' to allow non-local
-- files and you use a more expressive vfs, such as gnome-vfs, to load files.
--
fileChooserDialogNewWithBackend
  :: GlibString string
  => Maybe string              -- ^ Title of the dialog (or default)
  -> Maybe Window              -- ^ Transient parent of the dialog (or none)
  -> FileChooserAction         -- ^ Open or save mode for the dialog
  -> [(string, ResponseId)]    -- ^ Buttons and their response codes
  -> string                    -- ^ The name of the filesystem backend to use
  -> IO FileChooserDialog
fileChooserDialogNewWithBackend title parent action buttons backend =
  internalFileChooserDialogNew title parent action buttons (Just backend)

-- Annoyingly, the constructor for FileChooserDialog uses varargs so we can't
-- call it using the Haskell FFI. The GTK people do not consider this an api
-- bug, see <http://bugzilla.gnome.org/show_bug.cgi?id=141004>
-- The solution is to call objectNew and add the buttons manually.

internalFileChooserDialogNew :: GlibString string =>
  Maybe string ->           -- Title of the dialog (or default)
  Maybe Window ->           -- Transient parent of the dialog (or none)
  FileChooserAction ->      -- Open or save mode for the dialog
  [(string, ResponseId)] -> -- Buttons and their response codes
  Maybe string ->           -- The name of the backend to use (optional)
  IO FileChooserDialog
internalFileChooserDialogNew title parent action buttons backend = do
  objType <- {# call unsafe gtk_file_chooser_dialog_get_type #}
  dialog <-makeNewObject mkFileChooserDialog $ liftM castPtr $
           if (isJust backend)
             then allocaGValue $ \backendGValue -> do
                  valueSetMaybeString backendGValue backend
                  objectNew objType [("file-system-backend", backendGValue)]
             else objectNew objType []
  when (isJust title)
       (set dialog [windowTitle := fromJust title])
  when (isJust parent)
       (set dialog [windowTransientFor := fromJust parent])
  dialog `fileChooserSetAction` action
  mapM_ (\(btnName, btnResponse) ->
          dialogAddButton dialog btnName btnResponse) buttons
  return dialog
#endif
