-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget FileChooserDialog
--
--  Author : Duncan Coutts
--
--  Created: 24 April 2004
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/12 17:19:25 $
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
-- The file chooser dialog and widget is a replacement
-- for the old "FileSel"ection dialog. It provides a better user
-- interface and an improved API.
--
-- * This is the dialog variant of the "FileChooser"
--
-- * Added in GTK+ 2.4
--
module Graphics.UI.Gtk.Selectors.FileChooserDialog  (
#if GTK_CHECK_VERSION(2,4,0)
  FileChooserDialogClass,
  FileChooserDialog,
  fileChooserDialogNew,
  fileChooserDialogNewWithBackend
#endif
) where

#if GTK_CHECK_VERSION(2,4,0)

import Monad (liftM, when)
import Maybe (isJust, fromJust)

import System.Glib.FFI
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Selectors.FileChooser#}
import System.Glib.GObject (objectNew)
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Windows.Window
import Graphics.UI.Gtk.Windows.Dialog
import System.Glib.GValue
import System.Glib.StoreValue

{# context lib="gtk" prefix ="gtk" #}

-- The FileChooserDialog implements the FileChooser interface
-- which we model in Haskell as another instance decleration
instance FileChooserClass FileChooserDialog

fileChooserDialogNew
  :: Maybe String            -- ^ Title of the dialog (or default)
  -> Maybe Window            -- ^ Transient parent of the dialog (or none)
  -> FileChooserAction       -- ^ Open or save mode for the dialog
  -> [(String, ResponseId)]  -- ^ Buttons and their response codes
  -> IO FileChooserDialog
fileChooserDialogNew title parent action buttons =
  internalFileChooserDialogNew title parent action buttons Nothing


fileChooserDialogNewWithBackend
  :: Maybe String              -- ^ Title of the dialog (or default)
  -> Maybe Window              -- ^ Transient parent of the dialog (or none)
  -> FileChooserAction         -- ^ Open or save mode for the dialog
  -> [(String, ResponseId)]    -- ^ Buttons and their response codes
  -> String                    -- ^ The name of the filesystem backend to use
  -> IO FileChooserDialog
fileChooserDialogNewWithBackend title parent action buttons backend =
  internalFileChooserDialogNew title parent action buttons (Just backend)
  

-- Annoyingly, the constructor for FileChooserDialog uses varargs so we can't
-- call it using the Haskell FFI. The GTK people do not consider this an api
-- bug, see <http://bugzilla.gnome.org/show_bug.cgi?id=141004>
-- The solution is to call objectNew and add the buttons manually.

internalFileChooserDialogNew ::
  Maybe String ->           -- Title of the dialog (or default)
  Maybe Window ->           -- Transient parent of the dialog (or none)
  FileChooserAction ->      -- Open or save mode for the dialog
  [(String, ResponseId)] -> -- Buttons and their response codes
  Maybe String ->           -- The name of the backend to use (optional)
  IO FileChooserDialog
internalFileChooserDialogNew title parent action buttons backend = do
  objType <- {# call unsafe gtk_file_chooser_dialog_get_type #}
  dialog <-makeNewObject mkFileChooserDialog $ liftM castPtr $
           if (isJust backend)
             then with (GVstring backend) $ \backendGValue ->
                  objectNew objType [("file-system-backend", backendGValue)]
             else objectNew objType []
  when (isJust title)
       (dialog `windowSetTitle` fromJust title)
  when (isJust parent)
       (dialog `windowSetTransientFor` fromJust parent)
  dialog `fileChooserSetAction` action
  mapM_ (\(btnName, btnResponse) ->
          dialogAddButton dialog btnName btnResponse) buttons
  return dialog

#endif
