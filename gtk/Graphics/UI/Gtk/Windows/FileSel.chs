-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget FileSelection
--
--  Author : Manuel M T Chakravarty
--
--  Created: 20 January 1999
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/12 17:19:26 $
--
--  Copyright (C) 1999-2005 Manuel M T Chakravarty, Jens Petersen
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
-- The file selection widget is a quick and simple way to display a File
-- dialog box.  It comes complete with Ok & Cancel buttons; optionally, it
-- can have file operation buttons.
--
-- * As of gtk 2.4 this module has been deprecated in favour of "FileChooser"
--
-- TODO
--
-- * Fix fileSelectionQueryButtons
--
module Graphics.UI.Gtk.Windows.FileSel (
  FileSelectionClass,
  FileSelection,
  fileSelectionNew,
  fileSelectionSetFilename,
  fileSelectionGetFilename,
  fileSelectionShowFileopButtons,
  fileSelectionHideFileopButtons,
  fileSelectionGetButtons,
  fileSelectionComplete
  ) where

import Monad            (liftM)

import System.Glib.FFI
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
import Graphics.UI.Gtk.General.Structs		(fileSelectionGetButtons)

{#context lib="libgtk" prefix ="gtk"#}


-- operations
-- ----------

-- | Create a new file selection dialog with 
-- the given window title.
--
fileSelectionNew       :: String -> IO FileSelection
fileSelectionNew title  = do
  withUTFString title $ \strPtr -> 
    makeNewObject mkFileSelection $ liftM castPtr $ 
			{#call unsafe file_selection_new#} strPtr

-- | Set the filename for the given file 
-- selection dialog.
--
fileSelectionSetFilename :: FileSelectionClass fsel => fsel -> String -> IO ()
fileSelectionSetFilename fsel str = 
  withUTFString str $ \strPtr -> 
    {#call unsafe file_selection_set_filename#} (toFileSelection fsel) strPtr

-- | Get the filename currently selected by 
-- the given file selection dialog.
--
fileSelectionGetFilename :: FileSelectionClass fsel => fsel -> IO String
fileSelectionGetFilename fsel = 
  do
    strPtr <- {#call unsafe file_selection_get_filename#} 
      (toFileSelection fsel)
    peekUTFString strPtr

-- | Show the file operation buttons 
-- of the given file selection dialog.
--
fileSelectionShowFileopButtons :: FileSelectionClass fsel => fsel -> IO ()
fileSelectionShowFileopButtons  =
  {#call file_selection_show_fileop_buttons#} . toFileSelection

-- | Hide the file operation buttons 
-- of the given file selection dialog.
--
fileSelectionHideFileopButtons :: FileSelectionClass fsel => fsel -> IO ()
fileSelectionHideFileopButtons  =
  {#call file_selection_hide_fileop_buttons#} . toFileSelection

-- currently broken
-- -- query the widgets of the file selectors buttons
-- --
-- -- * this is useful to attach signals handlers to these buttons
-- --
-- -- * the buttons are OK & Cancel (in this order)
-- --
-- fileSelectionQueryButtons :: FileSelectionClass fsel 
-- 			  => fsel
-- 			  -> IO (Button, Button)
-- fileSelectionQueryButtons fsel =
--   withForeignPtr (unFileSelection $ toFileSelection fsel) $ \ ptr -> do
--       ok     <- {#get FileSelection.ok_button    #} ptr
--       cancel <- {#get FileSelection.cancel_button#} ptr
--       return (castToButton ok, castToButton cancel)

-- | Only show files matching pattern.
--
fileSelectionComplete :: FileSelectionClass fsel => fsel -> String -> IO ()
fileSelectionComplete fsel pattern =
  withUTFString pattern $ \patternPtr ->
    {#call file_selection_complete#} (toFileSelection fsel) patternPtr
