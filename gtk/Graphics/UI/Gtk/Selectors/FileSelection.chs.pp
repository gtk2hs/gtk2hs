-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget FileSelection
--
--  Author : Manuel M T Chakravarty
--
--  Created: 20 January 1999
--
--  Version $Revision: 1.1 $ from $Date: 2005/02/26 02:17:27 $
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
-- TODO
--
-- Fix fileSelectionQueryButtons
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Prompt the user for a file or directory name.
--
-- * As of Gtk+ 2.4 this module has been deprecated in favour of 'FileChooser'
--
module Graphics.UI.Gtk.Selectors.FileSelection (
-- * Description
-- 
-- | 'FileSelection' should be used to retrieve file or directory names from
-- the user. It will create a new dialog window containing a directory list,
-- and a file list corresponding to the current working directory. The
-- filesystem can be navigated using the directory list or the drop-down
-- history menu. Alternatively, the TAB key can be used to navigate using
-- filename completion - common in text based editors such as emacs and jed.
--
-- File selection dialogs are created with a call to 'fileSelectionNew'.
--
-- The default filename can be set using 'fileSelectionSetFilename' and the
-- selected filename retrieved using 'fileSelectionGetFilename'.
--
-- Use 'fileSelectionComplete' to display files and directories that match a
-- given pattern. This can be used for example, to show only *.txt files, or
-- only files beginning with gtk*.
--
-- Simple file operations; create directory, delete file, and rename file,
-- are available from buttons at the top of the dialog. These can be hidden
-- using 'fileSelectionHideFileopButtons' and shown again using
-- 'fileSelectionShowFileopButtons'.
--

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
-- |                                       +----FileSelection
-- @

-- * Types
  FileSelection,
  FileSelectionClass,
  castToFileSelection,

-- * Constructors
  fileSelectionNew,

-- * Methods
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

{# context lib="libgtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new file selection dialog with 
-- the given window title.
--
fileSelectionNew ::
    String
 -> IO FileSelection
fileSelectionNew title =
  makeNewObject mkFileSelection $ liftM castPtr $
  withUTFString title $ \titlePtr ->
  {# call unsafe file_selection_new #}
    titlePtr

--------------------
-- Methods

-- | Set the filename for the given file 
-- selection dialog.
--
fileSelectionSetFilename :: FileSelectionClass fsel => fsel -> String -> IO ()
fileSelectionSetFilename fsel str = 
  withUTFString str $ \strPtr -> 
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
    {#call unsafe file_selection_set_filename_utf8#}
#else
    {#call unsafe file_selection_set_filename#}
#endif
      (toFileSelection fsel) strPtr

-- | Get the filename currently selected by 
-- the given file selection dialog.
--
fileSelectionGetFilename :: FileSelectionClass fsel => fsel -> IO String
fileSelectionGetFilename fsel = 
  do
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0)
    strPtr <- {#call unsafe file_selection_get_filename_utf8#} 
#else
    strPtr <- {#call unsafe file_selection_get_filename#} 
#endif
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
