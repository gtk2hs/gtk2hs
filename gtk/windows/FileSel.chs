--  -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: @entry Widget FileSel@
--
--  Author : Manuel M T Chakravarty
--  Created: 20 January 1999
--
--  Version $Revision: 1.3 $ from $Date: 2002/08/02 06:10:03 $
--
--  Copyright (c) [1999..2002] Manuel M T Chakravarty
--  Copyright (c) 2002 Jens Petersen
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  The file selection widget is a quick and simple way to display a File
--  dialog box.  It comes complete with Ok & Cancel buttons; optionally, it
--  can have file operation buttons.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------
--
--  Fix fileSelectionQueryButtons
--
module FileSel(
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
import Foreign
import UTFCForeign
{#import Hierarchy#}
import Object		(makeNewObject)
import Structs		(fileSelectionGetButtons)

{#context lib="libgtk" prefix ="gtk"#}


-- operations
-- ----------

-- create a new file selection dialog with the given window title (EXPORTED)
--
fileSelectionNew       :: String -> IO FileSelection
fileSelectionNew title  = do
  withCString title $ \strPtr -> 
    makeNewObject mkFileSelection $ liftM castPtr $ 
			{#call unsafe file_selection_new#} strPtr

-- set the filename for the given file selection dialog (EXPORTED)
--
fileSelectionSetFilename :: FileSelectionClass fsel 
			 => fsel 
			 -> String 
			 -> IO ()
fileSelectionSetFilename fsel str = 
  withCString str $ \strPtr -> 
    {#call unsafe file_selection_set_filename#} (toFileSelection fsel) strPtr

-- get the filename currently selected by the given file selection dialog
-- (EXPORTED) 
--
fileSelectionGetFilename :: FileSelectionClass fsel 
			 => fsel 
			 -> IO String
fileSelectionGetFilename fsel = 
  do
    strPtr <- {#call unsafe file_selection_get_filename#} (toFileSelection fsel)
    peekCString strPtr

-- show the file operation buttons of the given file selection dialog
-- (EXPORTED)  
--
fileSelectionShowFileopButtons :: FileSelectionClass fsel 
			       => fsel 
			       -> IO ()
fileSelectionShowFileopButtons  =
  {#call file_selection_show_fileop_buttons#} . toFileSelection

-- hide the file operation buttons of the given file selection dialog
-- (EXPORTED)  
--
fileSelectionHideFileopButtons :: FileSelectionClass fsel 
			       => fsel
			       -> IO ()
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

-- Only show files matching pattern
--
fileSelectionComplete :: FileSelectionClass fsel
                      => fsel -> String
                      -> IO ()
fileSelectionComplete fsel pattern =
  withCString pattern $ \patternPtr ->
    {#call file_selection_complete#} (toFileSelection fsel) patternPtr
