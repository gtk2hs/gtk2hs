-- -*-haskell-*-
--  GIMP Toolkit (GTK) CellRendererText TreeView
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:36:43 $
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
-- A 'CellRenderer' which displays a single-line text.
--
-- * This widget derives from 'CellRenderer'. It provides the 
--   possibility to display some text by setting the 'Attribute' 
--   'cellText' to the column of a 'TreeModel' by means of 
--   'treeViewAddAttribute' from 'TreeModelColumn'.
--

module Graphics.UI.Gtk.TreeList.CellRendererText (
  CellRendererText,
  CellRendererTextClass,
  castToCellRendererText,
  cellRendererTextNew,
  cellText,
  cellMarkup,
  cellBackground,
  cellForeground,
  cellEditable,
  onEdited,
  afterEdited
  ) where

import Maybe	(fromMaybe)
import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.TreeList.TreeModel#}
import Graphics.UI.Gtk.General.Structs		(treeIterSize)
import Graphics.UI.Gtk.TreeList.CellRenderer	(Attribute(..))
import System.Glib.StoreValue			(GenericValue(..), TMType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new CellRendererText object.
--
cellRendererTextNew :: IO CellRendererText
cellRendererTextNew  = makeNewObject mkCellRendererText $ liftM castPtr $
  {#call unsafe cell_renderer_text_new#}

-- helper function
--
strAttr :: [String] -> Attribute CellRendererText String
strAttr str = Attribute str [TMstring]
	        (return . (\x -> [x]) . GVstring . Just)
		(\[GVstring str] -> return (fromMaybe "" str))

mStrAttr :: [String] -> Attribute CellRendererText (Maybe String)
mStrAttr str = Attribute str [TMstring]
	        (return . (\x -> [x]) . GVstring)
		(\[GVstring str] -> return str)

-- | Define the attribute that specifies the text to be
-- rendered.
--
cellText :: Attribute CellRendererText String
cellText  = strAttr ["text"]

-- | Define a markup string instead of a text.
--
cellMarkup :: Attribute CellRendererText String
cellMarkup  = strAttr ["markup"]

-- | A named color for the background paint.
--
cellBackground :: Attribute CellRendererText (Maybe String)
cellBackground  = mStrAttr ["background"]

-- | A named color for the foreground paint.
--
cellForeground :: Attribute CellRendererText (Maybe String)
cellForeground  = mStrAttr ["foreground"]

-- | Determines wether the content can be altered.
--
-- * If this flag is set, the user can alter the cell.
--
cellEditable :: Attribute CellRendererText (Maybe Bool)
cellEditable = Attribute ["editable","editable-set"] [TMboolean,TMboolean]
	         (\mb -> return $ case mb of
		   (Just bool) -> [GVboolean bool, GVboolean True]
		   Nothing     -> [GVboolean True, GVboolean True])
		 (\[GVboolean e, GVboolean s] -> return $
		   if s then Just e else Nothing)

-- | Emitted when the user finished editing a cell.
--
-- * This signal is not emitted when editing is disabled (see 
--   'cellEditable') or when the user aborts editing.
--
onEdited, afterEdited :: TreeModelClass tm => CellRendererText -> tm ->
			 (TreeIter -> String -> IO ()) ->
			 IO (ConnectId CellRendererText)
onEdited cr tm user = connect_PTR_STRING__NONE "edited" False cr $
  \strPtr string -> do
    iterPtr <- mallocBytes treeIterSize
    iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
    res <- liftM toBool $ withForeignPtr ((unTreeModel . toTreeModel) tm) $
      \tmPtr -> withForeignPtr (unTreeIter iter) $ \iterPtr ->
	gtk_tree_model_get_iter_from_string tmPtr iterPtr strPtr
    if res then user iter string else
      putStrLn "edited signal: invalid tree path"

afterEdited cr tm user = connect_PTR_STRING__NONE "edited" True cr $
  \strPtr string -> do
    iterPtr <- mallocBytes treeIterSize
    iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
    res <- liftM toBool $ withForeignPtr ((unTreeModel . toTreeModel) tm) $
      \tmPtr -> withForeignPtr (unTreeIter iter) $ \iterPtr ->
	gtk_tree_model_get_iter_from_string tmPtr iterPtr strPtr
    if res then user iter string else
      putStrLn "edited signal: invalid tree path"

unTreeIter (TreeIter iter) = iter
