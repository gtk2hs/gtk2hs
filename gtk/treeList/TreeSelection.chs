-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: TreeSelection
--
--  Author : Axel Simon
--          
--  Created: 8 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
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
--- DESCRIPTION ---------------------------------------------------------------
--
--  * A TreeSelection is a data type belonging to a TreeModel. As the name
--    suggests it holds the current selection which can even be a multiple
--    choice.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------
--
-- * treeSelectionGetSelected allows to retreive the associated TreeModel
--   object. We currently do not use this feature so it could be added
--   if needed.
--
module TreeSelection(
  TreeSelection,
  TreeSelectionClass,
  castToTreeSelection,
  SelectionMode(..),
  treeSelectionSetMode,
  TreeSelectionCB,
  treeSelectionSetSelectFunction,
  treeSelectionGetTreeView,
  treeSelectionGetSelected,
  TreeSelectionForeachCB,
  treeSelectionSelectedForeach,
  treeSelectionSelectPath,
  treeSelectionUnselectPath,
  treeSelectionSelectIter,
  treeSelectionUnselectIter,
  treeSelectionSelectAll,
  treeSelectionUnselectAll,
  treeSelectionSelectRange
  ) where

import Monad	(liftM)
import IOExts	(newIORef, readIORef, writeIORef)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums    (SelectionMode(..))
{#import TreeModel#}
import Structs	(treeIterSize, nullForeignPtr)
import General	(mkDestructor)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Set single or multiple choice. (EXPORTED)
--
treeSelectionSetMode :: (TreeSelectionClass ts) => 
  SelectionMode -> ts -> IO ()
treeSelectionSetMode sm ts = {#call tree_selection_set_mode#}
  (toTreeSelection ts) ((fromIntegral.fromEnum) sm)

-- Set a callback function if selection changes. (EXPORTED)
--
treeSelectionSetSelectFunction :: (TreeSelectionClass ts) => 
  TreeSelectionCB -> ts -> IO ()
treeSelectionSetSelectFunction fun ts = do
  fPtr <- mkTreeSelectionFunc (\_ _ tp _ -> do
    tpPtr <- tree_path_copy tp
    path <- liftM TreePath $ newForeignPtr tpPtr (tree_path_free tpPtr)
    fun path
    )
  dRef <- newIORef nullFunPtr
  dPtr <- mkDestructor $ do
    dPtr <- readIORef dRef
    freeHaskellFunPtr dPtr
    freeHaskellFunPtr fPtr
  writeIORef dRef dPtr
  {#call tree_selection_set_select_function#} (toTreeSelection ts) fPtr 
    nullPtr dPtr

-- Callback type for a function that is called everytime the selection
-- changes. This function is set with @treeSelectionSetSelectFunction.
-- (EXPORTED)
type TreeSelectionCB = TreePath -> IO ()
{#pointer TreeSelectionFunc#}

foreign export dynamic mkTreeSelectionFunc ::
  (Ptr () -> Ptr () -> Ptr TreePath -> Ptr () -> IO ())-> IO TreeSelectionFunc

foreign import ccall "gtk_tree_path_free" unsafe
  tree_path_free :: Ptr TreePath -> IO ()

foreign import ccall "gtk_tree_path_copy" unsafe
  tree_path_copy :: Ptr TreePath -> IO (Ptr TreePath)


-- Retrieve the TreeView widget that this TreeSelection works on. (EXPORTED)
--
treeSelectionGetTreeView :: (TreeSelectionClass ts) => ts -> IO TreeView
treeSelectionGetTreeView ts = makeNewObject mkTreeView $
  {#call unsafe tree_selection_get_tree_view#} (toTreeSelection ts)

-- Retrieves the selection of a single choice TreeSelection. (EXPORTED)
--
treeSelectionGetSelected :: (TreeSelectionClass ts) =>
  ts -> IO (Maybe TreeIter)
treeSelectionGetSelected ts = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  res <- {#call tree_selection_get_selected#} (toTreeSelection ts) 
    (nullPtr) iter
  return $ if (toBool res) then Just iter else Nothing

-- Execute a function for each selected node. (EXPORTED)
--
--
treeSelectionSelectedForeach :: (TreeSelectionClass ts) => 
  TreeSelectionForeachCB -> ts -> IO ()
treeSelectionSelectedForeach fun ts = do
  fPtr <- mkTreeSelectionForeachFunc (\_ ti _ -> do
    -- make a deep copy of the iterator. This make it possible to store this
    -- iterator in Haskell land somewhere. The TreeModel parameter is not
    -- passed to the function due to performance reasons. But since it is
    -- a constant member of Selection this does not matter.
    iterPtr <- mallocBytes treeIterSize
    copyBytes iterPtr ti treeIterSize
    iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
    fun iter
    )
  {#call tree_selection_selected_foreach#} (toTreeSelection ts) fPtr nullPtr
  freeHaskellFunPtr fPtr

-- Callback function type for @treeSelectionSelectedForeach. (EXPORTED)
type TreeSelectionForeachCB = TreeIter -> IO ()
{#pointer TreeSelectionForeachFunc#}

foreign export dynamic mkTreeSelectionForeachFunc ::
  (Ptr () -> Ptr TreeIter -> Ptr () -> IO ()) -> IO TreeSelectionForeachFunc


-- Select a specific item by TreePath. (EXPORTED)
--
treeSelectionSelectPath :: (TreeSelectionClass ts) =>
  TreePath -> ts -> IO ()
treeSelectionSelectPath tp ts =
  {#call tree_selection_select_path#} (toTreeSelection ts) tp

-- Deselect a specific item by TreePath. (EXPORTED)
--
treeSelectionUnselectPath :: (TreeSelectionClass ts) =>
  TreePath -> ts -> IO ()
treeSelectionUnselectPath tp ts =
  {#call tree_selection_unselect_path#} (toTreeSelection ts) tp

-- Select a specific item by TreeIter. (EXPORTED)
--
treeSelectionSelectIter :: (TreeSelectionClass ts) =>
  TreeIter -> ts -> IO ()
treeSelectionSelectIter ti ts =
  {#call tree_selection_select_iter#} (toTreeSelection ts) ti

-- Deselect a specific item by TreeIter. (EXPORTED)
--
treeSelectionUnselectIter :: (TreeSelectionClass ts) =>
  TreeIter -> ts -> IO ()
treeSelectionUnselectIter ti ts =
  {#call tree_selection_unselect_iter#} (toTreeSelection ts) ti


-- Select everything. (EXPORTED)
--
treeSelectionSelectAll :: (TreeSelectionClass ts) => ts -> IO ()
treeSelectionSelectAll ts = 
  {#call tree_selection_select_all#} (toTreeSelection ts)

-- Deselect everything. (EXPORTED)
--
treeSelectionUnselectAll :: (TreeSelectionClass ts) => ts -> IO ()
treeSelectionUnselectAll ts = 
  {#call tree_selection_unselect_all#} (toTreeSelection ts)


-- Select a range specified by two TreePaths. (EXPORTED)
--
treeSelectionSelectRange :: (TreeSelectionClass ts) => 
  TreePath -> TreePath -> ts -> IO ()
treeSelectionSelectRange start end ts =
  {#call tree_selection_select_range#} (toTreeSelection ts) start end








