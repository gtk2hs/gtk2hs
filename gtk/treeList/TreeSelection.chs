{-# OPTIONS -cpp #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) TreeSelection
--
--  Author : Axel Simon
--          
--  Created: 8 May 2001
--
--  Version $Revision: 1.12 $ from $Date: 2004/12/09 18:26:02 $
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
-- A 'TreeSelection' is a data type belonging to a 'TreeModel'. As the name
-- suggests it holds the current selection which can even be a multiple
-- choice.
--
-- TODO
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
  treeSelectionGetMode,
  TreeSelectionCB,
  treeSelectionSetSelectFunction,
  treeSelectionGetTreeView,
  treeSelectionGetSelected,
  TreeSelectionForeachCB,
  treeSelectionSelectedForeach,
  treeSelectionSelectPath,
  treeSelectionUnselectPath,
  treeSelectionPathIsSelected,
  treeSelectionSelectIter,
  treeSelectionUnselectIter,
  treeSelectionIterIsSelected,
  treeSelectionSelectAll,
  treeSelectionUnselectAll,
  treeSelectionSelectRange,
  onSelectionChanged,
  afterSelectionChanged
  ) where

import Monad	(liftM)
import Data.IORef (newIORef, readIORef, writeIORef)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums    (SelectionMode(..))
{#import TreeModel#}
import Structs	(treeIterSize)
import General	(mkDestructor)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Set single or multiple choice.
--
treeSelectionSetMode :: (TreeSelectionClass ts) => ts -> SelectionMode -> IO ()
treeSelectionSetMode ts sm = {#call tree_selection_set_mode#}
  (toTreeSelection ts) ((fromIntegral.fromEnum) sm)

-- | Gets the selection mode.
--
treeSelectionGetMode :: (TreeSelectionClass ts) => ts -> IO SelectionMode
treeSelectionGetMode ts = liftM (toEnum.fromIntegral) $
  {#call unsafe tree_selection_get_mode#} (toTreeSelection ts)

-- | Set a callback function if
-- selection changes.
--
treeSelectionSetSelectFunction :: (TreeSelectionClass ts) => ts ->
                                  TreeSelectionCB -> IO ()
treeSelectionSetSelectFunction ts fun = do
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

-- | Callback type for a function that is called
-- everytime the selection changes. This function is set with
-- 'treeSelectionSetSelectFunction'.
--
type TreeSelectionCB = TreePath -> IO ()
{#pointer TreeSelectionFunc#}

foreign import ccall "wrapper"  mkTreeSelectionFunc ::
  (Ptr () -> Ptr () -> Ptr TreePath -> Ptr () -> IO ())-> IO TreeSelectionFunc

-- | Retrieve the TreeView widget that this
-- TreeSelection works on.
--
treeSelectionGetTreeView :: (TreeSelectionClass ts) => ts -> IO TreeView
treeSelectionGetTreeView ts = makeNewObject mkTreeView $
  {#call unsafe tree_selection_get_tree_view#} (toTreeSelection ts)

-- | Retrieves the selection of a single
-- choice TreeSelection.
--
treeSelectionGetSelected :: (TreeSelectionClass ts) => ts ->
                            IO (Maybe TreeIter)
treeSelectionGetSelected ts = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  res <- {#call tree_selection_get_selected#} (toTreeSelection ts) 
    (nullPtr) iter
  return $ if (toBool res) then Just iter else Nothing

-- | Execute a function for each selected
-- node.
--
treeSelectionSelectedForeach :: (TreeSelectionClass ts) => ts ->
                                TreeSelectionForeachCB -> IO ()
treeSelectionSelectedForeach ts fun = do
  fPtr <- mkTreeSelectionForeachFunc (\_ ti _ -> do
    -- make a deep copy of the iterator. This makes it possible to store this
    -- iterator in Haskell land somewhere. The TreeModel parameter is not
    -- passed to the function due to performance reasons. But since it is
    -- a constant member of Selection this does not matter.
    iterPtr <- mallocBytes treeIterSize
    copyBytes iterPtr ti treeIterSize
    iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
    fun iter
    )
  {#call tree_selection_selected_foreach#} (toTreeSelection ts) fPtr nullPtr
  freeHaskellFunPtr fPtr

-- | Callback function type for 'treeSelectionSelectedForeach'.
--
type TreeSelectionForeachCB = TreeIter -> IO ()
{#pointer TreeSelectionForeachFunc#}

foreign import ccall "wrapper"  mkTreeSelectionForeachFunc ::
  (Ptr () -> Ptr TreeIter -> Ptr () -> IO ()) -> IO TreeSelectionForeachFunc

-- | Select a specific item by TreePath.
--
treeSelectionSelectPath :: (TreeSelectionClass ts) => ts -> TreePath -> IO ()
treeSelectionSelectPath ts tp =
  {#call tree_selection_select_path#} (toTreeSelection ts) tp

-- | Deselect a specific item by TreePath.
--
treeSelectionUnselectPath :: (TreeSelectionClass ts) => ts -> TreePath -> IO ()
treeSelectionUnselectPath ts tp =
  {#call tree_selection_unselect_path#} (toTreeSelection ts) tp

-- | Returns True if the row at the given path is currently selected.
--
treeSelectionPathIsSelected :: (TreeSelectionClass ts) => ts -> TreePath -> IO Bool
treeSelectionPathIsSelected ts tp = liftM toBool $
  {#call unsafe tree_selection_path_is_selected#} (toTreeSelection ts) tp

-- | Select a specific item by TreeIter.
--
treeSelectionSelectIter :: (TreeSelectionClass ts) => ts -> TreeIter -> IO ()
treeSelectionSelectIter ts ti =
  {#call tree_selection_select_iter#} (toTreeSelection ts) ti

-- | Deselect a specific item by TreeIter.
--
treeSelectionUnselectIter :: (TreeSelectionClass ts) => ts -> TreeIter -> IO ()
treeSelectionUnselectIter ts ti =
  {#call tree_selection_unselect_iter#} (toTreeSelection ts) ti

-- | Returns True if the row at the given iter is currently selected.
--
treeSelectionIterIsSelected :: (TreeSelectionClass ts) => ts -> TreeIter -> IO Bool
treeSelectionIterIsSelected ts ti = liftM toBool $
  {#call unsafe tree_selection_iter_is_selected#} (toTreeSelection ts) ti

-- | Select everything.
--
treeSelectionSelectAll :: (TreeSelectionClass ts) => ts -> IO ()
treeSelectionSelectAll ts = 
  {#call tree_selection_select_all#} (toTreeSelection ts)

-- | Deselect everything.
--
treeSelectionUnselectAll :: (TreeSelectionClass ts) => ts -> IO ()
treeSelectionUnselectAll ts = 
  {#call tree_selection_unselect_all#} (toTreeSelection ts)


-- | Select a range specified by two
-- TreePaths.
--
treeSelectionSelectRange :: (TreeSelectionClass ts) => ts -> TreePath ->
                            TreePath -> IO ()
treeSelectionSelectRange ts start end =
  {#call tree_selection_select_range#} (toTreeSelection ts) start end


-- | Emitted each time the user changes the selection.
--
onSelectionChanged, afterSelectionChanged :: TreeSelectionClass ts => ts -> (IO ()) ->
			   IO (ConnectId ts)
onSelectionChanged = connect_NONE__NONE "changed" False
afterSelectionChanged = connect_NONE__NONE "changed" True






