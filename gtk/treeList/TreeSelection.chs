-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry TreeSelection@
--
--  Author : Axel Simon
--          
--  Created: 8 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2002/07/21 16:07:17 $
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
-- @description@ --------------------------------------------------------------
--
--  * A TreeSelection is a data type belonging to a TreeModel. As the name
--    suggests it holds the current selection which can even be a multiple
--    choice.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
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
import LocalData(newIORef, readIORef, writeIORef)
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

-- @method treeSelectionSetMode@ Set single or multiple choice.
--
treeSelectionSetMode :: (TreeSelectionClass ts) => ts -> SelectionMode -> IO ()
treeSelectionSetMode ts sm = {#call tree_selection_set_mode#}
  (toTreeSelection ts) ((fromIntegral.fromEnum) sm)

-- @method treeSelectionSetSelectFunction@ Set a callback function if
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

-- @type TreeSelectionCB@ Callback type for a function that is called
-- everytime the selection changes. This function is set with
-- @ref method treeSelectionSetSelectFunction@.
--
type TreeSelectionCB = TreePath -> IO ()
{#pointer TreeSelectionFunc#}

foreign export dynamic mkTreeSelectionFunc ::
  (Ptr () -> Ptr () -> Ptr TreePath -> Ptr () -> IO ())-> IO TreeSelectionFunc

foreign import ccall "gtk_tree_path_free" unsafe
  tree_path_free :: Ptr TreePath -> IO ()

foreign import ccall "gtk_tree_path_copy" unsafe
  tree_path_copy :: Ptr TreePath -> IO (Ptr TreePath)


-- @method treeSelectionGetTreeView@ Retrieve the TreeView widget that this
-- TreeSelection works on.
--
treeSelectionGetTreeView :: (TreeSelectionClass ts) => ts -> IO TreeView
treeSelectionGetTreeView ts = makeNewObject mkTreeView $
  {#call unsafe tree_selection_get_tree_view#} (toTreeSelection ts)

-- @method treeSelectionGetSelected@ Retrieves the selection of a single
-- choice TreeSelection.
--
treeSelectionGetSelected :: (TreeSelectionClass ts) => ts ->
                            IO (Maybe TreeIter)
treeSelectionGetSelected ts = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  res <- {#call tree_selection_get_selected#} (toTreeSelection ts) 
    (nullPtr) iter
  return $ if (toBool res) then Just iter else Nothing

-- @method treeSelectionSelectedForeach@ Execute a function for each selected
-- node.
--
treeSelectionSelectedForeach :: (TreeSelectionClass ts) => ts ->
                                TreeSelectionForeachCB -> IO ()
treeSelectionSelectedForeach ts fun = do
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

-- @type TreeSelectionForeachCB@ Callback function type for
-- @ref method treeSelectionSelectedForeach@.
--
type TreeSelectionForeachCB = TreeIter -> IO ()
{#pointer TreeSelectionForeachFunc#}

foreign export dynamic mkTreeSelectionForeachFunc ::
  (Ptr () -> Ptr TreeIter -> Ptr () -> IO ()) -> IO TreeSelectionForeachFunc


-- @method treeSelectionSelectPath@ Select a specific item by TreePath.
--
treeSelectionSelectPath :: (TreeSelectionClass ts) => ts -> TreePath -> IO ()
treeSelectionSelectPath ts tp =
  {#call tree_selection_select_path#} (toTreeSelection ts) tp

-- @method treeSelectionUnselectPath@ Deselect a specific item by TreePath.
--
treeSelectionUnselectPath :: (TreeSelectionClass ts) => ts -> TreePath -> IO ()
treeSelectionUnselectPath ts tp =
  {#call tree_selection_unselect_path#} (toTreeSelection ts) tp

-- @method treeSelectionSelectIter@ Select a specific item by TreeIter.
--
treeSelectionSelectIter :: (TreeSelectionClass ts) => ts -> TreeIter -> IO ()
treeSelectionSelectIter ts ti =
  {#call tree_selection_select_iter#} (toTreeSelection ts) ti

-- @method treeSelectionUnselectIter@ Deselect a specific item by TreeIter.
--
treeSelectionUnselectIter :: (TreeSelectionClass ts) => ts -> TreeIter -> IO ()
treeSelectionUnselectIter ts ti =
  {#call tree_selection_unselect_iter#} (toTreeSelection ts) ti


-- @method treeSelectionSelectAll@ Select everything.
--
treeSelectionSelectAll :: (TreeSelectionClass ts) => ts -> IO ()
treeSelectionSelectAll ts = 
  {#call tree_selection_select_all#} (toTreeSelection ts)

-- @method treeSelectionUnselectAll@ Deselect everything.
--
treeSelectionUnselectAll :: (TreeSelectionClass ts) => ts -> IO ()
treeSelectionUnselectAll ts = 
  {#call tree_selection_unselect_all#} (toTreeSelection ts)


-- @method treeSelectionSelectRange@ Select a range specified by two
-- TreePaths.
--
treeSelectionSelectRange :: (TreeSelectionClass ts) => ts -> TreePath ->
                            TreePath -> IO ()
treeSelectionSelectRange ts start end =
  {#call tree_selection_select_range#} (toTreeSelection ts) start end








