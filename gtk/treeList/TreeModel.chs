-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry TreeModel@
--
--  Author : Axel Simon
--          
--  Created: 8 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2002/07/08 13:22:46 $
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
-- * A @TreeModel is the abstract base class for @TreeStore and @ListStore.
--   Most functions are defined in the latter two classes. This module
--   provides the @TreeIter and @TreePath objects.
--
--- DOCU ----------------------------------------------------------------------
--
--- TODO ----------------------------------------------------------------------
module TreeModel(
  TreeModel,
  TreeModelClass,
  castToTreeModel,
  treeModelGetNColumns,
  treeModelGetColumnType,
  treeModelGetValue,
  TreePath(..),
  treePathNew,
  treePathNewFromString,
  treePathToString,
  treePathNewFirst,
  treePathAppendIndex,
  treePathPrependIndex,
  treePathGetDepth,
  treePathGetIndices,
  treePathCopy,
  treePathCompare,
  treePathNext,
  treePathPrev,
  treePathUp,
  treePathDown,
  TreeIter(..),
  treeModelGetIter,
  treeModelGetPath,
  treeModelIterNext,
  treeModelIterChildren,
  treeModelIterHasChild,
  treeModelIterNChildren,
  treeModelIterNthChild,
  treeModelIterParent,
  treeModelRefNode,
  treeModelUnrefNode
  ) where

import Monad	(liftM, when)
import Maybe	(fromMaybe)
import Foreign
import UTFCForeign
{#import Hierarchy#}
import Signal	    
import Structs	                (treeIterSize, nullForeignPtr)
import StoreValue		(TMType)
{#import GValue#}		(GValue, GenericValue)

{# context lib="gtk" prefix="gtk" #}

-- @type TreeIter@ Tree Iterator : A pointer to an entry in a
-- @ref type TreeStore@ or @ref arg ListStore@.
--
{#pointer * TreeIter foreign newtype#}

-- @type TreePath@ TreePath : a list of indices to specify a subtree or node
-- in the hierarchical @ref type TreeStore@ database.
--
{#pointer * TreePath foreign newtype#}

-- methods

-- @method treeModelGetNColumns@ Read the number of columns this
-- @ref type TreeModel@ currently stores.
--
treeModelGetNColumns :: TreeModelClass tm => tm -> IO Int
treeModelGetNColumns tm = liftM fromIntegral $ 
  {#call unsafe tree_model_get_n_columns#} (toTreeModel tm)

-- @method treeModelGetColumnType@ Retrieves the type of a specific column.
--
treeModelGetColumnType :: TreeModelClass tm => tm -> Int -> IO TMType
treeModelGetColumnType tm col = liftM (toEnum.fromIntegral) $
  {#call unsafe tree_model_get_column_type#} (toTreeModel tm) 
  (fromIntegral col)

-- @method treeModelGetValue@ Read the value of at a specific column and
-- @ref arg Iterator@.
--
treeModelGetValue :: TreeModelClass tm => tm -> TreeIter -> Int ->
                     IO GenericValue
treeModelGetValue tm iter col = alloca $ \vaPtr -> do
  {#call unsafe tree_model_get_value#} (toTreeModel tm) iter 
    (fromIntegral col) vaPtr
  peek vaPtr

-- utilities related to tree models

treePathNew :: IO TreePath
treePathNew = do
  tpPtr <- {#call unsafe tree_path_new#}
  liftM TreePath $ newForeignPtr tpPtr (tree_path_free tpPtr)

foreign import ccall "gtk_tree_path_free" unsafe
  tree_path_free :: Ptr TreePath -> IO ()

treePathNewFromString :: String -> IO TreePath
treePathNewFromString path = do
  tpPtr <- withCString path {#call unsafe tree_path_new_from_string#}
  liftM TreePath $ newForeignPtr tpPtr (tree_path_free tpPtr)

treePathToString :: TreePath -> IO String
treePathToString tp = do
  strPtr <- {#call tree_path_to_string#} tp
  str <- peekCString strPtr
  {#call unsafe g_free#} (castPtr strPtr)
  return str

treePathNewFirst :: IO TreePath
treePathNewFirst = do
  tpPtr <- {#call unsafe tree_path_new_first#}
  liftM TreePath $ newForeignPtr tpPtr (tree_path_free tpPtr)

treePathAppendIndex :: TreePath -> Int -> IO ()
treePathAppendIndex tp ind = 
  {#call unsafe tree_path_append_index#} tp (fromIntegral ind)

treePathPrependIndex :: TreePath -> Int -> IO ()
treePathPrependIndex tp ind =
  {#call unsafe tree_path_prepend_index#} tp (fromIntegral ind)

treePathGetDepth :: TreePath -> IO Int
treePathGetDepth tp = liftM fromIntegral $ 
  {#call unsafe tree_path_get_depth#} tp

treePathGetIndices :: TreePath -> IO [Int]
treePathGetIndices tp = do
  depth <- treePathGetDepth tp
  arrayPtr <- {#call unsafe tree_path_get_indices#} tp
  if (depth==0 || arrayPtr==nullPtr) then return [] else
    sequence [liftM fromIntegral $ peekElemOff arrayPtr e 
	     | e <- [0..depth-1]]

treePathCopy :: TreePath -> IO TreePath
treePathCopy tp = do
  tpPtr' <- {#call unsafe tree_path_copy#} tp
  liftM TreePath $ newForeignPtr tpPtr' (tree_path_free tpPtr')

treePathCompare :: TreePath -> TreePath -> IO Ordering
treePathCompare tp1 tp2 = do
  res <- {#call unsafe tree_path_compare#} tp1 tp2
  return $ case res of
    (-1)   -> LT
    0	   -> EQ
    1	   -> GT

treePathNext :: TreePath -> IO ()
treePathNext = {#call unsafe tree_path_next#}

treePathPrev :: TreePath -> IO Bool
treePathPrev tp = liftM toBool $ {#call unsafe tree_path_prev#} tp

treePathUp :: TreePath -> IO Bool
treePathUp tp = liftM toBool $ {#call unsafe tree_path_up#} tp

treePathDown :: TreePath -> IO ()
treePathDown = {#call unsafe tree_path_down#}

treeModelGetIter :: TreeModelClass tm => tm -> TreePath -> IO (Maybe TreeIter)
treeModelGetIter tm tp = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  res <- {#call unsafe tree_model_get_iter#} (toTreeModel tm) iter tp
  return $ if (toBool res) then Just iter else Nothing
  
treeModelGetPath :: TreeModelClass tm => tm -> TreeIter -> IO TreePath
treeModelGetPath tm iter =  do
  tpPtr <- throwIfNull "treeModelGetPath: illegal iterator" $
    {#call unsafe tree_model_get_path#} (toTreeModel tm) iter
  liftM TreePath $ newForeignPtr tpPtr (tree_path_free tpPtr)

treeModelIterNext :: TreeModelClass tm => TreeIter -> tm -> IO Bool
treeModelIterNext iter tm = liftM toBool $
 {#call unsafe tree_model_iter_next#} (toTreeModel tm) iter


treeModelIterChildren :: TreeModelClass tm => tm -> TreeIter -> 
					      IO (Maybe TreeIter)
treeModelIterChildren tm parent = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  res <- {#call unsafe tree_model_iter_children#} (toTreeModel tm) iter 
    parent
  return $ if (toBool res) then Just iter else Nothing


treeModelIterHasChild :: TreeModelClass tm => tm -> TreeIter -> IO Bool
treeModelIterHasChild tm iter = liftM toBool $
  {#call unsafe tree_model_iter_has_child#} (toTreeModel tm) iter

-- The following functions take a (Maybe TreeIter) for the child parameter.
-- If Nothing is specified for this argument, the function will work on
-- the toplevel instead of a child.

treeModelIterNChildren :: TreeModelClass tm => tm -> Maybe TreeIter -> IO Int
treeModelIterNChildren tm iter = liftM fromIntegral $
  {#call unsafe tree_model_iter_n_children#} (toTreeModel tm) 
    (fromMaybe (TreeIter nullForeignPtr) iter)

treeModelIterNthChild :: TreeModelClass tm => 
  tm -> Maybe TreeIter -> Int -> IO (Maybe TreeIter)
treeModelIterNthChild tm parent n = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  res <- {#call unsafe tree_model_iter_nth_child#} (toTreeModel tm) iter 
    (fromMaybe (TreeIter nullForeignPtr) parent) (fromIntegral n)
  return $ if (toBool res) then Just iter else Nothing

treeModelIterParent :: TreeModelClass tm => tm -> 
  TreeIter -> IO (Maybe TreeIter)
treeModelIterParent tm child = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  res <- {#call unsafe tree_model_iter_parent#} (toTreeModel tm) iter child
  return $ if (toBool res) then Just iter else Nothing

treeModelRefNode :: TreeModelClass tm => tm -> TreeIter -> IO ()
treeModelRefNode tm iter = 
  {#call unsafe tree_model_ref_node#} (toTreeModel tm) iter

treeModelUnrefNode :: TreeModelClass tm => tm -> TreeIter -> IO ()
treeModelUnrefNode tm iter = 
  {#call unsafe tree_model_unref_node#} (toTreeModel tm) iter

