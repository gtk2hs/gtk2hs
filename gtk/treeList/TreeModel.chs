-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry TreeModel@
--
--  Author : Axel Simon
--          
--  Created: 8 May 2001
--
--  Version $Revision: 1.9 $ from $Date: 2003/03/08 17:44:05 $
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
-- * A @ref data TreeModel@ is the abstract base class for 
--   @ref data TreeStore@ and @ref data ListStore@.
--
--- @documentation@ -----------------------------------------------------------
--
-- * Most functions are defined in the latter two classes. This module
--   provides the @ref data TreeIter@ and @ref data TreePath@ objects.
--
--- @todo@ --------------------------------------------------------------------
--
--
module TreeModel(
  TreeModel,
  TreeModelClass,
  castToTreeModel,
  treeModelGetNColumns,
  treeModelGetColumnType,
  treeModelGetValue,
  TreePath(..),
  createTreePath,			-- internal
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
  createTreeIter,			-- internal
  treeModelGetIter,
  treeModelGetIterFromString,
  gtk_tree_model_get_iter_from_string,	-- internal
  treeModelGetIterFirst,
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
{#import Signal#}
import Structs	                (treeIterSize, nullForeignPtr)
import StoreValue		(TMType)
{#import GValue#}		(GValue, GenericValue, valueUnset)

{# context lib="gtk" prefix="gtk" #}

-- @data TreeIter@ Tree Iterator : A pointer to an entry in a
-- @ref data TreeStore@ or @ref data ListStore@.
--
{#pointer * TreeIter foreign newtype#}

-- @data TreePath@ TreePath : a list of indices to specify a subtree or node
-- in the hierarchical @ref data TreeStore@ database.
--
{#pointer * TreePath foreign newtype#}

-- methods

-- @method treeModelGetNColumns@ Read the number of columns this
-- @ref data TreeModel@ currently stores.
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
-- @ref data Iterator@.
--
treeModelGetValue :: TreeModelClass tm => tm -> TreeIter -> Int ->
                     IO GenericValue
treeModelGetValue tm iter col = alloca $ \vaPtr -> do
  -- don't know if this is necessary, see treeList/StoreValue.hsc
  poke (castPtr vaPtr) (0:: {#type GType#})
  {#call unsafe tree_model_get_value#} (toTreeModel tm) iter 
    (fromIntegral col) vaPtr
  val <- peek vaPtr
  valueUnset vaPtr
  return val

-- utilities related to tree models

-- Create a TreePath from a pointer.
createTreePath :: Ptr TreePath -> IO TreePath
createTreePath tpPtr = do
  tpPtr' <- tree_path_copy tpPtr
  liftM TreePath $ newForeignPtr tpPtr' (tree_path_free tpPtr')

foreign import ccall "gtk_tree_path_copy" unsafe
  tree_path_copy :: Ptr TreePath -> IO (Ptr TreePath)

-- @constructor treePathNew@ Create a new @ref data TreePath@.
--
-- * A @ref data TreePath@ is an hierarchical index. It is independent of
--   a specific @ref data TreeModel@.
-- 
treePathNew :: IO TreePath
treePathNew = do
  tpPtr <- {#call unsafe tree_path_new#} 
  liftM TreePath $ newForeignPtr tpPtr (tree_path_free tpPtr)

foreign import ccall "gtk_tree_path_free" unsafe
  tree_path_free :: Ptr TreePath -> IO ()

-- @constructor treePathNewFromString@ Turn a @literal String@ into a
-- @ref data TreePath@.
--
treePathNewFromString :: String -> IO TreePath
treePathNewFromString path = do
  tpPtr <- throwIfNull "treePathNewFromString: invalid path given" $
    withCString path {#call unsafe tree_path_new_from_string#}
  liftM TreePath $ newForeignPtr tpPtr (tree_path_free tpPtr)

-- @method treePathToString@ Turn a @ref data TreePath@ into a 
-- @literal String@.
--
treePathToString :: TreePath -> IO String
treePathToString tp = do
  strPtr <- {#call tree_path_to_string#} tp
  str <- peekCString strPtr
  {#call unsafe g_free#} (castPtr strPtr)
  return str

-- @method treePathNewFirst@ Create a @ref data TreePath@.
--
-- * The returned @ref data TreePath@ is an index to the first element.
--
treePathNewFirst :: IO TreePath
treePathNewFirst = do
  tpPtr <- {#call unsafe tree_path_new_first#}
  liftM TreePath $ newForeignPtr tpPtr (tree_path_free tpPtr)

-- @method treePathAppendIndex@ Add an index on the next level.
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


createTreeIter :: Ptr TreeIter -> IO TreeIter
createTreeIter tiPtr = do
  tiPtr' <- tree_iter_copy tiPtr
  liftM TreeIter $ newForeignPtr tiPtr' (tree_iter_free tiPtr')

foreign import ccall "gtk_tree_iter_free" unsafe
  tree_iter_free :: Ptr TreeIter -> IO ()

foreign import ccall "gtk_tree_iter_copy" unsafe
  tree_iter_copy :: Ptr TreeIter -> IO (Ptr TreeIter)


-- @method treeModelGetIter@ Turn a @ref data TreePath@ into a
-- @ref data TreeIter@.
--
-- * Returns @literal Nothing@ if the @ref arg tp@ is invalid.
--
treeModelGetIter :: TreeModelClass tm => tm -> TreePath -> IO (Maybe TreeIter)
treeModelGetIter tm tp = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  res <- {#call unsafe tree_model_get_iter#} (toTreeModel tm) iter tp
  return $ if (toBool res) then Just iter else Nothing
  
-- @method treeModelGetIterFromString@ Turn a @literal String@ into a
-- @ref data TreeIter@.
--
-- * Returns @literal Nothing@ if the table is empty.
--
treeModelGetIterFromString :: TreeModelClass tm => tm -> String -> 
						   IO (Maybe TreeIter)
treeModelGetIterFromString tm str = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  res <- withCString str $ \strPtr ->
    {#call unsafe tree_model_get_iter_from_string#} (toTreeModel tm) iter 
      strPtr
  return $ if (toBool res) then Just iter else Nothing

-- @method treeModelGetIterFirst@ Retrieves an @ref data TreeIter@ to the
-- first entry.
--
-- * Returns @literal Nothing@ if the table is empty.
--
treeModelGetIterFirst :: TreeModelClass tm => tm -> IO (Maybe TreeIter)
treeModelGetIterFirst tm = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  res <- {#call unsafe tree_model_get_iter_first#} (toTreeModel tm) iter
  return $ if (toBool res) then Just iter else Nothing

treeModelGetPath :: TreeModelClass tm => tm -> TreeIter -> IO TreePath
treeModelGetPath tm iter =  do
  tpPtr <- throwIfNull "treeModelGetPath: illegal iterator" $
    {#call unsafe tree_model_get_path#} (toTreeModel tm) iter
  liftM TreePath $ newForeignPtr tpPtr (tree_path_free tpPtr)

-- @method treeModelIterNext@ Advance the iterator to the next element.
--
-- * If there is no other element on this hierarchy level, return 
--   @literal False@.
--
treeModelIterNext :: TreeModelClass tm => tm -> TreeIter -> IO Bool
treeModelIterNext tm iter = liftM toBool $
 {#call unsafe tree_model_iter_next#} (toTreeModel tm) iter

-- @method treeModelIterChildren@ Retrieve an iterator to the first child.
--
treeModelIterChildren :: TreeModelClass tm => tm -> TreeIter -> 
					      IO (Maybe TreeIter)
treeModelIterChildren tm parent = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  res <- {#call unsafe tree_model_iter_children#} (toTreeModel tm) iter 
    parent
  return $ if (toBool res) then Just iter else Nothing

-- @method treeModeliterHasChild@ Test if this is the last hierarchy level.
treeModelIterHasChild :: TreeModelClass tm => tm -> TreeIter -> IO Bool
treeModelIterHasChild tm iter = liftM toBool $
  {#call unsafe tree_model_iter_has_child#} (toTreeModel tm) iter

-- @method treeModelIterNChildren@ Return the number of children.
--
-- * If @literal Nothing@ is specified for the @ref arg tm@ argument, the
--   function will work on toplevel elements.
--
treeModelIterNChildren :: TreeModelClass tm => tm -> Maybe TreeIter -> IO Int
treeModelIterNChildren tm iter = liftM fromIntegral $
  {#call unsafe tree_model_iter_n_children#} (toTreeModel tm) 
    (fromMaybe (TreeIter nullForeignPtr) iter)

-- @method treeModelIterNthChild@ Retrieve the @ref arg n@th child.
--
-- * If @literal Nothing@ is specified for the @ref arg tm@ argument, the
--   function will work on toplevel elements.
--
treeModelIterNthChild :: TreeModelClass tm => 
  tm -> Maybe TreeIter -> Int -> IO (Maybe TreeIter)
treeModelIterNthChild tm parent n = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  res <- {#call unsafe tree_model_iter_nth_child#} (toTreeModel tm) iter 
    (fromMaybe (TreeIter nullForeignPtr) parent) (fromIntegral n)
  return $ if (toBool res) then Just iter else Nothing

-- @method treeModelIterParent@ Retrieve the parent of this iterator.
--
treeModelIterParent :: TreeModelClass tm => tm -> 
  TreeIter -> IO (Maybe TreeIter)
treeModelIterParent tm child = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  res <- {#call unsafe tree_model_iter_parent#} (toTreeModel tm) iter child
  return $ if (toBool res) then Just iter else Nothing

-- @method treeModelRefNode@ No clue.
--
treeModelRefNode :: TreeModelClass tm => tm -> TreeIter -> IO ()
treeModelRefNode tm iter = 
  {#call unsafe tree_model_ref_node#} (toTreeModel tm) iter

-- @method treeModelUnrefNode@ No clue either.
--
treeModelUnrefNode :: TreeModelClass tm => tm -> TreeIter -> IO ()
treeModelUnrefNode tm iter = 
  {#call unsafe tree_model_unref_node#} (toTreeModel tm) iter

