{-# OPTIONS -cpp #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) TreeModel
--
--  Author : Axel Simon
--          
--  Created: 8 May 2001
--
--  Version $Revision: 1.12 $ from $Date: 2004/05/23 16:16:43 $
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
-- A 'TreeModel' is the abstract base class for 
-- 'TreeStore' and 'ListStore'.
--
-- * Most functions are defined in the latter two classes. This module
--   provides the 'TreeIter' and 'TreePath' objects.
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
  tree_path_copy,			-- internal
  tree_path_free,			-- internal
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
import FFI
{#import Hierarchy#}
{#import Signal#}
import Structs	                (treeIterSize)
import StoreValue		(TMType)
{#import GValue#}		(GValue, GenericValue, valueUnset)

{# context lib="gtk" prefix="gtk" #}

-- | Tree Iterator : A pointer to an entry in a
-- 'TreeStore' or 'ListStore'.
--
{#pointer * TreeIter foreign newtype#}

-- | TreePath : a list of indices to specify a subtree or node
-- in the hierarchical 'TreeStore' database.
--
{#pointer * TreePath foreign newtype#}

-- methods

-- | Read the number of columns this
-- 'TreeModel' currently stores.
--
treeModelGetNColumns :: TreeModelClass tm => tm -> IO Int
treeModelGetNColumns tm = liftM fromIntegral $ 
  {#call unsafe tree_model_get_n_columns#} (toTreeModel tm)

-- | Retrieves the type of a specific column.
--
treeModelGetColumnType :: TreeModelClass tm => tm -> Int -> IO TMType
treeModelGetColumnType tm col = liftM (toEnum.fromIntegral) $
  {#call unsafe tree_model_get_column_type#} (toTreeModel tm) 
  (fromIntegral col)

-- | Read the value of at a specific column and
-- 'Iterator'.
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

#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "&gtk_tree_path_free"
  tree_path_free' :: FinalizerPtr TreePath

tree_path_free :: Ptr TreePath -> FinalizerPtr TreePath
tree_path_free _ = tree_path_free'

#elif __GLASGOW_HASKELL__>=504

foreign import ccall unsafe "gtk_tree_path_free"
  tree_path_free :: Ptr TreePath -> IO ()

#else

foreign import ccall "gtk_tree_path_free" unsafe
  tree_path_free :: Ptr TreePath -> IO ()

#endif



-- | Create a new 'TreePath'.
--
-- * A 'TreePath' is an hierarchical index. It is independent of
--   a specific 'TreeModel'.
-- 
treePathNew :: IO TreePath
treePathNew = do
  tpPtr <- {#call unsafe tree_path_new#} 
  liftM TreePath $ newForeignPtr tpPtr (tree_path_free tpPtr)

-- | Turn a @String@ into a
-- 'TreePath'.
--
treePathNewFromString :: String -> IO TreePath
treePathNewFromString path = do
  tpPtr <- throwIfNull "treePathNewFromString: invalid path given" $
    withUTFString path {#call unsafe tree_path_new_from_string#}
  liftM TreePath $ newForeignPtr tpPtr (tree_path_free tpPtr)

-- | Turn a 'TreePath' into a 
-- @String@.
--
treePathToString :: TreePath -> IO String
treePathToString tp = do
  strPtr <- {#call tree_path_to_string#} tp
  str <- peekUTFString strPtr
  {#call unsafe g_free#} (castPtr strPtr)
  return str

-- | Create a 'TreePath'.
--
-- * The returned 'TreePath' is an index to the first element.
--
treePathNewFirst :: IO TreePath
treePathNewFirst = do
  tpPtr <- {#call unsafe tree_path_new_first#}
  liftM TreePath $ newForeignPtr tpPtr (tree_path_free tpPtr)

-- | Add an index on the next level.
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

#if __GLASGOW_HASKELL__>=504

foreign import ccall unsafe "gtk_tree_path_copy"
  tree_path_copy :: Ptr TreePath -> IO (Ptr TreePath)

#else

foreign import ccall "gtk_tree_path_copy" unsafe
  tree_path_copy :: Ptr TreePath -> IO (Ptr TreePath)

#endif


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


#if __GLASGOW_HASKELL__>=504

foreign import ccall unsafe "gtk_tree_iter_copy"
  tree_iter_copy :: Ptr TreeIter -> IO (Ptr TreeIter)

#else

foreign import ccall "gtk_tree_iter_copy" unsafe
  tree_iter_copy :: Ptr TreeIter -> IO (Ptr TreeIter)

#endif

#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "&gtk_tree_iter_free"
  tree_iter_free' :: FinalizerPtr TreeIter

tree_iter_free :: Ptr TreeIter -> FinalizerPtr TreeIter
tree_iter_free _ = tree_iter_free'

#elif __GLASGOW_HASKELL__>=504

foreign import ccall unsafe "gtk_tree_iter_free"
  tree_iter_free :: Ptr TreeIter -> IO ()

#else

foreign import ccall "gtk_tree_iter_free" unsafe
  tree_iter_free :: Ptr TreeIter -> IO ()

#endif


-- | Turn a 'TreePath' into a
-- 'TreeIter'.
--
-- * Returns @Nothing@ if the @tp@ is invalid.
--
treeModelGetIter :: TreeModelClass tm => tm -> TreePath -> IO (Maybe TreeIter)
treeModelGetIter tm tp = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  res <- {#call unsafe tree_model_get_iter#} (toTreeModel tm) iter tp
  return $ if (toBool res) then Just iter else Nothing
  
-- | Turn a @String@ into a
-- 'TreeIter'.
--
-- * Returns @Nothing@ if the table is empty.
--
treeModelGetIterFromString :: TreeModelClass tm => tm -> String -> 
						   IO (Maybe TreeIter)
treeModelGetIterFromString tm str = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  res <- withUTFString str $ \strPtr ->
    {#call unsafe tree_model_get_iter_from_string#} (toTreeModel tm) iter 
      strPtr
  return $ if (toBool res) then Just iter else Nothing

-- | Retrieves an 'TreeIter' to the
-- first entry.
--
-- * Returns @Nothing@ if the table is empty.
--
treeModelGetIterFirst :: TreeModelClass tm => tm -> IO (Maybe TreeIter)
treeModelGetIterFirst tm = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  res <- {#call unsafe tree_model_get_iter_first#} (toTreeModel tm) iter
  return $ if (toBool res) then Just iter else Nothing

treeModelGetPath :: TreeModelClass tm => tm -> TreeIter -> IO TreePath
treeModelGetPath tm iter =  do
  tpPtr <- throwIfNull "treeModelGetPath: illegal iterator" $
    {#call unsafe tree_model_get_path#} (toTreeModel tm) iter
  liftM TreePath $ newForeignPtr tpPtr (tree_path_free tpPtr)

-- | Advance the iterator to the next element.
--
-- * If there is no other element on this hierarchy level, return 
--   @False@.
--
treeModelIterNext :: TreeModelClass tm => tm -> TreeIter -> IO Bool
treeModelIterNext tm iter = liftM toBool $
 {#call unsafe tree_model_iter_next#} (toTreeModel tm) iter

-- | Retrieve an iterator to the first child.
--
treeModelIterChildren :: TreeModelClass tm => tm -> TreeIter -> 
					      IO (Maybe TreeIter)
treeModelIterChildren tm parent = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  res <- {#call unsafe tree_model_iter_children#} (toTreeModel tm) iter 
    parent
  return $ if (toBool res) then Just iter else Nothing

-- | Test if this is the last hierarchy level.
treeModelIterHasChild :: TreeModelClass tm => tm -> TreeIter -> IO Bool
treeModelIterHasChild tm iter = liftM toBool $
  {#call unsafe tree_model_iter_has_child#} (toTreeModel tm) iter

-- | Return the number of children.
--
-- * If @Nothing@ is specified for the @tm@ argument, the
--   function will work on toplevel elements.
--
treeModelIterNChildren :: TreeModelClass tm => tm -> Maybe TreeIter -> IO Int
treeModelIterNChildren tm iter = liftM fromIntegral $
  {#call unsafe tree_model_iter_n_children#} (toTreeModel tm) 
    (fromMaybe (TreeIter nullForeignPtr) iter)

-- | Retrieve the @n@th child.
--
-- * If @Nothing@ is specified for the @tm@ argument, the
--   function will work on toplevel elements.
--
treeModelIterNthChild :: TreeModelClass tm => 
  tm -> Maybe TreeIter -> Int -> IO (Maybe TreeIter)
treeModelIterNthChild tm parent n = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  res <- {#call unsafe tree_model_iter_nth_child#} (toTreeModel tm) iter 
    (fromMaybe (TreeIter nullForeignPtr) parent) (fromIntegral n)
  return $ if (toBool res) then Just iter else Nothing

-- | Retrieve the parent of this iterator.
--
treeModelIterParent :: TreeModelClass tm => tm -> 
  TreeIter -> IO (Maybe TreeIter)
treeModelIterParent tm child = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  res <- {#call unsafe tree_model_iter_parent#} (toTreeModel tm) iter child
  return $ if (toBool res) then Just iter else Nothing

-- | No clue.
--
treeModelRefNode :: TreeModelClass tm => tm -> TreeIter -> IO ()
treeModelRefNode tm iter = 
  {#call unsafe tree_model_ref_node#} (toTreeModel tm) iter

-- | No clue either.
--
treeModelUnrefNode :: TreeModelClass tm => tm -> TreeIter -> IO ()
treeModelUnrefNode tm iter = 
  {#call unsafe tree_model_unref_node#} (toTreeModel tm) iter

