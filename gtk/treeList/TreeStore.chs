-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: TreeStore (a TreeModel)
--
--  Author : Axel Simon
--          
--  Created: 9 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
--
--  Copyright (c) 2001 Axel Simon
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
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module TreeStore(
  TreeStore,
  TMType(..),
  GenericValue(..),
  treeStoreNew,
  treeStoreSetValue,
  treeStoreRemove,
  treeStoreInsert,
  treeStoreInsertBefore,
  treeStoreInsertAfter,
  treeStorePrepend,
  treeStoreAppend,
  treeStoreIsAncestor,
  treeStoreIterDepth
  ) where

import Monad	(liftM)
import Maybe	(fromMaybe)
import Foreign
import UTFCForeign
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import Signal#}
{#import TreeModel#}
import Structs	(treeIterSize, nullForeignPtr)
import StoreValue (TMType(..), GenericValue(..), tmTypeInvalid)
{#import GValue#} (GValue)
import GType	  (GType)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Generate a new entity to store tree information. (EXPORTED)
--
treeStoreNew :: [TMType] -> IO TreeStore
treeStoreNew cols = makeNewGObject mkTreeStore $ 
  withArray0 tmTypeInvalid (map (fromIntegral.fromEnum) cols) $ \tPtr ->
  {#call unsafe tree_store_newv#} ((fromIntegral.length) cols) tPtr

-- Set the data of a specific node. The supplied value must match the
-- type that was set for the column. (EXPORTED)
--
treeStoreSetValue :: (TreeStoreClass ts) => 
  TreeIter -> Int -> GenericValue -> ts -> IO ()
treeStoreSetValue ti col val ts = withObject val $
  {#call unsafe tree_store_set_value#} (toTreeStore ts) ti (fromIntegral col)

-- Remove a specific node. (EXPORTED)
--
treeStoreRemove :: (TreeStoreClass ts) => TreeIter -> ts -> IO ()
treeStoreRemove ti ts = {#call tree_store_remove#} (toTreeStore ts) ti

-- Insert a child node into the tree. If the parent is Nothing the insert at
-- the root of the tree. The pos parameter determines the position with
-- respect to other siblings. Set this to -1 to insert the node as last
-- node. (EXPORTED)
--
treeStoreInsert :: (TreeStoreClass ts) => 
  Maybe TreeIter -> Int -> ts -> IO TreeIter
treeStoreInsert parent pos ts = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call tree_store_insert#} (toTreeStore ts) iter 
    (fromMaybe (TreeIter nullForeignPtr) parent) (fromIntegral pos)
  return iter


-- Insert a node in front of the @sibling node on the same level. (EXPORTED)
--
treeStoreInsertBefore :: (TreeStoreClass ts) => TreeIter -> ts -> IO TreeIter
treeStoreInsertBefore sibling ts = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call tree_store_insert_before#} (toTreeStore ts) iter (TreeIter nullForeignPtr) sibling
  return iter

-- Insert a node behind the @sibling node on the same level. (EXPORTED)
--
treeStoreInsertAfter :: (TreeStoreClass ts) => TreeIter -> ts -> IO TreeIter
treeStoreInsertAfter sibling ts = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call tree_store_insert_after#} (toTreeStore ts) iter (TreeIter nullForeignPtr) sibling
  return iter

-- Insert a child node in front of every other sibling. (EXPORTED)
-- This is equivalent to @treeStoreInsert parent 0.
--
treeStorePrepend:: (TreeStoreClass ts) => Maybe TreeIter -> ts -> IO TreeIter
treeStorePrepend parent ts = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call tree_store_prepend#} (toTreeStore ts) iter 
    (fromMaybe (TreeIter nullForeignPtr) parent)
  return iter

-- Insert a child node behind other siblings. (EXPORTED)
-- This is equivalent to @treeStoreInsert parent (-1).
--
treeStoreAppend:: (TreeStoreClass ts) => Maybe TreeIter -> ts -> IO TreeIter
treeStoreAppend parent ts = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call tree_store_append#} (toTreeStore ts) iter 
    (fromMaybe (TreeIter nullForeignPtr) parent)
  return iter

-- Check if a node is in a parental relationship with another node. Returns
-- True even if parent is grandparent,... of child. (EXPORTED)
--
treeStoreIsAncestor:: (TreeStoreClass ts) => 
  TreeIter -> TreeIter -> ts -> IO Bool
treeStoreIsAncestor parent child ts = liftM toBool $
  {#call unsafe tree_store_is_ancestor#} (toTreeStore ts) parent child

-- Calculate the level of a node. Returns 1 for a root node. (EXPORTED)
--
treeStoreIterDepth :: (TreeStoreClass ts) => TreeIter -> ts -> IO Int
treeStoreIterDepth iter ts = liftM fromIntegral $
  {#call unsafe tree_store_iter_depth#} (toTreeStore ts) iter

