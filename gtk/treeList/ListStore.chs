-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: ListStore (a TreeModel)
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

module ListStore(
  ListStore,
  TMType(..),
  GenericValue(..),
  listStoreNew,
  listStoreSetValue,
  listStoreRemove,
  listStoreInsert,
  listStoreInsertBefore,
  listStoreInsertAfter,
  listStorePrepend,
  listStoreAppend
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
listStoreNew :: [TMType] -> IO ListStore
listStoreNew cols = makeNewGObject mkListStore $ 
  withArray0 tmTypeInvalid (map (fromIntegral.fromEnum) cols) $ \tPtr ->
  {#call unsafe list_store_newv#} ((fromIntegral.length) cols) tPtr

-- Set the data of a specific node. The supplied value must match the
-- type that was set for the column. (EXPORTED)
--
listStoreSetValue :: (ListStoreClass ts) => 
  TreeIter -> Int -> GenericValue -> ts -> IO ()
listStoreSetValue ti col val ts = withObject val $
  {#call unsafe list_store_set_value#} (toListStore ts) ti (fromIntegral col)

-- Remove a specific node. (EXPORTED)
--
listStoreRemove :: (ListStoreClass ts) => TreeIter -> ts -> IO ()
listStoreRemove ti ts = {#call list_store_remove#} (toListStore ts) ti

-- Insert a new row into the list. The pos parameter determines the row
-- number where the row should be inserted. Set this to -1 to insert at the
-- end of the list. (EXPORTED)
--
listStoreInsert :: (ListStoreClass ts) => Int -> ts -> IO TreeIter
listStoreInsert pos ts = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call list_store_insert#} (toListStore ts) iter (fromIntegral pos)
  return iter


-- Insert a row in front of the @sibling node. (EXPORTED)
--
listStoreInsertBefore :: (ListStoreClass ts) => TreeIter -> ts -> IO TreeIter
listStoreInsertBefore sibling ts = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call list_store_insert_before#} (toListStore ts) iter sibling
  return iter

-- Insert a row behind the @sibling row. (EXPORTED)
--
listStoreInsertAfter :: (ListStoreClass ts) => TreeIter -> ts -> IO TreeIter
listStoreInsertAfter sibling ts = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call list_store_insert_after#} (toListStore ts) iter sibling
  return iter

-- Insert a row in front of every other row. (EXPORTED)
--
-- * This is equivalent to @listStoreInsert 0.
--
listStorePrepend:: (ListStoreClass ts) => ts -> IO TreeIter
listStorePrepend ts = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call list_store_prepend#} (toListStore ts) iter 
  return iter

-- Insert a row at the end of the table . (EXPORTED)
--
-- * This is equivalent to @listStoreInsert (-1).
--
listStoreAppend:: (ListStoreClass ts) => ts -> IO TreeIter
listStoreAppend ts = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call list_store_append#} (toListStore ts) iter 
  return iter

