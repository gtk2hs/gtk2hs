-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry ListStore (a TreeModel)@
--
--  Author : Axel Simon
--          
--  Created: 9 May 2001
--
--  Version $Revision: 1.5 $ from $Date: 2002/07/21 16:59:05 $
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
-- @description@ --------------------------------------------------------------
--
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

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
  listStoreAppend,
  listStoreClear
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
import StoreValue (TMType(..), GenericValue(..))
{#import GValue#} (GValue, valueUnset)
import GType	  (GType)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor listStoreNew@ Generate a new entity to store tree information.
--
listStoreNew :: [TMType] -> IO ListStore
listStoreNew cols = makeNewGObject mkListStore $ 
  withArray0 ((fromIntegral.fromEnum) TMinvalid) 
  (map (fromIntegral.fromEnum) cols) $
  {#call unsafe list_store_newv#} ((fromIntegral.length) cols)

-- @method listStoreSetValue@ Set the data of a specific node. The supplied
-- value must match the type that was set for the column.
--
listStoreSetValue :: (ListStoreClass ts) => ts -> TreeIter -> Int ->
                     GenericValue -> IO ()
listStoreSetValue ts ti col val = with' val $ \vPtr -> do
  {#call unsafe list_store_set_value#} (toListStore ts) ti 
    (fromIntegral col) vPtr
  valueUnset vPtr

-- @method listStoreRemove@ Remove a specific node.
--
listStoreRemove :: (ListStoreClass ts) => ts -> TreeIter -> IO ()
listStoreRemove ts ti = {#call list_store_remove#} (toListStore ts) ti

-- @method listStoreInsert@ Insert a new row into the list. The pos parameter
-- determines the row number where the row should be inserted. Set this to -1
-- to insert at the end of the list.
--
listStoreInsert :: (ListStoreClass ts) => ts -> Int -> IO TreeIter
listStoreInsert ts pos = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call list_store_insert#} (toListStore ts) iter (fromIntegral pos)
  return iter


-- @method listStoreInsertBefore@ Insert a row in front of the
-- @ref arg sibling@ node.
--
listStoreInsertBefore :: (ListStoreClass ts) => ts -> TreeIter -> IO TreeIter
listStoreInsertBefore ts sibling = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call list_store_insert_before#} (toListStore ts) iter sibling
  return iter

-- @method listStoreInsertAfter@ Insert a row behind the @ref arg sibling@
-- row.
--
listStoreInsertAfter :: (ListStoreClass ts) => ts -> TreeIter -> IO TreeIter
listStoreInsertAfter ts sibling = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call list_store_insert_after#} (toListStore ts) iter sibling
  return iter

-- @method listStorePrepend@ Insert a row in front of every other row.
--
-- * This is equivalent to @ref method listStoreInsert@ 0.
--
listStorePrepend :: (ListStoreClass ts) => ts -> IO TreeIter
listStorePrepend ts = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call list_store_prepend#} (toListStore ts) iter 
  return iter

-- @method listStoreAppend@ Insert a row at the end of the table .
--
-- * This is equivalent to @ref method listStoreInsert@ (-1).
--
listStoreAppend :: (ListStoreClass ts) => ts -> IO TreeIter
listStoreAppend ts = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (free iterPtr)
  {#call list_store_append#} (toListStore ts) iter 
  return iter

-- @method listStoreClear@ Clear all rows in this table.
--
listStoreClear :: (ListStoreClass ts) => ts -> IO ()
listStoreClear = {#call list_store_clear#}.toListStore
