-- -*-haskell-*-
--  GIMP Toolkit (GTK) TreeStore TreeModel
--
--  Author : Axel Simon
--
--  Created: 9 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/12 17:19:26 $
--
--  Copyright (C) 2001-2005 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A tree-like data structure that can be used with the "TreeView"
-- 
module Graphics.UI.Gtk.TreeList.TreeStore (
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
  treeStoreIterDepth,
  treeStoreClear
  ) where

import Monad	(liftM)
import Maybe	(fromMaybe)

import System.Glib.FFI
import System.Glib.GObject			(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.TreeList.TreeModel#}
import Graphics.UI.Gtk.General.Structs		(treeIterSize)
import System.Glib.StoreValue			(TMType(..), GenericValue(..))
{#import System.Glib.GValue#}			(GValue, valueUnset)
import System.Glib.GType			(GType)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Generate a new entity to store tree information.
--
treeStoreNew :: [TMType] -> IO TreeStore
treeStoreNew cols = makeNewGObject mkTreeStore $ 
  withArray0 ((fromIntegral.fromEnum) TMinvalid) 
  (map (fromIntegral.fromEnum) cols) $
  {#call unsafe tree_store_newv#} ((fromIntegral.length) cols)

-- | Set the data of a specific node. The supplied
-- value must match the type that was set for the column.
--
treeStoreSetValue :: (TreeStoreClass ts) => ts -> TreeIter -> Int ->
                     GenericValue -> IO ()
treeStoreSetValue ts ti col val = with val $ \vPtr -> do
  {#call unsafe tree_store_set_value#} (toTreeStore ts) ti 
    (fromIntegral col) vPtr
  valueUnset vPtr

#if GTK_CHECK_VERSION(2,1,0)
-- | Remove a specific node.
--
-- * The 'TreeIter' will point to the entry following the one which
--   was just removed. The function returns @False@ if the
--   @ti@TreeIter does not point to a valid element (i.e. the
--   function just removed the bottom entry from the tree).
--
-- * This function returned @()@ in Gtk version 2.0.X
--
treeStoreRemove :: (TreeStoreClass ts) => ts -> TreeIter -> IO Bool
treeStoreRemove ts ti = liftM toBool $ 
  {#call tree_store_remove#} (toTreeStore ts) ti

#else
-- | Remove a specific node.
--
-- * The 'TreeIter' will point to the entry following the one which
--   was just removed.
--
-- * This function returns @Bool@ in Gtk version
--   2.2.0 and later
--
treeStoreRemove :: (TreeStoreClass ts) => ts -> TreeIter -> IO ()
treeStoreRemove ts ti = {#call tree_store_remove#} (toTreeStore ts) ti
#endif


-- | Insert a child node into the tree. If the parent
-- is Nothing the insert at the root of the tree. The pos parameter determines
-- the position with respect to other siblings. Set this to -1 to insert the
-- node as last node.
--
treeStoreInsert :: (TreeStoreClass ts) => ts -> Maybe TreeIter -> Int ->
                   IO TreeIter
treeStoreInsert ts parent pos = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  {#call tree_store_insert#} (toTreeStore ts) iter 
    (fromMaybe (TreeIter nullForeignPtr) parent) (fromIntegral pos)
  return iter


-- | Insert a node in front of the
-- @sibling@ node on the same level.
--
treeStoreInsertBefore :: (TreeStoreClass ts) => ts -> TreeIter -> IO TreeIter
treeStoreInsertBefore ts sibling = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  {#call tree_store_insert_before#} (toTreeStore ts) iter (TreeIter nullForeignPtr) sibling
  return iter

-- | Insert a node behind the @sibling@
-- node on the same level.
--
treeStoreInsertAfter :: (TreeStoreClass ts) => ts -> TreeIter -> IO TreeIter
treeStoreInsertAfter ts sibling = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  {#call tree_store_insert_after#} (toTreeStore ts) iter (TreeIter nullForeignPtr) sibling
  return iter

-- | Insert a child node in front of every other
-- sibling.
--
-- * This is equivalent to 'treeStoreInsert' @parent 0@ .
--
treeStorePrepend :: (TreeStoreClass ts) => ts -> Maybe TreeIter -> IO TreeIter
treeStorePrepend ts parent = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  {#call tree_store_prepend#} (toTreeStore ts) iter 
    (fromMaybe (TreeIter nullForeignPtr) parent)
  return iter

-- | Insert a child node behind other siblings.
--
-- * This is equivalent to 'treeStoreInsert' @parent (-1)@ .
--
treeStoreAppend :: (TreeStoreClass ts) => ts -> Maybe TreeIter -> IO TreeIter
treeStoreAppend ts parent = do
  iterPtr <- mallocBytes treeIterSize
  iter <- liftM TreeIter $ newForeignPtr iterPtr (foreignFree iterPtr)
  {#call tree_store_append#} (toTreeStore ts) iter 
    (fromMaybe (TreeIter nullForeignPtr) parent)
  return iter

-- | Check if a node is in a parental relationship
-- with another node. Returns True even if parent is grandparent,... of child.
--
treeStoreIsAncestor :: (TreeStoreClass ts) => ts -> TreeIter -> TreeIter ->
                       IO Bool
treeStoreIsAncestor ts parent child = liftM toBool $
  {#call unsafe tree_store_is_ancestor#} (toTreeStore ts) parent child

-- | Calculate the level of a node. Returns 1 for a
-- root node.
--
treeStoreIterDepth :: (TreeStoreClass ts) => ts -> TreeIter -> IO Int
treeStoreIterDepth ts iter = liftM fromIntegral $
  {#call unsafe tree_store_iter_depth#} (toTreeStore ts) iter

-- | Removes all rows from the store.
--
treeStoreClear :: (TreeStoreClass ts) => ts -> IO ()
treeStoreClear ts =
  {#call tree_store_clear#} (toTreeStore ts)
  
