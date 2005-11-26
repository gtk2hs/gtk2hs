-- -*-haskell-*-
--  GIMP Toolkit (GTK) TreeStore TreeModel
--
--  Author : Axel Simon
--
--  Created: 9 May 2001
--
--  Version $Revision: 1.9 $ from $Date: 2005/11/26 16:00:22 $
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
-- A tree-like data structure that can be used with the 'TreeView'
--
module Graphics.UI.Gtk.TreeList.TreeStore (
-- * Detail
-- 
-- | The 'TreeStore' object is a list model for use with a 'TreeView' widget.
-- It implements the 'TreeModel' interface, and consequentialy, can use all of
-- the methods available there. It also implements the 'TreeSortable' interface
-- so it can be sorted by the view. Finally, it also implements the tree drag
-- and drop interfaces.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----TreeStore
-- @

-- * Types
  TreeStore,
  TreeStoreClass,
  castToTreeStore,
  toTreeStore,
  TMType(..),
  GenericValue(..),

-- * Constructors
  treeStoreNew,

-- * Methods
  treeStoreSetValue,
  treeStoreRemove,
  treeStoreInsert,
  treeStoreInsertBefore,
  treeStoreInsertAfter,
  treeStorePrepend,
  treeStoreAppend,
  treeStoreIsAncestor,
  treeStoreIterDepth,
  treeStoreClear,
  ) where

import Monad	(liftM)
import Maybe	(fromMaybe)

import System.Glib.FFI
import System.Glib.GObject			(constructNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.TreeList.TreeModel#}
{#import Graphics.UI.Gtk.TreeList.TreeIter#}
import System.Glib.StoreValue			(TMType(..), GenericValue(..)
						,valueSetGenericValue)
{#import System.Glib.GValue#}			(GValue(GValue), allocaGValue)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Generate a new entity to store tree information.
--
treeStoreNew :: [TMType] -> IO TreeStore
treeStoreNew types =
  constructNewGObject mkTreeStore $
  withArray (map (fromIntegral . fromEnum) types) $ \typesArr ->
  {# call unsafe tree_store_newv #}
    ((fromIntegral . length) types)
    typesArr

--------------------
-- Methods

-- | Set the data of a specific node. The supplied
-- value must match the type that was set for the column.
--
treeStoreSetValue :: TreeStoreClass self => self
 -> TreeIter
 -> Int
 -> GenericValue
 -> IO ()
treeStoreSetValue self iter column value =
  allocaGValue $ \gvalue -> do
  valueSetGenericValue gvalue value
  {# call unsafe tree_store_set_value #}
    (toTreeStore self)
    iter
    (fromIntegral column)
    gvalue

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
treeStoreRemove :: TreeStoreClass self => self
 -> TreeIter
 -> IO Bool
treeStoreRemove self iter =
  liftM toBool $
  {# call tree_store_remove #}
    (toTreeStore self)
    iter

#else
-- | Remove a specific node.
--
-- * The 'TreeIter' will point to the entry following the one which
--   was just removed.
--
-- * This function returns @Bool@ in Gtk version
--   2.2.0 and later
--
treeStoreRemove :: TreeStoreClass self => self
 -> TreeIter
 -> IO ()
treeStoreRemove self iter =
  {# call tree_store_remove #}
    (toTreeStore self)
    iter
#endif

-- | Insert a child node into the tree. If the parent
-- is Nothing the insert at the root of the tree. The pos parameter determines
-- the position with respect to other siblings. Set this to -1 to insert the
-- node as last node.
--
treeStoreInsert :: TreeStoreClass self => self
 -> Maybe TreeIter -- ^ @parent@ - A valid 'TreeIter', or @Nothing@
 -> Int            -- ^ @position@ - position to insert the new row
 -> IO TreeIter
treeStoreInsert self parent position = do
  iter <- mallocTreeIter
  {# call tree_store_insert #}
    (toTreeStore self)
    iter
    (fromMaybe (TreeIter nullForeignPtr) parent)
    (fromIntegral position)
  return iter

-- | Insert a node in front of the @sibling@ node on the same level.
--
treeStoreInsertBefore :: TreeStoreClass self => self
 -> TreeIter
 -> IO TreeIter
treeStoreInsertBefore self sibling = do
  iter <- mallocTreeIter
  {# call tree_store_insert_before #}
    (toTreeStore self)
    iter
    (TreeIter nullForeignPtr)
    sibling
  return iter

-- | Insert a node behind the @sibling@ node on the same level.
--
treeStoreInsertAfter :: TreeStoreClass self => self
 -> TreeIter
 -> IO TreeIter
treeStoreInsertAfter self sibling = do
  iter <- mallocTreeIter
  {# call tree_store_insert_after #}
    (toTreeStore self)
    iter
    (TreeIter nullForeignPtr)
    sibling
  return iter

-- | Insert a child node in front of every other sibling.
--
-- * This is equivalent to 'treeStoreInsert' @parent 0@ .
--
treeStorePrepend :: TreeStoreClass self => self
 -> Maybe TreeIter -- ^ @parent@ - A valid 'TreeIter', or @Nothing@
 -> IO TreeIter
treeStorePrepend self parent = do
  iter <- mallocTreeIter
  {# call tree_store_prepend #}
    (toTreeStore self)
    iter
    (fromMaybe (TreeIter nullForeignPtr) parent)
  return iter

-- | Insert a child node behind other siblings.
--
-- * This is equivalent to 'treeStoreInsert' @parent (-1)@ .
--
treeStoreAppend :: TreeStoreClass self => self
 -> Maybe TreeIter -- ^ @parent@ - A valid 'TreeIter', or @Nothing@
 -> IO TreeIter
treeStoreAppend self parent = do
  iter <- mallocTreeIter
  {# call tree_store_append #}
    (toTreeStore self)
    iter
    (fromMaybe (TreeIter nullForeignPtr) parent)
  return iter

-- | Check if a node is in a parental relationship
-- with another node. Returns True even if parent is grandparent,... of child.
--
treeStoreIsAncestor :: TreeStoreClass self => self
 -> TreeIter
 -> TreeIter
 -> IO Bool
treeStoreIsAncestor self iter descendant =
  liftM toBool $
  {# call unsafe tree_store_is_ancestor #}
    (toTreeStore self)
    iter
    descendant

-- | Calculate the level of a node. Returns 1 for a root node.
--
treeStoreIterDepth :: TreeStoreClass self => self
 -> TreeIter
 -> IO Int
treeStoreIterDepth self iter =
  liftM fromIntegral $
  {# call unsafe tree_store_iter_depth #}
    (toTreeStore self)
    iter

-- | Removes all rows from the store.
--
treeStoreClear :: TreeStoreClass self => self -> IO ()
treeStoreClear self =
  {# call tree_store_clear #}
    (toTreeStore self)
