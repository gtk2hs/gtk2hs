{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Interface TreeSortable
--
--  Author : Axel Simon
--
--  Created: 8 Mar 2007
--
--  Copyright (C) 1999-2007 Duncan Coutts, Axel Simon
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
-- Note: there is a constant called GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID
-- which is only used in the C implementation of list store and tree store.
-- The TreeModelSort proxy only uses the default column constant. Hence, we do
-- not expose or tell the user about the UNSORTED constant since it can only
-- be confusing.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- The interface for sortable models used by 'TreeView'
--
module Graphics.UI.Gtk.ModelView.TreeSortable (

-- * Detail
--
-- | 'TreeSortable' is an interface to be implemented by tree models which
-- support sorting. The 'TreeView' uses the methods provided by this interface
-- to sort the model. As of now, only the
-- 'Graphics.UI.Gtk.ModelView.TreeModelSort.TreeModelSort' proxy supports the
-- sortable interface. Thus, in order to enable sortable columns in a
-- 'TreeView', it is necessary to wrap a
-- 'Graphics.UI.Gtk.ModelView.ListStore.ListStore' or
-- 'Graphics.UI.Gtk.ModelView.TreeStore.TreeStore' model in a
-- 'Graphics.UI.Gtk.ModelView.TreeModelSort.TreeModelSort'.
--
-- A 'Graphics.UI.Gtk.ModelView.TreeViewColumn' can be sorted by the user
-- though clicking into the column's header. The rows in the view will then be
-- sorted by the sorting function set for that column. Specifically, a set of
-- sorting functions must be set using the interface provided in this module.
-- Each sorting function is associated with a 'SortColumnId', which is some
-- positive number. A tree view column is then associated with the sorting
-- function by passing the 'SortColumnId' to
-- 'Graphics.UI.Gtk.ModelView.TreeViewColumn.treeViewColumnSetSortColumnId'.
-- There exists one special 'SortColumnId', namely
-- 'treeSortableDefaultSortColumnId' for which a default sorting function can
-- be set. If no such function is set, the order of the rows is the order in
-- which they are stored in the model.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GInterface'
-- |   +----TreeSortable
-- @

-- * Types
  TreeSortable,
  TreeSortableClass,
  castToTreeSortable, gTypeTreeSortable,
  toTreeSortable,
  SortColumnId,

-- * Constants
  treeSortableDefaultSortColumnId,

-- * Methods
  treeSortableGetSortColumnId,
  treeSortableSetSortColumnId,
  treeSortableSetSortFunc,
  treeSortableSetDefaultSortFunc,
  treeSortableHasDefaultSortFunc,
  treeSortableSortColumnChanged,

-- * Signals
  sortColumnChanged
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.General.Enums#}        (SortType(..))
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.ModelView.Types#}
import Graphics.UI.Gtk.General.Structs  (SortColumnId,
                                         treeSortableDefaultSortColumnId )

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- %hash c:a53 d:e0d2
-- | Query the sort column id that is currently in use. The return value may
--   be the special constant 'treeSortableDefaultSortColumnId' in which case
--   the returned Boolean flag is @False@.
--
treeSortableGetSortColumnId :: TreeSortableClass self => self
 -> IO (SortType, Bool, SortColumnId)   -- ^ @(type, columnSet, sortColumnId)@
                     -- returns @True@ in @columnSet@ if @sortColumnId@ is not
                     -- 'treeSortableDefaultSortColumnId'. The @type@ value indicates increasing
                     -- or decreasing ordering.
treeSortableGetSortColumnId self =
  alloca $ \orderPtr -> alloca $ \sortColumnIdPtr -> do
  columnSet <- liftM toBool $ {# call unsafe tree_sortable_get_sort_column_id #}
    (toTreeSortable self)
    sortColumnIdPtr
    orderPtr
  order <- peek orderPtr
  sortColumnId <- peek sortColumnIdPtr
  return (toEnum (fromIntegral order), columnSet, fromIntegral sortColumnId)

-- %hash c:8951 d:33ab
-- | Sets the current sort column to be @sortColumnId@. The @sortable@ will
-- resort itself to reflect this change, after emitting a 'sortColumnChanged'
-- signal. If @sortColumnId@ is 'treeSortableDefaultSortColumnId', then the
-- default sort function will be used, if it is set. Note that this function
-- is mainly used by the view and that the user program should simply set the
-- 'SortColumnId' of the 'TreeViewColumn's.
--
treeSortableSetSortColumnId :: TreeSortableClass self => self
 -> SortColumnId      -- ^ @sortColumnId@ - the sort column id to set
 -> SortType -- ^ @order@ - The sort order of the column
 -> IO ()
treeSortableSetSortColumnId self sortColumnId order =
  {# call tree_sortable_set_sort_column_id #}
    (toTreeSortable self)
    (fromIntegral sortColumnId)
    ((fromIntegral . fromEnum) order)


-- %hash c:9048 d:c49d
-- | Sets the comparison function used when sorting to be @sortFunc@. If the
-- current sort column id of @self@ is the same as @sortColumnId@, then the
-- model will sort using this function.
--
treeSortableSetSortFunc :: TreeSortableClass self => self
 -> SortColumnId                -- ^ @sortColumnId@ - the sort column id to set
                                -- the function for
 -> (TreeIter -> TreeIter -> IO Ordering)
                                -- ^ @sortFunc@ - The comparison function
 -> IO ()
treeSortableSetSortFunc self sortColumnId sortFunc = do
  fPtr <- mkTreeIterCompareFunc (\_ iter1Ptr iter2Ptr _ -> do
    iter1 <- peek iter1Ptr
    iter2 <- peek iter2Ptr
    liftM orderToGInt $ sortFunc iter1 iter2)
  {# call tree_sortable_set_sort_func #}
    (toTreeSortable self)
    (fromIntegral sortColumnId)
    fPtr (castFunPtrToPtr fPtr) destroyFunPtr

orderToGInt :: Ordering -> {#type gint#}
orderToGInt LT = -1
orderToGInt EQ = 0
orderToGInt GT = 1

{#pointer TreeIterCompareFunc#}

foreign import ccall "wrapper" mkTreeIterCompareFunc ::
  (Ptr TreeModel -> Ptr TreeIter -> Ptr TreeIter -> Ptr () -> IO {#type gint#}) ->
  IO TreeIterCompareFunc

-- %hash c:221e d:7c9
-- | Sets the default comparison function used when sorting to be @sortFunc@.
-- If the current sort column id of @self@ is
-- 'treeSortableDefaultSortColumnId' then the model will sort using
-- this function.
--
-- | If @sortFunc@ is 'Nothing', then there will be no default comparison function.
-- This means that once the
-- model has been sorted, it can't go back to the default state. In this case, when the current sort
-- column id of sortable is 'TreeSortableDefaultSortColumnId', the model will be unsorted.
treeSortableSetDefaultSortFunc :: TreeSortableClass self => self
 -> Maybe (TreeIter -> TreeIter -> IO Ordering)
                                -- ^ @sortFunc@ - The comparison function
                                -- or 'Nothing' to use default comparison function.
 -> IO ()
treeSortableSetDefaultSortFunc self Nothing = do
  {# call tree_sortable_set_default_sort_func #}
    (toTreeSortable self)
    nullFunPtr nullPtr nullFunPtr
treeSortableSetDefaultSortFunc self (Just sortFunc) = do
  fPtr <- mkTreeIterCompareFunc (\_ iter1Ptr iter2Ptr _ -> do
    iter1 <- peek iter1Ptr
    iter2 <- peek iter2Ptr
    liftM orderToGInt $ sortFunc iter1 iter2)
  {# call tree_sortable_set_default_sort_func #}
    (toTreeSortable self)
    fPtr (castFunPtrToPtr fPtr) destroyFunPtr

-- %hash c:78ec d:d949
-- | Emits a 'sortColumnChanged' signal on the model.
--
treeSortableSortColumnChanged :: TreeSortableClass self => self -> IO ()
treeSortableSortColumnChanged self =
  {# call gtk_tree_sortable_sort_column_changed #}
    (toTreeSortable self)

-- %hash c:4a10 d:f107
-- | Returns @True@ if the model has a default sort function. This is used
-- primarily by 'Graphics.UI.Gtk.ModelView.TreeViewColumn's in order to
-- determine if a model has a default ordering or if the entries are
-- retrieved in the sequence in which they are stored in the model.
--
treeSortableHasDefaultSortFunc :: TreeSortableClass self => self
 -> IO Bool -- ^ returns @True@, if the model has a default sort function
treeSortableHasDefaultSortFunc self =
  liftM toBool $
  {# call gtk_tree_sortable_has_default_sort_func #}
    (toTreeSortable self)

--------------------
-- Signals

-- %hash c:c461 d:af3f
-- |
--
sortColumnChanged :: TreeSortableClass self => Signal self (IO ())
sortColumnChanged = Signal (connect_NONE__NONE "sort-column-changed")
