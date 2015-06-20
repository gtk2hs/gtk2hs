{-# LANGUAGE CPP #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) TreeModelSort
--
--  Author : Duncan Coutts
--
--  Created: 4 August 2004
--
--  Copyright (C) 2004-2005 Duncan Coutts, Axel Simon
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
-- A 'TreeModel' which makes an underlying tree model sortable
--
module Graphics.UI.Gtk.ModelView.TreeModelSort (
-- * Detail
--
-- | The 'TreeModelSort' is a model which implements the 'TreeSortable'
-- interface. It does not hold any data itself, but rather is created with a
-- child model and proxies its data. It has identical rows to its
-- child model, and the changes in the child are propagated. The primary
-- purpose of this model is to provide a way to sort a model without
-- modifying it.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----TreeModelSort
-- @

-- * Types
  TreeModelSort,
  TreeModelSortClass,
  castToTreeModelSort, gTypeTreeModelSort,
  toTreeModelSort,

  TypedTreeModelSort,

-- * Constructors
  treeModelSortNewWithModel,

-- * Methods
  treeModelSortGetModel,
  treeModelSortConvertChildPathToPath,
  treeModelSortConvertPathToChildPath,
  treeModelSortConvertChildIterToIter,
  treeModelSortConvertIterToChildIter,
  treeModelSortResetDefaultSortFunc,
  treeModelSortClearCache,
#if GTK_CHECK_VERSION(2,2,0)
  treeModelSortIterIsValid,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.ModelView.TreeModel#}
{#import Graphics.UI.Gtk.ModelView.Types#}

{# context lib="gtk" prefix="gtk" #}

instance TreeModelClass (TypedTreeModelSort a)
instance TreeModelSortClass (TypedTreeModelSort a)
instance GObjectClass (TypedTreeModelSort a) where
  toGObject (TypedTreeModelSort tm) = GObject (castForeignPtr tm)
  unsafeCastGObject = TypedTreeModelSort . castForeignPtr . unGObject
instance TreeSortableClass TreeModelSort
instance TreeSortableClass (TypedTreeModelSort row)

--------------------
-- Constructors

-- | Creates a new 'TreeModelSort', that will be a sorted view of the given
-- model.
--
treeModelSortNewWithModel :: (TreeModelClass (childModel row),
                              TypedTreeModelClass childModel) =>
                              childModel row -> IO (TypedTreeModelSort row)
treeModelSortNewWithModel childModel = liftM unsafeTreeModelSortToGeneric $
  wrapNewGObject mkTreeModelSort $
  liftM (castPtr :: Ptr TreeModel -> Ptr TreeModelSort) $
  {# call tree_model_sort_new_with_model #}
    (toTreeModel childModel)

--------------------
-- Methods

-- | Returns the underlying model the 'TreeModelSort' is sorting.
--
treeModelSortGetModel :: TreeModelSortClass self => self -> IO TreeModel
treeModelSortGetModel self =
  makeNewGObject mkTreeModel $
  {# call tree_model_sort_get_model #}
    (toTreeModelSort self)

-- | Converts the given path to a path relative to the given sorted model.
--
-- * The given path points to a row in the child model. The returned path will
-- point to the same row in the sorted model.
--
treeModelSortConvertChildPathToPath :: TreeModelSortClass self => self
 -> TreePath
 -> IO TreePath
treeModelSortConvertChildPathToPath self [] = return []
treeModelSortConvertChildPathToPath self childPath =
  withTreePath childPath $ \childPath ->
  {# call tree_model_sort_convert_child_path_to_path #}
    (toTreeModelSort self)
    childPath
  >>= fromTreePath

-- | Converts path in the sorted model to a path on the unsorted model on which
-- the given 'TreeModelSort' is based. That is, the given path points to a
-- location in the given 'TreeModelSort'. The returned path will point to the
-- same location in the underlying unsorted model.
--
treeModelSortConvertPathToChildPath :: TreeModelSortClass self => self
 -> TreePath
 -> IO TreePath
treeModelSortConvertPathToChildPath self [] = return []
treeModelSortConvertPathToChildPath self sortedPath =
  withTreePath sortedPath $ \sortedPath ->
  {# call tree_model_sort_convert_path_to_child_path #}
    (toTreeModelSort self)
    sortedPath
  >>= fromTreePath

-- | Return an iterator in the sorted model that points to the row pointed to
-- by the given iter from the unsorted model.
--
treeModelSortConvertChildIterToIter :: TreeModelSortClass self => self
 -> TreeIter
 -> IO TreeIter
treeModelSortConvertChildIterToIter self childIter =
  with childIter $ \childIterPtr ->
  alloca $ \sortIterPtr -> do
  {# call tree_model_sort_convert_child_iter_to_iter #}
    (toTreeModelSort self)
    sortIterPtr
    childIterPtr
  peek sortIterPtr

-- | Return an iterator in the unsorted model that points to the row pointed to
-- by the given iter from the sorted model.
--
treeModelSortConvertIterToChildIter :: TreeModelSortClass self => self
 -> TreeIter
 -> IO TreeIter
treeModelSortConvertIterToChildIter self sortedIter =
  with sortedIter $ \sortedIterPtr ->
  alloca $ \childIterPtr -> do
  {# call tree_model_sort_convert_iter_to_child_iter #}
    (toTreeModelSort self)
    childIterPtr
    sortedIterPtr
  peek childIterPtr

-- | This resets the default sort function. As a consequence, the order of
-- this model will be the same order as that of the child model.
--
treeModelSortResetDefaultSortFunc :: TreeModelSortClass self => self -> IO ()
treeModelSortResetDefaultSortFunc self =
  {# call tree_model_sort_reset_default_sort_func #}
    (toTreeModelSort self)

-- | Clear the cache of unref'd iterators.
--
-- * This function should almost never be called. It clears the
-- 'TreeModelSort' of any cached iterators that haven't been reffed with
-- 'treeModelRefNode'. This might be useful if the child model being sorted is
-- static (and doesn't change often) and there has been a lot of unreffed
-- access to nodes. As a side effect of this function, all unreffed iters will
-- be invalid.
--
treeModelSortClearCache :: TreeModelSortClass self => self -> IO ()
treeModelSortClearCache self =
  {# call gtk_tree_model_sort_clear_cache #}
    (toTreeModelSort self)

#if GTK_CHECK_VERSION(2,2,0)
-- | Checks if the given iter is a valid iter for this 'TreeModelSort'.
--
-- * WARNING: This function is slow. Only use it for debugging and\/or testing
-- purposes.
--
-- * Available since Gtk+ version 2.2
--
treeModelSortIterIsValid :: TreeModelSortClass self => self
 -> TreeIter -- ^ @iter@ - A 'TreeIter'.
 -> IO Bool  -- ^ returns @True@ if the iter is valid, @False@ if the iter is
             -- invalid.
treeModelSortIterIsValid self iter =
  liftM toBool $
  with iter $ \iterPtr ->
  {# call gtk_tree_model_sort_iter_is_valid #}
    (toTreeModelSort self)
    iterPtr
#endif
