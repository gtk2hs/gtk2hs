-- -*-haskell-*-
-- GIMP Toolkit (GTK) TreeModelSort
--
--  Author : Duncan Coutts
--
--  Created: 4 August 2004
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/12 17:19:26 $
--
--  Copyright (C) 2004-2005 Duncan Coutts
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
-- The 'TreeModelSort' is a model that turns any object that implements the
-- 'TreeModel' interface into a store that is sorted.
--
-- It does not hold any data itself, but rather is created with a child model
-- and proxies its data. It has identical column types to this child model, and
-- the changes in the child are propagated. The primary purpose of this model is
-- to provide a way to sort a different model without modifying it.
--
module Graphics.UI.Gtk.TreeList.TreeModelSort (
  TreeModelSort,
  TreeModelSortClass,
  treeModelSortNewWithModel,
  treeModelSortGetModel,
  treeModelSortConvertChildPathToPath,
  treeModelSortConvertPathToChildPath,
  treeModelSortConvertChildIterToIter,
  treeModelSortConvertIterToChildIter,  
  treeModelSortResetDefaultSortFunc
  ) where

import Monad	(liftM, when)

import System.Glib.FFI
import System.Glib.GObject			(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.TreeList.TreeModel#}
import Graphics.UI.Gtk.General.Structs		(treeIterSize)

{# context lib="gtk" prefix="gtk" #}


-- | Creates a new 'TreeModelSort', that will be a sorted view of the given
-- model.
--
treeModelSortNewWithModel :: TreeModelClass tm => tm -> IO TreeModelSort
treeModelSortNewWithModel model =
  makeNewGObject mkTreeModelSort $ liftM castPtr $
  {#call unsafe tree_model_sort_new_with_model#} (toTreeModel model)

-- | Returns the underlying model the 'TreeModelSort' is sorting.
--
treeModelSortGetModel :: TreeModelSortClass obj => obj -> IO TreeModel
treeModelSortGetModel obj =
  makeNewGObject mkTreeModel $
  {#call tree_model_sort_get_model#} (toTreeModelSort obj)

-- | Converts the given path to a path relative to the given sorted model. That
-- is, the given path points to a row in the child model. The returned path will
-- point to the same row in the sorted model.
--
treeModelSortConvertChildPathToPath :: TreeModelSortClass obj => obj
                                    -> TreePath -> IO TreePath
treeModelSortConvertChildPathToPath obj childPath = do
  pathPtr <-
    throwIfNull "treeModelSortConvertChildPathToPath: invalid path given" $ 
    {#call unsafe tree_model_sort_convert_child_path_to_path#}
    (toTreeModelSort obj) childPath
  liftM TreePath $ newForeignPtr pathPtr (tree_path_free pathPtr)

-- | Converts path in the sorted model to a path on the unsorted model on which
-- the given 'TreeModelSort' is based. That is, the given path points to a
-- location in the given 'TreeModelSort'. The returned path will point to the
-- same location in the underlying unsorted model.
--
treeModelSortConvertPathToChildPath :: TreeModelSortClass obj => obj
                                    -> TreePath -> IO TreePath
treeModelSortConvertPathToChildPath obj sortedPath = do
  pathPtr <-
    throwIfNull "treeModelSortConvertPathToChildPath: invalid path given" $ 
    {#call unsafe tree_model_sort_convert_path_to_child_path#}
    (toTreeModelSort obj) sortedPath
  liftM TreePath $ newForeignPtr pathPtr (tree_path_free pathPtr)

-- | Return an iterator in the sorted model that points to the row pointed to
-- by the given iter from the unsorted model.
--
treeModelSortConvertChildIterToIter :: TreeModelSortClass obj => obj
                                    -> TreeIter -> IO TreeIter
treeModelSortConvertChildIterToIter obj childIter = do
  sortIterPtr <- mallocBytes treeIterSize
  sortIter <- liftM TreeIter $ newForeignPtr sortIterPtr
                (foreignFree sortIterPtr)
  {#call tree_model_sort_convert_child_iter_to_iter#} (toTreeModelSort obj)
    sortIter childIter
  return sortIter

-- | Return an iterator in the unsorted model that points to the row pointed to
-- by the given iter from the sorted model.
--
treeModelSortConvertIterToChildIter :: TreeModelSortClass obj => obj
                                    -> TreeIter -> IO TreeIter
treeModelSortConvertIterToChildIter obj sortedIter = do
  childIterPtr <- mallocBytes treeIterSize
  childIter <- liftM TreeIter $ newForeignPtr childIterPtr
                 (foreignFree childIterPtr)
  {#call unsafe tree_model_sort_convert_iter_to_child_iter#}
    (toTreeModelSort obj) childIter sortedIter
  return childIter

-- | This resets the default sort function to be in the \'unsorted\' state. That
-- is, it is in the same order as the child model. It will re-sort the model to
-- be in the same order as the child model only if the 'TreeModelSort' is in 
-- \'unsorted\' state.
--
treeModelSortResetDefaultSortFunc :: TreeModelSortClass obj => obj -> IO ()
treeModelSortResetDefaultSortFunc obj =
  {#call tree_model_sort_reset_default_sort_func#} (toTreeModelSort obj)
