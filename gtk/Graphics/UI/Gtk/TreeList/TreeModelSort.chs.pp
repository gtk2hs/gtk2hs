-- -*-haskell-*-
-- GIMP Toolkit (GTK) TreeModelSort
--
--  Author : Duncan Coutts
--  Created: 4 August 2004
--
--  Copyright (c) 2004..2005 Duncan Coutts, Axel Simon
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
-- A GtkTreeModel which makes an underlying tree model sortable
--
module Graphics.UI.Gtk.TreeList.TreeModelSort  (
-- * Description
-- 
-- | The "TreeModelSort" is a model which implements the "TreeSortable"
-- interface. It does not hold any data itself, but rather is created with a
-- child model and proxies its data. It has identical column types to this
-- child model, and the changes in the child are propagated. The primary
-- purpose of this model is to provide a way to sort a different model without
-- modifying it.
--
-- The use of this is best demonstrated through an example. In the following
-- sample code we create two "TreeView" widgets each with a view of the same
-- data. As the model is wrapped here by a "TreeModelSort", the two "TreeView"s
-- can each sort their view of the data without affecting the other. By
-- contrast, if we simply put the same model in each widget, then sorting the
-- first would sort the second.
--
-- * Using a GtkTreeModelSort
--
-- * FIXME: if the follwing is a C code example, port it to Haskell or remove it
-- 
-- > {
-- >   GtkTreeView *tree_view1;
-- >   GtkTreeView *tree_view2;
-- >   GtkTreeModel *sort_model1;
-- >   GtkTreeModel *sort_model2;
-- >   GtkTreeModel *child_model;
-- > 
-- >   /* get the child model */
-- >   child_model = get_my_model();
-- > 
-- >   /* Create the first tree */
-- >   sort_model1 = gtk_tree_model_sort_new_with_model (child_model);
-- >   tree_view1 = gtk_tree_view_new_with_model (sort_model1);
-- > 
-- >   /* Create the second tree */
-- >   sort_model2 = gtk_tree_model_sort_new_with_model (child_model);
-- >   tree_view2 = gtk_tree_view_new_with_model (sort_model2);
-- > 
-- >   /* Now we can sort the two models independently */
-- >   gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (sort_model1),
-- >                                         COLUMN_1, GTK_SORT_ASCENDING);
-- >   gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (sort_model2),
-- >                                         COLUMN_1, GTK_SORT_DESCENDING);
-- > }
--
-- To demonstrate how to access the underlying child model from the sort
-- model, the next example will be a callback for the "TreeSelection"
-- \"changed\" signal. In this callback, we get a string from COLUMN_1 of the
-- model. We then modify the string, find the same selected row on the child
-- model, and change the row there.
--
-- * Accessing the child model of in a selection changed callback
--
-- * FIXME: if the follwing is a C code example, port it to Haskell or remove it
-- 
-- > void
-- > selection_changed (GtkTreeSelection *selection, gpointer data)
-- > {
-- >   GtkTreeModel *sort_model = NULL;
-- >   GtkTreeModel *child_model;
-- >   GtkTreeIter sort_iter;
-- >   GtkTreeIter child_iter;
-- >   char *some_data = NULL;
-- >   char *modified_data;
-- > 
-- >   /* Get the current selected row and the model. */
-- >   if (! gtk_tree_selection_get_selected (selection,
-- >                                          sort_model,
-- >                                          sort_iter))
-- >     return;
-- > 
-- > 
-- >   /* Look up the current value on the selected row and get a new value
-- >    * to change it to.
-- >    */
-- >   gtk_tree_model_get (GTK_TREE_MODEL (sort_model), sort_iter,
-- >                       COLUMN_1, some_data,
-- >                       -1);
-- > 
-- >   modified_data = change_the_data (some_data);
-- >   g_free (some_data);
-- > 
-- >   /* Get an iterator on the child model, instead of the sort model. */
-- >   gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (sort_model),
-- >                                                   child_iter,
-- >                                                   sort_iter);
-- > 
-- >   /* Get the child model and change the value of the row.  In this
-- >    * example, the child model is a GtkListStore.  It could be any other
-- >    * type of model, though.
-- >    */
-- >   child_model = gtk_tree_model_sort_get_model (GTK_TREE_MODEL_SORT (sort_model));
-- >   gtk_list_store_set (GTK_LIST_STORE (child_model), child_iter,
-- >                       COLUMN_1, modified_data,
-- >                       -1);
-- >   g_free (modified_data);
-- > }

-- * Class Hierarchy
-- |
-- @
-- |  "GObject"
-- |   +----GtkTreeModelSort
-- @

-- * Types
  TreeModelSort,
  TreeModelSortClass,

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
  treeModelSortIterIsValid
#endif
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

-- | Converts the given path to a path relative to the given sorted model.
--
-- * The given path points to a row in the child model. The returned path will
-- point to the same row in the sorted model.
--
treeModelSortConvertChildPathToPath :: TreeModelSortClass obj => obj
                                    -> TreePath -> IO TreePath
treeModelSortConvertChildPathToPath obj [] = return []
treeModelSortConvertChildPathToPath obj tp = do
  nativePath <- nativeTreePathNew
  mapM_ ({#call unsafe tree_path_append_index#} nativePath . fromIntegral) tp
  tpPtr <-
    {#call unsafe tree_model_sort_convert_child_path_to_path#}
    (toTreeModelSort obj) nativePath
  nativeTreePathFree nativePath
  if tpPtr==nullPtr then return [] else do
  path <- nativeTreePathGetIndices (NativeTreePath tpPtr)
  nativeTreePathFree (NativeTreePath tpPtr)
  return path

-- | Converts path in the sorted model to a path on the unsorted model on which
-- the given 'TreeModelSort' is based. That is, the given path points to a
-- location in the given 'TreeModelSort'. The returned path will point to the
-- same location in the underlying unsorted model.
--
treeModelSortConvertPathToChildPath :: TreeModelSortClass obj => obj
                                    -> TreePath -> IO TreePath
treeModelSortConvertPathToChildPath obj [] = return []
treeModelSortConvertPathToChildPath obj tp = do
  nativePath <- nativeTreePathNew
  mapM_ ({#call unsafe tree_path_append_index#} nativePath . fromIntegral) tp
  tpPtr <-
    {#call unsafe tree_model_sort_convert_path_to_child_path#}
    (toTreeModelSort obj) nativePath
  nativeTreePathFree nativePath
  if tpPtr==nullPtr then return [] else do
  path <- nativeTreePathGetIndices (NativeTreePath tpPtr)
  nativeTreePathFree (NativeTreePath tpPtr)
  return path

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


-- | Clear the cache of unref\'d iterators.
--
-- * This function should almost never be called. It clears the
-- "TreeModelSort" of any cached iterators that haven\'t been reffed with
-- 'treeModelRefNode'. This might be useful if the child model being sorted is
-- static (and doesn\'t change often) and there has been a lot of unreffed
-- access to nodes. As a side effect of this function, all unreffed iters will
-- be invalid.
-- 
treeModelSortClearCache :: TreeModelSortClass self => self -> IO ()
treeModelSortClearCache self =
  {# call gtk_tree_model_sort_clear_cache #}
     (toTreeModelSort self)

#if GTK_CHECK_VERSION(2,2,0)
-- | Checks if the given iter is a valid iter for this "TreeModelSort".
--
-- * WARNING: This function is slow. Only use it for debugging and\/or testing
-- purposes.
--
-- * Available since Gtk version 2.2
-- 
treeModelSortIterIsValid :: TreeModelSortClass self => self
 -> TreeIter -- ^ @iter@ - A "TreeIter".
 -> IO Bool          -- ^ returns @True@ if the iter is valid, @False@ if the
                     -- iter is invalid.
treeModelSortIterIsValid self iter =
  liftM toBool $
  {# call gtk_tree_model_sort_iter_is_valid #}
     (toTreeModelSort self)
     iter
#endif

