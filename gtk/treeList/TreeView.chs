-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget TreeView
--
--  Author : Axel Simon
--          
--  Created: 9 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/04 14:02:30 $
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
-- * This widget constitutes the main widget for displaying lists and other
--   structured data.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------
--
-- * treeViewGetPathAtPos is surely useful but has to be used with events in
--   order not to pass GdkWindows around.
--
module TreeView(
  TreeView,
  TreeViewClass,
  castToTreeView,
  treeViewNew,
  treeViewNewWithModel,
  treeViewGetModel,
  treeViewSetModel,
  treeViewGetSelection,
  treeViewGetHadjustment,
  treeViewSetHadjustment,
  treeViewGetVadjustment,
  treeViewSetVadjustment,
  treeViewGetHeadersVisible,
  treeViewSetHeadersVisible,
  treeViewColumnsAutosize,
  treeViewSetHeadersClickable,
  treeViewAppendColumn,
  treeViewRemoveColumn,
  treeViewInsertColumn,
  treeViewGetColumn,
  treeViewScrollToCell,
  treeViewExpandAll,
  treeViewCollapseAll,
  treeViewCollapseRow
  ) where

import Monad	(liftM)
import Maybe	(fromMaybe)
import Foreign
import UTFCForeign
import Structs	(nullForeignPtr)
import GObject	(objectRef, objectUnref)
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
{#import TreeModel#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Make a new TreeView widget. (EXPORTED)
--
treeViewNew :: IO TreeView
treeViewNew = makeNewObject mkTreeView (liftM castPtr {#call tree_view_new#})

-- Make a new TreeView widget with @tm as the storage model. (EXPORTED)
--
treeViewNewWithModel :: TreeModelClass tm => tm -> IO TreeView
treeViewNewWithModel tm = makeNewObject mkTreeView $ liftM castPtr $
  {#call tree_view_new_with_model#} (toTreeModel tm)

-- Retrieve the TreeModel that supplies the data for this TreeView.
-- Returns Nothing if no model is currently set. (EXPORTED)
--
treeViewGetModel :: TreeViewClass tv => tv -> IO (Maybe TreeModel)
treeViewGetModel tv = do
  tmPtr <- {#call unsafe tree_view_get_model#} (toTreeView tv)
  if tmPtr==nullPtr then return Nothing else do
    objectRef tmPtr
    liftM (Just . mkTreeModel) $ newForeignPtr tmPtr (objectUnref tmPtr)

-- Set the TreeModel for the current View. (EXPORTED)
--
treeViewSetModel :: (TreeViewClass tv, TreeModelClass tm) => tm -> tv -> IO ()
treeViewSetModel tm tv =
  {#call tree_view_set_model#} (toTreeView tv) (toTreeModel tm)

-- Retrieve a @TreeSelection that holds the current selected nodes of the View.
-- (EXPORTED)
--
treeViewGetSelection :: TreeViewClass tv => tv -> IO TreeSelection
treeViewGetSelection tv = makeNewObject mkTreeSelection $
  {#call unsafe tree_view_get_selection#} (toTreeView tv)

-- Get the @Adjustment that represents the horizontal aspect. (EXPORTED)
--
treeViewGetHadjustment :: TreeViewClass tv => tv -> IO (Maybe Adjustment)
treeViewGetHadjustment tv = do
  adjPtr <- {#call unsafe tree_view_get_hadjustment#} (toTreeView tv)
  if adjPtr==nullPtr then return Nothing else do
    liftM Just $ makeNewObject mkAdjustment (return adjPtr)

-- Set the @Adjustment that controls the horizontal aspect. If @adj is
-- Nothing then set no Adjustment widget. (EXPORTED)
treeViewSetHadjustment :: TreeViewClass tv => tv -> (Maybe Adjustment) -> IO ()
treeViewSetHadjustment tv adj = {#call tree_view_set_hadjustment#} 
  (toTreeView tv) (fromMaybe (mkAdjustment nullForeignPtr) adj)

-- Get the @Adjustment that represents the vertical aspect. (EXPORTED)
--
treeViewGetVadjustment :: TreeViewClass tv => tv -> IO (Maybe Adjustment)
treeViewGetVadjustment tv = do
  adjPtr <- {#call unsafe tree_view_get_vadjustment#} (toTreeView tv)
  if adjPtr==nullPtr then return Nothing else do
    liftM Just $ makeNewObject mkAdjustment (return adjPtr)

-- Set the @Adjustment that controls the vertical aspect. If @adj is
-- Nothing then set no Adjustment widget. (EXPORTED)
treeViewSetVadjustment :: TreeViewClass tv => tv -> (Maybe Adjustment) -> IO ()
treeViewSetVadjustment tv adj = {#call tree_view_set_vadjustment#} 
  (toTreeView tv) (fromMaybe (mkAdjustment nullForeignPtr) adj)


-- Query if the column headers are visible. (EXPORTED)
--
treeViewGetHeadersVisible :: TreeViewClass tv => tv -> IO Bool
treeViewGetHeadersVisible tv = liftM toBool $
  {#call unsafe tree_view_get_headers_visible#} (toTreeView tv)

-- Set the visibility state of the column headers. (EXPORTED)
--
treeViewSetHeadersVisible :: TreeViewClass tv => Bool -> tv -> IO ()
treeViewSetHeadersVisible vis tv = {#call tree_view_set_headers_visible#}
  (toTreeView tv) (fromBool vis)

-- Resize the columns to their optimal size. (EXPORTED)
--
treeViewColumnsAutosize :: TreeViewClass tv => tv -> IO ()
treeViewColumnsAutosize tv =
  {#call tree_view_columns_autosize#} (toTreeView tv)

-- Set wether the columns headers are sensitive to mouse clicks. (EXPORTED)
-- 
treeViewSetHeadersClickable :: TreeViewClass tv => Bool -> tv -> IO ()
treeViewSetHeadersClickable click tv = {#call tree_view_set_headers_clickable#}
  (toTreeView tv) (fromBool click)

-- Append a new column to the TreeView. Returns the new number of columns.
-- (EXPORTED)
--
treeViewAppendColumn :: TreeViewClass tv => TreeViewColumn -> tv -> IO Int
treeViewAppendColumn tvc tv = liftM fromIntegral $
  {#call tree_view_append_column#} (toTreeView tv) tvc

-- Remove column @tvc from the TreeView widget. The number of remaining 
-- columns is returned. (EXPORTED)
--
treeViewRemoveColumn :: TreeViewClass tv => TreeViewColumn -> tv -> IO Int
treeViewRemoveColumn tvc tv = liftM fromIntegral $
  {#call tree_view_remove_column#} (toTreeView tv) tvc

-- Inserts column @tvc into the TreeView widget at the position @pos. Returns
-- the number of columns after insertion. Specify -1 for @pos to insert the
-- column at the end. (EXPORTED)
--
treeViewInsertColumn :: TreeViewClass tv => 
  TreeViewColumn -> Int -> tv -> IO Int
treeViewInsertColumn tvc pos tv = liftM fromIntegral $ 
  {#call tree_view_insert_column#} (toTreeView tv) tvc (fromIntegral pos)


-- Retrieve the @pos th columns of TreeView. If the index is out of range
-- Nothing is returned. (EXPORTED)
--
treeViewGetColumn :: TreeViewClass tv => Int -> tv -> IO (Maybe TreeViewColumn)
treeViewGetColumn pos tv = do
  tvcPtr <- {#call unsafe tree_view_get_column#} (toTreeView tv) 
    (fromIntegral pos)
  if tvcPtr==nullPtr then return Nothing else 
    liftM Just $ makeNewObject mkTreeViewColumn (return tvcPtr)

-- Scroll to a cell specified by @path and @tvc. The cell is aligned within
-- the TreeView widget as follows: horizontally by @hor from left (0.0) to
-- right (1.0) and vertically by @ver from top (0.0) to buttom (1.0). 
-- (EXPORTED)
--
treeViewScrollToCell :: TreeViewClass tv => 
  TreePath -> TreeViewColumn -> Maybe (Float,Float) -> tv -> IO ()
treeViewScrollToCell path tvc (Just (ver,hor)) tv = 
  {#call tree_view_scroll_to_cell#} 
  (toTreeView tv) path tvc 1 (realToFrac ver) (realToFrac hor)
treeViewScrollToCell path tvc Nothing tv = 
  {#call tree_view_scroll_to_cell#} 
  (toTreeView tv) path tvc 0 0.0 0.0

-- Expand all nodes in the TreeView. (EXPORTED)
--
treeViewExpandAll :: TreeViewClass tv => tv -> IO ()
treeViewExpandAll tv =
  {#call tree_view_expand_all#} (toTreeView tv)

-- Collapse all nodes in the TreeView. (EXPORTED)
--
treeViewCollapseAll :: TreeViewClass tv => tv -> IO ()
treeViewCollapseAll tv =
  {#call tree_view_collapse_all#} (toTreeView tv)

-- Expand a node that is specified by @path. If the flag @all is True every
-- child will be expanded recursively. Returns True if the row existed and
-- had children.(EXPORTED)
--
treeViewExpandRow :: TreeViewClass tv => TreePath -> Bool -> tv -> IO Bool
treeViewExpandRow path all tv = liftM toBool $
  {#call tree_view_expand_row#} (toTreeView tv) path (fromBool all)

-- Collapse a row. Returns True if the row existed. (EXPORTED)
--
treeViewCollapseRow :: TreeViewClass tv => TreePath -> tv -> IO Bool
treeViewCollapseRow path tv = liftM toBool $
  {#call tree_view_collapse_row#} (toTreeView tv) path





  
