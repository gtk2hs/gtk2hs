-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget TreeView@
--
--  Author : Axel Simon
--          
--  Created: 9 May 2001
--
--  Version $Revision: 1.7 $ from $Date: 2002/08/05 16:41:35 $
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
-- * This widget constitutes the main widget for displaying lists and other
--   structured data.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
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
  treeViewInsertColumnWithAttributes,
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
import GObject	(makeNewGObject, objectRef, objectUnref)
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
{#import TreeModel#}
{#import TreeViewColumn#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor treeViewNew@ Make a new TreeView widget.
--
treeViewNew :: IO TreeView
treeViewNew  = makeNewObject mkTreeView (liftM castPtr {#call tree_view_new#})

-- @constructor treeViewNewWithModel@ Make a new TreeView widget with 
-- @ref arg tm@ as the storage model.
--
treeViewNewWithModel :: TreeModelClass tm => tm -> IO TreeView
treeViewNewWithModel tm = makeNewObject mkTreeView $ liftM castPtr $
  {#call tree_view_new_with_model#} (toTreeModel tm)

-- @method treeViewGetModel@ Retrieve the TreeModel that supplies the data for
-- this TreeView. Returns Nothing if no model is currently set.
--
treeViewGetModel :: TreeViewClass tv => tv -> IO (Maybe TreeModel)
treeViewGetModel tv = do
  tmPtr <- {#call unsafe tree_view_get_model#} (toTreeView tv)
  if tmPtr==nullPtr then return Nothing else do
    objectRef tmPtr
    liftM (Just . mkTreeModel) $ newForeignPtr tmPtr (objectUnref tmPtr)

-- @method treeViewSetModel@ Set the TreeModel for the current View.
--
treeViewSetModel :: (TreeViewClass tv, TreeModelClass tm) => tv -> tm -> IO ()
treeViewSetModel tv tm =
  {#call tree_view_set_model#} (toTreeView tv) (toTreeModel tm)

-- @method treeViewGetSelection@ Retrieve a @ref type TreeSelection@ that
-- holds the current selected nodes of the View.
--
treeViewGetSelection :: TreeViewClass tv => tv -> IO TreeSelection
treeViewGetSelection tv = makeNewGObject mkTreeSelection $
  {#call unsafe tree_view_get_selection#} (toTreeView tv)

-- @method treeViewGetHadjustment@ Get the @ref arg Adjustment@ that
-- represents the horizontal aspect.
--
treeViewGetHadjustment :: TreeViewClass tv => tv -> IO (Maybe Adjustment)
treeViewGetHadjustment tv = do
  adjPtr <- {#call unsafe tree_view_get_hadjustment#} (toTreeView tv)
  if adjPtr==nullPtr then return Nothing else do
    liftM Just $ makeNewObject mkAdjustment (return adjPtr)

-- @method treeViewSetHadjustment@ Set the @ref arg Adjustment@ that controls
-- the horizontal aspect. If @ref arg adj@ is Nothing then set no Adjustment
-- widget.
--
treeViewSetHadjustment :: TreeViewClass tv => (Maybe Adjustment) -> tv -> IO ()
treeViewSetHadjustment adj tv = {#call tree_view_set_hadjustment#} 
  (toTreeView tv) (fromMaybe (mkAdjustment nullForeignPtr) adj)

-- @method treeViewGetVadjustment@ Get the @ref arg Adjustment@ that
-- represents the vertical aspect.
--
treeViewGetVadjustment :: TreeViewClass tv => tv -> IO (Maybe Adjustment)
treeViewGetVadjustment tv = do
  adjPtr <- {#call unsafe tree_view_get_vadjustment#} (toTreeView tv)
  if adjPtr==nullPtr then return Nothing else do
    liftM Just $ makeNewObject mkAdjustment (return adjPtr)

-- @method treeViewSetVadjustment@ Set the @ref arg Adjustment@ that controls
-- the vertical aspect. If @ref arg adj@ is Nothing then set no Adjustment
-- widget.
--
treeViewSetVadjustment :: TreeViewClass tv => (Maybe Adjustment) -> tv -> IO ()
treeViewSetVadjustment adj tv = {#call tree_view_set_vadjustment#} 
  (toTreeView tv) (fromMaybe (mkAdjustment nullForeignPtr) adj)


-- @method treeViewGetHeadersVisible@ Query if the column headers are visible.
--
treeViewGetHeadersVisible :: TreeViewClass tv => tv -> IO Bool
treeViewGetHeadersVisible tv = liftM toBool $
  {#call unsafe tree_view_get_headers_visible#} (toTreeView tv)

-- @method treeViewSetHeadersVisible@ Set the visibility state of the column
-- headers.
--
treeViewSetHeadersVisible :: TreeViewClass tv => tv -> Bool -> IO ()
treeViewSetHeadersVisible tv vis = {#call tree_view_set_headers_visible#}
  (toTreeView tv) (fromBool vis)

-- @method treeViewColumnsAutosize@ Resize the columns to their optimal size.
--
treeViewColumnsAutosize :: TreeViewClass tv => tv -> IO ()
treeViewColumnsAutosize tv =
  {#call tree_view_columns_autosize#} (toTreeView tv)

-- @method treeViewSetHeadersClickable@ Set wether the columns headers are
-- sensitive to mouse clicks.
--
-- *  @literal@
-- 

--
treeViewSetHeadersClickable :: TreeViewClass tv => tv -> Bool -> IO ()
treeViewSetHeadersClickable tv click = {#call tree_view_set_headers_clickable#}
  (toTreeView tv) (fromBool click)

-- @method treeViewAppendColumn@ Append a new column to the TreeView. Returns
-- the new number of columns.
--
treeViewAppendColumn :: TreeViewClass tv => tv -> TreeViewColumn -> IO Int
treeViewAppendColumn tv tvc = liftM fromIntegral $
  {#call tree_view_append_column#} (toTreeView tv) tvc

-- @method treeViewRemoveColumn@ Remove column @ref arg tvc@ from the TreeView
-- widget. The number of remaining columns is returned.
--
treeViewRemoveColumn :: TreeViewClass tv => tv -> TreeViewColumn -> IO Int
treeViewRemoveColumn tv tvc = liftM fromIntegral $
  {#call tree_view_remove_column#} (toTreeView tv) tvc

-- @method treeViewInsertColumn@ Inserts column @ref arg tvc@ into the
-- TreeView widget at the position @ref arg pos@. Returns the number of
-- columns after insertion. Specify -1 for @ref arg pos@ to insert the column
-- at the end.
--
treeViewInsertColumn :: TreeViewClass tv => tv -> TreeViewColumn -> Int ->
                        IO Int
treeViewInsertColumn tv tvc pos = liftM fromIntegral $ 
  {#call tree_view_insert_column#} (toTreeView tv) tvc (fromIntegral pos)


-- @method treeViewInsertColumnWithAttributes@ Inserts new column into the
-- TreeView @ref arg tv@ at position @ref arg pos@ with title
-- @ref argtitle@, cell renderer @ref arg cr@ and attributes
-- @ref arg attribs@. Specify -1 for @ref arg pos@ to insert the column at
-- the end.
--
treeViewInsertColumnWithAttributes :: (TreeViewClass tv, CellRendererClass cr)
   => tv -> Int -> String -> cr -> [(String,Int)] -> IO ()
treeViewInsertColumnWithAttributes tv pos title cr attribs = 
    do
    column <- treeViewColumnNew
    treeViewColumnSetTitle column title
    treeViewColumnPackStart column cr True
    treeViewColumnAddAttributes column cr attribs
    treeViewInsertColumn tv column pos
    return ()

-- @method treeViewGetColumn@ Retrieve the @ref arg pos@ th columns of
-- TreeView. If the index is out of range Nothing is returned.
--
treeViewGetColumn :: TreeViewClass tv => tv -> Int -> IO (Maybe TreeViewColumn)
treeViewGetColumn tv pos = do
  tvcPtr <- {#call unsafe tree_view_get_column#} (toTreeView tv) 
    (fromIntegral pos)
  if tvcPtr==nullPtr then return Nothing else 
    liftM Just $ makeNewObject mkTreeViewColumn (return tvcPtr)

-- @method treeViewScrollToCell@ Scroll to a cell specified by @ref arg path@
-- and @ref arg tvc@. The cell is aligned within the TreeView widget as
-- follows: horizontally by @ref arg hor@ from left (0.0) to right (1.0) and
-- vertically by @ref arg ver@ from top (0.0) to buttom (1.0).
--
treeViewScrollToCell :: TreeViewClass tv => tv -> TreePath -> TreeViewColumn ->
                        Maybe (Float,Float) -> IO ()
treeViewScrollToCell tv path tvc (Just (ver,hor)) = 
  {#call tree_view_scroll_to_cell#} 
  (toTreeView tv) path tvc 1 (realToFrac ver) (realToFrac hor)
treeViewScrollToCell tv path tvc Nothing = 
  {#call tree_view_scroll_to_cell#} 
  (toTreeView tv) path tvc 0 0.0 0.0

-- @method treeViewExpandAll@ Expand all nodes in the TreeView.
--
treeViewExpandAll :: TreeViewClass tv => tv -> IO ()
treeViewExpandAll tv =
  {#call tree_view_expand_all#} (toTreeView tv)

-- @method treeViewCollapseAll@ Collapse all nodes in the TreeView.
--
treeViewCollapseAll :: TreeViewClass tv => tv -> IO ()
treeViewCollapseAll tv =
  {#call tree_view_collapse_all#} (toTreeView tv)

-- @method treeViewExpandRow@ Expand a node that is specified by 
-- @ref arg path@. If the @ref arg all@ is True every
-- child will be expanded recursively. Returns True if the row existed and
-- had children.
--
treeViewExpandRow :: TreeViewClass tv => TreePath -> Bool -> tv -> IO Bool
treeViewExpandRow path all tv = liftM toBool $
  {#call tree_view_expand_row#} (toTreeView tv) path (fromBool all)

-- @method treeViewCollapseRow@ Collapse a row. Returns True if the row
-- existed.
--
treeViewCollapseRow :: TreeViewClass tv => tv -> TreePath -> IO Bool
treeViewCollapseRow tv path = liftM toBool $
  {#call tree_view_collapse_row#} (toTreeView tv) path





  
