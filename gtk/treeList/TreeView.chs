{-# OPTIONS -cpp #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget TreeView@
--
--  Author : Axel Simon
--          
--  Created: 9 May 2001
--
--  Version $Revision: 1.13 $ from $Date: 2003/05/16 22:25:16 $
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
-- * The widget supports scrolling natively. This implies that pixel 
--   coordinates can be given in two formats: relative to the current view's
--   upper left corner or relative to the whole list's coordinates. The former
--   are called widget coordinates while the letter are called tree 
--   coordinates.
--
-- @todo@ ---------------------------------------------------------------------
--
-- * treeViewMoveColumnAfter and treeViewMoveColumnFirst are not dealt with in
--   Mogul
--
-- * gtk_tree_view_get_bin_window is to compare the GDK window from incoming
--   events. We don't marshal that window parameter, so this function is not
--   bound either.
--
-- * All functions related to drag and drop are missing.
--
-- * get_search_equal_func is missing: proper memory management is impossible
--
-- * gtk_tree_view_set_destroy_count_func is not meant to be useful
--
-- * expand-collapse-cursor-row needs to be bound if it is useful to expand
--   and collapse rows in a user-defined manner. Would only work on Gtk 2.2
--   and higher since the return parameter changed
--
-- * move_cursor, select_all, select_cursor_parent, select_cursor_row
--   toggle_cursor_row, unselect_all are not bound.
--   These functions are only useful to change the widgets
--   behaviour for these actions. Everything else can be done with
--   cursor_changed and columns_changed
--
-- * set_scroll_adjustment makes sense if the user monitors the scroll bars
--   *and* the scroll bars can be replaced anytime (the latter is odd)
--
-- Let's hope this file will always only contain macros.
#include<gtk/gtkversion.h>

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
  treeViewGetRulesHint,
  treeViewSetRulesHint,
  treeViewAppendColumn,
  treeViewRemoveColumn,
  treeViewInsertColumn,
  treeViewInsertColumnWithAttributes,
  treeViewGetColumn,
  treeViewGetColumns,
  treeViewMoveColumnAfter,
  treeViewMoveColumnFirst,
  treeViewSetExpanderColumn,
  treeViewGetExpanderColumn,
  treeViewSetColumnDragFunction,
  treeViewScrollToPoint,
  treeViewScrollToCell,
  treeViewSetCursor,
#if GTK_CHECK_VERSION(2,2,0)
  treeViewSetCursorOnCell,
#endif
  treeViewGetCursor,
  treeViewRowActivated,
  treeViewExpandAll,
  treeViewCollapseAll,
#if GTK_CHECK_VERSION(2,2,0)
  treeViewExpandToPath,
#endif
  treeViewExpandRow,
  treeViewCollapseRow,
  treeViewMapExpandedRows,
  treeViewRowExpanded,
  treeViewGetReorderable,
  treeViewSetReorderable,
  Point,
  treeViewGetPathAtPos,
  treeViewGetCellArea,
  treeViewGetBackgroundArea,
  treeViewGetVisibleRect,
  treeViewWidgetToTreeCoords,
  treeViewTreeToWidgetCoords,
  treeViewGetEnableSearch,
  treeViewSetEnableSearch,
  treeViewGetSearchColumn,
  treeViewSetSearchColumn,
  treeViewSetSearchEqualFunc,
  onColumnsChanged,
  afterColumnsChanged,
  onCursorChanged,
  afterCursorChanged,
  onRowActivated,
  afterRowActivated,
  onRowCollapsed,
  afterRowCollapsed,
  onRowExpanded,
  afterRowExpanded,
  onStartInteractiveSearch,
  afterStartInteractiveSearch,
  onTestCollapseRow,
  afterTestCollapseRow,
  onTestExpandRow,
  afterTestExpandRow
  ) where

import Monad	(liftM, mapM)
import Maybe	(fromMaybe)
import LocalData(newIORef, readIORef, writeIORef)
import Foreign
import UTFCForeign
import General	(mkDestructor)
import Structs	(nullForeignPtr, Point, Rectangle)
import GObject	(makeNewGObject, objectRef, objectUnref)
import Object	(makeNewObject)
import GList	(GList, fromGList)
{#import Hierarchy#}
{#import Signal#}
{#import TreeModel#}
{#import TreeViewColumn#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor treeViewNew@ Make a new @ref data TreeView@ widget.
--
treeViewNew :: IO TreeView
treeViewNew  = makeNewObject mkTreeView (liftM castPtr {#call tree_view_new#})

-- @constructor treeViewNewWithModel@ Create a new @ref data TreeView@ 
-- widget with @ref arg tm@ as the storage model.
--
treeViewNewWithModel :: TreeModelClass tm => tm -> IO TreeView
treeViewNewWithModel tm = makeNewObject mkTreeView $ liftM castPtr $
  {#call tree_view_new_with_model#} (toTreeModel tm)

-- @method treeViewGetModel@ Retrieve the TreeModel that supplies the data for
-- this @ref data TreeView@. Returns Nothing if no model is currently set.
--
treeViewGetModel :: TreeViewClass tv => tv -> IO (Maybe TreeModel)
treeViewGetModel tv = do
  tmPtr <- {#call unsafe tree_view_get_model#} (toTreeView tv)
  if tmPtr==nullPtr then return Nothing else do
    objectRef tmPtr
    liftM (Just . mkTreeModel) $ newForeignPtr tmPtr (objectUnref tmPtr)

-- @method treeViewSetModel@ Set the @ref data TreeModel@ for the current View.
--
treeViewSetModel :: (TreeViewClass tv, TreeModelClass tm) => tv -> tm -> IO ()
treeViewSetModel tv tm =
  {#call tree_view_set_model#} (toTreeView tv) (toTreeModel tm)

-- @method treeViewGetSelection@ Retrieve a @ref data TreeSelection@ that
-- holds the current selected nodes of the View.
--
treeViewGetSelection :: TreeViewClass tv => tv -> IO TreeSelection
treeViewGetSelection tv = makeNewGObject mkTreeSelection $
  {#call unsafe tree_view_get_selection#} (toTreeView tv)

-- @method treeViewGetHadjustment@ Get the @ref data Adjustment@ that
-- represents the horizontal aspect.
--
treeViewGetHadjustment :: TreeViewClass tv => tv -> IO (Maybe Adjustment)
treeViewGetHadjustment tv = do
  adjPtr <- {#call unsafe tree_view_get_hadjustment#} (toTreeView tv)
  if adjPtr==nullPtr then return Nothing else do
    liftM Just $ makeNewObject mkAdjustment (return adjPtr)

-- @method treeViewSetHadjustment@ Set the @ref data Adjustment@ that controls
-- the horizontal aspect. If @ref arg adj@ is Nothing then set no Adjustment
-- widget.
--
treeViewSetHadjustment :: TreeViewClass tv => (Maybe Adjustment) -> tv -> IO ()
treeViewSetHadjustment adj tv = {#call tree_view_set_hadjustment#} 
  (toTreeView tv) (fromMaybe (mkAdjustment nullForeignPtr) adj)

-- @method treeViewGetVadjustment@ Get the @ref data Adjustment@ that
-- represents the vertical aspect.
--
treeViewGetVadjustment :: TreeViewClass tv => tv -> IO (Maybe Adjustment)
treeViewGetVadjustment tv = do
  adjPtr <- {#call unsafe tree_view_get_vadjustment#} (toTreeView tv)
  if adjPtr==nullPtr then return Nothing else do
    liftM Just $ makeNewObject mkAdjustment (return adjPtr)

-- @method treeViewSetVadjustment@ Set the @ref data Adjustment@ that controls
-- the vertical aspect. If @ref arg adj@ is @literal Nothing@ then set no
-- @ref data Adjustment@ widget.
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
treeViewSetHeadersClickable :: TreeViewClass tv => tv -> Bool -> IO ()
treeViewSetHeadersClickable tv click = {#call tree_view_set_headers_clickable#}
  (toTreeView tv) (fromBool click)

-- @method treeViewGetRulesHint@ Give visual aid for wide columns.
--
-- * This function tells GTK+ that the user interface for your
--   application requires users to read across tree columns. By default,
--   GTK+ will then render the tree with alternating row colors. Do not use
--   it just because you prefer the appearance of the ruled tree; that's a
--   question for the theme. Some themes will draw tree rows in alternating
--   colors even when rules are turned off, and users who prefer that
--   appearance all the time can choose those themes. You should call this
--   function only as a semantic hint to the theme engine that your tree
--   makes alternating colors useful from a functional standpoint (since it
--   has lots of columns, generally).
--
treeViewGetRulesHint :: TreeViewClass tv => tv -> IO Bool
treeViewGetRulesHint tv = liftM toBool $
  {#call unsafe tree_view_get_rules_hint#} (toTreeView tv)

-- @method treeViewSetRulesHint@ Query if visual aid for wide columns is
-- turned on.
--
treeViewSetRulesHint :: TreeViewClass tv => tv -> Bool -> IO ()
treeViewSetRulesHint tv vis = {#call tree_view_set_rules_hint#}
  (toTreeView tv) (fromBool vis)

-- @method treeViewAppendColumn@ Append a new column to the @ref data TreeView@. Returns
-- the new number of columns.
--
treeViewAppendColumn :: TreeViewClass tv => tv -> TreeViewColumn -> IO Int
treeViewAppendColumn tv tvc = liftM fromIntegral $
  {#call tree_view_append_column#} (toTreeView tv) tvc

-- @method treeViewRemoveColumn@ Remove column @ref arg tvc@ from the @ref data TreeView@
-- widget. The number of remaining columns is returned.
--
treeViewRemoveColumn :: TreeViewClass tv => tv -> TreeViewColumn -> IO Int
treeViewRemoveColumn tv tvc = liftM fromIntegral $
  {#call tree_view_remove_column#} (toTreeView tv) tvc

-- @method treeViewInsertColumn@ Inserts column @ref arg tvc@ into the
-- @ref data TreeView@ widget at the position @ref arg pos@. Returns the number of
-- columns after insertion. Specify -1 for @ref arg pos@ to insert the column
-- at the end.
--
treeViewInsertColumn :: TreeViewClass tv => tv -> TreeViewColumn -> Int ->
                        IO Int
treeViewInsertColumn tv tvc pos = liftM fromIntegral $ 
  {#call tree_view_insert_column#} (toTreeView tv) tvc (fromIntegral pos)


-- @method treeViewInsertColumnWithAttributes@ Insert new
-- @ref data TreeViewColumn@.
--
-- * Inserts new column into the
-- @ref data TreeView@ @ref arg tv@ at position @ref arg pos@ with title
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

-- @method treeViewGetColumn@ Retrieve a @ref data TreeViewColumn@.
--
-- * Retrieve the @ref arg pos@ th columns of
--   @ref data TreeView@. If the index is out of range Nothing is returned.
--
treeViewGetColumn :: TreeViewClass tv => tv -> Int -> IO (Maybe TreeViewColumn)
treeViewGetColumn tv pos = do
  tvcPtr <- {#call unsafe tree_view_get_column#} (toTreeView tv) 
    (fromIntegral pos)
  if tvcPtr==nullPtr then return Nothing else 
    liftM Just $ makeNewObject mkTreeViewColumn (return tvcPtr)


-- @method treeViewGetColumns@ Return all @ref data TreeViewColumn@s in this
-- @ref data TreeView@.
--
treeViewGetColumns :: TreeViewClass tv => tv -> IO [TreeViewColumn]
treeViewGetColumns tv = do
  colsList <- {#call unsafe tree_view_get_columns#} (toTreeView tv)
  colsPtr <- fromGList colsList
  mapM (makeNewObject mkTreeViewColumn) (map return colsPtr)

-- @method treeViewMoveColumnAfter@ Move a specific column.
--
-- * Use @ref method treeViewMoveColumnToFront@ if you want to move the column
--   to the left end of the @ref data TreeView@.
--
treeViewMoveColumnAfter :: TreeViewClass tv => tv -> TreeViewColumn ->
					       TreeViewColumn -> IO ()
treeViewMoveColumnAfter tv which after = {#call tree_view_move_column_after#}
  (toTreeView tv) which after

-- @method treeViewMoveColumnFirst@ Move a specific column.
--
-- * Use @ref method treeViewMoveColumnAfter@ if you want to move the column
--   somewhere else than to the leftmost position.
--
treeViewMoveColumnFirst :: TreeViewClass tv => tv -> TreeViewColumn -> IO ()
treeViewMoveColumnFirst tv which = {#call tree_view_move_column_after#}
  (toTreeView tv) which (mkTreeViewColumn nullForeignPtr)

-- @method treeViewSetExpanderColumn@ Set location of hierarchy controls.
--
-- * Sets the column to draw the expander arrow at. If @ref arg col@
--   is @literal Nothing@, then the expander arrow is always at the first
--   visible column.
--
treeViewSetExpanderColumn :: TreeViewClass tv => tv -> Maybe TreeViewColumn ->
						 IO ()
treeViewSetExpanderColumn tv (Just tvc) =
  {#call unsafe tree_view_set_expander_column#} (toTreeView tv) tvc
treeViewSetExpanderColumn tv Nothing =
  {#call unsafe tree_view_set_expander_column#} (toTreeView tv)
    (mkTreeViewColumn nullForeignPtr)

-- @method treeViewGetExpanderColumn@ Get location of hierarchy controls.
--
-- * Gets the column to draw the expander arrow at. If @ref arg col@
--   is @literal Nothing@, then the expander arrow is always at the first
--   visible column.
--
treeViewGetExpanderColumn :: TreeViewClass tv => tv -> IO TreeViewColumn
treeViewGetExpanderColumn tv = makeNewObject mkTreeViewColumn $
  {#call unsafe tree_view_get_expander_column#} (toTreeView tv)

-- @method treeViewSetColumnDragFunction@ Specify where a column may be
-- dropped.
--
-- * Sets a user function for determining where a column may be dropped when
--   dragged.  This function is called on every column pair in turn at the
--   beginning of a column drag to determine where a drop can take place.
-- * The callback function take the @ref data TreeViewColumn@ to be moved, the
--   second and third arguments are the columns on the left and right side
--   of the new location. At most one of them might be @literal Nothing@
--   which indicates that the column is about to be dropped at the left or
--   right end of the @ref data TreeView@.
-- * The predicate @ref arg pred@ should return @literal True@ if it is ok
--   to insert the column at this place.
-- * Use @literal Nothing@ for the predicate if columns can be inserted
--   anywhere.
--
treeViewSetColumnDragFunction :: TreeViewClass tv => tv -> 
				 Maybe (TreeViewColumn ->
				        Maybe TreeViewColumn ->
					Maybe TreeViewColumn -> IO Bool) ->
				 IO ()
treeViewSetColumnDragFunction tv Nothing =
  {#call tree_view_set_column_drag_function#} (toTreeView tv)
    nullFunPtr nullPtr nullFunPtr
treeViewSetColumnDragFunction tv (Just pred) = do
  fPtr <- mkTreeViewColumnDropFunc $ \_ target prev next _ -> do
    target' <- makeNewObject mkTreeViewColumn (return target)
    prev' <- if prev==nullPtr then return Nothing else liftM Just $
      makeNewObject mkTreeViewColumn (return prev)
    next' <- if next==nullPtr then return Nothing else liftM Just $
      makeNewObject mkTreeViewColumn (return next)
    res <- pred target' prev' next'
    return (fromBool res)
  {#call tree_view_set_column_drag_function#} (toTreeView tv) fPtr 
    nullPtr nullFunPtr
  freeHaskellFunPtr fPtr

{#pointer TreeViewColumnDropFunc#}

foreign export dynamic mkTreeViewColumnDropFunc ::
  (Ptr () -> Ptr TreeViewColumn -> Ptr TreeViewColumn -> Ptr TreeViewColumn ->
  Ptr () -> IO {#type gboolean#}) -> IO TreeViewColumnDropFunc

-- @method treeViewScrollToPoint@ Scroll to a coordinate.
--

-- * Scrolls the tree view such that the top-left corner of the
--   visible area is @ref arg treeX@, @ref arg treeY@, where @ref arg treeX@
--   and @ref arg treeY@ are specified in tree window coordinates.
--   The @ref data TreeView@ must be realized before this function is
--   called.  If it isn't, you probably want to use
--   @ref method treeViewScrollToCell@.
--
treeViewScrollToPoint :: TreeViewClass tv => tv -> Int -> Int -> IO ()
treeViewScrollToPoint tv treeX treeY = 
  {#call tree_view_scroll_to_point#} (toTreeView tv)
    (fromIntegral treeX) (fromIntegral treeY)

-- @method treeViewScrollToCell@ Scroll to a cell.
--
-- * Scroll to a cell as specified by @ref arg path@ and @ref arg tvc@. 
--   The cell is aligned within the @ref data TreeView@ widget as
--   follows: horizontally by @ref arg hor@ from left (@literal 0.0@) to
--   right (@literal 1.0@) and vertically by @ref arg ver@ from top
--   (@literal 0.0@) to buttom (@literal 1.0@).
--
treeViewScrollToCell :: TreeViewClass tv => tv -> TreePath -> TreeViewColumn ->
                        Maybe (Float,Float) -> IO ()
treeViewScrollToCell tv path tvc (Just (ver,hor)) = 
  {#call tree_view_scroll_to_cell#} 
  (toTreeView tv) path tvc 1 (realToFrac ver) (realToFrac hor)
treeViewScrollToCell tv path tvc Nothing = 
  {#call tree_view_scroll_to_cell#} 
  (toTreeView tv) path tvc 0 0.0 0.0


-- @method treeViewSetCursor@ Selects a specific row.
--
-- * Sets the current keyboard focus to be at @ref arg path@, and
--   selects it.  This is useful when you want to focus the user's
--   attention on a particular row.  If @ref arg focusColumn@ is given,
--   then the input focus is given to the column specified by
--   it. Additionally, if @ref arg focusColumn@ is specified, and 
--   @ref arg startEditing@ is @literal True@,
--   then editing will be started in the
--   specified cell.  This function is often followed by a
--   @ref method widgetGrabFocus@ to the @ref data TreeView@ in order
--   to give keyboard focus to the widget.
--
treeViewSetCursor :: TreeViewClass tv => tv -> TreePath ->
					 (Maybe (TreeViewColumn, Bool)) ->
					 IO ()
treeViewSetCursor tv tp Nothing =
  {#call tree_view_set_cursor#} (toTreeView tv) tp
    (mkTreeViewColumn nullForeignPtr) (fromBool False)
treeViewSetCursor tv tp (Just (focusColumn, startEditing)) =
  {#call tree_view_set_cursor#} (toTreeView tv) tp
    focusColumn (fromBool startEditing)

#if GTK_CHECK_VERSION(2,2,0)
-- @method treeViewSetCursorOnCell@ Selects a cell in a specific row.
--
-- * Similar to @ref method treeViewSetCursor@ but allows a column to
--   containt several @ref data CellRenderer@s.
--
-- * Only available in Gtk 2.2 and higher.
--
treeViewSetCursorOnCell :: TreeViewClass tv => tv -> TreePath ->
					 TreeViewColumn ->
					 CellRenderer ->
					 Bool -> IO ()
treeViewSetCursorOnCell tv tp focusColumn focusCell startEditing =
  {#call tree_view_set_cursor_on_cell#} (toTreeView tv) tp
    focusColumn focusCell (fromBool startEditing)
#endif

-- @method treeViewGetCursor@ Retrieves the position of the focus.
--
-- * Returns a pair @literal (path, column)@.If the cursor is not currently
--   set, @literal path@ will be @literal Nothing@. If no column is currently
--   selected, @literal column@ will be @literal Nothing@.
--
treeViewGetCursor :: TreeViewClass tv => tv -> 
		     IO (Maybe TreePath, Maybe TreeViewColumn)
treeViewGetCursor tv = alloca $ \tpPtrPtr -> alloca $ \tvcPtrPtr -> do
  {#call unsafe tree_view_get_cursor#} (toTreeView tv)
    (castPtr tpPtrPtr) (castPtr tvcPtrPtr)
  tpPtr <- peek tpPtrPtr
  tvcPtr <- peek tvcPtrPtr
  tp <- if tpPtr==nullPtr then return Nothing else liftM (Just . TreePath) $ 
    newForeignPtr tpPtr (tree_path_free tpPtr)
  tvc <- if tvcPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkTreeViewColumn (return tvcPtr)
  return (tp,tvc)

foreign import ccall "gtk_tree_path_free" unsafe
  tree_path_free :: Ptr TreePath -> IO ()
  
-- @method treeViewRowActivated@ Emit the activated signal on a cell.
--
treeViewRowActivated :: TreeViewClass tv => tv -> TreePath -> 
					    TreeViewColumn -> IO ()
treeViewRowActivated tv tp tvc = 
  {#call tree_view_row_activated#} (toTreeView tv) tp tvc

-- @method treeViewExpandAll@ Expand all nodes in the @ref data TreeView@.
--
treeViewExpandAll :: TreeViewClass tv => tv -> IO ()
treeViewExpandAll tv = {#call tree_view_expand_all#} (toTreeView tv)

-- @method treeViewCollapseAll@ Collapse all nodes in the @ref data TreeView@.
--
treeViewCollapseAll :: TreeViewClass tv => tv -> IO ()
treeViewCollapseAll tv =
  {#call tree_view_collapse_all#} (toTreeView tv)

#if GTK_CHECK_VERSION(2,2,0)
-- @method treeViewExpandToPath@ Make a certain path visible.
--
-- * This will expand all parent rows of @ref arg tp@ as necessary.
--
-- * Only available in Gtk 2.2 and higher.
--
treeViewExpandToPath :: TreeViewClass tv => tv -> TreePath -> IO ()
treeViewExpandToPath tv tp =
  {#call tree_view_expand_to_path#} (toTreeView tv) tp
#endif

-- @method treeViewExpandRow@ Expand a row.
--
-- * Expand a node that is specified by 
-- @ref arg path@. If the @ref arg all@ is @literal True@ every
-- child will be expanded recursively. Returns @literal True@ if the row 
-- existed and had children.
--
treeViewExpandRow :: TreeViewClass tv => TreePath -> Bool -> tv -> IO Bool
treeViewExpandRow path all tv = liftM toBool $
  {#call tree_view_expand_row#} (toTreeView tv) path (fromBool all)

-- @method treeViewCollapseRow@ Collapse a row. Returns @literal True@ if the
-- row existed.
--
treeViewCollapseRow :: TreeViewClass tv => tv -> TreePath -> IO Bool
treeViewCollapseRow tv path = liftM toBool $
  {#call tree_view_collapse_row#} (toTreeView tv) path

-- @method treeViewMapExpandedRows@ Call function for every expaned row.
--
treeViewMapExpandedRows :: TreeViewClass tv => tv -> (TreePath -> IO ()) ->
					       IO ()
treeViewMapExpandedRows tv func = do
  fPtr <- mkTreeViewMappingFunc $ \_ tpPtr _ -> do
    tp <- liftM TreePath $ newForeignPtr tpPtr (tree_path_free tpPtr)
    func tp
  {#call tree_view_map_expanded_rows#} (toTreeView tv) fPtr nullPtr
  freeHaskellFunPtr fPtr

{#pointer TreeViewMappingFunc#}

foreign export dynamic mkTreeViewMappingFunc ::
  (Ptr TreeView -> Ptr TreePath -> Ptr () -> IO ()) ->
  IO TreeViewMappingFunc

-- @method treeViewRowExpanded@ Check if row is expanded.
--
treeViewRowExpanded :: TreeViewClass tv => tv -> TreePath -> IO Bool
treeViewRowExpanded tv tp = liftM toBool $
  {#call unsafe tree_view_row_expanded#} (toTreeView tv) tp

-- @method treeViewGetReorderable@ Query if rows can be moved around.
--
-- * See @ref method treeViewSetReorderable@.
--
treeViewGetReorderable :: TreeViewClass tv => tv -> IO Bool
treeViewGetReorderable tv = liftM toBool $
  {#call unsafe tree_view_get_reorderable#} (toTreeView tv)

-- @method treeViewSetReorderable@ Check if rows can be moved around.
--
-- * Set whether the user can use drag and drop (DND) to reorder the
--   rows in the store. This works on both @ref data TreeStore@ and
--   @ref data ListStore@ models. If @ref arg ro@ is @literal True@, then the
--   user can reorder the model by dragging and dropping rows.  The
--   developer can listen to these changes by connecting to the model's
--   signals.  This function does not give you any degree of control over
--   the order -- any reorderering is allowed.  If more control is needed,
--   you should probably handle drag and drop manually.
--
treeViewSetReorderable :: TreeViewClass tv => tv -> Bool -> IO ()
treeViewSetReorderable tv ro = {#call tree_view_set_reorderable#}
  (toTreeView tv) (fromBool ro)

-- @method treeViewGetPathAtPos@ Map a pixel to the specific cell.
--
-- * Finds the path at the @ref type Point@ @ref arg (x, y)@. The
--   coordinates @literal x@ and @literal y@ are relative to the top left
--   corner of the @ref data TreeView@ drawing window. As such, coordinates
--   in a mouse click event can be used directly to determine the cell
--   which the user clicked on. This is therefore a way to realize for
--   popup menus.
--
-- * The returned point is the input point relative to the cell's upper
--   left corner. The whole @ref data TreeView@ is divided between all cells.
--   The returned point is relative to the rectangle this cell occupies
--   within the @ref data TreeView@.
--
treeViewGetPathAtPos :: TreeViewClass tv => tv -> Point ->
			IO (Maybe (TreePath, TreeViewColumn, Point))
treeViewGetPathAtPos tv (x,y) = alloca $ \tpPtrPtr -> alloca $ \tvcPtrPtr ->
  alloca $ \xPtr -> alloca $ \yPtr -> do
    res <- liftM toBool $ {#call unsafe tree_view_get_path_at_pos#}
      (toTreeView tv) (fromIntegral x) (fromIntegral y) (castPtr tpPtrPtr)
      (castPtr tvcPtrPtr) xPtr yPtr
    tpPtr <- peek tpPtrPtr
    tvcPtr <- peek tvcPtrPtr
    xCell <- peek xPtr
    yCell <- peek yPtr
    if not res then return Nothing else do
      tp <- liftM TreePath $ newForeignPtr tpPtr (tree_path_free tpPtr)
      tvc <- makeNewObject mkTreeViewColumn (return tvcPtr)
      return (Just (tp,tvc,(fromIntegral xCell, fromIntegral yCell)))

-- @method treeViewGetCellArea@ Retrieve the smallest bounding box of a cell.
--
-- * Fills the bounding rectangle in tree window coordinates for the
--   cell at the row specified by @ref arg tp and the column specified by
--   @ref arg tvc@.
--   If @ref arg path@ is @literal Nothing@ or points to a path not
--   currently displayed, the @literal y@ and @literal height@ fields of
--   the @ref data Rectangle@ will be filled with @literal 0@. The sum of
--   all cell rectangles does not cover the entire tree; there are extra
--   pixels in between rows, for example.
--
treeViewGetCellArea :: TreeViewClass tv => tv -> Maybe TreePath -> 
					   TreeViewColumn -> IO Rectangle
treeViewGetCellArea tv Nothing tvc = alloca $ \rPtr ->
  {#call unsafe tree_view_get_background_area#} (toTreeView tv)
    (TreePath nullForeignPtr) tvc (castPtr (rPtr :: Ptr Rectangle))
    >> peek rPtr
treeViewGetCellArea tv (Just tp) tvc = alloca $ \rPtr -> do
  {#call unsafe tree_view_get_background_area#} (toTreeView tv) tp
    tvc (castPtr (rPtr :: Ptr Rectangle)) >> peek rPtr

-- @method treeViewGetBackgroundArea@ Retrieve the largest bounding box 
-- of a cell.
--
-- * Fills the bounding rectangle in tree window coordinates for the
--   cell at the row specified by @ref arg tp and the column specified by
--   @ref arg tvc@.
--   If @ref arg path@ is @literal Nothing@ or points to a path not
--   currently displayed, the @literal y@ and @literal height@ fields of
--   the @ref data Rectangle@ will be filled with @literal 0@. The background
--   areas tile the widget's area to cover the entire tree window 
--   (except for the area used for header buttons). Contrast this with
--   @ref method treeViewGetCellArea@.
--
treeViewGetBackgroundArea :: TreeViewClass tv => tv -> Maybe TreePath -> 
					   TreeViewColumn -> IO Rectangle
treeViewGetBackgroundArea tv Nothing tvc = alloca $ \rPtr ->
  {#call unsafe tree_view_get_background_area#} (toTreeView tv)
    (TreePath nullForeignPtr) tvc (castPtr (rPtr :: Ptr Rectangle))
    >> peek rPtr
treeViewGetBackgroundArea tv (Just tp) tvc = alloca $ \rPtr -> do
  {#call unsafe tree_view_get_background_area#} (toTreeView tv) tp
    tvc (castPtr (rPtr :: Ptr Rectangle)) >> peek rPtr

-- @method treeViewGetVisibleRect@ Retrieve the currently visible area.
--
-- * The returned rectangle gives the visible part of the tree in tree
--   coordinates.
--
treeViewGetVisibleRect :: TreeViewClass tv => tv -> IO Rectangle
treeViewGetVisibleRect tv = alloca $ \rPtr -> do
  {#call unsafe tree_view_get_visible_rect#} (toTreeView tv)
    (castPtr (rPtr :: Ptr Rectangle))
  peek rPtr

-- @method treeViewWidgetToTreeCoords@ Convert widget to tree pixel
-- coordinates.
--
-- * See module description.
--
treeViewWidgetToTreeCoords :: TreeViewClass tv => tv -> Point -> IO Point
treeViewWidgetToTreeCoords tv (x,y) = alloca $ \xPtr -> alloca $ \yPtr -> do
  {#call unsafe tree_view_tree_to_widget_coords#} (toTreeView tv)
    (fromIntegral x) (fromIntegral y) xPtr yPtr
  x' <- peek xPtr
  y' <- peek yPtr
  return (fromIntegral x', fromIntegral y')

-- @method treeViewTreeToWidgetCoords@ Convert tree to widget pixel
-- coordinates.
--
-- * See module description.
--
treeViewTreeToWidgetCoords :: TreeViewClass tv => tv -> Point -> IO Point
treeViewTreeToWidgetCoords tv (x,y) = alloca $ \xPtr -> alloca $ \yPtr -> do
  {#call unsafe tree_view_widget_to_tree_coords#} (toTreeView tv)
    (fromIntegral x) (fromIntegral y) xPtr yPtr
  x' <- peek xPtr
  y' <- peek yPtr
  return (fromIntegral x', fromIntegral y')

-- @method treeViewGetEnableSearch@ Set if user can search entries.
--
-- * If enabled, the user can type in text which will set the cursor to
--   the first matching entry.
--
treeViewGetEnableSearch :: TreeViewClass tv => tv -> IO Bool
treeViewGetEnableSearch tv = liftM toBool $
  {#call unsafe tree_view_get_enable_search#} (toTreeView tv)

-- @method treeViewSetEnableSearch@ Check if user can search entries.
--
treeViewSetEnableSearch :: TreeViewClass tv => tv -> Bool -> IO ()
treeViewSetEnableSearch tv es = {#call tree_view_set_enable_search#}
  (toTreeView tv) (fromBool es)

-- @method treeViewGetSearchColumn@ Gets the column searched on by the
-- interactive search.
--
treeViewGetSearchColumn :: TreeViewClass tv => tv -> IO Int
treeViewGetSearchColumn tv = liftM fromIntegral $
  {#call unsafe tree_view_get_search_column#} (toTreeView tv)

-- @method treeViewSetSearchColumn@ Set the column searched on by
-- by the interactive search.
--
-- * Additionally, turns on interactive searching.
--
treeViewSetSearchColumn :: TreeViewClass tv => tv -> Int -> IO ()
treeViewSetSearchColumn tv sc = {#call tree_view_set_search_column#}
  (toTreeView tv) (fromIntegral sc)

-- @method treeViewSetSearchEqualFunc@ Set the predicate to test for equality.
--
-- * The default function assumes that the column @ref arg col@ has contains
--   @ref data Attribute@ @literal cr@ @literal String@. It conducts a
--   case insensitive comparison of the text typed by the user and the
--   text in the tree model. This function can be used to override this 
--   behaviour. The predicate returns @literal True@ if the entries should
--   be considered to match. The parameters are the column number, the text
--   the user typed in and a @ref data TreeIter@ which points to the cell
--   to be compared.
--
treeViewSetSearchEqualFunc :: TreeViewClass tv => tv ->
			      (Int -> String -> TreeIter -> IO Bool) ->
			      IO ()
treeViewSetSearchEqualFunc tv pred = do
  fPtr <- mkTreeViewSearchEqualFunc (\_ col keyPtr itPtr _ -> do
    key <- peekCString keyPtr
    iter <- createTreeIter itPtr
    liftM fromBool $ pred (fromIntegral col) key iter)
  dRef <- newIORef nullFunPtr
  dPtr <- mkDestructor $ do
    dPtr <- readIORef dRef
    freeHaskellFunPtr dPtr
    freeHaskellFunPtr fPtr
  writeIORef dRef dPtr
  {#call tree_view_set_search_equal_func#} (toTreeView tv) fPtr 
    nullPtr dPtr

{#pointer TreeViewSearchEqualFunc#}

foreign export dynamic mkTreeViewSearchEqualFunc ::
  (Ptr TreeModel -> {#type gint#} -> CString -> Ptr TreeIter -> Ptr () ->
   IO {#type gboolean#}) -> IO TreeViewSearchEqualFunc


-- @signal connectToColumnsChanged@ The user has dragged a column to another
-- position.
--
onColumnsChanged, afterColumnsChanged :: TreeViewClass tv => tv -> IO () ->
					 IO (ConnectId tv)
onColumnsChanged = connect_NONE__NONE "columns_changed" False
afterColumnsChanged = connect_NONE__NONE "columns_changed" True

-- @signal connectToCursorChanged@ The cursor in the tree has moved.
--
onCursorChanged, afterCursorChanged :: TreeViewClass tv => tv -> IO () ->
				       IO (ConnectId tv)
onCursorChanged = connect_NONE__NONE "cursor_changed" False
afterCursorChanged = connect_NONE__NONE "cursor_changed" True

-- @signal connectToRowActivated@ A row was activated.
--
-- * Activation usually means the user has pressed return on a row.
--
onRowActivated, afterRowActivated :: TreeViewClass tv => tv ->
				     (TreePath -> TreeViewColumn -> IO ()) ->
				     IO (ConnectId tv)
onRowActivated = connect_BOXED_OBJECT__NONE "row_activated" 
		   createTreePath False
afterRowActivated = connect_BOXED_OBJECT__NONE "row_activated" 
		      createTreePath True

-- @signal connectToRowCollapsed@ Children of this node were hidden.
--
onRowCollapsed, afterRowCollapsed :: TreeViewClass tv => tv ->
				     (TreeIter -> TreePath -> IO ()) ->
				     IO (ConnectId tv)
onRowCollapsed = connect_BOXED_BOXED__NONE "row_collapsed"
		   createTreeIter createTreePath False
afterRowCollapsed = connect_BOXED_BOXED__NONE "row_collapsed"
		      createTreeIter createTreePath True

-- @signal connectToRowExpanded@ Children of this node are made visible.
--
onRowExpanded, afterRowExpanded :: TreeViewClass tv => tv ->
				     (TreeIter -> TreePath -> IO ()) ->
				     IO (ConnectId tv)
onRowExpanded = connect_BOXED_BOXED__NONE "row_expanded"
		   createTreeIter createTreePath False
afterRowExpanded = connect_BOXED_BOXED__NONE "row_expanded"
		      createTreeIter createTreePath True

-- @signal connectToStartInteractiveSearch@ The user wants to search 
-- interactively.
--
-- * Connect to this signal if you want to provide you own search facility.
--   Note that you must handle all keyboard input yourself.
--
onStartInteractiveSearch, afterStartInteractiveSearch :: 
  TreeViewClass tv => tv -> IO () -> IO (ConnectId tv)

#if GTK_CHECK_VERSION(2,2,0)

onStartInteractiveSearch tv fun =
  connect_NONE__BOOL "start_interactive_search" False tv (fun >> return True)
afterStartInteractiveSearch tv fun =
  connect_NONE__BOOL "start_interactive_search" True tv (fun >> return True)

#else

onStartInteractiveSearch =
  connect_NONE__NONE "start_interactive_search" False
afterStartInteractiveSearch = 
  connect_NONE__NONE "start_interactive_search" True

#endif

-- @signal connectToTestCollapseRow@ Determine if this row should be collapsed.
--
-- * If the application connects to this function and returns @literal False@,
--   the specifc row will not be altered.
--
onTestCollapseRow, afterTestCollapseRow :: TreeViewClass tv => tv ->
					   (TreeIter -> TreePath -> IO ()) ->
					   IO (ConnectId tv)
onTestCollapseRow = connect_BOXED_BOXED__NONE "test_collapse_row"
		   createTreeIter createTreePath False
afterTestCollapseRow = connect_BOXED_BOXED__NONE "test_collapse_row"
		      createTreeIter createTreePath True

-- @signal connectToTestExpandRow@ Determine if this row should be expanded.
--
-- * If the application connects to this function and returns @literal False@,
--   the specifc row will not be altered.
--
onTestExpandRow, afterTestExpandRow :: TreeViewClass tv => tv ->
					   (TreeIter -> TreePath -> IO ()) ->
					   IO (ConnectId tv)
onTestExpandRow = connect_BOXED_BOXED__NONE "test_expand_row"
		   createTreeIter createTreePath False
afterTestExpandRow = connect_BOXED_BOXED__NONE "test_expand_row"
		      createTreeIter createTreePath True

