-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget TreeView
--
--  Author : Axel Simon
--
--  Created: 9 May 2001
--
--  Version $Revision: 1.6 $ from $Date: 2005/02/25 22:53:42 $
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
-- TODO
--
-- treeViewMoveColumnAfter and treeViewMoveColumnFirst are not dealt with in
--   Mogul
--
-- gtk_tree_view_get_bin_window is to compare the GDK window from incoming
--   events. We don't marshal that window parameter, so this function is not
--   bound either.
--
-- All functions related to drag and drop are missing.
--
-- get_search_equal_func is missing: proper memory management is impossible
--
-- gtk_tree_view_set_destroy_count_func is not meant to be useful
--
-- expand-collapse-cursor-row needs to be bound if it is useful to expand
--   and collapse rows in a user-defined manner. Would only work on Gtk 2.2
--   and higher since the return parameter changed
--
-- move_cursor, select_all, select_cursor_parent, select_cursor_row
--   toggle_cursor_row, unselect_all are not bound.
--   These functions are only useful to change the widgets
--   behaviour for these actions. Everything else can be done with
--   cursor_changed and columns_changed
--
-- set_scroll_adjustment makes sense if the user monitors the scroll bars
--   and the scroll bars can be replaced anytime (the latter is odd)
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A widget for displaying both trees and lists.
--
module Graphics.UI.Gtk.TreeList.TreeView (
-- * Description
-- 
-- | Widget that displays any object that implements the 'TreeModel'
-- interface.
--
-- The widget supports scrolling natively. This implies that pixel 
-- coordinates can be given in two formats: relative to the current view's
-- upper left corner or relative to the whole list's coordinates. The former
-- are called widget coordinates while the letter are called tree 
-- coordinates.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----TreeView
-- @

-- * Types
  TreeView,
  TreeViewClass,
  castToTreeView,
  Point,

-- * Constructors
  treeViewNew,
  treeViewNewWithModel,

-- * Methods
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
  treeViewGetPathAtPos,
  treeViewGetCellArea,
  treeViewGetBackgroundArea,
  treeViewGetVisibleRect,
  treeViewWidgetToTreeCoords,
  treeViewTreeToWidgetCoords,

  treeViewCreateRowDragIcon,

  treeViewGetEnableSearch,
  treeViewSetEnableSearch,
  treeViewGetSearchColumn,
  treeViewSetSearchColumn,
  treeViewSetSearchEqualFunc,

-- * Signals
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
import Data.IORef (newIORef, readIORef, writeIORef)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.General.General	(mkDestructor)
import Graphics.UI.Gtk.General.Structs	(Point, Rectangle)
import System.Glib.GObject		(makeNewGObject)
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import System.Glib.GList		(GList, fromGList)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.TreeList.TreeModel#}
{#import Graphics.UI.Gtk.TreeList.TreeViewColumn#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Make a new 'TreeView' widget.
--
treeViewNew :: IO TreeView
treeViewNew  = makeNewObject mkTreeView (liftM castPtr {#call tree_view_new#})

-- | Create a new 'TreeView' 
-- widget with @tm@ as the storage model.
--
treeViewNewWithModel :: TreeModelClass tm => tm -> IO TreeView
treeViewNewWithModel tm = makeNewObject mkTreeView $ liftM castPtr $
  {#call tree_view_new_with_model#} (toTreeModel tm)

--------------------
-- Methods

-- | Retrieve the TreeModel that supplies the data for
-- this 'TreeView'. Returns Nothing if no model is currently set.
--
treeViewGetModel :: TreeViewClass tv => tv -> IO (Maybe TreeModel)
treeViewGetModel tv = do
  tmPtr <- {#call unsafe tree_view_get_model#} (toTreeView tv)
  if tmPtr==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkTreeModel (return tmPtr)

-- | Set the 'TreeModel' for the current View.
--
treeViewSetModel :: (TreeViewClass tv, TreeModelClass tm) => tv -> tm -> IO ()
treeViewSetModel tv tm =
  {#call tree_view_set_model#} (toTreeView tv) (toTreeModel tm)

-- | Retrieve a 'TreeSelection' that
-- holds the current selected nodes of the View.
--
treeViewGetSelection :: TreeViewClass tv => tv -> IO TreeSelection
treeViewGetSelection tv = makeNewGObject mkTreeSelection $
  {#call unsafe tree_view_get_selection#} (toTreeView tv)

-- | Get the 'Adjustment' that
-- represents the horizontal aspect.
--
treeViewGetHadjustment :: TreeViewClass tv => tv -> IO (Maybe Adjustment)
treeViewGetHadjustment tv = do
  adjPtr <- {#call unsafe tree_view_get_hadjustment#} (toTreeView tv)
  if adjPtr==nullPtr then return Nothing else do
    liftM Just $ makeNewObject mkAdjustment (return adjPtr)

-- | Set the 'Adjustment' that controls
-- the horizontal aspect. If @adj@ is Nothing then set no Adjustment
-- widget.
--
treeViewSetHadjustment :: TreeViewClass tv => (Maybe Adjustment) -> tv -> IO ()
treeViewSetHadjustment adj tv = {#call tree_view_set_hadjustment#} 
  (toTreeView tv) (fromMaybe (mkAdjustment nullForeignPtr) adj)

-- | Get the 'Adjustment' that
-- represents the vertical aspect.
--
treeViewGetVadjustment :: TreeViewClass tv => tv -> IO (Maybe Adjustment)
treeViewGetVadjustment tv = do
  adjPtr <- {#call unsafe tree_view_get_vadjustment#} (toTreeView tv)
  if adjPtr==nullPtr then return Nothing else do
    liftM Just $ makeNewObject mkAdjustment (return adjPtr)

-- | Set the 'Adjustment' that controls
-- the vertical aspect. If @adj@ is @Nothing@ then set no
-- 'Adjustment' widget.
--
treeViewSetVadjustment :: TreeViewClass tv => (Maybe Adjustment) -> tv -> IO ()
treeViewSetVadjustment adj tv = {#call tree_view_set_vadjustment#} 
  (toTreeView tv) (fromMaybe (mkAdjustment nullForeignPtr) adj)

-- | Query if the column headers are visible.
--
treeViewGetHeadersVisible :: TreeViewClass tv => tv -> IO Bool
treeViewGetHeadersVisible tv = liftM toBool $
  {#call unsafe tree_view_get_headers_visible#} (toTreeView tv)

-- | Set the visibility state of the column
-- headers.
--
treeViewSetHeadersVisible :: TreeViewClass tv => tv -> Bool -> IO ()
treeViewSetHeadersVisible tv vis = {#call tree_view_set_headers_visible#}
  (toTreeView tv) (fromBool vis)

-- | Resize the columns to their optimal size.
--
treeViewColumnsAutosize :: TreeViewClass tv => tv -> IO ()
treeViewColumnsAutosize tv =
  {#call tree_view_columns_autosize#} (toTreeView tv)

-- | Set wether the columns headers are
-- sensitive to mouse clicks.
--
treeViewSetHeadersClickable :: TreeViewClass tv => tv -> Bool -> IO ()
treeViewSetHeadersClickable tv click = {#call tree_view_set_headers_clickable#}
  (toTreeView tv) (fromBool click)

-- | Give visual aid for wide columns.
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

-- | Query if visual aid for wide columns is
-- turned on.
--
treeViewSetRulesHint :: TreeViewClass tv => tv -> Bool -> IO ()
treeViewSetRulesHint tv vis = {#call tree_view_set_rules_hint#}
  (toTreeView tv) (fromBool vis)

-- | Append a new column to the 'TreeView'. Returns
-- the new number of columns.
--
treeViewAppendColumn :: TreeViewClass tv => tv -> TreeViewColumn -> IO Int
treeViewAppendColumn tv tvc = liftM fromIntegral $
  {#call tree_view_append_column#} (toTreeView tv) tvc

-- | Remove column @tvc@ from the 'TreeView'
-- widget. The number of remaining columns is returned.
--
treeViewRemoveColumn :: TreeViewClass tv => tv -> TreeViewColumn -> IO Int
treeViewRemoveColumn tv tvc = liftM fromIntegral $
  {#call tree_view_remove_column#} (toTreeView tv) tvc

-- | Inserts column @tvc@ into the
-- 'TreeView' widget at the position @pos@. Returns the number of
-- columns after insertion. Specify -1 for @pos@ to insert the column
-- at the end.
--
treeViewInsertColumn :: TreeViewClass tv => tv -> TreeViewColumn -> Int ->
                        IO Int
treeViewInsertColumn tv tvc pos = liftM fromIntegral $ 
  {#call tree_view_insert_column#} (toTreeView tv) tvc (fromIntegral pos)


-- | Insert new
-- 'TreeViewColumn'.
--
-- * Inserts new column into the
-- 'TreeView' @tv@ at position @pos@ with title
-- ref argtitle, cell renderer @cr@ and attributes
-- @attribs@. Specify -1 for @pos@ to insert the column at
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

-- | Retrieve a 'TreeViewColumn'.
--
-- * Retrieve the @pos@ th columns of
--   'TreeView'. If the index is out of range Nothing is returned.
--
treeViewGetColumn :: TreeViewClass tv => tv -> Int -> IO (Maybe TreeViewColumn)
treeViewGetColumn tv pos = do
  tvcPtr <- {#call unsafe tree_view_get_column#} (toTreeView tv) 
    (fromIntegral pos)
  if tvcPtr==nullPtr then return Nothing else 
    liftM Just $ makeNewObject mkTreeViewColumn (return tvcPtr)


-- | Return all 'TreeViewColumn's in this
-- 'TreeView'.
--
treeViewGetColumns :: TreeViewClass tv => tv -> IO [TreeViewColumn]
treeViewGetColumns tv = do
  colsList <- {#call unsafe tree_view_get_columns#} (toTreeView tv)
  colsPtr <- fromGList colsList
  mapM (makeNewObject mkTreeViewColumn) (map return colsPtr)

-- | Move a specific column.
--
-- * Use 'treeViewMoveColumnToFront' if you want to move the column
--   to the left end of the 'TreeView'.
--
treeViewMoveColumnAfter :: TreeViewClass tv => tv -> TreeViewColumn ->
					       TreeViewColumn -> IO ()
treeViewMoveColumnAfter tv which after = {#call tree_view_move_column_after#}
  (toTreeView tv) which after

-- | Move a specific column.
--
-- * Use 'treeViewMoveColumnAfter' if you want to move the column
--   somewhere else than to the leftmost position.
--
treeViewMoveColumnFirst :: TreeViewClass tv => tv -> TreeViewColumn -> IO ()
treeViewMoveColumnFirst tv which = {#call tree_view_move_column_after#}
  (toTreeView tv) which (mkTreeViewColumn nullForeignPtr)

-- | Set location of hierarchy controls.
--
-- * Sets the column to draw the expander arrow at. If @col@
--   is @Nothing@, then the expander arrow is always at the first
--   visible column.
--
treeViewSetExpanderColumn :: TreeViewClass tv => tv -> Maybe TreeViewColumn ->
						 IO ()
treeViewSetExpanderColumn tv (Just tvc) =
  {#call unsafe tree_view_set_expander_column#} (toTreeView tv) tvc
treeViewSetExpanderColumn tv Nothing =
  {#call unsafe tree_view_set_expander_column#} (toTreeView tv)
    (mkTreeViewColumn nullForeignPtr)

-- | Get location of hierarchy controls.
--
-- * Gets the column to draw the expander arrow at. If @col@
--   is @Nothing@, then the expander arrow is always at the first
--   visible column.
--
treeViewGetExpanderColumn :: TreeViewClass tv => tv -> IO TreeViewColumn
treeViewGetExpanderColumn tv = makeNewObject mkTreeViewColumn $
  {#call unsafe tree_view_get_expander_column#} (toTreeView tv)

-- | Specify where a column may be
-- dropped.
--
-- * Sets a user function for determining where a column may be dropped when
--   dragged.  This function is called on every column pair in turn at the
--   beginning of a column drag to determine where a drop can take place.
-- * The callback function take the 'TreeViewColumn' to be moved, the
--   second and third arguments are the columns on the left and right side
--   of the new location. At most one of them might be @Nothing@
--   which indicates that the column is about to be dropped at the left or
--   right end of the 'TreeView'.
-- * The predicate @pred@ should return @True@ if it is ok
--   to insert the column at this place.
-- * Use @Nothing@ for the predicate if columns can be inserted
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

foreign import ccall "wrapper" mkTreeViewColumnDropFunc ::
  (Ptr () -> Ptr TreeViewColumn -> Ptr TreeViewColumn -> Ptr TreeViewColumn ->
  Ptr () -> IO {#type gboolean#}) -> IO TreeViewColumnDropFunc

-- | Scroll to a coordinate.
--
-- * Scrolls the tree view such that the top-left corner of the
--   visible area is @treeX@, @treeY@, where @treeX@
--   and @treeY@ are specified in tree window coordinates.
--   The 'TreeView' must be realized before this function is
--   called.  If it isn't, you probably want to use
--   'treeViewScrollToCell'.
--
treeViewScrollToPoint :: TreeViewClass tv => tv -> Int -> Int -> IO ()
treeViewScrollToPoint tv treeX treeY = 
  {#call tree_view_scroll_to_point#} (toTreeView tv)
    (fromIntegral treeX) (fromIntegral treeY)

-- | Scroll to a cell.
--
-- * Scroll to a cell as specified by @path@ and @tvc@. 
--   The cell is aligned within the 'TreeView' widget as
--   follows: horizontally by @hor@ from left (@0.0@) to
--   right (@1.0@) and vertically by @ver@ from top
--   (@0.0@) to buttom (@1.0@).
--
treeViewScrollToCell :: TreeViewClass tv => tv -> TreePath -> TreeViewColumn ->
                        Maybe (Float,Float) -> IO ()
treeViewScrollToCell tv path tvc (Just (ver,hor)) =
  withTreePath path $ \path ->
  {#call tree_view_scroll_to_cell#} 
  (toTreeView tv) path tvc 1 (realToFrac ver) (realToFrac hor)
treeViewScrollToCell tv path tvc Nothing = 
  withTreePath path $ \path ->
  {#call tree_view_scroll_to_cell#} 
  (toTreeView tv) path tvc 0 0.0 0.0


-- | Selects a specific row.
--
-- * Sets the current keyboard focus to be at @path@, and
--   selects it.  This is useful when you want to focus the user\'s
--   attention on a particular row.  If @focusColumn@ is given,
--   then the input focus is given to the column specified by
--   it. Additionally, if @focusColumn@ is specified, and 
--   @startEditing@ is @True@,
--   then editing will be started in the
--   specified cell.  This function is often followed by a
--   'widgetGrabFocus' to the 'TreeView' in order
--   to give keyboard focus to the widget.
--
treeViewSetCursor :: TreeViewClass tv => tv -> TreePath ->
					 (Maybe (TreeViewColumn, Bool)) ->
					 IO ()
treeViewSetCursor tv tp Nothing = withTreePath tp $ \tp ->
  {#call tree_view_set_cursor#} (toTreeView tv) tp
    (mkTreeViewColumn nullForeignPtr) (fromBool False)
treeViewSetCursor tv tp (Just (focusColumn, startEditing)) =
  withTreePath tp $ \tp ->
  {#call tree_view_set_cursor#} (toTreeView tv) tp
    focusColumn (fromBool startEditing)

#if GTK_CHECK_VERSION(2,2,0)
-- | Selects a cell in a specific row.
--
-- * Similar to 'treeViewSetCursor' but allows a column to
--   containt several 'CellRenderer's.
--
-- * Only available in Gtk 2.2 and higher.
--
treeViewSetCursorOnCell :: TreeViewClass tv => tv -> TreePath ->
					 TreeViewColumn ->
					 CellRenderer ->
					 Bool -> IO ()
treeViewSetCursorOnCell tv tp focusColumn focusCell startEditing =
  withTreePath tp $ \tp ->
  {#call tree_view_set_cursor_on_cell#} (toTreeView tv) tp
    focusColumn focusCell (fromBool startEditing)
#endif

-- | Retrieves the position of the focus.
--
-- * Returns a pair @(path, column)@.If the cursor is not currently
--   set, @path@ will be @[]@. If no column is currently
--   selected, @column@ will be @Nothing@.
--
treeViewGetCursor :: TreeViewClass tv => tv -> 
		     IO (TreePath, Maybe TreeViewColumn)
treeViewGetCursor tv = alloca $ \tpPtrPtr -> alloca $ \tvcPtrPtr -> do
  {#call unsafe tree_view_get_cursor#} (toTreeView tv)
    (castPtr tpPtrPtr) (castPtr tvcPtrPtr)
  tpPtr <- peek tpPtrPtr
  tvcPtr <- peek tvcPtrPtr
  tp <- fromTreePath tpPtr
  tvc <- if tvcPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkTreeViewColumn (return tvcPtr)
  return (tp,tvc)

-- | Emit the activated signal on a cell.
--
treeViewRowActivated :: TreeViewClass tv => tv -> TreePath -> 
					    TreeViewColumn -> IO ()
treeViewRowActivated tv tp tvc = withTreePath tp $ \tp ->
  {#call tree_view_row_activated#} (toTreeView tv) tp tvc

-- | Expand all nodes in the 'TreeView'.
--
treeViewExpandAll :: TreeViewClass tv => tv -> IO ()
treeViewExpandAll tv = {#call tree_view_expand_all#} (toTreeView tv)

-- | Collapse all nodes in the 'TreeView'.
--
treeViewCollapseAll :: TreeViewClass tv => tv -> IO ()
treeViewCollapseAll tv =
  {#call tree_view_collapse_all#} (toTreeView tv)

#if GTK_CHECK_VERSION(2,2,0)
-- | Make a certain path visible.
--
-- * This will expand all parent rows of @tp@ as necessary.
--
-- * Only available in Gtk 2.2 and higher.
--
treeViewExpandToPath :: TreeViewClass tv => tv -> TreePath -> IO ()
treeViewExpandToPath tv tp = withTreePath tp $ \tp ->
  {#call tree_view_expand_to_path#} (toTreeView tv) tp
#endif

-- | Expand a row.
--
-- * Expand a node that is specified by 
-- @path@. If the @all@ is @True@ every
-- child will be expanded recursively. Returns @True@ if the row 
-- existed and had children.
--
treeViewExpandRow :: TreeViewClass tv => TreePath -> Bool -> tv -> IO Bool
treeViewExpandRow tp all tv = withTreePath tp $ \tp -> liftM toBool $
  {#call tree_view_expand_row#} (toTreeView tv) tp (fromBool all)

-- | Collapse a row. Returns @True@ if the
-- row existed.
--
treeViewCollapseRow :: TreeViewClass tv => tv -> TreePath -> IO Bool
treeViewCollapseRow tv tp = withTreePath tp $ \tp -> liftM toBool $
  {#call tree_view_collapse_row#} (toTreeView tv) tp

-- | Call function for every expaned row.
--
treeViewMapExpandedRows :: TreeViewClass tv => tv -> (TreePath -> IO ()) ->
					       IO ()
treeViewMapExpandedRows tv func = do
  fPtr <- mkTreeViewMappingFunc $ \_ tpPtr _ -> fromTreePath tpPtr >>= func
  {#call tree_view_map_expanded_rows#} (toTreeView tv) fPtr nullPtr
  freeHaskellFunPtr fPtr

{#pointer TreeViewMappingFunc#}

foreign import ccall "wrapper" mkTreeViewMappingFunc ::
  (Ptr () -> Ptr NativeTreePath -> Ptr () -> IO ()) ->
  IO TreeViewMappingFunc

-- | Check if row is expanded.
--
treeViewRowExpanded :: TreeViewClass tv => tv -> TreePath -> IO Bool
treeViewRowExpanded tv tp = withTreePath tp $ \tp -> liftM toBool $
  {#call unsafe tree_view_row_expanded#} (toTreeView tv) tp

-- | Query if rows can be moved around.
--
-- * See 'treeViewSetReorderable'.
--
treeViewGetReorderable :: TreeViewClass tv => tv -> IO Bool
treeViewGetReorderable tv = liftM toBool $
  {#call unsafe tree_view_get_reorderable#} (toTreeView tv)

-- | Check if rows can be moved around.
--
-- * Set whether the user can use drag and drop (DND) to reorder the
--   rows in the store. This works on both 'TreeStore' and
--   'ListStore' models. If @ro@ is @True@, then the
--   user can reorder the model by dragging and dropping rows.  The
--   developer can listen to these changes by connecting to the model's
--   signals.  This function does not give you any degree of control over
--   the order -- any reorderering is allowed.  If more control is needed,
--   you should probably handle drag and drop manually.
--
treeViewSetReorderable :: TreeViewClass tv => tv -> Bool -> IO ()
treeViewSetReorderable tv ro = {#call tree_view_set_reorderable#}
  (toTreeView tv) (fromBool ro)

-- | Map a pixel to the specific cell.
--
-- * Finds the path at the 'Point' @(x, y)@. The
--   coordinates @x@ and @y@ are relative to the top left
--   corner of the 'TreeView' drawing window. As such, coordinates
--   in a mouse click event can be used directly to determine the cell
--   which the user clicked on. This is therefore a way to realize for
--   popup menus.
--
-- * The returned point is the input point relative to the cell's upper
--   left corner. The whole 'TreeView' is divided between all cells.
--   The returned point is relative to the rectangle this cell occupies
--   within the 'TreeView'.
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
      tp <- fromTreePath tpPtr
      tvc <- makeNewObject mkTreeViewColumn (return tvcPtr)
      return (Just (tp,tvc,(fromIntegral xCell, fromIntegral yCell)))

-- | Retrieve the smallest bounding box of a cell.
--
-- * Fills the bounding rectangle in tree window coordinates for the
--   cell at the row specified by @tp@ and the column specified by
--   @tvc@.
--   If @path@ is @Nothing@ or points to a path not
--   currently displayed, the @y@ and @height@ fields of
--   the 'Rectangle' will be filled with @0@. The sum of
--   all cell rectangles does not cover the entire tree; there are extra
--   pixels in between rows, for example.
--
treeViewGetCellArea :: TreeViewClass tv => tv -> Maybe TreePath -> 
					   TreeViewColumn -> IO Rectangle
treeViewGetCellArea tv Nothing tvc = alloca $ \rPtr ->
  {#call unsafe tree_view_get_cell_area#} (toTreeView tv)
    (NativeTreePath nullPtr) tvc (castPtr (rPtr :: Ptr Rectangle))
    >> peek rPtr
treeViewGetCellArea tv (Just tp) tvc = 
  withTreePath tp $ \tp -> alloca $ \rPtr -> do
  {#call unsafe tree_view_get_cell_area#} (toTreeView tv) tp
    tvc (castPtr (rPtr :: Ptr Rectangle)) >> peek rPtr

-- | Retrieve the largest bounding box of a cell.
--
-- * Fills the bounding rectangle in tree window coordinates for the
--   cell at the row specified by @tp@ and the column specified by
--   @tvc@.
--   If @path@ is @Nothing@ or points to a path not
--   currently displayed, the @y@ and @height@ fields of
--   the 'Rectangle' will be filled with @0@. The background
--   areas tile the widget's area to cover the entire tree window 
--   (except for the area used for header buttons). Contrast this with
--   'treeViewGetCellArea'.
--
treeViewGetBackgroundArea :: TreeViewClass tv => tv -> Maybe TreePath -> 
					   TreeViewColumn -> IO Rectangle
treeViewGetBackgroundArea tv Nothing tvc = alloca $ \rPtr ->
  {#call unsafe tree_view_get_background_area#} (toTreeView tv)
    (NativeTreePath nullPtr) tvc (castPtr (rPtr :: Ptr Rectangle))
    >> peek rPtr
treeViewGetBackgroundArea tv (Just tp) tvc = 
  withTreePath tp $ \tp -> alloca $ \rPtr -> do
  {#call unsafe tree_view_get_background_area#} (toTreeView tv) tp
    tvc (castPtr (rPtr :: Ptr Rectangle)) >> peek rPtr

-- | Retrieve the currently visible area.
--
-- * The returned rectangle gives the visible part of the tree in tree
--   coordinates.
--
treeViewGetVisibleRect :: TreeViewClass tv => tv -> IO Rectangle
treeViewGetVisibleRect tv = alloca $ \rPtr -> do
  {#call unsafe tree_view_get_visible_rect#} (toTreeView tv)
    (castPtr (rPtr :: Ptr Rectangle))
  peek rPtr

-- | Convert widget to tree pixel coordinates.
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

-- | Convert tree to widget pixel coordinates.
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

-- | Creates a "Pixmap" representation of the row at the given path. This image
-- can be used for a drag icon.
--
treeViewCreateRowDragIcon :: TreeViewClass tv => tv -> TreePath -> IO Pixmap
treeViewCreateRowDragIcon tv tp = withTreePath tp $ \tp ->
  makeNewGObject mkPixmap $
  {#call unsafe tree_view_create_row_drag_icon#} (toTreeView tv) tp

-- | Set if user can search entries.
--
-- * If enabled, the user can type in text which will set the cursor to
--   the first matching entry.
--
treeViewGetEnableSearch :: TreeViewClass tv => tv -> IO Bool
treeViewGetEnableSearch tv = liftM toBool $
  {#call unsafe tree_view_get_enable_search#} (toTreeView tv)

-- | Check if user can search entries.
--
treeViewSetEnableSearch :: TreeViewClass tv => tv -> Bool -> IO ()
treeViewSetEnableSearch tv es = {#call tree_view_set_enable_search#}
  (toTreeView tv) (fromBool es)

-- | Gets the column searched on by the interactive search.
--
treeViewGetSearchColumn :: TreeViewClass tv => tv -> IO Int
treeViewGetSearchColumn tv = liftM fromIntegral $
  {#call unsafe tree_view_get_search_column#} (toTreeView tv)

-- | Set the column searched on by by the interactive search.
--
-- * Additionally, turns on interactive searching.
--
treeViewSetSearchColumn :: TreeViewClass tv => tv -> Int -> IO ()
treeViewSetSearchColumn tv sc = {#call tree_view_set_search_column#}
  (toTreeView tv) (fromIntegral sc)

-- | Set the predicate to test for equality.
--
-- * The default function assumes that the column @col@ has contains
--   'Attribute' @cr@ @String@. It conducts a
--   case insensitive comparison of the text typed by the user and the
--   text in the tree model. This function can be used to override this 
--   behaviour. The predicate returns @True@ if the entries should
--   be considered to match. The parameters are the column number, the text
--   the user typed in and a 'TreeIter' which points to the cell
--   to be compared.
--
treeViewSetSearchEqualFunc :: TreeViewClass tv => tv ->
			      (Int -> String -> TreeIter -> IO Bool) ->
			      IO ()
treeViewSetSearchEqualFunc tv pred = do
  fPtr <- mkTreeViewSearchEqualFunc (\_ col keyPtr itPtr _ -> do
    key <- peekUTFString keyPtr
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

foreign import ccall "wrapper" mkTreeViewSearchEqualFunc ::
  (Ptr TreeModel -> {#type gint#} -> CString -> Ptr TreeIter -> Ptr () ->
   IO {#type gboolean#}) -> IO TreeViewSearchEqualFunc


-- helper to marshal native tree paths to TreePaths
readNTP :: Ptr TreePath -> IO TreePath
readNTP ptr = nativeTreePathGetIndices (NativeTreePath (castPtr ptr))

--------------------
-- Signals

-- | The user has dragged a column to another
-- position.
--
onColumnsChanged, afterColumnsChanged :: TreeViewClass tv => tv -> IO () ->
					 IO (ConnectId tv)
onColumnsChanged = connect_NONE__NONE "columns_changed" False
afterColumnsChanged = connect_NONE__NONE "columns_changed" True

-- | The cursor in the tree has moved.
--
onCursorChanged, afterCursorChanged :: TreeViewClass tv => tv -> IO () ->
				       IO (ConnectId tv)
onCursorChanged = connect_NONE__NONE "cursor_changed" False
afterCursorChanged = connect_NONE__NONE "cursor_changed" True

-- | A row was activated.
--
-- * Activation usually means the user has pressed return on a row.
--
onRowActivated, afterRowActivated :: TreeViewClass tv => tv ->
				     (TreePath -> TreeViewColumn -> IO ()) ->
				     IO (ConnectId tv)
onRowActivated = connect_BOXED_OBJECT__NONE "row_activated" 
		   readNTP False
afterRowActivated = connect_BOXED_OBJECT__NONE "row_activated" 
		      readNTP True

-- | Children of this node were hidden.
--
onRowCollapsed, afterRowCollapsed :: TreeViewClass tv => tv ->
				     (TreeIter -> TreePath -> IO ()) ->
				     IO (ConnectId tv)
onRowCollapsed = connect_BOXED_BOXED__NONE "row_collapsed"
  createTreeIter readNTP False
afterRowCollapsed = connect_BOXED_BOXED__NONE "row_collapsed"
  createTreeIter readNTP True

-- | Children of this node are made visible.
--
onRowExpanded, afterRowExpanded :: TreeViewClass tv => tv ->
				     (TreeIter -> TreePath -> IO ()) ->
				     IO (ConnectId tv)
onRowExpanded = connect_BOXED_BOXED__NONE "row_expanded"
  createTreeIter readNTP False
afterRowExpanded = connect_BOXED_BOXED__NONE "row_expanded"
  createTreeIter readNTP True

-- | The user wants to search 
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

-- | Determine if this row should be collapsed.
--
-- * If the application connects to this function and returns @False@,
--   the specifc row will not be altered.
--
onTestCollapseRow, afterTestCollapseRow :: TreeViewClass tv => tv ->
					   (TreeIter -> TreePath -> IO Bool) ->
					   IO (ConnectId tv)
onTestCollapseRow = connect_BOXED_BOXED__BOOL "test_collapse_row"
  createTreeIter readNTP False
afterTestCollapseRow = connect_BOXED_BOXED__BOOL "test_collapse_row"
  createTreeIter readNTP True

-- | Determine if this row should be expanded.
--
-- * If the application connects to this function and returns @False@,
--   the specifc row will not be altered.
--
onTestExpandRow, afterTestExpandRow :: TreeViewClass tv => tv ->
					   (TreeIter -> TreePath -> IO Bool) ->
					   IO (ConnectId tv)
onTestExpandRow = connect_BOXED_BOXED__BOOL "test_expand_row"
  createTreeIter readNTP False
afterTestExpandRow = connect_BOXED_BOXED__BOOL "test_expand_row"
  createTreeIter readNTP True

