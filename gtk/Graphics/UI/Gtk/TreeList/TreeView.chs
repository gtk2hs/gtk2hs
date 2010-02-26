{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget TreeView
--
--  Author : Axel Simon
--
--  Created: 9 May 2001
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
-- * This module and all other modules in 'Graphics.UI.Gtk.TreeList' are
--   deprecated. Please use the modules in 'Graphics.UI.Gtk.ModelView'.
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
  toTreeView,
  Point,

-- * Constructors
  treeViewNew,
  treeViewNewWithModel,

-- * Methods
  treeViewGetModel,
  treeViewSetModel,
  treeViewGetSelection,
  treeViewGetHAdjustment,
  treeViewSetHAdjustment,
  treeViewGetVAdjustment,
  treeViewSetVAdjustment,
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
#if GTK_CHECK_VERSION(2,6,0)
  treeViewGetFixedHeightMode,
  treeViewSetFixedHeightMode,
  treeViewGetHoverSelection,
  treeViewSetHoverSelection,
  treeViewGetHoverExpand,
  treeViewSetHoverExpand,
#endif
-- * Attributes
  treeViewModel,
  treeViewHAdjustment,
  treeViewVAdjustment,
  treeViewHeadersVisible,
  treeViewHeadersClickable,
  treeViewExpanderColumn,
  treeViewReorderable,
  treeViewRulesHint,
  treeViewEnableSearch,
  treeViewSearchColumn,
#if GTK_CHECK_VERSION(2,6,0)
  treeViewFixedHeightMode,
  treeViewHoverSelection,
  treeViewHoverExpand,
#endif

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

import Control.Monad	(liftM, mapM)
import Data.Maybe	(fromMaybe)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList		(fromGList)
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GObject		(makeNewGObject, destroyFunPtr)
import Graphics.UI.Gtk.General.Structs	(Point, Rectangle)
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.TreeList.TreeModel#}
{#import Graphics.UI.Gtk.TreeList.TreePath#}
{#import Graphics.UI.Gtk.TreeList.TreeIter#}
{#import Graphics.UI.Gtk.TreeList.TreeViewColumn#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'TreeView' widget.
--
treeViewNew :: IO TreeView
treeViewNew =
  makeNewObject mkTreeView $
  liftM (castPtr :: Ptr Widget -> Ptr TreeView) $
  {# call tree_view_new #}

-- | Create a new 'TreeView' 
-- widget with @model@ as the storage model.
--
treeViewNewWithModel :: TreeModelClass model => model -> IO TreeView
treeViewNewWithModel model =
  makeNewObject mkTreeView $
  liftM (castPtr :: Ptr Widget -> Ptr TreeView) $
  {# call tree_view_new_with_model #}
    (toTreeModel model)

--------------------
-- Methods

-- | Returns the model that supplies the data for
-- this 'TreeView'. Returns @Nothing@ if the model is unset.
--
treeViewGetModel :: TreeViewClass self => self -> IO (Maybe TreeModel)
treeViewGetModel self =
  maybeNull (makeNewGObject mkTreeModel) $
  {# call unsafe tree_view_get_model #}
    (toTreeView self)

-- | Set the 'TreeModel' for the current View.
--
treeViewSetModel :: (TreeViewClass self, TreeModelClass model) => self
 -> model
 -> IO ()
treeViewSetModel self model =
  {# call tree_view_set_model #}
    (toTreeView self)
    (toTreeModel model)

-- | Retrieve a 'TreeSelection' that
-- holds the current selected nodes of the View.
--
treeViewGetSelection :: TreeViewClass self => self -> IO TreeSelection
treeViewGetSelection self =
  makeNewGObject mkTreeSelection $
  {# call unsafe tree_view_get_selection #}
    (toTreeView self)

-- | Gets the 'Adjustment' currently being used for the horizontal aspect.
--
treeViewGetHAdjustment :: TreeViewClass self => self -> IO (Maybe Adjustment)
treeViewGetHAdjustment self =
  maybeNull (makeNewObject mkAdjustment) $
  {# call unsafe tree_view_get_hadjustment #}
    (toTreeView self)

-- | Sets the 'Adjustment' for the current horizontal aspect.
--
treeViewSetHAdjustment :: TreeViewClass self => self
 -> Maybe Adjustment -- ^ @adjustment@ - The 'Adjustment' to set, or @Nothing@
 -> IO ()
treeViewSetHAdjustment self adjustment =
  {# call tree_view_set_hadjustment #}
    (toTreeView self)
    (fromMaybe (Adjustment nullForeignPtr) adjustment)

-- | Gets the 'Adjustment' currently being used for the vertical aspect.
--
treeViewGetVAdjustment :: TreeViewClass self => self -> IO (Maybe Adjustment)
treeViewGetVAdjustment self =
  maybeNull (makeNewObject mkAdjustment) $
  {# call unsafe tree_view_get_vadjustment #}
    (toTreeView self)

-- | Sets the 'Adjustment' for the current vertical aspect.
--
treeViewSetVAdjustment :: TreeViewClass self => self
 -> Maybe Adjustment -- ^ @adjustment@ - The 'Adjustment' to set, or @Nothing@
 -> IO ()
treeViewSetVAdjustment self adjustment =
  {# call tree_view_set_vadjustment #}
    (toTreeView self)
    (fromMaybe (Adjustment nullForeignPtr) adjustment)

-- | Query if the column headers are visible.
--
treeViewGetHeadersVisible :: TreeViewClass self => self -> IO Bool
treeViewGetHeadersVisible self =
  liftM toBool $
  {# call unsafe tree_view_get_headers_visible #}
    (toTreeView self)

-- | Set the visibility state of the column headers.
--
treeViewSetHeadersVisible :: TreeViewClass self => self -> Bool -> IO ()
treeViewSetHeadersVisible self headersVisible =
  {# call tree_view_set_headers_visible #}
    (toTreeView self)
    (fromBool headersVisible)

-- | Resize the columns to their optimal size.
--
treeViewColumnsAutosize :: TreeViewClass self => self -> IO ()
treeViewColumnsAutosize self =
  {# call tree_view_columns_autosize #}
    (toTreeView self)

-- | Set wether the columns headers are sensitive to mouse clicks.
--
treeViewSetHeadersClickable :: TreeViewClass self => self -> Bool -> IO ()
treeViewSetHeadersClickable self setting =
  {# call tree_view_set_headers_clickable #}
    (toTreeView self)
    (fromBool setting)

-- | Query if visual aid for wide columns is turned on.
--
treeViewGetRulesHint :: TreeViewClass self => self -> IO Bool
treeViewGetRulesHint self =
  liftM toBool $
  {# call unsafe tree_view_get_rules_hint #}
    (toTreeView self)

-- | This function tells Gtk+ that the user interface for your application
-- requires users to read across tree rows and associate cells with one
-- another. By default, Gtk+ will then render the tree with alternating row
-- colors. Do /not/ use it just because you prefer the appearance of the ruled
-- tree; that's a question for the theme. Some themes will draw tree rows in
-- alternating colors even when rules are turned off, and users who prefer that
-- appearance all the time can choose those themes. You should call this
-- function only as a /semantic/ hint to the theme engine that your tree makes
-- alternating colors useful from a functional standpoint (since it has lots of
-- columns, generally).
--
treeViewSetRulesHint :: TreeViewClass self => self -> Bool -> IO ()
treeViewSetRulesHint self setting =
  {# call tree_view_set_rules_hint #}
    (toTreeView self)
    (fromBool setting)

-- | Append a new column to the 'TreeView'. Returns the new number of columns.
--
treeViewAppendColumn :: TreeViewClass self => self -> TreeViewColumn -> IO Int
treeViewAppendColumn self column =
  liftM fromIntegral $
  {# call tree_view_append_column #}
    (toTreeView self)
    column

-- | Remove column @tvc@ from the 'TreeView'
-- widget. The number of remaining columns is returned.
--
treeViewRemoveColumn :: TreeViewClass self => self -> TreeViewColumn -> IO Int
treeViewRemoveColumn self column =
  liftM fromIntegral $
  {# call tree_view_remove_column #}
    (toTreeView self)
    column

-- | Inserts column @tvc@ into the
-- 'TreeView' widget at the position @pos@. Returns the number of
-- columns after insertion. Specify -1 for @pos@ to insert the column
-- at the end.
--
treeViewInsertColumn :: TreeViewClass self => self
 -> TreeViewColumn
 -> Int
 -> IO Int
treeViewInsertColumn self column position =
  liftM fromIntegral $
  {# call tree_view_insert_column #}
    (toTreeView self)
    column
    (fromIntegral position)

-- | Insert new 'TreeViewColumn'.
--
-- * Inserts new column into the
-- 'TreeView' @self@ at position @pos@ with title
-- ref argtitle, cell renderer @cr@ and attributes
-- @attribs@. Specify -1 for @pos@ to insert the column at
-- the end.
--
treeViewInsertColumnWithAttributes :: (TreeViewClass self, CellRendererClass cr)
   => self -> Int -> String -> cr -> [(String,Int)] -> IO ()
treeViewInsertColumnWithAttributes self pos title cr attribs = do
  column <- treeViewColumnNew
  treeViewColumnSetTitle column title
  treeViewColumnPackStart column cr True
  treeViewColumnAddAttributes column cr attribs
  treeViewInsertColumn self column pos
  return ()

-- | Retrieve a 'TreeViewColumn'.
--
-- * Retrieve the @pos@ th columns of
--   'TreeView'. If the index is out of range Nothing is returned.
--
treeViewGetColumn :: TreeViewClass self => self -> Int -> IO (Maybe TreeViewColumn)
treeViewGetColumn self pos = do
  tvcPtr <- {# call unsafe tree_view_get_column #} (toTreeView self) 
    (fromIntegral pos)
  if tvcPtr==nullPtr then return Nothing else 
    liftM Just $ makeNewObject mkTreeViewColumn (return tvcPtr)

-- | Return all 'TreeViewColumn's in this 'TreeView'.
--
treeViewGetColumns :: TreeViewClass self => self -> IO [TreeViewColumn]
treeViewGetColumns self = do
  colsList <- {# call unsafe tree_view_get_columns #} (toTreeView self)
  colsPtr <- fromGList colsList
  mapM (makeNewObject mkTreeViewColumn) (map return colsPtr)

-- | Move a specific column.
--
-- * Use 'treeViewMoveColumnToFront' if you want to move the column
--   to the left end of the 'TreeView'.
--
treeViewMoveColumnAfter :: TreeViewClass self => self
 -> TreeViewColumn
 -> TreeViewColumn
 -> IO ()
treeViewMoveColumnAfter self column baseColumn =
  {# call tree_view_move_column_after #}
    (toTreeView self)
    column
    baseColumn

-- | Move a specific column.
--
-- * Use 'treeViewMoveColumnAfter' if you want to move the column
--   somewhere else than to the leftmost position.
--
treeViewMoveColumnFirst :: TreeViewClass self => self -> TreeViewColumn -> IO ()
treeViewMoveColumnFirst self which =
  {# call tree_view_move_column_after #}
    (toTreeView self)
    which
    (TreeViewColumn nullForeignPtr)

-- | Set location of hierarchy controls.
--
-- * Sets the column to draw the expander arrow at. If @col@
--   is @Nothing@, then the expander arrow is always at the first
--   visible column.
--
-- If you do not want expander arrow to appear in your tree, set the
-- expander column to a hidden column.
--
treeViewSetExpanderColumn :: TreeViewClass self => self
 -> Maybe TreeViewColumn
 -> IO ()
treeViewSetExpanderColumn self column =
  {# call unsafe tree_view_set_expander_column #}
    (toTreeView self)
    (fromMaybe (TreeViewColumn nullForeignPtr) column)

-- | Get location of hierarchy controls.
--
-- * Gets the column to draw the expander arrow at. If @col@
--   is @Nothing@, then the expander arrow is always at the first
--   visible column.
--
treeViewGetExpanderColumn :: TreeViewClass self => self
 -> IO TreeViewColumn
treeViewGetExpanderColumn self =
  makeNewObject mkTreeViewColumn $
  {# call unsafe tree_view_get_expander_column #}
    (toTreeView self)

-- | Specify where a column may be dropped.
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
treeViewSetColumnDragFunction :: TreeViewClass self => self
 -> Maybe (TreeViewColumn
        -> Maybe TreeViewColumn
        -> Maybe TreeViewColumn
        -> IO Bool)
 -> IO ()
treeViewSetColumnDragFunction self Nothing =
  {# call tree_view_set_column_drag_function #} (toTreeView self)
    nullFunPtr nullPtr nullFunPtr
treeViewSetColumnDragFunction self (Just pred) = do
  fPtr <- mkTreeViewColumnDropFunc $ \_ target prev next _ -> do
    target' <- makeNewObject mkTreeViewColumn (return target)
    prev' <- if prev==nullPtr then return Nothing else liftM Just $
      makeNewObject mkTreeViewColumn (return prev)
    next' <- if next==nullPtr then return Nothing else liftM Just $
      makeNewObject mkTreeViewColumn (return next)
    res <- pred target' prev' next'
    return (fromBool res)
  {# call tree_view_set_column_drag_function #}
    (toTreeView self)
    fPtr
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
treeViewScrollToPoint :: TreeViewClass self => self
 -> Int
 -> Int
 -> IO ()
treeViewScrollToPoint self treeX treeY =
  {# call tree_view_scroll_to_point #}
    (toTreeView self)
    (fromIntegral treeX)
    (fromIntegral treeY)

-- | Scroll to a cell.
--
-- * Scroll to a cell as specified by @path@ and @tvc@. 
--   The cell is aligned within the 'TreeView' widget as
--   follows: horizontally by @hor@ from left (@0.0@) to
--   right (@1.0@) and vertically by @ver@ from top
--   (@0.0@) to buttom (@1.0@).
--
treeViewScrollToCell :: TreeViewClass self => self
 -> TreePath
 -> TreeViewColumn
 -> Maybe (Float, Float)
 -> IO ()
treeViewScrollToCell self path column (Just (ver,hor)) =
  withTreePath path $ \path ->
  {# call tree_view_scroll_to_cell #}
    (toTreeView self)
    path
    column
    1 
    (realToFrac ver)
    (realToFrac hor)
treeViewScrollToCell self path column Nothing = 
  withTreePath path $ \path ->
  {# call tree_view_scroll_to_cell #}
    (toTreeView self)
    path
    column
    0
    0.0
    0.0

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
treeViewSetCursor :: TreeViewClass self => self
 -> TreePath
 -> (Maybe (TreeViewColumn, Bool))
 -> IO ()
treeViewSetCursor self path Nothing =
  withTreePath path $ \path ->
  {# call tree_view_set_cursor #}
    (toTreeView self)
    path
    (TreeViewColumn nullForeignPtr)
    (fromBool False)
treeViewSetCursor self path (Just (focusColumn, startEditing)) =
  withTreePath path $ \path ->
  {# call tree_view_set_cursor #}
    (toTreeView self)
    path
    focusColumn
    (fromBool startEditing)

#if GTK_CHECK_VERSION(2,2,0)
-- | Selects a cell in a specific row.
--
-- * Similar to 'treeViewSetCursor' but allows a column to
--   containt several 'CellRenderer's.
--
-- * Only available in Gtk 2.2 and higher.
--
treeViewSetCursorOnCell :: (TreeViewClass self, CellRendererClass focusCell) => self
 -> TreePath
 -> TreeViewColumn
 -> focusCell
 -> Bool
 -> IO ()
treeViewSetCursorOnCell self path focusColumn focusCell startEditing =
  withTreePath path $ \path ->
  {# call tree_view_set_cursor_on_cell #}
    (toTreeView self)
    path
    focusColumn
    (toCellRenderer focusCell)
    (fromBool startEditing)
#endif

-- | Retrieves the position of the focus.
--
-- * Returns a pair @(path, column)@.If the cursor is not currently
--   set, @path@ will be @[]@. If no column is currently
--   selected, @column@ will be @Nothing@.
--
treeViewGetCursor :: TreeViewClass self => self
 -> IO (TreePath, Maybe TreeViewColumn)
treeViewGetCursor self =
  alloca $ \tpPtrPtr -> alloca $ \tvcPtrPtr -> do
  {# call unsafe tree_view_get_cursor #}
    (toTreeView self)
    (castPtr tpPtrPtr)
    (castPtr tvcPtrPtr)
  tpPtr <- peek tpPtrPtr
  tvcPtr <- peek tvcPtrPtr
  tp <- fromTreePath tpPtr
  tvc <- if tvcPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkTreeViewColumn (return tvcPtr)
  return (tp,tvc)

-- | Emit the activated signal on a cell.
--
treeViewRowActivated :: TreeViewClass self => self
 -> TreePath
 -> TreeViewColumn
 -> IO ()
treeViewRowActivated self path column =
  withTreePath path $ \path ->
  {# call tree_view_row_activated #}
    (toTreeView self)
    path
    column

-- | Recursively expands all nodes in the tree view.
--
treeViewExpandAll :: TreeViewClass self => self -> IO ()
treeViewExpandAll self =
  {# call tree_view_expand_all #}
    (toTreeView self)

-- | Recursively collapses all visible, expanded nodes in the tree view.
--
treeViewCollapseAll :: TreeViewClass self => self -> IO ()
treeViewCollapseAll self =
  {# call tree_view_collapse_all #}
    (toTreeView self)

#if GTK_CHECK_VERSION(2,2,0)
-- | Make a certain path visible.
--
-- * This will expand all parent rows of @tp@ as necessary.
--
-- * Only available in Gtk 2.2 and higher.
--
treeViewExpandToPath :: TreeViewClass self => self -> TreePath -> IO ()
treeViewExpandToPath self path =
  withTreePath path $ \path ->
  {# call tree_view_expand_to_path #}
    (toTreeView self)
    path
#endif

-- | Opens the row so its children are visible.
--
treeViewExpandRow :: TreeViewClass self => self
 -> TreePath -- ^ @path@ - path to a row
 -> Bool     -- ^ @openAll@ - whether to recursively expand, or just expand
             -- immediate children
 -> IO Bool  -- ^ returns @True@ if the row existed and had children
treeViewExpandRow self path openAll =
  liftM toBool $
  withTreePath path $ \path ->
  {# call tree_view_expand_row #}
    (toTreeView self)
    path
    (fromBool openAll)

-- | Collapses a row (hides its child rows, if they exist).
--
treeViewCollapseRow :: TreeViewClass self => self
 -> TreePath -- ^ @path@ - path to a row in the tree view
 -> IO Bool  -- ^ returns @True@ if the row was collapsed.
treeViewCollapseRow self path =
  liftM toBool $
  withTreePath path $ \path ->
  {# call tree_view_collapse_row #}
    (toTreeView self)
    path

-- | Call function for every expaned row.
--
treeViewMapExpandedRows :: TreeViewClass self => self
 -> (TreePath -> IO ())
 -> IO ()
treeViewMapExpandedRows self func = do
  fPtr <- mkTreeViewMappingFunc $ \_ tpPtr _ -> fromTreePath tpPtr >>= func
  {# call tree_view_map_expanded_rows #}
    (toTreeView self)
    fPtr
    nullPtr
  freeHaskellFunPtr fPtr

{#pointer TreeViewMappingFunc#}

foreign import ccall "wrapper" mkTreeViewMappingFunc ::
  (Ptr () -> Ptr NativeTreePath -> Ptr () -> IO ()) ->
  IO TreeViewMappingFunc

-- | Check if row is expanded.
--
treeViewRowExpanded :: TreeViewClass self => self
 -> TreePath -- ^ @path@ - A 'TreePath' to test expansion state.
 -> IO Bool  -- ^ returns @True@ if @path@ is expanded.
treeViewRowExpanded self path =
  liftM toBool $
  withTreePath path $ \path ->
  {# call unsafe tree_view_row_expanded #}
    (toTreeView self)
    path

-- | Query if rows can be moved around.
--
-- * See 'treeViewSetReorderable'.
--
treeViewGetReorderable :: TreeViewClass self => self -> IO Bool
treeViewGetReorderable self =
  liftM toBool $
  {# call unsafe tree_view_get_reorderable #}
    (toTreeView self)

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
treeViewSetReorderable :: TreeViewClass self => self
 -> Bool
 -> IO ()
treeViewSetReorderable self reorderable =
  {# call tree_view_set_reorderable #}
    (toTreeView self)
    (fromBool reorderable)

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
treeViewGetPathAtPos :: TreeViewClass self => self
 -> Point
 -> IO (Maybe (TreePath, TreeViewColumn, Point))
treeViewGetPathAtPos self (x,y) =
  alloca $ \tpPtrPtr ->
  alloca $ \tvcPtrPtr ->
  alloca $ \xPtr ->
  alloca $ \yPtr -> do
    res <- liftM toBool $
      {# call unsafe tree_view_get_path_at_pos #}
      (toTreeView self)
      (fromIntegral x)
      (fromIntegral y)
      (castPtr tpPtrPtr)
      (castPtr tvcPtrPtr)
      xPtr
      yPtr
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
treeViewGetCellArea :: TreeViewClass self => self
 -> Maybe TreePath
 -> TreeViewColumn
 -> IO Rectangle
treeViewGetCellArea self Nothing tvc =
  alloca $ \rPtr ->
  {# call unsafe tree_view_get_cell_area #}
    (toTreeView self)
    (NativeTreePath nullPtr)
    tvc
    (castPtr (rPtr :: Ptr Rectangle))
    >> peek rPtr
treeViewGetCellArea self (Just tp) tvc = 
  withTreePath tp $ \tp ->
  alloca $ \rPtr -> do
  {# call unsafe tree_view_get_cell_area #}
    (toTreeView self)
    tp
    tvc
    (castPtr (rPtr :: Ptr Rectangle))
    >> peek rPtr

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
treeViewGetBackgroundArea :: TreeViewClass self => self
 -> Maybe TreePath
 -> TreeViewColumn
 -> IO Rectangle
treeViewGetBackgroundArea self Nothing tvc =
  alloca $ \rPtr ->
  {# call unsafe tree_view_get_background_area #}
    (toTreeView self)
    (NativeTreePath nullPtr)
    tvc
    (castPtr (rPtr :: Ptr Rectangle))
  >> peek rPtr
treeViewGetBackgroundArea self (Just tp) tvc = 
  withTreePath tp $ \tp -> alloca $ \rPtr ->
  {# call unsafe tree_view_get_background_area #}
    (toTreeView self)
    tp
    tvc
    (castPtr (rPtr :: Ptr Rectangle))
  >> peek rPtr

-- | Retrieve the currently visible area.
--
-- * The returned rectangle gives the visible part of the tree in tree
--   coordinates.
--
treeViewGetVisibleRect :: TreeViewClass self => self -> IO Rectangle
treeViewGetVisibleRect self =
  alloca $ \rPtr -> do
  {# call unsafe tree_view_get_visible_rect #}
    (toTreeView self)
    (castPtr (rPtr :: Ptr Rectangle))
  peek rPtr

-- | Convert tree to widget pixel coordinates.
--
-- * See module description.
--
treeViewTreeToWidgetCoords :: TreeViewClass self => self
 -> Point    -- ^ @(tx, ty)@ - tree X and Y coordinates
 -> IO Point -- ^ @(wx, wy)@ returns widget X and Y coordinates
treeViewTreeToWidgetCoords self (tx, ty) =
  alloca $ \wxPtr ->
  alloca $ \wyPtr -> do
  {# call unsafe tree_view_tree_to_widget_coords #}
    (toTreeView self)
    (fromIntegral tx)
    (fromIntegral ty)
    wxPtr
    wyPtr
  wx <- peek wxPtr
  wy <- peek wyPtr
  return (fromIntegral wx, fromIntegral wy)

-- | Convert widget to tree pixel coordinates.
--
-- * See module description.
--
treeViewWidgetToTreeCoords :: TreeViewClass self => self
 -> Point    -- ^ @(wx, wy)@ - widget X and Y coordinates
 -> IO Point -- ^ @(tx, ty)@ returns tree X and Y coordinates
treeViewWidgetToTreeCoords self (wx, wy) =
  alloca $ \txPtr ->
  alloca $ \tyPtr -> do
  {# call unsafe tree_view_widget_to_tree_coords #}
    (toTreeView self)
    (fromIntegral wx)
    (fromIntegral wy)
    txPtr
    tyPtr
  tx <- peek txPtr
  ty <- peek tyPtr
  return (fromIntegral tx, fromIntegral ty)

-- | Creates a 'Pixmap' representation of the row at the given path. This image
-- can be used for a drag icon.
--
treeViewCreateRowDragIcon :: TreeViewClass self => self
 -> TreePath
 -> IO Pixmap
treeViewCreateRowDragIcon self path =
  makeNewGObject mkPixmap $
  withTreePath path $ \path ->
  {# call unsafe tree_view_create_row_drag_icon #}
    (toTreeView self)
    path

-- | Returns whether or not the tree allows to start interactive searching by
-- typing in text.
--
-- * If enabled, the user can type in text which will set the cursor to
--   the first matching entry.
--
treeViewGetEnableSearch :: TreeViewClass self => self -> IO Bool
treeViewGetEnableSearch self =
  liftM toBool $
  {# call unsafe tree_view_get_enable_search #}
    (toTreeView self)

-- | If this is set, then the user can type in text to search
-- through the tree interactively (this is sometimes called \"typeahead
-- find\").
--
-- Note that even if this is @False@, the user can still initiate a search
-- using the \"start-interactive-search\" key binding.
--
treeViewSetEnableSearch :: TreeViewClass self => self -> Bool -> IO ()
treeViewSetEnableSearch self enableSearch =
  {# call tree_view_set_enable_search #}
    (toTreeView self)
    (fromBool enableSearch)

-- | Gets the column searched on by the interactive search.
--
treeViewGetSearchColumn :: TreeViewClass self => self
 -> IO Int
treeViewGetSearchColumn self =
  liftM fromIntegral $
  {# call unsafe tree_view_get_search_column #}
    (toTreeView self)

-- | Sets @column@ as the column where the interactive search code should
-- search in.
--
-- If the sort column is set, users can use the \"start-interactive-search\"
-- key binding to bring up search popup. The enable-search property controls
-- whether simply typing text will also start an interactive search.
--
-- Note that @column@ refers to a column of the model.
--
treeViewSetSearchColumn :: TreeViewClass self => self
 -> Int   -- ^ @column@ - the column of the model to search in, or -1 to
          -- disable searching
 -> IO ()
treeViewSetSearchColumn self column =
  {# call tree_view_set_search_column #}
    (toTreeView self)
    (fromIntegral column)

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
treeViewSetSearchEqualFunc :: TreeViewClass self => self
 -> (Int -> String -> TreeIter -> IO Bool)
 -> IO ()
treeViewSetSearchEqualFunc self pred = do
  fPtr <- mkTreeViewSearchEqualFunc (\_ col keyPtr iterPtr _ -> do
    key <- peekUTFString keyPtr
    iter <- peek iterPtr
    liftM (fromBool.not) $ pred (fromIntegral col) key iter)
  {# call tree_view_set_search_equal_func #} (toTreeView self) fPtr 
    (castFunPtrToPtr fPtr) destroyFunPtr

{#pointer TreeViewSearchEqualFunc#}

foreign import ccall "wrapper" mkTreeViewSearchEqualFunc ::
  (Ptr TreeModel -> {#type gint#} -> CString -> Ptr TreeIter -> Ptr () ->
   IO {#type gboolean#}) -> IO TreeViewSearchEqualFunc

-- helper to marshal native tree paths to TreePaths
readNTP :: Ptr TreePath -> IO TreePath
readNTP ptr = peekTreePath (castPtr ptr)

#if GTK_CHECK_VERSION(2,6,0)
-- | Returns whether fixed height mode is turned on for the tree view.
--
-- * Available since Gtk+ version 2.6
--
treeViewGetFixedHeightMode :: TreeViewClass self => self
 -> IO Bool -- ^ returns @True@ if the tree view is in fixed height mode
treeViewGetFixedHeightMode self =
  liftM toBool $
  {# call gtk_tree_view_get_fixed_height_mode #}
    (toTreeView self)

-- | Enables or disables the fixed height mode of the tree view. Fixed height
-- mode speeds up 'TreeView' by assuming that all rows have the same height.
-- Only enable this option if all rows are the same height and all columns are
-- of type 'TreeViewColumnFixed'.
--
-- * Available since Gtk+ version 2.6
--
treeViewSetFixedHeightMode :: TreeViewClass self => self
 -> Bool  -- ^ @enable@ - @True@ to enable fixed height mode
 -> IO ()
treeViewSetFixedHeightMode self enable =
  {# call gtk_tree_view_set_fixed_height_mode #}
    (toTreeView self)
    (fromBool enable)

-- | Returns whether hover selection mode is turned on for @treeView@.
--
-- * Available since Gtk+ version 2.6
--
treeViewGetHoverSelection :: TreeViewClass self => self
 -> IO Bool -- ^ returns @True@ if the tree view is in hover selection mode
treeViewGetHoverSelection self =
  liftM toBool $
  {# call gtk_tree_view_get_hover_selection #}
    (toTreeView self)

-- | Enables of disables the hover selection mode of the tree view. Hover
-- selection makes the selected row follow the pointer. Currently, this works
-- only for the selection modes 'SelectionSingle' and 'SelectionBrowse'.
--
-- * Available since Gtk+ version 2.6
--
treeViewSetHoverSelection :: TreeViewClass self => self
 -> Bool  -- ^ @hover@ - @True@ to enable hover selection mode
 -> IO ()
treeViewSetHoverSelection self hover =
  {# call gtk_tree_view_set_hover_selection #}
    (toTreeView self)
    (fromBool hover)

-- | Returns whether hover expansion mode is turned on for the tree view.
--
-- * Available since Gtk+ version 2.6
--
treeViewGetHoverExpand :: TreeViewClass self => self
 -> IO Bool -- ^ returns @True@ if the tree view is in hover expansion mode
treeViewGetHoverExpand self =
  liftM toBool $
  {# call gtk_tree_view_get_hover_expand #}
    (toTreeView self)

-- | Enables of disables the hover expansion mode of the tree view. Hover
-- expansion makes rows expand or collaps if the pointer moves over them.
--
-- * Available since Gtk+ version 2.6
--
treeViewSetHoverExpand :: TreeViewClass self => self
 -> Bool  -- ^ @expand@ - @True@ to enable hover selection mode
 -> IO ()
treeViewSetHoverExpand self expand =
  {# call gtk_tree_view_set_hover_expand #}
    (toTreeView self)
    (fromBool expand)
#endif

--------------------
-- Attributes

-- | The model for the tree view.
--
treeViewModel :: (TreeViewClass self, TreeModelClass model) => ReadWriteAttr self (Maybe TreeModel) model
treeViewModel = newAttr
  treeViewGetModel
  treeViewSetModel

-- | Horizontal Adjustment for the widget.
--
treeViewHAdjustment :: TreeViewClass self => Attr self (Maybe Adjustment)
treeViewHAdjustment = newAttr
  treeViewGetHAdjustment
  treeViewSetHAdjustment

-- | Vertical Adjustment for the widget.
--
treeViewVAdjustment :: TreeViewClass self => Attr self (Maybe Adjustment)
treeViewVAdjustment = newAttr
  treeViewGetVAdjustment
  treeViewSetVAdjustment

-- | Show the column header buttons.
--
-- Default value: @True@
--
treeViewHeadersVisible :: TreeViewClass self => Attr self Bool
treeViewHeadersVisible = newAttr
  treeViewGetHeadersVisible
  treeViewSetHeadersVisible

-- | Column headers respond to click events.
--
-- Default value: @False@
--
treeViewHeadersClickable :: TreeViewClass self => Attr self Bool
treeViewHeadersClickable = newAttrFromBoolProperty "headers-clickable"

-- | Set the column for the expander column.
--
treeViewExpanderColumn :: TreeViewClass self => ReadWriteAttr self TreeViewColumn (Maybe TreeViewColumn)
treeViewExpanderColumn = newAttr
  treeViewGetExpanderColumn
  treeViewSetExpanderColumn

-- | View is reorderable.
--
-- Default value: @False@
--
treeViewReorderable :: TreeViewClass self => Attr self Bool
treeViewReorderable = newAttr
  treeViewGetReorderable
  treeViewSetReorderable

-- | Set a hint to the theme engine to draw rows in alternating colors.
--
-- Default value: @False@
--
treeViewRulesHint :: TreeViewClass self => Attr self Bool
treeViewRulesHint = newAttr
  treeViewGetRulesHint
  treeViewSetRulesHint

-- | View allows user to search through columns interactively.
--
-- Default value: @True@
--
treeViewEnableSearch :: TreeViewClass self => Attr self Bool
treeViewEnableSearch = newAttr
  treeViewGetEnableSearch
  treeViewSetEnableSearch

-- | Model column to search through when searching through code.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
treeViewSearchColumn :: TreeViewClass self => Attr self Int
treeViewSearchColumn = newAttr
  treeViewGetSearchColumn
  treeViewSetSearchColumn

#if GTK_CHECK_VERSION(2,6,0)
-- | Setting the fixed-height-mode property to @True@ speeds up 'TreeView'
-- by assuming that all rows have the same height. Only enable this option if
-- all rows are the same height. Please see 'treeViewSetFixedHeightMode' for
-- more information on this option.
--
-- Default value: @False@
--
treeViewFixedHeightMode :: TreeViewClass self => Attr self Bool
treeViewFixedHeightMode = newAttr
  treeViewGetFixedHeightMode
  treeViewSetFixedHeightMode

-- | Enables of disables the hover selection mode of the tree view. Hover
-- selection makes the selected row follow the pointer. Currently, this works
-- only for the selection modes 'SelectionSingle' and 'SelectionBrowse'.
--
-- This mode is primarily indended for treeviews in popups, e.g. in
-- 'ComboBox' or 'EntryCompletion'.
--
-- Default value: @False@
--
treeViewHoverSelection :: TreeViewClass self => Attr self Bool
treeViewHoverSelection = newAttr
  treeViewGetHoverSelection
  treeViewSetHoverSelection

-- | Enables of disables the hover expansion mode of the tree view. Hover
-- expansion makes rows expand or collaps if the pointer moves over them.
--
-- This mode is primarily indended for treeviews in popups, e.g. in
-- 'ComboBox' or 'EntryCompletion'.
--
-- Default value: @False@
--
treeViewHoverExpand :: TreeViewClass self => Attr self Bool
treeViewHoverExpand = newAttr
  treeViewGetHoverExpand
  treeViewSetHoverExpand
#endif

--------------------
-- Signals

-- | The user has dragged a column to another position.
--
onColumnsChanged, afterColumnsChanged :: TreeViewClass self => self
 -> IO ()
 -> IO (ConnectId self)
onColumnsChanged = connect_NONE__NONE "columns_changed" False
afterColumnsChanged = connect_NONE__NONE "columns_changed" True

-- | The cursor in the tree has moved.
--
onCursorChanged, afterCursorChanged :: TreeViewClass self => self
 -> IO ()
 -> IO (ConnectId self)
onCursorChanged = connect_NONE__NONE "cursor_changed" False
afterCursorChanged = connect_NONE__NONE "cursor_changed" True

-- | A row was activated.
--
-- * Activation usually means the user has pressed return on a row.
--
onRowActivated, afterRowActivated :: TreeViewClass self => self
 -> (TreePath -> TreeViewColumn -> IO ())
 -> IO (ConnectId self)
onRowActivated = connect_BOXED_OBJECT__NONE "row_activated" 
		   readNTP False
afterRowActivated = connect_BOXED_OBJECT__NONE "row_activated" 
		      readNTP True

-- | Children of this node were hidden.
--
onRowCollapsed, afterRowCollapsed :: TreeViewClass self => self
 -> (TreeIter -> TreePath -> IO ())
 -> IO (ConnectId self)
onRowCollapsed = connect_BOXED_BOXED__NONE "row_collapsed"
  peek readNTP False
afterRowCollapsed = connect_BOXED_BOXED__NONE "row_collapsed"
  peek readNTP True

-- | Children of this node are made visible.
--
onRowExpanded, afterRowExpanded :: TreeViewClass self => self
 -> (TreeIter -> TreePath -> IO ())
 -> IO (ConnectId self)
onRowExpanded = connect_BOXED_BOXED__NONE "row_expanded"
  peek readNTP False
afterRowExpanded = connect_BOXED_BOXED__NONE "row_expanded"
  peek readNTP True

-- | The user wants to search interactively.
--
-- * Connect to this signal if you want to provide you own search facility.
--   Note that you must handle all keyboard input yourself.
--
onStartInteractiveSearch, afterStartInteractiveSearch :: 
  TreeViewClass self => self -> IO () -> IO (ConnectId self)

#if GTK_CHECK_VERSION(2,2,0)

onStartInteractiveSearch self fun =
  connect_NONE__BOOL "start_interactive_search" False self (fun >> return True)
afterStartInteractiveSearch self fun =
  connect_NONE__BOOL "start_interactive_search" True self (fun >> return True)

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
onTestCollapseRow, afterTestCollapseRow :: TreeViewClass self => self
 -> (TreeIter -> TreePath -> IO Bool)
 -> IO (ConnectId self)
onTestCollapseRow = connect_BOXED_BOXED__BOOL "test_collapse_row"
  peek readNTP False
afterTestCollapseRow = connect_BOXED_BOXED__BOOL "test_collapse_row"
  peek readNTP True

-- | Determine if this row should be expanded.
--
-- * If the application connects to this function and returns @False@,
--   the specifc row will not be altered.
--
onTestExpandRow, afterTestExpandRow :: TreeViewClass self => self
 -> (TreeIter -> TreePath -> IO Bool)
 -> IO (ConnectId self)
onTestExpandRow = connect_BOXED_BOXED__BOOL "test_expand_row"
  peek readNTP False
afterTestExpandRow = connect_BOXED_BOXED__BOOL "test_expand_row"
  peek readNTP True
