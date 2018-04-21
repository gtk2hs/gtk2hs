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
-- gtk_tree_view_get_bin_window is to compare the GDK window from incoming
--   events. We don't marshal that window parameter, so this function is not
--   bound either.
--
-- The following functions related to drag and drop:
--   treeViewSetDragDestRow, treeViewGetDragDestRow, treeViewGetDestRowAtPos
-- these seem to be useful only in cases when the user wants to implement
-- drag and drop himself rather than use the widget's implementation. I
-- think this would be a bad idea in the first place.
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
module Graphics.UI.Gtk.ModelView.TreeView (
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
  castToTreeView, gTypeTreeView,
  toTreeView,
  Point,
  DragAction(..),
#if GTK_CHECK_VERSION(2,10,0)
  TreeViewGridLines(..),
#endif

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
#if GTK_CHECK_VERSION(2,12,0)
  treeViewConvertBinWindowToTreeCoords,
  treeViewConvertBinWindowToWidgetCoords,
  treeViewConvertTreeToBinWindowCoords,
  treeViewConvertTreeToWidgetCoords,
  treeViewConvertWidgetToBinWindowCoords,
  treeViewConvertWidgetToTreeCoords,
#endif
#if GTK_MAJOR_VERSION < 3
  treeViewCreateRowDragIcon,
#endif
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
#if GTK_CHECK_VERSION(2,10,0)
  treeViewGetHeadersClickable,
#endif
#endif
#if GTK_CHECK_VERSION(2,8,0)
  treeViewGetVisibleRange,
#endif
#if GTK_CHECK_VERSION(2,10,0)
  treeViewEnableModelDragDest,
  treeViewEnableModelDragSource,
  treeViewUnsetRowsDragSource,
  treeViewUnsetRowsDragDest,
  treeViewGetSearchEntry,
  treeViewSetSearchEntry,
#endif
#if GTK_CHECK_VERSION(2,6,0)
  treeViewSetRowSeparatorFunc,
#if GTK_CHECK_VERSION(2,10,0)
  treeViewGetRubberBanding,
  treeViewSetRubberBanding,
  treeViewGetEnableTreeLines,
  treeViewSetEnableTreeLines,
  treeViewGetGridLines,
  treeViewSetGridLines,
#endif
#endif
#if GTK_CHECK_VERSION(2,12,0)
  treeViewSetTooltipRow,
  treeViewSetTooltipCell,
  treeViewGetTooltipContext,
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
#if GTK_CHECK_VERSION(2,4,0)
  treeViewFixedHeightMode,
#if GTK_CHECK_VERSION(2,6,0)
  treeViewHoverSelection,
  treeViewHoverExpand,
#endif
#endif
  treeViewShowExpanders,
  treeViewLevelIndentation,
  treeViewRubberBanding,
#if GTK_CHECK_VERSION(2,10,0)
  treeViewEnableGridLines,
#endif
  treeViewEnableTreeLines,
#if GTK_CHECK_VERSION(2,10,0)
  treeViewGridLines,
  treeViewSearchEntry,
#endif
#if GTK_CHECK_VERSION(2,12,0)
  treeViewTooltipColumn,
#endif

-- * Signals
  columnsChanged,
  cursorChanged,
  rowCollapsed,
  rowExpanded,
  rowActivated,
  testCollapseRow,
  testExpandRow,

-- * Deprecated
#ifndef DISABLE_DEPRECATED
#if GTK_MAJOR_VERSION < 3
  treeViewWidgetToTreeCoords,
  treeViewTreeToWidgetCoords,
#endif
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
#endif
  ) where

import Control.Monad    (liftM,)
import Data.Maybe       (fromMaybe)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList                (fromGList)
import System.Glib.Flags
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Gdk.Enums        (DragAction(..))
import Graphics.UI.Gtk.Gdk.Events       (Modifier(..))
import Graphics.UI.Gtk.General.Structs  (Point, Rectangle)
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.ModelView.TreeModel (columnIdToNumber,
                                            makeColumnIdString)
{#import Graphics.UI.Gtk.ModelView.Types#}
{#import Graphics.UI.Gtk.General.DNDTypes#}     (TargetList(..))

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
 -> Maybe model
 -> IO ()
treeViewSetModel self model =
  {# call tree_view_set_model #}
    (toTreeView self)
    (maybe (TreeModel nullForeignPtr) toTreeModel model)

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
--
-- * The callback function take the 'TreeViewColumn' to be moved, the
--   second and third arguments are the columns on the left and right side
--   of the new location. At most one of them might be @Nothing@
--   which indicates that the column is about to be dropped at the left or
--   right end of the 'TreeView'.
--
-- * The predicate @pred@ should return @True@ if it is ok
--   to insert the column at this place.
--
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
    (castFunPtrToPtr fPtr) destroyFunPtr

{#pointer TreeViewColumnDropFunc#}

foreign import ccall "wrapper" mkTreeViewColumnDropFunc ::
  (Ptr TreeView -> Ptr TreeViewColumn -> Ptr TreeViewColumn -> Ptr TreeViewColumn ->
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
-- Moves the alignments of tree_view to the position specified by mbColumn and mbPath.
-- If mbColumn is Nothing, then no horizontal scrolling occurs. Likewise, if mbPath
-- is Nothing no vertical scrolling occurs. At a minimum, one of mbColumn or mbPath
-- need to be provided. @rowAlign@ determines where the row is placed, and
-- @colAlign@ determines where column is placed. Both are expected to be between
-- 0.0 and 1.0. 0.0 means left/top alignment, 1.0 means right/bottom alignment,
-- 0.5 means center.
--
-- If Nothing is passed instead of @rowAlign@ and @colAlign@, then the tree does
-- the minimum amount of work to scroll the cell onto the screen. This means
-- that the cell will be scrolled to the edge closest to its current position.
-- If the cell is currently visible on the screen, nothing is done.
--
-- This function only works if the model is set, and path is a valid row on
-- the model. If the model changes before the tree_view is realized, the
-- centered path will be modified to reflect this change.
--
treeViewScrollToCell :: TreeViewClass self => self
 -> Maybe TreePath
 -> Maybe TreeViewColumn
 -> Maybe (Float, Float)
 -> IO ()
treeViewScrollToCell self mbPath mbColumn (Just (rowAlign, colAlign)) =
  maybeWithTreePath mbPath $ \path ->
  {# call tree_view_scroll_to_cell #}
    (toTreeView self)
    path
    (maybe (TreeViewColumn nullForeignPtr) toTreeViewColumn mbColumn)
    1
    (realToFrac rowAlign)
    (realToFrac colAlign)
treeViewScrollToCell self mbPath mbColumn Nothing =
  maybeWithTreePath mbPath $ \path ->
  {# call tree_view_scroll_to_cell #}
    (toTreeView self)
    path
    (maybe (TreeViewColumn nullForeignPtr) toTreeViewColumn mbColumn)
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
  (Ptr TreeView -> Ptr NativeTreePath -> Ptr () -> IO ()) ->
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
-- * Set whether the user can use drag and drop (DND) to reorder the rows in
--   the store. This works on both 'TreeStore' and 'ListStore' models. If @ro@
--   is @True@, then the user can reorder the model by dragging and dropping
--   rows.  The developer can listen to these changes by connecting to the
--   model's signals. If you need to control which rows may be dragged or
--   where rows may be dropped, you can override the
--   'Graphics.UI.Gtk.ModelView.CustomStore.treeDragSourceRowDraggable'
--   function in the default DND implementation of the model.
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
--   which the user clicked on. This function is useful to realize
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

#ifndef DISABLE_DEPRECATED
#if GTK_MAJOR_VERSION < 3
-- | 'treeViewTreeToWidgetCoords' has been deprecated since version 2.12 and should not be used in
-- newly-written code. Due to historial reasons the name of this function is incorrect. For converting
-- bin window coordinates to coordinates relative to bin window, please see
-- 'treeViewConvertBinWindowToWidgetCoords'.
--
-- Converts tree coordinates (coordinates in full scrollable area of the tree) to bin window
-- coordinates.
--
-- Removed in Gtk3.
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

-- | 'treeViewWidgetToTreeCoords' has been deprecated since version 2.12 and should not be used in
-- newly-written code. Due to historial reasons the name of this function is incorrect. For converting
-- coordinates relative to the widget to bin window coordinates, please see
-- 'treeViewConvertWidgetToBinWindowCoords'.
--
-- Converts bin window coordinates to coordinates for the tree (the full scrollable area of the tree).
--
-- Removed in Gtk3.
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
#endif
#endif

#if GTK_CHECK_VERSION(2,12,0)
-- | Converts bin window coordinates to coordinates for the tree (the full scrollable area of the tree).
treeViewConvertBinWindowToTreeCoords :: TreeViewClass self => self
 -> Point -- ^ @(bx, by)@ - bin window X and Y coordinates
 -> IO Point -- ^ @(tx, ty)@ returns tree X and Y coordinates
treeViewConvertBinWindowToTreeCoords self (bx, by) =
  alloca $ \txPtr ->
  alloca $ \tyPtr -> do
  {# call unsafe tree_view_convert_bin_window_to_tree_coords #}
    (toTreeView self)
    (fromIntegral bx)
    (fromIntegral by)
    txPtr
    tyPtr
  tx <- peek txPtr
  ty <- peek tyPtr
  return (fromIntegral tx, fromIntegral ty)

-- | Converts bin window coordinates (see 'treeViewGetBinWindow' to widget relative coordinates.
treeViewConvertBinWindowToWidgetCoords :: TreeViewClass self => self
 -> Point -- ^ @(bx, by)@ - bin window X and Y coordinates
 -> IO Point -- ^ @(wx, wy)@ returns widget X and Y coordinates
treeViewConvertBinWindowToWidgetCoords self (bx, by) =
  alloca $ \wxPtr ->
  alloca $ \wyPtr -> do
  {# call unsafe tree_view_convert_bin_window_to_widget_coords #}
    (toTreeView self)
    (fromIntegral bx)
    (fromIntegral by)
    wxPtr
    wyPtr
  wx <- peek wxPtr
  wy <- peek wyPtr
  return (fromIntegral wx, fromIntegral wy)

-- | Converts tree coordinates (coordinates in full scrollable area of the tree) to bin window
-- coordinates.
treeViewConvertTreeToBinWindowCoords :: TreeViewClass self => self
 -> Point -- ^ @(tx, ty)@ - tree X and Y coordinates
 -> IO Point -- ^ @(bx, by)@ returns bin window X and Y coordinates
treeViewConvertTreeToBinWindowCoords self (tx, ty) =
  alloca $ \bxPtr ->
  alloca $ \byPtr -> do
  {# call unsafe tree_view_convert_tree_to_bin_window_coords #}
    (toTreeView self)
    (fromIntegral tx)
    (fromIntegral ty)
    bxPtr
    byPtr
  bx <- peek bxPtr
  by <- peek byPtr
  return (fromIntegral bx, fromIntegral by)

-- | Converts tree coordinates (coordinates in full scrollable area of the tree) to widget coordinates.
treeViewConvertTreeToWidgetCoords :: TreeViewClass self => self
 -> Point -- ^ @(tx, ty)@ - tree X and Y coordinates
 -> IO Point -- ^ @(wx, wy)@ returns widget X and Y coordinates
treeViewConvertTreeToWidgetCoords self (wx, wy) =
  alloca $ \bxPtr ->
  alloca $ \byPtr -> do
  {# call unsafe tree_view_convert_tree_to_widget_coords #}
    (toTreeView self)
    (fromIntegral wx)
    (fromIntegral wy)
    bxPtr
    byPtr
  bx <- peek bxPtr
  by <- peek byPtr
  return (fromIntegral bx, fromIntegral by)

-- | Converts widget coordinates to coordinates for the window (see 'treeViewGetBinWindow' ).
treeViewConvertWidgetToBinWindowCoords :: TreeViewClass self => self
 -> Point -- ^ @(wx, wy)@ - widget X and Y coordinates
 -> IO Point -- ^ @(bx, by)@ returns bin window X and Y coordinates
treeViewConvertWidgetToBinWindowCoords self (wx, wy) =
  alloca $ \bxPtr ->
  alloca $ \byPtr -> do
  {# call unsafe tree_view_convert_widget_to_bin_window_coords #}
    (toTreeView self)
    (fromIntegral wx)
    (fromIntegral wy)
    bxPtr
    byPtr
  bx <- peek bxPtr
  by <- peek byPtr
  return (fromIntegral bx, fromIntegral by)

-- | Converts widget coordinates to coordinates for the tree (the full scrollable area of the tree).
treeViewConvertWidgetToTreeCoords :: TreeViewClass self => self
 -> Point -- ^ @(wx, wy)@ - bin window X and Y coordinates
 -> IO Point -- ^ @(tx, ty)@ returns tree X and Y coordinates
treeViewConvertWidgetToTreeCoords self (wx, wy) =
  alloca $ \txPtr ->
  alloca $ \tyPtr -> do
  {# call unsafe tree_view_convert_widget_to_tree_coords #}
    (toTreeView self)
    (fromIntegral wx)
    (fromIntegral wy)
    txPtr
    tyPtr
  tx <- peek txPtr
  ty <- peek tyPtr
  return (fromIntegral tx, fromIntegral ty)
#endif

#if GTK_MAJOR_VERSION < 3
-- | Creates a 'Pixmap' representation of the row at the given path. This image
-- can be used for a drag icon.
--
-- Removed in Gtk3.
treeViewCreateRowDragIcon :: TreeViewClass self => self
 -> TreePath
 -> IO Pixmap
treeViewCreateRowDragIcon self path =
  wrapNewGObject mkPixmap $
  withTreePath path $ \path ->
  {# call unsafe tree_view_create_row_drag_icon #}
    (toTreeView self)
    path
#endif

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
-- using the \"start-interactive-search\" key binding. In any case,
-- a predicate that compares a row of the model with the text the user
-- has typed must be set using 'treeViewSetSearchEqualFunc'.
--
treeViewSetEnableSearch :: TreeViewClass self => self -> Bool -> IO ()
treeViewSetEnableSearch self enableSearch =
  {# call tree_view_set_enable_search #}
    (toTreeView self)
    (fromBool enableSearch)

-- %hash c:ecc5 d:bed6
-- | Gets the column searched on by the interactive search code.
--
treeViewGetSearchColumn :: (TreeViewClass self, GlibString string) => self
 -> IO (ColumnId row string) -- ^ returns the column the interactive search code searches in.
treeViewGetSearchColumn self =
  liftM (makeColumnIdString . fromIntegral) $
  {# call unsafe tree_view_get_search_column #}
    (toTreeView self)

-- %hash c:d0d0
-- | Sets @column@ as the column where the interactive search code should
-- search in.
--
-- If the sort column is set, users can use the \"start-interactive-search\"
-- key binding to bring up search popup. The enable-search property controls
-- whether simply typing text will also start an interactive search.
--
-- Note that @column@ refers to a column of the model. Furthermore, the
-- search column is not used if a comparison function is set, see
-- 'treeViewSetSearchEqualFunc'.
--
treeViewSetSearchColumn :: (TreeViewClass self, GlibString string) => self
 -> (ColumnId row string) -- ^ @column@ - the column of the model to search in, or -1 to disable
        -- searching
 -> IO ()
treeViewSetSearchColumn self column =
  {# call tree_view_set_search_column #}
    (toTreeView self)
    (fromIntegral (columnIdToNumber column))


-- | Set the predicate to test for equality.
--
-- * The predicate must returns @True@ if the text entered by the user
--   and the row of the model match. Calling this function will overwrite
--   the 'treeViewSearchColumn' (which isn't used anyway when a comparison
--   function is installed).
--
treeViewSetSearchEqualFunc :: (TreeViewClass self, GlibString string) => self
 -> Maybe (string -> TreeIter -> IO Bool)
 -> IO ()
treeViewSetSearchEqualFunc self (Just pred) = do
  fPtr <- mkTreeViewSearchEqualFunc (\_ _ keyPtr iterPtr _ -> do
    key <- peekUTFString keyPtr
    iter <- peek iterPtr
    liftM (fromBool . not) $ pred key iter)
  {# call tree_view_set_search_equal_func #} (toTreeView self) fPtr
    (castFunPtrToPtr fPtr) destroyFunPtr
  {# call tree_view_set_search_column #} (toTreeView self) 0
treeViewSetSearchEqualFunc self Nothing = do
  {# call tree_view_set_search_equal_func #} (toTreeView self)
    nullFunPtr nullPtr nullFunPtr
  {# call tree_view_set_search_column #} (toTreeView self) (-1)

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


#if GTK_CHECK_VERSION(2,10,0)
-- %hash c:88cb d:65c9
-- | Returns whether all header columns are clickable.
--
-- * Available since Gtk+ version 2.10
--
treeViewGetHeadersClickable :: TreeViewClass self => self
 -> IO Bool -- ^ returns @True@ if all header columns are clickable, otherwise
            -- @False@
treeViewGetHeadersClickable self =
  liftM toBool $
  {# call gtk_tree_view_get_headers_clickable #}
    (toTreeView self)
#endif

#if GTK_CHECK_VERSION(2,8,0)
-- %hash c:1d81 d:3587
-- | Return the first and last visible path.
-- Note that there may be invisible paths in between.
--
-- * Available since Gtk+ version 2.8
--
treeViewGetVisibleRange :: TreeViewClass self => self
 -> IO (TreePath, TreePath)     -- ^ the first and the last node that is visible
treeViewGetVisibleRange self  = alloca $ \startPtr -> alloca $ \endPtr -> do
  valid <- liftM toBool $
    {# call gtk_tree_view_get_visible_range #}
    (toTreeView self) (castPtr startPtr) (castPtr endPtr)
  if not valid then return ([],[]) else do
    startTPPtr <- peek startPtr
    endTPPtr <- peek endPtr
    startPath <- fromTreePath startTPPtr
    endPath <- fromTreePath endTPPtr
    return (startPath, endPath)

#endif

#if GTK_CHECK_VERSION(2,10,0)
-- %hash c:61e1 d:3a0a
-- | Turns @treeView@ into a drop destination for automatic DND.
--
treeViewEnableModelDragDest :: TreeViewClass self => self
  -> TargetList                -- ^ @targets@ - the list of targets that the
                               -- the view will support
  -> [DragAction]              -- ^ @actions@ - flags denoting the possible actions
                               -- for a drop into this widget
  -> IO ()
treeViewEnableModelDragDest self targets actions =
  alloca $ \nTargetsPtr -> do
  tlPtr <- {#call unsafe gtk_target_table_new_from_list#} targets nTargetsPtr
  nTargets <- peek nTargetsPtr
  {# call gtk_tree_view_enable_model_drag_dest #}
    (toTreeView self)
    tlPtr
    nTargets
    ((fromIntegral . fromFlags) actions)
  {#call unsafe gtk_target_table_free#} tlPtr nTargets

-- %hash c:1df9 d:622
-- | Turns @treeView@ into a drag source for automatic DND.
--
treeViewEnableModelDragSource :: TreeViewClass self => self
 -> [Modifier]                -- ^ @startButtonMask@ - Mask of allowed buttons
                              -- to start drag
 -> TargetList                -- ^ @targets@ - the list of targets that the
                              -- the view will support
 -> [DragAction]              -- ^ @actions@ - flags denoting the possible actions
                              -- for a drag from this widget
 -> IO ()
treeViewEnableModelDragSource self startButtonMask targets actions =
  alloca $ \nTargetsPtr -> do
  tlPtr <- {#call unsafe gtk_target_table_new_from_list#} targets nTargetsPtr
  nTargets <- peek nTargetsPtr
  {# call gtk_tree_view_enable_model_drag_source #}
    (toTreeView self)
    ((fromIntegral . fromFlags) startButtonMask)
    tlPtr
    nTargets
    ((fromIntegral . fromFlags) actions)
  {#call unsafe gtk_target_table_free#} tlPtr nTargets

-- %hash c:5201 d:f3be
-- | Undoes the effect of 'treeViewEnableModelDragSource'.
--
treeViewUnsetRowsDragSource :: TreeViewClass self => self -> IO ()
treeViewUnsetRowsDragSource self =
  {# call gtk_tree_view_unset_rows_drag_source #}
    (toTreeView self)

-- %hash c:e31e d:323d
-- | Undoes the effect of 'treeViewEnableModelDragDest'.
--
treeViewUnsetRowsDragDest :: TreeViewClass self => self -> IO ()
treeViewUnsetRowsDragDest self =
  {# call gtk_tree_view_unset_rows_drag_dest #}
    (toTreeView self)

-- %hash c:3355 d:3bbe
-- | Returns the 'Entry' which is currently in use as interactive search entry
-- for @treeView@. In case the built-in entry is being used, @Nothing@ will be
-- returned.
--
-- * Available since Gtk+ version 2.10
--
treeViewGetSearchEntry :: TreeViewClass self => self
 -> IO (Maybe Entry) -- ^ returns the entry currently in use as search entry.
treeViewGetSearchEntry self = do
  ePtr <- {# call gtk_tree_view_get_search_entry #}
    (toTreeView self)
  if ePtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkEntry (return ePtr)

-- %hash c:5e11 d:8ec5
-- | Sets the entry which the interactive search code will use for this
-- @treeView@. This is useful when you want to provide a search entry in our
-- interface at all time at a fixed position. Passing @Nothing@ for @entry@
-- will make the interactive search code use the built-in popup entry again.
--
-- * Available since Gtk+ version 2.10
--
treeViewSetSearchEntry :: (TreeViewClass self, EntryClass entry) => self
 -> (Maybe entry)
          -- ^ @entry@ - the entry the interactive search code of @treeView@
          -- should use or @Nothing@
 -> IO ()
treeViewSetSearchEntry self (Just entry) =
  {# call gtk_tree_view_set_search_entry #}
    (toTreeView self)
    (toEntry entry)
treeViewSetSearchEntry self Nothing =
  {# call gtk_tree_view_set_search_entry #}
    (toTreeView self)
    (Entry nullForeignPtr)
#endif

#if GTK_CHECK_VERSION(2,6,0)
-- %hash c:6326 d:a050
-- | Sets the row separator function, which is used to determine whether a row
-- should be drawn as a separator. If the row separator function is @Nothing@,
-- no separators are drawn. This is the default value.
--
-- * Available since Gtk+ version 2.6
--
treeViewSetRowSeparatorFunc :: TreeViewClass self => self
 -> Maybe (TreeIter -> IO Bool)     -- ^ @func@ - a callback function that
                                    -- returns @True@ if the given row of
                                    -- the model should be drawn as separator
 -> IO ()
treeViewSetRowSeparatorFunc self Nothing =
  {# call gtk_tree_view_set_row_separator_func #}
    (toTreeView self) nullFunPtr nullPtr nullFunPtr
treeViewSetRowSeparatorFunc self (Just func) = do
  funcPtr <- mkTreeViewRowSeparatorFunc $ \_ tiPtr _ -> do
    ti <- peekTreeIter tiPtr
    liftM fromBool $ func ti
  {# call gtk_tree_view_set_row_separator_func #}
    (toTreeView self) funcPtr (castFunPtrToPtr funcPtr) destroyFunPtr

{#pointer TreeViewRowSeparatorFunc #}

foreign import ccall "wrapper" mkTreeViewRowSeparatorFunc ::
  (Ptr TreeModel -> Ptr TreeIter -> Ptr () -> IO {#type gboolean#}) ->
  IO TreeViewRowSeparatorFunc

#if GTK_CHECK_VERSION(2,10,0)
-- %hash c:778a d:eacd
-- | Returns whether rubber banding is turned on for @treeView@. If the
-- selection mode is 'SelectionMultiple', rubber banding will allow the user to
-- select multiple rows by dragging the mouse.
--
-- * Available since Gtk+ version 2.10
--
treeViewGetRubberBanding :: TreeViewClass self => self
 -> IO Bool -- ^ returns @True@ if rubber banding in @treeView@ is enabled.
treeViewGetRubberBanding self =
  liftM toBool $
  {# call gtk_tree_view_get_rubber_banding #}
    (toTreeView self)

-- %hash c:4a69 d:93aa
-- | Enables or disables rubber banding in @treeView@. If the selection mode
-- is 'SelectionMultiple', rubber banding will allow the user to select
-- multiple rows by dragging the mouse.
--
-- * Available since Gtk+ version 2.10
--
treeViewSetRubberBanding :: TreeViewClass self => self
 -> Bool -- ^ @enable@ - @True@ to enable rubber banding
 -> IO ()
treeViewSetRubberBanding self enable =
  {# call gtk_tree_view_set_rubber_banding #}
    (toTreeView self)
    (fromBool enable)

-- %hash c:c8f8 d:c47
-- | Returns whether or not tree lines are drawn in @treeView@.
--
-- * Available since Gtk+ version 2.10
--
treeViewGetEnableTreeLines :: TreeViewClass self => self
 -> IO Bool -- ^ returns @True@ if tree lines are drawn in @treeView@, @False@
            -- otherwise.
treeViewGetEnableTreeLines self =
  liftM toBool $
  {# call gtk_tree_view_get_enable_tree_lines #}
    (toTreeView self)

-- %hash c:205d d:1df9
-- | Sets whether to draw lines interconnecting the expanders in @treeView@.
-- This does not have any visible effects for lists.
--
-- * Available since Gtk+ version 2.10
--
treeViewSetEnableTreeLines :: TreeViewClass self => self
 -> Bool -- ^ @enabled@ - @True@ to enable tree line drawing, @False@
         -- otherwise.
 -> IO ()
treeViewSetEnableTreeLines self enabled =
  {# call gtk_tree_view_set_enable_tree_lines #}
    (toTreeView self)
    (fromBool enabled)

-- | Grid lines.
{#enum TreeViewGridLines {underscoreToCase}#}

-- %hash c:cd40 d:fe96
-- | Returns which grid lines are enabled in @treeView@.
--
-- * Available since Gtk+ version 2.10
--
treeViewGetGridLines :: TreeViewClass self => self
 -> IO TreeViewGridLines -- ^ returns a 'TreeViewGridLines' value indicating
                         -- which grid lines are enabled.
treeViewGetGridLines self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_tree_view_get_grid_lines #}
    (toTreeView self)

-- %hash c:74b0 d:79f0
-- | Sets which grid lines to draw in @treeView@.
--
-- * Available since Gtk+ version 2.10
--
treeViewSetGridLines :: TreeViewClass self => self
 -> TreeViewGridLines -- ^ @gridLines@ - a 'TreeViewGridLines' value
                      -- indicating which grid lines to enable.
 -> IO ()
treeViewSetGridLines self gridLines =
  {# call gtk_tree_view_set_grid_lines #}
    (toTreeView self)
    ((fromIntegral . fromEnum) gridLines)
#endif
#endif

#if GTK_CHECK_VERSION(2,12,0)
-- | Sets the tip area of @tooltip@ to be the area covered by @path@. See also
-- 'treeViewTooltipColumn' for a simpler alternative. See also
-- 'tooltipSetTipArea'.
treeViewSetTooltipRow :: TreeViewClass self => self
  -> Tooltip -- ^ the @tooltip@
  -> TreePath -- ^ @path@ - the position of the @tooltip@
  -> IO ()
treeViewSetTooltipRow self tip path =
  withTreePath path $ \path ->
  {#call gtk_tree_view_set_tooltip_row #} (toTreeView self) tip path

-- | Sets the tip area of tooltip to the area path, column and cell have in
-- common. For example if @path@ is @Nothing@ and @column@ is set, the tip area will be
-- set to the full area covered by column. See also
-- 'tooltipSetTipArea'. Note that if @path@ is not specified and @cell@ is
-- set and part of a column containing the expander, the tooltip might not
-- show and hide at the correct position. In such cases @path@ must be set to
-- the current node under the mouse cursor for this function to operate
-- correctly. See also 'treeViewTooltipColumn' for a simpler alternative.
--
treeViewSetTooltipCell :: (TreeViewClass self, TreeViewColumnClass col,
                           CellRendererClass renderer) => self
  -> Tooltip -- ^ the @tooltip@
  -> Maybe TreePath -- ^ @path@ at which the tip should be shown
  -> Maybe col -- ^ @column@ at which the tip should be shown
  -> Maybe renderer -- ^ the @renderer@ for which to show the tip
  -> IO ()
treeViewSetTooltipCell self tip mPath mColumn mRenderer =
  (case mPath of Just path -> withTreePath path
                 Nothing -> \f -> f (NativeTreePath nullPtr)) $ \path -> do
  {#call gtk_tree_view_set_tooltip_cell#} (toTreeView self) tip path
    (maybe (TreeViewColumn nullForeignPtr) toTreeViewColumn mColumn)
    (maybe (CellRenderer nullForeignPtr) toCellRenderer mRenderer)

-- | This function is supposed to be used in a 'widgetQueryTooltip' signal handler
-- for this 'TreeView'. The @point@ value which is received in the
-- signal handler should be passed to this function without modification. A
-- return value of @Just iter@ indicates that there is a tree view row at the given
-- coordinates (if @Just (x,y)@ is passed in, denoting a mouse position), resp.
-- the cursor row (if @Nothing@ is passed in, denoting a keyboard request).
--
treeViewGetTooltipContext :: TreeViewClass self => self
  -> Maybe Point -- ^ @point@ - the coordinates of the mouse or @Nothing@
                 --   if a keyboard tooltip is to be generated
  -> IO (Maybe TreeIter) -- ^ @Just iter@ if a tooltip should be shown for that row
treeViewGetTooltipContext self (Just (x,y)) =
  alloca $ \xPtr -> alloca $ \yPtr -> receiveTreeIter $
    {#call gtk_tree_view_get_tooltip_context#} (toTreeView self)
    xPtr yPtr 0 nullPtr nullPtr
treeViewGetTooltipContext self Nothing =
  receiveTreeIter $
    {#call gtk_tree_view_get_tooltip_context#} (toTreeView self)
    nullPtr nullPtr 1 nullPtr nullPtr
#endif

--------------------
-- Attributes

-- | The model for the tree view.
--
treeViewModel :: TreeViewClass self => Attr self (Maybe TreeModel)
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

-- %hash c:e732
-- | Model column to search through when searching through code.
--
-- Default value: 'invalidColumnId'
--
treeViewSearchColumn :: (TreeViewClass self, GlibString string) => Attr self (ColumnId row string)
treeViewSearchColumn = newAttr
  treeViewGetSearchColumn
  treeViewSetSearchColumn

#if GTK_CHECK_VERSION(2,4,0)
-- %hash c:c7ff d:24d1
-- | Setting the 'treeViewFixedHeightMode' property to @True@ speeds up 'TreeView'
-- by assuming that all rows have the same height. Only enable this option if
-- all rows are the same height. Please see 'treeViewSetFixedHeightMode' for
-- more information on this option.
--
-- Default value: @False@
--
-- * Available since Gtk+ version 2.4
--
treeViewFixedHeightMode :: TreeViewClass self => Attr self Bool
treeViewFixedHeightMode = newAttrFromBoolProperty "fixed-height-mode"

#if GTK_CHECK_VERSION(2,6,0)
-- %hash c:2026 d:839a
-- | Enables of disables the hover selection mode of @treeView@. Hover
-- selection makes the selected row follow the pointer. Currently, this works
-- only for the selection modes 'SelectionSingle' and 'SelectionBrowse'.
--
-- This mode is primarily intended for 'TreeView's in popups, e.g. in
-- 'ComboBox' or 'EntryCompletion'.
--
-- Default value: @False@
--
-- * Available since Gtk+ version 2.6
--
treeViewHoverSelection :: TreeViewClass self => Attr self Bool
treeViewHoverSelection = newAttrFromBoolProperty "hover-selection"

-- %hash c:c694 d:3f15
-- | Enables of disables the hover expansion mode of @treeView@. Hover
-- expansion makes rows expand or collaps if the pointer moves over them.
--
-- This mode is primarily intended for 'TreeView's in popups, e.g. in
-- 'ComboBox' or 'EntryCompletion'.
--
-- Default value: @False@
--
-- * Available since Gtk+ version 2.6
--
treeViewHoverExpand :: TreeViewClass self => Attr self Bool
treeViewHoverExpand = newAttrFromBoolProperty "hover-expand"
#endif
#endif

-- %hash c:b409 d:2ed2
-- | View has expanders.
--
-- Default value: @True@
--
treeViewShowExpanders :: TreeViewClass self => Attr self Bool
treeViewShowExpanders = newAttrFromBoolProperty "show-expanders"

-- %hash c:f0e5 d:9017
-- | Extra indentation for each level.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
treeViewLevelIndentation :: TreeViewClass self => Attr self Int
treeViewLevelIndentation = newAttrFromIntProperty "level-indentation"

-- %hash c:a647 d:9e53
-- | Whether to enable selection of multiple items by dragging the mouse
-- pointer.
--
-- Default value: @False@
--
treeViewRubberBanding :: TreeViewClass self => Attr self Bool
treeViewRubberBanding = newAttrFromBoolProperty "rubber-banding"

#if GTK_CHECK_VERSION(2,10,0)
-- %hash c:e926 d:86a8
-- | Whether grid lines should be drawn in the tree view.
--
-- Default value: 'TreeViewGridLinesNone'
--
treeViewEnableGridLines :: TreeViewClass self => Attr self TreeViewGridLines
treeViewEnableGridLines = newAttrFromEnumProperty "enable-grid-lines"
                            {# call pure unsafe gtk_tree_view_grid_lines_get_type #}
#endif

-- %hash c:a7eb d:4c53
-- | Whether tree lines should be drawn in the tree view.
--
-- Default value: @False@
--
treeViewEnableTreeLines :: TreeViewClass self => Attr self Bool
treeViewEnableTreeLines = newAttrFromBoolProperty "enable-tree-lines"

#if GTK_CHECK_VERSION(2,10,0)
-- %hash c:688c d:cbcd
-- | \'gridLines\' property. See 'treeViewGetGridLines' and
-- 'treeViewSetGridLines'
--
treeViewGridLines :: TreeViewClass self => Attr self TreeViewGridLines
treeViewGridLines = newAttr
  treeViewGetGridLines
  treeViewSetGridLines

-- %hash c:9cbe d:2962
-- | \'searchEntry\' property. See 'treeViewGetSearchEntry' and
-- 'treeViewSetSearchEntry'
--
treeViewSearchEntry :: (TreeViewClass self, EntryClass entry) => ReadWriteAttr self (Maybe Entry) (Maybe entry)
treeViewSearchEntry = newAttr
  treeViewGetSearchEntry
  treeViewSetSearchEntry
#endif

#if GTK_CHECK_VERSION(2,12,0)
-- | The column for which to show tooltips.
--
-- If you only plan to have simple (text-only) tooltips on full rows, you can
-- use this function to have 'TreeView' handle these automatically for you.
-- @column@ should be set to a column in model containing the tooltip texts,
-- or @-1@ to disable this feature. When enabled, 'widgetHasTooltip' will be
-- set to @True@ and this view will connect to the 'widgetQueryTooltip' signal
-- handler.
--
-- Note that the signal handler sets the text as 'Markup',
-- so \&, \<, etc have to be escaped in the text.
--
-- Default value: 'invalidColumnId'
--
treeViewTooltipColumn :: (TreeViewClass self, GlibString string) => Attr self (ColumnId row string)
treeViewTooltipColumn = newAttr
  (\self -> liftM (makeColumnIdString . fromIntegral) $
  {# call unsafe tree_view_get_tooltip_column #}
    (toTreeView self)
  )
  (\self column ->
  {# call tree_view_set_tooltip_column #}
    (toTreeView self)
    (fromIntegral (columnIdToNumber column))
  )
#endif

--------------------
-- Signals

-- %hash c:9fc5 d:3e66
-- | The given row is about to be expanded (show its children nodes). Use this
-- signal if you need to control the expandability of individual rows.
--
testExpandRow :: TreeViewClass self => Signal self (TreeIter -> TreePath -> IO Bool)
testExpandRow = Signal (connect_BOXED_BOXED__BOOL "test-expand-row" peek readNTP)

-- %hash c:20de d:96a3
-- | The given row is about to be collapsed (hide its children nodes). Use
-- this signal if you need to control the collapsibility of individual rows.
--
testCollapseRow :: TreeViewClass self => Signal self (TreeIter -> TreePath -> IO Bool)
testCollapseRow = Signal (connect_BOXED_BOXED__BOOL "test-collapse-row" peek readNTP)

-- %hash c:16dc d:b113
-- | The given row has been expanded (child nodes are shown).
--
rowExpanded :: TreeViewClass self => Signal self (TreeIter -> TreePath -> IO ())
rowExpanded = Signal (connect_BOXED_BOXED__NONE "row-expanded" peek readNTP)

-- | A row was activated.
--
-- * Activation usually means the user has pressed return on a row.
--
rowActivated :: TreeViewClass self => Signal self (TreePath -> TreeViewColumn -> IO ())
rowActivated = Signal (connect_BOXED_OBJECT__NONE "row-activated" readNTP)

-- %hash c:9ee6 d:325e
-- | The given row has been collapsed (child nodes are hidden).
--
rowCollapsed :: TreeViewClass self => Signal self (TreeIter -> TreePath -> IO ())
rowCollapsed = Signal (connect_BOXED_BOXED__NONE "row-collapsed" peek readNTP)

-- %hash c:4350 d:4f94
-- | The number of columns of the treeview has changed.
--
columnsChanged :: TreeViewClass self => Signal self (IO ())
columnsChanged = Signal (connect_NONE__NONE "columns-changed")

-- %hash c:6487 d:5b57
-- | The position of the cursor (focused cell) has changed.
--
cursorChanged :: TreeViewClass self => Signal self (IO ())
cursorChanged = Signal (connect_NONE__NONE "cursor-changed")

--------------------
-- Deprecated Signals

#ifndef DISABLE_DEPRECATED

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
#endif
