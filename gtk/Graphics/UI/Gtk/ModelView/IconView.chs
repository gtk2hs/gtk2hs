{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget IconView
--
--  Author : Duncan Coutts
--
--  Created: 25 March 2005
--
--  Copyright (C) 2005-2007 Duncan Coutts, Axel Simon
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
-- A widget which displays a list of icons in a grid
--
-- * Module available since Gtk+ version 2.6
--
module Graphics.UI.Gtk.ModelView.IconView (
-- * Detail
--
-- | 'IconView' provides an alternative view on a list model. It displays the
-- model as a grid of icons with labels. Like 'TreeView', it allows to select
-- one or multiple items (depending on the selection mode, see
-- 'iconViewSetSelectionMode'). In addition to selection with the arrow keys,
-- 'IconView' supports rubberband selection, which is controlled by dragging
-- the pointer.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----IconView
-- @

#if GTK_CHECK_VERSION(2,6,0)
-- * Types
  IconView,
  IconViewClass,
  castToIconView, gTypeIconView,
  toIconView,

-- * Constructors
  iconViewNew,
  iconViewNewWithModel,

-- * Methods
  iconViewSetModel,
  iconViewGetModel,
  iconViewSetTextColumn,
  iconViewGetTextColumn,
  iconViewSetMarkupColumn,
  iconViewGetMarkupColumn,
  iconViewSetPixbufColumn,
  iconViewGetPixbufColumn,
  iconViewGetPathAtPos,
  iconViewSelectedForeach,
  iconViewSetSelectionMode,
  iconViewGetSelectionMode,
#if GTK_MAJOR_VERSION < 3
  iconViewSetOrientation,
  iconViewGetOrientation,
#endif
  iconViewSetColumns,
  iconViewGetColumns,
  iconViewSetItemWidth,
  iconViewGetItemWidth,
  iconViewSetSpacing,
  iconViewGetSpacing,
  iconViewSetRowSpacing,
  iconViewGetRowSpacing,
  iconViewSetColumnSpacing,
  iconViewGetColumnSpacing,
  iconViewSetMargin,
  iconViewGetMargin,
  iconViewSelectPath,
  iconViewUnselectPath,
  iconViewPathIsSelected,
  iconViewGetSelectedItems,
  iconViewSelectAll,
  iconViewUnselectAll,
  iconViewItemActivated,
#if GTK_CHECK_VERSION(2,8,0)
  iconViewGetItemAtPos,
  iconViewSetCursor,
  iconViewGetCursor,
  iconViewScrollToPath,
  iconViewGetVisibleRange,
#if GTK_CHECK_VERSION(2,10,0)
  iconViewEnableModelDragSource,
  iconViewEnableModelDragDest,
  iconViewUnsetModelDragSource,
  iconViewUnsetModelDragDest,
#endif
  iconViewSetReorderable,
  iconViewGetReorderable,
#endif
#if GTK_CHECK_VERSION(2,22,0)
  iconViewGetItemRow,
  iconViewGetItemColumn,
#endif

-- * Attributes
  iconViewSelectionMode,
  iconViewTextColumn,
  iconViewMarkupColumn,
  iconViewPixbufColumn,
  iconViewModel,
  iconViewColumns,
  iconViewItemWidth,
  iconViewSpacing,
  iconViewRowSpacing,
  iconViewColumnSpacing,
  iconViewMargin,
  iconViewOrientation,
#if GTK_CHECK_VERSION(2,8,0)
  iconViewReorderable,
#endif
#if GTK_CHECK_VERSION(2,22,0)
  iconViewItemOrientation,
#endif

-- * Signals
  setIconViewScrollAdjustments,
  itemActivated,
  selectionChanged
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GList                        (fromGList)
import System.Glib.Flags
import Graphics.UI.Gtk.Abstract.Object          (makeNewObject)
import Graphics.UI.Gtk.Gdk.Enums                (DragAction(..))
import Graphics.UI.Gtk.Gdk.Events               (Modifier(..))
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.General.Enums#}        (Orientation, SelectionMode)
{#import Graphics.UI.Gtk.ModelView.TreeModel#}
{#import Graphics.UI.Gtk.ModelView.Types#}
{#import Graphics.UI.Gtk.General.DNDTypes#}     (TargetList(..))

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,6,0)
--------------------
-- Constructors

-- | Creates a new 'IconView' widget
--
iconViewNew :: IO IconView
iconViewNew =
  makeNewObject mkIconView $
  liftM (castPtr :: Ptr Widget -> Ptr IconView) $
  {# call gtk_icon_view_new #}

-- %hash c:dbf4
-- | Creates a new 'IconView' widget with the model @model@ and defines
--   how to extract a string and a pixbuf from the model.
--
iconViewNewWithModel :: TreeModelClass model =>
    model -- ^ @model@ - The model.
 -> IO IconView
iconViewNewWithModel model =
  makeNewObject mkIconView $
    liftM (castPtr :: Ptr Widget -> Ptr IconView) $
    {# call gtk_icon_view_new_with_model #}
    (toTreeModel model)

--------------------
-- Methods

-- %hash c:5ba8 d:c5c8
-- | Sets the model for a 'IconView'. If the @iconView@ already has a model
-- set, it will remove it before setting the new model. If @model@ is
-- @Nothing@, then it will unset the old model.
--
iconViewSetModel :: (IconViewClass self, TreeModelClass model) => self
 -> Maybe model -- ^ @model@ - The model.
 -> IO ()
iconViewSetModel self model =
  {# call gtk_icon_view_set_model #}
    (toIconView self)
    (maybe (TreeModel nullForeignPtr) toTreeModel model)

-- %hash c:6709 d:c0c5
-- | Returns the model the 'IconView' is based on. Returns @Nothing@ if the
-- model is unset.
--
iconViewGetModel :: IconViewClass self => self
 -> IO (Maybe TreeModel) -- ^ returns a 'TreeModel', or @Nothing@ if none is
                         -- currently being used.
iconViewGetModel self =
  maybeNull (makeNewGObject mkTreeModel) $
  {# call unsafe gtk_icon_view_get_model #}
    (toIconView self)

-- %hash c:c3fe d:fb7b
-- | Sets the column of the text for entries in the 'IconView'. If a markup
-- source is set using 'iconViewSetMarkupSource', then the text source is
-- ignored.
--
iconViewSetTextColumn :: (IconViewClass self, GlibString string) => self
 -> ColumnId row string -- ^ @column@ - A column in the currently used model.
 -> IO ()
iconViewSetTextColumn self column =
  {# call gtk_icon_view_set_text_column #}
    (toIconView self)
    ((fromIntegral . columnIdToNumber) column)

-- | Returns the column with text for @iconView@.
--
iconViewGetTextColumn :: (IconViewClass self, GlibString string) => self
 -> IO (ColumnId row string) -- ^ returns the text column, or 'invalidColumnId' if it's unset.
iconViewGetTextColumn self =
  liftM (makeColumnIdString . fromIntegral) $
  {# call gtk_icon_view_get_text_column #}
    (toIconView self)


-- %hash c:995f d:801c
-- | Sets the column of the text for entries in the 'IconView' as a markup
-- string (see 'Graphics.Rendering.Pango.Markup'). A text source that is set
-- using 'iconViewSetTextSource' is ignored once a markup source is set.
--
iconViewSetMarkupColumn :: (IconViewClass self, GlibString markup) => self
 -> ColumnId row markup -- ^ @column@ - A column in the currently used model.
 -> IO ()
iconViewSetMarkupColumn self column =
  {# call gtk_icon_view_set_markup_column #}
    (toIconView self)
    ((fromIntegral . columnIdToNumber) column)

-- | Returns the column with markup text for @iconView@.
--
iconViewGetMarkupColumn :: (IconViewClass self, GlibString markup) => self
 -> IO (ColumnId row markup) -- ^ returns the markup column, or 'invalidColumnId' if it's unset.
iconViewGetMarkupColumn self =
  liftM (makeColumnIdString . fromIntegral) $
  {# call gtk_icon_view_get_markup_column #}
    (toIconView self)


-- %hash c:4079 d:bf8
-- | Sets the column of the 'Graphics.UI.Gtk.Gdk.Pixbuf' for entries in the
-- 'IconView'.
--
iconViewSetPixbufColumn :: IconViewClass self => self
 -> ColumnId row Pixbuf -- ^ @column@ - A column in the currently used model.
 -> IO ()
iconViewSetPixbufColumn self column =
  {# call gtk_icon_view_set_pixbuf_column #}
    (toIconView self)
    ((fromIntegral . columnIdToNumber) column)

-- | Returns the column with pixbufs for @iconView@.
--
iconViewGetPixbufColumn :: IconViewClass self => self
 -> IO (ColumnId row Pixbuf) -- ^ returns the pixbuf column, or 'invalidColumnId' if it's unset.
iconViewGetPixbufColumn self =
  liftM (makeColumnIdPixbuf . fromIntegral) $
  {# call gtk_icon_view_get_pixbuf_column #}
    (toIconView self)

-- %hash c:2486 d:5e7
-- | Finds the path at the point (@x@, @y@), relative to widget coordinates.
-- See 'iconViewGetItemAtPos', if you are also interested in the cell at the
-- specified position.
--
iconViewGetPathAtPos :: IconViewClass self => self
 -> Int         -- ^ @x@ - The x position to be identified
 -> Int         -- ^ @y@ - The y position to be identified
 -> IO TreePath -- ^ returns The 'TreePath' corresponding to the icon or @[]@
                -- if no icon exists at that position.
iconViewGetPathAtPos self x y =
  {# call gtk_icon_view_get_path_at_pos #}
    (toIconView self)
    (fromIntegral x)
    (fromIntegral y)
  >>= fromTreePath

-- %hash c:dfc5
-- | Calls a function for each selected icon. Note that the model or selection
-- cannot be modified from within this function.
--
iconViewSelectedForeach :: IconViewClass self => self
 -> (TreePath -> IO ()) -- ^ @(\path -> ...)@ - The funcion to call for each
                        -- selected icon.
 -> IO ()
iconViewSelectedForeach self func = do
  funcPtr <- mkIconViewForeachFunc (\_ tpPtr _ -> do
    path <- peekTreePath tpPtr
    func path
    )
  {# call gtk_icon_view_selected_foreach #}
    (toIconView self)
    funcPtr
    nullPtr
  freeHaskellFunPtr funcPtr

{# pointer IconViewForeachFunc #}

foreign import ccall "wrapper" mkIconViewForeachFunc ::
  (Ptr IconView -> Ptr NativeTreePath -> Ptr () -> IO ()) -> IO IconViewForeachFunc

-- | Sets the selection mode of the @iconView@.
--
iconViewSetSelectionMode :: IconViewClass self => self
 -> SelectionMode -- ^ @mode@ - The selection mode
 -> IO ()
iconViewSetSelectionMode self mode =
  {# call gtk_icon_view_set_selection_mode #}
    (toIconView self)
    ((fromIntegral . fromEnum) mode)

-- | Gets the selection mode of the @iconView@.
--
iconViewGetSelectionMode :: IconViewClass self => self
 -> IO SelectionMode -- ^ returns the current selection mode
iconViewGetSelectionMode self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_icon_view_get_selection_mode #}
    (toIconView self)

#if GTK_MAJOR_VERSION < 3
-- | Sets the ::orientation property which determines whether the labels are
-- drawn beside the icons instead of below.
--
iconViewSetOrientation :: IconViewClass self => self
 -> Orientation -- ^ @orientation@ - the relative position of texts and icons
 -> IO ()
iconViewSetOrientation self orientation =
  {# call gtk_icon_view_set_orientation #}
    (toIconView self)
    ((fromIntegral . fromEnum) orientation)

-- | Returns the value of the ::orientation property which determines whether
-- the labels are drawn beside the icons instead of below.
--
iconViewGetOrientation :: IconViewClass self => self
 -> IO Orientation -- ^ returns the relative position of texts and icons
iconViewGetOrientation self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_icon_view_get_orientation #}
    (toIconView self)
#endif

-- %hash c:7d23 d:d4e7
-- | Sets the ::columns property which determines in how many columns the
-- icons are arranged. If @columns@ is -1, the number of columns will be chosen
-- automatically to fill the available area.
--
iconViewSetColumns :: IconViewClass self => self
 -> Int -- ^ @columns@ - the number of columns
 -> IO ()
iconViewSetColumns self columns =
  {# call gtk_icon_view_set_columns #}
    (toIconView self)
    (fromIntegral columns)

-- %hash c:f0f6 d:fc0e
-- | Returns the value of the ::columns property.
--
iconViewGetColumns :: IconViewClass self => self
 -> IO Int -- ^ returns the number of columns, or -1
iconViewGetColumns self =
  liftM fromIntegral $
  {# call gtk_icon_view_get_columns #}
    (toIconView self)

-- %hash c:643e d:b756
-- | Sets the ::item-width property which specifies the width to use for each
-- item. If it is set to -1, the icon view will automatically determine a
-- suitable item size.
--
iconViewSetItemWidth :: IconViewClass self => self
 -> Int -- ^ @itemWidth@ - the width for each item
 -> IO ()
iconViewSetItemWidth self itemWidth =
  {# call gtk_icon_view_set_item_width #}
    (toIconView self)
    (fromIntegral itemWidth)

-- %hash c:9f27 d:8569
-- | Returns the value of the ::item-width property.
--
iconViewGetItemWidth :: IconViewClass self => self
 -> IO Int -- ^ returns the width of a single item, or -1
iconViewGetItemWidth self =
  liftM fromIntegral $
  {# call gtk_icon_view_get_item_width #}
    (toIconView self)

-- %hash c:7e61 d:3186
-- | Sets the ::spacing property which specifies the space which is inserted
-- between the cells (i.e. the icon and the text) of an item.
--
iconViewSetSpacing :: IconViewClass self => self
 -> Int -- ^ @spacing@ - the spacing
 -> IO ()
iconViewSetSpacing self spacing =
  {# call gtk_icon_view_set_spacing #}
    (toIconView self)
    (fromIntegral spacing)

-- %hash c:5bc1 d:a1d2
-- | Returns the value of the ::spacing property.
--
iconViewGetSpacing :: IconViewClass self => self
 -> IO Int -- ^ returns the space between cells
iconViewGetSpacing self =
  liftM fromIntegral $
  {# call gtk_icon_view_get_spacing #}
    (toIconView self)

-- %hash c:dd08 d:730c
-- | Sets the ::row-spacing property which specifies the space which is
-- inserted between the rows of the icon view.
--
iconViewSetRowSpacing :: IconViewClass self => self
 -> Int -- ^ @rowSpacing@ - the row spacing
 -> IO ()
iconViewSetRowSpacing self rowSpacing =
  {# call gtk_icon_view_set_row_spacing #}
    (toIconView self)
    (fromIntegral rowSpacing)

-- %hash c:a040 d:bc37
-- | Returns the value of the ::row-spacing property.
--
iconViewGetRowSpacing :: IconViewClass self => self
 -> IO Int -- ^ returns the space between rows
iconViewGetRowSpacing self =
  liftM fromIntegral $
  {# call gtk_icon_view_get_row_spacing #}
    (toIconView self)

-- %hash c:3042 d:b4f8
-- | Sets the ::column-spacing property which specifies the space which is
-- inserted between the columns of the icon view.
--
iconViewSetColumnSpacing :: IconViewClass self => self
 -> Int -- ^ @columnSpacing@ - the column spacing
 -> IO ()
iconViewSetColumnSpacing self columnSpacing =
  {# call gtk_icon_view_set_column_spacing #}
    (toIconView self)
    (fromIntegral columnSpacing)

-- %hash c:3818 d:c1cd
-- | Returns the value of the ::column-spacing property.
--
iconViewGetColumnSpacing :: IconViewClass self => self
 -> IO Int -- ^ returns the space between columns
iconViewGetColumnSpacing self =
  liftM fromIntegral $
  {# call gtk_icon_view_get_column_spacing #}
    (toIconView self)

-- %hash c:990 d:d43c
-- | Sets the ::margin property which specifies the space which is inserted at
-- the top, bottom, left and right of the icon view.
--
iconViewSetMargin :: IconViewClass self => self
 -> Int -- ^ @margin@ - the margin
 -> IO ()
iconViewSetMargin self margin =
  {# call gtk_icon_view_set_margin #}
    (toIconView self)
    (fromIntegral margin)

-- %hash c:a116 d:6fab
-- | Returns the value of the ::margin property.
--
iconViewGetMargin :: IconViewClass self => self
 -> IO Int -- ^ returns the space at the borders
iconViewGetMargin self =
  liftM fromIntegral $
  {# call gtk_icon_view_get_margin #}
    (toIconView self)

-- %hash c:77b3
-- | Selects the row at @path@.
--
iconViewSelectPath :: IconViewClass self => self
 -> TreePath -- ^ @path@ - The 'TreePath' to be selected.
 -> IO ()
iconViewSelectPath self path =
  withTreePath path $ \path ->
  {# call gtk_icon_view_select_path #}
    (toIconView self)
    path

-- %hash c:7e5f
-- | Unselects the row at @path@.
--
iconViewUnselectPath :: IconViewClass self => self
 -> TreePath -- ^ @path@ - The 'TreePath' to be unselected.
 -> IO ()
iconViewUnselectPath self path =
  withTreePath path $ \path ->
  {# call gtk_icon_view_unselect_path #}
    (toIconView self)
    path

-- %hash c:8ea0
-- | Returns @True@ if the icon pointed to by @path@ is currently selected. If
-- @icon@ does not point to a valid location, @False@ is returned.
--
iconViewPathIsSelected :: IconViewClass self => self
 -> TreePath -- ^ @path@ - A 'TreePath' to check selection on.
 -> IO Bool  -- ^ returns @True@ if @path@ is selected.
iconViewPathIsSelected self path =
  liftM toBool $
  withTreePath path $ \path ->
  {# call gtk_icon_view_path_is_selected #}
    (toIconView self)
    path

-- %hash c:90f8 d:9c43
-- | Creates a list of paths of all selected items. Additionally, if you are
-- planning on modifying the model after calling this function, you may want
-- to convert the returned list into a list of 'TreeRowReference's. To do
-- this, you can use 'treeRowReferenceNew'.
--
iconViewGetSelectedItems :: IconViewClass self => self
 -> IO [TreePath] -- ^ returns a list of 'TreePath's, one for each selected row.
iconViewGetSelectedItems self =
  {# call gtk_icon_view_get_selected_items #}
    (toIconView self)
  >>= fromGList
  >>= mapM fromTreePath

-- | Selects all the icons. @iconView@ must has its selection mode set to
-- 'SelectionMultiple'.
--
iconViewSelectAll :: IconViewClass self => self -> IO ()
iconViewSelectAll self =
  {# call gtk_icon_view_select_all #}
    (toIconView self)

-- | Unselects all the icons.
--
iconViewUnselectAll :: IconViewClass self => self -> IO ()
iconViewUnselectAll self =
  {# call gtk_icon_view_unselect_all #}
    (toIconView self)

-- %hash c:6916
-- | Activates the item determined by @path@.
--
iconViewItemActivated :: IconViewClass self => self
 -> TreePath -- ^ @path@ - The 'TreePath' to be activated
 -> IO ()
iconViewItemActivated self path =
  withTreePath path $ \path ->
  {# call gtk_icon_view_item_activated #}
    (toIconView self)
    path


#if GTK_CHECK_VERSION(2,8,0)
-- %hash c:3122 d:346e
-- | Finds the path at the point (@x@, @y@), relative to widget coordinates.
-- In contrast to 'iconViewGetPathAtPos', this function also obtains the cell
-- at the specified position.
--
-- * Available since Gtk+ version 2.8
--
iconViewGetItemAtPos :: IconViewClass self => self
 -> Int                   -- ^ @x@ - The x position to be identified
 -> Int                   -- ^ @y@ - The y position to be identified
 -> IO (Maybe (TreePath, CellRenderer))
                          -- specified position
iconViewGetItemAtPos self x y =
  alloca $ \pathPtrPtr -> alloca $ \crPtrPtr -> do
  success <- liftM toBool $ {# call gtk_icon_view_get_item_at_pos #}
    (toIconView self)
    (fromIntegral x)
    (fromIntegral y)
    (castPtr pathPtrPtr)
    (castPtr crPtrPtr)
  if not success then return Nothing else do
  pathPtr <- peek pathPtrPtr
  crPtr <- peek crPtrPtr
  path <- fromTreePath pathPtr
  cr <- makeNewGObject mkCellRenderer (return crPtr)
  return (Just (path, cr))

-- %hash c:357b d:32d6
-- | Given @Left path@ as argument , sets the current keyboard focus to be at
-- @path@, and selects it. This is useful when you want to focus the user's
-- attention on a particular item. If @Right cell@ is given, then focus is
-- given to the cell specified by it. Additionally, if @startEditing@ is
-- @True@, then editing should be started in the specified cell.
--
-- This function is often followed by
-- 'Graphics.UI.Gtk.Abstract.Widget.widgetGrabFocus' in order to give keyboard
-- focus to the widget. Please note that editing can only happen when the
-- widget is realized.
--
-- * Available since Gtk+ version 2.8
--
iconViewSetCursor :: (IconViewClass self, CellRendererClass cell) => self
 -> (Either TreePath cell) -- ^ the path or the cell
 -> Bool     -- ^ @startEditing@ - @True@ if the specified cell should start
             -- being edited.
 -> IO ()
iconViewSetCursor self (Left path) startEditing =
  withTreePath path $ \path ->
  {# call gtk_icon_view_set_cursor #}
    (toIconView self)
    path
    (CellRenderer nullForeignPtr)
    (fromBool startEditing)
iconViewSetCursor self (Right cell) startEditing =
  {# call gtk_icon_view_set_cursor #}
    (toIconView self)
    (NativeTreePath nullPtr)
    (toCellRenderer cell)
    (fromBool startEditing)

-- %hash c:3307 d:9cf8
-- | Return a @path@ and a @cell@ with the current cursor path and cell. If the
-- cursor isn't currently set, then @[]@ will be returned for the @path@. If no cell currently has focus,
-- then @cell@ will be @Nothing@.
--
-- * Available since Gtk+ version 2.8
--
iconViewGetCursor :: IconViewClass self => self
 -> IO (TreePath, Maybe CellRenderer)               -- ^ returns a @path@ to the cursor and a @cell@ if the widget has the input focus
iconViewGetCursor self =
  alloca $ \pathPtrPtr -> alloca $ \crPtrPtr -> do
  {# call gtk_icon_view_get_cursor #}
    (toIconView self)
    (castPtr pathPtrPtr)
    (castPtr crPtrPtr)
  pathPtr <- peek pathPtrPtr
  crPtr <- peek crPtrPtr
  path <- fromTreePath pathPtr
  cr <- if crPtr==nullPtr then return Nothing else
        liftM Just $ makeNewGObject mkCellRenderer (return crPtr)
  return (path, cr)

-- %hash c:1c9e d:20c5
-- | Moves the alignments of @iconView@ to the position specified by @path@.
-- @rowAlign@ determines where the row is placed, and @colAlign@ determines
-- where @column@ is placed. Both are expected to be between 0.0 and 1.0. 0.0
-- means left\/top alignment, 1.0 means right\/bottom alignment, 0.5 means
-- center.
--
-- If @useAlign@ is @False@, then the alignment arguments are ignored, and the
-- tree does the minimum amount of work to scroll the item onto the screen.
-- This means that the item will be scrolled to the edge closest to its
-- current position. If the item is currently visible on the screen, nothing
-- is done.
--
-- This function only works if the model is set, and @path@ is a valid row on
-- the model. If the model changes before the @iconView@ is realized, the
-- centered path will be modified to reflect this change.
--
-- * Available since Gtk+ version 2.8
--
iconViewScrollToPath :: IconViewClass self => self
 -> TreePath -- ^ @path@ - The path of the item to move to.
 -> Bool     -- ^ @useAlign@ - whether to use alignment arguments, or @False@.
 -> Float    -- ^ @rowAlign@ - The vertical alignment of the item specified by
             -- @path@.
 -> Float    -- ^ @colAlign@ - The horizontal alignment of the item specified
             -- by @path@.
 -> IO ()
iconViewScrollToPath self path useAlign rowAlign colAlign =
  withTreePath path $ \path ->
  {# call gtk_icon_view_scroll_to_path #}
    (toIconView self)
    path
    (fromBool useAlign)
    (realToFrac rowAlign)
    (realToFrac colAlign)

-- %hash c:8354 d:f7f3
-- | Retrieve the first and last visible path.
-- Note that there may be invisible paths inbetween.
--
-- * Available since Gtk+ version 2.8
--
iconViewGetVisibleRange :: IconViewClass self => self
 -> IO (Maybe (TreePath, TreePath))
                -- ^ returns the first and last visible path, the return value
                -- @Nothing@ if every element is visible
iconViewGetVisibleRange self = alloca $ \fPtrPtr -> alloca $ \lPtrPtr -> do
  success <- liftM toBool $ {# call gtk_icon_view_get_visible_range #}
    (toIconView self)
    (castPtr fPtrPtr)
    (castPtr lPtrPtr)
  if not success then return Nothing else do
  fPtr <- peek fPtrPtr
  lPtr <- peek lPtrPtr
  f <- fromTreePath fPtr
  l <- fromTreePath lPtr
  return (Just (f,l))

#if GTK_CHECK_VERSION(2,10,0)
-- %hash c:bd16 d:3f4f
-- | Turns @iconView@ into a drag source for automatic DND.
--
-- * Available since Gtk+ version 2.10
--
iconViewEnableModelDragSource :: IconViewClass self => self
  -> [Modifier]         -- ^ @startButtonMask@ - Mask of allowed buttons
                        -- to start drag
  -> TargetList         -- ^ @targets@ - the list of targets that the
                        -- the view will support
  -> [DragAction]       -- ^ @actions@ - flags denoting the possible actions
                        -- for a drag from this widget
  -> IO ()
iconViewEnableModelDragSource self startButtonMask targets actions =
  alloca $ \nTargetsPtr -> do
  tlPtr <- {#call unsafe gtk_target_table_new_from_list#} targets nTargetsPtr
  nTargets <- peek nTargetsPtr
  {# call gtk_icon_view_enable_model_drag_source #}
    (toIconView self)
    ((fromIntegral . fromFlags) startButtonMask)
    tlPtr
    nTargets
    ((fromIntegral . fromFlags) actions)
  {#call unsafe gtk_target_table_free#} tlPtr nTargets

-- %hash c:b14d d:23d7
-- | Turns @iconView@ into a drop destination for automatic DND.
--
-- * Available since Gtk+ version 2.10
--
iconViewEnableModelDragDest :: IconViewClass self => self
  -> TargetList                -- ^ @targets@ - the list of targets that the
                               -- the view will support
  -> [DragAction]              -- ^ @actions@ - flags denoting the possible actions
                               -- for a drop into this widget
  -> IO ()
iconViewEnableModelDragDest self targets actions =
  alloca $ \nTargetsPtr -> do
  tlPtr <- {#call unsafe gtk_target_table_new_from_list#} targets nTargetsPtr
  nTargets <- peek nTargetsPtr
  {# call gtk_icon_view_enable_model_drag_dest #}
    (toIconView self)
    tlPtr
    nTargets
    ((fromIntegral . fromFlags) actions)

-- %hash c:25b0 d:5a6b
-- | Undoes the effect of 'iconViewEnableModelDragSource'.
--
-- * Available since Gtk+ version 2.10
--
iconViewUnsetModelDragSource :: IconViewClass self => self -> IO ()
iconViewUnsetModelDragSource self =
  {# call gtk_icon_view_unset_model_drag_source #}
    (toIconView self)

-- %hash c:d76d d:f18a
-- | Undoes the effect of 'iconViewEnableModelDragDest'.
--
-- * Available since Gtk+ version 2.10
--
iconViewUnsetModelDragDest :: IconViewClass self => self -> IO ()
iconViewUnsetModelDragDest self =
  {# call gtk_icon_view_unset_model_drag_dest #}
    (toIconView self)
#endif

-- %hash c:c270 d:b94d
-- | Check if icons can be moved around.
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
-- * Available since Gtk+ version 2.8
--
iconViewSetReorderable :: IconViewClass self => self
 -> Bool -- ^ @reorderable@ - @True@, if the list of items can be reordered.
 -> IO ()
iconViewSetReorderable self reorderable =
  {# call gtk_icon_view_set_reorderable #}
    (toIconView self)
    (fromBool reorderable)

-- %hash c:532 d:1d07
-- | Retrieves whether the user can reorder the list via drag-and-drop. See
-- 'iconViewSetReorderable'.
--
-- * Available since Gtk+ version 2.8
--
iconViewGetReorderable :: IconViewClass self => self
 -> IO Bool -- ^ returns @True@ if the list can be reordered.
iconViewGetReorderable self =
  liftM toBool $
  {# call gtk_icon_view_get_reorderable #}
    (toIconView self)

#endif

#if GTK_CHECK_VERSION(2,22,0)
-- | Gets the row in which the item path is currently displayed. Row numbers start at 0.
--
-- * Available since Gtk+ version 2.22
--
iconViewGetItemRow :: IconViewClass self => self
                   -> TreePath -- ^ @path@      the 'TreePath' of the item
                   -> IO Int -- ^ returns   The row in which the item is displayed
iconViewGetItemRow self path =
  liftM fromIntegral $
  withTreePath path $ \path ->
  {# call gtk_icon_view_get_item_row #}
    (toIconView self)
    path

-- | Gets the column in which the item path is currently displayed. Column numbers start at 0.
--
-- * Available since Gtk+ version 2.22
--
iconViewGetItemColumn :: IconViewClass self => self
                   -> TreePath -- ^ @path@      the 'TreePath' of the item
                   -> IO Int -- ^ returns   The column in which the item is displayed
iconViewGetItemColumn self path =
  liftM fromIntegral $
  withTreePath path $ \path ->
  {# call gtk_icon_view_get_item_column #}
    (toIconView self)
    path

#endif

--------------------
-- Attributes

-- | The ::selection-mode property specifies the selection mode of icon view.
-- If the mode is 'SelectionMultiple', rubberband selection is enabled, for the
-- other modes, only keyboard selection is possible.
--
-- Default value: 'SelectionSingle'
--
iconViewSelectionMode :: IconViewClass self => Attr self SelectionMode
iconViewSelectionMode = newAttr
  iconViewGetSelectionMode
  iconViewSetSelectionMode

-- %hash c:4ce5 d:c77a
-- | The 'iconViewPixbufColumn' property contains the number of the model column
-- containing the pixbufs which are displayed. Setting this property to
-- 'invalidColumnId' turns off the display of pixbufs.
--
-- Default value: 'invalidColumnId'
--
iconViewPixbufColumn :: IconViewClass self => Attr self (ColumnId row Pixbuf)
iconViewPixbufColumn = newAttr
  iconViewGetPixbufColumn
  iconViewSetPixbufColumn

-- %hash c:702a d:f7ed
-- | The 'iconViewTextColumn' property contains the number of the model column
-- containing the texts which are displayed. If this property and the
-- 'iconViewMarkupColumn' property are both set to 'invalidColumnId', no texts
-- are displayed.
--
-- Default value: 'invalidColumnId'
--
iconViewTextColumn :: (IconViewClass self, GlibString string) => Attr self (ColumnId row string)
iconViewTextColumn = newAttr
  iconViewGetTextColumn
  iconViewSetTextColumn

-- %hash c:37cb d:ee83
-- | The 'iconViewMarkupColumn' property contains the number of the model column
-- containing markup information to be displayed. If this property and the
-- 'iconViewTextColumn' property are both set to column numbers, it overrides the text
-- column. If both are set to 'invalidColumnId', no texts are displayed.
--
-- Default value: 'invalidColumnId'
--
iconViewMarkupColumn :: (IconViewClass self, GlibString markup) => Attr self (ColumnId row markup)
iconViewMarkupColumn = newAttr
  iconViewGetMarkupColumn
  iconViewSetMarkupColumn

-- %hash c:723d
-- | The model for the icon view.
--
iconViewModel :: (IconViewClass self, TreeModelClass model)
 => ReadWriteAttr self (Maybe TreeModel) (Maybe model)
iconViewModel = newAttr
  iconViewGetModel
  iconViewSetModel

-- %hash c:6347
-- | The columns property contains the number of the columns in which the
-- items should be displayed. If it is -1, the number of columns will be chosen
-- automatically to fill the available area.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
iconViewColumns :: IconViewClass self => Attr self Int
iconViewColumns = newAttrFromIntProperty "columns"

-- %hash c:d0fe d:42c5
-- | The item-width property specifies the width to use for each item. If it
-- is set to -1, the icon view will automatically determine a suitable item
-- size.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
iconViewItemWidth :: IconViewClass self => Attr self Int
iconViewItemWidth = newAttrFromIntProperty "item-width"

-- %hash c:3813 d:23f9
-- | The spacing property specifies the space which is inserted between the
-- cells (i.e. the icon and the text) of an item.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
iconViewSpacing :: IconViewClass self => Attr self Int
iconViewSpacing = newAttrFromIntProperty "spacing"

-- %hash c:6a28 d:8e65
-- | The row-spacing property specifies the space which is inserted between
-- the rows of the icon view.
--
-- Allowed values: >= 0
--
-- Default value: 6
--
iconViewRowSpacing :: IconViewClass self => Attr self Int
iconViewRowSpacing = newAttrFromIntProperty "row-spacing"

-- %hash c:56a d:2971
-- | The column-spacing property specifies the space which is inserted between
-- the columns of the icon view.
--
-- Allowed values: >= 0
--
-- Default value: 6
--
iconViewColumnSpacing :: IconViewClass self => Attr self Int
iconViewColumnSpacing = newAttrFromIntProperty "column-spacing"

-- %hash c:89de d:8e41
-- | The margin property specifies the space which is inserted at the edges of
-- the icon view.
--
-- Allowed values: >= 0
--
-- Default value: 6
--
iconViewMargin :: IconViewClass self => Attr self Int
iconViewMargin = newAttrFromIntProperty "margin"

-- %hash c:b606 d:31c3
-- | The orientation property specifies how the cells (i.e. the icon and the
-- text) of the item are positioned relative to each other.
--
-- Default value: 'OrientationVertical'
--
iconViewOrientation :: IconViewClass self => Attr self Orientation
iconViewOrientation = newAttrFromEnumProperty "orientation"
                        {# call pure unsafe gtk_orientation_get_type #}

#if GTK_CHECK_VERSION(2,8,0)
-- %hash c:f17b d:54d0
-- | The reorderable property specifies if the items can be reordered by DND.
--
-- Default value: @False@
--
-- * Available since Gtk+ version 2.8
--
iconViewReorderable :: IconViewClass self => Attr self Bool
iconViewReorderable = newAttrFromBoolProperty "reorderable"
#endif

#if GTK_CHECK_VERSION(2,22,0)
-- | The item-orientation property specifies how the cells (i.e. the icon and the text) of the item are
-- positioned relative to each other.
--
-- Default value: 'OrientationVertical'
--
-- * Available since Gtk+ version 2.22
--
iconViewItemOrientation :: IconViewClass self => Attr self Orientation
iconViewItemOrientation = newAttrFromEnumProperty "item-orientation"
                          {# call pure unsafe gtk_orientation_get_type #}
#endif

--------------------
-- Signals

-- %hash c:4671 d:af3f
-- | New scroll adjustment have been set for this widget.
--
setIconViewScrollAdjustments :: IconViewClass self => Signal self (Adjustment -> Adjustment -> IO ())
setIconViewScrollAdjustments = Signal (connect_OBJECT_OBJECT__NONE "set-scroll-adjustments")

-- %hash c:4090 d:af3f
-- | A specific element has been activated (by pressing enter or double clicking).
--
itemActivated :: IconViewClass self => Signal self (TreePath -> IO ())
itemActivated = Signal (connect_BOXED__NONE "item-activated" (peekTreePath . castPtr))

-- %hash c:6098 d:af3f
-- | The selected item changed.
--
selectionChanged :: IconViewClass self => Signal self (IO ())
selectionChanged = Signal (connect_NONE__NONE "selection-changed")

#endif
