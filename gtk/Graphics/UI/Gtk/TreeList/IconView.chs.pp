-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget IconView
--
--  Author : Duncan Coutts
--
--  Created: 25 March 2005
--
--  Version $Revision: 1.1 $ from $Date: 2005/04/05 18:29:52 $
--
--  Copyright (C) 2005 Duncan Coutts
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
module Graphics.UI.Gtk.TreeList.IconView (
-- * Detail
-- 
-- | 'IconView' provides an alternative view on a list model. It displays the
-- model as a grid of icons with labels. Like 'TreeView', it allows to select
-- one or multiple items (depending on the selection mode, see
-- 'iconViewSetSelectionMode'). In addition to selection with the arrow keys,
-- 'IconView' supports rubberband selection, which is controlled by dragging
-- the pointer.

-- * Class Hierarchy
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
  castToIconView,

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
  iconViewSetOrientation,
  iconViewGetOrientation,
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

-- * Properties
  iconViewSelectionMode,
  iconViewPixbufColumn,
  iconViewTextColumn,
  iconViewMarkupColumn,
  iconViewModel,
  iconViewColumns,
  iconViewItemWidth,
  iconViewSpacing,
  iconViewRowSpacing,
  iconViewColumnSpacing,
  iconViewMargin,
  iconViewOrientation,

-- * Signals
  onSelectAll,
  afterSelectAll,
  onUnselectAll,
  afterUnselectAll,
  onSelectCursorItem,
  afterSelectCursorItem,
  onToggleCursorItem,
  afterToggleCursorItem,
  onActivateCursorItem,
  afterActivateCursorItem,
#endif
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes
{#import System.Glib.GList#}
import System.Glib.GObject			(makeNewGObject)
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.General.Enums#}	(Orientation, SelectionMode,
						MovementStep)
{#import Graphics.UI.Gtk.TreeList.TreeModel#}

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

-- | Creates a new 'IconView' widget with the model @model@.
--
iconViewNewWithModel :: TreeModelClass model => 
    model       -- ^ @model@ - The model.
 -> IO IconView
iconViewNewWithModel model =
  makeNewObject mkIconView $
  liftM (castPtr :: Ptr Widget -> Ptr IconView) $
  {# call gtk_icon_view_new_with_model #}
    (toTreeModel model)

--------------------
-- Methods

-- | Sets the model for a 'IconView'. If the @iconView@ already has a model
-- set, it will remove it before setting the new model. If @model@ is {@NULL@,
-- FIXME: this should probably be converted to a Maybe data type}, then it will
-- unset the old model.
--
iconViewSetModel :: (IconViewClass self, TreeModelClass model) => self
 -> model -- ^ @model@ - The model.
 -> IO ()
iconViewSetModel self model =
  {# call gtk_icon_view_set_model #}
    (toIconView self)
    (toTreeModel model)

-- | Returns the model the 'IconView' is based on. Returns {@NULL@, FIXME:
-- this should probably be converted to a Maybe data type} if the model is
-- unset.
--
iconViewGetModel :: IconViewClass self => self
 -> IO TreeModel -- ^ returns A 'TreeModel', or {@NULL@, FIXME: this should
                 -- probably be converted to a Maybe data type} if none is
                 -- currently being used.
iconViewGetModel self =
  makeNewGObject mkTreeModel $
  {# call gtk_icon_view_get_model #}
    (toIconView self)

-- | Sets the column with text for @iconView@ to be @column@. The text column
-- must be of type {G_TYPE_STRING, FIXME: unknown type\/value}.
--
iconViewSetTextColumn :: IconViewClass self => self
 -> Int   -- ^ @column@ - A column in the currently used model.
 -> IO ()
iconViewSetTextColumn self column =
  {# call gtk_icon_view_set_text_column #}
    (toIconView self)
    (fromIntegral column)

-- | Returns the column with text for @iconView@.
--
iconViewGetTextColumn :: IconViewClass self => self
 -> IO Int -- ^ returns the text column, or -1 if it's unset.
iconViewGetTextColumn self =
  liftM fromIntegral $
  {# call gtk_icon_view_get_text_column #}
    (toIconView self)

-- | Sets the column with markup information for @iconView@ to be @column@.
-- The markup column must be of type {G_TYPE_STRING, FIXME: unknown
-- type\/value}. If the markup column is set to something, it overrides the text
-- column set by 'iconViewSetTextColumn'.
--
iconViewSetMarkupColumn :: IconViewClass self => self
 -> Int   -- ^ @column@ - A column in the currently used model.
 -> IO ()
iconViewSetMarkupColumn self column =
  {# call gtk_icon_view_set_markup_column #}
    (toIconView self)
    (fromIntegral column)

-- | Returns the column with markup text for @iconView@.
--
iconViewGetMarkupColumn :: IconViewClass self => self
 -> IO Int -- ^ returns the markup column, or -1 if it's unset.
iconViewGetMarkupColumn self =
  liftM fromIntegral $
  {# call gtk_icon_view_get_markup_column #}
    (toIconView self)

-- | Sets the column with pixbufs for @iconView@ to be @column@. The pixbuf
-- column must be of type {GDK_TYPE_PIXBUF, FIXME: unknown type\/value}
--
-- * Available since Gtk version 2.6
--
iconViewSetPixbufColumn :: IconViewClass self => self
 -> Int   -- ^ @column@ - A column in the currently used model.
 -> IO ()
iconViewSetPixbufColumn self column =
  {# call gtk_icon_view_set_pixbuf_column #}
    (toIconView self)
    (fromIntegral column)

-- | Returns the column with pixbufs for @iconView@.
--
iconViewGetPixbufColumn :: IconViewClass self => self
 -> IO Int -- ^ returns the pixbuf column, or -1 if it's unset.
iconViewGetPixbufColumn self =
  liftM fromIntegral $
  {# call gtk_icon_view_get_pixbuf_column #}
    (toIconView self)

-- | Finds the path at the point (@x@, @y@), relative to widget coordinates.
--
iconViewGetPathAtPos :: IconViewClass self => self
 -> Int         -- ^ @x@ - The x position to be identified
 -> Int         -- ^ @y@ - The y position to be identified
 -> IO TreePath -- ^ returns The 'TreePath' corresponding to the icon or @[]@
                -- if no icon exists at that position.
iconViewGetPathAtPos self x y = do
  tpPtr <- {# call gtk_icon_view_get_path_at_pos #}
    (toIconView self)
    (fromIntegral x)
    (fromIntegral y)
  if tpPtr==nullPtr then return [] else do
  path <- nativeTreePathGetIndices (NativeTreePath tpPtr)
  nativeTreePathFree (NativeTreePath tpPtr)
  return path

-- | Calls a function for each selected icon. Note that the model or selection
-- cannot be modified from within this function.
--
iconViewSelectedForeach :: IconViewClass self => self
 -> (TreePath -> IO ()) -- ^ @(\path -> ...)@ - The funcion to call for each
                        -- selected icon.
 -> IO ()
iconViewSelectedForeach self func = do
  funcPtr <- mkIconViewForeachFunc (\_ tpPtr _ -> do
    path <- nativeTreePathGetIndices (NativeTreePath tpPtr)
    nativeTreePathFree (NativeTreePath tpPtr)
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

-- | 
--
iconViewSetColumns :: IconViewClass self => self
 -> Int   -- ^ @columns@ -
 -> IO ()
iconViewSetColumns self columns =
  {# call gtk_icon_view_set_columns #}
    (toIconView self)
    (fromIntegral columns)

-- | 
--
iconViewGetColumns :: IconViewClass self => self
 -> IO Int -- ^ returns
iconViewGetColumns self =
  liftM fromIntegral $
  {# call gtk_icon_view_get_columns #}
    (toIconView self)

-- | 
--
iconViewSetItemWidth :: IconViewClass self => self
 -> Int   -- ^ @itemWidth@ -
 -> IO ()
iconViewSetItemWidth self itemWidth =
  {# call gtk_icon_view_set_item_width #}
    (toIconView self)
    (fromIntegral itemWidth)

-- | 
--
iconViewGetItemWidth :: IconViewClass self => self
 -> IO Int -- ^ returns
iconViewGetItemWidth self =
  liftM fromIntegral $
  {# call gtk_icon_view_get_item_width #}
    (toIconView self)

-- | 
--
iconViewSetSpacing :: IconViewClass self => self
 -> Int   -- ^ @spacing@ -
 -> IO ()
iconViewSetSpacing self spacing =
  {# call gtk_icon_view_set_spacing #}
    (toIconView self)
    (fromIntegral spacing)

-- | 
--
iconViewGetSpacing :: IconViewClass self => self
 -> IO Int -- ^ returns
iconViewGetSpacing self =
  liftM fromIntegral $
  {# call gtk_icon_view_get_spacing #}
    (toIconView self)

-- | 
--
iconViewSetRowSpacing :: IconViewClass self => self
 -> Int   -- ^ @rowSpacing@ -
 -> IO ()
iconViewSetRowSpacing self rowSpacing =
  {# call gtk_icon_view_set_row_spacing #}
    (toIconView self)
    (fromIntegral rowSpacing)

-- | 
--
iconViewGetRowSpacing :: IconViewClass self => self
 -> IO Int -- ^ returns
iconViewGetRowSpacing self =
  liftM fromIntegral $
  {# call gtk_icon_view_get_row_spacing #}
    (toIconView self)

-- | 
--
iconViewSetColumnSpacing :: IconViewClass self => self
 -> Int   -- ^ @columnSpacing@ -
 -> IO ()
iconViewSetColumnSpacing self columnSpacing =
  {# call gtk_icon_view_set_column_spacing #}
    (toIconView self)
    (fromIntegral columnSpacing)

-- | 
--
iconViewGetColumnSpacing :: IconViewClass self => self
 -> IO Int -- ^ returns
iconViewGetColumnSpacing self =
  liftM fromIntegral $
  {# call gtk_icon_view_get_column_spacing #}
    (toIconView self)

-- | 
--
iconViewSetMargin :: IconViewClass self => self
 -> Int   -- ^ @margin@ -
 -> IO ()
iconViewSetMargin self margin =
  {# call gtk_icon_view_set_margin #}
    (toIconView self)
    (fromIntegral margin)

-- | 
--
iconViewGetMargin :: IconViewClass self => self
 -> IO Int -- ^ returns
iconViewGetMargin self =
  liftM fromIntegral $
  {# call gtk_icon_view_get_margin #}
    (toIconView self)

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

-- | Creates a list of paths of all selected items. Additionally, if you are
-- planning on modifying the model after calling this function, you may want to
-- convert the returned list into a list of {GtkTreeRowReference, FIXME: boxed
-- type}s. To do this, you can use 'treeRowReferenceNew'.
--
iconViewGetSelectedItems :: IconViewClass self => self
 -> IO [TreePath] -- ^ returns a list of 'TreePath's, one for each selected row.
iconViewGetSelectedItems self =
  {# call gtk_icon_view_get_selected_items #}
    (toIconView self)
  >>= fromGList
  >>= mapM (\elemPtr -> do
        path <- nativeTreePathGetIndices (NativeTreePath elemPtr)
        nativeTreePathFree (NativeTreePath elemPtr)
        return path
      )

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

--------------------
-- Properties

-- | The ::selection-mode property specifies the selection mode of icon view.
-- If the mode is 'SelectionMultiple', rubberband selection is enabled, for the
-- other modes, only keyboard selection is possible.
--
-- Default value: 'SelectionSingle'
--
iconViewSelectionMode :: IconViewClass self => Attr self SelectionMode
iconViewSelectionMode = Attr 
  iconViewGetSelectionMode
  iconViewSetSelectionMode

-- | The ::pixbuf-column property contains the number of the model column
-- containing the pixbufs which are displayed. The pixbuf column must be of
-- type {GDK_TYPE_PIXBUF, FIXME: unknown type\/value}. Setting this property to
-- -1 turns off the display of pixbufs.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
iconViewPixbufColumn :: IconViewClass self => Attr self Int
iconViewPixbufColumn = Attr 
  iconViewGetPixbufColumn
  iconViewSetPixbufColumn

-- | The ::text-column property contains the number of the model column
-- containing the texts which are displayed. The text column must be of type
-- {G_TYPE_STRING, FIXME: unknown type\/value}. If this property and the
-- :markup-column property are both set to -1, no texts are displayed.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
iconViewTextColumn :: IconViewClass self => Attr self Int
iconViewTextColumn = Attr 
  iconViewGetTextColumn
  iconViewSetTextColumn

-- | The ::markup-column property contains the number of the model column
-- containing markup information to be displayed. The markup column must be of
-- type {G_TYPE_STRING, FIXME: unknown type\/value}. If this property and the
-- :text-column property are both set to column numbers, it overrides the text
-- column. If both are set to -1, no texts are displayed.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
iconViewMarkupColumn :: IconViewClass self => Attr self Int
iconViewMarkupColumn = Attr 
  iconViewGetMarkupColumn
  iconViewSetMarkupColumn

-- | The model for the icon view.
--
iconViewModel :: IconViewClass self => Attr self TreeModel
iconViewModel = Attr 
  iconViewGetModel
  iconViewSetModel

-- | The columns property contains the number of the columns in which the
-- items should be displayed. If it is -1, the number of columns will be chosen
-- automatically to fill the available area.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
iconViewColumns :: IconViewClass self => Attr self Int
iconViewColumns = Attr 
  iconViewGetColumns
  iconViewSetColumns

-- | The width used for each item.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
iconViewItemWidth :: IconViewClass self => Attr self Int
iconViewItemWidth = Attr 
  iconViewGetItemWidth
  iconViewSetItemWidth

-- | Space which is inserted between cells of an item.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
iconViewSpacing :: IconViewClass self => Attr self Int
iconViewSpacing = Attr 
  iconViewGetSpacing
  iconViewSetSpacing

-- | Space which is inserted between grid rows.
--
-- Allowed values: >= 0
--
-- Default value: 6
--
iconViewRowSpacing :: IconViewClass self => Attr self Int
iconViewRowSpacing = Attr 
  iconViewGetRowSpacing
  iconViewSetRowSpacing

-- | Space which is inserted between grid column.
--
-- Allowed values: >= 0
--
-- Default value: 6
--
iconViewColumnSpacing :: IconViewClass self => Attr self Int
iconViewColumnSpacing = Attr 
  iconViewGetColumnSpacing
  iconViewSetColumnSpacing

-- | Space which is inserted at the edges of the icon view.
--
-- Allowed values: >= 0
--
-- Default value: 6
--
iconViewMargin :: IconViewClass self => Attr self Int
iconViewMargin = Attr 
  iconViewGetMargin
  iconViewSetMargin

-- | How the text and icon of each item are positioned relative to each other.
--
-- Default value: 'OrientationVertical'
--
iconViewOrientation :: IconViewClass self => Attr self Orientation
iconViewOrientation = Attr 
  iconViewGetOrientation
  iconViewSetOrientation

--------------------
-- Signals

-- | 
--
onSelectAll, afterSelectAll :: IconViewClass self => self
 -> IO ()
 -> IO (ConnectId self)
onSelectAll = connect_NONE__NONE "select_all" False
afterSelectAll = connect_NONE__NONE "select_all" True

-- | 
--
onUnselectAll, afterUnselectAll :: IconViewClass self => self
 -> IO ()
 -> IO (ConnectId self)
onUnselectAll = connect_NONE__NONE "unselect_all" False
afterUnselectAll = connect_NONE__NONE "unselect_all" True

-- | 
--
onSelectCursorItem, afterSelectCursorItem :: IconViewClass self => self
 -> IO ()
 -> IO (ConnectId self)
onSelectCursorItem = connect_NONE__NONE "select_cursor_item" False
afterSelectCursorItem = connect_NONE__NONE "select_cursor_item" True

-- | 
--
onToggleCursorItem, afterToggleCursorItem :: IconViewClass self => self
 -> IO ()
 -> IO (ConnectId self)
onToggleCursorItem = connect_NONE__NONE "toggle_cursor_item" False
afterToggleCursorItem = connect_NONE__NONE "toggle_cursor_item" True

-- | 
--
onActivateCursorItem, afterActivateCursorItem :: IconViewClass self => self
 -> IO Bool
 -> IO (ConnectId self)
onActivateCursorItem = connect_NONE__BOOL "activate_cursor_item" False
afterActivateCursorItem = connect_NONE__BOOL "activate_cursor_item" True
#endif
