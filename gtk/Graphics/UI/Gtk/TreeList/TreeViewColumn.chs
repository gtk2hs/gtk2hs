{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) TreeViewColumn TreeView
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
-- NOTES
--
-- tree_view_column_new_with_attributes and tree_view_column_set_attributes 
--   are variadic and the funcitonality can be achieved through other 
--   functions.
--
-- tree_view_column_set_cell_data and tree_view_column_cell_get_size are not
--   bound because I am not sure what they do and when they are useful
--
-- TODO
--
-- treeViewColumnSetCellData is not bound. With this function the user has
--   control over how data in the store is mapped to the attributes of a
--   cell renderer. This functin should be bound in the future to allow the
--   user to insert Haskell data types into the store and convert these
--   values to attributes of cell renderers.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A visible column in a 'TreeView' widget
--
-- * This module and all other modules in 'Graphics.UI.Gtk.TreeList' are
--   deprecated. Please use the modules in 'Graphics.UI.Gtk.ModelView'.
--
module Graphics.UI.Gtk.TreeList.TreeViewColumn (
-- * Detail
-- 
-- | The 'TreeViewColumn' object represents a visible column in a 'TreeView'
-- widget. It allows to set properties of the column header, and functions as a
-- holding pen for the cell renderers which determine how the data in the
-- column is displayed.
--
-- Please refer to the tree widget conceptual overview for an overview of
-- all the objects and data types related to the tree widget and how they work
-- together.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----TreeViewColumn
-- @

-- * Types
  TreeViewColumn,
  TreeViewColumnClass,
  castToTreeViewColumn,
  toTreeViewColumn,

-- * Constructors
  treeViewColumnNew,
  treeViewColumnNewWithAttributes,

-- * Methods
  treeViewColumnPackStart,
  treeViewColumnPackEnd,
  treeViewColumnClear,
  treeViewColumnGetCellRenderers,
  treeViewColumnAddAttribute,
  treeViewColumnAddAttributes,
  treeViewColumnSetAttributes,
  treeViewColumnClearAttributes,
  treeViewColumnSetSpacing,
  treeViewColumnGetSpacing,
  treeViewColumnSetVisible,
  treeViewColumnGetVisible,
  treeViewColumnSetResizable,
  treeViewColumnGetResizable,
  TreeViewColumnSizing(..),
  treeViewColumnSetSizing,
  treeViewColumnGetSizing,
  treeViewColumnGetWidth,
  treeViewColumnSetFixedWidth,
  treeViewColumnGetFixedWidth,
  treeViewColumnSetMinWidth,
  treeViewColumnGetMinWidth,
  treeViewColumnSetMaxWidth,
  treeViewColumnGetMaxWidth,
  treeViewColumnClicked,
  treeViewColumnSetTitle,
  treeViewColumnGetTitle,
  treeViewColumnSetClickable,
  treeViewColumnGetClickable,
  treeViewColumnSetWidget,
  treeViewColumnGetWidget,
  treeViewColumnSetAlignment,
  treeViewColumnGetAlignment,
  treeViewColumnSetReorderable,
  treeViewColumnGetReorderable,
  treeViewColumnSetSortColumnId,
  treeViewColumnGetSortColumnId,
  treeViewColumnSetSortIndicator,
  treeViewColumnGetSortIndicator,
  treeViewColumnSetSortOrder,
  treeViewColumnGetSortOrder,
  SortType(..),

-- * Attributes
  treeViewColumnVisible,
  treeViewColumnResizable,
  treeViewColumnWidth,
  treeViewColumnSpacing,
  treeViewColumnSizing,
  treeViewColumnFixedWidth,
  treeViewColumnMinWidth,
  treeViewColumnMaxWidth,
  treeViewColumnTitle,
  treeViewColumnClickable,
  treeViewColumnWidget,
  treeViewColumnAlignment,
  treeViewColumnReorderable,
  treeViewColumnSortIndicator,
  treeViewColumnSortOrder,
  treeViewColumnSortColumnId,

-- * Signals
  onColClicked,
  afterColClicked
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
{#import System.Glib.GList#}			(fromGList)
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums		(TreeViewColumnSizing(..), SortType(..))
{#import Graphics.UI.Gtk.TreeList.TreeModel#}	()

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Generate a new TreeViewColumn widget.
--
treeViewColumnNew :: IO TreeViewColumn
treeViewColumnNew  = makeNewObject mkTreeViewColumn 
  {# call tree_view_column_new #}

-- | Returns a new TreeViewColumn with title @title@, cell renderer @cr@, and
-- attributes @attribs@.
--
treeViewColumnNewWithAttributes :: CellRendererClass cr =>
    String
 -> cr
 -> [(String, Int)]
 -> IO TreeViewColumn
treeViewColumnNewWithAttributes title cr attribs =
    do
    tvc <- treeViewColumnNew
    treeViewColumnSetTitle tvc title
    treeViewColumnPackStart tvc  cr True
    treeViewColumnAddAttributes tvc cr attribs
    return tvc

--------------------
-- Methods

-- | Add a cell renderer at the beginning of a column.
--
-- * Excess space is divided equally among all renderers which have
--   @expand@ set to True.
--
treeViewColumnPackStart :: CellRendererClass cell => TreeViewColumn
 -> cell
 -> Bool
 -> IO ()
treeViewColumnPackStart self cell expand =
  {# call unsafe tree_view_column_pack_start #}
    self
    (toCellRenderer cell)
    (fromBool expand)

-- | Add a cell renderer at the end of a column.
--
-- * Excess space is divided equally among all renderers which have
--   @expand@ set to True.
--
treeViewColumnPackEnd :: CellRendererClass cell => TreeViewColumn
 -> cell
 -> Bool
 -> IO ()
treeViewColumnPackEnd self cell expand =
  {# call unsafe tree_view_column_pack_end #}
    self
    (toCellRenderer cell)
    (fromBool expand)

-- | Remove the associations of attributes
-- to a store for all 'CellRenderers'.
--
treeViewColumnClear :: TreeViewColumn -> IO ()
treeViewColumnClear self =
  {# call tree_view_column_clear #}
    self

-- | Retrieve all 'CellRenderer's that are contained in this column.
--
treeViewColumnGetCellRenderers :: TreeViewColumn -> IO [CellRenderer]
treeViewColumnGetCellRenderers self =
  {# call unsafe tree_view_column_get_cell_renderers #}
    self
  >>= fromGList
  >>= mapM (makeNewObject mkCellRenderer . return)

-- | Insert an attribute to change the behaviour of the column's cell renderer.
--
-- * The 'CellRenderer' @cr@ must already be in 
--   'TreeViewColumn'.
--
treeViewColumnAddAttribute :: CellRendererClass cellRenderer => TreeViewColumn
 -> cellRenderer
 -> String
 -> Int
 -> IO ()
treeViewColumnAddAttribute self cellRenderer attribute column =
  withUTFString attribute $ \attributePtr ->
  {# call unsafe tree_view_column_add_attribute #}
    self
    (toCellRenderer cellRenderer)
    attributePtr
    (fromIntegral column)

-- | Insert attributes @attribs@ to change the behaviour of column @tvc@'s cell
-- renderer @cr@.
--
treeViewColumnAddAttributes :: CellRendererClass cr => TreeViewColumn
 -> cr
 -> [(String,Int)]
 -> IO ()
treeViewColumnAddAttributes self cr attribs = 
    mapM_ (\ (attr, col) -> treeViewColumnAddAttribute self cr attr col) attribs

-- | Set the attributes of the cell renderer @cr@ in the tree column @tvc@
-- be  @attribs@. The attributes are given as a list of attribute\/column pairs.
-- All existing attributes are removed, and replaced with the new attributes.
--
treeViewColumnSetAttributes :: CellRendererClass cr => TreeViewColumn
 -> cr
 -> [(String, Int)]
 -> IO ()
treeViewColumnSetAttributes self cr attribs = do
  treeViewColumnClearAttributes self cr
  treeViewColumnAddAttributes self cr attribs

-- | Clears all existing attributes
-- of the column @tvc@.
--
treeViewColumnClearAttributes :: CellRendererClass cellRenderer => TreeViewColumn
 -> cellRenderer
 -> IO ()
treeViewColumnClearAttributes self cellRenderer =
  {# call tree_view_column_clear_attributes #}
    self
    (toCellRenderer cellRenderer)

-- | Set the number of pixels between two cell renderers.
--
treeViewColumnSetSpacing :: TreeViewColumn -> Int -> IO ()
treeViewColumnSetSpacing self spacing =
  {# call tree_view_column_set_spacing #}
    self
    (fromIntegral spacing)


-- | Get the number of pixels between two cell renderers.
--
treeViewColumnGetSpacing :: TreeViewColumn -> IO Int
treeViewColumnGetSpacing self =
  liftM fromIntegral $
  {# call unsafe tree_view_column_get_spacing #}
    self

-- | Set the visibility of a given column.
--
treeViewColumnSetVisible :: TreeViewColumn -> Bool -> IO ()
treeViewColumnSetVisible self visible =
  {# call tree_view_column_set_visible #}
    self
    (fromBool visible)

-- | Get the visibility of a given column.
--
treeViewColumnGetVisible :: TreeViewColumn -> IO Bool
treeViewColumnGetVisible self =
  liftM toBool $
  {# call unsafe tree_view_column_get_visible #}
    self

-- | Set if a given column is resizable by the user.
--
treeViewColumnSetResizable :: TreeViewColumn -> Bool -> IO ()
treeViewColumnSetResizable self resizable =
  {# call tree_view_column_set_resizable #}
    self
    (fromBool resizable)

-- | Get if a given column is resizable by the user.
--
treeViewColumnGetResizable :: TreeViewColumn -> IO Bool
treeViewColumnGetResizable self =
  liftM toBool $
  {# call unsafe tree_view_column_get_resizable #}
    self

-- | Set wether the column can be resized.
--
treeViewColumnSetSizing :: TreeViewColumn
 -> TreeViewColumnSizing
 -> IO ()
treeViewColumnSetSizing self type_ =
  {# call tree_view_column_set_sizing #}
    self
    ((fromIntegral . fromEnum) type_)

-- | Return the resizing type of the column.
--
treeViewColumnGetSizing :: TreeViewColumn
 -> IO TreeViewColumnSizing
treeViewColumnGetSizing self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe tree_view_column_get_sizing #}
    self

-- | Query the current width of the column.
--
treeViewColumnGetWidth :: TreeViewColumn -> IO Int
treeViewColumnGetWidth self =
  liftM fromIntegral $
  {# call unsafe tree_view_column_get_width #}
    self

-- | Set the width of the column.
--
-- * This is meaningful only if the sizing type is 'TreeViewColumnFixed'.
--
treeViewColumnSetFixedWidth :: TreeViewColumn -> Int -> IO ()
treeViewColumnSetFixedWidth self fixedWidth =
  {# call tree_view_column_set_fixed_width #}
    self
    (fromIntegral fixedWidth)

-- | Gets the fixed width of the column.
--
-- * This is meaningful only if the sizing type is 'TreeViewColumnFixed'.
--
-- * This value is only meaning may not be the actual width of the column on the
-- screen, just what is requested.
--
treeViewColumnGetFixedWidth :: TreeViewColumn -> IO Int
treeViewColumnGetFixedWidth self =
  liftM fromIntegral $
  {# call unsafe tree_view_column_get_fixed_width #}
    self

-- | Set minimum width of the column.
--
treeViewColumnSetMinWidth :: TreeViewColumn -> Int -> IO ()
treeViewColumnSetMinWidth self minWidth =
  {# call tree_view_column_set_min_width #}
    self
    (fromIntegral minWidth)

-- | Get the minimum width of a column. Returns -1 if this width was not set.
--
treeViewColumnGetMinWidth :: TreeViewColumn -> IO Int
treeViewColumnGetMinWidth self =
  liftM fromIntegral $
  {# call unsafe tree_view_column_get_min_width #}
    self

-- | Set maximum width of the column.
--
treeViewColumnSetMaxWidth :: TreeViewColumn -> Int -> IO ()
treeViewColumnSetMaxWidth self maxWidth =
  {# call tree_view_column_set_max_width #}
    self
    (fromIntegral maxWidth)

-- | Get the maximum width of a column. Returns -1 if this width was not set.
--
treeViewColumnGetMaxWidth :: TreeViewColumn -> IO Int
treeViewColumnGetMaxWidth self =
  liftM fromIntegral $
  {# call unsafe tree_view_column_get_max_width #}
    self

-- | Emit the @clicked@ signal on the column.
--
treeViewColumnClicked :: TreeViewColumn -> IO ()
treeViewColumnClicked self =
  {# call tree_view_column_clicked #}
    self

-- | Set the widget's title if a custom widget has not been set.
--
treeViewColumnSetTitle :: TreeViewColumn -> String -> IO ()
treeViewColumnSetTitle self title =
  withUTFString title $ \titlePtr ->
  {# call tree_view_column_set_title #}
    self
    titlePtr

-- | Get the widget's title.
--
treeViewColumnGetTitle :: TreeViewColumn -> IO (Maybe String)
treeViewColumnGetTitle self =
  {# call unsafe tree_view_column_get_title #}
    self
  >>= maybePeek peekUTFString

-- | Set if the column should be sensitive to mouse clicks.
--
treeViewColumnSetClickable :: TreeViewColumn -> Bool -> IO ()
treeViewColumnSetClickable self clickable =
  {# call tree_view_column_set_clickable #}
    self
    (fromBool clickable)

-- | Returns True if the user can click on the header for the column.
--
treeViewColumnGetClickable :: TreeViewColumn -> IO Bool
treeViewColumnGetClickable self =
  liftM toBool $
  {# call tree_view_column_get_clickable #}
    self

-- | Set the column's title to this widget.
--
treeViewColumnSetWidget :: WidgetClass widget => TreeViewColumn
 -> widget
 -> IO ()
treeViewColumnSetWidget self widget =
  {# call tree_view_column_set_widget #}
    self
    (toWidget widget)

-- | Retrieve the widget responsible for
-- showing the column title. In case only a text title was set this will be a
-- 'Alignment' widget with a 'Label' inside.
--
treeViewColumnGetWidget :: TreeViewColumn -> IO Widget
treeViewColumnGetWidget self =
  makeNewObject mkWidget $
  {# call unsafe tree_view_column_get_widget #}
    self

-- | Sets the alignment of the title or custom widget inside the column
-- header. The alignment determines its location inside the button -- 0.0 for
-- left, 0.5 for center, 1.0 for right.
--
treeViewColumnSetAlignment :: TreeViewColumn
 -> Float          -- ^ @xalign@ - The alignment, which is between [0.0 and
                   -- 1.0] inclusive.
 -> IO ()
treeViewColumnSetAlignment self xalign =
  {# call tree_view_column_set_alignment #}
    self
    (realToFrac xalign)

-- | Returns the current x alignment of the tree column. This value can range
-- between 0.0 and 1.0.
--
treeViewColumnGetAlignment :: TreeViewColumn -> IO Float
treeViewColumnGetAlignment self =
  liftM realToFrac $
  {# call unsafe tree_view_column_get_alignment #}
    self

-- | Set if the column can be reordered by the end user dragging the header.
--
treeViewColumnSetReorderable :: TreeViewColumn -> Bool -> IO ()
treeViewColumnSetReorderable self reorderable =
  {# call tree_view_column_set_reorderable #}
    self
    (fromBool reorderable)

-- | Returns whether the column can be reordered by the user.
--
treeViewColumnGetReorderable :: TreeViewColumn -> IO Bool
treeViewColumnGetReorderable self =
  liftM toBool $
  {# call unsafe tree_view_column_get_reorderable #}
    self

-- | Set the column by which to sort.
--
-- * Sets the logical @columnId@ that this column sorts on when
--   this column is selected for sorting. The selected column's header
--   will be clickable after this call. Logical refers to the column in
--   the 'TreeModel'.
--
treeViewColumnSetSortColumnId :: TreeViewColumn -> Int -> IO ()
treeViewColumnSetSortColumnId self sortColumnId =
  {# call tree_view_column_set_sort_column_id #}
    self
    (fromIntegral sortColumnId)

-- | Get the column by which to sort.
--
-- * Retrieves the logical @columnId@ that the model sorts on when
--   this column is selected for sorting.
--
-- * Returns -1 if this column can't be used for sorting.
--
treeViewColumnGetSortColumnId :: TreeViewColumn -> IO Int
treeViewColumnGetSortColumnId self =
  liftM fromIntegral $
  {# call unsafe tree_view_column_get_sort_column_id #}
    self

-- | Set if a given column has sorting arrows in its heading.
--
treeViewColumnSetSortIndicator :: TreeViewColumn
 -> Bool -> IO ()
treeViewColumnSetSortIndicator self setting =
  {# call tree_view_column_set_sort_indicator #}
    self
    (fromBool setting)

-- | Query if a given column has sorting arrows in its heading.
--
treeViewColumnGetSortIndicator :: TreeViewColumn -> IO Bool
treeViewColumnGetSortIndicator self =
  liftM toBool $
  {# call unsafe tree_view_column_get_sort_indicator #}
    self

-- | Set if a given column is sorted in ascending or descending order.
--
-- * In order for sorting to work, it is necessary to either use automatic
--   sorting via 'treeViewColumnSetSortColumnId' or to use a
--   user defined sorting on the elements in a 'TreeModel'.
--
treeViewColumnSetSortOrder :: TreeViewColumn
 -> SortType -> IO ()
treeViewColumnSetSortOrder self order =
  {# call tree_view_column_set_sort_order #}
    self
    ((fromIntegral . fromEnum) order)

-- | Query if a given column is sorted in ascending or descending order.
--
treeViewColumnGetSortOrder :: TreeViewColumn -> IO SortType
treeViewColumnGetSortOrder self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe tree_view_column_get_sort_order #}
    self

--------------------
-- Attributes

-- | Whether to display the column.
--
-- Default value: @True@
--
treeViewColumnVisible :: Attr TreeViewColumn Bool
treeViewColumnVisible = newAttr
  treeViewColumnGetVisible
  treeViewColumnSetVisible

-- | Column is user-resizable.
--
-- Default value: @False@
--
treeViewColumnResizable :: Attr TreeViewColumn Bool
treeViewColumnResizable = newAttr
  treeViewColumnGetResizable
  treeViewColumnSetResizable

-- | Current width of the column.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
treeViewColumnWidth :: ReadAttr TreeViewColumn Int
treeViewColumnWidth = readAttrFromIntProperty "width"

-- | Space which is inserted between cells.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
treeViewColumnSpacing :: Attr TreeViewColumn Int
treeViewColumnSpacing = newAttr
  treeViewColumnGetSpacing
  treeViewColumnSetSpacing

-- | Resize mode of the column.
--
-- Default value: 'TreeViewColumnGrowOnly'
--
treeViewColumnSizing :: Attr TreeViewColumn TreeViewColumnSizing
treeViewColumnSizing = newAttr
  treeViewColumnGetSizing
  treeViewColumnSetSizing

-- | Current fixed width of the column.
--
-- Allowed values: >= 1
--
-- Default value: 1
--
treeViewColumnFixedWidth :: Attr TreeViewColumn Int
treeViewColumnFixedWidth = newAttr
  treeViewColumnGetFixedWidth
  treeViewColumnSetFixedWidth

-- | Minimum allowed width of the column.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
treeViewColumnMinWidth :: Attr TreeViewColumn Int
treeViewColumnMinWidth = newAttr
  treeViewColumnGetMinWidth
  treeViewColumnSetMinWidth

-- | Maximum allowed width of the column.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
treeViewColumnMaxWidth :: Attr TreeViewColumn Int
treeViewColumnMaxWidth = newAttr
  treeViewColumnGetMaxWidth
  treeViewColumnSetMaxWidth

-- | Title to appear in column header.
--
-- Default value: \"\"
--
treeViewColumnTitle :: ReadWriteAttr TreeViewColumn (Maybe String) String
treeViewColumnTitle = newAttr
  treeViewColumnGetTitle
  treeViewColumnSetTitle

-- | Whether the header can be clicked.
--
-- Default value: @False@
--
treeViewColumnClickable :: Attr TreeViewColumn Bool
treeViewColumnClickable = newAttr
  treeViewColumnGetClickable
  treeViewColumnSetClickable

-- | Widget to put in column header button instead of column title.
--
treeViewColumnWidget :: WidgetClass widget => ReadWriteAttr TreeViewColumn Widget widget
treeViewColumnWidget = newAttr
  treeViewColumnGetWidget
  treeViewColumnSetWidget

-- | X Alignment of the column header text or widget.
--
-- Allowed values: [0,1]
--
-- Default value: 0
--
treeViewColumnAlignment :: Attr TreeViewColumn Float
treeViewColumnAlignment = newAttr
  treeViewColumnGetAlignment
  treeViewColumnSetAlignment

-- | Whether the column can be reordered around the headers.
--
-- Default value: @False@
--
treeViewColumnReorderable :: Attr TreeViewColumn Bool
treeViewColumnReorderable = newAttr
  treeViewColumnGetReorderable
  treeViewColumnSetReorderable

-- | Whether to show a sort indicator.
--
-- Default value: @False@
--
treeViewColumnSortIndicator :: Attr TreeViewColumn Bool
treeViewColumnSortIndicator = newAttr
  treeViewColumnGetSortIndicator
  treeViewColumnSetSortIndicator

-- | Sort direction the sort indicator should indicate.
--
-- Default value: 'SortAscending'
--
treeViewColumnSortOrder :: Attr TreeViewColumn SortType
treeViewColumnSortOrder = newAttr
  treeViewColumnGetSortOrder
  treeViewColumnSetSortOrder

-- | \'sortColumnId\' property. See 'treeViewColumnGetSortColumnId' and
-- 'treeViewColumnSetSortColumnId'
--
treeViewColumnSortColumnId :: Attr TreeViewColumn Int
treeViewColumnSortColumnId = newAttr
  treeViewColumnGetSortColumnId
  treeViewColumnSetSortColumnId

--------------------
-- Signals

-- | Emitted when the header of this column has been clicked on.
--
onColClicked, afterColClicked :: TreeViewColumnClass self => self
 -> IO ()
 -> IO (ConnectId self)
onColClicked = connect_NONE__NONE "clicked" False
afterColClicked = connect_NONE__NONE "clicked" True
