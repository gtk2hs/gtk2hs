-- -*-haskell-*-
--  GIMP Toolkit (GTK) TreeViewColumn TreeView
--
--  Author : Axel Simon
--          
--  Created: 9 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:34:40 $
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
-- |
--
-- * tree_view_column_new_with_attributes and tree_view_column_set_attributes 
--   are variadic and the funcitonality can be achieved through other 
--   functions.
--
-- * tree_view_column_set_cell_data and tree_view_column_cell_get_size are not
--   bound because I am not sure what they do and when they are useful
--
-- TODO
--
-- * treeViewColumnSetCellData is not bound. With this function the user has
--   control over how data in the store is mapped to the attributes of a
--   cell renderer. This functin should be bound in the future to allow the
--   user to insert Haskell data types into the store and convert these
--   values to attributes of cell renderers.
--
module Graphics.UI.Gtk.TreeList.TreeViewColumn (
  TreeViewColumn,
  TreeViewColumnClass,
  castToTreeViewColumn,
  treeViewColumnNew,
  treeViewColumnNewWithAttributes,
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
  onColClicked,
  afterColClicked
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums		(TreeViewColumnSizing(..), SortType(..))
{#import Graphics.UI.Gtk.TreeList.TreeModel#}
import Graphics.UI.Gtk.TreeList.CellRenderer	(Attribute(..))
{#import System.Glib.GList#}

{# context lib="gtk" prefix="gtk" #}

-- TreeViewColumn type declaration

-- methods

-- | Generate a new TreeViewColumn widget.
--
treeViewColumnNew :: IO TreeViewColumn
treeViewColumnNew  = makeNewObject mkTreeViewColumn 
  {#call tree_view_column_new#}

-- | Returns a new TreeViewColumn with title @title@, cell renderer @cr@, and
-- attributes @attribs@.
--
treeViewColumnNewWithAttributes :: CellRendererClass cr => String -> cr -> 
				   [(String, Int)] -> IO TreeViewColumn
treeViewColumnNewWithAttributes title cr attribs =
    do
    tvc <- treeViewColumnNew
    treeViewColumnSetTitle tvc title
    treeViewColumnPackStart tvc  cr True
    treeViewColumnAddAttributes tvc cr attribs
    return tvc

-- | Add a cell renderer at the beginning of
-- a column.
--
-- * Excess space is divided equally among all renderers which have
--   @expand@ set to True.
--
treeViewColumnPackStart :: (TreeViewColumnClass tvc, CellRendererClass cr) =>
			   tvc -> cr -> Bool -> IO ()
treeViewColumnPackStart tvc cr expand = 
  {#call unsafe tree_view_column_pack_start#} (toTreeViewColumn tvc)
  (toCellRenderer cr) (fromBool expand)

-- | Add a cell renderer at the end of a column.
--
-- * Excess space is divided equally among all renderers which have
--   @expand@ set to True.
--
treeViewColumnPackEnd :: (TreeViewColumnClass tvc, CellRendererClass cr) =>
			 tvc -> cr -> Bool -> IO ()
treeViewColumnPackEnd tvc cr expand = 
  {#call unsafe tree_view_column_pack_end#} (toTreeViewColumn tvc)
  (toCellRenderer cr) (fromBool expand)

-- | Remove the associations of attributes
-- to a store for all 'CellRenderers'.
--
treeViewColumnClear :: TreeViewColumnClass tvc => tvc -> IO ()
treeViewColumnClear tvc = 
  {#call tree_view_column_clear#} (toTreeViewColumn tvc)

-- | Retrieve all 
-- 'CellRenderer's that are contained in this column.
--
treeViewColumnGetCellRenderers :: TreeViewColumnClass tvc => 
				  tvc -> IO [CellRenderer]
treeViewColumnGetCellRenderers tvc = do
  glist <- {#call unsafe tree_view_column_get_cell_renderers#} 
	   (toTreeViewColumn tvc)
  crs <- fromGList glist
  mapM (makeNewObject mkCellRenderer) (map return crs)

-- | Insert an attribute to change the
-- behaviour of the column's cell renderer.
--
-- * The 'CellRenderer' @cr@ must already be in 
--   'TreeViewColumn'.
--
treeViewColumnAddAttribute :: (TreeViewColumnClass tvc, CellRendererClass cr)
			      => tvc -> cr -> String -> Int -> IO ()
treeViewColumnAddAttribute tvc cr attr col = 
  withUTFString attr $ \cstr ->  {#call unsafe tree_view_column_add_attribute#} 
    (toTreeViewColumn tvc) (toCellRenderer cr) cstr (fromIntegral col)

-- | Insert attributes @attribs@
-- to change the behaviour of column @tvc@'s cell renderer
-- @cr@.
--
treeViewColumnAddAttributes :: 
  (TreeViewColumnClass tvc, CellRendererClass cr) => 
  tvc -> cr -> [(String,Int)] -> IO ()
treeViewColumnAddAttributes tvc cr attribs = 
    mapM_ (\ (attr, col) -> treeViewColumnAddAttribute tvc cr attr col) attribs

-- | Set the attributes of
-- the cell renderer @cr@ in the tree column @tvc@
-- be  @attribs@.
-- The attributes are given as a list of attribute\/column pairs.
-- All existing attributes are removed, and replaced with the new attributes.
--
treeViewColumnSetAttributes :: 
    (TreeViewColumnClass tvc, CellRendererClass cr)  =>
    tvc -> cr -> [(String, Int)] -> IO ()
treeViewColumnSetAttributes tvc cr attribs =
    do
    treeViewColumnClearAttributes tvc cr
    treeViewColumnAddAttributes tvc cr attribs

-- | Clears all existing attributes
-- of the column @tvc@.
--
treeViewColumnClearAttributes :: 
    (TreeViewColumnClass tvc, CellRendererClass cr) =>
    tvc -> cr -> IO ()
treeViewColumnClearAttributes tvc cr =
  {#call tree_view_column_clear_attributes#} (toTreeViewColumn tvc)
    (toCellRenderer cr)


-- | Set the number of pixels between two
-- cell renderers.
--
treeViewColumnSetSpacing :: TreeViewColumnClass tvc => tvc -> Int -> IO ()
treeViewColumnSetSpacing tvc vis =
  {#call tree_view_column_set_spacing#} (toTreeViewColumn tvc) 
    (fromIntegral vis)


-- | Get the number of pixels between two
-- cell renderers.
--
treeViewColumnGetSpacing :: TreeViewColumnClass tvc => tvc -> IO Int
treeViewColumnGetSpacing tvc = liftM fromIntegral $
  {#call unsafe tree_view_column_get_spacing#} (toTreeViewColumn tvc)


-- | Set the visibility of a given column.
--
treeViewColumnSetVisible :: TreeViewColumnClass tvc => tvc -> Bool -> IO ()
treeViewColumnSetVisible tvc vis =
  {#call tree_view_column_set_visible#} (toTreeViewColumn tvc) 
    (fromBool vis)


-- | Get the visibility of a given column.
--
treeViewColumnGetVisible :: TreeViewColumnClass tvc => tvc -> IO Bool
treeViewColumnGetVisible tvc = liftM toBool $
  {#call unsafe tree_view_column_get_visible#} (toTreeViewColumn tvc)


-- | Set if a given column is resizable
-- by the user.
--
treeViewColumnSetResizable :: TreeViewColumnClass tvc => tvc -> Bool -> IO ()
treeViewColumnSetResizable tvc vis =
  {#call tree_view_column_set_resizable#} (toTreeViewColumn tvc) 
    (fromBool vis)


-- | Get if a given column is resizable
-- by the user.
--
treeViewColumnGetResizable :: TreeViewColumnClass tvc => tvc -> IO Bool
treeViewColumnGetResizable tvc = liftM toBool $
  {#call unsafe tree_view_column_get_resizable#} (toTreeViewColumn tvc)


-- | Set wether the column can be resized.
--
treeViewColumnSetSizing :: TreeViewColumnClass tvc => tvc ->
                           TreeViewColumnSizing -> IO ()
treeViewColumnSetSizing tvc size = {#call tree_view_column_set_sizing#} 
  (toTreeViewColumn tvc) ((fromIntegral.fromEnum) size)


-- | Return the resizing type of the column.
--
treeViewColumnGetSizing :: TreeViewColumnClass tvc => tvc ->
                           IO TreeViewColumnSizing
treeViewColumnGetSizing tvc = liftM (toEnum.fromIntegral) $
  {#call unsafe tree_view_column_get_sizing#} (toTreeViewColumn tvc)


-- | Query the current width of the column.
--
treeViewColumnGetWidth :: TreeViewColumnClass tvc => tvc -> IO Int
treeViewColumnGetWidth tvc = liftM fromIntegral $
  {#call unsafe tree_view_column_get_width#} (toTreeViewColumn tvc)


-- | Set the width of the column.
--
-- * This is meaningful only if the sizing type is 'TreeViewColumnFixed'.
--
treeViewColumnSetFixedWidth :: TreeViewColumnClass tvc => tvc -> Int -> IO ()
treeViewColumnSetFixedWidth tvc width = 
  {#call tree_view_column_set_fixed_width#} 
  (toTreeViewColumn tvc) (fromIntegral width)


-- | Gets the fixed width of the column.
--
-- * This is meaningful only if the sizing type is 'TreeViewColumnFixed'.
--
-- * This value is only meaning may not be the actual width of the column on the
-- screen, just what is requested.
--
treeViewColumnGetFixedWidth :: TreeViewColumnClass tvc => tvc -> IO Int
treeViewColumnGetFixedWidth tvc = liftM fromIntegral $
  {#call unsafe tree_view_column_get_fixed_width#} (toTreeViewColumn tvc)


-- | Set minimum width of the column.
--
treeViewColumnSetMinWidth :: TreeViewColumnClass tvc => tvc -> Int -> IO ()
treeViewColumnSetMinWidth tvc width = {#call tree_view_column_set_min_width#} 
  (toTreeViewColumn tvc) (fromIntegral width)

-- | Get the minimum width of a column.
-- Returns -1 if this width was not set.
--
treeViewColumnGetMinWidth :: TreeViewColumnClass tvc => tvc -> IO Int
treeViewColumnGetMinWidth tvc = liftM fromIntegral $
  {#call unsafe tree_view_column_get_min_width#} (toTreeViewColumn tvc)

-- | Set maximum width of the column.
--
treeViewColumnSetMaxWidth :: TreeViewColumnClass tvc => tvc -> Int -> IO ()
treeViewColumnSetMaxWidth tvc width = {#call tree_view_column_set_max_width#} 
  (toTreeViewColumn tvc) (fromIntegral width)

-- | Get the maximum width of a column.
-- Returns -1 if this width was not set.
--
treeViewColumnGetMaxWidth :: TreeViewColumnClass tvc => tvc -> IO Int
treeViewColumnGetMaxWidth tvc = liftM fromIntegral $
  {#call unsafe tree_view_column_get_max_width#} (toTreeViewColumn tvc)

-- | Emit the @clicked@ signal on the
-- column.
--
treeViewColumnClicked :: TreeViewColumnClass tvc => tvc -> IO ()
treeViewColumnClicked tvc =
  {#call tree_view_column_clicked#} (toTreeViewColumn tvc)

-- | Set the widget's title if a custom widget
-- has not been set.
--
treeViewColumnSetTitle :: TreeViewColumnClass tvc => tvc -> String -> IO ()
treeViewColumnSetTitle tvc title = withUTFString title $
  {#call tree_view_column_set_title#} (toTreeViewColumn tvc)

-- | Get the widget's title.
--
treeViewColumnGetTitle :: TreeViewColumnClass tvc => tvc -> IO (Maybe String)
treeViewColumnGetTitle tvc = do
  strPtr <- {#call unsafe tree_view_column_get_title#} (toTreeViewColumn tvc)
  if strPtr==nullPtr then return Nothing else liftM Just $ peekUTFString strPtr

-- | Set if the column should be sensitive to mouse clicks.
--
treeViewColumnSetClickable :: TreeViewColumnClass tvc => tvc -> Bool -> IO ()
treeViewColumnSetClickable tvc click = {#call tree_view_column_set_clickable#} 
  (toTreeViewColumn tvc) (fromBool click)

-- | Returns True if the user can click on the header for the column.
--
treeViewColumnGetClickable :: TreeViewColumnClass tvc => tvc -> IO Bool
treeViewColumnGetClickable tvc = liftM toBool $
  {#call tree_view_column_get_clickable#} (toTreeViewColumn tvc)

-- | Set the column's title to this widget.
--
treeViewColumnSetWidget :: (TreeViewColumnClass tvc, WidgetClass w) => tvc ->
                           w -> IO ()
treeViewColumnSetWidget tvc w =
  {#call tree_view_column_set_widget#} (toTreeViewColumn tvc) (toWidget w)

-- | Retrieve the widget responsible for
-- showing the column title. In case only a text title was set this will be a
-- 'Alignment' widget with a 'Label' inside.
--
treeViewColumnGetWidget :: TreeViewColumnClass tvc => tvc -> IO Widget
treeViewColumnGetWidget tvc = makeNewObject mkWidget $
  {#call unsafe tree_view_column_get_widget#} (toTreeViewColumn tvc)

-- | Set the alignment of the title.
--
treeViewColumnSetAlignment :: TreeViewColumnClass tvc => tvc -> Float -> IO ()
treeViewColumnSetAlignment tvc align = {#call tree_view_column_set_alignment#}
  (toTreeViewColumn tvc) (realToFrac align)

-- | Get the alignment of the titlte.
--
treeViewColumnGetAlignment :: TreeViewColumnClass tvc => tvc -> IO Float
treeViewColumnGetAlignment tvc = liftM realToFrac $
  {#call unsafe tree_view_column_get_alignment#} (toTreeViewColumn tvc)

-- | Set if a given column is reorderable
-- by the user.
--
treeViewColumnSetReorderable :: TreeViewColumnClass tvc => tvc -> Bool -> IO ()
treeViewColumnSetReorderable tvc vis =
  {#call tree_view_column_set_reorderable#} (toTreeViewColumn tvc) 
    (fromBool vis)

-- | Get if a given column is reorderable
-- by the user.
--
treeViewColumnGetReorderable :: TreeViewColumnClass tvc => tvc -> IO Bool
treeViewColumnGetReorderable tvc = liftM toBool $
  {#call unsafe tree_view_column_get_reorderable#} (toTreeViewColumn tvc)

-- | Set the column by which to sort.
--
-- * Sets the logical @columnId@ that this column sorts on when
--   this column is selected for sorting. The selected column's header
--   will be clickable after this call. Logical refers to the column in
--   the 'TreeModel'.
--
treeViewColumnSetSortColumnId :: TreeViewColumnClass tvc => 
				 tvc -> Int -> IO ()
treeViewColumnSetSortColumnId tvc columnId = 
  {#call tree_view_column_set_sort_column_id#} 
  (toTreeViewColumn tvc) (fromIntegral columnId)

-- | Get the column by which to sort.
--
-- * Retrieves the logical @columnId@ that the model sorts on when
--   this column is selected for sorting.
--
-- * Returns -1 if this column can't be used for sorting.
--
treeViewColumnGetSortColumnId :: TreeViewColumnClass tvc => tvc -> IO Int
treeViewColumnGetSortColumnId tvc = liftM fromIntegral $
  {#call unsafe tree_view_column_get_sort_column_id#} (toTreeViewColumn tvc)

-- | Set if a given column has
-- sorting arrows in its heading.
--
treeViewColumnSetSortIndicator :: TreeViewColumnClass tvc => 
				  tvc -> Bool -> IO ()
treeViewColumnSetSortIndicator tvc sort =
  {#call tree_view_column_set_sort_indicator#} (toTreeViewColumn tvc) 
    (fromBool sort)

-- | Query if a given column has
-- sorting arrows in its heading.
--
treeViewColumnGetSortIndicator :: TreeViewColumnClass tvc => tvc -> IO Bool
treeViewColumnGetSortIndicator tvc = liftM toBool $
  {#call unsafe tree_view_column_get_sort_indicator#} (toTreeViewColumn tvc)

-- | Set if a given column is sorted
-- in ascending or descending order.
--
-- * In order for sorting to work, it is necessary to either use automatic
--   sorting via 'treeViewColumnSetSortColumnId' or to use a
--   user defined sorting on the elements in a 'TreeModel'.
--
treeViewColumnSetSortOrder :: TreeViewColumnClass tvc => 
			      tvc -> SortType -> IO ()
treeViewColumnSetSortOrder tvc sort =
  {#call tree_view_column_set_sort_order#} (toTreeViewColumn tvc) 
    ((fromIntegral.fromEnum) sort)

-- | Query if a given column is sorted
-- in ascending or descending order.
--
treeViewColumnGetSortOrder :: TreeViewColumnClass tvc => tvc -> IO SortType
treeViewColumnGetSortOrder tvc = liftM (toEnum.fromIntegral) $
  {#call unsafe tree_view_column_get_sort_order#} (toTreeViewColumn tvc)

-- | Emitted when the header of this column has been
-- clicked on.
--
onColClicked, afterColClicked :: TreeViewColumnClass tvc => tvc -> IO () -> 
							    IO (ConnectId tvc)
onColClicked = connect_NONE__NONE "clicked" False
afterColClicked = connect_NONE__NONE "clicked" True
