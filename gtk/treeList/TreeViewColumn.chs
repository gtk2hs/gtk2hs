-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget TreeViewColumn@
--
--  Author : Axel Simon
--          
--  Created: 9 May 2001
--
--  Version $Revision: 1.4 $ from $Date: 2002/07/17 15:59:19 $
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
--
-- @documentation@ ------------------------------------------------------------
--
-- * tree_view_column_new_with_attributes and tree_view_column_set_attributes 
--   are variadic and the funcitonality can be achieved through other 
--   functions.
--
-- * tree_view_column_set_cell_data and tree_view_column_cell_get_size are not
--   bound because I am not sure what they do and when they are useful
--
-- @todo@ ---------------------------------------------------------------------
--
-- * treeViewColumnSetCellData is not bound. With this function the user has
--   control over how data in the store is mapped to the attributes of a
--   cell renderer. This functin should be bound in the future to allow the
--   user to insert Haskell data types into the store and convert these
--   values to attributes of cell renderers.
--
module TreeViewColumn(
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
  treeViewColumnSetMinWidth,
  treeViewColumnGetMinWidth,
  treeViewColumnSetMaxWidth,
  treeViewColumnGetMaxWidth,
  treeViewColumnClicked,
  treeViewColumnSetTitle,
  treeViewColumnGetTitle,
  treeViewColumnSetClickable,
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
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(TreeViewColumnSizing(..), SortType(..))
{#import TreeModel#}
import CellRenderer (Attribute(..))
{#import GList#}

{# context lib="gtk" prefix="gtk" #}

-- TreeViewColumn type declaration

-- methods

-- @constructor treeViewColumnNew@ Generate a new TreeViewColumn widget.
--
treeViewColumnNew :: IO TreeViewColumn
treeViewColumnNew  = makeNewObject mkTreeViewColumn 
  {#call tree_view_column_new#}

-- @method treeViewColumnNewWithAttributes@ Returns a new TreeViewColumn with
-- title @ref arg title@, cell renderer @ref arg cr@, and attributes
-- @ref arg attribs@.
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

-- @method treeViewColumnPackStart@ Add a cell renderer at the beginning of
-- a column.
--
-- * Excess space is divided equally among all renderers which have
--   @arg expand@ set to True.
--
treeViewColumnPackStart :: (TreeViewColumnClass tvc, CellRendererClass cr) =>
			   tvc -> cr -> Bool -> IO ()
treeViewColumnPackStart tvc cr expand = 
  {#call unsafe tree_view_column_pack_start#} (toTreeViewColumn tvc)
  (toCellRenderer cr) (fromBool expand)

-- @method treeViewColumnPackEnd@ Add a cell renderer at the end of a column.
--
-- * Excess space is divided equally among all renderers which have
--   @arg expand@ set to True.
--
treeViewColumnPackEnd :: (TreeViewColumnClass tvc, CellRendererClass cr) =>
			 tvc -> cr -> Bool -> IO ()
treeViewColumnPackEnd tvc cr expand = 
  {#call unsafe tree_view_column_pack_end#} (toTreeViewColumn tvc)
  (toCellRenderer cr) (fromBool expand)

-- @method treeViewColumnClear@ Remove the associations of attributes
-- to a store for all @ref type CellRenderers@.
--
treeViewColumnClear :: TreeViewColumnClass tvc => tvc -> IO ()
treeViewColumnClear tvc = 
  {#call tree_view_column_clear#} (toTreeViewColumn tvc)

-- @method treeViewColumnGetCellRenderers@ Retrieve all 
-- @ref type CellRenderer@s that are contained in this column.
--
treeViewColumnGetCellRenderers :: TreeViewColumnClass tvc => 
				  tvc -> IO [CellRenderer]
treeViewColumnGetCellRenderers tvc = do
  glist <- {#call unsafe tree_view_column_get_cell_renderers#} 
	   (toTreeViewColumn tvc)
  crs <- fromGList glist
  mapM (makeNewObject mkCellRenderer) (map return crs)

-- @method treeViewColumnAddAttribute@ Insert an attribute to change the
-- behaviour of the column's cell renderer.
--
-- * The @ref type CellRenderer@ @ref arg cr@ must already be in 
--   @ref type TreeViewColumn@.
--
treeViewColumnAddAttribute :: (TreeViewColumnClass tvc, CellRendererClass cr)
			      => tvc -> cr -> String -> Int -> IO ()
treeViewColumnAddAttribute tvc cr attr col = 
  withCString attr $ \cstr ->  {#call unsafe tree_view_column_add_attribute#} 
    (toTreeViewColumn tvc) (toCellRenderer cr) cstr (fromIntegral col)

-- @method treeViewColumnAddAttributes@ Insert attributes @ref arg attribs@
-- to change the behaviour of column @ref arg tvc@'s cell renderer
-- @ref arg cr@.
--
treeViewColumnAddAttributes :: 
  (TreeViewColumnClass tvc, CellRendererClass cr) => 
  tvc -> cr -> [(String,Int)] -> IO ()
treeViewColumnAddAttributes tvc cr attribs = 
    mapM_ (\ (attr, col) -> treeViewColumnAddAttribute tvc cr attr col) attribs

-- @method treeViewColumnSetAttributes@ Set the attributes of
-- the cell renderer @ref arg cr@ in the tree column @ref arg tvc@
-- be  @red arg attribs@.
-- The attributes are given as a list of attribute/column pairs.
-- All existing attributes are removed, and replaced with the new attributes.
--
treeViewColumnSetAttributes :: 
    (TreeViewColumnClass tvc, CellRendererClass cr)  =>
    tvc -> cr -> [(String, Int)] -> IO ()
treeViewColumnSetAttributes tvc cr attribs =
    do
    treeViewColumnClearAttributes tvc cr
    treeViewColumnAddAttributes tvc cr attribs

-- @method treeViewColumnClearAttributes@ Clears all existing attributes
-- of the column @ref arg tvc@.
--
treeViewColumnClearAttributes :: 
    (TreeViewColumnClass tvc, CellRendererClass cr) =>
    tvc -> cr -> IO ()
treeViewColumnClearAttributes tvc cr =
  {#call tree_view_column_clear_attributes#} (toTreeViewColumn tvc)
    (toCellRenderer cr)


-- @method treeViewColumnSetSpacing@ Set the number of pixels between two
-- cell renderers.
--
treeViewColumnSetSpacing :: TreeViewColumnClass tvc => tvc -> Int -> IO ()
treeViewColumnSetSpacing tvc vis =
  {#call tree_view_column_set_spacing#} (toTreeViewColumn tvc) 
    (fromIntegral vis)


-- @method treeViewColumnGetSpacing@ Get the number of pixels between two
-- cell renderers.
--
treeViewColumnGetSpacing :: TreeViewColumnClass tvc => tvc -> IO Int
treeViewColumnGetSpacing tvc = liftM fromIntegral $
  {#call unsafe tree_view_column_get_spacing#} (toTreeViewColumn tvc)


-- @method treeViewColumnSetVisible@ Set the visibility of a given column.
--
treeViewColumnSetVisible :: TreeViewColumnClass tvc => tvc -> Bool -> IO ()
treeViewColumnSetVisible tvc vis =
  {#call tree_view_column_set_visible#} (toTreeViewColumn tvc) 
    (fromBool vis)


-- @method treeViewColumnGetVisible@ Get the visibility of a given column.
--
treeViewColumnGetVisible :: TreeViewColumnClass tvc => tvc -> IO Bool
treeViewColumnGetVisible tvc = liftM toBool $
  {#call unsafe tree_view_column_get_visible#} (toTreeViewColumn tvc)


-- @method treeViewColumnSetResizable@ Set if a given column is resizable
-- by the user.
--
treeViewColumnSetResizable :: TreeViewColumnClass tvc => tvc -> Bool -> IO ()
treeViewColumnSetResizable tvc vis =
  {#call tree_view_column_set_resizable#} (toTreeViewColumn tvc) 
    (fromBool vis)


-- @method treeViewColumnGetResizable@ Get if a given column is resizable
-- by the user.
--
treeViewColumnGetResizable :: TreeViewColumnClass tvc => tvc -> IO Bool
treeViewColumnGetResizable tvc = liftM toBool $
  {#call unsafe tree_view_column_get_resizable#} (toTreeViewColumn tvc)


-- @method treeViewColumnSetSizing@ Set wether the column can be resized.
--
treeViewColumnSetSizing :: TreeViewColumnClass tvc => tvc ->
                           TreeViewColumnSizing -> IO ()
treeViewColumnSetSizing tvc size = {#call tree_view_column_set_sizing#} 
  (toTreeViewColumn tvc) ((fromIntegral.fromEnum) size)


-- @method treeViewColumnGetSizing@ Return the resizing type of the column.
--
treeViewColumnGetSizing :: TreeViewColumnClass tvc => tvc ->
                           IO TreeViewColumnSizing
treeViewColumnGetSizing tvc = liftM (toEnum.fromIntegral) $
  {#call unsafe tree_view_column_get_sizing#} (toTreeViewColumn tvc)


-- @method treeViewColumnGetWidth@ Query the current width of the column.
--
treeViewColumnGetWidth :: TreeViewColumnClass tvc => tvc -> IO Int
treeViewColumnGetWidth tvc = liftM fromIntegral $
  {#call unsafe tree_view_column_get_width#} (toTreeViewColumn tvc)


-- @method treeViewColumnSetFixedWidth@ Set the width of the column.
--
-- * This is meaningful only if the sizing type is
--   @ref type TreeViewColumnFixed@.
--
treeViewColumnSetFixedWidth :: TreeViewColumnClass tvc => Int -> tvc -> IO ()
treeViewColumnSetFixedWidth width tvc = 
  {#call tree_view_column_set_fixed_width#} 
  (toTreeViewColumn tvc) (fromIntegral width)


-- @method treeViewColumnSetMinWidth@ Set minimum width of the column.
--
treeViewColumnSetMinWidth :: TreeViewColumnClass tvc => tvc -> Int -> IO ()
treeViewColumnSetMinWidth tvc width = {#call tree_view_column_set_min_width#} 
  (toTreeViewColumn tvc) (fromIntegral width)

-- @method treeViewColumnGetMinWidth@ Get the minimum width of a column.
-- Returns -1 if this width was not set.
--
treeViewColumnGetMinWidth :: TreeViewColumnClass tvc => tvc -> IO Int
treeViewColumnGetMinWidth tvc = liftM fromIntegral $
  {#call unsafe tree_view_column_get_min_width#} (toTreeViewColumn tvc)

-- @method treeViewColumnSetMaxWidth@ Set maximum width of the column.
--
treeViewColumnSetMaxWidth :: TreeViewColumnClass tvc => tvc -> Int -> IO ()
treeViewColumnSetMaxWidth tvc width = {#call tree_view_column_set_max_width#} 
  (toTreeViewColumn tvc) (fromIntegral width)

-- @method treeViewColumnGetMaxWidth@ Get the maximum width of a column.
-- Returns -1 if this width was not set.
--
treeViewColumnGetMaxWidth :: TreeViewColumnClass tvc => tvc -> IO Int
treeViewColumnGetMaxWidth tvc = liftM fromIntegral $
  {#call unsafe tree_view_column_get_max_width#} (toTreeViewColumn tvc)

-- @method treeViewColumnClicked@ Emit the @ref arg clicked@ signal on the
-- column.
--
treeViewColumnClicked :: TreeViewColumnClass tvc => tvc -> IO ()
treeViewColumnClicked tvc =
  {#call tree_view_column_clicked#} (toTreeViewColumn tvc)

-- @method treeViewColumnSetTitle@ Set the widget's title if a custom widget
-- has not been set.
--
treeViewColumnSetTitle :: TreeViewColumnClass tvc => tvc -> String -> IO ()
treeViewColumnSetTitle tvc title = withCString title $
  {#call tree_view_column_set_title#} (toTreeViewColumn tvc)

-- @method treeViewColumnGetTitle@ Get the widget's title.
--
treeViewColumnGetTitle :: TreeViewColumnClass tvc => tvc -> IO (Maybe String)
treeViewColumnGetTitle tvc = do
  strPtr <- {#call unsafe tree_view_column_get_title#} (toTreeViewColumn tvc)
  if strPtr==nullPtr then return Nothing else liftM Just $ peekCString strPtr

-- @method treeViewColumnSetClickable@ Set if the column should be sensitive
-- to mouse clicks.
--
treeViewColumnSetClickable :: TreeViewColumnClass tvc => tvc -> Bool -> IO ()
treeViewColumnSetClickable tvc click = {#call tree_view_column_set_clickable#} 
  (toTreeViewColumn tvc) (fromBool click)

-- @method treeViewColumnSetWidget@ Set the column's title to this widget.
--
treeViewColumnSetWidget :: (TreeViewColumnClass tvc, WidgetClass w) => tvc ->
                           w -> IO ()
treeViewColumnSetWidget tvc w =
  {#call tree_view_column_set_widget#} (toTreeViewColumn tvc) (toWidget w)

-- @method treeViewColumnGetWidget@ Retrieve the widget responsible for
-- showing the column title. In case only a text title was set this will be a
-- @ref arg Alignment@ widget with a @ref arg Label@ inside.
--
treeViewColumnGetWidget :: TreeViewColumnClass tvc => tvc -> IO Widget
treeViewColumnGetWidget tvc = makeNewObject mkWidget $
  {#call unsafe tree_view_column_get_widget#} (toTreeViewColumn tvc)

-- @method treeViewColumnSetAlignment@ Set the alignment of the title.
--
treeViewColumnSetAlignment :: TreeViewColumnClass tvc => tvc -> Float -> IO ()
treeViewColumnSetAlignment tvc align = {#call tree_view_column_set_alignment#}
  (toTreeViewColumn tvc) (realToFrac align)

-- @method treeViewColumnGetAlignment@ Get the alignment of the titlte.
--
treeViewColumnGetAlignment :: TreeViewColumnClass tvc => tvc -> IO Float
treeViewColumnGetAlignment tvc = liftM realToFrac $
  {#call unsafe tree_view_column_get_alignment#} (toTreeViewColumn tvc)

-- @method treeViewColumnSetReorderable@ Set if a given column is reorderable
-- by the user.
--
treeViewColumnSetReorderable :: TreeViewColumnClass tvc => tvc -> Bool -> IO ()
treeViewColumnSetReorderable tvc vis =
  {#call tree_view_column_set_reorderable#} (toTreeViewColumn tvc) 
    (fromBool vis)

-- @method treeViewColumnGetReorderable@ Get if a given column is reorderable
-- by the user.
--
treeViewColumnGetReorderable :: TreeViewColumnClass tvc => tvc -> IO Bool
treeViewColumnGetReorderable tvc = liftM toBool $
  {#call unsafe tree_view_column_get_reorderable#} (toTreeViewColumn tvc)

-- @method treeViewColumnSetSortColumnId@ Set the column by which to sort.
--
-- * Sets the logical @ref arg columnId@ that this column sorts on when
--   this column is selected for sorting. The selected column's header
--   will be clickable after this call. Logical refers to the column in
--   the @ref type TreeModel@.
--
treeViewColumnSetSortColumnId :: TreeViewColumnClass tvc => 
				 tvc -> Int -> IO ()
treeViewColumnSetSortColumnId tvc columnId = 
  {#call tree_view_column_set_sort_column_id#} 
  (toTreeViewColumn tvc) (fromIntegral columnId)

-- @method treeViewColumnGetSortColumnId@ Get the column by which to sort.
--
-- * Retrieves the logical @ref arg columnId@ that the model sorts on when
--   this column is selected for sorting.
--
-- * Returns -1 if this column can't be used for sorting.
--
treeViewColumnGetSortColumnId :: TreeViewColumnClass tvc => tvc -> IO Int
treeViewColumnGetSortColumnId tvc = liftM fromIntegral $
  {#call unsafe tree_view_column_get_sort_column_id#} (toTreeViewColumn tvc)

-- @method treeViewColumnSetSortIndicator@ Set if a given column has
-- sorting arrows in its heading.
--
treeViewColumnSetSortIndicator :: TreeViewColumnClass tvc => 
				  tvc -> Bool -> IO ()
treeViewColumnSetSortIndicator tvc sort =
  {#call tree_view_column_set_sort_indicator#} (toTreeViewColumn tvc) 
    (fromBool sort)

-- @method treeViewColumnGetSortIndicator@ Query if a given column has
-- sorting arrows in its heading.
--
treeViewColumnGetSortIndicator :: TreeViewColumnClass tvc => tvc -> IO Bool
treeViewColumnGetSortIndicator tvc = liftM toBool $
  {#call unsafe tree_view_column_get_sort_indicator#} (toTreeViewColumn tvc)

-- @method treeViewColumnSetSortOrder@ Set if a given column is sorted
-- in ascending or descending order.
--
-- * In order for sorting to work, it is necessary to either use automatic
--   sorting via @ref method treeViewColumnSetSortColumnId@ or to use a
--   user defined sorting on the elements in a @ref type TreeModel@.
--
treeViewColumnSetSortOrder :: TreeViewColumnClass tvc => 
			      tvc -> SortType -> IO ()
treeViewColumnSetSortOrder tvc sort =
  {#call tree_view_column_set_sort_order#} (toTreeViewColumn tvc) 
    ((fromIntegral.fromEnum) sort)

-- @method treeViewColumnGetSortOrder@ Query if a given column is sorted
-- in ascending or descending order.
--
treeViewColumnGetSortOrder :: TreeViewColumnClass tvc => tvc -> IO SortType
treeViewColumnGetSortOrder tvc = liftM (toEnum.fromIntegral) $
  {#call unsafe tree_view_column_get_sort_order#} (toTreeViewColumn tvc)

-- @signal colClicked@ Emitted when the header of this column has been
-- clicked on.
--
onColClicked, afterColClicked :: TreeViewColumnClass tvc => tvc -> IO () -> 
							    IO (ConnectId tvc)
onColClicked = connect_NONE__NONE "clicked" False
afterColClicked = connect_NONE__NONE "clicked" True
