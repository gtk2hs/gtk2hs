-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget TreeViewColumn
--
--  Author : Axel Simon
--          
--  Created: 9 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
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
--- DESCRIPTION ---------------------------------------------------------------
--
--
--- DOCU ----------------------------------------------------------------------
--
-- * treeViewColumnSetCellData is not bound. It retrieves the data for one 
--   cell from the @TreeStore and updates the @CellRenderer property to display
--   the change in the @TreeView. This is an internal function and is not 
--   useful to the user: It is automatically called each time the data in the 
--   @TreeModel is changed.
--
--- TODO ----------------------------------------------------------------------

module TreeViewColumn(
  TreeViewColumn,
  TreeViewColumnClass,
  castToTreeViewColumn,
  treeViewColumnNew,
  treeViewColumnAddAttribute,
  treeViewColumnSetVisible,
  treeViewColumnGetVisible,
  TreeViewColumnSizing(..),
  treeViewColumnSetSizing,
  treeViewColumnGetSizing,
  treeViewColumnGetWidth,
  treeViewColumnSetFixedWidth,
  treeViewColumnSetMinWidth,
  treeViewColumnGetMinWidth,
  treeViewColumnSetMaxWidth,
  treeViewColumnGetMaxWidth,
  treeViewColumnSetTitle,
  treeViewColumnGetTitle,
  treeViewColumnSetClickable,
  treeViewColumnSetWidget,
  treeViewColumnGetWidget,
  treeViewColumnSetAlignment,
  treeViewColumnGetAlignment,
  treeViewColumnClicked
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(TreeViewColumnSizing(..))
{#import TreeModel#}
import CellRenderer (Attribute(..))

{# context lib="gtk" prefix="gtk" #}

-- TreeViewColumn type declaration

-- methods

-- Generate a new TreeViewColumn widget. (EXPORTED)
--
treeViewColumnNew :: IO TreeViewColumn
treeViewColumnNew = makeNewObject mkTreeViewColumn 
  {#call tree_view_column_new#}

-- Insert an attribute to change the behaviour of the column's cell renderer.
-- (EXPORTED)
--
treeViewColumnAddAttribute :: 
  (TreeViewColumnClass tvc, CellRendererClass cr) => 
  cr -> String -> Int -> tvc -> IO ()
treeViewColumnAddAttribute cr attr col tvc = 
  withCString attr $ \cstr ->  {#call unsafe tree_view_column_add_attribute#} 
    (toTreeViewColumn tvc) (toCellRenderer cr) cstr (fromIntegral col)

-- Set the visibility of a given column. (EXPORTED)
--
treeViewColumnSetVisible :: TreeViewColumnClass tvc => Bool -> tvc -> IO ()
treeViewColumnSetVisible vis tvc =
  {#call tree_view_column_set_visible#} (toTreeViewColumn tvc) 
    (fromBool vis)


-- Get the visibility of a given column. (EXPORTED)
--
treeViewColumnGetVisible :: TreeViewColumnClass tvc => tvc -> IO Bool
treeViewColumnGetVisible tvc = liftM toBool $
  {#call unsafe tree_view_column_get_visible#} (toTreeViewColumn tvc)


-- Set wether the column can be resized. (EXPORTED)
--
treeViewColumnSetSizing :: TreeViewColumnClass tvc =>
  TreeViewColumnSizing -> tvc -> IO ()
treeViewColumnSetSizing size tvc = {#call tree_view_column_set_sizing#} 
  (toTreeViewColumn tvc) ((fromIntegral.fromEnum) size)


-- Return the resizing type of the column. (EXPORTED)
--
treeViewColumnGetSizing :: TreeViewColumnClass tvc =>
  tvc -> IO TreeViewColumnSizing
treeViewColumnGetSizing tvc = liftM (toEnum.fromIntegral) $
  {#call unsafe tree_view_column_get_sizing#} (toTreeViewColumn tvc)


-- Query the current width of the column. (EXPORTED)
--
treeViewColumnGetWidth :: TreeViewColumnClass tvc => tvc -> IO Int
treeViewColumnGetWidth tvc = liftM fromIntegral $
  {#call unsafe tree_view_column_get_width#} (toTreeViewColumn tvc)


-- Set the width of the column. (EXPORTED)
--
-- * This is meaningful only if the sizing type is @TreeViewColumnFixed.
--
treeViewColumnSetFixedWidth :: TreeViewColumnClass tvc => tvc -> Int -> IO ()
treeViewColumnSetFixedWidth tvc width = 
  {#call tree_view_column_set_fixed_width#} 
  (toTreeViewColumn tvc) (fromIntegral width)


-- Set minimum width of the column. (EXPORTED)
--
treeViewColumnSetMinWidth :: TreeViewColumnClass tvc => Int -> tvc -> IO ()
treeViewColumnSetMinWidth width tvc = {#call tree_view_column_set_min_width#} 
  (toTreeViewColumn tvc) (fromIntegral width)

-- Get the minimum width of a column. Returns -1 if this width was not set.
-- (EXPORTED)
--
treeViewColumnGetMinWidth :: TreeViewColumnClass tvc => tvc -> IO Int
treeViewColumnGetMinWidth tvc = liftM fromIntegral $
  {#call unsafe tree_view_column_get_min_width#} (toTreeViewColumn tvc)

-- Set maximum width of the column. (EXPORTED)
--
treeViewColumnSetMaxWidth :: TreeViewColumnClass tvc => Int -> tvc -> IO ()
treeViewColumnSetMaxWidth width tvc = {#call tree_view_column_set_max_width#} 
  (toTreeViewColumn tvc) (fromIntegral width)

-- Get the maximum width of a column. Returns -1 if this width was not set.
-- (EXPORTED)
--
treeViewColumnGetMaxWidth :: TreeViewColumnClass tvc => tvc -> IO Int
treeViewColumnGetMaxWidth tvc = liftM fromIntegral $
  {#call unsafe tree_view_column_get_max_width#} (toTreeViewColumn tvc)

-- Set the widget's title if a custom widget has not been set. (EXPORTED)
--
treeViewColumnSetTitle :: TreeViewColumnClass tvc => String -> tvc -> IO ()
treeViewColumnSetTitle title tvc = withCString title $
  {#call tree_view_column_set_title#} (toTreeViewColumn tvc)

-- Get the widget's title. (EXPORTED)
--
treeViewColumnGetTitle :: TreeViewColumnClass tvc => tvc -> IO (Maybe String)
treeViewColumnGetTitle tvc = do
  strPtr <- {#call unsafe tree_view_column_get_title#} (toTreeViewColumn tvc)
  if strPtr==nullPtr then return Nothing else liftM Just $ peekCString strPtr

-- Set if the column should be sensible to mouse clicks. (EXPORTED)
--
treeViewColumnSetClickable :: TreeViewColumnClass tvc => Bool -> tvc -> IO ()
treeViewColumnSetClickable click tvc = {#call tree_view_column_set_clickable#} 
  (toTreeViewColumn tvc) (fromBool click)


-- Set the column's title to this widget. (EXPORTED)
--
treeViewColumnSetWidget :: (TreeViewColumnClass tvc, WidgetClass w) =>
  w -> tvc -> IO ()
treeViewColumnSetWidget w tvc =
  {#call tree_view_column_set_widget#} (toTreeViewColumn tvc) (toWidget w)

-- Retrieve the widget responsible for showing the column title. In case
-- only a text title was set this will be a @Alignment widget with a
-- @Label inside. (EXPORTED)
--
treeViewColumnGetWidget :: TreeViewColumnClass tvc => tvc -> IO Widget
treeViewColumnGetWidget tvc = makeNewObject mkWidget $
  {#call unsafe tree_view_column_get_widget#} (toTreeViewColumn tvc)

-- Set the alignment of the title. (EXPORTED)
--
treeViewColumnSetAlignment :: TreeViewColumnClass tvc => Float -> tvc -> IO ()
treeViewColumnSetAlignment align tvc = {#call tree_view_column_set_alignment#}
  (toTreeViewColumn tvc) (realToFrac align)

-- Get the alignment of the titlte. (EXPORTED)
--
treeViewColumnGetAlignment :: TreeViewColumnClass tvc => tvc -> IO Float
treeViewColumnGetAlignment tvc = liftM realToFrac $
  {#call unsafe tree_view_column_get_alignment#} (toTreeViewColumn tvc)

-- Emit the @clicked signal on the column. (EXPORTED)
--
treeViewColumnClicked :: TreeViewColumnClass tvc => tvc -> IO ()
treeViewColumnClicked tvc =
  {#call tree_view_column_clicked#} (toTreeViewColumn tvc)

