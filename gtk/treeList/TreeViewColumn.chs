-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget TreeViewColumn@
--
--  Author : Axel Simon
--          
--  Created: 9 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/24 09:43:25 $
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
-- * treeViewColumnSetCellData is not bound. It retrieves the data for one 
--   cell from the @TreeStore and updates the @CellRenderer property to display
--   the change in the @TreeView. This is an internal function and is not 
--   useful to the user: It is automatically called each time the data in the 
--   @TreeModel is changed.
--
-- @todo@ ---------------------------------------------------------------------

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

-- @constructor treeViewColumnNew@ Generate a new TreeViewColumn widget.
--
treeViewColumnNew :: IO TreeViewColumn
treeViewColumnNew  = makeNewObject mkTreeViewColumn 
  {#call tree_view_column_new#}

-- @method treeViewColumnAddAttribute@ Insert an attribute to change the
-- behaviour of the column's cell renderer.
--
treeViewColumnAddAttribute :: (TreeViewColumnClass tvc, CellRendererClass cr) => 
                              tvc -> cr -> String -> Int -> IO ()
treeViewColumnAddAttribute tvc cr attr col = 
  withCString attr $ \cstr ->  {#call unsafe tree_view_column_add_attribute#} 
    (toTreeViewColumn tvc) (toCellRenderer cr) cstr (fromIntegral col)

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

-- @method treeViewColumnSetClickable@ Set if the column should be sensible to
-- mouse clicks.
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

-- @method treeViewColumnClicked@ Emit the @ref arg clicked@ signal on the
-- column.
--
treeViewColumnClicked :: TreeViewColumnClass tvc => tvc -> IO ()
treeViewColumnClicked tvc =
  {#call tree_view_column_clicked#} (toTreeViewColumn tvc)

