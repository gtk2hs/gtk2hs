{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ToolItemGroup
--
--  Author : Andy Stewart
--
--  Created: 08 Sep 2010
--
--  Copyright (C) 2010 Andy Stewart
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
-- A sub container used in a tool palette
--
-- * Module available since Gtk+ version 2.20
--
module Graphics.UI.Gtk.MenuComboToolbar.ToolItemGroup (

-- * Detail
-- | A 'ToolItemGroup' is used together with 'ToolPalette' to add 'ToolItems' to a palette like
-- container with different categories and drag and drop support.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'ToolItemGroup'
-- @

#if GTK_CHECK_VERSION(2,20,0)
-- * Types
  ToolItemGroup,
  ToolItemGroupClass,
  castToToolItemGroup,
  toToolItemGroup,

-- * Constructors
  toolItemGroupNew,

-- * Methods
  toolItemGroupGetDropItem,
  toolItemGroupGetItemPosition,
  toolItemGroupGetNItems,
  toolItemGroupGetNthItem,
  toolItemGroupInsert,
  toolItemGroupSetItemPosition,

-- * Attributes
  toolItemGroupCollapsed,
  toolItemGroupEllipsize,
  toolItemGroupHeaderRelief,
  toolItemGroupLabel,
  toolItemGroupLabelWidget,

-- * Child Attributes
  toolItemGroupChildExpand,
  toolItemGroupChildFill,
  toolItemGroupChildHomogeneous,
  toolItemGroupChildNewRow,
  toolItemGroupChildPosition,
#endif
) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.UTFString
import Graphics.Rendering.Pango.Enums   (EllipsizeMode (..))
import Graphics.UI.Gtk.General.Enums    (ReliefStyle(..))
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,20,0)
-- | Creates a new tool item group with label label.
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupNew :: GlibString string => string -- ^ @label@   the label of the new group
                 -> IO ToolItemGroup -- ^ returns a new 'ToolItemGroup'.
toolItemGroupNew label =
  makeNewObject mkToolItemGroup $
  liftM (castPtr :: Ptr Widget -> Ptr ToolItemGroup) $
  withUTFString label $ \ labelPtr ->
    {#call gtk_tool_item_group_new #}
      labelPtr

-- | Gets the tool item at position (x, y).
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupGetDropItem :: ToolItemGroupClass self => self
                         -> (Int, Int)
                         -> IO ToolItem
toolItemGroupGetDropItem group (x, y) =
  makeNewObject mkToolItem $
  {#call gtk_tool_item_group_get_drop_item #}
    (toToolItemGroup group)
    (fromIntegral x)
    (fromIntegral y)

-- | Gets the position of item in group as index.
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupGetItemPosition :: (ToolItemGroupClass group, ToolItemClass item)
                               => group -- ^ @group@   a 'ToolItemGroup'
                               -> item -- ^ @item@    a 'ToolItem'
                               -> IO Int -- ^ returns the index of item in group or -1 if item is no child of group
toolItemGroupGetItemPosition group item =
    liftM fromIntegral $
    {#call gtk_tool_item_group_get_item_position #}
      (toToolItemGroup group)
      (toToolItem item)

-- | Gets the number of tool items in group.
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupGetNItems :: ToolItemGroupClass group => group
                       -> IO Int -- ^ returns the number of tool items in group
toolItemGroupGetNItems group =
    liftM fromIntegral $
    {#call gtk_tool_item_group_get_n_items #}
      (toToolItemGroup group)

-- | Gets the tool item at index in group.
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupGetNthItem :: ToolItemGroupClass group => group
                        -> Int  -- ^ @index@   the index
                        -> IO ToolItem  -- ^ returns the 'ToolItem' at index
toolItemGroupGetNthItem group index =
  makeNewObject mkToolItem $
  {#call gtk_tool_item_group_get_nth_item #}
    (toToolItemGroup group)
    (fromIntegral index)

-- | Inserts item at position in the list of children of group.
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupInsert :: (ToolItemGroupClass group, ToolItemClass item)
                     => group -- ^ @group@    a 'ToolItemGroup'
                     -> item -- ^ @item@     the 'ToolItem' to insert into group
                     -> Int -- ^ @position@ the position of item in group, starting with 0.
                           -- The position -1 means end of list.
                     -> IO ()
toolItemGroupInsert group item position =
  {#call gtk_tool_item_group_insert #}
    (toToolItemGroup group)
    (toToolItem item)
    (fromIntegral position)

-- | Sets the position of item in the list of children of group.
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupSetItemPosition :: (ToolItemGroupClass group, ToolItemClass item)
                               => group -- ^ @group@    a 'ToolItemGroup'
                               -> item -- ^ @item@     the 'ToolItem' to move to a new position, should be a child of group.
                               -> Int  -- ^ @position@ the new position of item in group, starting with 0. The position -1 means end of list.
                               -> IO ()
toolItemGroupSetItemPosition group item position =
  {#call gtk_tool_item_group_set_item_position #}
    (toToolItemGroup group)
    (toToolItem item)
    (fromIntegral position)

-- | Wether the group has been collapsed and items are hidden.
--
-- Default value: 'False'
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupCollapsed :: ToolItemGroupClass group => Attr group Bool
toolItemGroupCollapsed =
  newAttrFromBoolProperty "collapsed"

-- | Ellipsize for item group headers.
--
-- Default value: EllipsizeNone
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupEllipsize :: ToolItemGroupClass group => Attr group EllipsizeMode
toolItemGroupEllipsize =
  newAttrFromEnumProperty "ellipsize"
     {# call pure unsafe pango_ellipsize_mode_get_type #}

-- | Relief of the group header button.
--
-- Default value: 'ReliefNormal'
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupHeaderRelief :: ToolItemGroupClass group => Attr group ReliefStyle
toolItemGroupHeaderRelief =
  newAttrFromEnumProperty "header-relief"
     {# call pure unsafe gtk_relief_style_get_type #}

-- | The human-readable title of this item group.
--
-- Default value: \"\"
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupLabel :: GlibString string => ToolItemGroupClass group => Attr group string
toolItemGroupLabel =
  newAttrFromStringProperty "label"

-- | A widget to display in place of the usual label.
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupLabelWidget :: ToolItemGroupClass group => Attr group Widget
toolItemGroupLabelWidget =
  newAttrFromObjectProperty "label-widget"
      {# call pure unsafe gtk_widget_get_type #}

-- | Whether the item should receive extra space when the group grows.
--
-- Default value: 'False'
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupChildExpand :: ToolItemGroupClass group => Attr group Bool
toolItemGroupChildExpand =
  newAttrFromBoolProperty "expand"

-- | Whether the item should fill the available space.
--
-- Default value: 'True'
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupChildFill :: ToolItemGroupClass group => Attr group Bool
toolItemGroupChildFill =
  newAttrFromBoolProperty "fill"

-- | Whether the item should be the same size as other homogeneous items.
--
-- Default value: 'True'
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupChildHomogeneous :: ToolItemGroupClass group => Attr group Bool
toolItemGroupChildHomogeneous =
  newAttrFromBoolProperty "homogeneous"

-- | Whether the item should start a new row.
--
-- Default value: 'False'
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupChildNewRow :: ToolItemGroupClass group => Attr group Bool
toolItemGroupChildNewRow =
  newAttrFromBoolProperty "new-row"

-- | Position of the item within this group.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
-- * Available since Gtk+ version 2.20
--
toolItemGroupChildPosition :: ToolItemGroupClass group => Attr group Int
toolItemGroupChildPosition =
  newAttrFromIntProperty "position"
#endif

