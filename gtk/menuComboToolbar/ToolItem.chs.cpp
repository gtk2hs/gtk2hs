-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget ToolItem
--
--  Author : Duncan Coutts
--  Created: 1 August 2004
--
--  Copyright (c) 2004 Duncan Coutts
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- |
--
-- ToolItem is the base class of widgets that can be added to "Toolbar".
--
-- ToolItems are widgets that can appear on a toolbar.
--
-- * For toolbar items that contain buttons, see the 'ToolButton',
-- 'ToggleToolButton' and 'RadioToolButton' widgets.
--
-- * To create a toolbar item that contain something else than a button, use
-- 'toolItemNew'. Use 'containerAdd' to add a child widget to the tool item.
--
-- See the "Toolbar" for a description of the toolbar widget.
--
-- * Added in GTK+ 2.4
--
module ToolItem (
#if GTK_CHECK_VERSION(2,4,0)
  toolItemNew,
  toolItemSetHomogeneous,
  toolItemGetHomogeneous,
  toolItemSetExpand,
  toolItemGetExpand,
  toolItemSetTooltip,
  toolItemSetUseDragWindow,
  toolItemGetUseDragWindow,
  toolItemSetVisibleHorizontal,
  toolItemGetVisibleHorizontal,
  toolItemSetVisibleVertical,
  toolItemGetVisibleVertical,
  toolItemSetIsImportant,
  toolItemGetIsImportant,
  IconSize,
  toolItemGetIconSize,
  Orientation(..),
  toolItemGetOrientation,
  ToolbarStyle(..),
  toolItemGetToolbarStyle,
  ReliefStyle(..),
  toolItemGetReliefStyle,
  toolItemRetrieveProxyMenuItem,
  toolItemGetProxyMenuItem,
  toolItemSetProxyMenuItem
#endif
  ) where

#if GTK_CHECK_VERSION(2,4,0)

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Structs  (IconSize)
import Enums	  (Orientation(..), ToolbarStyle(..), ReliefStyle(..))

{# context lib="gtk" prefix="gtk" #}

-- | Creates a new "ToolItem".
--
toolItemNew :: IO ToolItem
toolItemNew =
  makeNewObject mkToolItem {#call unsafe tool_item_new#}

-- | Sets whether the tool item is to be allocated the same size as other
-- homogeneous items. The effect is that all homogeneous items will have the
-- same width as the widest of the items.
--
toolItemSetHomogeneous :: ToolItemClass item => item -> Bool -> IO ()
toolItemSetHomogeneous item homogeneous =
  {#call tool_item_set_homogeneous#} (toToolItem item) (fromBool homogeneous)

-- | Returns whether the tool item is the same size as other homogeneous items.
--
toolItemGetHomogeneous :: ToolItemClass item => item -> IO Bool
toolItemGetHomogeneous item = liftM toBool $
  {#call unsafe tool_item_get_homogeneous#} (toToolItem item)

-- | Sets whether the tool item is allocated extra space when there is more room
-- on the toolbar then needed for the items. The effect is that the item gets
-- bigger when the toolbar gets bigger and smaller when the toolbar gets
-- smaller.
--
toolItemSetExpand :: ToolItemClass item => item -> Bool -> IO ()
toolItemSetExpand item expand = 
  {#call tool_item_set_expand#} (toToolItem item) (fromBool expand)

-- | Returns whether the tool item is allocated extra space.
--
toolItemGetExpand :: ToolItemClass item => item -> IO Bool
toolItemGetExpand item = liftM toBool $
  {#call unsafe tool_item_get_expand#} (toToolItem item)

-- | Sets the "Tooltips" object to be used for the tool item, the text to be
-- displayed as tooltip on the item and the private text to be used. See
-- 'tooltipsSetTip'.
--
toolItemSetTooltip :: ToolItemClass item => item -> Tooltips
                   -> String  -- ^ 
                   -> String  -- ^ 
                   -> IO ()
toolItemSetTooltip item tips text private =
  withUTFString text $ \strPtr1 ->
  withUTFString private $ \strPtr2 ->
  {#call tool_item_set_tooltip#} (toToolItem item) tips strPtr1 strPtr2

-- | Sets whether toolitem has a drag window. When True the tool item can be
-- used as a drag source through 'dragSourceSet'. When the tool item has a drag
-- window it will intercept all events, even those that would otherwise be sent
-- to a child widget.
--
toolItemSetUseDragWindow :: ToolItemClass item => item -> Bool -> IO ()
toolItemSetUseDragWindow item useDragWin = 
  {#call tool_item_set_use_drag_window#} (toToolItem item) (fromBool useDragWin)

-- | Returns whether the tool item has a drag window. See
-- 'toolItemSetUseDragWindow'.
--
toolItemGetUseDragWindow :: ToolItemClass item => item -> IO Bool
toolItemGetUseDragWindow item = liftM toBool $
  {#call unsafe tool_item_get_use_drag_window#} (toToolItem item)

-- | Sets whether the tool item is visible when the toolbar is docked
-- horizontally.
--
toolItemSetVisibleHorizontal :: ToolItemClass item => item -> Bool -> IO ()
toolItemSetVisibleHorizontal item visible = 
  {#call tool_item_set_visible_horizontal#} (toToolItem item) (fromBool visible)

-- | Returns whether the tool item is visible on toolbars that are docked
-- horizontally.
--
toolItemGetVisibleHorizontal :: ToolItemClass item => item -> IO Bool
toolItemGetVisibleHorizontal item = liftM toBool $
  {#call unsafe tool_item_get_visible_horizontal#} (toToolItem item)

-- | Sets whether the tool item is visible when the toolbar is docked
-- vertically. Some tool items, such as text entries, are too wide to be useful
-- on a vertically docked toolbar. If False the tool item will not appear on
-- toolbars that are docked vertically.
--
toolItemSetVisibleVertical :: ToolItemClass item => item -> Bool -> IO ()
toolItemSetVisibleVertical item visible = 
  {#call tool_item_set_visible_vertical#} (toToolItem item) (fromBool visible)

-- | Returns whether the tool item is visible when the toolbar is docked
-- vertically.
--
toolItemGetVisibleVertical :: ToolItemClass item => item -> IO Bool
toolItemGetVisibleVertical item = liftM toBool $
  {#call unsafe tool_item_get_visible_vertical#} (toToolItem item)

-- | Sets whether the tool item should be considered important. The "ToolButton"
-- class uses this property to determine whether to show or hide its label when
-- the toolbar style is 'ToolbarBothHoriz'. The result is that only tool buttons
-- with the \"is important\" property set have labels, an effect known as
-- \"priority text\".
--
toolItemSetIsImportant :: ToolItemClass item => item -> Bool -> IO ()
toolItemSetIsImportant item important = 
  {#call tool_item_set_is_important#} (toToolItem item) (fromBool important)

-- | Returns whether the tool item is considered important.
--
toolItemGetIsImportant :: ToolItemClass item => item -> IO Bool
toolItemGetIsImportant item = liftM toBool $
  {#call unsafe tool_item_get_is_important#} (toToolItem item)

-- | Returns the icon size used for the tool item.
--
toolItemGetIconSize :: ToolItemClass item => item -> IO IconSize
toolItemGetIconSize item = liftM (toEnum.fromIntegral) $
  {#call unsafe tool_item_get_icon_size#} (toToolItem item)

-- | Returns the orientation used for the tool item.
--
toolItemGetOrientation :: ToolItemClass item => item -> IO Orientation
toolItemGetOrientation item = liftM (toEnum.fromIntegral) $
  {#call unsafe tool_item_get_orientation#} (toToolItem item)

-- | Returns the toolbar style used for the tool item.
--
toolItemGetToolbarStyle :: ToolItemClass item => item -> IO ToolbarStyle
toolItemGetToolbarStyle item = liftM (toEnum.fromIntegral) $
  {#call unsafe tool_item_get_toolbar_style#} (toToolItem item)

-- | Returns the relief style of the tool item. See 'buttonSetReliefStyle'.
--
toolItemGetReliefStyle :: ToolItemClass item => item -> IO ReliefStyle
toolItemGetReliefStyle item = liftM (toEnum.fromIntegral) $
  {#call unsafe tool_item_get_relief_style#} (toToolItem item)

-- | Returns the "MenuItem" that was last set by 'toolItemSetProxyMenuItem',
-- ie. the "MenuItem" that is going to appear in the overflow menu.
--
toolItemRetrieveProxyMenuItem :: ToolItemClass item => item -> IO (Maybe Widget)
toolItemRetrieveProxyMenuItem item = do
  wPtr <- {#call unsafe tool_item_retrieve_proxy_menu_item#} (toToolItem item)
  if wPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkWidget $ return wPtr

-- | If the menu item identifier string matches the string passed to
-- 'toolItemSetProxyMenuItem' the returns the corresponding "MenuItem".
--
toolItemGetProxyMenuItem :: ToolItemClass item => item -> String -> IO (Maybe Widget)
toolItemGetProxyMenuItem item itemId =
  withCString itemId $ \strPtr -> do
  wPtr <- {#call unsafe tool_item_get_proxy_menu_item#} (toToolItem item) strPtr
  if wPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkWidget $ return wPtr

-- | Sets the "MenuItem" used in the toolbar overflow menu. The menu item identifier
-- is used to identify the caller of this function and should also be used with
-- 'toolItemGetProxyMenuItem'.
--
toolItemSetProxyMenuItem :: (ToolItemClass item, MenuItemClass menuItem) => item
                         -> String   -- ^ Menu item identifier string
                         -> menuItem -- ^ A "MenuItem" to be used in the
                                     -- overflow menu
                         -> IO ()
toolItemSetProxyMenuItem item menuItemId menuItem = 
  withCString menuItemId $ \strPtr ->
  {#call tool_item_set_proxy_menu_item#} (toToolItem item)
    strPtr (toWidget menuItem)

#endif
