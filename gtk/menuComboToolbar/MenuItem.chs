-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget MenuItem
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.5 $ from $Date: 2004/05/23 16:05:21 $
--
--  Copyright (c) 1999..2002 Axel Simon
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
-- This widget represents a singe menu item.
--
-- * The widget derives from Item. Since CList and CTree are deprecated, it
--   is the only child of that widget. The three signals defined by Item are
--   therefore bound in this module.
--
-- TODO
--
-- * figure out what the signals \"toggle-size-allocate\" and 
--   \"toggle-size-request\" are good for and bind them if useful
--
-- * figure out if the connectToToggle signal is useful at all
--
module MenuItem(
  MenuItem,
  MenuItemClass,
  castToMenuItem,
  menuItemNew,
  menuItemNewWithLabel,
  menuItemSetSubmenu,
  menuItemRemoveSubmenu,
--  menuItemConfigure,
  menuItemSelect,
  menuItemDeselect,
  menuItemActivate,
  menuItemSetRightJustified,
  onActivateLeaf,
  afterActivateLeaf,
  onActivateItem,
  afterActivateItem,
  onSelect,
  afterSelect,
  onDeselect,
  afterDeselect,
  onToggle,
  afterToggle
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new menu item. This is the smallest part
-- of a menu that the user can click on or select with the keyboard.
--
menuItemNew :: IO MenuItem
menuItemNew  = makeNewObject mkMenuItem $ 
  liftM castPtr {#call unsafe menu_item_new#}

-- | Create a new menu item and place a label
-- inside.
--
menuItemNewWithLabel :: String -> IO MenuItem
menuItemNewWithLabel label = withUTFString label $ \strPtr -> 
  makeNewObject mkMenuItem $ liftM castPtr $
  {#call unsafe menu_item_new_with_label#} strPtr

-- | Set the item's submenu.
--
menuItemSetSubmenu :: (MenuItemClass mi, MenuClass m) => mi -> m -> IO ()
menuItemSetSubmenu mi submenu = 
  {#call menu_item_set_submenu#} (toMenuItem mi) (toWidget submenu)

-- | Remove the item's submenu.
--
menuItemRemoveSubmenu :: MenuItemClass mi => mi -> IO ()
menuItemRemoveSubmenu mi = {#call menu_item_remove_submenu#} (toMenuItem mi)


-- | Should the item display a submenu 
-- indicator (an arrow) if there is a
-- submenu?
--menuItemConfigure :: MenuItemClass mi => Bool -> mi -> IO ()
--menuItemConfigure subInd mi =
--  {#call unsafe menu_item_configure#} (toMenuItem mi) 0 (fromBool subInd)


-- | Select the menu item.
--
menuItemSelect :: MenuItemClass mi => mi -> IO ()
menuItemSelect mi = {#call menu_item_select#} (toMenuItem mi)

-- | Deselect the menu item.
--
menuItemDeselect :: MenuItemClass mi => mi -> IO ()
menuItemDeselect mi = {#call menu_item_deselect#} (toMenuItem mi)

-- | Simulate a click on the menu item.
--
menuItemActivate :: MenuItemClass mi => mi -> IO ()
menuItemActivate mi = {#call menu_item_activate#} (toMenuItem mi)

-- | Make the menu item right justified. Only
-- useful for menu bars.
--
menuItemSetRightJustified :: MenuItemClass mi => Bool -> mi -> IO ()
menuItemSetRightJustified yes mi = 
  {#call menu_item_set_right_justified#} (toMenuItem mi) (fromBool yes)

-- signals

-- | The user has chosen the menu item and the
-- item does not contain a submenu.
--
onActivateLeaf, afterActivateLeaf :: MenuItemClass mi => mi -> IO () ->
                                     IO (ConnectId mi)
onActivateLeaf = connect_NONE__NONE "activate" False
afterActivateLeaf = connect_NONE__NONE "activate" True

-- | Emitted when the user chooses this item even
-- if it has submenus.
--
onActivateItem, afterActivateItem :: MenuItemClass mi => mi -> IO () ->
                                     IO (ConnectId mi)
onActivateItem = connect_NONE__NONE "activate-item" False
afterActivateItem = connect_NONE__NONE "activate-item" True

-- | This signal is emitted when the item is selected.
--
onSelect, afterSelect :: ItemClass i => i -> IO () -> IO (ConnectId i)
onSelect = connect_NONE__NONE "select" False
afterSelect = connect_NONE__NONE "select" True

-- | This signal is emitted when the item is
-- deselected.
--
onDeselect, afterDeselect :: ItemClass i => i -> IO () -> IO (ConnectId i)
onDeselect = connect_NONE__NONE "deselect" False
afterDeselect = connect_NONE__NONE "deselect" True

-- | This signal is emitted when the item is toggled.
--
onToggle, afterToggle :: ItemClass i => i -> IO () -> IO (ConnectId i)
onToggle = connect_NONE__NONE "toggled" False
afterToggle = connect_NONE__NONE "toggled" True

