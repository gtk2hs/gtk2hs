-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget MenuItem
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
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
-- * This widget represents a singe menu item.
--
--- DOCU ----------------------------------------------------------------------
--
-- * The widget derives from Item. Since CList and CTree are deprecated, it
--   is the only child of that widget. The three signals defined by Item are
--   therefore bound in this module.
--
--- TODO ----------------------------------------------------------------------
--
-- * figure out what the signals "toggle-size-allocate" and 
--   "toggle-size-request" are good for and bind them if useful
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
  connectToActivateLeaf,
  connectToActivateItem,
  connectToSelect,
  connectToDeselect,
  connectToToggle
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a new menu item. This is the smallest part of a menu that the
-- user can click on or select with the keyboard. (EXPORTED)
--
menuItemNew :: IO MenuItem
menuItemNew = makeNewObject mkMenuItem $ 
  liftM castPtr {#call unsafe menu_item_new#}

-- Create a new menu item and place a label inside. (EXPORTED)
--
menuItemNewWithLabel :: String -> IO MenuItem
menuItemNewWithLabel label = withCString label $ \strPtr -> 
  makeNewObject mkMenuItem $ liftM castPtr $
  {#call unsafe menu_item_new_with_label#} strPtr

-- Set the item's submenu. (EXPORTED)
--
menuItemSetSubmenu :: (MenuItemClass mi, MenuClass m) => m -> mi -> IO ()
menuItemSetSubmenu submenu mi = 
  {#call menu_item_set_submenu#} (toMenuItem mi) (toWidget submenu)

-- Remove the item's submenu. (EXPORTED)
--
menuItemRemoveSubmenu :: MenuItemClass mi => mi -> IO ()
menuItemRemoveSubmenu mi = {#call menu_item_remove_submenu#} (toMenuItem mi)


-- Should the item display a submenu indicator (an arrow) if there is a
-- submenu? (EXPORTED)
--
--menuItemConfigure :: MenuItemClass mi => Bool -> mi -> IO ()
--menuItemConfigure subInd mi =
--  {#call unsafe menu_item_configure#} (toMenuItem mi) 0 (fromBool subInd)

-- Select the menu item. (EXPORTED)
--
menuItemSelect :: MenuItemClass mi => mi -> IO ()
menuItemSelect mi = {#call menu_item_select#} (toMenuItem mi)

-- Deselect the menu item. (EXPORTED)
--
menuItemDeselect :: MenuItemClass mi => mi -> IO ()
menuItemDeselect mi = {#call menu_item_deselect#} (toMenuItem mi)

-- Simulate a click on the menu item. (EXPORTED)
--
menuItemActivate :: MenuItemClass mi => mi -> IO ()
menuItemActivate mi = {#call menu_item_activate#} (toMenuItem mi)

-- Make the menu item right justified. Only useful for menu bars. (EXPORTED)
--
menuItemSetRightJustified :: MenuItemClass mi => mi -> Bool -> IO ()
menuItemSetRightJustified mi yes = 
  {#call menu_item_set_right_justified#} (toMenuItem mi) (fromBool yes)

-- signals

-- The user has chosen the menu item and the item does not contain a submenu. 
-- (EXPORTED)
--
connectToActivateLeaf :: MenuItemClass mi => 
  IO () -> ConnectAfter -> mi -> IO (ConnectId mi)
connectToActivateLeaf = connect_NONE__NONE "activate"

-- Emitted when the user chooses this item even if it has submenus. (EXPORTED)
--
connectToActivateItem :: MenuItemClass mi =>
  IO () -> ConnectAfter -> mi -> IO (ConnectId mi)
connectToActivateItem = connect_NONE__NONE "activate-item"

-- This signal is emitted when the item is selected. (EXPORTED)
--
connectToSelect :: ItemClass i =>
  IO () -> ConnectAfter -> i -> IO (ConnectId i)
connectToSelect = connect_NONE__NONE "select"

-- This signal is emitted when the item is deselected. (EXPORTED)
--
connectToDeselect :: ItemClass i =>
  IO () -> ConnectAfter -> i -> IO (ConnectId i)
connectToDeselect = connect_NONE__NONE "deselect"

-- This signal is emitted when the item is toggled. (EXPORTED)
--
connectToToggle :: ItemClass i =>
  IO () -> ConnectAfter -> i -> IO (ConnectId i)
connectToToggle = connect_NONE__NONE "toggled"

