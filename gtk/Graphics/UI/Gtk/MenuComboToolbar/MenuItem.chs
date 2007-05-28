-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget MenuItem
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- This widget derives from 'Item'. Since CList and CTree are deprecated, it
--   is the only child of that widget. The three signals defined by Item are
--   therefore bound in this module.
--
-- TODO
--
-- figure out what the signals \"toggle-size-allocate\" and 
--   \"toggle-size-request\" are good for and bind them if useful
--
-- figure out if the connectToToggle signal is useful at all
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- The widget used for item in menus
--
module Graphics.UI.Gtk.MenuComboToolbar.MenuItem (
-- * Detail
-- 
-- | The 'MenuItem' widget and the derived widgets are the only valid childs
-- for menus. Their function is to correctly handle highlighting, alignment,
-- events and submenus.
--
-- As it derives from 'Bin' it can hold any valid child widget, altough only
-- a few are really useful.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Item'
-- |                                 +----MenuItem
-- |                                       +----'CheckMenuItem'
-- |                                       +----'ImageMenuItem'
-- |                                       +----'SeparatorMenuItem'
-- |                                       +----'TearoffMenuItem'
-- @

-- * Types
  MenuItem,
  MenuItemClass,
  castToMenuItem,
  toMenuItem,

-- * Constructors
  menuItemNew,
  menuItemNewWithLabel,
  menuItemNewWithMnemonic,

-- * Methods
  menuItemSetSubmenu,
  menuItemGetSubmenu,
  menuItemRemoveSubmenu,
  menuItemSelect,
  menuItemDeselect,
  menuItemActivate,
  menuItemSetRightJustified,
  menuItemGetRightJustified,
  menuItemSetAccelPath,

-- * Attributes
  menuItemSubmenu,
  menuItemRightJustified,

-- * Signals
  onActivateItem,
  afterActivateItem,
  onActivateLeaf,
  afterActivateLeaf,
  onSelect,
  afterSelect,
  onDeselect,
  afterDeselect,
  onToggle,
  afterToggle
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'MenuItem'.
--
menuItemNew :: IO MenuItem
menuItemNew =
  makeNewObject mkMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr MenuItem) $
  {# call unsafe menu_item_new #}

-- | Creates a new 'MenuItem' whose child is a 'Label'.
--
menuItemNewWithLabel :: 
    String      -- ^ @label@ - the text for the label
 -> IO MenuItem
menuItemNewWithLabel label =
  makeNewObject mkMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr MenuItem) $
  withUTFString label $ \labelPtr ->
  {# call unsafe menu_item_new_with_label #}
    labelPtr

-- | Creates a new 'MenuItem' containing a label. The label will be created
-- using 'labelNewWithMnemonic', so underscores in @label@ indicate the
-- mnemonic for the menu item.
--
menuItemNewWithMnemonic :: 
    String      -- ^ @label@ - The text of the label, with an underscore in
                -- front of the mnemonic character
 -> IO MenuItem
menuItemNewWithMnemonic label =
  makeNewObject mkMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr MenuItem) $
  withUTFString label $ \labelPtr ->
  {# call unsafe menu_item_new_with_mnemonic #}
    labelPtr

--------------------
-- Methods

-- | Sets the item's submenu, or changes it.
--
menuItemSetSubmenu :: (MenuItemClass self, MenuClass submenu) => self -> submenu -> IO ()
menuItemSetSubmenu self submenu =
  {# call menu_item_set_submenu #}
    (toMenuItem self)
    (toWidget submenu)

-- | Gets the submenu underneath this menu item, if any. See
-- 'menuItemSetSubmenu'.
--
menuItemGetSubmenu :: MenuItemClass self => self
 -> IO (Maybe Widget) -- ^ returns submenu for this menu item, or @Nothing@ if
                      -- none.
menuItemGetSubmenu self =
  maybeNull (makeNewObject mkWidget) $
  {# call unsafe menu_item_get_submenu #}
    (toMenuItem self)

-- | Removes the item's submenu.
--
menuItemRemoveSubmenu :: MenuItemClass self => self -> IO ()
menuItemRemoveSubmenu self =
  {# call menu_item_remove_submenu #}
    (toMenuItem self)

-- | Select the menu item. Emits the \"select\" signal on the item.
--
menuItemSelect :: MenuItemClass self => self -> IO ()
menuItemSelect self =
  {# call menu_item_select #}
    (toMenuItem self)

-- | Deselect the menu item. Emits the \"deselect\" signal on the item.
--
menuItemDeselect :: MenuItemClass self => self -> IO ()
menuItemDeselect self =
  {# call menu_item_deselect #}
    (toMenuItem self)

-- | Simulate a click on the menu item. Emits the \"activate\" signal on the item.
--
menuItemActivate :: MenuItemClass self => self -> IO ()
menuItemActivate self =
  {# call menu_item_activate #}
    (toMenuItem self)

-- | Sets whether the menu item appears justified at the right side of a menu
-- bar. This was traditionally done for \"Help\" menu items, but is now
-- considered a bad idea. (If the widget layout is reversed for a right-to-left
-- language like Hebrew or Arabic, right-justified-menu-items appear at the
-- left.)
--
menuItemSetRightJustified :: MenuItemClass self => self
 -> Bool  -- ^ @rightJustified@ - if @True@ the menu item will appear at the
          -- far right if added to a menu bar.
 -> IO ()
menuItemSetRightJustified self rightJustified =
  {# call menu_item_set_right_justified #}
    (toMenuItem self)
    (fromBool rightJustified)

-- | Gets whether the menu item appears justified at the right side of the
-- menu bar.
--
menuItemGetRightJustified :: MenuItemClass self => self -> IO Bool
menuItemGetRightJustified self =
  liftM toBool $
  {# call unsafe menu_item_get_right_justified #}
    (toMenuItem self)

-- | Set the accelerator path on the menu item, through which runtime changes of
-- the menu item's accelerator caused by the user can be identified and saved
-- to persistant storage (see 'accelMapSave' on this). To setup a default
-- accelerator for this menu item, call 'accelMapAddEntry' with the same accel
-- path. See also 'accelMapAddEntry' on the specifics of accelerator paths, and
-- 'menuSetAccelPath' for a more convenient variant of this function.
--
-- This function is basically a convenience wrapper that handles calling
-- 'widgetSetAccelPath' with the appropriate accelerator group for the menu
-- item.
--
-- Note that you do need to set an accelerator on the parent menu with
-- 'menuSetAccelGroup' for this to work.
--
menuItemSetAccelPath :: MenuItemClass self => self
 -> Maybe String -- ^ @accelPath@ - accelerator path, corresponding to this
                 -- menu item's functionality, or @Nothing@ to unset the
                 -- current path.
 -> IO ()
menuItemSetAccelPath self accelPath =
  maybeWith withUTFString accelPath $ \accelPathPtr ->
  {# call menu_item_set_accel_path #}
    (toMenuItem self)
    accelPathPtr

--------------------
-- Attributes

-- | \'submenu\' property. See 'menuItemGetSubmenu' and 'menuItemSetSubmenu'
--
menuItemSubmenu :: (MenuItemClass self, MenuClass submenu) => ReadWriteAttr self (Maybe Widget) submenu
menuItemSubmenu = newAttr
  menuItemGetSubmenu
  menuItemSetSubmenu

-- | \'rightJustified\' property. See 'menuItemGetRightJustified' and
-- 'menuItemSetRightJustified'
--
menuItemRightJustified :: MenuItemClass self => Attr self Bool
menuItemRightJustified = newAttr
  menuItemGetRightJustified
  menuItemSetRightJustified

--------------------
-- Signals

-- | The user has chosen the menu item.
--
-- * This is the only function applications normally connect to.
--   It is not emitted if the item has a submenu.
--
onActivateLeaf, afterActivateLeaf :: MenuItemClass self => self
 -> IO ()
 -> IO (ConnectId self)
onActivateLeaf = connect_NONE__NONE "activate" False
afterActivateLeaf = connect_NONE__NONE "activate" True

-- | Emitted when the user chooses a menu item that has a submenu.
--
-- * This signal is not emitted if the menu item does not have a
--   submenu.
--
onActivateItem, afterActivateItem :: MenuItemClass self => self
 -> IO ()
 -> IO (ConnectId self)
onActivateItem = connect_NONE__NONE "activate-item" False
afterActivateItem = connect_NONE__NONE "activate-item" True

-- | This signal is emitted when the item is selected.
--
onSelect, afterSelect :: ItemClass i => i
 -> IO ()
 -> IO (ConnectId i)
onSelect = connect_NONE__NONE "select" False
afterSelect = connect_NONE__NONE "select" True

-- | This signal is emitted when the item is deselected.
--
onDeselect, afterDeselect :: ItemClass i => i
 -> IO ()
 -> IO (ConnectId i)
onDeselect = connect_NONE__NONE "deselect" False
afterDeselect = connect_NONE__NONE "deselect" True

-- | This signal is emitted when the item is toggled.
--
onToggle, afterToggle :: ItemClass i => i
 -> IO ()
 -> IO (ConnectId i)
onToggle = connect_NONE__NONE "toggled" False
afterToggle = connect_NONE__NONE "toggled" True
