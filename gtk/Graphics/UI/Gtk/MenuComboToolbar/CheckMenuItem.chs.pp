-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget CheckMenuItem
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.4 $ from $Date: 2005/02/25 22:53:41 $
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
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- This widget implements a 'MenuItem' with a check next to it.
--
module Graphics.UI.Gtk.MenuComboToolbar.CheckMenuItem (
-- * Description
-- 
-- | A 'CheckMenuItem' is a menu item that maintains the state of a boolean
-- value in addition to a 'MenuItem''s usual role in activating application
-- code.
--
-- A check box indicating the state of the boolean value is displayed at the
-- left side of the 'MenuItem'. Activating the 'MenuItem' toggles the value.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Item'
-- |                                 +----'MenuItem'
-- |                                       +----CheckMenuItem
-- |                                             +----'RadioMenuItem'
-- @

-- * Types
  CheckMenuItem,
  CheckMenuItemClass,
  castToCheckMenuItem,

-- * Constructors
  checkMenuItemNew,
  checkMenuItemNewWithLabel,
  checkMenuItemNewWithMnemonic,

-- * Methods
  checkMenuItemSetActive,
  checkMenuItemGetActive,
  checkMenuItemToggled,
  checkMenuItemSetInconsistent,
  checkMenuItemGetInconsistent
#if GTK_CHECK_VERSION(2,4,0)
 ,checkMenuItemGetDrawAsRadio,
  checkMenuItemSetDrawAsRadio
#endif
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new 'MenuItem' with a check next to it.
--
checkMenuItemNew :: IO CheckMenuItem
checkMenuItemNew  = makeNewObject mkCheckMenuItem $ liftM castPtr $
  {#call unsafe check_menu_item_new#}

-- | Create a new 'CheckMenuItem' with a 'Label' inside.
--
checkMenuItemNewWithLabel :: String -> IO CheckMenuItem
checkMenuItemNewWithLabel str = withUTFString str $ \strPtr ->
  makeNewObject mkCheckMenuItem $ liftM castPtr $
  {#call unsafe check_menu_item_new_with_label#} strPtr

-- | Create a new 'CheckMenuItem' with a 'Label' inside. Underscores in the
-- label string indicate the mnemonic for the menu item.
--
checkMenuItemNewWithMnemonic :: String -> IO CheckMenuItem
checkMenuItemNewWithMnemonic str =
  withUTFString str $ \strPtr ->
  makeNewObject mkCheckMenuItem $ liftM castPtr $
  {#call unsafe check_menu_item_new_with_mnemonic#} strPtr

--------------------
-- Methods

-- | Sets the active state of the menu item's check box.
--
checkMenuItemSetActive :: CheckMenuItemClass mi => mi -> Bool -> IO ()
checkMenuItemSetActive mi active = {#call check_menu_item_set_active#}
  (toCheckMenuItem mi) (fromBool active)

-- | Returns whether the check menu item is active.
--
checkMenuItemGetActive :: CheckMenuItemClass mi => mi -> IO Bool
checkMenuItemGetActive mi =
  liftM toBool $ {#call unsafe check_menu_item_get_active#} (toCheckMenuItem mi)

-- | Emits the toggled signal.
--
checkMenuItemToggled :: CheckMenuItemClass mi => mi -> IO ()
checkMenuItemToggled mi =
  {#call check_menu_item_toggled#} (toCheckMenuItem mi)

-- | Set the state of the menu item check to \`inconsistent'.
--
checkMenuItemSetInconsistent :: CheckMenuItemClass mi => mi -> Bool -> IO ()
checkMenuItemSetInconsistent mi inconsistent = 
  {#call check_menu_item_set_inconsistent#} (toCheckMenuItem mi) 
    (fromBool inconsistent)

-- | Query if the menu check is inconsistent (inbetween).
--
checkMenuItemGetInconsistent :: CheckMenuItemClass mi => mi -> IO Bool
checkMenuItemGetInconsistent mi = liftM toBool $
  {#call unsafe check_menu_item_get_inconsistent#} (toCheckMenuItem mi)

#if GTK_CHECK_VERSION(2,4,0)
-- | Sets whether the menu item is drawn like a 'RadioMenuItem'.
--
checkMenuItemSetDrawAsRadio :: CheckMenuItemClass mi => mi -> Bool -> IO ()
checkMenuItemSetDrawAsRadio mi asRadio =
  {#call check_menu_item_set_draw_as_radio#} (toCheckMenuItem mi)
    (fromBool asRadio)

-- | Returns whether the menu item is drawn like a 'RadioMenuItem'.
--
checkMenuItemGetDrawAsRadio :: CheckMenuItemClass mi => mi -> IO Bool
checkMenuItemGetDrawAsRadio mi = liftM toBool $
  {#call unsafe check_menu_item_get_draw_as_radio#} (toCheckMenuItem mi)
#endif

