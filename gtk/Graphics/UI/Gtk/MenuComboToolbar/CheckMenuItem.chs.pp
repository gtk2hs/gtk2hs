-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget CheckMenuItem
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:25:36 $
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
-- This widget implements a 'MenuItem' with a check next to it.
--
module Graphics.UI.Gtk.MenuComboToolbar.CheckMenuItem (
  CheckMenuItem,
  CheckMenuItemClass,
  checkMenuItemNew,
  checkMenuItemNewWithLabel,
  checkMenuItemNewWithMnemonic,
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

{#context lib="gtk" prefix="gtk" #}

-- methods

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

