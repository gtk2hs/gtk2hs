-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Menu
--
--  Author : Axel Simon
--          
--  Created: 21 May 2001
--
--  Version $Revision: 1.7 $ from $Date: 2004/05/23 16:05:21 $
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
-- A Menu is a vertically aligned set of options that can be selected. There
-- are two kinds: Those that are part of a 'MenuBar' and those 
-- that appear as a context menu (within the work space). 
--
-- TODO
--
-- * The following not bound functions might be useful:
--   menuSetAccelGroup, menuSetAccelGroup, menuReposition
--
-- * The function menuPopup at a specific position is difficult to bind:
--   The callback function that determines at which position the menu is
--   to be shown is keept after the call returns. Maybe we could destroy
--   this function pointer with a destory event?
--
module Menu(
  Menu,
  MenuClass,
  castToMenu,
  menuNew,
  menuReorderChild,
  menuPopup,
  menuSetTitle,
  menuPopdown,
  menuGetActive,
  menuSetActive,
  menuSetTearoffState,
  menuAttachToWidget,
  menuDetach,
  menuGetAttachWidget
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Events	(Event(Button), time, button)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Make an empty Menu.
--
menuNew :: IO Menu
menuNew = makeNewObject mkMenu $
  liftM castPtr {#call unsafe menu_new#}

-- | Move a child to a new position within the menu.
--
-- * The position is counted from 0 to n-1 if the menu contains n entries.
--
menuReorderChild :: (MenuClass m, MenuItemClass mi) => m -> mi -> Int -> IO ()
menuReorderChild m child pos = {#call menu_reorder_child#}
  (toMenu m) (toWidget child) (fromIntegral pos)

-- | Popup a context menu where a button press occurred. 
--
--
menuPopup :: MenuClass m => m -> Event -> IO ()
menuPopup m (Events.Button { time=t, button=b }) = {#call menu_popup#}
  (toMenu m) (mkWidget nullForeignPtr) (mkWidget nullForeignPtr) nullFunPtr
  nullPtr ((fromIntegral.fromEnum) b) (fromIntegral t)
menuPopup _ _ = error "menuPopup: Button event expected."

-- | Set the @title@ of the menu. It is displayed
-- if the menu is shown as a tearoff menu.
--
menuSetTitle :: MenuClass m => m -> String -> IO ()
menuSetTitle m title = withUTFString title $ \strPtr ->
  {#call unsafe menu_set_title#} (toMenu m) strPtr

-- | Remove a context or tearoff menu from the screen.
--
menuPopdown :: MenuClass m => m -> IO ()
menuPopdown m = {#call menu_popdown#} (toMenu m)

-- | Return the currently selected menu item.
--
menuGetActive :: MenuClass m => m -> IO MenuItem
menuGetActive m = makeNewObject mkMenuItem $
  throwIfNull "menuGetActive: menu contains no menu items." $
  liftM castPtr $ {#call menu_get_active#} (toMenu m)

-- | Select the @n@th item of the menu.
--
menuSetActive :: MenuClass m => m -> Int -> IO ()
menuSetActive m n = {#call menu_set_active#} (toMenu m) (fromIntegral n)

-- | Specify whether the menu is to be shown as a
-- tearoff menu.
--
menuSetTearoffState :: MenuClass m => m -> Bool -> IO ()
menuSetTearoffState m tornOff =
  {#call menu_set_tearoff_state#} (toMenu m) (fromBool tornOff)

-- | Attach this menu to another widget.
--
-- * Should we support the DetachFunction?
--
menuAttachToWidget :: (MenuClass m, WidgetClass w) => m -> w -> IO ()
menuAttachToWidget m w =
  {#call menu_attach_to_widget#} (toMenu m) (toWidget w) nullFunPtr

-- | Detach this menu from the widget it is attached to.
--
menuDetach :: MenuClass m => m -> IO ()
menuDetach m = {#call menu_detach#} (toMenu m)

-- | Get the widget this menu is attached to.
-- Returns Nothing if this is a tearoff (context) menu.
--
menuGetAttachWidget :: MenuClass m => m -> IO (Maybe Widget)
menuGetAttachWidget m = do
  wPtr <- {#call unsafe menu_get_attach_widget#} (toMenu m)
  if wPtr==nullPtr then return Nothing else liftM Just $ 
    makeNewObject mkWidget (return wPtr)

