-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Menu
--
--  Author : Axel Simon
--          
--  Created: 21 May 2001
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
-- * A Menu is a vertically aligned set of options that can be selected. There
--   are two kinds: Those that are part of a @MenuBar and those that appear
--   as a context menu (within the work space). 
--
--- DOCU ----------------------------------------------------------------------
--
--- TODO ----------------------------------------------------------------------
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
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Events	(Event(..))
import Structs	(nullForeignPtr)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Make an empty Menu. (EXPORTED)
--
menuNew :: IO Menu
menuNew = makeNewObject mkMenu $
  liftM castPtr {#call unsafe menu_new#}

-- Move a child to a new position within the menu. The position is counted
-- from 0 to n-1 if the menu contains n entries. (EXPORTED)
--
menuReorderChild :: (MenuClass m, MenuItemClass mi) => mi -> Int -> m -> IO ()
menuReorderChild child pos m = {#call menu_reorder_child#}
  (toMenu m) (toWidget child) (fromIntegral pos)

-- Popup a context menu where a button press occurred. -- (EXPORTED)
--
menuPopup :: MenuClass m => Event -> m -> IO ()
menuPopup (Button { time=t, button=b }) m = {#call menu_popup#} (toMenu m) 
  (mkWidget nullForeignPtr) (mkWidget nullForeignPtr) nullFunPtr nullPtr
  ((fromIntegral.fromEnum) b) (fromIntegral t)
menuPopup _ _ = error "menuPopup: Button event expected."

-- Set the @title of the menu. It is displayed if the menu is shown as a
-- tearoff menu. (EXPORTED)
--
menuSetTitle :: MenuClass m => String -> m -> IO ()
menuSetTitle title m = withCString title $ \strPtr ->
  {#call unsafe menu_set_title#} (toMenu m) strPtr

-- Remove a context or tearoff menu from the screen. (EXPORTED)
--
menuPopdown :: MenuClass m => m -> IO ()
menuPopdown m = {#call menu_popdown#} (toMenu m)

-- Return the currently selected menu item. (EXPORTED)
--
menuGetActive :: MenuClass m => m -> IO MenuItem
menuGetActive m = makeNewObject mkMenuItem $
  throwIfNull "menuGetActive: menu contains no menu items." $
  liftM castPtr $ {#call menu_get_active#} (toMenu m)

-- Select the @n th item of the menu. (EXPORTED)
--
menuSetActive :: MenuClass m => Int -> m -> IO ()
menuSetActive n m = {#call menu_set_active#} (toMenu m) (fromIntegral n)

-- Specify whether the menu is to be shown as a tearoff menu. (EXPORTED)
--
menuSetTearoffState :: MenuClass m => Bool -> m -> IO ()
menuSetTearoffState tornOff m =
  {#call menu_set_tearoff_state#} (toMenu m) (fromBool tornOff)

-- Attach this menu to another widget. (EXPORTED)
--
-- * Should we support the DetachFunction?
--
menuAttachToWidget :: (MenuClass m, WidgetClass w) => w -> m -> IO ()
menuAttachToWidget w m =
  {#call menu_attach_to_widget#} (toMenu m) (toWidget w) nullFunPtr

-- Detach this menu from the widget it is attached to. (EXPORTED)
--
menuDetach :: MenuClass m => m -> IO ()
menuDetach m = {#call menu_detach#} (toMenu m)

-- Get the widget this menu is attached to. Returns Nothing if this
-- is a tearoff (context) menu. (EXPORTED)
menuGetAttachWidget :: MenuClass m => m -> IO (Maybe Widget)
menuGetAttachWidget m = do
  wPtr <- {#call unsafe menu_get_attach_widget#} (toMenu m)
  if wPtr==nullPtr then return Nothing else liftM Just $ 
    makeNewObject mkWidget (return wPtr)

