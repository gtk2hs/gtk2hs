-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Menu
--
--  Author : Axel Simon
--
--  Created: 21 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/25 01:11:34 $
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
-- TODO
--
-- The following not bound functions might be useful:
--   menuSetAccelGroup, menuSetAccelGroup, menuReposition
--
-- The function menuPopup at a specific position is difficult to bind:
--   The callback function that determines at which position the menu is
--   to be shown is keept after the call returns. Maybe we could destroy
--   this function pointer with a destory event?
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A Menu is a vertically aligned set of options that can be selected. There
-- are two kinds: Those that are part of a 'MenuBar' and those 
-- that appear as a context menu (within the work space). 
--
module Graphics.UI.Gtk.MenuComboToolbar.Menu (
-- * Description
-- 
-- | A 'Menu' is a 'MenuShell' that implements a drop down menu consisting of
-- a list of 'MenuItem' objects which can be navigated and activated by the
-- user to perform application functions.
--
-- A 'Menu' is most commonly dropped down by activating a 'MenuItem' in a
-- 'MenuBar' or popped up by activating a 'MenuItem' in another 'Menu'.
--
-- A 'Menu' can also be popped up by activating a 'OptionMenu'. Other
-- composite widgets such as the 'Notebook' can pop up a 'Menu' as well.
--
-- Applications can display a 'Menu' as a popup menu by calling the
-- 'menuPopup' function.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'MenuShell'
-- |                           +----Menu
-- @

-- * Types
  Menu,
  MenuClass,
  castToMenu,

-- * Constructors
  menuNew,

-- * Methods
  menuReorderChild,
  menuPopup,
  menuSetAccelGroup,
  menuGetAccelGroup,
  menuSetAccelPath,
  menuSetTitle,
  menuGetTitle,
  menuPopdown,
  menuReposition,
  menuGetActive,
  menuSetActive,
  menuSetTearoffState,
  menuGetTearoffState,
  menuAttachToWidget,
  menuDetach,
  menuGetAttachWidget,
#if GTK_CHECK_VERSION(2,2,0)
  menuSetScreen,
#endif
#if GTK_CHECK_VERSION(2,4,0)
  menuSetMonitor,
#endif
  ) where

import Monad	(liftM)
import Maybe  (fromMaybe)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject		(makeNewGObject)
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.Gdk.Events as Events (Event(Button), time, button)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Make an empty Menu.
--
menuNew :: IO Menu
menuNew = makeNewObject mkMenu $
  liftM castPtr {#call unsafe menu_new#}

--------------------
-- Methods

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

-- | Set the "AccelGroup" which holds global accelerators for the menu. This
-- accelerator group needs to also be added to all windows that this menu is
-- being used in with 'windowAddAccelGroup', in order for those windows to
-- support all the accelerators contained in this group.
--
menuSetAccelGroup :: MenuClass m => m -> AccelGroup -> IO ()
menuSetAccelGroup m accel =
  {#call menu_set_accel_group#} (toMenu m) accel

-- | Gets the "AccelGroup" which holds global accelerators for the menu. See
-- 'menuSetAccelGroup'.
--
menuGetAccelGroup :: MenuClass m => m -> IO AccelGroup
menuGetAccelGroup m =
  makeNewGObject mkAccelGroup $
  {#call unsafe menu_get_accel_group#} (toMenu m)

-- | Sets an accelerator path for this menu from which accelerator paths for its
-- immediate children, its menu items, can be constructed. The main purpose of
-- this function is to spare the programmer the inconvenience of having to call
-- 'menuItemSetAccelPath' on each menu item that should support runtime user
-- changable accelerators. Instead, by just calling 'menuSetAccelPath' on their
-- parent, each menu item of this menu, that contains a label describing its
-- purpose, automatically gets an accel path assigned.
--
-- For example, a menu containing menu items \"New\" and \"Exit\", will, after
-- calling
--
-- > menu `menuSetAccelPath` "<Gnumeric-Sheet>/File"
--
-- assign its items the accel paths: \"<Gnumeric-Sheet>\/File\/New\" and
-- \"<Gnumeric-Sheet>\/File\/Exit\".
--
-- Assigning accel paths to menu items then enables the user to change their
-- accelerators at runtime. More details about accelerator paths and their
-- default setups can be found at 'accelMapAddEntry'.
--
menuSetAccelPath :: MenuClass m => m -> String -> IO ()
menuSetAccelPath m accelPath =
  withUTFString accelPath $ \strPtr ->
  {#call menu_set_accel_path#} (toMenu m) strPtr

-- | Set the title of the menu. It is displayed if the menu is shown as a
-- tearoff menu.
--
menuSetTitle :: MenuClass m => m -> String -> IO ()
menuSetTitle m title = withUTFString title $ \strPtr ->
  {#call unsafe menu_set_title#} (toMenu m) strPtr

-- | Returns the title of the menu, orNothing if the menu has no title set on
-- it.
--
menuGetTitle :: MenuClass m => m -> IO (Maybe String)
menuGetTitle m =
  {#call unsafe menu_get_title#} (toMenu m) >>= maybePeek peekUTFString

-- | Remove a context or tearoff menu from the screen.
--
menuPopdown :: MenuClass m => m -> IO ()
menuPopdown m = {#call menu_popdown#} (toMenu m)

-- | Repositions the menu according to its position function.
--
menuReposition :: MenuClass m => m -> IO ()
menuReposition m = {#call menu_reposition#} (toMenu m)

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

-- | Specify whether the menu is to be shown as a tearoff menu.
--
menuSetTearoffState :: MenuClass m => m -> Bool -> IO ()
menuSetTearoffState m tornOff =
  {#call menu_set_tearoff_state#} (toMenu m) (fromBool tornOff)

-- | Returns whether the menu is torn off.
--
menuGetTearoffState :: MenuClass m => m -> IO Bool
menuGetTearoffState m = liftM toBool $
  {#call unsafe menu_get_tearoff_state#} (toMenu m)

-- | Attach this menu to another widget.
--
menuAttachToWidget :: (MenuClass m, WidgetClass w) => m -> w -> IO ()
menuAttachToWidget m w =
  {#call menu_attach_to_widget#} (toMenu m) (toWidget w) nullFunPtr

-- | Detach this menu from the widget it is attached to.
--
menuDetach :: MenuClass m => m -> IO ()
menuDetach m = {#call menu_detach#} (toMenu m)

-- | Get the widget this menu is attached to. Returns Nothing if this is a
-- tearoff (context) menu.
--
menuGetAttachWidget :: MenuClass m => m -> IO (Maybe Widget)
menuGetAttachWidget m = do
  wPtr <- {#call unsafe menu_get_attach_widget#} (toMenu m)
  if wPtr==nullPtr then return Nothing else liftM Just $ 
    makeNewObject mkWidget (return wPtr)

#if GTK_CHECK_VERSION(2,2,0)
-- | Sets the "Screen" on which the menu will be displayed.
--
menuSetScreen :: MenuClass m => m -> Maybe Screen -> IO ()
menuSetScreen m screen =
  {#call menu_set_screen#} (toMenu m)
    (fromMaybe (Screen nullForeignPtr) screen)

#endif
#if GTK_CHECK_VERSION(2,4,0)
-- | Informs GTK+ on which monitor a menu should be popped up.
--
menuSetMonitor :: MenuClass m => m
               -> Int -- ^ The number of the monitor on which the menu
                      --   should be popped up
               -> IO ()
menuSetMonitor m monitorNum =
  {#call menu_set_monitor#} (toMenu m) (fromIntegral monitorNum)
#endif
