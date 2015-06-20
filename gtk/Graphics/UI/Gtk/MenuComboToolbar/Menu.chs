{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Menu
--
--  Author : Axel Simon
--
--  Created: 21 May 2001
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
-- A menu widget
--
module Graphics.UI.Gtk.MenuComboToolbar.Menu (
-- * Detail
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
  castToMenu, gTypeMenu,
  toMenu,

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
  menuAttach,
#endif
#if GTK_CHECK_VERSION(2,6,0)
  menuGetForAttachWidget,
#endif

-- * Attributes
#if GTK_CHECK_VERSION(2,6,0)
  menuTearoffState,
#endif
  menuAccelGroup,
  menuActive,
  menuTitle,

-- * Child Attributes
  menuChildLeftAttach,
  menuChildRightAttach,
  menuChildTopAttach,
  menuChildBottomAttach,
  ) where

import Control.Monad    (liftM)
import Data.Maybe  (fromMaybe)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.Abstract.ContainerChildProperties
import Graphics.UI.Gtk.Gdk.Events (MouseButton, TimeStamp)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'Menu'.
--
menuNew :: IO Menu
menuNew =
  makeNewObject mkMenu $
  liftM (castPtr :: Ptr Widget -> Ptr Menu) $
  {# call unsafe menu_new #}

--------------------
-- Methods

-- | Moves a 'MenuItem' to a new position within the 'Menu'.
--
menuReorderChild :: (MenuClass self, MenuItemClass child) => self
 -> child -- ^ @child@ - the 'MenuItem' to move.
 -> Int   -- ^ @position@ - the new position to place @child@. Positions are
          -- numbered from 0 to n-1.
 -> IO ()
menuReorderChild self child position =
  {# call menu_reorder_child #}
    (toMenu self)
    (toWidget child)
    (fromIntegral position)

-- | Popup a context menu where a button press occurred.
--
-- * This function must be called in response to a button click. It opens
--   the given menu at a place determined by the last emitted event (hence
--   the requirement that this function is called as response to a button
--   press signal).
--
menuPopup :: MenuClass self => self -- ^ The menu to be shown.
  -> Maybe (MouseButton, TimeStamp)
  -- ^ The mouse button returned by 'Graphics.UI.Gtk.Gdk.EventM.eventButton' and
  -- the time of the event returned by 'Graphics.UI.Gtk.Gdk.eventTime'. These
  -- values are used to match the corresponding release of the button. If this
  -- context menu is shown by programmatic means, supply @Nothing@.
  -> IO ()
menuPopup self (Just (b,t)) =
  {# call menu_popup #}
    (toMenu self)
    (Widget nullForeignPtr)
    (Widget nullForeignPtr)
    nullFunPtr
    nullPtr
    ((fromIntegral . fromEnum) b)
    (fromIntegral t)
menuPopup self Nothing = do
  t <- {# call unsafe get_current_event_time #}
  {# call menu_popup #}
    (toMenu self)
    (Widget nullForeignPtr)
    (Widget nullForeignPtr)
    nullFunPtr nullPtr 0 t

-- | Set the 'AccelGroup' which holds global accelerators for the menu. This
-- accelerator group needs to also be added to all windows that this menu is
-- being used in with 'windowAddAccelGroup', in order for those windows to
-- support all the accelerators contained in this group.
--
menuSetAccelGroup :: MenuClass self => self
 -> AccelGroup -- ^ @accelGroup@ - the 'AccelGroup' to be associated with the
               -- menu.
 -> IO ()
menuSetAccelGroup self accelGroup =
  {# call menu_set_accel_group #}
    (toMenu self)
    accelGroup

-- | Gets the 'AccelGroup' which holds global accelerators for the menu. See
-- 'menuSetAccelGroup'.
--
menuGetAccelGroup :: MenuClass self => self
 -> IO AccelGroup -- ^ returns the 'AccelGroup' associated with the menu.
menuGetAccelGroup self =
  makeNewGObject mkAccelGroup $
  {# call unsafe menu_get_accel_group #}
    (toMenu self)

-- | Sets an accelerator path for this menu from which accelerator paths for
-- its immediate children, its menu items, can be constructed. The main purpose
-- of this function is to spare the programmer the inconvenience of having to
-- call 'menuItemSetAccelPath' on each menu item that should support runtime
-- user changable accelerators. Instead, by just calling 'menuSetAccelPath' on
-- their parent, each menu item of this menu, that contains a label describing
-- its purpose, automatically gets an accel path assigned.
--
-- For example, a menu containing menu items \"New\" and \"Exit\", will, after
-- calling
--
-- > menu `menuSetAccelPath` "<Gnumeric-Sheet>/File"
--
-- assign its items the accel paths: @\"\<Gnumeric-Sheet\>\/File\/New\"@ and
-- @\"\<Gnumeric-Sheet\>\/File\/Exit\"@.
--
-- Assigning accel paths to menu items then enables the user to change their
-- accelerators at runtime. More details about accelerator paths and their
-- default setups can be found at 'accelMapAddEntry'.
--
menuSetAccelPath :: (MenuClass self, GlibString string) => self
 -> string -- ^ @accelPath@ - a valid accelerator path
 -> IO ()
menuSetAccelPath self accelPath =
  withUTFString accelPath $ \accelPathPtr ->
  {# call menu_set_accel_path #}
    (toMenu self)
    accelPathPtr

-- | Sets the title string for the menu. The title is displayed when the menu
-- is shown as a tearoff menu.
--
menuSetTitle :: (MenuClass self, GlibString string) => self -> string -> IO ()
menuSetTitle self title =
  withUTFString title $ \titlePtr ->
  {# call unsafe menu_set_title #}
    (toMenu self)
    titlePtr

-- | Returns the title of the menu. See 'menuSetTitle'.
--
menuGetTitle :: (MenuClass self, GlibString string) => self
 -> IO (Maybe string) -- ^ returns the title of the menu, or @Nothing@ if the
                      -- menu has no title set on it.
menuGetTitle self =
  {# call unsafe menu_get_title #}
    (toMenu self)
  >>= maybePeek peekUTFString

-- | Removes the menu from the screen.
--
menuPopdown :: MenuClass self => self -> IO ()
menuPopdown self =
  {# call menu_popdown #}
    (toMenu self)

-- | Repositions the menu according to its position function.
--
menuReposition :: MenuClass self => self -> IO ()
menuReposition self =
  {# call menu_reposition #}
    (toMenu self)

-- | Returns the selected menu item from the menu. This is used by the
-- 'OptionMenu'.
--
menuGetActive :: MenuClass self => self
 -> IO MenuItem -- ^ returns the 'MenuItem' that was last selected in the menu.
                -- If a selection has not yet been made, the first menu item is
                -- selected.
menuGetActive self =
  makeNewObject mkMenuItem $
  throwIfNull "menuGetActive: menu contains no menu items." $
  liftM castPtr $
  {# call menu_get_active #}
    (toMenu self)

-- | Selects the specified menu item within the menu. This is used by the
-- 'OptionMenu' and should not be used by anyone else.
--
menuSetActive :: MenuClass self => self
 -> Int   -- ^ @index@ - the index of the menu item to select. Index values
          -- are from 0 to n-1.
 -> IO ()
menuSetActive self index =
  {# call menu_set_active #}
    (toMenu self)
    (fromIntegral index)

-- | Changes the tearoff state of the menu. A menu is normally displayed as
-- drop down menu which persists as long as the menu is active. It can also be
-- displayed as a tearoff menu which persists until it is closed or reattached.
--
menuSetTearoffState :: MenuClass self => self
 -> Bool  -- ^ @tornOff@ - If @True@, menu is displayed as a tearoff menu.
 -> IO ()
menuSetTearoffState self tornOff =
  {# call menu_set_tearoff_state #}
    (toMenu self)
    (fromBool tornOff)

-- | Returns whether the menu is torn off. See 'menuSetTearoffState'.
--
menuGetTearoffState :: MenuClass self => self
 -> IO Bool -- ^ returns @True@ if the menu is currently torn off.
menuGetTearoffState self =
  liftM toBool $
  {# call unsafe menu_get_tearoff_state #}
    (toMenu self)

-- | Attach this menu to another widget.
--
menuAttachToWidget :: (MenuClass self, WidgetClass attachWidget) => self -> attachWidget -> IO ()
menuAttachToWidget self attachWidget =
  {# call menu_attach_to_widget #}
    (toMenu self)
    (toWidget attachWidget)
    nullFunPtr

-- | Detach this menu from the widget it is attached to.
--
menuDetach :: MenuClass self => self -> IO ()
menuDetach self =
  {# call menu_detach #}
    (toMenu self)

-- | Get the widget this menu is attached to. Returns Nothing if this is a
-- tearoff (context) menu.
--
menuGetAttachWidget :: MenuClass self => self -> IO (Maybe Widget)
menuGetAttachWidget self = do
  wPtr <- {#call unsafe menu_get_attach_widget#} (toMenu self)
  if wPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkWidget (return wPtr)

#if GTK_CHECK_VERSION(2,2,0)
-- | Sets the 'Screen' on which the menu will be displayed.
--
-- * Available since Gtk+ version 2.2
--
menuSetScreen :: MenuClass self => self
 -> Maybe Screen -- ^ @screen@ - a 'Screen', or @Nothing@ if the screen should
                 -- be determined by the widget the menu is attached to.
 -> IO ()
menuSetScreen self screen =
  {# call menu_set_screen #}
    (toMenu self)
    (fromMaybe (Screen nullForeignPtr) screen)
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Informs Gtk+ on which monitor a menu should be popped up. See
-- 'screenGetMonitorGeometry'.
--
-- * Available since Gtk+ version 2.4
--
menuSetMonitor :: MenuClass self => self
 -> Int   -- ^ @monitorNum@ - the number of the monitor on which the menu
          -- should be popped up
 -> IO ()
menuSetMonitor self monitorNum =
  {# call menu_set_monitor #}
    (toMenu self)
    (fromIntegral monitorNum)

-- | Adds a new 'MenuItem' to a (table) menu. The number of \'cells\' that an
-- item will occupy is specified by @leftAttach@, @rightAttach@, @topAttach@
-- and @bottomAttach@. These each represent the leftmost, rightmost, uppermost
-- and lower column and row numbers of the table. (Columns and rows are indexed
-- from zero).
--
-- Note that this function is not related to 'menuDetach'.
--
-- * Available since Gtk+ version 2.4
--
menuAttach :: (MenuClass self, MenuItemClass child) => self
 -> child -- ^ @child@ - a 'MenuItem'.
 -> Int   -- ^ @leftAttach@ - The column number to attach the left side of the
          -- item to.
 -> Int   -- ^ @rightAttach@ - The column number to attach the right side of
          -- the item to.
 -> Int   -- ^ @topAttach@ - The row number to attach the top of the item to.
 -> Int   -- ^ @bottomAttach@ - The row number to attach the bottom of the
          -- item to.
 -> IO ()
menuAttach self child leftAttach rightAttach topAttach bottomAttach =
  {# call gtk_menu_attach #}
    (toMenu self)
    (toWidget child)
    (fromIntegral leftAttach)
    (fromIntegral rightAttach)
    (fromIntegral topAttach)
    (fromIntegral bottomAttach)
#endif

#if GTK_CHECK_VERSION(2,6,0)
-- | Returns a list of the menus which are attached to this widget.
--
-- * Available since Gtk+ version 2.6
--
menuGetForAttachWidget :: WidgetClass widget =>
    widget                  -- ^ @widget@ - a 'Widget'
 -> IO [Menu]
menuGetForAttachWidget widget =
  {# call gtk_menu_get_for_attach_widget #}
    (toWidget widget)
  >>= fromGList
  >>= mapM (\elemPtr -> makeNewObject mkMenu (return elemPtr))
#endif

--------------------
-- Attributes

-- | A title that may be displayed by the window manager when this menu is
-- torn-off.
--
-- Default value: \"\"
--
menuTitle :: (MenuClass self, GlibString string) => Attr self string
menuTitle = newAttrFromStringProperty "tearoff-title"

#if GTK_CHECK_VERSION(2,6,0)
-- | A boolean that indicates whether the menu is torn-off.
--
-- Default value: @False@
--
menuTearoffState :: MenuClass self => Attr self Bool
menuTearoffState = newAttr
  menuGetTearoffState
  menuSetTearoffState
#endif

-- | \'accelGroup\' property. See 'menuGetAccelGroup' and 'menuSetAccelGroup'
--
menuAccelGroup :: MenuClass self => Attr self AccelGroup
menuAccelGroup = newAttr
  menuGetAccelGroup
  menuSetAccelGroup

-- | \'active\' property. See 'menuGetActive' and 'menuSetActive'
--
menuActive :: MenuClass self => ReadWriteAttr self MenuItem Int
menuActive = newAttr
  menuGetActive
  menuSetActive

--------------------
-- Child Attributes

-- | The column number to attach the left side of the child to.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
menuChildLeftAttach :: (MenuClass self, WidgetClass child) => child -> Attr self Int
menuChildLeftAttach = newAttrFromContainerChildIntProperty "left-attach"

-- | The column number to attach the right side of the child to.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
menuChildRightAttach :: (MenuClass self, WidgetClass child) => child -> Attr self Int
menuChildRightAttach = newAttrFromContainerChildIntProperty "right-attach"

-- | The row number to attach the top of the child to.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
menuChildTopAttach :: (MenuClass self, WidgetClass child) => child -> Attr self Int
menuChildTopAttach = newAttrFromContainerChildIntProperty "top-attach"

-- | The row number to attach the bottom of the child to.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
menuChildBottomAttach :: (MenuClass self, WidgetClass child) => child -> Attr self Int
menuChildBottomAttach = newAttrFromContainerChildIntProperty "bottom-attach"
