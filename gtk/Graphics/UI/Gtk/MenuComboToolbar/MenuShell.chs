{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget MenuShell
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
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A base class for menu objects
--
module Graphics.UI.Gtk.MenuComboToolbar.MenuShell (
-- * Detail
--
-- | A 'MenuShell' is the abstract base class used to derive the 'Menu' and
-- 'MenuBar' subclasses.
--
-- A 'MenuShell' is a container of 'MenuItem' objects arranged in a list
-- which can be navigated, selected, and activated by the user to perform
-- application functions. A 'MenuItem' can have a submenu associated with it,
-- allowing for nested hierarchical menus.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----MenuShell
-- |                           +----'MenuBar'
-- |                           +----'Menu'
-- @

-- * Types
  MenuShell,
  MenuShellClass,
  castToMenuShell, gTypeMenuShell,
  toMenuShell,

-- * Methods
  menuShellAppend,
  menuShellPrepend,
  menuShellInsert,
  menuShellDeactivate,
  menuShellActivateItem,
  menuShellSelectItem,
  menuShellDeselect,
#if GTK_CHECK_VERSION(2,2,0)
  menuShellSelectFirst,
#endif
#if GTK_CHECK_VERSION(2,4,0)
  menuShellCancel,
#endif
#if GTK_CHECK_VERSION(2,8,0)
  menuShellSetTakeFocus,
  menuShellGetTakeFocus,
#endif

-- * Attributes
#if GTK_CHECK_VERSION(2,8,0)
  menuShellTakeFocus,
#endif

-- * Signals
  onActivateCurrent,
  afterActivateCurrent,
  onCancel,
  afterCancel,
  onDeactivated,
  afterDeactivated,
  MenuDirectionType(..),
  onMoveCurrent,
  afterMoveCurrent,
  onSelectionDone,
  afterSelectionDone
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums    (MenuDirectionType(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Adds a new 'MenuItem' to the end of the menu shell's item list.
--
menuShellAppend :: (MenuShellClass self, MenuItemClass child) => self
 -> child -- ^ @child@ - The 'MenuItem' to add.
 -> IO ()
menuShellAppend self child =
  {# call menu_shell_append #}
    (toMenuShell self)
    (toWidget child)

-- | Adds a new 'MenuItem' to the beginning of the menu shell's item list.
--
menuShellPrepend :: (MenuShellClass self, MenuItemClass child) => self
 -> child -- ^ @child@ - The 'MenuItem' to add.
 -> IO ()
menuShellPrepend self child =
  {# call menu_shell_prepend #}
    (toMenuShell self)
    (toWidget child)

-- | Adds a new 'MenuItem' to the menu shell's item list at the position
-- indicated by @position@.
--
menuShellInsert :: (MenuShellClass self, MenuItemClass child) => self
 -> child -- ^ @child@ - The 'MenuItem' to add.
 -> Int   -- ^ @position@ - The position in the item list where @child@ is
          -- added. Positions are numbered from 0 to n-1.
 -> IO ()
menuShellInsert self child position =
  {# call menu_shell_insert #}
    (toMenuShell self)
    (toWidget child)
    (fromIntegral position)

-- | Deactivates the menu shell. Typically this results in the menu shell
-- being erased from the screen.
--
menuShellDeactivate :: MenuShellClass self => self -> IO ()
menuShellDeactivate self =
  {# call menu_shell_deactivate #}
    (toMenuShell self)

-- | Activates the menu item within the menu shell. If the menu was deactivated
-- and @forceDeactivate@ is set, the previously deactivated menu is reactivated.
--
menuShellActivateItem :: (MenuShellClass self, MenuItemClass menuItem) => self
 -> menuItem -- ^ @menuItem@ - The 'MenuItem' to activate.
 -> Bool     -- ^ @forceDeactivate@ - If @True@, force the deactivation of the
             -- menu shell after the menu item is activated.
 -> IO ()
menuShellActivateItem self menuItem forceDeactivate =
  {# call menu_shell_activate_item #}
    (toMenuShell self)
    (toWidget menuItem)
    (fromBool forceDeactivate)

-- | Selects the menu item from the menu shell.
--
menuShellSelectItem :: (MenuShellClass self, MenuItemClass menuItem) => self
 -> menuItem -- ^ @menuItem@ - The 'MenuItem' to select.
 -> IO ()
menuShellSelectItem self menuItem =
  {# call menu_shell_select_item #}
    (toMenuShell self)
    (toWidget menuItem)

-- | Deselects the currently selected item from the menu shell, if any.
--
menuShellDeselect :: MenuShellClass self => self -> IO ()
menuShellDeselect self =
  {# call menu_shell_deselect #}
    (toMenuShell self)

#if GTK_CHECK_VERSION(2,2,0)
-- | Select the first visible or selectable child of the menu shell; don't
-- select tearoff items unless the only item is a tearoff item.
--
-- * Available since Gtk+ version 2.2
--
menuShellSelectFirst :: MenuShellClass self => self
 -> Bool  -- ^ @searchSensitive@ - if @True@, search for the first selectable
          -- menu item, otherwise select nothing if the first item isn't
          -- sensitive. This should be @False@ if the menu is being popped up
          -- initially.
 -> IO ()
menuShellSelectFirst self searchSensitive =
  {# call gtk_menu_shell_select_first #}
    (toMenuShell self)
    (fromBool searchSensitive)
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Cancels the selection within the menu shell.
--
-- * Available since Gtk+ version 2.4
--
menuShellCancel :: MenuShellClass self => self -> IO ()
menuShellCancel self =
  {# call gtk_menu_shell_cancel #}
    (toMenuShell self)
#endif

#if GTK_CHECK_VERSION(2,8,0)
-- | If @takeFocus@ is @True@ (the default) the menu shell will take the
-- keyboard focus so that it will receive all keyboard events which is needed
-- to enable keyboard navigation in menus.
--
-- Setting @takeFocus@ to @False@ is useful only for special applications
-- like virtual keyboard implementations which should not take keyboard focus.
--
-- The @takeFocus@ state of a menu or menu bar is automatically propagated
-- to submenus whenever a submenu is popped up, so you don't have to worry
-- about recursively setting it for your entire menu hierarchy. Only when
-- programmatically picking a submenu and popping it up manually, the
-- @takeFocus@ property of the submenu needs to be set explicitely.
--
-- Note that setting it to @False@ has side-effects:
--
-- If the focus is in some other app, it keeps the focus and keynav in the
-- menu doesn't work. Consequently, keynav on the menu will only work if the
-- focus is on some toplevel owned by the onscreen keyboard.
--
-- To avoid confusing the user, menus with @takeFocus@ set to @False@ should
-- not display mnemonics or accelerators, since it cannot be guaranteed that
-- they will work.
--
-- * Available since Gtk+ version 2.8
--
menuShellSetTakeFocus :: MenuShellClass self => self
 -> Bool  -- ^ @takeFocus@ - @True@ if the menu shell should take the keyboard
          -- focus on popup.
 -> IO ()
menuShellSetTakeFocus self takeFocus =
  {# call gtk_menu_shell_set_take_focus #}
    (toMenuShell self)
    (fromBool takeFocus)

-- | Returns @True@ if the menu shell will take the keyboard focus on popup.
--
-- * Available since Gtk+ version 2.8
--
menuShellGetTakeFocus :: MenuShellClass self => self
 -> IO Bool -- ^ returns @True@ if the menu shell will take the keyboard focus
            -- on popup.
menuShellGetTakeFocus self =
  liftM toBool $
  {# call gtk_menu_shell_get_take_focus #}
    (toMenuShell self)
#endif

--------------------
-- Attributes

#if GTK_CHECK_VERSION(2,8,0)
-- | A boolean that determines whether the menu and its submenus grab the
-- keyboard focus. See 'menuShellSetTakeFocus' and 'menuShellGetTakeFocus'.
--
-- Default value: @True@
--
menuShellTakeFocus :: MenuShellClass self => Attr self Bool
menuShellTakeFocus = newAttr
  menuShellGetTakeFocus
  menuShellSetTakeFocus
#endif

--------------------
-- Signals

-- | This signal is called if an item is
-- activated. The boolean flag @hide@ is True whenever the menu will
-- behidden after this action.
--
onActivateCurrent, afterActivateCurrent :: MenuShellClass self => self
 -> (Bool -> IO ())
 -> IO (ConnectId self)
onActivateCurrent = connect_BOOL__NONE "activate-current" False
afterActivateCurrent = connect_BOOL__NONE "activate-current" True

-- | This signal will be emitted when a selection is
-- aborted and thus does not lead to an activation. This is in contrast to the
-- @selection@ done signal which is always emitted.
--
onCancel, afterCancel :: MenuShellClass self => self
 -> IO ()
 -> IO (ConnectId self)
onCancel = connect_NONE__NONE "cancel" False
afterCancel = connect_NONE__NONE "cancel" True

-- | This signal is sent whenever the menu shell
-- is deactivated (hidden).
--
onDeactivated, afterDeactivated :: MenuShellClass self => self
 -> IO ()
 -> IO (ConnectId self)
onDeactivated = connect_NONE__NONE "deactivate" False
afterDeactivated = connect_NONE__NONE "deactivate" True

-- | This signal is emitted for each move the
-- cursor makes.
--
onMoveCurrent, afterMoveCurrent :: MenuShellClass self => self
 -> (MenuDirectionType -> IO ())
 -> IO (ConnectId self)
onMoveCurrent = connect_ENUM__NONE "move-current" False
afterMoveCurrent = connect_ENUM__NONE "move-current" True

-- | This signal is emitted when the user
-- finished using the menu. Note that this signal is emitted even if no menu
-- item was activated.
--
onSelectionDone, afterSelectionDone :: MenuShellClass self => self
 -> IO ()
 -> IO (ConnectId self)
onSelectionDone = connect_NONE__NONE "selection-done" False
afterSelectionDone = connect_NONE__NONE "selection-done" True
