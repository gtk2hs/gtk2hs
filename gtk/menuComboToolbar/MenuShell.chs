-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget MenuShell
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
-- * An abstract base class which provides the control of navigation through
--   menu items.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module MenuShell(
  MenuShell,
  MenuShellClass,
  castToMenuShell,
  menuShellAppend,
  menuShellPrepend,
  menuShellInsert,
  menuShellDeactivate,
  menuShellSelectItem,
  menuShellDeselect,
  connectToActivateCurrent,
  connectToCancel,
  connectToDeactivated,
  MenuDirectionType(..),
  connectToMoveCurrent,
  connectToSelectionDone
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(MenuDirectionType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Append the new entry @child to a menu. (EXPORTED)
--
menuShellAppend :: (MenuShellClass ms, MenuItemClass w) => w -> ms -> IO ()
menuShellAppend child ms = 
  {#call menu_shell_append#} (toMenuShell ms) (toWidget child)

-- Prepend the new entry @child to a menu. (EXPORTED)
--
menuShellPrepend :: (MenuShellClass ms, MenuItemClass w) => w -> ms -> IO ()
menuShellPrepend child ms = 
  {#call menu_shell_prepend#} (toMenuShell ms) (toWidget child)

-- Insert the @child menu item at the specified position (0..n-1). (EXPORTED)
--
menuShellInsert ::  (MenuShellClass ms, MenuItemClass w) => 
  w -> Int -> ms -> IO ()
menuShellInsert child pos ms = {#call menu_shell_insert#} 
  (toMenuShell ms) (toWidget child) (fromIntegral pos)


-- Temporary deactivate a complete menu definition. (EXPORTED)
--
menuShellDeactivate :: MenuShellClass ms => ms -> IO ()
menuShellDeactivate ms = {#call menu_shell_deactivate#} (toMenuShell ms)

-- Activate a specific item in the menu. If the menu was deactivated
-- and @force is set, the previously deactivated menu is reactivated. 
-- (EXPORTED)
--
menuShellActivateItem :: (MenuShellClass ms, MenuItemClass w) => 
  w -> Bool -> ms -> IO ()
menuShellActivateItem child force ms = {#call menu_shell_activate_item#} 
  (toMenuShell ms) (toWidget child) (fromBool force)

-- Select a specific item within the menu. (EXPORTED)
--
menuShellSelectItem :: (MenuShellClass ms, MenuItemClass w) => w -> ms -> IO ()
menuShellSelectItem child ms =
  {#call menu_shell_select_item#} (toMenuShell ms) (toWidget child)

-- Deselect a the selected item within the menu. (EXPORTED)
--
menuShellDeselect :: MenuShellClass ms => ms -> IO ()
menuShellDeselect ms =
  {#call menu_shell_deselect#} (toMenuShell ms)

-- signals

-- This signal is called if an item is activated. The boolean flag @hide is
-- True whenever the menu will behidden after this action. (EXPORTED)
--
connectToActivateCurrent :: MenuShellClass ms => 
  (Bool -> IO ()) -> ConnectAfter -> ms -> IO (ConnectId ms)
connectToActivateCurrent = connect_BOOL__NONE "activate-current"

-- This signal will be emitted when a selection is aborted and thus does not
-- lead to an activation. This is in contrast to the @selection done signal
-- which is always emitted. (EXPORTED)
--
connectToCancel :: MenuShellClass ms =>
  IO () -> ConnectAfter -> ms -> IO (ConnectId ms)
connectToCancel = connect_NONE__NONE "cancel"

-- This signal is sent whenever the menu shell is deactivated (hidden). 
-- (EXPORTED)
--
connectToDeactivated :: MenuShellClass ms =>
  IO () -> ConnectAfter -> ms -> IO (ConnectId ms)
connectToDeactivated = connect_NONE__NONE "deactivate"

-- This signal is emitted for each move the cursor makes. (EXPORTED)
--
connectToMoveCurrent :: MenuShellClass ms =>
  (MenuDirectionType -> IO ()) -> ConnectAfter -> ms -> IO (ConnectId ms)
connectToMoveCurrent = connect_ENUM__NONE "move-current"

-- This signal is emitted when the user finished using the menu. Note that this
-- signal is emitted even if no menu item was activated. (EXPORTED)
--
connectToSelectionDone :: MenuShellClass ms =>
  IO () -> ConnectAfter -> ms -> IO (ConnectId ms)
connectToSelectionDone = connect_NONE__NONE "selection-done"
