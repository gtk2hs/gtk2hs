-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget MenuShell@
--
--  Author : Axel Simon
--          
--  Created: 21 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/24 09:43:25 $
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
-- @description@ --------------------------------------------------------------
--
-- * An abstract base class which provides the control of navigation through
--   menu items.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

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

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(MenuDirectionType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @method menuShellAppend@ Append the new entry @ref arg child@ to a menu.
--
menuShellAppend :: (MenuShellClass ms, MenuItemClass w) => ms -> w -> IO ()
menuShellAppend ms child = 
  {#call menu_shell_append#} (toMenuShell ms) (toWidget child)

-- @method menuShellPrepend@ Prepend the new entry @ref arg child@ to a menu.
--
menuShellPrepend :: (MenuShellClass ms, MenuItemClass w) => ms -> w -> IO ()
menuShellPrepend ms child = 
  {#call menu_shell_prepend#} (toMenuShell ms) (toWidget child)

-- @method menuShellInsert@ Insert the @ref arg child@ menu item at the
-- specified position (0..n-1).
--
menuShellInsert :: (MenuShellClass ms, MenuItemClass w) => ms -> w -> Int ->
                   IO ()
menuShellInsert ms child pos = {#call menu_shell_insert#} 
  (toMenuShell ms) (toWidget child) (fromIntegral pos)


-- @method menuShellDeactivate@ Temporary deactivate a complete menu
-- definition.
--
menuShellDeactivate :: MenuShellClass ms => ms -> IO ()
menuShellDeactivate ms = {#call menu_shell_deactivate#} (toMenuShell ms)

-- @method menuShellActivateItem@ Activate a specific item in the menu. If the
-- menu was deactivated and @ref arg force@ is set, the previously deactivated
-- menu is reactivated.
--
menuShellActivateItem :: (MenuShellClass ms, MenuItemClass w) => ms -> w ->
                         Bool -> IO ()
menuShellActivateItem ms child force = {#call menu_shell_activate_item#} 
  (toMenuShell ms) (toWidget child) (fromBool force)

-- @method menuShellSelectItem@ Select a specific item within the menu.
--
menuShellSelectItem :: (MenuShellClass ms, MenuItemClass w) => ms -> w -> IO ()
menuShellSelectItem ms child =
  {#call menu_shell_select_item#} (toMenuShell ms) (toWidget child)

-- @method menuShellDeselect@ Deselect a the selected item within the menu.
--
menuShellDeselect :: MenuShellClass ms => ms -> IO ()
menuShellDeselect ms =
  {#call menu_shell_deselect#} (toMenuShell ms)

-- signals

-- @signal connectToActivateCurrent@ This signal is called if an item is
-- activated. The boolean flag @ref arg hide@ is True whenever the menu will
-- behidden after this action.
--
onActivateCurrent, afterActivateCurrent :: MenuShellClass ms => ms ->
                                           (Bool -> IO ()) ->
                                           IO (ConnectId ms)
onActivateCurrent = connect_BOOL__NONE "activate-current" False
afterActivateCurrent = connect_BOOL__NONE "activate-current" True

-- @signal connectToCancel@ This signal will be emitted when a selection is
-- aborted and thus does not lead to an activation. This is in contrast to the
-- @ref arg selection@ done signal which is always emitted.
--
onCancel, afterCancel :: MenuShellClass ms => ms -> IO () -> IO (ConnectId ms)
onCancel = connect_NONE__NONE "cancel" False
afterCancel = connect_NONE__NONE "cancel" True

-- @signal connectToDeactivated@ This signal is sent whenever the menu shell
-- is deactivated (hidden).
--
onDeactivated, afterDeactivated :: MenuShellClass ms => ms -> IO () ->
                                   IO (ConnectId ms)
onDeactivated = connect_NONE__NONE "deactivate" False
afterDeactivated = connect_NONE__NONE "deactivate" True

-- @signal connectToMoveCurrent@ This signal is emitted for each move the
-- cursor makes.
--
onMoveCurrent, afterMoveCurrent :: MenuShellClass ms => ms ->
                                   (MenuDirectionType -> IO ()) ->
                                   IO (ConnectId ms)
onMoveCurrent = connect_ENUM__NONE "move-current" False
afterMoveCurrent = connect_ENUM__NONE "move-current" True

-- @signal connectToSelectionDone@ This signal is emitted when the user
-- finished using the menu. Note that this signal is emitted even if no menu
-- item was activated.
--
onSelectionDone, afterSelectionDone :: MenuShellClass ms => ms -> IO () ->
                                       IO (ConnectId ms)
onSelectionDone = connect_NONE__NONE "selection-done" False
afterSelectionDone = connect_NONE__NONE "selection-done" True
