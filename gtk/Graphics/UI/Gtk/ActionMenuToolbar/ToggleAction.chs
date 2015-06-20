{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ToggleAction
--
--  Author : Duncan Coutts
--
--  Created: 6 April 2005
--
--  Copyright (C) 2005 Duncan Coutts
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
-- An action which can be toggled between two states
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.ActionMenuToolbar.ToggleAction (
-- * Detail
--
-- | A 'ToggleAction' corresponds roughly to a 'CheckMenuItem'. It has an
-- \"active\" state specifying whether the action has been checked or not.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Action'
-- |         +----ToggleAction
-- |               +----'RadioAction'
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  ToggleAction,
  ToggleActionClass,
  castToToggleAction, gTypeToggleAction,
  toToggleAction,

-- * Constructors
  toggleActionNew,

-- * Methods
  toggleActionToggled,
  toggleActionSetActive,
  toggleActionGetActive,
  toggleActionSetDrawAsRadio,
  toggleActionGetDrawAsRadio,

-- * Attributes
  toggleActionDrawAsRadio,
#if GTK_CHECK_VERSION(2,10,0)
  toggleActionActive,
#endif

-- * Signals
  actionToggled,

-- * Deprecated
#ifndef DISABLE_DEPRECATED
  onActionToggled,
  afterActionToggled,
#endif

#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.StockItems

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,4,0)
--------------------
-- Constructors

-- | Creates a new 'ToggleAction' object. To add the action to a 'ActionGroup'
-- and set the accelerator for the action, call
-- 'Graphics.UI.Gtk.ActionMenuToolbar.ActionGroup.actionGroupAddActionWithAccel'.
--
toggleActionNew :: GlibString string
 => string          -- ^ @name@ - A unique name for the action
 -> string          -- ^ @label@ - The label displayed in menu items and on
                    -- buttons
 -> Maybe string    -- ^ @tooltip@ - A tooltip for the action
 -> Maybe StockId   -- ^ @stockId@ - The stock icon to display in widgets
                    -- representing the action
 -> IO ToggleAction
toggleActionNew name label tooltip stockId =
  wrapNewGObject mkToggleAction $
  maybeWith withUTFString stockId $ \stockIdPtr ->
  maybeWith withUTFString tooltip $ \tooltipPtr ->
  withUTFString label $ \labelPtr ->
  withUTFString name $ \namePtr ->
  {# call gtk_toggle_action_new #}
    namePtr
    labelPtr
    tooltipPtr
    stockIdPtr

--------------------
-- Methods

-- | Emits the \"toggled\" signal on the toggle action.
--
toggleActionToggled :: ToggleActionClass self => self -> IO ()
toggleActionToggled self =
  {# call gtk_toggle_action_toggled #}
    (toToggleAction self)

-- | Sets the checked state on the toggle action.
--
toggleActionSetActive :: ToggleActionClass self => self
 -> Bool  -- ^ @isActive@ - whether the action should be checked or not
 -> IO ()
toggleActionSetActive self isActive =
  {# call gtk_toggle_action_set_active #}
    (toToggleAction self)
    (fromBool isActive)

-- | Returns the checked state of the toggle action.
--
toggleActionGetActive :: ToggleActionClass self => self -> IO Bool
toggleActionGetActive self =
  liftM toBool $
  {# call gtk_toggle_action_get_active #}
    (toToggleAction self)

-- | Sets whether the action should have proxies like a radio action.
--
toggleActionSetDrawAsRadio :: ToggleActionClass self => self -> Bool -> IO ()
toggleActionSetDrawAsRadio self drawAsRadio =
  {# call gtk_toggle_action_set_draw_as_radio #}
    (toToggleAction self)
    (fromBool drawAsRadio)

-- | Returns whether the action should have proxies like a radio action.
--
toggleActionGetDrawAsRadio :: ToggleActionClass self => self -> IO Bool
toggleActionGetDrawAsRadio self =
  liftM toBool $
  {# call gtk_toggle_action_get_draw_as_radio #}
    (toToggleAction self)

--------------------
-- Attributes

-- | Whether the proxies for this action look like radio action proxies.
--
-- Default value: @False@
--
toggleActionDrawAsRadio :: ToggleActionClass self => Attr self Bool
toggleActionDrawAsRadio = newAttr
  toggleActionGetDrawAsRadio
  toggleActionSetDrawAsRadio

#if GTK_CHECK_VERSION(2,10,0)
-- %hash c:cd0e d:4024
-- | If the toggle action should be active in or not.
--
-- Default value: @False@
--
-- * Available since Gtk+ version 2.10
--
toggleActionActive :: ToggleActionClass self => Attr self Bool
toggleActionActive = newAttrFromBoolProperty "active"
#endif

--------------------
-- Signals

-- %hash c:3829 d:af3f
-- |
--
actionToggled :: ToggleActionClass self => Signal self (IO ())
actionToggled = Signal (connect_NONE__NONE "toggled")

--------------------
-- Deprecated Signals

#ifndef DISABLE_DEPRECATED
-- %hash c:9cc4
onActionToggled :: ToggleActionClass self => self
 -> IO ()
 -> IO (ConnectId self)
onActionToggled = connect_NONE__NONE "toggled" False
{-# DEPRECATED onActionToggled "instead of 'onActionToggled obj' use 'on obj actionToggled'" #-}

-- %hash c:61e3
afterActionToggled :: ToggleActionClass self => self
 -> IO ()
 -> IO (ConnectId self)
afterActionToggled = connect_NONE__NONE "toggled" True
{-# DEPRECATED afterActionToggled "instead of 'afterActionToggled obj' use 'after obj actionToggled'" #-}
#endif
#endif
