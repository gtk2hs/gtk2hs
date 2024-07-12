{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widgets Switch
--
--  Author : Warlock <internalmike@gmail.com>
--
--  Created: 10 November 2017
--
--  Copyright (C) 2017 Warlock
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
-- A "light switch" style toggle
--
module Graphics.UI.Gtk.Misc.Switch (
-- * Detail
--
-- [...]
--
-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Switch'
-- @

-- * Types
#if GTK_MAJOR_VERSION >= 3
    Switch
  , castToSwitch
  , gTypeSwitch
  , toSwitch

-- * Constructors
  , switchNew

-- * Methods
  , switchSetActive
  , switchGetActive
#if GTK_CHECK_VERSION(3,14,0)
  , switchSetState
  , switchGetState
#endif

-- * Attributes
  , switchActive
#if GTK_CHECK_VERSION(3,14,0)
  , switchState
#endif

-- * Signals
  , switchActivate
#if GTK_CHECK_VERSION(3,14,0)
  , stateSet
#endif
#endif
) where

#if GTK_MAJOR_VERSION >= 3

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'Switch'.
--
switchNew :: IO Switch
switchNew =
  makeNewObject mkSwitch $
  liftM (castPtr :: Ptr Widget -> Ptr Switch) $
  {# call unsafe switch_new #}

--------------------
-- Methods

-- | Changes the state of control to the desired one.
-- See 'switchGetActive'.
switchSetActive :: SwitchClass self => self
 -> Bool
 -> IO ()
switchSetActive self is_active =
  {# call switch_set_active #}
    (toSwitch self)
    (fromBool is_active)

-- | Gets whether the GtkSwitch is in its on or off state.
switchGetActive :: SwitchClass self => self
 -> IO Bool
switchGetActive self =
  liftM toBool $
  {# call switch_get_active #}
    (toSwitch self)

#if GTK_CHECK_VERSION(3,14,0)
-- | Sets the underlying state of the GtkSwitch.
-- Normally, this is the same as 'switchActive', unless the switch is set up for
-- delayed state changes. This function is typically called
-- from a 'stateSet' signal handler.
--
-- See 'stateSet' for details.
switchSetState :: SwitchClass self => self
 -> Bool
 -> IO ()
switchSetState self state =
  {# call switch_set_state #}
    (toSwitch self)
    (fromBool state)

-- | Gets the underlying state of the GtkSwitch.
-- Set 'switchSetState'.
switchGetState :: SwitchClass self => self
 -> IO Bool
switchGetState self =
  liftM toBool $
  {# call switch_get_state #}
    (toSwitch self)
#endif

--------------------
-- Attributes

-- | Whether the switch is in its on or off state.
--
-- Default value: @False@
--
switchActive :: SwitchClass self => Attr self Bool
switchActive = newAttr
  switchGetActive
  switchSetActive

#if GTK_CHECK_VERSION(3,14,0)
-- | The backend state that is controlled by the switch. See 'stateSet' for details.
--
-- Default value: @False@
--
switchState :: SwitchClass self => Attr self Bool
switchState = newAttr
  switchGetState
  switchSetState
#endif

--------------------
-- Signals


-- | This signal on GtkSwitch is an action signal and emitting it causes
-- the switch to animate. Applications should never connect to this signal,
-- but use the notify::active signal.
--
switchActivate :: SwitchClass self => Signal self (IO ())
switchActivate = Signal (connect_NONE__NONE "activate")

#if GTK_CHECK_VERSION(3,14,0)
-- | This signal on GtkSwitch is emitted to change the underlying state.
-- It is emitted when the user changes the switch position.
-- The default handler keeps the state in sync with the 'switchActive' property.
--
-- To implement delayed state change, applications can connect to this signal,
-- initiate the change of the underlying state, and call 'switchSetState'
-- when the underlying state change is complete.
-- The signal handler should return @True@ to prevent the default handler from running.
--
-- Visually, the underlying state is represented by the through color of the switch,
-- while the 'switchActive' property is represented by the position of the switch.
--
stateSet :: SwitchClass self => Signal self (Bool -> IO Bool)
stateSet = Signal (connect_BOOL__BOOL "state-set")
#endif

#endif
