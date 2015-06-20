{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ToggleToolButton
--
--  Author : Duncan Coutts
--
--  Created: 7 April 2005
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
-- A 'ToolItem' containing a toggle button
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.MenuComboToolbar.ToggleToolButton (
-- * Detail
--
-- | A 'ToggleToolButton' is a 'ToolItem' that contains a toggle button.
--
-- Use 'toggleToolButtonNew' to create a new 'ToggleToolButton'. Use
-- 'toggleToolButtonNewFromStock' to create a new 'ToggleToolButton' containing
-- a stock item.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'ToolItem'
-- |                                 +----'ToolButton'
-- |                                       +----ToggleToolButton
-- |                                             +----'RadioToolButton'
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  ToggleToolButton,
  ToggleToolButtonClass,
  castToToggleToolButton, gTypeToggleToolButton,
  toToggleToolButton,

-- * Constructors
  toggleToolButtonNew,
  toggleToolButtonNewFromStock,

-- * Methods
  toggleToolButtonSetActive,
  toggleToolButtonGetActive,

-- * Attributes
#if GTK_CHECK_VERSION(2,8,0)
  toggleToolButtonActive,
#endif

-- * Signals
  onToolButtonToggled,
  afterToolButtonToggled,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.StockItems

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,4,0)
--------------------
-- Constructors

-- | Returns a new 'ToggleToolButton'
--
toggleToolButtonNew :: IO ToggleToolButton
toggleToolButtonNew =
  makeNewObject mkToggleToolButton $
  liftM (castPtr :: Ptr ToolItem -> Ptr ToggleToolButton) $
  {# call gtk_toggle_tool_button_new #}

-- | Creates a new 'ToggleToolButton' containing the image and text from a
-- stock item.
--
-- It is an error if @stockId@ is not a name of a stock item.
--
toggleToolButtonNewFromStock ::
    StockId             -- ^ @stockId@ - the name of the stock item
 -> IO ToggleToolButton
toggleToolButtonNewFromStock stockId =
  makeNewObject mkToggleToolButton $
  liftM (castPtr :: Ptr ToolItem -> Ptr ToggleToolButton) $
  withUTFString stockId $ \stockIdPtr ->
  {# call gtk_toggle_tool_button_new_from_stock #}
    stockIdPtr

--------------------
-- Methods

-- | Sets the status of the toggle tool button. Set to @True@ if you want the
-- 'ToggleButton' to be \'pressed in\', and @False@ to raise it. This action
-- causes the toggled signal to be emitted.
--
toggleToolButtonSetActive :: ToggleToolButtonClass self => self -> Bool -> IO ()
toggleToolButtonSetActive self isActive =
  {# call gtk_toggle_tool_button_set_active #}
    (toToggleToolButton self)
    (fromBool isActive)

-- | Queries a 'ToggleToolButton' and returns its current state. Returns
-- @True@ if the toggle button is pressed in and @False@ if it is raised.
--
toggleToolButtonGetActive :: ToggleToolButtonClass self => self -> IO Bool
toggleToolButtonGetActive self =
  liftM toBool $
  {# call gtk_toggle_tool_button_get_active #}
    (toToggleToolButton self)

--------------------
-- Attributes

#if GTK_CHECK_VERSION(2,8,0)
-- | If the toggle tool button should be pressed in or not.
--
-- Default value: @False@
--
toggleToolButtonActive :: ToggleToolButtonClass self => Attr self Bool
toggleToolButtonActive = newAttr
  toggleToolButtonGetActive
  toggleToolButtonSetActive
#endif

--------------------
-- Signals

-- | Emitted whenever the toggle tool button changes state.
--
onToolButtonToggled, afterToolButtonToggled :: ToggleToolButtonClass self => self
 -> IO ()
 -> IO (ConnectId self)
onToolButtonToggled = connect_NONE__NONE "toggled" False
afterToolButtonToggled = connect_NONE__NONE "toggled" True
#endif
