{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget MenuBar
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
-- A subclass widget for 'MenuShell' which holds 'MenuItem' widgets
--
module Graphics.UI.Gtk.MenuComboToolbar.MenuBar (
-- * Detail
--
-- | The 'MenuBar' is a subclass of 'MenuShell' which contains one to many
-- 'MenuItem'. The result is a standard menu bar which can hold many menu
-- items.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'MenuShell'
-- |                           +----MenuBar
-- @

-- * Types
  MenuBar,
  MenuBarClass,
  castToMenuBar, gTypeMenuBar,
  toMenuBar,
#if GTK_CHECK_VERSION(2,8,0)
  PackDirection(..),
#endif

-- * Constructors
  menuBarNew,

-- * Methods
#if GTK_CHECK_VERSION(2,8,0)
  menuBarSetPackDirection,
  menuBarGetPackDirection,
  menuBarSetChildPackDirection,
  menuBarGetChildPackDirection,
#endif

-- * Attributes
#if GTK_CHECK_VERSION(2,8,0)
  menuBarPackDirection,
  menuBarChildPackDirection,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,8,0)
-- | Determines how to pack a menu bar: left-to-right, right-to-left,
-- top-to-bottom or bottom-to-top.
{# enum PackDirection {underscoreToCase} #}
#endif

--------------------
-- Constructors

-- | Creates the new 'MenuBar'
--
menuBarNew :: IO MenuBar
menuBarNew =
  makeNewObject mkMenuBar $
  liftM (castPtr :: Ptr Widget -> Ptr MenuBar) $
  {# call unsafe menu_bar_new #}

--------------------
-- Methods

#if GTK_CHECK_VERSION(2,8,0)
-- | Sets how items should be packed inside a menubar.
--
-- * Available since Gtk+ version 2.8
--
menuBarSetPackDirection :: MenuBarClass self => self
 -> PackDirection -- ^ @packDir@ - a new 'PackDirection'.
 -> IO ()
menuBarSetPackDirection self packDir =
  {# call gtk_menu_bar_set_pack_direction #}
    (toMenuBar self)
    ((fromIntegral . fromEnum) packDir)

-- | Retrieves the current pack direction of the menubar. See
-- 'menuBarSetPackDirection'.
--
-- * Available since Gtk+ version 2.8
--
menuBarGetPackDirection :: MenuBarClass self => self
 -> IO PackDirection -- ^ returns the pack direction
menuBarGetPackDirection self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_menu_bar_get_pack_direction #}
    (toMenuBar self)

-- | Sets how widgets should be packed inside the children of a menubar.
--
-- * Available since Gtk+ version 2.8
--
menuBarSetChildPackDirection :: MenuBarClass self => self
 -> PackDirection -- ^ @childPackDir@ - a new 'PackDirection'.
 -> IO ()
menuBarSetChildPackDirection self childPackDir =
  {# call gtk_menu_bar_set_child_pack_direction #}
    (toMenuBar self)
    ((fromIntegral . fromEnum) childPackDir)

-- | Retrieves the current child pack direction of the menubar. See
-- 'menuBarSetChildPackDirection'.
--
-- * Available since Gtk+ version 2.8
--
menuBarGetChildPackDirection :: MenuBarClass self => self
 -> IO PackDirection -- ^ returns the child pack direction
menuBarGetChildPackDirection self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_menu_bar_get_child_pack_direction #}
    (toMenuBar self)
#endif

--------------------
-- Attributes

#if GTK_CHECK_VERSION(2,8,0)
-- | The pack direction of the menubar. It determines how menuitems are
-- arranged in the menubar.
--
-- Default value: 'PackDirectionLtr'
--
menuBarPackDirection :: MenuBarClass self => Attr self PackDirection
menuBarPackDirection = newAttr
  menuBarGetPackDirection
  menuBarSetPackDirection

-- | The pack direction of the menubar. It determines how the widgets
-- contained in child menuitems are arranged.
--
-- Default value: 'PackDirectionLtr'
--
menuBarChildPackDirection :: MenuBarClass self => Attr self PackDirection
menuBarChildPackDirection = newAttr
  menuBarGetChildPackDirection
  menuBarSetChildPackDirection
#endif
