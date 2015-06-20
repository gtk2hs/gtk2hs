{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget VPaned
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
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
-- A container with two panes arranged vertically
--
module Graphics.UI.Gtk.Layout.VPaned (
-- * Detail
--
-- | The VPaned widget is a container widget with two children arranged
-- vertically. The division between the two panes is adjustable by the user by
-- dragging a handle. See 'Paned' for details.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Paned'
-- |                           +----VPaned
-- @

-- * Types
  VPaned,
  VPanedClass,
  castToVPaned, gTypeVPaned,
  toVPaned,

-- * Constructors
  vPanedNew,
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new 'VPaned'
--
vPanedNew :: IO VPaned
vPanedNew =
  makeNewObject mkVPaned $
  liftM (castPtr :: Ptr Widget -> Ptr VPaned) $
  {# call unsafe vpaned_new #}
