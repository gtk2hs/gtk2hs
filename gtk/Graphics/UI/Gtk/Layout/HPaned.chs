{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget HPaned
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
-- A container with two panes arranged horizontally
--
module Graphics.UI.Gtk.Layout.HPaned (
-- * Detail
--
-- | The HPaned widget is a container widget with two children arranged
-- horizontally. The division between the two panes is adjustable by the user
-- by dragging a handle. See 'Paned' for details.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Paned'
-- |                           +----HPaned
-- @

-- * Types
  HPaned,
  HPanedClass,
  castToHPaned, gTypeHPaned,
  toHPaned,

-- * Constructors
  hPanedNew,
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new 'HPaned'
--
hPanedNew :: IO HPaned
hPanedNew =
  makeNewObject mkHPaned $
  liftM (castPtr :: Ptr Widget -> Ptr HPaned) $
  {# call unsafe hpaned_new #}
