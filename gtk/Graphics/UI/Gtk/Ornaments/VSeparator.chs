{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget VSeparator
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
-- A vertical separator
--
module Graphics.UI.Gtk.Ornaments.VSeparator (
-- * Detail
--
-- | The 'VSeparator' widget is a vertical separator, used to group the
-- widgets within a window. It displays a vertical line with a shadow to make
-- it appear sunken into the interface.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Separator'
-- |                     +----VSeparator
-- @

-- * Types
  VSeparator,
  VSeparatorClass,
  castToVSeparator, gTypeVSeparator,
  toVSeparator,

-- * Constructors
  vSeparatorNew,
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'VSeparator'.
--
vSeparatorNew :: IO VSeparator
vSeparatorNew =
  makeNewObject mkVSeparator $
  liftM (castPtr :: Ptr Widget -> Ptr VSeparator) $
  {# call unsafe vseparator_new #}
