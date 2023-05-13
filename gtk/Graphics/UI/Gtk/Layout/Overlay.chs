{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GTK3 Widget Overlay
--
--  Author : Vincent Hanquez
--
--  Created: 15 May 2014
--
--  Copyright (C) 2014 Vincent Hanquez
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
-- A simple container that can display overlaid widgets
--
module Graphics.UI.Gtk.Layout.Overlay (
-- * Detail
--
-- | The Overlay widget is a Bin widget where widgets can be added as overlay of the bin widget.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----Overlay
-- @

-- * Types
  Overlay,
  OverlayClass,
  castToOverlay, gTypeOverlay,
  toOverlay,

-- * Constructors
  overlayNew,
  overlayAdd
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new 'Overlay'
--
overlayNew :: IO Overlay
overlayNew =
  makeNewObject mkOverlay $
  liftM (castPtr :: Ptr Widget -> Ptr Overlay) $
  {# call unsafe overlay_new #}

overlayAdd :: (OverlayClass self, WidgetClass widget) => self
  -> widget -- ^ @widget@ - a widget to be placed as overlay
  -> IO ()
overlayAdd self widget =
  {# call unsafe overlay_add_overlay #}
    (toOverlay self)
    (toWidget widget)
