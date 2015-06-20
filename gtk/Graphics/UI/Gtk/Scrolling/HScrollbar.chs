{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget HScrollbar
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
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
-- A horizontal scrollbar
--
module Graphics.UI.Gtk.Scrolling.HScrollbar (
-- * Detail
--
-- | The 'HScrollbar' widget is a widget arranged horizontally creating a
-- scrollbar. See 'Scrollbar' for details on scrollbars. An 'Adjustment'
-- may be added to handle the adjustment of the scrollbar using
-- 'hScrollbarNew' or you can use 'hScrollbarNewDefaults' in
-- which case one will be created for you. See 'Adjustment' for details.
--
-- All interesting functions can be found in 'Range', from which it is derived.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Range'
-- |                     +----'Scrollbar'
-- |                           +----HScrollbar
-- @

-- * Types
  HScrollbar,
  HScrollbarClass,
  castToHScrollbar, gTypeHScrollbar,
  toHScrollbar,

-- * Constructors
  hScrollbarNew,
  hScrollbarNewDefaults
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new horizontal scrollbar.
--
hScrollbarNew ::
    Adjustment    -- ^ @adjustment@ - the 'Adjustment' to use.
 -> IO HScrollbar
hScrollbarNew adjustment =
  makeNewObject mkHScrollbar $
  liftM (castPtr :: Ptr Widget -> Ptr HScrollbar) $
  {# call unsafe hscrollbar_new #}
    adjustment

-- | Create a new HScrollbar without specifying an existing 'Adjustment'. A
-- new one will be created instead.
--
hScrollbarNewDefaults :: IO HScrollbar
hScrollbarNewDefaults = hScrollbarNew (Adjustment nullForeignPtr)
