{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget VScrollbar
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
-- A vertical scrollbar
--
module Graphics.UI.Gtk.Scrolling.VScrollbar (
-- * Detail
--
-- | The 'VScrollbar' widget is a widget arranged vertically creating a
-- scrollbar. See 'Scrollbar' for details on scrollbars. An 'Adjustment'
-- may be added to handle the adjustment of the scrollbar using
-- 'vScrollbarNew' or you can use 'vScrollbarNewDefaults' in
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
-- |                           +----VScrollbar
-- @

-- * Types
  VScrollbar,
  VScrollbarClass,
  castToVScrollbar, gTypeVScrollbar,
  toVScrollbar,

-- * Constructors
  vScrollbarNew,
  vScrollbarNewDefaults
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new vertical scrollbar.
--
vScrollbarNew ::
    Adjustment    -- ^ @adjustment@ - the 'Adjustment' to use.
 -> IO VScrollbar
vScrollbarNew adjustment =
  makeNewObject mkVScrollbar $
  liftM (castPtr :: Ptr Widget -> Ptr VScrollbar) $
  {# call unsafe vscrollbar_new #}
    adjustment

-- | Create a new HScrollbar without specifying an existing 'Adjustment'. A
-- new one will be created instead.
--
vScrollbarNewDefaults :: IO VScrollbar
vScrollbarNewDefaults = vScrollbarNew (Adjustment nullForeignPtr)
