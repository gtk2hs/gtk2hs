{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Bin
--
--  Author : Duncan Coutts
--
--  Created: 25 April 2004
--
--  Copyright (C) 2004-2005 Duncan Coutts
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
-- A container with just one child
--
module Graphics.UI.Gtk.Abstract.Bin (
-- * Detail
--
-- | The 'Bin' widget is a container with just one child. It is not very
-- useful itself, but it is useful for deriving subclasses, since it provides
-- common code needed for handling a single child widget.
--
-- Many Gtk+ widgets are subclasses of 'Bin', including 'Window', 'Button',
-- 'Frame', 'HandleBox', and 'ScrolledWindow'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----Bin
-- |                           +----'Window'
-- |                           +----'Alignment'
-- |                           +----'Frame'
-- |                           +----'Button'
-- |                           +----'Item'
-- |                           +----'ComboBox'
-- |                           +----'EventBox'
-- |                           +----'Expander'
-- |                           +----'HandleBox'
-- |                           +----'ToolItem'
-- |                           +----'ScrolledWindow'
-- |                           +----'Viewport'
-- @

-- * Types
  Bin,
  BinClass,
  castToBin, gTypeBin,
  toBin,

-- * Methods
  binGetChild,
  ) where

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Gets the child of the 'Bin', or @Nothing@ if the bin contains no child
-- widget.
--
binGetChild :: BinClass self => self
 -> IO (Maybe Widget) -- ^ returns pointer to child of the 'Bin'
binGetChild self =
  maybeNull (makeNewObject mkWidget) $
  {# call gtk_bin_get_child #}
    (toBin self)
