-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Bin
--
--  Author : Duncan Coutts
--
--  Created: 25 April 2004
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/25 01:11:31 $
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
-- This abstract widget implements a container with just one child.
--
module Graphics.UI.Gtk.Abstract.Bin (
-- * Description
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
-- |                           +----'Alignment'
-- |                           +----'Frame'
-- |                           +----'Button'
-- |                           +----'Item'
-- |                           +----'Window'
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
  castToBin,

-- * Methods
  binGetChild
  ) where

import System.Glib.FFI

import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

binGetChild :: BinClass bin => bin -> IO Widget
binGetChild bin =
  makeNewObject mkWidget $ {# call gtk_bin_get_child #} (toBin bin)
