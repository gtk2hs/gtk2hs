{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Alignment
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
-- A widget which controls the alignment and size of its child
--
module Graphics.UI.Gtk.Layout.Grid (
-- * Detail
--
-- | 'Grid' packs widgets into rows and columns.
--
-- * Class Hierarchy
-- |
-- @
-- |   'GObject'
-- |    +----'Object'
-- |          +----'Widget'
-- |                +----'Container'
-- |                      +----Grid
-- @

-- * Types
  Grid,
  GridClass,
  castToGrid, gTypeGrid,
  toGrid,

-- * Constructors
  gridNew,

-- * Methods
  gridAttach
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Flags                (fromFlags)
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.General.Enums    (AttachOptions(..))
import Graphics.UI.Gtk.Abstract.ContainerChildProperties

{# context lib="gtk" prefix="gtk" #}

---------------------
-- Constructors

-- | Creates a new grid widget.
--
gridNew :: IO Grid
gridNew = 
    makeNewObject mkGrid $
    liftM (castPtr :: Ptr Widget -> Ptr Grid) $
    {# call unsafe grid_new #}

---------------------
-- Methods

-- | Adds a widget to the grid. The position of child is determined by left and top.
-- the number of "cells" that child will occupy is determined by width and height.
--
gridAttach :: (GridClass self, WidgetClass child) 
 => self
 -> child
 -> Int
 -> Int
 -> Int
 -> Int
 -> IO ()
gridAttach self child left top width height =
 {# call grid_attach #}
   (toGrid self)
   (toWidget child)
   (fromIntegral left)
   (fromIntegral top)
   (fromIntegral width)
   (fromIntegral height)
