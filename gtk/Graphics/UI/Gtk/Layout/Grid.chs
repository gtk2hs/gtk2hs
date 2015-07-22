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
import Graphics.UI.Gtk.General.Enums    (AttachOptions(..), PositionType, BaselinePosition)
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

gridAttachNextTo :: (GridClass self, WidgetClass child, WidgetClass sibling)
 => self
 -> child
 -> sibling
 -> PositionType
 -> Int
 -> Int
 -> IO()
gridAttachNextTo self child sib pos width height =
 {# call grid_attach_next_to #}
    (toGrid self)
    (toWidget child)
    (toWidget sib)
    (fromIntegral $ fromEnum pos)
    (fromIntegral width)
    (fromIntegral height)

gridGetChildAt :: GridClass self
 => self
 -> Int
 -> Int
 -> IO Widget
gridGetChildAt self left top =
 makeNewObject mkWidget $
 {# call grid_get_child_at #}
    (toGrid self)
    (fromIntegral left)
    (fromIntegral top)

gridInsertRow :: GridClass self
 => self
 -> Int
 -> IO ()
gridInsertRow self pos =
 {# call grid_insert_row #}
    (toGrid self)
    (fromIntegral pos)

gridInsertColumn :: GridClass self
 => self
 -> Int
 -> IO ()
gridInsertColumn self pos =
 {# call grid_insert_column #}
    (toGrid self)
    (fromIntegral pos)

gridRemoveRow :: GridClass self
 => self
 -> Int
 -> IO ()
gridRemoveRow self pos =
 {# call grid_remove_row #}
    (toGrid self)
    (fromIntegral pos)

gridRemoveColumn :: GridClass self
 => self
 -> Int
 -> IO ()
gridRemoveColumn self pos =
 {# call grid_remove_column #}
    (toGrid self)
    (fromIntegral pos)

gridInsertNextTo :: (GridClass self, WidgetClass sibling)
 => self
 -> sibling
 -> PositionType
 -> IO ()
gridInsertNextTo self sib pos =
 {# call grid_insert_next_to #}
    (toGrid self)
    (toWidget sib)
    (fromIntegral $ fromEnum pos)

gridSetRowHomogeneous :: GridClass self
 => self
 -> Bool
 -> IO ()
gridSetRowHomogeneous self homogeneous =
 {# call grid_set_row_homogeneous #}
    (toGrid self)
    (fromBool homogeneous)

gridGetRowHomogeneous :: GridClass self
 => self
 -> IO Bool
gridGetRowHomogeneous self =
 liftM toBool $
 {# call grid_get_row_homogeneous #}
    (toGrid self)

gridSetRowSpacing :: GridClass self
 => self
 -> Int
 -> IO ()
gridSetRowSpacing self spacing =
 {# call grid_set_row_spacing #}
    (toGrid self)
    (fromIntegral spacing)

gridGetRowSpacing :: GridClass self
 => self
 -> IO Int
gridGetRowSpacing self =
 liftM fromIntegral $
 {# call grid_get_row_spacing #}
    (toGrid self)

gridSetColumnHomogeneous :: GridClass self
 => self
 -> Bool
 -> IO ()
gridSetColumnHomogeneous self homogeneous =
 {# call grid_set_row_homogeneous #}
    (toGrid self)
    (fromBool homogeneous)

gridGetColumnHomogeneous :: GridClass self
 => self
 -> IO Bool
gridGetColumnHomogeneous self =
 liftM toBool $
 {# call grid_get_column_homogeneous #}
    (toGrid self)

gridSetColumnSpacing :: GridClass self
 => self
 -> Int
 -> IO ()
gridSetColumnSpacing self spacing =
 {# call grid_set_column_spacing #}
    (toGrid self)
    (fromIntegral spacing)

gridGetColumnSpacing :: GridClass self
 => self
 -> IO Int
gridGetColumnSpacing self =
 liftM fromIntegral $
 {# call grid_get_column_spacing #}
    (toGrid self)

gridGetBaselineRow :: GridClass self
 => self
 -> IO Int
gridGetBaselineRow self =
 liftM fromIntegral $
 {# call grid_get_baseline_row #}
    (toGrid self)

gridSetBaselineRow :: GridClass self
 => self
 -> Int
 -> IO ()
gridSetBaselineRow self row =
 {# call grid_set_baseline_row #}
    (toGrid self)
    (fromIntegral row)

gridGetRowBaselinePosition :: GridClass self
 => self
 -> Int
 -> IO BaselinePosition 
gridGetRowBaselinePosition self row =
 liftM (toEnum . fromIntegral) $
 {# call grid_get_row_baseline_position #}
    (toGrid self)
    (fromIntegral row)

gridSetRowBaselinePosition :: GridClass self
 => self
 -> Int 
 -> BaselinePosition
 -> IO ()
gridSetRowBaselinePosition self row pos =
 {# call grid_set_row_baseline_position #}
    (toGrid self)
    (fromIntegral row)
    (fromIntegral $ fromEnum pos)

