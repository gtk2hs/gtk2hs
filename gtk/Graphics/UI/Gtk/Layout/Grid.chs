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
 => self -- ^ @self@ - the grid.
 -> child -- ^ @child@ - the widget to add.
 -> Int -- ^ @left@ - the column number of to attach the left side of child to.
 -> Int -- ^ @top@ - the row number to attach the top side of child to.
 -> Int -- ^ @width@ - the number of columns that child will span.
 -> Int -- ^ @height@ - the number of rows that child will span.
 -> IO ()
gridAttach self child left top width height =
 {# call grid_attach #}
    (toGrid self)
    (toWidget child)
    (fromIntegral left)
    (fromIntegral top)
    (fromIntegral width)
    (fromIntegral height)

-- | Adds a widget to the grid. The widget is placed next to sibling , on the side 
-- determined by side . When sibling is Nothing, the widget is placed in row (for 
-- left or right placement) or column 0 (for top or bottom placement), at the end 
-- indicated by side.
--
-- Attaching widgets labeled [1], [2], [3] with sibling == Nothing and side == GTK_POS_LEFT 
-- yields a layout of 3[1].
-- 
gridAttachNextTo :: (GridClass self, WidgetClass child, WidgetClass sibling)
 => self -- ^ @self@ - the grid.
 -> child -- ^ @child@ - the widget to add
 -> Maybe sibling -- ^ @sib@ - the child of grid that child will be placed next to.
 -> PositionType -- ^ @pos@ - the side of the sibling that child is positioned next to.
 -> Int -- ^ @width@ - the number of columns that child will span.
 -> Int -- ^ @height@ - the number of rows that child will span.
 -> IO()
gridAttachNextTo self child sib pos width height =
 {# call grid_attach_next_to #}
    (toGrid self)
    (toWidget child)
    (maybe (Widget nullForeignPtr) toWidget sib)
    (fromIntegral $ fromEnum pos)
    (fromIntegral width)
    (fromIntegral height)

-- | Gets the child of grid whose area covers the grid cell whose upper left corner is at left , top . 
--
gridGetChildAt :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @left@ - the left edge of the cell.
 -> Int -- ^ @top@ - the top edge of the cell.
 -> IO (Maybe Widget) -- ^ returns the child at the given position or Nothing.
gridGetChildAt self left top = do
 ptr <- {# call grid_get_child_at #}
           (toGrid self)
           (fromIntegral left)
           (fromIntegral top)
 if ptr == nullPtr 
    then return Nothing 
    else liftM Just $ makeNewObject mkWidget (return ptr)

-- |
--
gridInsertRow :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @pos@ -
 -> IO ()
gridInsertRow self pos =
 {# call grid_insert_row #}
    (toGrid self)
    (fromIntegral pos)

-- |
--
gridInsertColumn :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @pos@ -
 -> IO ()
gridInsertColumn self pos =
 {# call grid_insert_column #}
    (toGrid self)
    (fromIntegral pos)

-- |
--
gridRemoveRow :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @pos@ - 
 -> IO ()
gridRemoveRow self pos =
 {# call grid_remove_row #}
    (toGrid self)
    (fromIntegral pos)

-- |
--
gridRemoveColumn :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @pos@ -
 -> IO ()
gridRemoveColumn self pos =
 {# call grid_remove_column #}
    (toGrid self)
    (fromIntegral pos)

-- |
--
gridInsertNextTo :: (GridClass self, WidgetClass sibling)
 => self -- ^ @self@ - the grid.
 -> sibling -- ^ @sib@ -
 -> PositionType -- ^ @pos@ -
 -> IO ()
gridInsertNextTo self sib pos =
 {# call grid_insert_next_to #}
    (toGrid self)
    (toWidget sib)
    (fromIntegral $ fromEnum pos)

-- |
--
gridSetRowHomogeneous :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Bool -- ^ @homogeneous@ - 
 -> IO ()
gridSetRowHomogeneous self homogeneous =
 {# call grid_set_row_homogeneous #}
    (toGrid self)
    (fromBool homogeneous)

-- |
--
gridGetRowHomogeneous :: GridClass self
 => self -- ^ @self@ - the grid.
 -> IO Bool -- ^ @
gridGetRowHomogeneous self =
 liftM toBool $
 {# call grid_get_row_homogeneous #}
    (toGrid self)

-- |
--
gridSetRowSpacing :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @spacing@ - 
 -> IO ()
gridSetRowSpacing self spacing =
 {# call grid_set_row_spacing #}
    (toGrid self)
    (fromIntegral spacing)

-- |
--
gridGetRowSpacing :: GridClass self
 => self -- ^ @self@ - the grid.
 -> IO Int -- ^ @
gridGetRowSpacing self =
 liftM fromIntegral $
 {# call grid_get_row_spacing #}
    (toGrid self)

gridSetColumnHomogeneous :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Bool -- ^ @homogeneous@ -
 -> IO ()
gridSetColumnHomogeneous self homogeneous =
 {# call grid_set_row_homogeneous #}
    (toGrid self)
    (fromBool homogeneous)

-- |
--
gridGetColumnHomogeneous :: GridClass self
 => self -- ^ @self@ - the grid.
 -> IO Bool -- ^ @
gridGetColumnHomogeneous self =
 liftM toBool $
 {# call grid_get_column_homogeneous #}
    (toGrid self)

-- |
--
gridSetColumnSpacing :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @spacing@ -
 -> IO ()
gridSetColumnSpacing self spacing =
 {# call grid_set_column_spacing #}
    (toGrid self)
    (fromIntegral spacing)

-- |
--
gridGetColumnSpacing :: GridClass self
 => self -- ^ @self@ - the grid.
 -> IO Int -- ^ @
gridGetColumnSpacing self =
 liftM fromIntegral $
 {# call grid_get_column_spacing #}
    (toGrid self)

-- |
--
gridGetBaselineRow :: GridClass self
 => self -- ^ @self@ - the grid.
 -> IO Int -- ^ @
gridGetBaselineRow self =
 liftM fromIntegral $
 {# call grid_get_baseline_row #}
    (toGrid self)

-- |
--
gridSetBaselineRow :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @row@ -
 -> IO ()
gridSetBaselineRow self row =
 {# call grid_set_baseline_row #}
    (toGrid self)
    (fromIntegral row)

-- |
--
gridGetRowBaselinePosition :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @row@ -
 -> IO BaselinePosition 
gridGetRowBaselinePosition self row =
 liftM (toEnum . fromIntegral) $
 {# call grid_get_row_baseline_position #}
    (toGrid self)
    (fromIntegral row)

-- |
--
gridSetRowBaselinePosition :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int  -- ^ @row@ -
 -> BaselinePosition -- ^ @pos@ -
 -> IO ()
gridSetRowBaselinePosition self row pos =
 {# call grid_set_row_baseline_position #}
    (toGrid self)
    (fromIntegral row)
    (fromIntegral $ fromEnum pos)

