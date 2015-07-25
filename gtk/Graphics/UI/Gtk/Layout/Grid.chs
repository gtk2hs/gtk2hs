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
  castToGrid,
  gTypeGrid,
  toGrid,

-- * Constructors
  gridNew,

-- * Methods
  gridAttach,
  gridAttachNextTo,
  gridSetRowHomogeneous,
  gridGetRowHomogeneous,
  gridSetRowSpacing,
  gridGetRowSpacing,
  gridSetColumnHomogeneous,
  gridGetColumnHomogeneous,
  gridSetColumnSpacing,
  gridGetColumnSpacing,

#if GTK_CHECK_VERSION(3,2,0)
  gridGetChildAt,
  gridInsertRow,
  gridInsertColumn,
  gridInsertNextTo,
#endif

#if GTK_CHECK_VERSION(3,10,0)
  gridRemoveRow,
  gridRemoveColumn,
  gridGetBaselineRow,
  gridSetBaselineRow,
  gridGetRowBaselinePosition,
  gridSetRowBaselinePosition
#endif
 
 ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Flags                (fromFlags)
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.General.Enums    (AttachOptions(..), PositionType, BaselinePosition)

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

-- | Sets whether all rows of grid will have the same height.
--
gridSetRowHomogeneous :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Bool -- ^ @homogeneous@ - True to make row homogeneous.
 -> IO ()
gridSetRowHomogeneous self homogeneous =
 {# call grid_set_row_homogeneous #}
    (toGrid self)
    (fromBool homogeneous)

-- | Returns whether all rows of grid have the same height.
--
gridGetRowHomogeneous :: GridClass self
 => self -- ^ @self@ - the grid.
 -> IO Bool -- ^ returns whether all rows of grid have same height.
gridGetRowHomogeneous self =
 liftM toBool $
 {# call grid_get_row_homogeneous #}
    (toGrid self)

-- | Sets the amount of space between rows of grid.
--
gridSetRowSpacing :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @spacing@ - the amount of space to insert between rows. 
 -> IO ()
gridSetRowSpacing self spacing =
 {# call grid_set_row_spacing #}
    (toGrid self)
    (fromIntegral spacing)

-- | Returns the amount of space between the rows of grid.
--
gridGetRowSpacing :: GridClass self
 => self -- ^ @self@ - the grid.
 -> IO Int -- ^ returns the spacing of grid.
gridGetRowSpacing self =
 liftM fromIntegral $
 {# call grid_get_row_spacing #}
    (toGrid self)

-- | Sets whether all columns of grid will have the same width.
--
gridSetColumnHomogeneous :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Bool -- ^ @homogeneous@ - True to make columns homogeneous.
 -> IO ()
gridSetColumnHomogeneous self homogeneous =
 {# call grid_set_row_homogeneous #}
    (toGrid self)
    (fromBool homogeneous)

-- | Returns whether all columns of grid have the same width.
--
gridGetColumnHomogeneous :: GridClass self
 => self -- ^ @self@ - the grid.
 -> IO Bool -- ^ returns whether all columns of grid have the same width.
gridGetColumnHomogeneous self =
 liftM toBool $
 {# call grid_get_column_homogeneous #}
    (toGrid self)

-- | Sets the amount of space between columns of grid.
--
gridSetColumnSpacing :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @spacing@ - the amount of space to insert between columns.
 -> IO ()
gridSetColumnSpacing self spacing =
 {# call grid_set_column_spacing #}
    (toGrid self)
    (fromIntegral spacing)

-- | Returns the  amount of space between the columns of grid.
--
gridGetColumnSpacing :: GridClass self
 => self -- ^ @self@ - the grid.
 -> IO Int -- ^ returns the spacing of grid. 
gridGetColumnSpacing self =
 liftM fromIntegral $
 {# call grid_get_column_spacing #}
    (toGrid self)

#if GTK_CHECK_VERSION(3,2,0)

-- | Gets the child of grid whose area covers the grid cell whose upper left corner is at 
-- left , top . 
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

-- | Inserts a row at the specified position. Children which are attached at or below this 
-- position are moved one row down. Children which span across this position are grown to 
-- span the new row.
--
gridInsertRow :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @pos@ - the position to insert the row at.
 -> IO ()
gridInsertRow self pos =
 {# call grid_insert_row #}
    (toGrid self)
    (fromIntegral pos)

-- | Inserts a column at the specified position. Children which are attached at or to the
-- right of this position are moved one column to the right. Children which span across 
-- this position are grown to span the new column
--
gridInsertColumn :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @pos@ - the positiion to insert the column at.
 -> IO ()
gridInsertColumn self pos =
 {# call grid_insert_column #}
    (toGrid self)
    (fromIntegral pos)

-- | Inserts a row or column at the specified position. The new row or column is placed
-- next to sibling , on the side determined by side. If side is GTK_POS_TOP or 
-- GTK_POS_BOTTOM, a row is inserted. If side is GTK_POS_LEFT of GTK_POS_RIGHT, a 
-- column is inserted.
--
gridInsertNextTo :: (GridClass self, WidgetClass sibling)
 => self -- ^ @self@ - the grid.
 -> sibling -- ^ @sib@ - the child of grid that the new row or column will be placed next to.
 -> PositionType -- ^ @pos@ - the isde of the sibling that child is positioned next to.
 -> IO ()
gridInsertNextTo self sib pos =
 {# call grid_insert_next_to #}
    (toGrid self)
    (toWidget sib)
    (fromIntegral $ fromEnum pos)

#endif

#if GTK_CHECK_VERSION(3,10,0)

-- | Removes a row from the grid. Children that are placed in this row are removed,
-- spanning children that overlap this row have their height reduced by one, and children
-- below the row are moved up.
--
gridRemoveRow :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @pos@ - the position of the row to remove.
 -> IO ()
gridRemoveRow self pos =
 {# call grid_remove_row #}
    (toGrid self)
    (fromIntegral pos)

-- | Removes a column from the grid. Children that are placed in this column are removed,
-- spanning children that overlap this column have their width reduced by one, and
-- children after the column are moved to the left.
--
gridRemoveColumn :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @pos@ -the position of the column to remove.
 -> IO ()
gridRemoveColumn self pos =
 {# call grid_remove_column #}
    (toGrid self)
    (fromIntegral pos)

-- | Returns which row defines the global baseline of grid.
--
gridGetBaselineRow :: GridClass self
 => self -- ^ @self@ - the grid.
 -> IO Int -- ^ returns the row index defining the global baseline. 
gridGetBaselineRow self =
 liftM fromIntegral $
 {# call grid_get_baseline_row #}
    (toGrid self)

-- | Sets which row defines the global baseline for the entire grid. Each row in 
-- the grid can have its own local baseline, but only one of those is global,
-- meaning it will be the baseline in the parent of the grid.
--
gridSetBaselineRow :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @row@ - the row index.
 -> IO ()
gridSetBaselineRow self row =
 {# call grid_set_baseline_row #}
    (toGrid self)
    (fromIntegral row)

-- | Returns the baseline position of row as set by gridSetRowBaselinePosition 
-- or the default value BASELINE_POSITION_CENTER
--
gridGetRowBaselinePosition :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int -- ^ @row@ - a row index.
 -> IO BaselinePosition -- ^ returns  the baseline position of row.
gridGetRowBaselinePosition self row =
 liftM (toEnum . fromIntegral) $
 {# call grid_get_row_baseline_position #}
    (toGrid self)
    (fromIntegral row)

-- | Sets how the baseline should be positioned on row of the grid, in case that row 
-- is assigned more space than is requested.
--
gridSetRowBaselinePosition :: GridClass self
 => self -- ^ @self@ - the grid.
 -> Int  -- ^ @row@ - a row index.
 -> BaselinePosition -- ^ @pos@ - a BaselinePosition.
 -> IO ()
gridSetRowBaselinePosition self row pos =
 {# call grid_set_row_baseline_position #}
    (toGrid self)
    (fromIntegral row)
    (fromIntegral $ fromEnum pos)

#endif
