-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Table
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/12 17:19:23 $
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
-- The table widget is a container in which widgets can be aligned in cells.
--
module Graphics.UI.Gtk.Layout.Table (
  Table,
  TableClass,
  castToTable,
  tableNew,
  tableResize,
  AttachOptions(..),
  tableAttach,
  tableAttachDefaults,
  tableSetRowSpacing,
  tableGetRowSpacing,
  tableSetColSpacing,
  tableGetColSpacing,
  tableSetRowSpacings,
  tableGetDefaultRowSpacing,
  tableSetColSpacings,
  tableGetDefaultColSpacing,
  tableSetHomogeneous,
  tableGetHomogeneous
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(AttachOptions(..), fromFlags)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new table with the specified dimensions.
-- Set @homogeneous@ to True if all cells should be of the same size.
--
tableNew :: Int -> Int -> Bool -> IO Table
tableNew rows columns homogeneous = makeNewObject mkTable $ liftM castPtr $
  {#call unsafe table_new#} (fromIntegral rows) (fromIntegral columns)
  (fromBool homogeneous)

-- | Change the dimensions of an already existing table.
--
tableResize :: TableClass tb => tb -> Int -> Int -> IO ()
tableResize tb rows columns = {#call table_resize#} (toTable tb)
  (fromIntegral rows) (fromIntegral columns)

-- | Put a new widget in the table container. The widget should span the cells
-- (leftAttach,topAttach) to (rightAttach,bottomAttach). Further formatting
-- options have to be specified.
--
tableAttach :: (TableClass tb, WidgetClass w) => tb -> w -> Int -> Int ->
               Int -> Int -> [AttachOptions] -> [AttachOptions] -> Int ->
               Int -> IO ()
tableAttach tb child leftAttach rightAttach topAttach bottomAttach xoptions
            yoptions xpadding ypadding = {#call table_attach#} (toTable tb)
  (toWidget child) (fromIntegral leftAttach) (fromIntegral rightAttach) 
  (fromIntegral topAttach) (fromIntegral bottomAttach) 
  ((fromIntegral.fromFlags) xoptions) ((fromIntegral.fromFlags) yoptions) 
  (fromIntegral xpadding) (fromIntegral ypadding)

-- | Put a new widget in the table container. As opposed to 'tableAttach' this
-- function assumes default values for the packing options.
--
tableAttachDefaults :: (TableClass tb, WidgetClass w) => tb -> w -> Int ->
                       Int -> Int -> Int -> IO ()
tableAttachDefaults tb child leftAttach rightAttach topAttach bottomAttach =
  {#call table_attach_defaults#} (toTable tb) (toWidget child) 
  (fromIntegral leftAttach) (fromIntegral rightAttach) 
  (fromIntegral topAttach) (fromIntegral bottomAttach)

-- | Set the amount of space (in pixels) between the specified row and its
-- neighbours.
--
tableSetRowSpacing :: TableClass tb => tb
                   -> Int  -- ^ Row number, indexed from 0
                   -> Int  -- ^ Spacing size in pixels
                   -> IO ()
tableSetRowSpacing tb row space = {#call table_set_row_spacing#}
  (toTable tb) (fromIntegral row) (fromIntegral space)

-- | Get the amount of space (in pixels) between the specified row and the
-- next row.
--
tableGetRowSpacing :: TableClass tb => tb -> Int -> IO Int
tableGetRowSpacing tb row = liftM fromIntegral $
  {#call unsafe table_get_row_spacing#} (toTable tb) (fromIntegral row)

-- | Set the amount of space (in pixels) between the specified column and
-- its neighbours.
--
tableSetColSpacing :: TableClass tb => tb -> Int -> Int -> IO ()
tableSetColSpacing tb col space = {#call table_set_col_spacing#}
  (toTable tb) (fromIntegral col) (fromIntegral space)

-- | Get the amount of space (in pixels) between the specified column and the
-- next column.
--
tableGetColSpacing :: TableClass tb => tb -> Int -> IO Int
tableGetColSpacing tb col = liftM fromIntegral $
  {#call unsafe table_get_col_spacing#} (toTable tb) (fromIntegral col)

-- | Set the amount of space between any two rows.
--
tableSetRowSpacings :: TableClass tb => tb -> Int -> IO ()
tableSetRowSpacings tb space = {#call table_set_row_spacings#}
  (toTable tb) (fromIntegral space)

-- | Gets the default row spacing for the table. This is the spacing that will
-- be used for newly added rows.
--
tableGetDefaultRowSpacing :: TableClass tb => tb -> IO Int
tableGetDefaultRowSpacing tb = liftM fromIntegral $
  {#call unsafe table_get_default_row_spacing#} (toTable tb)

-- | Set the amount of space between any two columns.
--
tableSetColSpacings :: TableClass tb => tb -> Int -> IO ()
tableSetColSpacings tb space = {#call table_set_col_spacings#}
  (toTable tb) (fromIntegral space)

-- | Gets the default column spacing for the table. This is the spacing that
-- will be used for newly added columns.
--
tableGetDefaultColSpacing :: TableClass tb => tb -> IO Int
tableGetDefaultColSpacing tb = liftM fromIntegral $
  {#call unsafe table_get_default_col_spacing#} (toTable tb)

-- | Make all cells the same size.
--
tableSetHomogeneous :: TableClass tb => tb -> Bool -> IO ()
tableSetHomogeneous tb hom = 
  {#call table_set_homogeneous#} (toTable tb) (fromBool hom)

-- | Returns whether the table cells are all constrained to the same width and
-- height.
--
tableGetHomogeneous :: TableClass tb => tb -> IO Bool
tableGetHomogeneous tb =
  liftM toBool $ {#call unsafe table_get_homogeneous#} (toTable tb)
