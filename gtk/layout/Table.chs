-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Table
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
-- * The table widget is a container in which widgets can be aligned in cells.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module Table(
  Table,
  TableClass,
  castToTable,
  tableNew,
  tableResize,
  AttachOptions(..),
  tableAttach,
  tableAttachDefaults,
  tableSetRowSpacing,
  tableSetColSpacing,
  tableSetRowSpacings,
  tableSetColSpacings,
  tableSetHomogeneous
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(AttachOptions(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a new table with the specified dimensions. Set @homogeneous to
-- True if all cells should be of the same size. (EXPORTED)
--
tableNew :: Int -> Int -> Bool -> IO Table
tableNew rows columns homogeneous = makeNewObject mkTable $ liftM castPtr $
  {#call unsafe table_new#} (fromIntegral rows) (fromIntegral columns)
  (fromBool homogeneous)

-- Change the dimensions of an already existing table. (EXPORTED)
--
tableResize :: TableClass tb => Int -> Int -> tb -> IO ()
tableResize rows columns tb = {#call table_resize#} (toTable tb)
  (fromIntegral rows) (fromIntegral columns)

-- Put a new widget in the table container. The widget should span the cells
-- (leftAttach,topAttach) to (rightAttach,bottomAttach). Further formatting
-- options have to be specified. (EXPORTED)
--
tableAttach :: (TableClass tb, WidgetClass w) =>  w -> Int -> Int -> 
  Int -> Int -> AttachOptions -> AttachOptions -> Int -> Int -> tb -> IO ()
tableAttach child leftAttach rightAttach topAttach bottomAttach
  xoptions yoptions xpadding ypadding tb = {#call table_attach#} (toTable tb)
  (toWidget child) (fromIntegral leftAttach) (fromIntegral rightAttach) 
  (fromIntegral topAttach) (fromIntegral bottomAttach) 
  ((fromIntegral.fromEnum) xoptions) ((fromIntegral.fromEnum) yoptions) 
  (fromIntegral xpadding) (fromIntegral ypadding)

-- Put a new widget in the table container. As opposed to @tableAttach this
-- function assumes default values for the packing options. (EXPORTED)
--
tableAttachDefaults :: (TableClass tb, WidgetClass w) =>  
  w -> Int -> Int -> Int -> Int -> tb -> IO ()
tableAttachDefaults child leftAttach rightAttach topAttach bottomAttach tb =
  {#call table_attach_defaults#} (toTable tb) (toWidget child) 
  (fromIntegral leftAttach) (fromIntegral rightAttach) 
  (fromIntegral topAttach) (fromIntegral bottomAttach)

-- Set the amount of space (in pixels) between the specified @row and its 
-- neighbours. (EXPORTED)
--
tableSetRowSpacing :: TableClass tb => Int -> Int -> tb -> IO ()
tableSetRowSpacing row space tb = {#call table_set_row_spacing#}
  (toTable tb) (fromIntegral row) (fromIntegral space)

-- Set the amount of space (in pixels) between the specified column @col and
-- its neighbours. (EXPORTED)
--
tableSetColSpacing :: TableClass tb => Int -> Int -> tb -> IO ()
tableSetColSpacing col space tb = {#call table_set_col_spacing#}
  (toTable tb) (fromIntegral col) (fromIntegral space)

-- Set the amount of space between any two rows. (EXPORTED)
--
tableSetRowSpacings :: TableClass tb => Int -> tb -> IO ()
tableSetRowSpacings space tb = {#call table_set_row_spacings#}
  (toTable tb) (fromIntegral space)

-- Set the amount of space between any two columns. (EXPORTED)
--
tableSetColSpacings :: TableClass tb => Int -> tb -> IO ()
tableSetColSpacings space tb = {#call table_set_col_spacings#}
  (toTable tb) (fromIntegral space)

-- Make all cells the same size. (EXPORTED)
--
tableSetHomogeneous :: TableClass tb => Bool -> tb -> IO ()
tableSetHomogeneous hom tb = 
  {#call table_set_homogeneous#} (toTable tb) (fromBool hom)

