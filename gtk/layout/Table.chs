-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Table@
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2002/06/20 14:13:05 $
--
--  Copyright (c) 1999..2002 Axel Simon
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
-- @description@ --------------------------------------------------------------
--
-- * The table widget is a container in which widgets can be aligned in cells.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

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
import Enums	(AttachOptions(..), fromFlags)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor tableNew@ Create a new table with the specified dimensions.
-- Set @ref arg homogeneous@ to True if all cells should be of the same size.
--
tableNew :: Int -> Int -> Bool -> IO Table
tableNew rows columns homogeneous = makeNewObject mkTable $ liftM castPtr $
  {#call unsafe table_new#} (fromIntegral rows) (fromIntegral columns)
  (fromBool homogeneous)

-- @method tableResize@ Change the dimensions of an already existing table.
--
tableResize :: TableClass tb => tb -> Int -> Int -> IO ()
tableResize tb rows columns = {#call table_resize#} (toTable tb)
  (fromIntegral rows) (fromIntegral columns)

-- @method tableAttach@ Put a new widget in the table container. The widget
-- should span the cells (leftAttach,topAttach) to (rightAttach,bottomAttach).
-- Further formatting options have to be specified.
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

-- @method tableAttachDefaults@ Put a new widget in the table container. As
-- opposed to @ref method tableAttach@ this function assumes default values
-- for the packing options.
--
tableAttachDefaults :: (TableClass tb, WidgetClass w) => tb -> w -> Int ->
                       Int -> Int -> Int -> IO ()
tableAttachDefaults tb child leftAttach rightAttach topAttach bottomAttach =
  {#call table_attach_defaults#} (toTable tb) (toWidget child) 
  (fromIntegral leftAttach) (fromIntegral rightAttach) 
  (fromIntegral topAttach) (fromIntegral bottomAttach)

-- @method tableSetRowSpacing@ Set the amount of space (in pixels) between the
-- specified @ref arg row@ and its neighbours.
--
tableSetRowSpacing :: TableClass tb => tb -> Int -> Int -> IO ()
tableSetRowSpacing tb row space = {#call table_set_row_spacing#}
  (toTable tb) (fromIntegral row) (fromIntegral space)

-- @method tableSetColSpacing@ Set the amount of space (in pixels) between the
-- specified column @ref arg col@ and its neighbours.
--
tableSetColSpacing :: TableClass tb => tb -> Int -> Int -> IO ()
tableSetColSpacing tb col space = {#call table_set_col_spacing#}
  (toTable tb) (fromIntegral col) (fromIntegral space)

-- @method tableSetRowSpacings@ Set the amount of space between any two rows.
--
tableSetRowSpacings :: TableClass tb => tb -> Int -> IO ()
tableSetRowSpacings tb space = {#call table_set_row_spacings#}
  (toTable tb) (fromIntegral space)

-- @method tableSetColSpacings@ Set the amount of space between any two
-- columns.
--
tableSetColSpacings :: TableClass tb => tb -> Int -> IO ()
tableSetColSpacings tb space = {#call table_set_col_spacings#}
  (toTable tb) (fromIntegral space)

-- @method tableSetHomogeneous@ Make all cells the same size.
--
tableSetHomogeneous :: TableClass tb => tb -> Bool -> IO ()
tableSetHomogeneous tb hom = 
  {#call table_set_homogeneous#} (toTable tb) (fromBool hom)

