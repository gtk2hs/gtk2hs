-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Scale
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:19 $
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
-- * This is the abstract base class for HScale and VScale. It implements the
--   management of an adjustable value.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module Scale(
  Scale,
  ScaleClass,
  castToScale,
  scaleSetDigits,
  scaleSetDrawValue,
  PositionType(..),
  scaleSetValuePos
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(PositionType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Set the number of displayed digits after the comma. (EXPORTED)
--
scaleSetDigits :: ScaleClass s => Int -> s -> IO ()
scaleSetDigits prec s = 
  {#call scale_set_digits#} (toScale s) (fromIntegral prec)

-- Specify if the current value is to be drawn next to the slider. (EXPORTED)
--
scaleSetDrawValue :: ScaleClass s => Bool -> s -> IO ()
scaleSetDrawValue draw s =
  {#call scale_set_draw_value#} (toScale s) (fromBool draw)

-- Specify where the value is to be displayed (relative to the slider).
-- (EXPORTED)
--
scaleSetValuePos :: ScaleClass s => PositionType -> s -> IO ()
scaleSetValuePos pos s =
  {#call scale_set_value_pos#} (toScale s) ((fromIntegral.fromEnum) pos)


