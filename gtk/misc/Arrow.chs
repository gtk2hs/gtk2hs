-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Arrow
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
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
-- * An Arrow pointing to one of the four cardinal direction.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module Arrow(
  Arrow,
  ArrowClass,
  castToArrow,
  ArrowType(..),
  ShadowType(..),
  arrowNew,
  arrowSet
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(ArrowType(..), ShadowType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a new arrow with display options. (EXPORTED)
--
arrowNew :: ArrowType -> ShadowType -> IO Arrow
arrowNew at st = makeNewObject mkArrow $ liftM castPtr $ 
  {#call unsafe arrow_new#} ((fromIntegral.fromEnum) at) 
  ((fromIntegral.fromEnum) st)

-- Change the visual appearance of this widget. (EXPORTED)
--
arrowSet :: ArrowClass a => ArrowType -> ShadowType -> a -> IO ()
arrowSet at st a = {#call arrow_set#} (toArrow a) ((fromIntegral.fromEnum) at)
  ((fromIntegral.fromEnum) st)

