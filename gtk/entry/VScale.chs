-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget VScale
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
-- * The user may enter a value by moving the handle on the scale.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module VScale(
  VScale,
  VScaleClass,
  castToVScale,
  vScaleNew
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a new VScale widget. (EXPORTED)
--
vScaleNew :: Adjustment -> IO VScale
vScaleNew adj = makeNewObject mkVScale $ liftM castPtr $
  {#call unsafe vscale_new#} adj

