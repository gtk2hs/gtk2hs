-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget VScale@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/24 09:43:24 $
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
-- * The user may enter a value by moving the handle on the scale.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

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

-- @constructor vScaleNew@ Create a new VScale widget.
--
vScaleNew :: Adjustment -> IO VScale
vScaleNew adj = makeNewObject mkVScale $ liftM castPtr $
  {#call unsafe vscale_new#} adj

