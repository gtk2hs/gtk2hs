-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget VBox@
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2002/08/05 16:41:34 $
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
-- * This is a special version of @ref data Box@. 
--   This widget shows its child widgets
--   in a vertical line.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

module VBox(
  VBox,
  VBoxClass,
  castToVBox,
  vBoxNew
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor vBoxNew@ 
-- Create a container that shows several children vertically. 
--
-- * If @ref arg homogeneous@
-- is set all children will be allotted the same amount of space. There will be
-- @ref arg spacing@ pixel between each two children.
--
vBoxNew :: Bool -> Int -> IO VBox
vBoxNew homogeneous spacing = makeNewObject mkVBox $ liftM castPtr $
  {#call unsafe vbox_new#} (fromBool homogeneous) (fromIntegral spacing)



