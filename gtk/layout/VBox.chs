-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget VBox
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
-- * This is a special version of @Box. This widget shows its child widgets
--   in a vertical line.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

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

-- Create a container that shows several children vertically. If @homogeneous
-- is set all children will be allotted the same amount of space. There will be
-- @spacing pixel between each two children.
--
vBoxNew :: Bool -> Int -> IO VBox
vBoxNew homogeneous spacing = makeNewObject mkVBox $ liftM castPtr $
  {#call unsafe vbox_new#} (fromBool homogeneous) (fromIntegral spacing)



