-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget VBox
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:23:39 $
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
-- |
--
-- This is a special version of 'Box'. 
-- This widget shows its child widgets
-- in a vertical line.
--

module Graphics.UI.Gtk.Layout.VBox (
  VBox,
  VBoxClass,
  castToVBox,
  vBoxNew
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | 
-- Create a container that shows several children vertically. 
--
-- * If @homogeneous@
-- is set all children will be allotted the same amount of space. There will be
-- @spacing@ pixel between each two children.
--
vBoxNew :: Bool -> Int -> IO VBox
vBoxNew homogeneous spacing = makeNewObject mkVBox $ liftM castPtr $
  {#call unsafe vbox_new#} (fromBool homogeneous) (fromIntegral spacing)



