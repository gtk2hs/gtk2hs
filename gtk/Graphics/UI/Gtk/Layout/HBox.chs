-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget HBox
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
-- This is a special version of 'Box'. This widget shows its child 
-- widgets in a horizontal line.
--
module Graphics.UI.Gtk.Layout.HBox (
  HBox,
  HBoxClass,
  castToHBox,
  hBoxNew
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | 
-- Create a container that shows several children horizontally. If 
-- @homogeneous@
-- is set all children will be allotted the same amount of space. There will be
-- @spacing@ pixel between each two children.
--
hBoxNew :: Bool -> Int -> IO HBox
hBoxNew homogeneous spacing = makeNewObject mkHBox $ liftM castPtr $
  {#call unsafe hbox_new#} (fromBool homogeneous) (fromIntegral spacing)


--
