-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget MenuBar
--
--  Author : Axel Simon
--          
--  Created: 21 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:27:03 $
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

module Graphics.UI.Gtk.MenuComboToolbar.MenuBar (
  MenuBar,
  MenuBarClass,
  castToMenuBar,
  menuBarNew
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a horizontal bar that contains menu items.
--
menuBarNew :: IO MenuBar
menuBarNew  = makeNewObject mkMenuBar $ 
  liftM castPtr {#call unsafe menu_bar_new#}
