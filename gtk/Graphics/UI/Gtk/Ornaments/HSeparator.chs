-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget HSeparator
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:30:19 $
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
-- The HSeparator widget is a horizontal separator, used to group the 
-- widgets within a window. It displays a horizontal line with a shadow 
-- to make it appear sunken into the interface.
--
-- * This has nothing to do with a menu separator.
--

module Graphics.UI.Gtk.Ornaments.HSeparator (
  HSeparator,
  HSeparatorClass,
  castToHSeparator,
  hSeparatorNew
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

-- methods

hSeparatorNew :: IO HSeparator
hSeparatorNew = makeNewObject mkHSeparator $ 
  liftM castPtr {#call unsafe hseparator_new#}
