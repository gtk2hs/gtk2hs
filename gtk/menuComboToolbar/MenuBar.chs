-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Blah
--
--  Author : Axel Simon
--          
--  Created: 21 May 2001
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
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module MenuBar(
  MenuBar,
  MenuBarClass,
  castToMenuBar,
  menuBarNew
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a horizontal bar that contains menu items. (EXPORTED)
--
menuBarNew :: IO MenuBar
menuBarNew = makeNewObject mkMenuBar $ 
  liftM castPtr {#call unsafe menu_bar_new#}
