-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Blah@
--
--  Author : Axel Simon
--          
--  Created: 21 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/24 09:43:25 $
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
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

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

-- @constructor menuBarNew@ Create a horizontal bar that contains menu items.
--
menuBarNew :: IO MenuBar
menuBarNew  = makeNewObject mkMenuBar $ 
  liftM castPtr {#call unsafe menu_bar_new#}
