-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget TearoffMenuItem@
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
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
-- * A TearoffMenuItem is a special GtkMenuItem which is used to tear off 
--   and reattach its menu. When its menu is shown normally, the 
--   TearoffMenuItem is drawn as a dotted line indicating that the menu can 
--   be torn off. Activating it causes its menu to be torn off and displayed 
--   in its own window as a tearoff menu. When its menu is shown as a tearoff 
--   menu, the TearoffMenuItem is drawn as a dotted line which has a left 
--   pointing arrow graphic indicating that the tearoff menu can be reattached.
--   Activating it will erase the tearoff menu window.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

module TearoffMenuItem(
  TearoffMenuItem,
  TearoffMenuItemClass,
  castToTearoffMenuItem,
  tearoffMenuItemNew
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor tearoffMenuItemNew@ Create a new tear off menu item.
--
tearoffMenuItemNew :: IO TearoffMenuItem
tearoffMenuItemNew  = makeNewObject mkTearoffMenuItem $ liftM castPtr
  {#call unsafe tearoff_menu_item_new#}

