-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget CheckMenuItem@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.4 $ from $Date: 2002/11/08 10:39:21 $
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
-- * This widget implements a @ref data MenuItem@ with a check next to it.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

module CheckMenuItem(
  CheckMenuItem,
  CheckMenuItemClass,
  checkMenuItemNew,
  checkMenuItemNewWithLabel,
  checkMenuItemSetActive,
  checkMenuItemGetActive,
  checkMenuItemSetInconsistent,
  checkMenuItemGetInconsistent
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Structs  (checkMenuItemGetActive)

{#context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor checkMenuItemNew@ Create a new @ref data MenuItem@ with a check
-- next to it.
--
checkMenuItemNew :: IO CheckMenuItem
checkMenuItemNew  = makeNewObject mkCheckMenuItem $ liftM castPtr $
  {#call unsafe check_menu_item_new#}

-- @method checkMenuItemNewWithLabel@ Create a new @ref type CheckMenuItem@
-- with a @ref data Label@ inside.
--
checkMenuItemNewWithLabel :: String -> IO CheckMenuItem
checkMenuItemNewWithLabel str = withCString str $ \strPtr ->
  makeNewObject mkCheckMenuItem $ liftM castPtr $
  {#call unsafe check_menu_item_new_with_label#} strPtr


-- @method checkMenuItemSetActive@ Set the state of the menu item check.
--
checkMenuItemSetActive :: CheckMenuItemClass mi => mi -> Bool -> IO ()
checkMenuItemSetActive mi active = {#call check_menu_item_set_active#}
  (toCheckMenuItem mi) (fromBool active)

-- @method checkMenuItemSetInconsistent@ Set the state of the menu item check
-- to `inconsistent'.
--
checkMenuItemSetInconsistent :: CheckMenuItemClass mi => mi -> Bool -> IO ()
checkMenuItemSetInconsistent mi inconsistent = 
  {#call check_menu_item_set_inconsistent#} (toCheckMenuItem mi) 
    (fromBool inconsistent)

-- @method checkMenuItemGetInconsistent@ Query if the menu check is
-- inconsistent (inbetween).
--
checkMenuItemGetInconsistent :: CheckMenuItemClass mi => mi -> IO Bool
checkMenuItemGetInconsistent mi = liftM toBool $
  {#call unsafe check_menu_item_get_inconsistent#} (toCheckMenuItem mi)


