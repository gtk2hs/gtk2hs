-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget CheckMenuItem
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
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
-- * This widget implements a @MenuItem with a check next to it.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

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

-- Create a new @MenuItem with a check next to it. (EXPORTED)
--
checkMenuItemNew :: IO CheckMenuItem
checkMenuItemNew = makeNewObject mkCheckMenuItem $ liftM castPtr $
  {#call unsafe check_menu_item_new#}

-- Create a new @CheckMenuItem with a @Label inside. (EXPORTED)
--
checkMenuItemNewWithLabel :: String -> IO CheckMenuItem
checkMenuItemNewWithLabel str = withCString str $ \strPtr ->
  makeNewObject mkCheckMenuItem $ liftM castPtr $
  {#call unsafe check_menu_item_new_with_label#} strPtr


-- Set the state of the menu item check. (EXPORTED)
--
checkMenuItemSetActive :: CheckMenuItemClass mi => Bool -> mi -> IO ()
checkMenuItemSetActive active mi = {#call check_menu_item_set_active#}
  (toCheckMenuItem mi) (fromBool active)

-- Set the state of the menu item check to `inconsistent'. (EXPORTED)
--
checkMenuItemSetInconsistent :: CheckMenuItemClass mi => Bool -> mi -> IO ()
checkMenuItemSetInconsistent inconsistent mi = 
  {#call check_menu_item_set_inconsistent#} (toCheckMenuItem mi) 
    (fromBool inconsistent)

-- Query if the menu check is inconsistent (inbetween). (EXPORTED)
--
checkMenuItemGetInconsistent :: CheckMenuItemClass mi => mi -> IO Bool
checkMenuItemGetInconsistent mi = liftM toBool $
  {#call unsafe check_menu_item_get_inconsistent#} (toCheckMenuItem mi)


