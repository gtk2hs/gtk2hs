-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget VSeparator
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
-- * The VSeparator widget is a horizontal separator, used to group the 
--   widgets within a window. It displays a horizontal line with a shadow 
--   to make it appear sunken into the interface.
--
-- * This has nothing to do with a menu separator.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module VSeparator(
  VSeparator,
  VSeparatorClass,
  castToVSeparator,
  vSeparatorNew
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

vSeparatorNew :: IO VSeparator
vSeparatorNew = makeNewObject mkVSeparator $ 
  liftM castPtr {#call unsafe vseparator_new#}
