-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget HSeparator
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.4 $ from $Date: 2004/05/23 16:10:57 $
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

module HSeparator(
  HSeparator,
  HSeparatorClass,
  castToHSeparator,
  hSeparatorNew
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

hSeparatorNew :: IO HSeparator
hSeparatorNew = makeNewObject mkHSeparator $ 
  liftM castPtr {#call unsafe hseparator_new#}
