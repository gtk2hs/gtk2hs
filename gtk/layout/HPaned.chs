-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget HPaned
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.5 $ from $Date: 2004/05/23 16:02:58 $
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

module HPaned(
  HPaned,
  HPanedClass,
  castToHPaned,
  hPanedNew
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | 
--
hPanedNew :: IO HPaned
hPanedNew = makeNewObject mkHPaned $ liftM castPtr {#call unsafe hpaned_new#}
