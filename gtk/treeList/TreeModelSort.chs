-- -*-haskell-*-
--  GIMP Toolkit (GTK) TreeModelSort
--
--  Author : Axel Simon
--          
--  Created: 9 July 2002
--
--  Version $Revision: 1.3 $ from $Date: 2004/05/23 16:16:43 $
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
-- 'TreeModelSort' is an aggregated class to 'TreeModel'.
-- It turns any object derived from 'TreeModel' into a store that
-- is sorted.
--
module TreeModelSort(
  TreeModelSort,
  TreeModelSortClass

  ) where

import Monad	(liftM, when)
import FFI

{#import Hierarchy#}
import Signal	    

{# context lib="gtk" prefix="gtk" #}

