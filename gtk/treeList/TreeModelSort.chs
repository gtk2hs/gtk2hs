-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry TreeModelSort@
--
--  Author : Axel Simon
--          
--  Created: 9 July 2002
--
--  Version $Revision: 1.2 $ from $Date: 2003/07/09 22:42:46 $
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
-- * @ref type TreeModelSort@ is an aggregated class to @ref type TreeModel@.
--   It turns any object derived from @ref type TreeModel@ into a store that
--   is sorted.
--
--- DOCU ----------------------------------------------------------------------
--
--- TODO ----------------------------------------------------------------------
module TreeModelSort(
  TreeModelSort,
  TreeModelSortClass

  ) where

import Monad	(liftM, when)
import FFI

{#import Hierarchy#}
import Signal	    

{# context lib="gtk" prefix="gtk" #}

