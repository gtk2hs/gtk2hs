-- -*-haskell-*-
--  GIMP Toolkit (GTK) GType
--
--  Author : Axel Simon
--          
--  Created: 1 June 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/01/16 21:29:41 $
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
-- * This module implements only the necessities for the GTK binding.
--
module System.Glib.GType (
  GType,
  typeInstanceIsA
  ) where

import Monad	(liftM)
import System.Glib.FFI

{# context lib="glib" prefix="g" #}

type GType = {#type GType#}

-- | Check if an object is of the specific type or derived from it.
--
-- * Internally used by Hierarchy.
--
typeInstanceIsA :: Ptr () -> GType -> Bool
typeInstanceIsA obj p = toBool $
  unsafePerformIO ({#call unsafe g_type_check_instance_is_a#} obj p)

