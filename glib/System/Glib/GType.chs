-- -*-haskell-*-
--  GIMP Toolkit (GTK) GType
--
--  Author : Axel Simon
--
--  Created: 1 June 2001
--
--  Copyright (c) 1999..2002 Axel Simon
--
--  Copyright (C) 2001 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- This module implements only the necessities for the GTK binding.
--
module System.Glib.GType (
  GType,
  typeInstanceIsA,
  glibTypeInit,
  ) where

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


-- | Prior to any use of the glib type/object system, @glibTypeInit@ has to
-- be called to initialise the system.
--
-- Note that this is not needed for gtk applications using @initGUI@ since
-- that initialises everything itself. It is only needed for applications
-- using glib directly, without also using gtk.
--
glibTypeInit :: IO ()
glibTypeInit = {# call g_type_init #}
