-- -*-haskell-*-
--  GIMP Toolkit (GTK) GParameter
--
--  Author : Duncan Coutts
--          
--  Created: 29 March 2004
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 17:46:16 $
--
--  Copyright (c) 2004 Duncan Coutts
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
-- Storable instance for GParameter, used by objectNew
--

module System.Glib.GParameter (
  GParameter(..)
  ) where

import Foreign
import Foreign.C

import System.Glib.GValue
--import StoreValue

#include <glib-object.h>

--newtype GParameter = GParameter (String, GenericValue)
newtype GParameter = GParameter (String, GValue)

instance Storable GParameter where
  sizeOf    _ = #const sizeof(GParameter)
  alignment _ = #{const __alignof__(GParameter)}
  poke ptr (GParameter (name, value)) = do
    strPtr <- newCString name
    #{poke GParameter, name}  ptr strPtr
--    poke (#{ptr GParameter, value} ptr) value
    #{poke GParameter, value} ptr value
