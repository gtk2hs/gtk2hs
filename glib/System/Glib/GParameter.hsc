-- -*-haskell-*-
--  GIMP Toolkit (GTK) GParameter
--
--  Author : Duncan Coutts
--
--  Created: 29 March 2004
--
--  Copyright (c) 2004 Duncan Coutts
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
-- Storable instance for GParameter, used by objectNew
--

module System.Glib.GParameter (
  GParameter(..)
  ) where

import Foreign
import Foreign.C

import System.Glib.GValue

#include<glib-object.h>

--newtype GParameter = GParameter (String, GenericValue)
newtype GParameter = GParameter (String, GValue)

instance Storable GParameter where
  sizeOf    _ = #const sizeof(GParameter)
  alignment _ = #{const __alignof__(GParameter)}
  poke ptr (GParameter (name, GValue gvaluePtr)) = do
    strPtr <- newCString name
    #{poke GParameter, name}  ptr strPtr
--    poke (#{ptr GParameter, value} ptr) value
    #{poke GParameter, value} ptr gvaluePtr
