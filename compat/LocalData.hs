{-# OPTIONS -cpp #-}
--  GIMP Toolkit (GTK) @entry Reexport of moved entities@
--
--  Author : Axel Simon
--          
--  Created: 21 July 2002
--
--  Version $Revision: 1.2 $ from $Date: 2002/07/21 16:59:05 $
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
-- * This module reexports things that were in IOExts, ...
--   modules in the old module system and are now in Data.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module LocalData(
#if __GLASGOW_HASKELL__>=504
  module Data.IORef,
  module Data.Bits,
  module Data.FiniteMap,
  module System.IO.Unsafe
#else
  module IOExts,
  module Bits,
  module FiniteMap
#endif
  ) where

#if __GLASGOW_HASKELL__>=504
import Data.IORef
import Data.Bits
import Data.FiniteMap
import System.IO.Unsafe
#else
import IOExts
import Bits
import FiniteMap
#endif
