--								  -*-haskell-*-
--  ** c2hs/toplevel/C2HSConfig.hs.  Generated from C2HSConfig.hs.in by configure. **
--  ===========================================================================
--  C -> Haskell Compiler: configuration
--
--  Author : Manuel M T Chakravarty
--  Created: 27 September 99
--
--  Version $Revision: 1.1 $ from $Date: 2002/04/14 16:57:47 $
--
--  Copyright (c) [1999..2001] Manuel M T Chakravarty
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
--  Configuration options; largely set by `configure'.
--
--- TODO ----------------------------------------------------------------------
--

module C2HSConfig (
  --
  -- programs and paths
  --
  cpp, cppopts, hpaths, dlsuffix,
  --
  -- system-dependent definitions, as the New FFI isn't fully supported on all
  -- systems yet
  --
  Ptr, FunPtr, Storable(sizeOf, alignment),
  --
  -- parameters of the targeted C compiler
  --
  bitfieldDirection, bitfieldPadding, bitfieldIntSigned, bitfieldAlignment
) where

import Ix    (Ix)
import Array (Array, array)


import 
 Ptr  (Ptr, FunPtr)  -- on an extra line to trick the stupid `mkdependHS'


{- for systems including the Legacy FFI
import 
 Addr (Addr)  -- on an extra line to trick the stupid `mkdependHS'
-}


import Storable (Storable(sizeOf, alignment))


{- NHC does some things differently...
import 
 FFI (Storable(sizeOf, alignment))  -- on an extra line to trick the stupid `mkdependHS'
-}

import CTypes       (CInt)
import MarshalUtils (toBool)


-- program settings
-- ----------------

-- C preprocessor executable (EXPORTED)
--
cpp :: FilePath
cpp  = "gcc -E"

-- C preprocessor options (EXPORTED)
--
-- * `-P' would suppress `#line' directives
--
cppopts :: String
cppopts  = "-x c"

-- standard system search paths for header files (EXPORTED)
--
hpaths :: [FilePath]
hpaths  = [".", "/usr/include", "/usr/local/include"]

-- OS-dependent suffix for dynamic libraries
--
dlsuffix :: String
dlsuffix  = ".dll"


-- the FFI of older systems doesn't have Ptr
-- -----------------------------------------

{- for systems including the Legacy FFI
type Ptr    a = Addr
type FunPtr a = Ptr a
-}


-- parameters of the targeted C compiler
-- -------------------------------------

-- indicates in which direction the C compiler fills bitfields (EXPORTED)
--
-- * the value is 1 or -1, depending on whether the direction is growing
--   towards the MSB
--
bitfieldDirection :: Int
bitfieldDirection  = fromIntegral bitfield_direction

foreign import ccall bitfield_direction :: CInt

-- indicates whether a bitfield that does not fit into a partially filled
-- storage unit in its entirety introduce padding or split over two storage
-- units (EXPORTED)
--
-- * `True' means that such a bitfield introduces padding (instead of being
--   split)
--
bitfieldPadding :: Bool
bitfieldPadding  = toBool bitfield_padding

foreign import ccall bitfield_padding :: CInt

-- indicates whether a bitfield of type `int' is signed in the targeted C
-- compiler (EXPORTED)
--
bitfieldIntSigned :: Bool
bitfieldIntSigned  = toBool bitfield_int_signed

foreign import ccall bitfield_int_signed :: CInt

-- the alignment constraint for a bitfield (EXPORTED)
--
-- * this makes the assumption that the alignment of a bitfield is independent
--   of the bitfield's size
--
bitfieldAlignment :: Int
bitfieldAlignment  = fromIntegral bitfield_alignment

foreign import ccall bitfield_alignment :: CInt
