--								  -*-haskell-*-
--  ===========================================================================
--  C -> Haskell Compiler: configuration
--
--  Author : Manuel M T Chakravarty
--  Created: 27 September 99
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/07 00:04:28 $
--
--  Copyright (c) [1999..2003] Manuel M T Chakravarty
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
  cpp, cppopts, cppoptsdef, hpaths, dlsuffix, tmpdir,
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

import Data.Ix    (Ix)
import Data.Array (Array, array)

import Foreign (Ptr, FunPtr)
import Foreign  (Storable(sizeOf, alignment), toBool)
import Foreign.C (CInt(..))
import System.Info (os)

-- program settings
-- ----------------

-- C preprocessor executable (EXPORTED)
--
cpp :: FilePath
cpp  = case os of
  "darwin" -> "gcc"
  _        -> "cpp"

-- C preprocessor options (EXPORTED)
--
-- * `-x c' forces CPP to regard the input as C code; this option seems to be
--   understood at least on Linux, FreeBSD, and Solaris and seems to make a
--   difference over the default language setting on FreeBSD
--
-- * `-P' would suppress `#line' directives
--
cppopts :: [String]
cppopts  = case (os,cpp) of
  ("openbsd","cpp") -> ["-xc", "-w"]
  (_,"cpp")         -> ["-x", "c", "-w"]
  (_,"gcc")         -> ["-E", "-x", "c", "-w"]
  _                 -> []

-- C preprocessor option for including only definitions (EXPORTED)
cppoptsdef :: String
cppoptsdef = "-imacros"

-- standard system search paths for header files (EXPORTED)
--
hpaths :: [FilePath]
hpaths  = [".", "/usr/include", "/usr/local/include"]

-- OS-dependent suffix for dynamic libraries
--
dlsuffix :: String
dlsuffix  = error "C2HSConfig.dlsuffix" -- used to be: "@DLSUFFIX@"

-- possibly system-dependent location for temporary files
--
tmpdir :: String
tmpdir = error "C2HSConfig.tmpdir" -- used to be: "@TMPDIR@"
  -- tmpdir is unused and it causes problems on widows since it ends up with
  -- the value "C:\TMP" which is not a valid string. It'd need to be "C:\\TMP"
  -- so just remove the thing for now.

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
