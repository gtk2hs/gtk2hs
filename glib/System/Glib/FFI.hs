{-# OPTIONS -cpp #-}
--  GIMP Toolkit (GTK) version dependencies
--
--  Author : Axel Simon
--          
--  Created: 22 June 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 17:45:06 $
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
-- This module serves as an impedance matcher for different compiler
-- versions.
--

module System.Glib.FFI (
  with,
  nullForeignPtr,
  foreignFree,
  newForeignPtr,
  foreignPtrToPtr,
  module Foreign,
  module Foreign.C
  ) where

import Foreign (unsafePerformIO)

import Foreign.C
import qualified Foreign
# if __GLASGOW_HASKELL__>=602
import Foreign	 hiding		(with, newForeignPtr)
import qualified Foreign hiding	(newForeignPtr)
# else
import Foreign	 hiding (with)
# endif

with :: (Storable a) => a -> (Ptr a -> IO b) -> IO b
with = Foreign.with

#if __GLASGOW_HASKELL__>=602
newForeignPtr = flip Foreign.newForeignPtr

foreignPtrToPtr = unsafeForeignPtrToPtr
#endif

#if __GLASGOW_HASKELL__>=600
foreign import ccall unsafe "&free"	--TODO: should we be using g_free?
  free' :: FinalizerPtr a

foreignFree :: Ptr a -> FinalizerPtr a
foreignFree _ = free'

nullForeignPtr :: ForeignPtr a
nullForeignPtr = unsafePerformIO $ newForeignPtr nullPtr free'
#else
nullForeignPtr :: ForeignPtr a
nullForeignPtr = unsafePerformIO $ newForeignPtr nullPtr (return ())

foreignFree :: Ptr a -> IO ()
foreignFree = free
#endif

