--  GIMP Toolkit (GTK) FFI extras and version dependencies
--
--  Author : Axel Simon
--
--  Created: 22 June 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/12/08 17:30:54 $
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- #hide

-- |
--
-- This module serves as an impedance matcher for different compiler
-- versions. It also adds a few FFI utility functions.
--

module System.Glib.FFI (
  with,
  nullForeignPtr,
  maybeNull,
  newForeignPtr,
  withForeignPtrs,
#if __GLASGOW_HASKELL__<604
  withArrayLen,
#endif
#if __GLASGOW_HASKELL__<602
  unsafeForeignPtrToPtr,
#endif
# if __GLASGOW_HASKELL__<600
  -- ghc 6 exports unsafePerformIO from module Foreign
  -- provide it here for ghc 5
  unsafePerformIO,
# endif
  module Foreign,
  module Foreign.C
  ) where

import System.IO.Unsafe (unsafePerformIO)

-- We should almost certainly not be using the standard free function anywhere
-- in the glib or gtk bindings, so we do not re-export it from this module.

import Foreign.C
import qualified Foreign hiding (free)
# if __GLASGOW_HASKELL__>=602
import Foreign  hiding	(with, newForeignPtr, free)
# else
import Foreign  hiding (with, free)
# endif

with :: (Storable a) => a -> (Ptr a -> IO b) -> IO b
with = Foreign.with

#if __GLASGOW_HASKELL__<604
withArrayLen :: Storable a => [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen elems act = let len = length elems in withArray elems (act len)
#endif


#if __GLASGOW_HASKELL__>=602
newForeignPtr = flip Foreign.newForeignPtr
#endif

#if __GLASGOW_HASKELL__<602
unsafeForeignPtrToPtr = foreignPtrToPtr
#endif

#if __GLASGOW_HASKELL__>=602
nullForeignPtr :: ForeignPtr a
nullForeignPtr = unsafePerformIO $ newForeignPtr_ nullPtr
#elif __GLASGOW_HASKELL__>=600
nullForeignPtr :: ForeignPtr a
nullForeignPtr = unsafePerformIO $ newForeignPtr nullPtr freePtr

foreign import ccall unsafe "&free"
  freePtr :: FinalizerPtr a
#else
nullForeignPtr :: ForeignPtr a
nullForeignPtr = unsafePerformIO $ newForeignPtr nullPtr (return ())
#endif

-- This is useful when it comes to marshaling lists of GObjects
--
withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs fptrs body = do
  result <- body (map unsafeForeignPtrToPtr fptrs)
  mapM_ touchForeignPtr fptrs
  return result

-- A marshaling utility function that is used by the code produced by the code
-- generator to marshal return values that can be null
maybeNull :: (IO (Ptr a) -> IO a) -> IO (Ptr a) -> IO (Maybe a)
maybeNull marshal genPtr = do
  ptr <- genPtr
  if ptr == nullPtr
    then return Nothing
    else do result <- marshal (return ptr)
            return (Just result)
