{-# OPTIONS -cpp #-}
--  GIMP Toolkit (GTK) @entry UTF@
--
--  Author : Axel Simon
--          
--  Created: 22 June 2001
--
--  Version $Revision: 1.5 $ from $Date: 2002/08/05 16:41:34 $
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
-- * This module replaces the CString functions with UTF aware drop-ins.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module UTFCForeign(
  with',
  withCString,
  withCStringLen,
  newCString,
  newCStringLen,
  peekCString,
  peekCStringLen,
#if __GLASGOW_HASKELL__>=504
  module Foreign.C
#else
  module CForeign
#endif
  ) where

import Monad	(liftM)
#if __GLASGOW_HASKELL__<=502
import Bits
#endif
#if __GLASGOW_HASKELL__>=504
import Data.Bits
#endif
import Char
#if __GLASGOW_HASKELL__>=504
import qualified Foreign.C as CForeign
import Foreign.C hiding (withCString, withCStringLen,
		         newCString,  newCStringLen,
		         peekCString, peekCStringLen)

import qualified Foreign
import Foreign	 hiding (with)

with' :: (Storable a) => a -> (Ptr a -> IO b) -> IO b
with' = Foreign.with
#else
import qualified CForeign
import CForeign hiding (withCString, withCStringLen,
		        newCString,  newCStringLen,
		        peekCString, peekCStringLen)

import qualified Foreign
import Foreign	 hiding (withObject)

with' :: (Storable a) => a -> (Ptr a -> IO b) -> IO b
with' = Foreign.withObject
#endif

-- Convert withCString to emit UTF-8.
--
withCString :: String -> (CString -> IO a) -> IO a
withCString hsStr = CForeign.withCString (toUTF hsStr)

-- Convert withCStringLen to emit UTF-8.
--
withCStringLen :: String -> (CStringLen -> IO a) -> IO a
withCStringLen hsStr = CForeign.withCStringLen (toUTF hsStr)

-- Convert newCString to emit UTF-8.
--
newCString :: String -> IO CString
newCString = CForeign.newCString . toUTF

-- Convert newCStringLen to emit UTF-8.
--
newCStringLen :: String -> IO CStringLen
newCStringLen = CForeign.newCStringLen . toUTF

-- Convert peekCString to retrieve UTF-8.
--
peekCString :: CString -> IO String
peekCString strPtr = liftM fromUTF $ CForeign.peekCString strPtr

-- Convert peekCStringLen to retrieve UTF-8.
--
peekCStringLen :: CStringLen -> IO String
peekCStringLen strPtr = liftM fromUTF $ CForeign.peekCStringLen strPtr

-- Convert Unicode characters to UTF-8.
--
toUTF :: String -> String
toUTF [] = []
toUTF (x:xs) | ord x<=0x007F = x:toUTF xs
	     | ord x<=0x07FF = chr (0xC0 .|. ((ord x `shift` (-6)) .&. 0x1F)):
			       chr (0x80 .|. (ord x .&. 0x3F)):
			       toUTF xs
	     | otherwise     = chr (0xE0 .|. ((ord x `shift` (-12)) .&. 0x0F)):
			       chr (0x80 .|. ((ord x `shift` (-6)) .&. 0x3F)):
			       chr (0x80 .|. (ord x .&. 0x3F)):
			       toUTF xs

-- Convert UTF-8 to Unicode.
--
fromUTF :: String -> String
fromUTF [] = []
fromUTF (all@(x:xs)) | ord x<=0x7F = x:fromUTF xs
		     | ord x<=0xBF = err
		     | ord x<=0xDF = twoBytes all
		     | ord x<=0xEF = threeBytes all
		     | otherwise   = err
  where
    twoBytes (x1:x2:xs) = chr (((ord x1 .&. 0x1F) `shift` 6) .|.
			       (ord x2 .&. 0x3F)):fromUTF xs
    twoBytes _ = error "fromUTF: illegal two byte sequence"

    threeBytes (x1:x2:x3:xs) = chr (((ord x1 .&. 0x0F) `shift` 12) .|.
				    ((ord x2 .&. 0x3F) `shift` 6) .|.
				    (ord x3 .&. 0x3F)):fromUTF xs
    threeBytes _ = error "fromUTF: illegal three byte sequence" 
    
    err = error "fromUTF: illegal UTF-8 character"
