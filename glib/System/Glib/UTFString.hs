{-# OPTIONS -cpp #-}
--  GIMP Toolkit (GTK) UTF aware string marshalling
--
--  Author : Axel Simon
--          
--  Created: 22 June 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/01/16 21:40:33 $
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
-- This module adds CString-like functions that handle UTF8 strings.
--

module System.Glib.UTFString (
  withUTFString,
  withUTFStringLen,
  newUTFString,
  newUTFStringLen,
  peekUTFString,
  peekUTFStringLen,
  readUTFString,
  readCString,
  ) where

import Monad	(liftM)
import Char
import Data.Bits

import System.Glib.FFI

-- Define withUTFString to emit UTF-8.
--
withUTFString :: String -> (CString -> IO a) -> IO a
withUTFString hsStr = withCString (toUTF hsStr)

-- Define withUTFStringLen to emit UTF-8.
--
withUTFStringLen :: String -> (CStringLen -> IO a) -> IO a
withUTFStringLen hsStr = withCStringLen (toUTF hsStr)

-- Define newUTFString to emit UTF-8.
--
newUTFString :: String -> IO CString
newUTFString = newCString . toUTF

-- Define newUTFStringLen to emit UTF-8.
--
newUTFStringLen :: String -> IO CStringLen
newUTFStringLen = newCStringLen . toUTF

-- Define peekUTFString to retrieve UTF-8.
--
peekUTFString :: CString -> IO String
peekUTFString strPtr = liftM fromUTF $ peekCString strPtr

-- Define peekUTFStringLen to retrieve UTF-8.
--
peekUTFStringLen :: CStringLen -> IO String
peekUTFStringLen strPtr = liftM fromUTF $ peekCStringLen strPtr

-- like peekUTFString but then frees the string using g_free
--
readUTFString :: CString -> IO String
readUTFString strPtr = do
  str <- peekUTFString strPtr
  free (castPtr strPtr)
  return str 

-- like peekCString but then frees the string using g_free
--
readCString :: CString -> IO String
readCString strPtr = do
  str <- peekCString strPtr
  free (castPtr strPtr)
  return str

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
