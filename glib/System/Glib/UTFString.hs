--  GIMP Toolkit (GTK) UTF aware string marshalling
--
--  Author : Axel Simon
--
--  Created: 22 June 2001
--
--  Copyright (c) 1999..2002 Axel Simon
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
-- This module adds CString-like functions that handle UTF8 strings.
--

module System.Glib.UTFString (
  withUTFString,
  withUTFStringLen,
  newUTFString,
  newUTFStringLen,
  peekUTFString,
  peekUTFStringLen,
  maybePeekUTFString,
  readUTFString,
  readCString,
  withUTFStrings,
  withUTFStringArray,
  withUTFStringArray0,
  peekUTFStringArray,
  peekUTFStringArray0,
  readUTFStringArray0,
  UTFCorrection,
  genUTFOfs,
  ofsToUTF,
  ofsFromUTF
  ) where

import Control.Monad	(liftM)
import Data.Char (ord, chr)
import Data.Maybe (maybe)

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

-- Define maybePeekUTFString to retrieve UTF-8 from a ptr which is maybe null.
--
maybePeekUTFString :: CString -> IO (Maybe String)
maybePeekUTFString strPtr = liftM (maybe Nothing (Just . fromUTF)) $ maybePeek peekCString strPtr

-- Define peekUTFStringLen to retrieve UTF-8.
--
peekUTFStringLen :: CStringLen -> IO String
peekUTFStringLen strPtr = liftM fromUTF $ peekCStringLen strPtr

-- like peekUTFString but then frees the string using g_free
--
readUTFString :: CString -> IO String
readUTFString strPtr = do
  str <- peekUTFString strPtr
  g_free strPtr
  return str

-- like peekCString but then frees the string using g_free
--
readCString :: CString -> IO String
readCString strPtr = do
  str <- peekCString strPtr
  g_free strPtr
  return str

foreign import ccall unsafe "g_free"
  g_free :: Ptr a -> IO ()

-- Temporarily allocate a list of UTF-8 Strings.
--
withUTFStrings :: [String] -> ([CString] -> IO a) -> IO a
withUTFStrings hsStrs = withUTFStrings' hsStrs []
  where withUTFStrings' :: [String] -> [CString] -> ([CString] -> IO a) -> IO a
        withUTFStrings' []     cs body = body (reverse cs)
        withUTFStrings' (s:ss) cs body = withUTFString s $ \c ->
                                         withUTFStrings' ss (c:cs) body

-- Temporarily allocate an array of UTF-8 Strings.
--
withUTFStringArray :: [String] -> (Ptr CString -> IO a) -> IO a
withUTFStringArray hsStr body =
  withUTFStrings hsStr $ \cStrs -> do
  withArray cStrs body

-- Temporarily allocate a null-terminated array of UTF-8 Strings.
--
withUTFStringArray0 :: [String] -> (Ptr CString -> IO a) -> IO a
withUTFStringArray0 hsStr body =
  withUTFStrings hsStr $ \cStrs -> do
  withArray0 nullPtr cStrs body

-- Convert an array of UTF-8 strings of the given length to a list of Haskell
-- strings.
--
peekUTFStringArray :: Int -> Ptr CString -> IO [String]
peekUTFStringArray len cStrArr = do
  cStrs <- peekArray len cStrArr
  mapM peekUTFString cStrs

-- Convert a null-terminated array of UTF-8 strings to a list of Haskell
-- strings.
--
peekUTFStringArray0 :: Ptr CString -> IO [String]
peekUTFStringArray0 cStrArr = do
  cStrs <- peekArray0 nullPtr cStrArr
  mapM peekUTFString cStrs

-- Like 'peekUTFStringArray0' but then free the string array including all strings.
--
-- To be used when functions indicate that their return value should be freed
-- with \"g_strfreev\".
--
readUTFStringArray0 :: Ptr CString -> IO [String]
readUTFStringArray0 cStrArr | cStrArr == nullPtr = return []
                            | otherwise = do
  cStrs <- peekArray0 nullPtr cStrArr
  strings <- mapM peekUTFString cStrs
  g_strfreev cStrArr
  return strings

foreign import ccall unsafe "g_strfreev"
  g_strfreev :: Ptr a -> IO ()

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

-- Offset correction for String to UTF8 mapping.
--
newtype UTFCorrection = UTFCorrection [Int] deriving Show

-- Create a list of offset corrections.
genUTFOfs :: String -> UTFCorrection
genUTFOfs str = UTFCorrection (gUO 0 str)
  where
  gUO n [] = []
  gUO n (x:xs) | ord x<=0x007F = gUO (n+1) xs
	       | ord x<=0x07FF = n:gUO (n+1) xs
	       | otherwise     = n:n:gUO (n+1) xs

ofsToUTF :: Int -> UTFCorrection -> Int
ofsToUTF n (UTFCorrection oc) = oTU oc
  where
  oTU [] = n
  oTU (x:xs) | n<=x = n
	     | otherwise = 1+oTU xs

ofsFromUTF :: Int -> UTFCorrection -> Int
ofsFromUTF n (UTFCorrection oc) = oFU n oc
  where
  oFU n [] = n
  oFU n (x:xs) | n<=x = n
	       | otherwise = oFU (n-1) xs 
