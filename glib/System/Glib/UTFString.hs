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

import Codec.Binary.UTF8.String
import Control.Monad (liftM)
import Data.Char (ord, chr)
import Data.Maybe (maybe)

import System.Glib.FFI

-- | Like 'withCString' but using the UTF-8 encoding.
--
withUTFString :: String -> (CString -> IO a) -> IO a
withUTFString = withCAString . encodeString

-- | Like 'withCStringLen' but using the UTF-8 encoding.
--
withUTFStringLen :: String -> (CStringLen -> IO a) -> IO a
withUTFStringLen = withCAStringLen . encodeString

-- | Like 'newCString' but using the UTF-8 encoding.
--
newUTFString :: String -> IO CString
newUTFString = newCAString . encodeString

-- | Like  Define newUTFStringLen to emit UTF-8.
--
newUTFStringLen :: String -> IO CStringLen
newUTFStringLen = newCAStringLen . encodeString

-- | Like 'peekCString' but using the UTF-8 encoding.
--
peekUTFString :: CString -> IO String
peekUTFString = liftM decodeString . peekCAString

-- | Like 'maybePeek' 'peekCString' but using the UTF-8 encoding to retrieve
-- UTF-8 from a 'CString' which may be the 'nullPtr'.
--
maybePeekUTFString :: CString -> IO (Maybe String)
maybePeekUTFString = liftM (maybe Nothing (Just . decodeString)) . maybePeek peekCAString

-- | Like 'peekCStringLen' but using the UTF-8 encoding.
--
peekUTFStringLen :: CStringLen -> IO String
peekUTFStringLen = liftM decodeString . peekCAStringLen

-- | Like like 'peekUTFString' but then frees the string using g_free
--
readUTFString :: CString -> IO String
readUTFString strPtr = do
  str <- peekUTFString strPtr
  g_free strPtr
  return str

-- | Like 'peekCString' but then frees the string using @g_free@.
--
readCString :: CString -> IO String
readCString strPtr = do
  str <- peekCAString strPtr
  g_free strPtr
  return str

foreign import ccall unsafe "g_free"
  g_free :: Ptr a -> IO ()

-- | Temporarily allocate a list of UTF-8 'CString's.
--
withUTFStrings :: [String] -> ([CString] -> IO a) -> IO a
withUTFStrings hsStrs = withUTFStrings' hsStrs []
  where withUTFStrings' :: [String] -> [CString] -> ([CString] -> IO a) -> IO a
        withUTFStrings' []     cs body = body (reverse cs)
        withUTFStrings' (s:ss) cs body = withUTFString s $ \c ->
                                         withUTFStrings' ss (c:cs) body

-- | Temporarily allocate an array of UTF-8 encoded 'CString's.
--
withUTFStringArray :: [String] -> (Ptr CString -> IO a) -> IO a
withUTFStringArray hsStr body =
  withUTFStrings hsStr $ \cStrs -> do
  withArray cStrs body

-- | Temporarily allocate a null-terminated array of UTF-8 encoded 'CString's.
--
withUTFStringArray0 :: [String] -> (Ptr CString -> IO a) -> IO a
withUTFStringArray0 hsStr body =
  withUTFStrings hsStr $ \cStrs -> do
  withArray0 nullPtr cStrs body

-- | Convert an array (of the given length) of UTF-8 encoded 'CString's to a
--   list of Haskell 'String's.
--
peekUTFStringArray :: Int -> Ptr CString -> IO [String]
peekUTFStringArray len cStrArr = do
  cStrs <- peekArray len cStrArr
  mapM peekUTFString cStrs

-- | Convert a null-terminated array of UTF-8 encoded 'CString's to a list of
--   Haskell 'String's.
--
peekUTFStringArray0 :: Ptr CString -> IO [String]
peekUTFStringArray0 cStrArr = do
  cStrs <- peekArray0 nullPtr cStrArr
  mapM peekUTFString cStrs

-- | Like 'peekUTFStringArray0' but then free the string array including all
-- strings.
--
-- To be used when functions indicate that their return value should be freed
-- with @g_strfreev@.
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

-- | Offset correction for String to UTF8 mapping.
--
newtype UTFCorrection = UTFCorrection [Int] deriving Show

-- | Create a list of offset corrections.
--
genUTFOfs :: String -> UTFCorrection
genUTFOfs str = UTFCorrection (gUO 0 str)
  where
  gUO n [] = []
  gUO n (x:xs) | ord x<=0x007F = gUO (n+1) xs
	       | ord x<=0x07FF = n:gUO (n+1) xs
	       | ord x<=0xFFFF = n:n:gUO (n+1) xs
	       | otherwise     = n:n:n:gUO (n+1) xs

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
