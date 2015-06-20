{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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
  GlibString(..),
  readUTFString,
  readCString,
  withUTFStrings,
  withUTFStringArray,
  withUTFStringArray0,
  peekUTFStringArray,
  peekUTFStringArray0,
  readUTFStringArray0,
  UTFCorrection,
  ofsToUTF,
  ofsFromUTF,

  glibToString,
  stringToGlib,

  DefaultGlibString,

  GlibFilePath(..),
  withUTFFilePaths,
  withUTFFilePathArray,
  withUTFFilePathArray0,
  peekUTFFilePathArray0,
  readUTFFilePathArray0
  ) where

import Codec.Binary.UTF8.String
import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.Char (ord, chr)
import Data.Maybe (maybe)
import Data.String (IsString)
import Data.Monoid (Monoid)
import System.Glib.FFI
import qualified Data.Text as T (replace, length, pack, unpack, Text)
import qualified Data.Text.Foreign as T
       (withCStringLen, peekCStringLen)
import Data.ByteString (useAsCString)
import Data.Text.Encoding (encodeUtf8)

class (IsString s, Monoid s, Show s) => GlibString s where
    -- | Like 'withCString' but using the UTF-8 encoding.
    --
    withUTFString :: s -> (CString -> IO a) -> IO a

    -- | Like 'withCStringLen' but using the UTF-8 encoding.
    --
    withUTFStringLen :: s -> (CStringLen -> IO a) -> IO a

    -- | Like 'peekCString' but using the UTF-8 encoding.
    --
    peekUTFString :: CString -> IO s

    -- | Like 'maybePeek' 'peekCString' but using the UTF-8 encoding to retrieve
    -- UTF-8 from a 'CString' which may be the 'nullPtr'.
    --
    maybePeekUTFString :: CString -> IO (Maybe s)

    -- | Like 'peekCStringLen' but using the UTF-8 encoding.
    --
    peekUTFStringLen :: CStringLen -> IO s

    -- | Like 'newCString' but using the UTF-8 encoding.
    --
    newUTFString :: s -> IO CString

    -- | Like  Define newUTFStringLen to emit UTF-8.
    --
    newUTFStringLen :: s -> IO CStringLen

    -- | Create a list of offset corrections.
    --
    genUTFOfs :: s -> UTFCorrection

    -- | Length of the string in characters
    --
    stringLength :: s -> Int

    -- Escape percent signs (used in MessageDialog)
    unPrintf :: s -> s

-- GTK+ has a lot of asserts that the ptr is not NULL even if the length is 0
-- Until they fix this we need to fudge pointer values to keep the noise level
-- in the logs.
noNullPtrs :: CStringLen -> CStringLen
noNullPtrs (p, 0) | p == nullPtr = (plusPtr p 1, 0)
noNullPtrs s = s

instance GlibString [Char] where
    withUTFString = withCAString . encodeString
    withUTFStringLen s f = withCAStringLen (encodeString s) (f . noNullPtrs)
    peekUTFString = liftM decodeString . peekCAString
    maybePeekUTFString = liftM (maybe Nothing (Just . decodeString)) . maybePeek peekCAString
    peekUTFStringLen = liftM decodeString . peekCAStringLen
    newUTFString = newCAString . encodeString
    newUTFStringLen = newCAStringLen . encodeString
    genUTFOfs str = UTFCorrection (gUO 0 str)
      where
      gUO n [] = []
      gUO n (x:xs) | ord x<=0x007F = gUO (n+1) xs
                   | ord x<=0x07FF = n:gUO (n+1) xs
                   | ord x<=0xFFFF = n:n:gUO (n+1) xs
                   | otherwise     = n:n:n:gUO (n+1) xs
    stringLength = length
    unPrintf s = s >>= replace
        where
            replace '%' = "%%"
            replace c = return c

foreign import ccall unsafe "string.h strlen" c_strlen
    :: CString -> IO CSize

instance GlibString T.Text where
    withUTFString = useAsCString . encodeUtf8
    withUTFStringLen s f = T.withCStringLen s (f . noNullPtrs)
    peekUTFString s = do
        len <- c_strlen s
        T.peekCStringLen (s, fromIntegral len)
    maybePeekUTFString = maybePeek peekUTFString
    peekUTFStringLen = T.peekCStringLen
    newUTFString = newUTFString . T.unpack -- TODO optimize
    newUTFStringLen = newUTFStringLen . T.unpack -- TODO optimize
    genUTFOfs = genUTFOfs . T.unpack -- TODO optimize
    stringLength = T.length
    unPrintf = T.replace "%" "%%"

glibToString :: T.Text -> String
glibToString = T.unpack

stringToGlib :: String -> T.Text
stringToGlib = T.pack

-- | Like like 'peekUTFString' but then frees the string using g_free
--
readUTFString :: GlibString s => CString -> IO s
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
withUTFStrings :: GlibString s => [s] -> ([CString] -> IO a) -> IO a
withUTFStrings hsStrs = withUTFStrings' hsStrs []
  where withUTFStrings' :: GlibString s => [s] -> [CString] -> ([CString] -> IO a) -> IO a
        withUTFStrings' []     cs body = body (reverse cs)
        withUTFStrings' (s:ss) cs body = withUTFString s $ \c ->
                                         withUTFStrings' ss (c:cs) body

-- | Temporarily allocate an array of UTF-8 encoded 'CString's.
--
withUTFStringArray :: GlibString s => [s] -> (Ptr CString -> IO a) -> IO a
withUTFStringArray hsStr body =
  withUTFStrings hsStr $ \cStrs -> do
  withArray cStrs body

-- | Temporarily allocate a null-terminated array of UTF-8 encoded 'CString's.
--
withUTFStringArray0 :: GlibString s => [s] -> (Ptr CString -> IO a) -> IO a
withUTFStringArray0 hsStr body =
  withUTFStrings hsStr $ \cStrs -> do
  withArray0 nullPtr cStrs body

-- | Convert an array (of the given length) of UTF-8 encoded 'CString's to a
--   list of Haskell 'String's.
--
peekUTFStringArray :: GlibString s => Int -> Ptr CString -> IO [s]
peekUTFStringArray len cStrArr = do
  cStrs <- peekArray len cStrArr
  mapM peekUTFString cStrs

-- | Convert a null-terminated array of UTF-8 encoded 'CString's to a list of
--   Haskell 'String's.
--
peekUTFStringArray0 :: GlibString s => Ptr CString -> IO [s]
peekUTFStringArray0 cStrArr = do
  cStrs <- peekArray0 nullPtr cStrArr
  mapM peekUTFString cStrs

-- | Like 'peekUTFStringArray0' but then free the string array including all
-- strings.
--
-- To be used when functions indicate that their return value should be freed
-- with @g_strfreev@.
--
readUTFStringArray0 :: GlibString s => Ptr CString -> IO [s]
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

type DefaultGlibString = T.Text

class fp ~ FilePath => GlibFilePath fp where
    withUTFFilePath :: fp -> (CString -> IO a) -> IO a
    peekUTFFilePath :: CString -> IO fp

instance GlibFilePath FilePath where
    withUTFFilePath = withUTFString . T.pack
    peekUTFFilePath f = T.unpack <$> peekUTFString f

withUTFFilePaths :: GlibFilePath fp => [fp] -> ([CString] -> IO a) -> IO a
withUTFFilePaths hsStrs = withUTFFilePath' hsStrs []
  where withUTFFilePath' :: GlibFilePath fp => [fp] -> [CString] -> ([CString] -> IO a) -> IO a
        withUTFFilePath' []       cs body = body (reverse cs)
        withUTFFilePath' (fp:fps) cs body = withUTFFilePath fp $ \c ->
                                            withUTFFilePath' fps (c:cs) body

withUTFFilePathArray :: GlibFilePath fp => [fp] -> (Ptr CString -> IO a) -> IO a
withUTFFilePathArray hsFP body =
  withUTFFilePaths hsFP $ \cStrs -> do
  withArray cStrs body

withUTFFilePathArray0 :: GlibFilePath fp => [fp] -> (Ptr CString -> IO a) -> IO a
withUTFFilePathArray0 hsFP body =
  withUTFFilePaths hsFP $ \cStrs -> do
  withArray0 nullPtr cStrs body

peekUTFFilePathArray0 :: GlibFilePath fp => Ptr CString -> IO [fp]
peekUTFFilePathArray0 cStrArr = do
  cStrs <- peekArray0 nullPtr cStrArr
  mapM peekUTFFilePath cStrs

readUTFFilePathArray0 :: GlibFilePath fp => Ptr CString -> IO [fp]
readUTFFilePathArray0 cStrArr | cStrArr == nullPtr = return []
                              | otherwise = do
  cStrs <- peekArray0 nullPtr cStrArr
  fps <- mapM peekUTFFilePath cStrs
  g_strfreev cStrArr
  return fps
