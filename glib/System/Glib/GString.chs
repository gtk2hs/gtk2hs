-- -*-haskell-*-
--  GIMP Toolkit (GTK)
--
--  Author : Andreas Baldeau
--
--  Created: 14 November 2010
--
--  Copyright (C) 2010 Andreas Baldeau
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
-- Defines functions to extract data from a GString.
--
module System.Glib.GString (
  GString,
  readGString,
  readGStringByteString,
  fromGString,
  ) where

import Foreign
import Control.Exception        (bracket)
import Control.Monad            (foldM)
import Data.ByteString          (ByteString, packCStringLen)

import System.Glib.FFI

{# context lib="glib" prefix="g" #}

{#pointer * GString#}

-- methods

-- Turn a GString into a String but don't destroy it.
--
readGString :: GString -> IO (Maybe String)
readGString gstring
  | gstring == nullPtr = return Nothing
  | otherwise          = do
    gstr <- {#get GString->str#} gstring
    len <- {#get GString->len#} gstring
    fmap Just $ peekCStringLen (gstr, fromIntegral len)

-- Turn a GString into a ByteString but don't destroy it.
--
readGStringByteString :: GString -> IO (Maybe ByteString)
readGStringByteString gstring
  | gstring == nullPtr = return Nothing
  | otherwise          = do
    gstr <- {#get GString->str#} gstring
    len <- {#get GString->len#} gstring
    fmap Just $ packCStringLen (gstr, fromIntegral len)

-- Turn a GList into a list of pointers (freeing the GList in the process).
--
fromGString :: GString -> IO (Maybe String)
fromGString gstring
  | gstring == nullPtr = return Nothing
  | otherwise          = do
    gstr <- {#get GString->str#} gstring
    len <- {#get GString->len#} gstring
    str <- fmap Just $ peekCStringLen (gstr, fromIntegral len)
    _ <- {#call unsafe string_free#} gstring $ fromBool True
    return str

