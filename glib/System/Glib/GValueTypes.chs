-- -*-haskell-*-
--  GIMP Toolkit (GTK) GValueTypes
--
--  Author : Axel Simon
--
--  Created: 1 June 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/05/07 18:58:18 $
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
-- This is used by the implementation of properties and by the 'TreeModel' and
-- related modules.
--
module System.Glib.GValueTypes (
  valueSetUInt,
  valueGetUInt,
  valueSetInt,
  valueGetInt,
--  valueSetUChar,
--  valueGetUChar,
--  valueSetChar,
--  valueGetChar,
  valueSetBool,
  valueGetBool,
  valueSetPointer,
  valueGetPointer,
  valueSetFloat,
  valueGetFloat,
  valueSetDouble,
  valueGetDouble,
  valueSetString,
  valueGetString,
  valueSetMaybeString,
  valueGetMaybeString,
  valueSetGObject,
  valueGetGObject,
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import qualified System.Glib.GTypeConstants as GType
{#import System.Glib.GValue#}		(GValue(GValue), valueInit)
import System.Glib.GObject

{# context lib="glib" prefix="g" #}

valueSetUInt :: GValue -> Word -> IO ()
valueSetUInt gvalue value = do 
  valueInit gvalue GType.uint
  {# call unsafe value_set_uint #} gvalue (fromIntegral value)

valueGetUInt :: GValue -> IO Word
valueGetUInt gvalue =
  liftM fromIntegral $
  {# call unsafe value_get_uint #} gvalue

valueSetInt :: GValue -> Int -> IO ()
valueSetInt gvalue value = do 
  valueInit gvalue GType.int
  {# call unsafe value_set_int #} gvalue (fromIntegral value)

valueGetInt :: GValue -> IO Int
valueGetInt gvalue =
  liftM fromIntegral $
  {# call unsafe value_get_int #} gvalue

{-
valueSetUChar :: GValue -> Word8 -> IO ()
valueSetUChar gvalue value = do 
  valueInit gvalue GType.uchar
  {# call unsafe value_set_uchar #} gvalue value

valueGetUChar :: GValue -> IO Word8
valueGetUChar gvalue =
  {# call unsafe value_get_uchar #} gvalue

valueSetChar :: GValue -> {#type gchar#} -> IO ()
valueSetChar gvalue value = do 
  valueInit gvalue GType.char
  {# call unsafe value_set_char #} gvalue value

valueGetChar :: GValue -> IO {#type gchar#}
valueGetChar gvalue =
  {# call unsafe value_get_char #} gvalue
-}

valueSetBool :: GValue -> Bool -> IO ()
valueSetBool gvalue value = do 
  valueInit gvalue GType.bool
  {# call unsafe value_set_boolean #} gvalue (fromBool value)

valueGetBool :: GValue -> IO Bool
valueGetBool gvalue =
  liftM toBool $
  {# call  unsafe value_get_boolean #} gvalue

-- These functions should probably never be used as they are dangerous.
--
valueSetPointer :: GValue -> (Ptr ()) -> IO ()
valueSetPointer gvalue value = do 
  valueInit gvalue GType.pointer
  {# call unsafe value_set_pointer #} gvalue value

valueGetPointer :: GValue -> IO (Ptr ())
valueGetPointer gvalue =
  {# call unsafe value_get_pointer #} gvalue

valueSetFloat :: GValue -> Float -> IO ()
valueSetFloat gvalue value = do 
  valueInit gvalue GType.float
  {# call unsafe value_set_float #} gvalue (realToFrac value)

valueGetFloat :: GValue -> IO Float
valueGetFloat gvalue =
  liftM realToFrac $
  {# call unsafe value_get_float #} gvalue

valueSetDouble :: GValue -> Double -> IO ()
valueSetDouble gvalue value = do
  valueInit gvalue GType.double
  {# call unsafe value_set_double #} gvalue (realToFrac value)

valueGetDouble :: GValue -> IO Double
valueGetDouble gvalue =
  liftM realToFrac $
  {# call unsafe value_get_double #} gvalue

valueSetString :: GValue -> String -> IO ()
valueSetString gvalue str = do
  valueInit gvalue GType.string
  strPtr <- newUTFString str
  {# call unsafe value_set_static_string #} gvalue strPtr

valueGetString :: GValue -> IO String
valueGetString gvalue = do
  strPtr <- {# call unsafe value_get_string #} gvalue
  if strPtr == nullPtr
    then return ""
    else peekUTFString strPtr

valueSetMaybeString :: GValue -> Maybe String -> IO ()
valueSetMaybeString gvalue (Just str) = do
  valueInit gvalue GType.string
  strPtr <- newUTFString str
  {# call unsafe value_set_static_string #} gvalue strPtr

valueSetMaybeString gvalue Nothing = do
  valueInit gvalue GType.string
  {# call unsafe value_set_static_string #} gvalue nullPtr

valueGetMaybeString :: GValue -> IO (Maybe String)
valueGetMaybeString gvalue =
  {# call unsafe value_get_string #} gvalue
  >>= maybePeek peekUTFString

-- for some weird reason the API says that gv is a gpointer, not a GObject
--
valueSetGObject :: GObjectClass gobj => GValue -> gobj -> IO ()
valueSetGObject gvalue obj =
  withForeignPtr ((unGObject.toGObject) obj) $ \objPtr ->
  {# call unsafe g_value_set_object #} gvalue (castPtr objPtr)

-- Unsafe because it performs an unchecked downcast. Only for internal use.
--
valueGetGObject :: GObjectClass gobj => GValue -> IO gobj
valueGetGObject gvalue =
  liftM fromGObject $
  makeNewGObject mkGObject $
  throwIfNull "GType.valueGetObject: extracting invalid object" $
  liftM castPtr $
  {# call unsafe value_get_object #} gvalue
