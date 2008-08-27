-- -*-haskell-*-
--  GIMP Toolkit (GTK) GObject Properties
--
--  Author : Duncan Coutts
--
--  Created: 16 April 2005
--
--  Copyright (C) 2005 Duncan Coutts
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
-- Functions for getting and setting GObject properties
--
module System.Glib.Properties (
  -- * per-type functions for getting and setting GObject properties
  objectSetPropertyInt,
  objectGetPropertyInt,
  objectSetPropertyUInt,
  objectGetPropertyUInt,
  objectSetPropertyInt64,
  objectGetPropertyInt64,
  objectSetPropertyUInt64,
  objectGetPropertyUInt64,
  objectSetPropertyChar,
  objectGetPropertyChar,
  objectSetPropertyBool,
  objectGetPropertyBool,
  objectSetPropertyEnum,
  objectGetPropertyEnum,
  objectSetPropertyFlags,
  objectGetPropertyFlags,
  objectSetPropertyFloat,
  objectGetPropertyFloat,
  objectSetPropertyDouble,
  objectGetPropertyDouble,
  objectSetPropertyString,
  objectGetPropertyString,
  objectSetPropertyMaybeString,
  objectGetPropertyMaybeString,  
  objectSetPropertyBoxedOpaque,
  objectGetPropertyBoxedOpaque,
  objectSetPropertyBoxedStorable,
  objectGetPropertyBoxedStorable,
  objectSetPropertyGObject,
  objectGetPropertyGObject,

  -- * constructors for attributes backed by GObject properties
  newAttrFromIntProperty,
  readAttrFromIntProperty,
  newAttrFromUIntProperty,
  newAttrFromCharProperty,
  writeAttrFromUIntProperty,
  newAttrFromBoolProperty,
  readAttrFromBoolProperty,
  newAttrFromFloatProperty,
  newAttrFromDoubleProperty,
  newAttrFromEnumProperty,
  readAttrFromEnumProperty,
  writeAttrFromEnumProperty,
  newAttrFromFlagsProperty,
  newAttrFromStringProperty,
  readAttrFromStringProperty,
  writeAttrFromStringProperty,
  writeAttrFromMaybeStringProperty,
  newAttrFromMaybeStringProperty,
  newAttrFromBoxedOpaqueProperty,
  writeAttrFromBoxedOpaqueProperty,
  newAttrFromBoxedStorableProperty,
  newAttrFromObjectProperty,
  writeAttrFromObjectProperty,
  newAttrFromMaybeObjectProperty,
  
  -- TODO: do not export these once we dump the old TreeList API:
  objectGetPropertyInternal,
  objectSetPropertyInternal,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Flags	(Flags)
{#import System.Glib.Types#}
{#import System.Glib.GValue#}	(GValue(GValue), valueInit, allocaGValue)
import qualified System.Glib.GTypeConstants as GType
import System.Glib.GType
import System.Glib.GValueTypes
import System.Glib.Attributes	(Attr, ReadAttr, WriteAttr, ReadWriteAttr,
				newAttr, readAttr, writeAttr)

{# context lib="glib" prefix="g" #}

objectSetPropertyInternal :: GObjectClass gobj => GType -> (GValue -> a -> IO ()) -> String -> gobj -> a -> IO ()
objectSetPropertyInternal gtype valueSet prop obj val =
  withCString prop $ \propPtr ->
  allocaGValue $ \gvalue -> do
  valueInit gvalue gtype
  valueSet gvalue val
  {# call g_object_set_property #}
    (toGObject obj)
    propPtr
    gvalue

objectGetPropertyInternal :: GObjectClass gobj => GType -> (GValue -> IO a) -> String -> gobj -> IO a
objectGetPropertyInternal gtype valueGet prop obj =
  withCString prop $ \propPtr ->
  allocaGValue $ \gvalue -> do
  valueInit gvalue gtype
  {# call unsafe g_object_get_property #}
    (toGObject obj)
    propPtr
    gvalue
  valueGet gvalue

objectSetPropertyInt :: GObjectClass gobj => String -> gobj -> Int -> IO ()
objectSetPropertyInt = objectSetPropertyInternal GType.int valueSetInt

objectGetPropertyInt :: GObjectClass gobj => String -> gobj -> IO Int
objectGetPropertyInt = objectGetPropertyInternal GType.int valueGetInt

objectSetPropertyUInt :: GObjectClass gobj => String -> gobj -> Int -> IO ()
objectSetPropertyUInt = objectSetPropertyInternal GType.uint (\gv v -> valueSetUInt gv (fromIntegral v))

objectGetPropertyUInt :: GObjectClass gobj => String -> gobj -> IO Int
objectGetPropertyUInt = objectGetPropertyInternal GType.uint (\gv -> liftM fromIntegral $ valueGetUInt gv)

objectSetPropertyInt64 :: GObjectClass gobj => String -> gobj -> Int64 -> IO ()
objectSetPropertyInt64 = objectSetPropertyInternal GType.int64 valueSetInt64

objectGetPropertyInt64 :: GObjectClass gobj => String -> gobj -> IO Int64
objectGetPropertyInt64 = objectGetPropertyInternal GType.int64 valueGetInt64

objectSetPropertyUInt64 :: GObjectClass gobj => String -> gobj -> Word64 -> IO ()
objectSetPropertyUInt64 = objectSetPropertyInternal GType.uint64 (\gv v -> valueSetUInt64 gv (fromIntegral v))

objectGetPropertyUInt64 :: GObjectClass gobj => String -> gobj -> IO Word64
objectGetPropertyUInt64 = objectGetPropertyInternal GType.uint64 (\gv -> liftM fromIntegral $ valueGetUInt64 gv)

objectSetPropertyChar :: GObjectClass gobj => String -> gobj -> Char -> IO ()
objectSetPropertyChar = objectSetPropertyInternal GType.uint (\gv v -> valueSetUInt gv (fromIntegral (fromEnum v)))

objectGetPropertyChar :: GObjectClass gobj => String -> gobj -> IO Char
objectGetPropertyChar = objectGetPropertyInternal GType.uint (\gv -> liftM (toEnum . fromIntegral) $ valueGetUInt gv)

objectSetPropertyBool :: GObjectClass gobj => String -> gobj -> Bool -> IO ()
objectSetPropertyBool = objectSetPropertyInternal GType.bool valueSetBool

objectGetPropertyBool :: GObjectClass gobj => String -> gobj -> IO Bool
objectGetPropertyBool = objectGetPropertyInternal GType.bool valueGetBool

objectSetPropertyEnum :: (GObjectClass gobj, Enum enum) => GType -> String -> gobj -> enum -> IO ()
objectSetPropertyEnum gtype = objectSetPropertyInternal gtype valueSetEnum

objectGetPropertyEnum :: (GObjectClass gobj, Enum enum) => GType -> String -> gobj -> IO enum
objectGetPropertyEnum gtype = objectGetPropertyInternal gtype valueGetEnum

objectSetPropertyFlags :: (GObjectClass gobj, Flags flag) => GType -> String -> gobj -> [flag] -> IO ()
objectSetPropertyFlags gtype = objectSetPropertyInternal gtype valueSetFlags

objectGetPropertyFlags :: (GObjectClass gobj, Flags flag) => GType -> String -> gobj -> IO [flag]
objectGetPropertyFlags gtype = objectGetPropertyInternal gtype valueGetFlags

objectSetPropertyFloat :: GObjectClass gobj => String -> gobj -> Float -> IO ()
objectSetPropertyFloat = objectSetPropertyInternal GType.float valueSetFloat

objectGetPropertyFloat :: GObjectClass gobj => String -> gobj -> IO Float
objectGetPropertyFloat = objectGetPropertyInternal GType.float valueGetFloat

objectSetPropertyDouble :: GObjectClass gobj => String -> gobj -> Double -> IO ()
objectSetPropertyDouble = objectSetPropertyInternal GType.double valueSetDouble

objectGetPropertyDouble :: GObjectClass gobj => String -> gobj -> IO Double
objectGetPropertyDouble = objectGetPropertyInternal GType.double valueGetDouble

objectSetPropertyString :: GObjectClass gobj => String -> gobj -> String -> IO ()
objectSetPropertyString = objectSetPropertyInternal GType.string valueSetString

objectGetPropertyString :: GObjectClass gobj => String -> gobj -> IO String
objectGetPropertyString = objectGetPropertyInternal GType.string valueGetString

objectSetPropertyMaybeString :: GObjectClass gobj => String -> gobj -> Maybe String -> IO ()
objectSetPropertyMaybeString = objectSetPropertyInternal GType.string valueSetMaybeString

objectGetPropertyMaybeString :: GObjectClass gobj => String -> gobj -> IO (Maybe String)
objectGetPropertyMaybeString = objectGetPropertyInternal GType.string valueGetMaybeString

objectSetPropertyBoxedOpaque :: GObjectClass gobj => (boxed -> (Ptr boxed -> IO ()) -> IO ()) -> GType -> String -> gobj -> boxed -> IO ()
objectSetPropertyBoxedOpaque with gtype = objectSetPropertyInternal gtype (valueSetBoxed with)

objectGetPropertyBoxedOpaque :: GObjectClass gobj => (Ptr boxed -> IO boxed) -> GType -> String -> gobj -> IO boxed
objectGetPropertyBoxedOpaque peek gtype = objectGetPropertyInternal gtype (valueGetBoxed peek)

objectSetPropertyBoxedStorable :: (GObjectClass gobj, Storable boxed) => GType -> String -> gobj -> boxed -> IO ()
objectSetPropertyBoxedStorable = objectSetPropertyBoxedOpaque with

objectGetPropertyBoxedStorable :: (GObjectClass gobj, Storable boxed) => GType -> String -> gobj -> IO boxed
objectGetPropertyBoxedStorable = objectGetPropertyBoxedOpaque peek

objectSetPropertyGObject :: (GObjectClass gobj, GObjectClass gobj') => GType -> String -> gobj -> gobj' -> IO ()
objectSetPropertyGObject gtype = objectSetPropertyInternal gtype valueSetGObject

objectGetPropertyGObject :: (GObjectClass gobj, GObjectClass gobj') => GType -> String -> gobj -> IO gobj'
objectGetPropertyGObject gtype = objectGetPropertyInternal gtype valueGetGObject

objectSetPropertyMaybeGObject :: (GObjectClass gobj, GObjectClass gobj') => GType -> String -> gobj -> (Maybe gobj') -> IO ()
objectSetPropertyMaybeGObject gtype = objectSetPropertyInternal gtype valueSetMaybeGObject

objectGetPropertyMaybeGObject :: (GObjectClass gobj, GObjectClass gobj') => GType -> String -> gobj -> IO (Maybe gobj')
objectGetPropertyMaybeGObject gtype = objectGetPropertyInternal gtype valueGetMaybeGObject


-- Convenience functions to make attribute implementations in the other modules
-- shorter and more easily extensible.
--

newAttrFromIntProperty :: GObjectClass gobj => String -> Attr gobj Int
newAttrFromIntProperty propName =
  newAttr (objectGetPropertyInt propName) (objectSetPropertyInt propName)

readAttrFromIntProperty :: GObjectClass gobj => String -> ReadAttr gobj Int
readAttrFromIntProperty propName =
  readAttr (objectGetPropertyInt propName)

newAttrFromUIntProperty :: GObjectClass gobj => String -> Attr gobj Int
newAttrFromUIntProperty propName =
  newAttr (objectGetPropertyUInt propName) (objectSetPropertyUInt propName)

newAttrFromCharProperty :: GObjectClass gobj => String -> Attr gobj Char
newAttrFromCharProperty propName =
  newAttr (objectGetPropertyChar propName) (objectSetPropertyChar propName)

writeAttrFromUIntProperty :: GObjectClass gobj => String -> WriteAttr gobj Int
writeAttrFromUIntProperty propName =
  writeAttr (objectSetPropertyUInt propName)

newAttrFromBoolProperty :: GObjectClass gobj => String -> Attr gobj Bool
newAttrFromBoolProperty propName =
  newAttr (objectGetPropertyBool propName) (objectSetPropertyBool propName)

readAttrFromBoolProperty :: GObjectClass gobj => String -> ReadAttr gobj Bool
readAttrFromBoolProperty propName =
  readAttr (objectGetPropertyBool propName)

newAttrFromFloatProperty :: GObjectClass gobj => String -> Attr gobj Float
newAttrFromFloatProperty propName =
  newAttr (objectGetPropertyFloat propName) (objectSetPropertyFloat propName)

newAttrFromDoubleProperty :: GObjectClass gobj => String -> Attr gobj Double
newAttrFromDoubleProperty propName =
  newAttr (objectGetPropertyDouble propName) (objectSetPropertyDouble propName)

newAttrFromEnumProperty :: (GObjectClass gobj, Enum enum) => String -> GType -> Attr gobj enum
newAttrFromEnumProperty propName gtype =
  newAttr (objectGetPropertyEnum gtype propName) (objectSetPropertyEnum gtype propName)

readAttrFromEnumProperty :: (GObjectClass gobj, Enum enum) => String -> GType -> ReadAttr gobj enum
readAttrFromEnumProperty propName gtype =
  readAttr (objectGetPropertyEnum gtype propName)

writeAttrFromEnumProperty :: (GObjectClass gobj, Enum enum) => String -> GType -> WriteAttr gobj enum
writeAttrFromEnumProperty propName gtype =
  writeAttr (objectSetPropertyEnum gtype propName)

newAttrFromFlagsProperty :: (GObjectClass gobj, Flags flag) => String -> GType -> Attr gobj [flag]
newAttrFromFlagsProperty propName gtype =
  newAttr (objectGetPropertyFlags gtype propName) (objectSetPropertyFlags gtype propName)

newAttrFromStringProperty :: GObjectClass gobj => String -> Attr gobj String
newAttrFromStringProperty propName =
  newAttr (objectGetPropertyString propName) (objectSetPropertyString propName)

readAttrFromStringProperty :: GObjectClass gobj => String -> ReadAttr gobj String
readAttrFromStringProperty propName =
  readAttr (objectGetPropertyString propName)

writeAttrFromStringProperty :: GObjectClass gobj => String -> WriteAttr gobj String
writeAttrFromStringProperty propName =
  writeAttr (objectSetPropertyString propName)

writeAttrFromMaybeStringProperty :: GObjectClass gobj => String -> WriteAttr gobj (Maybe String)
writeAttrFromMaybeStringProperty propName =
  writeAttr (objectSetPropertyMaybeString propName)

newAttrFromMaybeStringProperty :: GObjectClass gobj => String -> Attr gobj (Maybe String)
newAttrFromMaybeStringProperty propName =
  newAttr (objectGetPropertyMaybeString propName) (objectSetPropertyMaybeString propName)

newAttrFromBoxedOpaqueProperty :: GObjectClass gobj => (Ptr boxed -> IO boxed) -> (boxed -> (Ptr boxed -> IO ()) -> IO ()) -> String -> GType -> Attr gobj boxed
newAttrFromBoxedOpaqueProperty peek with propName gtype =
  newAttr (objectGetPropertyBoxedOpaque peek gtype propName) (objectSetPropertyBoxedOpaque with gtype propName)

writeAttrFromBoxedOpaqueProperty :: GObjectClass gobj => (boxed -> (Ptr boxed -> IO ()) -> IO ()) -> String -> GType -> WriteAttr gobj boxed
writeAttrFromBoxedOpaqueProperty with propName gtype =
  writeAttr (objectSetPropertyBoxedOpaque with gtype propName)

newAttrFromBoxedStorableProperty :: (GObjectClass gobj, Storable boxed) => String -> GType -> Attr gobj boxed
newAttrFromBoxedStorableProperty propName gtype =
  newAttr (objectGetPropertyBoxedStorable gtype propName) (objectSetPropertyBoxedStorable gtype propName)

newAttrFromObjectProperty :: (GObjectClass gobj, GObjectClass gobj', GObjectClass gobj'') => String -> GType -> ReadWriteAttr gobj gobj' gobj''
newAttrFromObjectProperty propName gtype =
  newAttr (objectGetPropertyGObject gtype propName) (objectSetPropertyGObject gtype propName)

newAttrFromMaybeObjectProperty :: (GObjectClass gobj, GObjectClass gobj', GObjectClass gobj'') => String -> GType -> ReadWriteAttr gobj (Maybe gobj') (Maybe gobj'')
newAttrFromMaybeObjectProperty propName gtype =
  newAttr (objectGetPropertyMaybeGObject gtype propName) (objectSetPropertyMaybeGObject gtype propName)
 
writeAttrFromObjectProperty :: (GObjectClass gobj, GObjectClass gobj') => String -> GType -> WriteAttr gobj gobj'
writeAttrFromObjectProperty propName gtype =
  writeAttr (objectSetPropertyGObject gtype propName)
