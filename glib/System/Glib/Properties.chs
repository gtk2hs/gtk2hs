-- -*-haskell-*-
--  GIMP Toolkit (GTK) GObject Properties
--
--  Author : Duncan Coutts
--
--  Created: 16 April 2005
--
--  Version $Revision: 1.3 $ from $Date: 2005/05/08 03:21:12 $
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
  objectSetPropertyGObject,
  objectGetPropertyGObject,
  
  objectSetPropertyInternal,
  objectGetPropertyInternal,

  -- * constructors for attributes backed by GObject properties
  newAttrFromIntProperty,
  readAttrFromIntProperty,
  newAttrFromUIntProperty,
  writeAttrFromUIntProperty,
  newAttrFromBoolProperty,
  newAttrFromFloatProperty,
  newAttrFromDoubleProperty,
  newAttrFromEnumProperty,
  readAttrFromEnumProperty,
  newAttrFromFlagsProperty,
  newAttrFromStringProperty,
  writeAttrFromStringProperty,
  newAttrFromMaybeStringProperty,
  newAttrFromObjectProperty,
  writeAttrFromObjectProperty,
  ) where

import Monad (liftM)

import System.Glib.FFI
import System.Glib.Flags	(Flags, fromFlags, toFlags)
import System.Glib.UTFString
{#import System.Glib.Types#}
{#import System.Glib.GValue#}	(GValue(GValue), valueInit, allocaGValue)
import System.Glib.GObject	(makeNewGObject)
import System.Glib.GValueTypes
import System.Glib.Attributes	(Attr, ReadAttr, WriteAttr, ReadWriteAttr,
				newAttr, readAttr, writeAttr)

{# context lib="glib" prefix="g" #}

objectSetPropertyInternal :: GObjectClass gobj => (GValue -> a -> IO ()) -> String -> gobj -> a -> IO ()
objectSetPropertyInternal valueSet prop obj val =
  withUTFString prop $ \propPtr ->
  allocaGValue  $ \gvalue -> do
  valueSet gvalue val
  {# call unsafe g_object_set_property #}
    (toGObject obj)
    propPtr
    gvalue

objectGetPropertyInternal :: GObjectClass gobj => (GValue -> IO a) -> String -> gobj -> IO a
objectGetPropertyInternal valueGet prop obj =
  withUTFString prop $ \propPtr ->
  allocaGValue $ \gvalue -> do
  {# call unsafe g_object_get_property #}
    (toGObject obj)
    propPtr
    gvalue
  valueGet gvalue

objectSetPropertyInt :: GObjectClass gobj => String -> gobj -> Int -> IO ()
objectSetPropertyInt = objectSetPropertyInternal valueSetInt

objectGetPropertyInt :: GObjectClass gobj => String -> gobj -> IO Int
objectGetPropertyInt = objectGetPropertyInternal valueGetInt

objectSetPropertyUInt :: GObjectClass gobj => String -> gobj -> Int -> IO ()
objectSetPropertyUInt = objectSetPropertyInternal (\gv v -> valueSetUInt gv (fromIntegral v))

objectGetPropertyUInt :: GObjectClass gobj => String -> gobj -> IO Int
objectGetPropertyUInt = objectGetPropertyInternal (\gv -> liftM fromIntegral $ valueGetUInt gv)

objectSetPropertyBool :: GObjectClass gobj => String -> gobj -> Bool -> IO ()
objectSetPropertyBool = objectSetPropertyInternal valueSetBool

objectGetPropertyBool :: GObjectClass gobj => String -> gobj -> IO Bool
objectGetPropertyBool = objectGetPropertyInternal valueGetBool

objectSetPropertyEnum :: (GObjectClass gobj, Enum enum) => String -> gobj -> enum -> IO ()
objectSetPropertyEnum = objectSetPropertyInternal (\gv v -> valueSetInt gv (fromEnum v))

objectGetPropertyEnum :: (GObjectClass gobj, Enum enum) => String -> gobj -> IO enum
objectGetPropertyEnum = objectGetPropertyInternal (\gv -> liftM toEnum $ valueGetInt gv)

objectSetPropertyFlags :: (GObjectClass gobj, Flags flag) => String -> gobj -> [flag] -> IO ()
objectSetPropertyFlags = objectSetPropertyInternal (\gv v -> valueSetInt gv (fromFlags v))

objectGetPropertyFlags :: (GObjectClass gobj, Flags flag) => String -> gobj -> IO [flag]
objectGetPropertyFlags = objectGetPropertyInternal (\gv -> liftM toFlags $ valueGetInt gv)

objectSetPropertyFloat :: GObjectClass gobj => String -> gobj -> Float -> IO ()
objectSetPropertyFloat = objectSetPropertyInternal valueSetFloat

objectGetPropertyFloat :: GObjectClass gobj => String -> gobj -> IO Float
objectGetPropertyFloat = objectGetPropertyInternal valueGetFloat

objectSetPropertyDouble :: GObjectClass gobj => String -> gobj -> Double -> IO ()
objectSetPropertyDouble = objectSetPropertyInternal valueSetDouble

objectGetPropertyDouble :: GObjectClass gobj => String -> gobj -> IO Double
objectGetPropertyDouble = objectGetPropertyInternal valueGetDouble

objectSetPropertyString :: GObjectClass gobj => String -> gobj -> String -> IO ()
objectSetPropertyString = objectSetPropertyInternal valueSetString

objectGetPropertyString :: GObjectClass gobj => String -> gobj -> IO String
objectGetPropertyString = objectGetPropertyInternal valueGetString

objectSetPropertyMaybeString :: GObjectClass gobj => String -> gobj -> Maybe String -> IO ()
objectSetPropertyMaybeString = objectSetPropertyInternal valueSetMaybeString

objectGetPropertyMaybeString :: GObjectClass gobj => String -> gobj -> IO (Maybe String)
objectGetPropertyMaybeString = objectGetPropertyInternal valueGetMaybeString

objectSetPropertyGObject :: (GObjectClass gobj, GObjectClass gobj') => String -> gobj -> gobj' -> IO ()
objectSetPropertyGObject = objectSetPropertyInternal valueSetGObject

objectGetPropertyGObject :: (GObjectClass gobj, GObjectClass gobj') => String -> gobj -> IO gobj'
objectGetPropertyGObject = objectGetPropertyInternal valueGetGObject


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

writeAttrFromUIntProperty :: GObjectClass gobj => String -> WriteAttr gobj Int
writeAttrFromUIntProperty propName =
  writeAttr (objectSetPropertyUInt propName)

newAttrFromBoolProperty :: GObjectClass gobj => String -> Attr gobj Bool
newAttrFromBoolProperty propName =
  newAttr (objectGetPropertyBool propName) (objectSetPropertyBool propName)

newAttrFromFloatProperty :: GObjectClass gobj => String -> Attr gobj Float
newAttrFromFloatProperty propName =
  newAttr (objectGetPropertyFloat propName) (objectSetPropertyFloat propName)

newAttrFromDoubleProperty :: GObjectClass gobj => String -> Attr gobj Double
newAttrFromDoubleProperty propName =
  newAttr (objectGetPropertyDouble propName) (objectSetPropertyDouble propName)

newAttrFromEnumProperty :: (GObjectClass gobj, Enum enum) => String -> Attr gobj enum
newAttrFromEnumProperty propName =
  newAttr (objectGetPropertyEnum propName) (objectSetPropertyEnum propName)

readAttrFromEnumProperty :: (GObjectClass gobj, Enum enum) => String -> ReadAttr gobj enum
readAttrFromEnumProperty propName =
  readAttr (objectGetPropertyEnum propName)

newAttrFromFlagsProperty :: (GObjectClass gobj, Flags flag) => String -> Attr gobj [flag]
newAttrFromFlagsProperty propName =
  newAttr (objectGetPropertyFlags propName) (objectSetPropertyFlags propName)

newAttrFromStringProperty :: GObjectClass gobj => String -> Attr gobj String
newAttrFromStringProperty propName =
  newAttr (objectGetPropertyString propName) (objectSetPropertyString propName)

writeAttrFromStringProperty :: GObjectClass gobj => String -> WriteAttr gobj String
writeAttrFromStringProperty propName =
  writeAttr (objectSetPropertyString propName)

newAttrFromMaybeStringProperty :: GObjectClass gobj => String -> Attr gobj (Maybe String)
newAttrFromMaybeStringProperty propName =
  newAttr (objectGetPropertyMaybeString propName) (objectSetPropertyMaybeString propName)

newAttrFromObjectProperty :: (GObjectClass gobj, GObjectClass gobj', GObjectClass gobj'') => String -> ReadWriteAttr gobj gobj' gobj''
newAttrFromObjectProperty propName =
  newAttr (objectGetPropertyGObject propName) (objectSetPropertyGObject propName)

writeAttrFromObjectProperty :: (GObjectClass gobj, GObjectClass gobj') => String -> WriteAttr gobj gobj'
writeAttrFromObjectProperty propName =
  writeAttr (objectSetPropertyGObject propName)
