{-# OPTIONS -fallow-overlapping-instances #-} -- String & [a] overlap
-- -*-haskell-*-
-- GIMP Toolkit (GTK) GConf API
--
--  Author : Duncan Coutts
--  Created: 16 April 2004
--
--  Copyright (c) 2004 Duncan Coutts
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
--  |
--  
--  Module for dealing with the values stored in the GConf system.
--
--  GConfValue has its own primitive type system which is represented in
--  Haskell using type classes. This allows values to be get and set without
--  needing to perform any dynamic type casting or needing a union type.
--  
--  Alternatively, a dynamic\/union type is provided for the rare occasions
--  when that degree of flexability is required. It should only be necessary
--  if you need to deal with configuration values without statically knowing
--  their type.
--

module System.Gnome.GConf.GConfValue (
 GConfPrimitiveValueClass,
 GConfValueClass(marshalFromGConfValue, marshalToGConfValue),
 GConfValue(GConfValue),
 GConfValueDyn(..),
 ) where

import Control.Monad (liftM, when)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList (toGSList, readGSList)

--{# context lib="gconf" prefix ="gconf_value" #}
{# context lib="gconf" #}

{# enum GConfValueType {underscoreToCase} deriving (Eq, Show) #}

{# pointer *GConfValue newtype #}

-- | Class of types which can be kept by GConf
class GConfValueClass value where
  --unsafe because assumes non-null pointer and correct type
  unsafeMarshalFromGConfValue :: GConfValue -> IO value
  
  -- safe checked version, may throw exception
  marshalFromGConfValue :: GConfValue -> IO value
  marshalFromGConfValue value = do
    checkForNullAndExpectedType (typeofGConfValue (undefined::value)) value
    unsafeMarshalFromGConfValue value

  typeofGConfValue :: value -> GConfValueType

  marshalToGConfValue :: value -> IO GConfValue

-- The above methods follow the following memory management rules regarding
-- GConfValues: marshalFrom reads the value but does not gain ownership and thus
-- does not deallocate. marshalTo allocates a new value and gives up ownership;
-- it is not responsible for dellocation (it does not attach a finaliser).
-- The code that uses marshalTo must ensure that it hands the value off to a
-- function that is prepared to asume ownership of the value.

-- | Dynamic version for when the type is not known statically.
data GConfValueDyn = GConfValueString String
                   | GConfValueInt Int
                   | GConfValueFloat Double
                   | GConfValueBool Bool
                   | GConfValueSchema               -- ^ Not supported
                   | GConfValueList [GConfValueDyn] -- ^ Must all be of same primitive type
                   | GConfValuePair (GConfValueDyn, GConfValueDyn) -- ^ Must both be primitive

-- Allow variant using Maybe, where Nothing means the value was not set
-- Use this variant when you expect the gconf key to not be set somethimes;
-- otherwise the 'raw' types will raise an exception if you get an unset key.
-- Just for consistency, setting a key to Nothing will unset the key, however
-- it is preferable to use gconfClientUnset explicitly.
instance GConfValueClass value => GConfValueClass (Maybe value) where
  typeofGConfValue _ = typeofGConfValue (undefined :: value)
  unsafeMarshalFromGConfValue = marshalFromGConfValue
  marshalFromGConfValue value =
    catch (liftM Just $ marshalFromGConfValue value)
          (\_ -> return Nothing)
  marshalToGConfValue (Just v) = marshalToGConfValue v
  marshalToGConfValue Nothing  = return $ GConfValue nullPtr

-- The GConfValue type system says some types are primitive.
-- Compound types (lists & pairs) may only be constructed from primitive types.
class GConfValueClass value => GConfPrimitiveValueClass value
instance GConfPrimitiveValueClass Int
instance GConfPrimitiveValueClass Bool
instance GConfPrimitiveValueClass Double
instance GConfPrimitiveValueClass String

instance GConfValueClass Int where
  typeofGConfValue _ = GconfValueInt
  unsafeMarshalFromGConfValue = liftM fromIntegral . {# call unsafe gconf_value_get_int #}
  marshalToGConfValue n = do
    value <- {# call unsafe gconf_value_new #}
      (fromIntegral $ fromEnum GconfValueInt)
    {# call unsafe gconf_value_set_int #} (GConfValue value) (fromIntegral n)
    return (GConfValue value)

instance GConfValueClass Bool where
  typeofGConfValue _ = GconfValueBool
  unsafeMarshalFromGConfValue = liftM toBool . {# call unsafe gconf_value_get_bool #}
  marshalToGConfValue b = do
    value <- {# call unsafe gconf_value_new #}
      (fromIntegral $ fromEnum GconfValueBool)
    {# call unsafe gconf_value_set_bool #} (GConfValue value) (fromBool b)
    return (GConfValue value)

instance GConfValueClass Double where
  typeofGConfValue _ = GconfValueFloat
  unsafeMarshalFromGConfValue = liftM realToFrac . {# call unsafe gconf_value_get_float #}
  marshalToGConfValue f = do
    value <- {# call unsafe gconf_value_new #}
      (fromIntegral $ fromEnum GconfValueFloat)
    {# call unsafe gconf_value_set_float #} (GConfValue value) (realToFrac f)
    return (GConfValue value)

-- Now unfortunately String & [a] overlap, although really they don't since Char
-- is not an instance of GConfPrimitiveValueClass, however classes are open so
-- we don't know that Char would never be an instance. I want closed classes!
instance GConfValueClass String where
  typeofGConfValue _ = GconfValueString

  unsafeMarshalFromGConfValue value = do
    strPtr <- {# call unsafe gconf_value_get_string #} value
    peekUTFString strPtr

  marshalToGConfValue s = do
    value <- {# call unsafe gconf_value_new #}
      (fromIntegral $ fromEnum GconfValueString)
    withCString s $ \strPtr ->
      {# call unsafe gconf_value_set_string #} (GConfValue value) strPtr
    return (GConfValue value)

instance (GConfPrimitiveValueClass a, GConfPrimitiveValueClass b) => GConfValueClass (a,b) where
  typeofGConfValue _ = GconfValuePair

  unsafeMarshalFromGConfValue value = do
    a <- {# call unsafe gconf_value_get_car #} value
    b <- {# call unsafe gconf_value_get_cdr #} value
    a' <- marshalFromGConfValue (GConfValue a)
    b' <- marshalFromGConfValue (GConfValue b)
    return (a',b')

  marshalToGConfValue (a,b) = do
    value <- {# call unsafe gconf_value_new #}
      (fromIntegral $ fromEnum GconfValuePair)
    a' <- marshalToGConfValue a
    b' <- marshalToGConfValue b
    {# call unsafe gconf_value_set_car_nocopy #} (GConfValue value) a'
    {# call unsafe gconf_value_set_cdr_nocopy #} (GConfValue value) b'
    return (GConfValue value)


instance GConfPrimitiveValueClass a => GConfValueClass [a] where
  typeofGConfValue _ = GconfValueList

  unsafeMarshalFromGConfValue value = do
    gsList <- {# call unsafe gconf_value_get_list #} value
    valuesPtrs <- readGSList gsList
    mapM (unsafeMarshalFromGConfValue . GConfValue) valuesPtrs

  marshalFromGConfValue value = do
    checkForNullAndExpectedType GconfValueList value
    listType <- liftM (toEnum . fromIntegral) $
                {# call unsafe gconf_value_get_list_type #} value
    when (listType /= typeofGConfValue (undefined :: a))
         (fail "GConf: key is list with elements of unexpected type")
    unsafeMarshalFromGConfValue value

  marshalToGConfValue list = do
    value <- {# call unsafe gconf_value_new #}
      (fromIntegral $ fromEnum GconfValueList)
    valuesPtrs <- mapM (liftM (\(GConfValue ptr) -> ptr) . marshalToGConfValue) list
    valuesList <- toGSList valuesPtrs
    {# call unsafe gconf_value_set_list_type #} (GConfValue value)
      (fromIntegral $ fromEnum $ typeofGConfValue (undefined::a))
    {# call unsafe gconf_value_set_list_nocopy #} (GConfValue value) valuesList
    return (GConfValue value)

----------------
-- For convenience and best practice, an instance for Enum 
-- This conforms to the GConf GTK+/Gnome convention for storing enum types,
-- which is to store them as a string using ThisStlyeOfCapitalisation.

-- Note: currently disabled since it requires -fallow-undecidable-instances
{-
instance (Show enum, Read enum, Enum enum, GConfValueClass enum)
      => GConfPrimitiveValueClass enum
instance (Show enum, Read enum, Enum enum) => GConfValueClass enum where
  marshalFromGConfValue value = do
    enumStr <- marshalFromGConfValue value
    case reads enumStr of
      [(enum,_)] -> return enum
      _          -> fail "GCconf: invalid enum value"
  marshalFromGConfValue' value = do
    maybeEnumStr <- marshalFromGConfValue' value
    case maybeEnumStr of
      Nothing -> return Nothing
      (Just enumStr) -> case reads enumStr of
                          [(enum,_)] -> return (Just enum)
                          _          -> return Nothing
  marshalToGConfValue enum = marshalToGConfValue (show enum)
  typeofGConfValue _ = GconfValueString
-}

----------------
-- Helper funcs

gconfValueGetType :: GConfValue ->  IO GConfValueType
--we mean the following but unfortunately c2hs barfs on 'type'
--gconfValueGetType (GConfValue valuePtr) = {# get GConfValue->type #} valuePtr
-- so instead we have the ugly:
gconfValueGetType (GConfValue valuePtr) =
  liftM (toEnum . fromIntegral) $ peek (castPtr valuePtr :: Ptr CInt)
--TODO: check that sizeof(GConfValueType) == sizeof(int)

-- returns Nothing if ok, or and error message
checkForNullAndExpectedType :: GConfValueType -> GConfValue -> IO ()
checkForNullAndExpectedType expectedType value@(GConfValue ptr)
 | ptr == nullPtr = fail "GConf: cannot get value of key, key is unset"
 | otherwise = do valueType <- gconfValueGetType value
                  when (valueType /= expectedType)
                       (fail $ "GConf: key is of unexpected type, expected: "
                          ++ show expectedType ++ ", got: " ++ show valueType)

{-
checkForNullAndExpectedType :: GConfValueType -> GConfValue -> IO GConfValue
checkForNullAndExpectedType expectedType value@(GConfValue ptr)
 | ptr == nullPtr = fail "GConf: cannot get value of key, key is unset"
 | otherwise = do valueType <- gconfValueGetType value
                  if valueType /= expectedType
                    then fail $ "GConf: key is of unexpected type, expected: "
                             ++ show expectedType ++ ", got: " ++ show valueType
                    else return value

checkForNullAndExpectedType' :: GConfValueType -> GConfValue -> IO (Maybe GConfValue)
checkForNullAndExpectedType' expectedType value@(GConfValue ptr)
 | ptr == nullPtr = return Nothing
 | otherwise = do valueType <- gconfValueGetType value
                  if valueType /= expectedType
                    then return Nothing
                    else return (Just value)
-}
----------------
-- GConfValueDyn

unsafeMarshalGConfValueDynListFromGConfValue :: GConfValue -> IO [GConfValueDyn]
unsafeMarshalGConfValueDynListFromGConfValue value = do
  gsList <- {# call unsafe gconf_value_get_list #} value
  valuesPtrs <- readGSList gsList
  mapM (unsafeMarshalFromGConfValue . GConfValue) valuesPtrs

marshalGConfValueDynListToGConfValue :: [GConfValueDyn] -> IO GConfValue
marshalGConfValueDynListToGConfValue as = do
  value <- {# call unsafe gconf_value_new #}
    (fromIntegral $ fromEnum GconfValueList)
  valuesPtrs <- mapM (liftM (\(GConfValue ptr) -> ptr) . marshalToGConfValue) as
  valuesList <- toGSList valuesPtrs
  {# call unsafe gconf_value_set_list_type #} (GConfValue value)
    (fromIntegral $ fromEnum $ (case as of
                                  []    -> GconfValueInvalid  --unknown type
                                  (a:_) -> gconfValueDynGetType (head as)))
  {# call unsafe gconf_value_set_list_nocopy #} (GConfValue value) valuesList
  return (GConfValue value)

unsafeMarshalGConfValueDynPairFromGConfValue :: GConfValue -> IO (GConfValueDyn, GConfValueDyn)
unsafeMarshalGConfValueDynPairFromGConfValue value = do
  a <- {# call unsafe gconf_value_get_car #} value
  b <- {# call unsafe gconf_value_get_cdr #} value
  a' <- marshalFromGConfValue (GConfValue a)
  b' <- marshalFromGConfValue (GConfValue b)
  return (a', b')

marshalGConfValueDynPairToGConfValue :: (GConfValueDyn, GConfValueDyn) -> IO GConfValue
marshalGConfValueDynPairToGConfValue (a,b) = do
  value <- {# call unsafe gconf_value_new #}
    (fromIntegral $ fromEnum GconfValuePair)
  a' <- marshalToGConfValue a
  b' <- marshalToGConfValue b
  {# call unsafe gconf_value_set_car_nocopy #} (GConfValue value) a'
  {# call unsafe gconf_value_set_cdr_nocopy #} (GConfValue value) b'
  return (GConfValue value)

instance GConfValueClass GConfValueDyn where
  typeofGConfValue _ = undefined -- will never be used
  unsafeMarshalFromGConfValue value = do
    valueType <- gconfValueGetType value
    case valueType of
      GconfValueString -> liftM GConfValueString $ unsafeMarshalFromGConfValue value
      GconfValueInt    -> liftM GConfValueInt    $ unsafeMarshalFromGConfValue value
      GconfValueFloat  -> liftM GConfValueFloat  $ unsafeMarshalFromGConfValue value
      GconfValueBool   -> liftM GConfValueBool   $ unsafeMarshalFromGConfValue value
      GconfValueSchema -> return GConfValueSchema
      GconfValueList   -> liftM GConfValueList   $ unsafeMarshalGConfValueDynListFromGConfValue value
      GconfValuePair   -> liftM GConfValuePair   $ unsafeMarshalGConfValueDynPairFromGConfValue value
  
  marshalFromGConfValue value@(GConfValue ptr) = do
    when (ptr == nullPtr) $ fail "GConf: cannot get value of key, key is unset"
    unsafeMarshalFromGConfValue value
  
  marshalToGConfValue v = case v of
    (GConfValueString v') -> marshalToGConfValue v'
    (GConfValueInt    v') -> marshalToGConfValue v'
    (GConfValueFloat  v') -> marshalToGConfValue v'
    (GConfValueBool   v') -> marshalToGConfValue v'
    (GConfValueSchema   ) -> fail "GConf: setting schema types not supported"
    (GConfValueList   v') -> marshalGConfValueDynListToGConfValue v'
    (GConfValuePair   v') -> marshalGConfValueDynPairToGConfValue v'

gconfValueDynGetType :: GConfValueDyn -> GConfValueType
gconfValueDynGetType (GConfValueString _) = GconfValueString
gconfValueDynGetType (GConfValueInt    _) = GconfValueInt
gconfValueDynGetType (GConfValueFloat  _) = GconfValueFloat
gconfValueDynGetType (GConfValueBool   _) = GconfValueBool
gconfValueDynGetType (GConfValueList   _) = GconfValueList
gconfValueDynGetType (GConfValuePair   _) = GconfValuePair
