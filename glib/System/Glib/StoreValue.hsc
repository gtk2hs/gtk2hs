-- -*-haskell-*-
--  GIMP Toolkit (GTK) StoreValue TreeStore
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 17:45:41 $
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

module System.Glib.StoreValue (
  TMType(..),
  GenericValue(..)
  ) where

import Monad	(liftM)
import Foreign

import System.Glib.GValue	(GValue, GenericValue(..), valueInit)
import System.Glib.GValueTypes
import System.Glib.GType	(GType)
import Control.Exception  (throw, Exception(AssertionFailed))

#include <glib-object.h>

-- This is an enumeration of all GTypes that can be used in a TreeModel.
--
data TMType = TMinvalid
	    | TMuint
	    | TMint
	    | TMuchar
	    | TMchar
	    | TMboolean
	    | TMenum
	    | TMflags
	    | TMpointer
	    | TMfloat
	    | TMdouble
	    | TMstring
	    | TMobject
	    | TMboxed


instance Enum TMType where
  fromEnum TMinvalid = #const G_TYPE_INVALID
  fromEnum TMuint    = #const G_TYPE_UINT
  fromEnum TMint     = #const G_TYPE_INT
  fromEnum TMuchar   = #const G_TYPE_UCHAR
  fromEnum TMchar    = #const G_TYPE_CHAR
  fromEnum TMboolean = #const G_TYPE_BOOLEAN
  fromEnum TMenum    = #const G_TYPE_ENUM
  fromEnum TMflags   = #const G_TYPE_FLAGS
  fromEnum TMpointer = #const G_TYPE_POINTER
  fromEnum TMfloat   = #const G_TYPE_FLOAT
  fromEnum TMdouble  = #const G_TYPE_DOUBLE
  fromEnum TMstring  = #const G_TYPE_STRING
  fromEnum TMobject  = #const G_TYPE_OBJECT
  fromEnum TMboxed   = #const G_TYPE_BOXED
  toEnum #{const G_TYPE_INVALID} = TMinvalid
  toEnum #{const G_TYPE_UINT}    = TMuint    
  toEnum #{const G_TYPE_INT}	 = TMint     
  toEnum #{const G_TYPE_UCHAR}	 = TMuchar   
  toEnum #{const G_TYPE_CHAR}	 = TMchar    
  toEnum #{const G_TYPE_BOOLEAN} = TMboolean 
  toEnum #{const G_TYPE_ENUM}	 = TMenum
  toEnum #{const G_TYPE_FLAGS}	 = TMflags
  toEnum #{const G_TYPE_POINTER} = TMpointer 
  toEnum #{const G_TYPE_FLOAT}	 = TMfloat   
  toEnum #{const G_TYPE_DOUBLE}	 = TMdouble  
  toEnum #{const G_TYPE_STRING}	 = TMstring  
  toEnum #{const G_TYPE_OBJECT}	 = TMobject  
  toEnum #{const G_TYPE_BOXED}	 = TMboxed
  toEnum _			 = 
    error "StoreValue.toEnum(TMType): no dynamic types allowed."


instance Storable GenericValue where
  sizeOf    _ = #const sizeof(GValue)
  alignment _ = alignment (undefined::#type GType)
  peek gvPtr  = do
    gtype <- liftM (toEnum.fromIntegral::#{type GType} -> TMType) $ 
	     #{peek GValue, g_type} gvPtr
    case gtype of
      TMinvalid	-> throw $ AssertionFailed 
        "StoreValue.peek(GenericValue): invalid or unavailable value."
      TMuint    -> liftM GVuint			  $ valueGetUInt    gvPtr
      TMint	-> liftM GVint	                  $ valueGetInt	    gvPtr
      TMuchar	-> liftM GVuchar		  $ valueGetUChar   gvPtr
      TMchar	-> liftM GVchar			  $ valueGetChar    gvPtr
      TMboolean	-> liftM GVboolean		  $ valueGetBoolean gvPtr
      TMenum	-> liftM (GVenum . fromIntegral)  $ valueGetUInt    gvPtr
      TMflags	-> liftM (GVflags . fromIntegral) $ valueGetUInt    gvPtr
      TMpointer	-> liftM GVpointer		  $ valueGetPointer gvPtr
      TMfloat	-> liftM GVfloat		  $ valueGetFloat   gvPtr
      TMdouble	-> liftM GVdouble		  $ valueGetDouble  gvPtr
      TMstring	-> liftM GVstring		  $ valueGetString  gvPtr
      TMobject	-> liftM GVobject		  $ valueGetObject  gvPtr
      TMboxed   -> liftM GVpointer		  $ valueGetPointer gvPtr
  poke gvPtr val = do
    -- The g_type field of the value must be zero or valueInit will fail.
    poke (castPtr gvPtr) (0::#type GType)
    case val of
      (GVuint x) -> do
	valueInit gvPtr ((fromIntegral.fromEnum) TMuint)
	valueSetUInt    gvPtr x
      (GVint x)	 -> do
	valueInit gvPtr ((fromIntegral.fromEnum) TMint)
	valueSetInt     gvPtr x
      (GVuchar x) -> do
	valueInit gvPtr ((fromIntegral.fromEnum) TMuchar)
	valueSetUChar   gvPtr x
      (GVchar x) -> do
	valueInit gvPtr ((fromIntegral.fromEnum) TMchar)
	valueSetChar    gvPtr x
      (GVboolean x) -> do
	valueInit gvPtr ((fromIntegral.fromEnum) TMboolean)
	valueSetBoolean gvPtr x
      (GVenum x) -> do
	valueInit gvPtr ((fromIntegral.fromEnum) TMenum)
	valueSetUInt    gvPtr (fromIntegral x)
      (GVflags x) -> do
	valueInit gvPtr ((fromIntegral.fromEnum) TMflags)
	valueSetUInt    gvPtr (fromIntegral x)
      (GVpointer x) -> do
	valueInit gvPtr ((fromIntegral.fromEnum) TMpointer)
	valueSetPointer gvPtr x
      (GVfloat x) -> do
	valueInit gvPtr ((fromIntegral.fromEnum) TMfloat)
	valueSetFloat   gvPtr x
      (GVdouble x) -> do
	valueInit gvPtr ((fromIntegral.fromEnum) TMdouble)
	valueSetDouble  gvPtr x
      (GVstring x) -> do
	valueInit gvPtr ((fromIntegral.fromEnum) TMstring)
	valueSetString  gvPtr x
      (GVobject x) -> do
	valueInit gvPtr ((fromIntegral.fromEnum) TMobject)
	valueSetObject  gvPtr x
      (GVboxed x) -> do
	valueInit gvPtr ((fromIntegral.fromEnum) TMboxed)
	valueSetPointer gvPtr x

