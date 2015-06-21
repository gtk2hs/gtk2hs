-- -*-haskell-*-
--  GIMP Toolkit (GTK) StoreValue GenericValue
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
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
-- TODO: this module is deprecated and should be removed. The GenericValue
-- type is currently exposed to users and it should not be.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
module System.Glib.StoreValue (
  TMType(..),
  GenericValue(..),
  valueSetGenericValue,
  valueGetGenericValue,
  ) where

import Control.Monad    (liftM)
import Data.Text (Text)

import Control.Exception  (throw, AssertionFailed(..))

#include<glib-object.h>

import System.Glib.FFI
import System.Glib.GValue       (GValue, valueInit, valueGetType)
import System.Glib.GValueTypes
import qualified System.Glib.GTypeConstants as GType
import System.Glib.Types        (GObject)

-- | A union with information about the currently stored type.
--
-- * Internally used by "Graphics.UI.Gtk.TreeList.TreeModel".
--
data GenericValue = GVuint    Word
                  | GVint     Int
--                | GVuchar   #{type guchar}
--                | GVchar    #{type gchar}
                  | GVboolean Bool
                  | GVenum    Int
                  | GVflags   Int
--                | GVpointer (Ptr ())
                  | GVfloat   Float
                  | GVdouble  Double
                  | GVstring  (Maybe Text)
                  | GVobject  GObject
--                | GVboxed   (Ptr ())

-- This is an enumeration of all GTypes that can be used in a TreeModel.
--
data TMType = TMinvalid
            | TMuint
            | TMint
--          | TMuchar
--          | TMchar
            | TMboolean
            | TMenum
            | TMflags
--          | TMpointer
            | TMfloat
            | TMdouble
            | TMstring
            | TMobject
--          | TMboxed

instance Enum TMType where
  fromEnum TMinvalid = #const G_TYPE_INVALID
  fromEnum TMuint    = #const G_TYPE_UINT
  fromEnum TMint     = #const G_TYPE_INT
--  fromEnum TMuchar   = #const G_TYPE_UCHAR
--  fromEnum TMchar    = #const G_TYPE_CHAR
  fromEnum TMboolean = #const G_TYPE_BOOLEAN
  fromEnum TMenum    = #const G_TYPE_ENUM
  fromEnum TMflags   = #const G_TYPE_FLAGS
--  fromEnum TMpointer = #const G_TYPE_POINTER
  fromEnum TMfloat   = #const G_TYPE_FLOAT
  fromEnum TMdouble  = #const G_TYPE_DOUBLE
  fromEnum TMstring  = #const G_TYPE_STRING
  fromEnum TMobject  = #const G_TYPE_OBJECT
--  fromEnum TMboxed   = #const G_TYPE_BOXED
  toEnum #{const G_TYPE_INVALID} = TMinvalid
  toEnum #{const G_TYPE_UINT}    = TMuint
  toEnum #{const G_TYPE_INT}     = TMint
--  toEnum #{const G_TYPE_UCHAR} = TMuchar
--  toEnum #{const G_TYPE_CHAR}  = TMchar
  toEnum #{const G_TYPE_BOOLEAN} = TMboolean
  toEnum #{const G_TYPE_ENUM}    = TMenum
  toEnum #{const G_TYPE_FLAGS}   = TMflags
--  toEnum #{const G_TYPE_POINTER} = TMpointer
  toEnum #{const G_TYPE_FLOAT}   = TMfloat
  toEnum #{const G_TYPE_DOUBLE}  = TMdouble
  toEnum #{const G_TYPE_STRING}  = TMstring
  toEnum #{const G_TYPE_OBJECT}  = TMobject
--  toEnum #{const G_TYPE_BOXED}         = TMboxed
  toEnum _                       =
    error "StoreValue.toEnum(TMType): no dynamic types allowed."

valueSetGenericValue :: GValue -> GenericValue -> IO ()
valueSetGenericValue gvalue (GVuint x)    = do valueInit gvalue GType.uint
                                               valueSetUInt gvalue x
valueSetGenericValue gvalue (GVint x)     = do valueInit gvalue GType.int
                                               valueSetInt  gvalue x
--valueSetGenericValue gvalue (GVuchar x)   = valueSetUChar   gvalue x
--valueSetGenericValue gvalue (GVchar x)    = valueSetChar    gvalue x
valueSetGenericValue gvalue (GVboolean x) = do valueInit gvalue GType.bool
                                               valueSetBool    gvalue x
valueSetGenericValue gvalue (GVenum x)    = do valueInit gvalue GType.enum
                                               valueSetUInt    gvalue (fromIntegral x)
valueSetGenericValue gvalue (GVflags x)   = do valueInit gvalue GType.flags
                                               valueSetUInt    gvalue (fromIntegral x)
--valueSetGenericValue gvalue (GVpointer x) = valueSetPointer gvalue x
valueSetGenericValue gvalue (GVfloat x)   = do valueInit gvalue GType.float
                                               valueSetFloat   gvalue x
valueSetGenericValue gvalue (GVdouble x)  = do valueInit gvalue GType.double
                                               valueSetDouble  gvalue x
valueSetGenericValue gvalue (GVstring x)  = do valueInit gvalue GType.string
                                               valueSetMaybeString  gvalue x
valueSetGenericValue gvalue (GVobject x)  = do valueInit gvalue GType.object
                                               valueSetGObject gvalue x
--valueSetGenericValue gvalue (GVboxed x)   = valueSetPointer gvalue x

valueGetGenericValue :: GValue -> IO GenericValue
valueGetGenericValue gvalue = do
  gtype <- valueGetType gvalue
  case (toEnum . fromIntegral) gtype of
    TMinvalid   -> throw $ AssertionFailed
      "StoreValue.valueGetGenericValue: invalid or unavailable value."
    TMuint    -> liftM GVuint                     $ valueGetUInt    gvalue
    TMint       -> liftM GVint                    $ valueGetInt     gvalue
--    TMuchar   -> liftM GVuchar                  $ valueGetUChar   gvalue
--    TMchar    -> liftM GVchar                   $ valueGetChar    gvalue
    TMboolean   -> liftM GVboolean                $ valueGetBool    gvalue
    TMenum      -> liftM (GVenum . fromIntegral)  $ valueGetUInt    gvalue
    TMflags     -> liftM (GVflags . fromIntegral) $ valueGetUInt    gvalue
--    TMpointer -> liftM GVpointer                $ valueGetPointer gvalue
    TMfloat     -> liftM GVfloat                  $ valueGetFloat   gvalue
    TMdouble    -> liftM GVdouble                 $ valueGetDouble  gvalue
    TMstring    -> liftM GVstring                 $ valueGetMaybeString  gvalue
    TMobject    -> liftM GVobject                 $ valueGetGObject gvalue
--    TMboxed   -> liftM GVpointer                $ valueGetPointer gvalue
