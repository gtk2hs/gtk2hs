{-# OPTIONS_HADDOCK hide #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget GObject
--
--  Author : Axel Simon
--
--  Created: 9 April 2001
--
--  Copyright (c) 2001 Axel Simon
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
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Implements the base GObject class.
--
module System.Glib.Types (
  GObject(..),
  GObjectClass,
  mkGObject,
  unGObject,
  toGObject,
  unsafeCastGObject,
  castToGObject,
  objectUnref
  ) where

import System.Glib.FFI

{# context lib="glib" prefix="g" #}

{#pointer *GObject foreign newtype #} deriving (Eq)

mkGObject = (GObject, objectUnref)
unGObject (GObject o) = o

class GObjectClass o where
  -- | Safe upcast.
  toGObject         :: o -> GObject
  -- | Unchecked downcast.
  unsafeCastGObject :: GObject -> o

instance GObjectClass GObject where
  toGObject = id
  unsafeCastGObject = id

castToGObject :: GObjectClass obj => obj -> obj
castToGObject = id

-- | Decrease the reference counter of an object
--
foreign import ccall unsafe "&g_object_unref"
  objectUnref :: FinalizerPtr a

