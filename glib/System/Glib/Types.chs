-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget GObject
--
--  Author : Axel Simon
--
--  Created: 9 April 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/04/19 02:15:23 $
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
  fromGObject,
  castToGObject,
  ) where

import Foreign (ForeignPtr)
import GHC.Base (unsafeCoerce#)

{# context lib="glib" prefix="g" #}

{#pointer *GObject foreign newtype #}

mkGObject = GObject
unGObject (GObject o) = o

class GObjectClass o
toGObject   :: GObjectClass o => o -> GObject
toGObject   = unsafeCoerce#
fromGObject :: GObjectClass o => GObject -> o
fromGObject = unsafeCoerce#

instance GObjectClass GObject

castToGObject :: GObjectClass obj => obj -> obj
castToGObject = id

