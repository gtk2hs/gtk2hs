-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget GObject
--
--  Author : Axel Simon
--          
--  Created: 9 April 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 18:12:49 $
--
--  Copyright (c) 2001 Axel Simon
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

