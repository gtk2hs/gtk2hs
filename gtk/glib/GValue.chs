-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry GValue@
--
--  Author : Axel Simon
--          
--  Created: 1 June 2001
--
--  Version $Revision: 1.5 $ from $Date: 2003/07/09 22:42:44 $
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
-- @description@ --------------------------------------------------------------
--
-- * This module implements only the necessities for the GTK binding.
--
-- @documentation@ ------------------------------------------------------------
--
-- * Everything here is only used by @ref data TreeStore@ and friends.
--
-- @todo@ ---------------------------------------------------------------------
--
module GValue(
  GenericValue(..),
  GValue,
  valueInit,
  valueUnset
  ) where

import Monad	(liftM)
import FFI

import GType	(GType)
import Hierarchy(GObject)

{# context lib="glib" prefix="g" #}

{#pointer *GValue -> GenericValue#}

-- @data GenericValue@ A union with information about the currently
-- stored type.
--
-- * Internally used by @ref data TreeStore@.
--
data GenericValue = GVuint    {#type guint#}
		  | GVint     {#type gint#}
		  | GVuchar   {#type guchar#}
		  | GVchar    {#type gchar#}
		  | GVboolean Bool
		  | GVenum    Int
		  | GVflags   Int
		  | GVpointer (Ptr ())
		  | GVfloat   Float
		  | GVdouble  Double
		  | GVstring  (Maybe String)
		  | GVobject  GObject
		  | GVboxed   (Ptr ())

-- @function valueInit@ Clear a GValue.
--
valueInit :: GValue -> GType -> IO ()
valueInit gv gt = liftM (const ()) $ {#call unsafe value_init#} gv gt


-- @function valueUnset@ Free the data in a GValue.
--
valueUnset :: GValue -> IO ()
valueUnset = {#call unsafe value_unset#}


