-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry GValue (variant record)@
--
--  Author : Axel Simon
--          
--  Created: 1 June 2001
--
--  Version $Revision: 1.3 $ from $Date: 2002/07/18 18:14:30 $
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
--- DOCU ----------------------------------------------------------------------
--
-- * Everything here is only used by @TreeStore and friends.
--
--- TODO ----------------------------------------------------------------------

module GValue(
  GenericValue(..),
  GValue,
  valueInit,
  valueUnset
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import GType	(GType)
import Hierarchy(GObject)

{# context lib="glib" prefix="g" #}

{#pointer *GValue -> GenericValue#}

-- GenericValue (or GValue) is a union with information about the currently
-- stored type.
--
-- * Internally used by @TreeStore.
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

-- Clear a GValue.
--
valueInit :: GValue -> GType -> IO ()
valueInit gv gt = liftM (const ()) $ {#call unsafe value_init#} gv gt


-- Free the data in a GValue.
--
valueUnset :: GValue -> IO ()
valueUnset = {#call unsafe value_unset#}


