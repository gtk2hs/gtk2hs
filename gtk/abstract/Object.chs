-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Root of the object hierarchy@
--
--  Author : Axel Simon
--          
--  Created: 9 April 2001
--
--  Version $Revision: 1.4 $ from $Date: 2002/07/08 09:15:08 $
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
-- @description@ --------------------------------------------------------------
--

--
-- @documentation@ ------------------------------------------------------------
--
-- * Widget representation
--   Each widget is a represented as a purely abstract data type. It can only 
--   be accessed through and the special access functions that are defined
--   in each widget file.
--
-- @todo@ ---------------------------------------------------------------------
--
--
--
module Object(
  Object,
  ObjectClass(..),
  castToObject,
  objectSink,
  makeNewObject,
  objectSetProperty,
  objectGetProperty
  ) where

import Foreign
import CForeign	(withCString, CChar)
import GObject	(objectRef, objectUnref)
{#import Signal#}
{#import Hierarchy#}
{#import GValue#}
import StoreValue

{# context lib="gtk" prefix="gtk" #}

-- methods

-- turn the initial floating state to sunk
--
-- * The floating/sunk concept of a GTK object is not very useful to us.
--   The following procedure circumvents the whole subject and ensures 
--   proper cleanup:
--     on creation:      objectRef, objectSink
--     on finalization:  objectUnref
--
-- * This function cannot be bound by c2hs because it is not possible to
--   override the pointer hook.
objectSink :: ObjectClass obj => Ptr obj -> IO ()
objectSink = object_sink.castPtr

foreign import ccall "gtk_object_sink" unsafe 
  object_sink :: Ptr Object -> IO ()

-- This is a convenience function to generate a new widget. It adds the
-- finalizer with the method described under @objectSink.
--
-- * The @constr argument is the contructor of the specific object.
--
makeNewObject :: ObjectClass obj => 
  (ForeignPtr obj -> obj) -> IO (Ptr obj) -> IO obj
makeNewObject constr generator = do
  objPtr <- generator
  objectRef objPtr
  obj <- newForeignPtr objPtr (objectUnref objPtr)
  objectSink objPtr
  return $ constr obj


-- @method private objectSetProperty@ Sets a specific attribute of this object.
--
-- * Most attributes in a widget can be set and retrieved by passing the
--   name (as a string) and the value to special set/get functions. These
--   are undocumented because each derived objects implements custom (and
--   welltyped) set and get functions for most attributes.
--
objectSetProperty :: GObjectClass obj => obj -> String -> GenericValue -> IO ()
objectSetProperty obj prop val = alloca $ \vaPtr -> withCString prop $ 
  \sPtr -> poke vaPtr val >> {#call unsafe g_object_set_property#} 
  (toGObject obj) sPtr vaPtr
  

-- @method private objectGetProperty@ Gets a specific attribute of this object.
--
-- * See @ref method objectSetProperty@.
--
objectGetProperty :: GObjectClass obj => obj -> String -> 
					IO GenericValue
objectGetProperty obj prop = alloca $ \vaPtr -> withCString prop $ \str ->
  {#call unsafe g_object_get_property#} (toGObject obj) str vaPtr >> 
  peek vaPtr

