-- -*-haskell-*-
--  GIMP Toolkit (GTK) Object
--
--  Author : Axel Simon
--          
--  Created: 9 April 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:11:46 $
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
-- Widget representation
--
-- * Each widget is a represented as a purely abstract data type. It can only 
--   be accessed through and the special access functions that are defined
--   in each widget file.
--
module Graphics.UI.Gtk.Abstract.Object (
  Object,
  ObjectClass,
  castToObject,
  objectSink,
  makeNewObject,
  objectSetProperty,
  objectGetProperty
  ) where

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject		(objectRef, objectUnref)
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.Types#}
{#import System.Glib.GValue#}
import System.Glib.StoreValue

{# context lib="gtk" prefix="gtk" #}

-- methods

-- turn the initial floating state to sunk
--
-- * The floating\/sunk concept of a GTK object is not very useful to us.
--   The following procedure circumvents the whole subject and ensures 
--   proper cleanup:
--     on creation:      objectRef, objectSink
--     on finalization:  objectUnref
--
-- * This function cannot be bound by c2hs because it is not possible to
--   override the pointer hook.
objectSink :: ObjectClass obj => Ptr obj -> IO ()
objectSink = object_sink.castPtr

#if __GLASGOW_HASKELL__>=504

foreign import ccall unsafe "gtk_object_sink"
  object_sink :: Ptr Object -> IO ()

#else

foreign import ccall "gtk_object_sink" unsafe 
  object_sink :: Ptr Object -> IO ()

#endif

-- This is a convenience function to generate a new widget. It adds the
-- finalizer with the method described under objectSink.
--
-- * The constr argument is the contructor of the specific object.
--
makeNewObject :: ObjectClass obj => 
  (ForeignPtr obj -> obj) -> IO (Ptr obj) -> IO obj
makeNewObject constr generator = do
  objPtr <- generator
  objectRef objPtr
  obj <- newForeignPtr objPtr (objectUnref objPtr)
  objectSink objPtr
  return $ constr obj


-- Sets a specific attribute of this object.
--
-- * Most attributes in a widget can be set and retrieved by passing the
--   name (as a string) and the value to special set\/get functions. These
--   are undocumented because each derived objects implements custom (and
--   welltyped) set and get functions for most attributes.
--
objectSetProperty :: GObjectClass gobj => gobj -> String -> GenericValue -> 
					  IO ()
objectSetProperty obj prop val = alloca $ \vaPtr -> withUTFString prop $ 
  \sPtr -> poke vaPtr val >> {#call unsafe g_object_set_property#} 
  (toGObject obj) sPtr vaPtr >> valueUnset vaPtr
  

-- Gets a specific attribute of this object.
--
-- * See 'objectSetProperty'.
--
objectGetProperty :: GObjectClass gobj => gobj -> String -> 
					IO GenericValue
objectGetProperty obj prop = alloca $ \vaPtr -> withUTFString prop $ \str -> do
  {#call unsafe g_object_get_property#} (toGObject obj) str vaPtr
  res <- peek vaPtr
  valueUnset vaPtr
  return res

