-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget GObject
--
--  Author : Axel Simon
--          
--  Created: 9 April 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 17:46:16 $
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
-- Implements the base GObject class to satisfy the type checker.
--
module System.Glib.GObject (
  GObject(..),
  GObjectClass,
  mkGObject,
  unGObject,
  toGObject,
  fromGObject,
  castToGObject,
  objectNew,
  objectRef,
  objectUnref,
  makeNewGObject,
  GWeakNotify,
  mkDestructor,
  objectWeakref,
  objectWeakunref
  ) where


import Monad (liftM)
import Data.IORef (newIORef, readIORef, writeIORef)

import System.Glib.FFI
{#import System.Glib.Types#}
import System.Glib.GValue (GValue)
import System.Glib.GType  (GType)
import System.Glib.GParameter

{# context lib="glib" prefix="g" #}



{# pointer *GParameter as GParm -> GParameter #}

-- | Construct a new object (should rairly be used directly)
--
objectNew :: GType -> [(String, GValue)] -> IO (Ptr GObject)
objectNew objType parameters =
  liftM castPtr $ --caller must makeNewGObject as we don't know
                  --if it this a GObject or a GtkObject
  withArray (map GParameter parameters) $ \paramArrayPtr ->
  {# call g_object_newv #} objType
  (fromIntegral $ length parameters) paramArrayPtr

-- | Increase the reference counter of an object
--
objectRef :: GObjectClass obj => Ptr obj -> IO ()
objectRef obj = do
  {#call unsafe object_ref#} (castPtr obj)
  return ()

-- | Decrease the reference counter of an object
--
#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "&g_object_unref"
  object_unref' :: FinalizerPtr a

objectUnref :: Ptr a -> FinalizerPtr a
objectUnref _ = object_unref'

#elif __GLASGOW_HASKELL__>=504

foreign import ccall unsafe "g_object_unref"
  objectUnref :: Ptr a -> IO ()

#else

foreign import ccall "g_object_unref" unsafe
  objectUnref :: Ptr a -> IO ()

#endif


-- | This is a convenience function to generate an object that does not
-- derive from Object. It adds objectUnref as finalizer.
--
-- * The constr argument is the contructor of the specific object.
--
makeNewGObject :: GObjectClass obj => 
  (ForeignPtr obj -> obj) -> IO (Ptr obj) -> IO obj
makeNewGObject constr generator = do
  objPtr <- generator
  objectRef objPtr
  obj <- newForeignPtr objPtr (objectUnref objPtr)
  return $ constr obj

{#pointer GWeakNotify#}

foreign import ccall "wrapper" mkDestructor :: IO () -> IO GWeakNotify

-- | Attach a callback that will be called after the
-- destroy hooks have been called
--
objectWeakref :: GObjectClass o => o -> IO () -> IO GWeakNotify
objectWeakref obj uFun = do
  funPtrContainer <- newIORef nullFunPtr
  uFunPtr <- mkDestructor $ do
    uFun
    funPtr <- readIORef funPtrContainer
    freeHaskellFunPtr funPtr
  writeIORef funPtrContainer uFunPtr
  {#call unsafe object_weak_ref#} (toGObject obj) uFunPtr nullPtr
  return uFunPtr

-- | Detach a weak destroy callback function
--
objectWeakunref :: GObjectClass o => o -> GWeakNotify -> IO ()
objectWeakunref obj fun = 
  {#call unsafe object_weak_unref#} (toGObject obj) fun nullPtr

