-- -*-haskell-*-
--  GIMP Toolkit (GTK) GObject
--
--  Author : Axel Simon
--
--  Created: 9 April 2001
--
--  Version $Revision: 1.5 $ from $Date: 2005/11/26 16:00:21 $
--
--  Copyright (C) 2001 Axel Simon
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
-- The base object type for all glib objects
--
module System.Glib.GObject (
  -- * Types
  module System.Glib.Types,

  -- * Low level binding functions

  -- | All these functions are internal and are only interesting to people
  -- writing bindings to GObject-style C libraries.
  objectNew,
  objectRef,
  objectUnref,
  makeNewGObject,
  constructNewGObject,
  
  -- ** Callback support
  DestroyNotify,
  mkFunPtrDestroyNotify,

  -- ** Weak references
  GWeakNotify,
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

#else

foreign import ccall unsafe "g_object_unref"
  objectUnref :: Ptr a -> IO ()

#endif


-- | This function wraps any object that does not
-- derive from Object. The object is reference, hence it should be used
-- whenever a function returns a pointer to an internal 'GObject'.
--
-- * The first argument is the contructor of the specific object.
--
makeNewGObject :: GObjectClass obj => 
  (ForeignPtr obj -> obj) -> IO (Ptr obj) -> IO obj
makeNewGObject constr generator = do
  objPtr <- generator
  objectRef objPtr
  obj <- newForeignPtr objPtr (objectUnref objPtr)
  return $ constr obj

{#pointer GDestroyNotify as DestroyNotify#}

foreign import ccall "wrapper" mkDestroyNotifyPtr :: IO () -> IO DestroyNotify

-- | This function wraps any object that does not
-- derive from Object. The object is NOT reference, hence it should be used
-- when a new object is created. Newly created 'GObject's have a reference
-- count of one, hence don't need ref'ing.
--
-- * The first argument is the contructor of the specific object.
--
constructNewGObject :: GObjectClass obj => 
  (ForeignPtr obj -> obj) -> IO (Ptr obj) -> IO obj
constructNewGObject constr generator = do
  objPtr <- generator
  obj <- newForeignPtr objPtr (objectUnref objPtr)
  return $ constr obj

-- | Many methods in classes derived from GObject take a callback function and
-- a destructor function which is called to free that callback function when
-- it is no longer required. This function constructs a DestroyNotify function
-- pointer which when called from C land will free the given Haskell function
-- pointer (and itself).
mkFunPtrDestroyNotify :: FunPtr a -> IO DestroyNotify
mkFunPtrDestroyNotify hPtr = do
  dRef <- newIORef nullFunPtr
  dPtr <- mkDestroyNotifyPtr $ do
    freeHaskellFunPtr hPtr
    dPtr <- readIORef dRef
    freeHaskellFunPtr dPtr
  writeIORef dRef dPtr
  return dPtr

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

