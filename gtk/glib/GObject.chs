{-# OPTIONS -cpp #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget GObject@
--
--  Author : Axel Simon
--          
--  Created: 9 April 2001
--
--  Version $Revision: 1.6 $ from $Date: 2003/07/09 22:42:44 $
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
--  * Implements the base GObject class to satisfy the type checker.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
module GObject(
  objectRef,
  objectUnref,
  makeNewGObject,
  GWeakNotify,
  mkDestructor,
  objectWeakref,
  objectWeakunref
  ) where


import FFI
import LocalData(newIORef, readIORef, writeIORef)
import Hierarchy(GObjectClass, toGObject, unGObject)
{#import GValue#}

{# context lib="glib" prefix="g" #}

-- increase the reference counter of an object
--
objectRef :: GObjectClass obj => Ptr obj -> IO ()
objectRef obj = do
  {#call unsafe object_ref#} (castPtr obj)
  return ()

-- decrease the reference counter of an object
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


-- This is a convenience function to generate an object that does not
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

#if __GLASGOW_HASKELL__>=600

foreign import ccall "wrapper" mkDestructor :: IO () -> IO GWeakNotify

#else

foreign export dynamic mkDestructor :: IO () -> IO GWeakNotify

#endif

-- @method objectWeakref@ attach a callback that will be called after the
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
  withForeignPtr ((castForeignPtr.unGObject.toGObject) obj) $ \objPtr ->
    {#call unsafe object_weak_ref#} objPtr uFunPtr nullPtr
  return uFunPtr

-- @method objectWeakunref@ detach a weak destroy callback function
--
objectWeakunref :: GObjectClass o => o -> GWeakNotify -> IO ()
objectWeakunref obj fun = 
  withForeignPtr ((castForeignPtr.unGObject.toGObject) obj) $ \objPtr ->
    {#call unsafe object_weak_unref#} objPtr fun nullPtr
  


  
