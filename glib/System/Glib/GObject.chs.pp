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
#if GLIB_CHECK_VERSION(2,10,0)
  objectRefSink,
#endif
  makeNewGObject,
  constructNewGObject,
  
  -- ** Callback support
  DestroyNotify,
  mkFunPtrDestroyNotify,

  -- ** Weak references
  GWeakNotify,
  objectWeakref,
  objectWeakunref,

  -- ** User-Defined Attributes
  Quark,
  quarkFromString,
  objectCreateAttribute,
  objectSetAttribute,
  objectGetAttributeUnsafe
  ) where

import Monad (liftM)
import Data.IORef (newIORef, readIORef, writeIORef)

import System.Glib.FFI
import System.Glib.UTFString
{#import System.Glib.Types#}
import System.Glib.GValue (GValue)
import System.Glib.GType  (GType)
import System.Glib.GParameter
import System.Glib.Attributes (newAttr, Attr)
import Foreign.StablePtr
import Control.Concurrent.MVar ( MVar, newMVar, modifyMVar )

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

#if GLIB_CHECK_VERSION(2,10,0)
-- | Reference and sink an object.
objectRefSink :: GObjectClass obj => Ptr obj -> IO ()
objectRefSink obj = do
  {#call unsafe object_ref_sink#} (castPtr obj)
  return ()
#endif

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

-- | This function wraps any object that does not derive from Object.
-- It should be used whenever a function returns a pointer to an existing
-- 'GObject' (as opposed to a function that constructs a new object).
--
-- * The first argument is the contructor of the specific object.
--
makeNewGObject ::
    GObjectClass obj
 => (ForeignPtr obj -> obj) -- ^ constructor for the Haskell object
 -> IO (Ptr obj)            -- ^ action which yields a pointer to the C object
 -> IO obj
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


type Quark = {#type GQuark#}

-- | A counter for generating unique names.
{-# NOINLINE uniqueCnt #-}
uniqueCnt :: MVar Int
uniqueCnt = unsafePerformIO $ newMVar 0

-- | Create a unique id based on the given string.
quarkFromString :: String -> IO Quark
quarkFromString name = withUTFString name {#call unsafe quark_from_string#}

-- | Add an attribute to this object.
--
-- * The function returns a new attribute that can be set or retrieved from
--   any 'GObject'. The attribute is wrapped in a 'Maybe' type to reflect
--   the circumstance when the attribute is not set or if it should be unset.
--
objectCreateAttribute :: GObjectClass o => IO (Attr o (Maybe a))
objectCreateAttribute = do
  cnt <- modifyMVar uniqueCnt (\cnt -> return (cnt+1, cnt))
  attr <- quarkFromString ("Gtk2HsAttr"++show cnt)
  return (newAttr (objectGetAttributeUnsafe attr)
	          (objectSetAttribute attr)) 

-- | Set the value of an association.
--
objectSetAttribute :: GObjectClass o => Quark -> o -> Maybe a -> IO ()
objectSetAttribute attr obj Nothing = do
  {#call object_set_qdata#} (toGObject obj) attr nullPtr
objectSetAttribute attr obj (Just val) = do
  sPtr <- newStablePtr val
  funPtrContainer <- newIORef nullFunPtr
  destrFunPtr <- mkDestroyNotifyPtr $ do
    freeStablePtr sPtr
    funPtr <- readIORef funPtrContainer
    freeHaskellFunPtr funPtr
  writeIORef funPtrContainer destrFunPtr
  {#call object_set_qdata_full#} (toGObject obj) attr (castStablePtrToPtr sPtr)
				 destrFunPtr

-- | Get the value of an association.
--
-- * Note that this function may crash the Haskell run-time since the
--   returned type can be forced to be anything. See 'objectAddAttribute'
--   for a safe wrapper around this funciton.
--
objectGetAttributeUnsafe :: GObjectClass o => Quark -> o -> IO (Maybe a)
objectGetAttributeUnsafe attr obj = do
  sPtr <- {#call unsafe object_get_qdata#} (toGObject obj) attr
  if sPtr==nullPtr then return Nothing else
    liftM Just $ deRefStablePtr (castPtrToStablePtr sPtr)
