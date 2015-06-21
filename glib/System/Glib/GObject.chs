{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) GObject
--
--  Author : Axel Simon
--
--  Created: 9 April 2001
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
#if GLIB_CHECK_VERSION(2,10,0)
  objectRefSink,
#endif
  makeNewGObject,
  constructNewGObject,
  wrapNewGObject,

  -- ** GType queries
  gTypeGObject,
  isA,

  -- ** Callback support
  DestroyNotify,
  destroyFunPtr,
  destroyStablePtr,

  -- ** User-Defined Attributes
  Quark,
  quarkFromString,
  objectCreateAttribute,
  objectSetAttribute,
  objectGetAttributeUnsafe
  ) where

import Control.Monad (liftM, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T (pack)

import System.Glib.FFI
import System.Glib.UTFString
{#import System.Glib.Types#}
import System.Glib.GValue (GValue)
import System.Glib.GType  (GType, typeInstanceIsA)
import System.Glib.GTypeConstants ( object )
import System.Glib.GParameter
import System.Glib.Attributes (newNamedAttr, Attr)
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

-- | The type constant to check if an instance is of 'GObject' type.
gTypeGObject :: GType
gTypeGObject = object

-- | This function wraps any object that does not derive from Object.
-- It should be used whenever a function returns a pointer to an existing
-- 'GObject' (as opposed to a function that constructs a new object).
--
-- * The first argument is the contructor of the specific object.
--
makeNewGObject ::
    GObjectClass obj
 => (ForeignPtr obj -> obj, FinalizerPtr obj)
    -- ^ constructor for the Haskell object and finalizer C function
 -> IO (Ptr obj)            -- ^ action which yields a pointer to the C object
 -> IO obj
makeNewGObject (constr, objectUnref) generator = do
  objPtr <- generator
  when (objPtr == nullPtr) (fail "makeNewGObject: object is NULL")
  objectRef objPtr
  obj <- newForeignPtr objPtr objectUnref
  return $! constr obj

{#pointer GDestroyNotify as DestroyNotify#}

-- | This function wraps any newly created objects that derives from
-- GInitiallyUnowned also known as objects with
-- \"floating-references\". The object will be refSink (for glib
-- versions >= 2.10). On non-floating objects, this function behaves
-- exactly the same as "makeNewGObject".
--
constructNewGObject :: GObjectClass obj =>
  (ForeignPtr obj -> obj, FinalizerPtr obj) -> IO (Ptr obj) -> IO obj
constructNewGObject (constr, objectUnref) generator = do
  objPtr <- generator
#if GLIB_CHECK_VERSION(2,10,0)
  -- change the exisiting floating reference into a proper reference;
  -- the name is confusing, what the function does is ref,sink,unref
  objectRefSink objPtr
#endif
  obj <- newForeignPtr objPtr objectUnref
  return $! constr obj

-- | This function wraps any newly created object that does not derived
-- from GInitiallyUnowned (that is a GObject with no floating
-- reference). Since newly created 'GObject's have a reference count of
-- one, they don't need ref'ing.
--
wrapNewGObject :: GObjectClass obj =>
  (ForeignPtr obj -> obj, FinalizerPtr obj) -> IO (Ptr obj) -> IO obj
wrapNewGObject (constr, objectUnref) generator = do
  objPtr <- generator
  when (objPtr == nullPtr) (fail "wrapNewGObject: object is NULL")
  obj <- newForeignPtr objPtr objectUnref
  return $! constr obj

-- | Many methods in classes derived from GObject take a callback function and
-- a destructor function which is called to free that callback function when
-- it is no longer required. This constants is an address of a functions in
-- C land that will free a function pointer.
foreign import ccall unsafe "&freeHaskellFunctionPtr" destroyFunPtr :: DestroyNotify

type Quark = {#type GQuark#}

-- | A counter for generating unique names.
{-# NOINLINE uniqueCnt #-}
uniqueCnt :: MVar Int
uniqueCnt = unsafePerformIO $ newMVar 0

-- | Create a unique id based on the given string.
quarkFromString :: GlibString string => string -> IO Quark
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
  let propName = "Gtk2HsAttr"++show cnt
  attr <- quarkFromString $ T.pack propName
  return (newNamedAttr propName (objectGetAttributeUnsafe attr)
                                (objectSetAttribute attr))

-- | The address of a function freeing a 'StablePtr'. See 'destroyFunPtr'.
foreign import ccall unsafe "&hs_free_stable_ptr" destroyStablePtr :: DestroyNotify

-- | Set the value of an association.
--
objectSetAttribute :: GObjectClass o => Quark -> o -> Maybe a -> IO ()
objectSetAttribute attr obj Nothing = do
  {#call object_set_qdata#} (toGObject obj) attr nullPtr
objectSetAttribute attr obj (Just val) = do
  sPtr <- newStablePtr val
  {#call object_set_qdata_full#} (toGObject obj) attr (castStablePtrToPtr sPtr)
                                 destroyStablePtr

-- | Get the value of an association.
--
-- * Note that this function may crash the Haskell run-time since the
--   returned type can be forced to be anything. See 'objectCreateAttribute'
--   for a safe wrapper around this funciton.
--
objectGetAttributeUnsafe :: GObjectClass o => Quark -> o -> IO (Maybe a)
objectGetAttributeUnsafe attr obj = do
  sPtr <- {#call unsafe object_get_qdata#} (toGObject obj) attr
  if sPtr==nullPtr then return Nothing else
    liftM Just $! deRefStablePtr (castPtrToStablePtr sPtr)

-- | Determine if this is an instance of a particular GTK type
--
isA :: GObjectClass o => o -> GType -> Bool
isA obj gType =
        typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr.unGObject.toGObject) obj) gType

-- at this point we would normally implement the notify signal handler;
-- I've moved this definition into the Object class of the gtk package
-- since there's a quite a bit of machinery missing here (generated signal
-- register functions and the problem of recursive modules)
