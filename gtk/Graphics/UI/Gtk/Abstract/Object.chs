{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Object
--
--  Author : Axel Simon
--
--  Created: 9 April 2001
--
--  Copyright (C) 2001-2005 Axel Simon
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
-- The base class of the Gtk+ type hierarchy.
--
-- * Each widget is a represented as a purely abstract data type. It can only
--   be accessed through and the special access functions that are defined
--   in each widget file.
--
module Graphics.UI.Gtk.Abstract.Object (
-- * Detail
--
-- | 'Object' is the base class for all widgets, and for a few non-widget
-- objects such as 'Adjustment'. 'Object' predates 'GObject'; non-widgets that
-- derive from 'Object' rather than 'GObject' do so for backward compatibility
-- reasons.
--
-- Object has been removed in Gt3k, but this module still provides useful
-- functions.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----Object
-- |         +----'Widget'
-- |         +----'Adjustment'
-- |         +----'CellRenderer'
-- |         +----'FileFilter'
-- |         +----'ItemFactory'
-- |         +----'Tooltips'
-- |         +----'TreeViewColumn'
-- @
#if GTK_MAJOR_VERSION < 3
-- * Types
  Object,
  ObjectClass,
  castToObject, gTypeObject,
  toObject,
#endif

-- * Methods
  makeNewObject,

-- * Weak references
  GWeakNotify,
  objectWeakref,
  objectWeakunref,

-- * Signals
  objectDestroy,
  notifyProperty
  ) where
import Control.Monad (when)

import System.Glib.FFI
import System.Glib.Attributes (ReadWriteAttr)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Data.IORef

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

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
#if !GLIB_CHECK_VERSION(2,10,0)
foreign import ccall unsafe "gtk_object_sink"
  objectSink :: Ptr obj -> IO ()
#endif
-- This is a convenience function to generate a new widget. It adds the
-- finalizer with the method described under objectSink.
--
-- * The constr argument is the contructor of the specific object.
--
#if GTK_MAJOR_VERSION < 3
makeNewObject :: ObjectClass obj =>
#else
makeNewObject :: GObjectClass obj =>
#endif
  (ForeignPtr obj -> obj, FinalizerPtr obj) -> IO (Ptr obj) -> IO obj
makeNewObject (constr, objectUnref) generator = do
  objPtr <- generator
  when (objPtr == nullPtr) (fail "makeNewObject: object is NULL")
#if GLIB_CHECK_VERSION(2,10,0)
  objectRefSink objPtr
#else
  objectRef objPtr
  objectSink objPtr
#endif
  obj <- newForeignPtr objPtr objectUnref
  return $! constr obj

{#pointer GWeakNotify#}

foreign import ccall "wrapper" mkDestructor
  :: (Ptr () -> Ptr GObject -> IO ()) -> IO GWeakNotify

-- | Attach a callback that will be called after the
-- destroy hooks have been called
--
#if GTK_MAJOR_VERSION < 3
objectWeakref :: ObjectClass o => o -> IO () -> IO GWeakNotify
#else
objectWeakref :: GObjectClass o => o -> IO () -> IO GWeakNotify
#endif
objectWeakref obj uFun = do
  funPtrContainer <- newIORef nullFunPtr
  uFunPtr <- mkDestructor $ \_ _ -> do
    uFun
    funPtr <- readIORef funPtrContainer
    freeHaskellFunPtr funPtr
  writeIORef funPtrContainer uFunPtr
  {#call unsafe g_object_weak_ref#} (toGObject obj) uFunPtr nullPtr
  return uFunPtr

-- | Detach a weak destroy callback function
--
#if GTK_MAJOR_VERSION < 3
objectWeakunref :: ObjectClass o => o -> GWeakNotify -> IO ()
#else
objectWeakunref :: GObjectClass o => o -> GWeakNotify -> IO ()
#endif
objectWeakunref obj fun =
  {#call unsafe g_object_weak_unref#} (toGObject obj) fun nullPtr


--------------------
-- Signals

-- | Signals that all holders of a reference to the 'Object' should release
-- the reference that they hold. May result in finalization of the object if
-- all references are released.
--
#if GTK_MAJOR_VERSION < 3
objectDestroy :: ObjectClass self => Signal self (IO ())
#else
objectDestroy :: WidgetClass self => Signal self (IO ())
#endif
objectDestroy = Signal (connect_NONE__NONE "destroy")

-- | Register a notify callback that is triggered when the given property
--   has been modified.
--
-- * Note that this callback is triggered even if the actual value of
--   the property has not changed.
-- * Not all attributes are properties. A warning will be generated at
--   runtime if the passed-in attribute is not a property of the class
--   with which it was registered.
--
#if GTK_MAJOR_VERSION < 3
notifyProperty :: ObjectClass self => ReadWriteAttr self a b -> Signal self (IO ())
#else
notifyProperty :: GObjectClass self => ReadWriteAttr self a b -> Signal self (IO ())
#endif
notifyProperty attr = Signal (\on obj cb -> connect_PTR__NONE ("notify::"++show attr) on obj (const cb))
