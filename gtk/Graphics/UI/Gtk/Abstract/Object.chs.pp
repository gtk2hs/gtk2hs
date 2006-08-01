-- -*-haskell-*-
--  GIMP Toolkit (GTK) Object
--
--  Author : Axel Simon
--
--  Created: 9 April 2001
--
--  Version $Revision: 1.7 $ from $Date: 2005/10/19 12:57:36 $
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

-- * Types
  Object,
  ObjectClass,
  castToObject,
  toObject,

-- * Methods
  makeNewObject,
  ) where

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject	(objectRef, objectUnref)
#if GLIB_CHECK_VERSION(2,10,0)
import System.Glib.GObject	(objectRefSink)
#endif
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.Types#}

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
makeNewObject :: ObjectClass obj => 
  (ForeignPtr obj -> obj) -> IO (Ptr obj) -> IO obj
makeNewObject constr generator = do
  objPtr <- generator
#if GLIB_CHECK_VERSION(2,10,0)
  objectRefSink objPtr
#else
  objectRef objPtr
  objectSink objPtr 
#endif
  obj <- newForeignPtr objPtr (objectUnref objPtr)
  return $ constr obj
