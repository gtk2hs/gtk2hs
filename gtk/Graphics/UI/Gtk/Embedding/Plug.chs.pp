-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Plug
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/11/18 15:54:57 $
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- Toplevel for embedding into other processes
--
module Graphics.UI.Gtk.Embedding.Plug (
-- * Detail
-- 
-- | Together with 'Socket', 'Plug' provides the ability to embed widgets from
-- one process into another process in a fashion that is transparent to the
-- user. One process creates a 'Socket' widget and, passes the ID of that
-- widgets window to the other process, which then creates a 'Plug' with that
-- window ID. Any widgets contained in the 'Plug' then will appear inside the
-- first applications window.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Window'
-- |                                 +----Plug
-- @

#if !defined(WIN32) || GTK_CHECK_VERSION(2,8,0)
-- * Types
  Plug,
  PlugClass,
  castToPlug,
  toPlug,
  NativeWindowId,

-- * Constructors
  plugNew,

-- * Methods
  plugGetId,

-- * Signals
  onEmbedded,
  afterEmbedded,
#endif
  ) where

import Monad	(liftM)
import Maybe	(fromMaybe)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.Embedding.Embedding

{# context lib="gtk" prefix="gtk" #}

#if !defined(WIN32) || GTK_CHECK_VERSION(2,8,0)

--------------------
-- Constructors

-- | Creates a new plug widget inside the 'Socket' identified by @socketId@.
-- If @socketId@ is @Nothing@, the plug is left \"unplugged\" and can later be
-- plugged into a 'Socket' by 'Graphics.UI.Gtk.Embedding.Socket.socketAddId'.
--
-- If a NativeWindowId is supplied the foreign application window will
-- immediatly appear in this 'Plug' once it is shown. If @Nothing@ is passed
-- then a 'NativeWindowId' can be extracted from this 'Plug' using 'plugGetId'
-- and be passed to the application which is to be embedded.
--
plugNew :: 
  Maybe NativeWindowId -- ^ @socketId@ - the window ID of the socket, or
                       -- @Nothing@.
 -> IO Plug
plugNew socketId =
  makeNewObject mkPlug $
  liftM (castPtr :: Ptr Widget -> Ptr Plug) $
  {# call unsafe plug_new #}
    (fromIntegral (fromMaybe 0 socketId))

--------------------
-- Methods

-- | Gets the window ID of a 'Plug' widget, which can then be used to embed
-- this window inside another window, for instance with
-- 'Graphics.UI.Gtk.Embedding.Socket.socketAddId'.
--
plugGetId :: PlugClass self => self
 -> IO NativeWindowId -- ^ returns the window ID for the plug
plugGetId self =
  liftM fromIntegral $
  {# call unsafe plug_get_id #}
    (toPlug self)

--------------------
-- Signals

-- | This plug received another application.
--
onEmbedded, afterEmbedded :: PlugClass self => self
 -> IO ()
 -> IO (ConnectId self)
onEmbedded = connect_NONE__NONE "embedded" False
afterEmbedded = connect_NONE__NONE "embedded" True

#endif
