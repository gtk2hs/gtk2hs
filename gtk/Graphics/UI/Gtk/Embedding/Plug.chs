{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Plug
--
--  Author : Axel Simon, Andy Stewart
--
--  Created: 23 May 2001
--
--  Copyright (C) 1999-2005 Axel Simon
--  Copyright (C) 2009      Andy Stewart
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

#if (defined(HAVE_PLUG_AND_SOCKET) && (!defined(WIN32) || GTK_CHECK_VERSION(2,8,0))) || defined(GDK_WINDOWING_X11)
-- * Types
  Plug,
  PlugClass,
  castToPlug, gTypePlug,
  toPlug,
  NativeWindowId,

-- * Constructors
  plugNew,
#if GTK_CHECK_VERSION(2,2,0)
  plugNewForDisplay,
#endif

-- * Methods
  plugGetId,
#if GTK_CHECK_VERSION(2,14,0)
  plugGetEmbedded,
  plugGetSocketWindow,
#endif

-- * Attributes
  plugAttrEmbedded,
  plugAttrSocketWindow,

-- * Signals
  plugEmbedded,
#endif
  ) where

#if (defined(HAVE_PLUG_AND_SOCKET) && (!defined(WIN32) || GTK_CHECK_VERSION(2,8,0))) || defined(GDK_WINDOWING_X11)

import Control.Monad    (liftM)
import Data.Maybe       (fromMaybe)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Embedding.Types#}
{#import Graphics.UI.Gtk.Signals#}
#if GTK_MAJOR_VERSION < 3
import Graphics.UI.Gtk.Embedding.Embedding
#endif
import Graphics.UI.Gtk.General.Structs

{# context lib="gtk" prefix="gtk" #}

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
    (fromNativeWindowId (fromMaybe nativeWindowIdNone socketId))

#if GTK_CHECK_VERSION(2,2,0)
-- | Create a new plug widget inside the 'Socket' identified by socket_id.
--
-- * Available since Gtk+ version 2.2
--
plugNewForDisplay ::
    Display             -- ^ @display@ - the 'Display' on which @socketId@ is
                        -- displayed
 -> Maybe NativeWindowId -- ^ @socketId@ - the XID of the socket's window.
 -> IO Plug
plugNewForDisplay display socketId =
  makeNewObject mkPlug $
  liftM (castPtr :: Ptr Widget -> Ptr Plug) $
  {# call gtk_plug_new_for_display #}
    display
    (fromNativeWindowId (fromMaybe nativeWindowIdNone socketId))
#endif

--------------------
-- Methods

-- | Gets the window ID of a 'Plug' widget, which can then be used to embed
-- this window inside another window, for instance with
-- 'Graphics.UI.Gtk.Embedding.Socket.socketAddId'.
--
plugGetId :: PlugClass self => self
 -> IO NativeWindowId -- ^ returns the window ID for the plug
plugGetId self =
  liftM toNativeWindowId $
  {# call unsafe plug_get_id #}
    (toPlug self)

#if GTK_CHECK_VERSION(2,14,0)
-- | Determines whether the plug is embedded in a socket.
--
-- * Available since Gtk+ version 2.14
--
plugGetEmbedded :: PlugClass self => self
 -> IO Bool -- ^ returns @True@ if the plug is embedded in a socket
plugGetEmbedded self =
  liftM toBool $
  {# call gtk_plug_get_embedded #}
    (toPlug self)

-- | Retrieves the socket the plug is embedded in.
--
-- * Available since Gtk+ version 2.14
--
plugGetSocketWindow :: PlugClass self => self
 -> IO (Maybe DrawWindow) -- ^ returns the window of the socket
plugGetSocketWindow self =
  maybeNull (makeNewGObject mkDrawWindow) $
  {# call gtk_plug_get_socket_window #}
    (toPlug self)
#endif

--------------------
-- Attributes

-- | @True@ if the plug is embedded in a socket.
--
-- Default value: @False@
--
-- * Available since Gtk+ version 2.12
--
plugAttrEmbedded :: PlugClass self => ReadAttr self Bool
plugAttrEmbedded = readAttrFromBoolProperty "embedded"

-- | The window of the socket the plug is embedded in.
--
-- * Available since Gtk+ version 2.14
--
plugAttrSocketWindow :: PlugClass self => ReadAttr self (Maybe DrawWindow)
plugAttrSocketWindow = readAttrFromMaybeObjectProperty "socket-window"
#if GTK_MAJOR_VERSION < 3
                       {# call pure unsafe gdk_window_object_get_type #}
#else
                       {# call pure unsafe gdk_window_get_type #}
#endif

--------------------
-- Signals

-- | Gets emitted when the plug becomes embedded in a socket and when the
-- embedding ends.
--
plugEmbedded :: PlugClass self => Signal self (IO ())
plugEmbedded = Signal (connect_NONE__NONE "embedded")

#endif
