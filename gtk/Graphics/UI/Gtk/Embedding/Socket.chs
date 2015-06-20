{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Socket
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
-- Container for widgets from other processes
--
module Graphics.UI.Gtk.Embedding.Socket (
-- * Detail
--
-- | Together with 'Plug', 'Socket' provides the ability to embed widgets from
-- one process into another process in a fashion that is transparent to the
-- user. One process creates a 'Socket' widget and, passes the that widget's
-- window ID to the other process, which then creates a 'Plug' with that window
-- ID. Any widgets contained in the 'Plug' then will appear inside the first
-- applications window.
--
-- The socket's window ID is obtained by using 'socketGetId'. Before using
-- this function, the socket must have been realized, and for hence, have been
-- added to its parent.
--
-- * Obtaining the window ID of a socket.
--
-- > socket <- socketNew
-- > widgetShow socket
-- > containerAdd parent socket
-- >
-- > -- The following call is only necessary if one of
-- > -- the ancestors of the socket is not yet visible.
-- > --
-- > widgetRealize socket
-- > socketId <- socketGetId socket
-- > putStrLn ("The ID of the sockets window is " ++ show socketId)
--
-- Note that if you pass the window ID of the socket to another process that
-- will create a plug in the socket, you must make sure that the socket widget
-- is not destroyed until that plug is created. Violating this rule will cause
-- unpredictable consequences, the most likely consequence being that the plug
-- will appear as a separate toplevel window. You can check if the plug has
-- been created by calling 'socketHasPlug'.
-- If this returns @True@, then the plug has been successfully created inside
--  of the socket.
--
-- When Gtk+ is notified that the embedded window has been destroyed, then
-- it will destroy the socket as well. You should always, therefore, be
-- prepared for your sockets to be destroyed at any time when the main event
-- loop is running.
--
-- The communication between a 'Socket' and a 'Plug' follows the XEmbed
-- protocol. This protocol has also been implemented in other toolkits, e.g.
-- Qt, allowing the same level of integration when embedding a Qt widget in
-- Gtk+ or vice versa.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----Socket
-- @

#if (defined(HAVE_PLUG_AND_SOCKET) && (!defined(WIN32) || GTK_CHECK_VERSION(2,8,0))) || defined(GDK_WINDOWING_X11)
-- * Types
  Socket,
  SocketClass,
  castToSocket, gTypeSocket,
  toSocket,
  NativeWindowId,

-- * Constructors
  socketNew,

-- * Methods
  socketHasPlug,
  socketAddId,
  socketGetId,
#if GTK_CHECK_VERSION(2,14,0)
  socketGetPlugWindow,
#endif

-- * Signals
  socketPlugAdded,
  socketPlugRemoved,

-- * Deprecated
#ifndef DISABLE_DEPRECATED
  onPlugAdded,
  afterPlugAdded,
  onPlugRemoved,
  afterPlugRemoved,
#endif
#endif
  ) where

#if (defined(HAVE_PLUG_AND_SOCKET) && (!defined(WIN32) || GTK_CHECK_VERSION(2,8,0))) || defined(GDK_WINDOWING_X11)

import Control.Monad    (liftM)
import Data.Maybe (isJust)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object          (makeNewObject)
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

-- | Create a new empty 'Socket'.
--
-- 'Socket' is a 'Container' for foreign applications that support the XEMBED
-- protocol. To connect two applications the 'NativeWindowId' has to be passed
-- either from this socket to the other application's 'Plug' or vice versa.
--
socketNew :: IO Socket
socketNew =
  makeNewObject mkSocket $
  liftM (castPtr :: Ptr Widget -> Ptr Socket) $
  {# call unsafe socket_new #}

--------------------
-- Methods

-- | Adds an XEMBED client, such as a 'Plug', to the 'Socket'. The client may
-- be in the same process or in a different process.
--
-- To embed a 'Plug' in a 'Socket', you can either create the 'Plug' with
-- @Graphics.UI.Gtk.Embedding.Plug.plugNew Nothing@, call
-- 'Graphics.UI.Gtk.Embedding.Plug.plugGetId' to get the window ID of the
-- plug, and then pass that to the 'socketAddId', or you can call
-- 'socketGetId' to get the window ID for the socket, and call
-- 'Graphics.UI.Gtk.Embedding.Plug.plugNew' passing in that ID.
--
-- The 'Socket' must have already be added into a toplevel window before you
-- can make this call.
--
socketAddId :: SocketClass self => self
 -> NativeWindowId -- ^ @windowId@ - the window ID of a client
                        -- participating in the XEMBED protocol.
 -> IO ()
socketAddId self windowId =
  {# call unsafe socket_add_id #}
    (toSocket self)
    (fromNativeWindowId windowId)

-- | Gets the window ID of a 'Socket' widget, which can then be used to create
-- a client embedded inside the socket, for instance with
-- 'Graphics.UI.Gtk.Embedding.Plug.plugNew'.
--
-- The 'Socket' must have already be added into a toplevel window before you
-- can make this call.
--
socketGetId :: SocketClass self => self -> IO NativeWindowId
socketGetId self =
  liftM toNativeWindowId $
  {# call unsafe socket_get_id #}
    (toSocket self)

#if GTK_CHECK_VERSION(2,14,0)
-- | Retrieves the window of the plug. Use this to check if the plug has been
-- created inside of the socket.
--
-- * Available since Gtk+ version 2.14
--
socketGetPlugWindow :: SocketClass self => self
 -> IO (Maybe DrawWindow) -- ^ returns the window of the plug if available,
                          -- or Nothing
socketGetPlugWindow self =
  maybeNull (makeNewGObject mkDrawWindow) $
  {# call gtk_socket_get_plug_window #}
    (toSocket self)

#if GTK_MAJOR_VERSION >= 3
socketHasPlug :: SocketClass s => s -> IO Bool
socketHasPlug = liftM isJust . socketGetPlugWindow
#endif

#endif

--------------------
-- Signals

-- | This signal is emitted when a client is successfully added to the socket.
--
socketPlugAdded :: SocketClass self => Signal self (IO ())
socketPlugAdded = Signal (connect_NONE__NONE "plug-added")

-- | This signal is emitted when a client is removed from the socket. The
-- default action is to destroy the 'Socket' widget, so if you want to reuse it
-- you must add a signal handler that returns @True@.
--
socketPlugRemoved :: SocketClass self => Signal self (IO Bool)
socketPlugRemoved = Signal (connect_NONE__BOOL "plug-removed")

--------------------
-- Deprecated Signals

#ifndef DISABLE_DEPRECATED
onPlugAdded :: SocketClass self => self
 -> IO ()
 -> IO (ConnectId self)
onPlugAdded = connect_NONE__NONE "plug-added" False
{-# DEPRECATED onPlugAdded "instead of 'onPlugAdded obj' use 'on obj socketPlugAdded'" #-}

afterPlugAdded :: SocketClass self => self
 -> IO ()
 -> IO (ConnectId self)
afterPlugAdded = connect_NONE__NONE "plug-added" True
{-# DEPRECATED afterPlugAdded "instead of 'afterPlugAdded obj' use 'after obj socketPlugAdded'" #-}

onPlugRemoved :: SocketClass self => self
 -> IO Bool
 -> IO (ConnectId self)
onPlugRemoved = connect_NONE__BOOL "plug-removed" False
{-# DEPRECATED onPlugRemoved "instead of 'onPlugRemoved obj' use 'on obj socketPlugRemoved'" #-}

afterPlugRemoved :: SocketClass self => self
 -> IO Bool
 -> IO (ConnectId self)
afterPlugRemoved = connect_NONE__BOOL "plug-removed" True
{-# DEPRECATED afterPlugRemoved "instead of 'afterPlugRemoved obj' use 'after obj socketPlugRemoved'" #-}
#endif

#endif
