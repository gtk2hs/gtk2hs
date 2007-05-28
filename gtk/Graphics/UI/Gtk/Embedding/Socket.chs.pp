-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Socket
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

#if !defined(WIN32) || GTK_CHECK_VERSION(2,8,0)
-- * Types
  Socket,
  SocketClass,
  castToSocket,
  toSocket,
  NativeWindowId,

-- * Constructors
  socketNew,

-- * Methods
  socketHasPlug,
  socketAddId,
  socketGetId,

-- * Signals
  onPlugAdded,
  afterPlugAdded,
  onPlugRemoved,
  afterPlugRemoved,
#endif
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.Embedding.Embedding

{# context lib="gtk" prefix="gtk" #}

#if !defined(WIN32) || GTK_CHECK_VERSION(2,8,0)

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
    (fromIntegral windowId)

-- | Gets the window ID of a 'Socket' widget, which can then be used to create
-- a client embedded inside the socket, for instance with
-- 'Graphics.UI.Gtk.Embedding.Plug.plugNew'.
--
-- The 'Socket' must have already be added into a toplevel window before you
-- can make this call.
--
socketGetId :: SocketClass self => self -> IO NativeWindowId
socketGetId self =
  liftM fromIntegral $
  {# call unsafe socket_get_id #}
    (toSocket self)

--------------------
-- Signals

-- | This signal is emitted when a client is successfully added to the socket.
--
onPlugAdded, afterPlugAdded :: SocketClass self => self
 -> IO ()
 -> IO (ConnectId self)
onPlugAdded = connect_NONE__NONE "plug-added" False
afterPlugAdded = connect_NONE__NONE "plug-added" True

-- | This signal is emitted when a client is removed from the socket.
--
onPlugRemoved, afterPlugRemoved :: SocketClass self => self
 -> IO ()
 -> IO (ConnectId self)
onPlugRemoved = connect_NONE__NONE "plug-removed" False
afterPlugRemoved = connect_NONE__NONE "plug-removed" True

#endif
