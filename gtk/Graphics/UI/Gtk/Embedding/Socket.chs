-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Socket
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/25 01:11:33 $
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
-- Container for widgets from other processes.
--
module Graphics.UI.Gtk.Embedding.Socket (
-- * Description
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
--
-- A socket can also be used to swallow arbitrary pre-existing top-level
-- windows using 'socketSteal', though the integration when this is done will
-- not be as close as between a 'Plug' and a 'Socket'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----Socket
-- @

-- * Types
  Socket,
  SocketClass,
  castToSocket,
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
  afterPlugRemoved
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.Embedding.Embedding	(NativeWindowId, socketHasPlug)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a 'Container' for embedding.
--
-- * 'Socket' is a 'Container' for foreign applications
--   that support the XEMBED protocol. To connect two applications the
--   'NativeWindowId' has to be passed either from this socket
--   to the other application's 'Plug' or vice versa.
--
socketNew :: IO Socket
socketNew = makeNewObject mkSocket $ liftM castPtr {#call unsafe socket_new#}

--------------------
-- Methods

-- | Insert another application into this socket.
--
-- * Inserts the other application into this plug. The
--   'NativeWindowId' comes from the other application.
--
-- * The 'Socket' must have already be added into a toplevel
--   window before you can make this call.
--
socketAddId :: SocketClass s => s -> NativeWindowId -> IO ()
socketAddId soc nwi = {#call unsafe socket_add_id#} (toSocket soc) 
		      (fromIntegral nwi)

-- | Prepare to insert this application into another.
--
-- * The extracted 'NativeWindowId' can be passed to another
--   application which can then embed this socket 'Container'.
--
socketGetId :: SocketClass s => s -> IO NativeWindowId
socketGetId soc = liftM fromIntegral $
		  {#call unsafe socket_get_id#} (toSocket soc)

--------------------
-- Signals

-- | This socket was added into another application.
--
onPlugAdded, afterPlugAdded :: SocketClass s => s -> IO () -> IO (ConnectId s)
onPlugAdded = connect_NONE__NONE "plug-added" False
afterPlugAdded = connect_NONE__NONE "plug-added" True

-- | This socket was removed from another
-- application.
--
onPlugRemoved, afterPlugRemoved :: SocketClass s => s -> IO () -> 
							 IO (ConnectId s)
onPlugRemoved = connect_NONE__NONE "plug-removed" False
afterPlugRemoved = connect_NONE__NONE "plug-removed" True
