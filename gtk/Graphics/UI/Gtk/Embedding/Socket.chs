-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Socket
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:15:19 $
--
--  Copyright (c) 1999..2002 Axel Simon
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
-- |
--
-- This widget provides the possibility that other application display their
-- widgets within this application.
--
-- * After creation of the Socket, you may retrieve the 
--   'NativeWindow' of the socket. 
--   For this to work, the socket must at least be realized (e.g. shown).
--
-- * The application has to make sure the 'Socket'
--   is not destroyed while the
--   other application tries to connect. If the 'NativeWindow' was 
--   transmitted, the
--   inviting application can check with 'socketHasPlug' if the 
--   plug has
--   already connected.
--

module Graphics.UI.Gtk.Embedding.Socket (
  Socket,
  SocketClass,
  castToSocket,
  NativeWindowId,
  socketNew,
  socketHasPlug,
  socketAddId,
  socketGetId,
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

-- methods

-- | Create a 'Container' for embedding.
--
-- * 'Socket' is a 'Container' for foreign applications
--   that support the XEMBED protocol. To connect two applications the
--   'NativeWindowId' has to be passed either from this socket
--   to the other application's 'Plug' or vice versa.
--
socketNew :: IO Socket
socketNew = makeNewObject mkSocket $ liftM castPtr {#call unsafe socket_new#}

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
