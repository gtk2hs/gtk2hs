-- -*-haskell-*-

#include <gtk/gtk.h>
#include "template-hsc-gtk2hs.h"

--  GIMP Toolkit (GTK) Widget Socket
--
--  Author : Axel Simon
--
--  Created: 20 January 2003
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
module Graphics.UI.Gtk.Embedding.Embedding (
#if defined(HAVE_PLUG_AND_SOCKET) && (!defined(WIN32) || GTK_CHECK_VERSION(2,8,0)) && GTK_MAJOR_VERSION < 3
  socketHasPlug,
#endif
  ) where

#if defined(HAVE_PLUG_AND_SOCKET) && (!defined(WIN32) || GTK_CHECK_VERSION(2,8,0)) && GTK_MAJOR_VERSION < 3
import System.Glib.FFI
import Graphics.UI.Gtk.Types
import Graphics.UI.Gtk.Embedding.Types

-- | Test if a Plug is connected to the socket.
--
socketHasPlug :: SocketClass s => s -> IO Bool
socketHasPlug socket = do
  plugPtr <- withForeignPtr (unSocket (toSocket socket))
             #{peek GtkSocket, plug_window}
  return (plugPtr/=nullPtr)

#endif
