-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Socket
--
--  Author : Axel Simon
--
--  Created: 20 January 2003
--
--  Version $Revision: 1.3 $ from $Date: 2005/05/08 12:58:40 $
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
-- TODO
--
-- * NativeWindowId is a CUInt for c2hs and a Word32 for hsc2hs. I used
--   fromIntegral to make it work, but it doesn't feel right.
--
module Graphics.UI.Gtk.Embedding.Embedding (
  socketHasPlug,
  NativeWindowId
  ) where

import Control.Exception

import System.Glib.FFI
import Graphics.UI.Gtk.Types

#include<gtk/gtk.h>

-- | The identifer of a window to be embedded.
--
type NativeWindowId = #type GdkNativeWindow

-- | Test if a Plug is connected to the socket.
-- 
socketHasPlug :: SocketClass s => s -> IO Bool
socketHasPlug socket = do
  plugPtr <- withForeignPtr (unSocket (toSocket socket))
	     #{peek GtkSocket, plug_window}
  return (plugPtr/=nullPtr)

