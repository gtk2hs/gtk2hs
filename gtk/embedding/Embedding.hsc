-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Socket@
--
--  Author : Axel Simon
--          
--  Created: 20 January 2003
--
--  Version $Revision: 1.2 $ from $Date: 2003/07/09 22:42:43 $
--
--  Copyright (c) 1999..2003 Axel Simon
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
-- @description@ --------------------------------------------------------------
--
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
--
-- * NativeWindowId is a CUInt for c2hs and a Word32 for hsc2hs. I used
--   fromIntegral to make it work, but it doesn't feel right.
--
module Embedding(
  socketHasPlug,
  NativeWindowId
  ) where

import FFI

import Hierarchy
import Exception

#include<gtk/gtk.h>

-- @type NativeWindowId@ The identifer of a window to be embedded.
--
type NativeWindowId = #type GdkNativeWindow

-- @method socketHasPlug@ Test if a Plug is connected to the socket.
-- 
socketHasPlug :: SocketClass s => s -> IO Bool
socketHasPlug socket = do
  plugPtr <- withForeignPtr (unSocket (toSocket socket))
	     #{peek GtkSocket, plug_window}
  return (plugPtr/=nullPtr)

