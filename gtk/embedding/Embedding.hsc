-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Socket
--
--  Author : Axel Simon
--          
--  Created: 20 January 2003
--
--  Version $Revision: 1.4 $ from $Date: 2004/12/09 18:25:45 $
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
-- |
--
-- TODO
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
import Control.Exception

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

