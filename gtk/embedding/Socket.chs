-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Socket@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.4 $ from $Date: 2003/01/18 18:19:26 $
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
-- @description@ --------------------------------------------------------------
--
-- * This widget provides the possibility that other application display their
--   widgets within this application.
--
-- @documentation@ ------------------------------------------------------------
--
-- * After creation of the Socket, you may retrieve the @ref data XID@ of the
--   socket. 
--   For this to work, the socket must at least be realized (e.g. shown).
--
-- * The application has to make sure the Socket is not destroyed while the
--   other application tries to connect. If the @ref data XID@ was 
--   transmitted, the
--   inviting application can check with @ref method socketHasPlug@ if the 
--   plug has
--   already connected.
--
-- @todo@ ---------------------------------------------------------------------

module Socket(
  Socket,
  SocketClass,
  castToSocket,
  socketNew,
  XID,
  socketGetXID,
  socketHasPlug
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Structs	(XID, socketGetXID, socketHasPlug)

{# context lib="gtk" prefix="gtk" #}

-- methods

socketNew :: IO Socket
socketNew = makeNewObject mkSocket $ liftM castPtr {#call unsafe socket_new#}

