-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Socket
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:19 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
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
--- DESCRIPTION ---------------------------------------------------------------
--
-- * This widget provides the possibility that other application display their
--   widgets within this application.
--
--- DOCU ----------------------------------------------------------------------
--
-- * After creation of the Socket, you may retrieve the @XID of the socket. 
--   For this to work, the socket must at least be realized (e.g. shown).
--
-- * The application has to make sure the Socket is not destroyed while the
--   other application tries to connect. If the @XID was transmitted, the
--   inviting application can check with @socketHasPlug if the plug has
--   already connected.
--
--- TODO ----------------------------------------------------------------------

module Socket(
  Socket,
  SocketClass,
  castToSocket,
  socketNew,
  XID,
--  socketGetXID,
  socketHasPlug
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Structs	(XID, --socketGetXID, 
  socketHasPlug)

{# context lib="gtk" prefix="gtk" #}

-- methods

socketNew :: IO Socket
socketNew = makeNewObject mkSocket $ liftM castPtr {#call unsafe socket_new#}

