-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Plug@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2003/01/18 17:53:52 $
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
-- * Plug is a window that is to be attached to the window of another
--   application. If you have managed to receive the @ref type XID@ from
--   the inviting application you can construct the Plug and add your widgets
--   to it.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

module Plug(
  Plug,
  PlugClass,
  castToPlug,
  plugNew
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Structs	(XID)

{# context lib="gtk" prefix="gtk" #}

-- methods

plugNew :: XID -> IO Plug
plugNew nw = makeNewObject mkPlug $ liftM castPtr $
  {#call unsafe plug_new#} nw
