-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Plug@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2003/01/21 15:53:25 $
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
  NativeWindowId,
  plugNew,
  plugGetId
  ) where

import Monad	(liftM)
import Maybe	(fromMaybe)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Embedding (NativeWindowId)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor plugNew@ Create a new @ref data Window@ to hold another
-- application.
--
-- * The Plug may be constructed with a @ref data NativeWindowId@. In this
--   the foreign application will immediatly appear in this @ref data Plug@
--   once it is shown. If @literal Nothing@ is passed for @ref arg nmw@ a
--   @ref data NativeWindowId@ can be extracted from this @ref data Plug@
--   and be passed to the application which is to be embedded.
--
plugNew :: Maybe NativeWindowId -> IO Plug
plugNew mnw = makeNewObject mkPlug $ liftM castPtr $
  {#call unsafe plug_new#} (fromIntegral (fromMaybe 0 mnw))

-- @method plugGetId@ Retrieve the @ref data NativeWindowId@.
--
-- * The result should be passed to the application which is to be embedded.
--   See @ref constructor plugNew@.
--
plugGetId :: PlugClass p => p -> IO NativeWindowId
plugGetId p = liftM fromIntegral $ {#call unsafe plug_get_id#} (toPlug p)

-- @signal connectToEmbedded@ This plug received another application.
--
onEmbedded, afterEmbedded :: PlugClass p => p -> IO () -> IO (ConnectId p)
onEmbedded = connect_NONE__NONE "embedded" False
afterEmbedded = connect_NONE__NONE "embedded" True

