-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Plug
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
-- Plug is a window that is to be attached to the window of another
-- application. If you have managed to receive the 'XID' from
-- the inviting application you can construct the Plug and add your widgets
-- to it.
--

module Graphics.UI.Gtk.Embedding.Plug (
  Plug,
  PlugClass,
  castToPlug,
  NativeWindowId,
  plugNew,
  plugGetId
  ) where

import Monad	(liftM)
import Maybe	(fromMaybe)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.Embedding.Embedding (NativeWindowId)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new 'Window' to hold another
-- application.
--
-- * The Plug may be constructed with a 'NativeWindowId'. In this
--   the foreign application will immediatly appear in this 'Plug'
--   once it is shown. If @Nothing@ is passed for @nmw@ a
--   'NativeWindowId' can be extracted from this 'Plug'
--   and be passed to the application which is to be embedded.
--
plugNew :: Maybe NativeWindowId -> IO Plug
plugNew mnw = makeNewObject mkPlug $ liftM castPtr $
  {#call unsafe plug_new#} (fromIntegral (fromMaybe 0 mnw))

-- | Retrieve the 'NativeWindowId'.
--
-- * The result should be passed to the application which is to be embedded.
--   See 'plugNew'.
--
plugGetId :: PlugClass p => p -> IO NativeWindowId
plugGetId p = liftM fromIntegral $ {#call unsafe plug_get_id#} (toPlug p)

-- | This plug received another application.
--
onEmbedded, afterEmbedded :: PlugClass p => p -> IO () -> IO (ConnectId p)
onEmbedded = connect_NONE__NONE "embedded" False
afterEmbedded = connect_NONE__NONE "embedded" True

