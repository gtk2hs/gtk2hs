-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Plug
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/25 01:11:33 $
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
-- Toplevel for embedding into other processes.
--
module Graphics.UI.Gtk.Embedding.Plug (
-- * Description
-- 
-- | Together with 'Socket', 'Plug' provides the ability to embed widgets from
-- one process into another process in a fashion that is transparent to the
-- user. One process creates a 'Socket' widget and, passes the ID of that
-- widgets window to the other process, which then creates a 'Plug' with that
-- window ID. Any widgets contained in the 'Plug' then will appear inside the
-- first applications window.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Window'
-- |                                 +----Plug
-- @

-- * Types
  Plug,
  PlugClass,
  castToPlug,
  NativeWindowId,

-- * Constructors
  plugNew,

-- * Methods
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

--------------------
-- Constructors

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

--------------------
-- Methods

-- | Retrieve the 'NativeWindowId'.
--
-- * The result should be passed to the application which is to be embedded.
--   See 'plugNew'.
--
plugGetId :: PlugClass p => p -> IO NativeWindowId
plugGetId p = liftM fromIntegral $ {#call unsafe plug_get_id#} (toPlug p)

--------------------
-- Signals

-- | This plug received another application.
--
onEmbedded, afterEmbedded :: PlugClass p => p -> IO () -> IO (ConnectId p)
onEmbedded = connect_NONE__NONE "embedded" False
afterEmbedded = connect_NONE__NONE "embedded" True

