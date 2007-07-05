-- GIMP Toolkit (GTK) Binding for Haskell: binding to libgnomevfs   -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Copyright (c) 2007 Peter Gavin
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.Gnome.VFS.Init (
  
-- * Initialization and Shutdown
  init,
  shutdown,
  initialized
  
  ) where

import System.Glib.FFI
import Control.Monad   (liftM)
import Prelude hiding  (init)

{# context lib = "gnomevfs" prefix = "gnome_vfs" #}

-- | If gnome-vfs is not already initialized, initialize it. This must
--   be called prior to performing any other gnome-vfs operations, and
--   may be called multiple times without error.
init :: IO Bool
init = liftM toBool {# call gnome_vfs_init #}

-- | Cease all active gnome-vfs operations and unload the MIME database
--   from memory.
shutdown :: IO ()
shutdown = {# call gnome_vfs_shutdown #}

-- | Detects if gnome-vfs has already been initialized (gnome-vfs must
--   be initialized prior to using any methods or operations).
initialized :: IO Bool
initialized = liftM toBool {# call gnome_vfs_initialized #}
