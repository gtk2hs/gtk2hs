--  GIMP Toolkit (GTK) Binding for Haskell: binding to libgnomevfs -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Copyright (c) 2007 Peter Gavin
--
--  This library is free software: you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public License
--  as published by the Free Software Foundation, either version 3 of
--  the License, or (at your option) any later version.
--  
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--  
--  You should have received a copy of the GNU Lesser General Public
--  License along with this program.  If not, see
--  <http://www.gnu.org/licenses/>.
--  
--  GnomeVFS, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GnomeVFS documentation,
--  Copyright (c) 2001 Seth Nickell <snickell@stanford.edu>. The
--  documentation is covered by the GNU Free Documentation License,
--  version 1.2.
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
