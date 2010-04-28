-- -*-haskell-*-
--  GIMP Toolkit (GTK) General
--
--  Author : Axel Simon
--
--  Created: 9 May 2009
--
--  Copyright (C) 2009 Axel Simon
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
-- Support for the threaded RTS of ghc.
--
-- This file contains functions that are needed by other library wrappers that build
-- on Gtk2Hs. An application should not need this function nor include this file.
--
module Graphics.UI.Gtk.General.Threading (
  objectUnrefFromMainloop
  ) where

import System.Glib.FFI

foreign import ccall unsafe "hsgthread.h &gtk2hs_g_object_unref_from_mainloop"
  objectUnrefFromMainloop :: FinalizerPtr a
