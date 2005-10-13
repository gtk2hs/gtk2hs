-- -*-haskell-*-
--  GIMP Toolkit (GTK) Gdk
--
--  Author : Jens Petersen <petersen@haskell.org>
--
--  Created: 6 June 2003
--
--  Version $Revision: 1.5 $ from $Date: 2005/10/13 23:35:39 $
--
--  Copyright (C) 2003-2005 Jens-Ulrik Holger Petersen
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
-- Gdk general functions.
--
module Graphics.UI.Gtk.Gdk.Gdk (
  beep,
  flush,
  ) where

{#context lib="gdk" prefix ="gdk"#}

-- | Emits a short beep.
--
beep :: IO ()
beep = {#call beep#}

-- | Flushes the X output buffer and waits until all requests have been
-- processed by the server. This is rarely needed by applications.
--
flush :: IO ()
flush = {#call flush#}
