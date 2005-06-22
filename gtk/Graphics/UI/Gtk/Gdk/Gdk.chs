-- -*-haskell-*-
--  GIMP Toolkit (GTK) Gdk
--
--  Author : Jens Petersen <petersen@haskell.org>
--
--  Created: 6 June 2003
--
--  Version $Revision: 1.4 $ from $Date: 2005/06/22 16:00:48 $
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
-- TODO
--
-- Documentation
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Gdk general functions.
--
module Graphics.UI.Gtk.Gdk.Gdk (beep) where

{#context lib="gdk" prefix ="gdk"#}

beep :: IO ()
beep = {#call beep#}
