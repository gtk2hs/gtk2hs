--  -*-haskell-*-
--  GIMP Toolkit (GTK) Gdk
--
--  Author : Jens Petersen <petersen@haskell.org>
--  Created: 2003-06-06
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:18:00 $
--
--  Copyright (c) 2003 Jens-Ulrik Holger Petersen
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
-- |
--
-- Gdk general functions.
--
-- TODO
--
--  * Documentation
--
module Graphics.UI.Gtk.Gdk.Gdk (beep) where

{#context lib="libgdk" prefix ="gdk"#}

beep :: IO ()
beep = {#call beep#}
