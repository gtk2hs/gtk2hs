-- -*-haskell-*-
--  GIMP Toolkit (GTK) Attributes interface
--
--  Author : Duncan Coutts
--
--  Created: 21 January 2005
--
--  Copyright (C) 2005 Duncan Coutts
--
--  Partially derived from the hs-fltk and wxHaskell projects which
--  are both under LGPL compatible licenses.
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
-- Portability : non-portable (uses Gtk+ C library)
--
-- |
--
module System.Glib.Attributes where

-- | Object attributes can be get and set.
data Attr w a = Attr (w -> IO a) (w -> a -> IO ())

