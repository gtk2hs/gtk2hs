-- -*-haskell-*-
--  GIMP Toolkit (GTK) Keys
--
--  Author : Jens Petersen
--
--  Created: 24 May 2002
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/12 17:19:22 $
--
--  Copyright (C) 2002-2005 Jens Petersen
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
-- Maintainer  : gtk2hs-users\@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Gdk keyval functions.
--
-- TODO
--
--  * Documentation
--
module Graphics.UI.Gtk.Gdk.Keys (
  keyvalName,
  keyvalFromName
  ) where


import System.Glib.FFI

{#context lib="libgdk" prefix ="gdk"#}

{#fun pure keyval_name as ^ {fromIntegral `Integer'} -> `Maybe String'
    maybePeekUTFString#}
  where
  maybePeekUTFString = unsafePerformIO . (maybePeek peekCString)
--   maybePeekUTFString = maybePeek peekCString

{#fun pure keyval_from_name as ^ {`String'} -> `Integer' fromIntegral#}

