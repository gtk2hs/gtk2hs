--  GIMP Toolkit (GTK) @entry Enumeration types@     -*-haskell-*-@
--
--  Author : Jens Petersen
--  Created: 24 May 2002
--
--  Version $Revision: 1.2 $ from $Date: 2002/07/21 16:07:17 $
--
--  Copyright (c) 2002 Jens Petersen
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
-- @description@ --------------------------------------------------------------
--
--  Gdk keyval functions.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
--
--  * Documentation
--
module GdkKeys(
  keyvalName,
  keyvalFromName
  ) where

import UTFCForeign
import Foreign
import LocalData(unsafePerformIO)

{#context lib="libgdk" prefix ="gdk"#}

{#fun pure keyval_name as ^ {fromIntegral `Integer'} -> `Maybe String'
    maybePeekCString#}
  where
  maybePeekCString = unsafePerformIO . (maybePeek peekCString)
--   maybePeekCString = maybePeek peekCString

{#fun pure keyval_from_name as ^ {`String'} -> `Integer' fromIntegral#}

