-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry TextTagTable TextBuffer@
--
--  Author : Axel Simon
--          
--  Created: 20 March 2002
--
--  Version $Revision: 1.5 $ from $Date: 2003/07/09 22:42:45 $
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- @description@ --------------------------------------------------------------
--
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
--
-- * Everything.
--
module TextTagTable(
  TextTagTable,
  TextTagTableClass,
  castToTextTagTable,
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

