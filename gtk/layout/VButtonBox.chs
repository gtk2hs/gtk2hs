-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget VButtonBox@
--
--  Author : Matthew Walton
--          
--  Created: 28 April 2004
--
--  Version $Revision: 1.1 $ from $Date: 2004/04/30 13:45:32 $
--
--  Copyright (c) 2004 Matthew Walton
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

module VButtonBox (
  VButtonBox,
  VButtonBoxClass,
  castToVButtonBox,
  vButtonBoxNew
  ) where

import Monad (liftM)
import FFI

import Object (makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

--methods

-- @constructor vButtonBoxNew@
--
vButtonBoxNew :: IO VButtonBox
vButtonBoxNew = makeNewObject mkVButtonBox $
  liftM castPtr {#call unsafe vbutton_box_new#}

