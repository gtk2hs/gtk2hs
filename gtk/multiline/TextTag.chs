-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry TextTag for the editor widget@
--
--  Author : Axel Simon
--          
--  Created: 24 February 2002
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/24 09:43:25 $
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

module TextTag(
  TextTag,
  TextTagClass,
  castToTextTag,
  TagName
  ) where

import Monad	(liftM)
import Foreign
import CForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

type TagName = String

-- methods

