--  GIMP Toolkit (GTK) - text layout functions @entry Rendering@
--
--  Author : Axel Simon
--          
--  Created: 8 Feburary 2003
--
--  Version $Revision: 1.1 $ from $Date: 2003/02/09 10:43:01 $
--
--  Copyright (c) 1999..2003 Axel Simon
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
-- * Functions to run the rendering pipeline.
--
-- @documentation@ ------------------------------------------------------------
--
-- * The Pango rendering pipeline takes a string of Unicode characters
--   and converts it into glyphs.  The functions described in this module
--   accomplish various steps of this process.
--
-- @todo@ ---------------------------------------------------------------------
--
--
module Rendering(
  PangoContext
  ) where

import Monad    (liftM)
import Foreign
import UTFCForeign
{#import Hierarchy#}
import GObject  (makeNewGObject)

{# context lib="pango" prefix="pango" #}

-- The constructor context_new is not really public and only enabled if
-- the header files are compiled with PANGO_ENABLE_BACKEND. The same holds
-- for pango_context_set_font_map, so we better restrict ourselved to the
-- gdk functions.


