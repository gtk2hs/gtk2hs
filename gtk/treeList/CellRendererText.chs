-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry CellRendererText@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.4 $ from $Date: 2002/07/08 13:22:46 $
--
--  Copyright (c) 1999..2002 Axel Simon
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
-- * This widget derives from @CellRenderer. It provides the possibility to
--   display some text by setting the @Attribute @cellText to the column
--   of a @TreeModel by means of @treeViewAddAttribute from @TreeModelColumn.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

module CellRendererText(
  CellRendererText,
  CellRendererTextClass,
  castToCellRendererText,
  cellRendererTextNew,
  cellText,
  cellMarkup,
  cellBackground,
  cellForeground
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import CellRenderer (Attribute(..))
import StoreValue   (GenericValue(..), TMType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor cellRendererTextNew@ Create a new CellRendererText object.
--
cellRendererTextNew :: IO CellRendererText
cellRendererTextNew  = makeNewObject mkCellRendererText $ liftM castPtr $
  {#call unsafe cell_renderer_text_new#}

-- helper function
--
strAttr str = Attribute str TMstring
	        (return.GVstring)
		(\(GVstring str) -> return str)

-- @method cellText@ Define the attribute that specifies the text to be
-- rendered.
--
cellText :: Attribute rendererText String
cellText  = strAttr "text"

-- @method cellMarkup@ Define a markup string instead of a text.
--
cellMarkup :: Attribute rendererText String
cellMarkup  = strAttr "markup"

-- @method cellBackground@ A named color for the background paint.
--
cellBackground :: Attribute rendererText String
cellBackground  = strAttr "background"

-- @method cellForeground@ A named color for the foreground paint.
--
cellForeground :: Attribute rendererText String
cellForeground  = strAttr "foreground"





-- Define the markup
