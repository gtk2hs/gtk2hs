-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: CellRendererText
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
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
--- DESCRIPTION ---------------------------------------------------------------
--
-- * This widget derives from @CellRenderer. It provides the possibility to
--   display some text by setting the @Attribute @cellText to the column
--   of a @TreeModel by means of @treeViewAddAttribute from @TreeModelColumn.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

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

-- Create a new CellRendererText object. (EXPORTED)
--
cellRendererTextNew :: IO CellRendererText
cellRendererTextNew = makeNewObject mkCellRendererText $ liftM castPtr $
  {#call unsafe cell_renderer_text_new#}

-- helper function
--
strAttr str = AttrSingle str TMstring
	        (return.GVstring)
		(\(GVstring str) -> return str)

-- Define the attribute that specifies the text to be rendered. (EXPORTED)
--
cellText :: Attribute CellRendererText String
cellText = strAttr "text"

-- Define a markup string instead of a text. (EXPORTED)
--
cellMarkup :: Attribute CellRendererText String
cellMarkup = strAttr "markup"

-- A named color for the background paint. (EXPORTED)
--
cellBackground :: Attribute CellRendererText String
cellBackground = strAttr "background"

-- A named color for the foreground paint. (EXPORTED)
--
cellForeground :: Attribute CellRendererText String
cellForeground = strAttr "foreground"





-- Define the markup
