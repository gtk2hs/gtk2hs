-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget DrawingArea@
--
--  Author : Axel Simon
--          
--  Created: 22 September 2002
--
--  Version $Revision: 1.2 $ from $Date: 2002/11/03 20:35:45 $
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
-- * A user-defined widget.
--
-- @documentation@ ------------------------------------------------------------
--
-- * The @ref data DrawingArea@ widget is used for creating custom
--   user interface elements. It's essentially a blank widget. Drawing on
--   the @ref type Drawable@ returned by @ref method drawingAreaGetWindow@
--   has to be done each time the window manager sends @ref signal expose@
--   events. Note that the library automatically clears the exposed area to
--   the background color before sending the expose event, and that drawing
--   is implicitly clipped to the exposed area. Other events which are
--   interesting for interacting are mouse and butten events defined in
--   @ref data Widget@. If the widget changes in size (which it does
--   initially), a @ref signal configure@ event is emitted.
--
-- @todo@ ---------------------------------------------------------------------
--
module DrawingArea(
  DrawingArea,
  DrawingAreaClass,
  castToDrawingArea,
  drawingAreaNew,
  drawingAreaGetWindow,
  drawingAreaGetSize) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Structs	(drawingAreaGetWindow, drawingAreaGetSize)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor drawingAreaNew@ Create a new custom widget.
--
drawingAreaNew :: IO DrawingArea
drawingAreaNew = makeNewObject mkDrawingArea $ 
  liftM castPtr {#call unsafe drawing_area_new#}

