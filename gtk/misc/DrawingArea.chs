-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget DrawingArea
--
--  Author : Axel Simon
--          
--  Created: 22 September 2002
--
--  Version $Revision: 1.5 $ from $Date: 2004/05/23 16:07:53 $
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
-- |
--
-- A user-defined widget.
--
-- * The 'DrawingArea' widget is used for creating custom
--   user interface elements. It's essentially a blank widget. Drawing on
--   the 'Drawable' returned by 'drawingAreaGetWindow'
--   has to be done each time the window manager sends @\"expose\"@
--   events. Note that the library automatically clears the exposed area to
--   the background color before sending the expose event, and that drawing
--   is implicitly clipped to the exposed area. Other events which are
--   interesting for interacting are mouse and butten events defined in
--   'Widget'. If the widget changes in size (which it does
--   initially), a @\"configure\"@ event is emitted.
--
module DrawingArea(
  DrawingArea,
  DrawingAreaClass,
  castToDrawingArea,
  drawingAreaNew,
  drawingAreaGetDrawWindow,
  drawingAreaGetSize) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Structs	(drawingAreaGetDrawWindow, drawingAreaGetSize)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new custom widget.
--
drawingAreaNew :: IO DrawingArea
drawingAreaNew = makeNewObject mkDrawingArea $ 
  liftM castPtr {#call unsafe drawing_area_new#}

