-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget DrawingArea
--
--  Author : Axel Simon
--
--  Created: 22 September 2002
--
--  Version $Revision: 1.5 $ from $Date: 2005/03/16 02:32:27 $
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A widget for custom user interface elements
--
module Graphics.UI.Gtk.Misc.DrawingArea (
-- * Detail
-- 
-- | The 'DrawingArea' widget is used for creating custom user interface
-- elements. It's essentially a blank widget; you can draw on
-- the 'Drawable' returned by 'drawingAreaGetWindow'.
--
-- After creating a drawing area, the application may want to connect to:
--
-- * Mouse and button press signals to respond to input from the user. (Use
-- 'widgetAddEvents' to enable events you wish to receive.)
--
-- * The \"realize\" signal to take any necessary actions when the widget is
-- instantiated on a particular display. (Create GDK resources in response to
-- this signal.)
--
-- * The \"configure_event\" signal to take any necessary actions when the
-- widget changes size.
--
-- * The \"expose_event\" signal to handle redrawing the contents of the
-- widget.
--
-- Expose events are normally delivered when a drawing area first comes
-- onscreen, or when it's covered by another window and then uncovered
-- (exposed). You can also force an expose event by adding to the \"damage
-- region\" of the drawing area's window; 'widgetQueueDrawArea' and
-- 'windowInvalidateRect' are equally good ways to do this. You\'ll then get an
-- expose event for the invalid region.
--
-- The available routines for drawing are documented on the GDK Drawing
-- Primitives page. See also 'pixbufRenderToDrawable' for drawing a 'Pixbuf'.
--
-- To receive mouse events on a drawing area, you will need to enable them
-- with 'widgetAddEvents'. To receive keyboard events, you will need to set the
-- 'CanFocus' flag on the drawing area, and should probably draw some
-- user-visible indication that the drawing area is focused.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----DrawingArea
-- |                     +----'Curve'
-- @

-- * Types
  DrawingArea,
  DrawingAreaClass,
  castToDrawingArea,

-- * Constructors
  drawingAreaNew,

-- * Methods
  drawingAreaGetDrawWindow,
  drawingAreaGetSize
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Structs	(drawingAreaGetDrawWindow, drawingAreaGetSize)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new drawing area.
--
drawingAreaNew :: IO DrawingArea
drawingAreaNew =
  makeNewObject mkDrawingArea $ liftM castPtr $
  {# call unsafe drawing_area_new #}
