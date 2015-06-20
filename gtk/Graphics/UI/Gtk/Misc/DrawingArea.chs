{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget DrawingArea
--
--  Author : Axel Simon
--
--  Created: 22 September 2002
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
-- the 'Drawable' returned by 'drawingAreaGetDrawWindow'.
--
-- After creating a drawing area, the application may want to connect to:
--
-- * Mouse and button press signals to respond to input from the user.
--
-- * The 'realize' signal to take any necessary actions when the widget is
-- instantiated on a particular display. (Create GDK resources in response to
-- this signal.)
--
-- * The 'configureEvent' signal to take any necessary actions when the
-- widget changes size.
--
-- * The 'exposeEvent' signal to handle redrawing the contents of the
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
-- Primitives page.
--
-- To receive mouse events on a drawing area, you will need to enable them
-- with 'widgetAddEvents'. To receive keyboard events, you will need to set the
-- 'widgetCanFocus' attribute on the drawing area, and should probably draw some
-- user-visible indication that the drawing area is focused.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----DrawingArea
-- @

-- * Types
  DrawingArea,
  DrawingAreaClass,
  castToDrawingArea, gTypeDrawingArea,
  toDrawingArea,

-- * Constructors
  drawingAreaNew,

-- * Methods
#if GTK_MAJOR_VERSION < 3
  drawingAreaGetDrawWindow,
  drawingAreaGetSize
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
#if GTK_MAJOR_VERSION < 3
import Graphics.UI.Gtk.General.Structs  (widgetGetDrawWindow, widgetGetSize)
#endif

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new drawing area.
--
drawingAreaNew :: IO DrawingArea
drawingAreaNew =
  makeNewObject mkDrawingArea $
  liftM (castPtr :: Ptr Widget -> Ptr DrawingArea) $
  {# call unsafe drawing_area_new #}

#if GTK_MAJOR_VERSION < 3
-- | See 'widgetGetDrawWindow'
--
-- Removed in Gtk3.
drawingAreaGetDrawWindow :: DrawingArea -> IO DrawWindow
drawingAreaGetDrawWindow = widgetGetDrawWindow
{-# DEPRECATED drawingAreaGetDrawWindow "use widgetGetDrawWindow instead" #-}

-- | See 'widgetGetSize'
--
-- Removed in Gtk3.
drawingAreaGetSize :: DrawingArea -> IO (Int, Int)
drawingAreaGetSize = widgetGetSize
{-# DEPRECATED drawingAreaGetSize "use widgetGetSize instead" #-}
#endif
