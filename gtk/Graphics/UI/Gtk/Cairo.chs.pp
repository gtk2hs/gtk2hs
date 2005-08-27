-- -*-haskell-*-
--  GIMP Toolkit (GTK) Cairo GDK integration
--
--  Author : Duncan Coutts
--
--  Created: 17 August 2005
--
--  Version $Revision: 1.2 $ from $Date: 2005/08/27 14:42:56 $
--
--  Copyright (C) 2005 Duncan Coutts
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
-- 
-- Cairo interaction - functions to support using Cairo
--
module Graphics.UI.Gtk.Cairo (
-- * Detail
--
-- | Cairo is a graphics library that supports vector graphics and image
-- compositing that can be used with Gdk. Since 2.8, Gtk+ does most of its
-- drawing using Cairo.
--
-- Gdk does not wrap the Cairo API, instead it allows to create Cairo contexts
-- which can be used to draw on Gdk 'Drawable's. Additional functions allow to
-- convert Gdk's rectangles and regions into Cairo paths and to use 'Pixbuf's as
-- sources for drawing operations.

-- * Methods
#if GTK_CHECK_VERSION(2,8,0) && defined(ENABLE_CAIRO)
  cairoCreate,
  cairoRectangle,
  cairoRegion,
  cairoSetSourceColor,
  cairoSetSourcePixbuf,
#endif
  ) where

import Monad	(liftM)

import System.Glib.FFI
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Gdk.Region#} (Region(..))
import Graphics.UI.Gtk.General.Structs (Rectangle(..), Color(..))

#if GTK_CHECK_VERSION(2,8,0) && defined(ENABLE_CAIRO)
{#import Graphics.Rendering.Cairo.Types#} as Cairo
import Graphics.Rendering.Cairo.Internal as Cairo
#endif

{# context lib="gdk" prefix="gdk" #}

--------------------
-- Methods

#if GTK_CHECK_VERSION(2,8,0) && defined(ENABLE_CAIRO)
-- | Creates a Cairo context for drawing to drawable.
--
-- * Available since Gtk+ version 2.8
--
cairoCreate :: DrawableClass drawable =>
    drawable -- ^ drawable - a 'Drawable'
 -> IO Cairo -- ^ A newly created Cairo context.
cairoCreate drawable =
  liftM Cairo $
  {# call unsafe gdk_cairo_create #}
    (toDrawable drawable)

-- | Sets the specified 'Color' as the source color of @cr@.
--
-- * Available since Gtk+ version 2.8
--
cairoSetSourceColor :: Cairo -> Color -> IO ()
cairoSetSourceColor cr (Color red green blue) =
  Cairo.setSourceRGB cr
    (realToFrac red   / 65535.0)
    (realToFrac green / 65535.0)
    (realToFrac blue  / 65535.0)

-- | Sets the given pixbuf as the source pattern for the Cairo context. The
-- pattern has an extend mode of CAIRO_EXTEND_NONE and is aligned so that the
-- origin of pixbuf is pixbuf_x, pixbuf_y
--
-- * Available since Gtk+ version 2.8
--
cairoSetSourcePixbuf :: Cairo -> Pixbuf -> Double -> Double -> IO ()
cairoSetSourcePixbuf cr pixbuf pixbufX pixbufY =
  {# call unsafe gdk_cairo_set_source_pixbuf #}
    cr
    pixbuf
    (realToFrac pixbufX)
    (realToFrac pixbufY)

-- | Adds the given rectangle to the current path of cr.
--
-- * Available since Gtk+ version 2.8
--
cairoRectangle :: Cairo -> Rectangle -> IO ()
cairoRectangle cr (Rectangle x y width height) =
  Cairo.rectangle
    cr
    (realToFrac x)
    (realToFrac y)
    (realToFrac width)
    (realToFrac height)

-- | Adds the given region to the current path of cr.
--
-- * Available since Gtk+ version 2.8
--
cairoRegion :: Cairo -> Region -> IO ()
cairoRegion cr region =
  {# call unsafe gdk_cairo_region #}
    cr
    region
#endif
