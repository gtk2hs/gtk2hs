{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Cairo GDK integration
--
--  Author : Duncan Coutts
--
--  Created: 17 August 2005
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
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
--
-- Gtk specific functions to for rendering with Cairo.
--
-- Cairo is a graphics library that supports vector graphics and image
-- compositing that can be used with Gdk.
-- The Cairo API is an addition to Gdk\/Gtk (rather than a replacement).
-- Cairo rendering can be performed on any 'Graphics.UI.Gtk.Gdk.Drawable'
-- by calling 'renderWithDrawable'. The functions in this module provide
-- ways of drawing Gtk specific elements, such as 'Pixbuf's or text
-- laid out with Pango.
--
-- All functions in this module are only available in Gtk 2.8 or higher.
--
module Graphics.UI.Gtk.Cairo (
#if GTK_CHECK_VERSION(2,8,0)
  -- * Global Cairo settings.
  cairoFontMapGetDefault,
  cairoFontMapSetResolution,
  cairoFontMapGetResolution,
  cairoCreateContext,
  cairoContextSetResolution,
  cairoContextGetResolution,
  cairoContextSetFontOptions,
  cairoContextGetFontOptions,
  -- * Functions for the 'Render' monad.
#if GTK_MAJOR_VERSION < 3
  renderWithDrawable,
#else
  getClipRectangle,
  renderWithDrawWindow,
#endif
  region,
  setSourceColor,
  setSourcePixbuf,
  rectangle,
  updateContext,
  createLayout,
  updateLayout,
  showGlyphString,
  showLayoutLine,
  showLayout,
  glyphStringPath,
  layoutLinePath,
  layoutPath
#endif
  ) where

import Control.Exception    (bracket)

import System.Glib.FFI
{#import Graphics.UI.Gtk.Types#}
#if GTK_MAJOR_VERSION < 3
{#import Graphics.UI.Gtk.Gdk.Region#} (Region(..))
#endif
{#import Graphics.Rendering.Pango.Cairo#}

#if GTK_CHECK_VERSION(2,8,0)
#if GTK_MAJOR_VERSION < 3
{#import Graphics.Rendering.Cairo.Types#} as Cairo hiding (Region)
#else
{#import Graphics.Rendering.Cairo.Types#} as Cairo
#endif
import qualified Graphics.Rendering.Cairo.Internal as Cairo.Internal
import Graphics.Rendering.Cairo.Internal (Render(Render))
import Control.Monad.Reader
import Graphics.UI.Gtk.General.Structs (Rectangle(..))
#endif

{# context lib="gdk" prefix="gdk" #}

--------------------
-- Methods

#if GTK_CHECK_VERSION(2,8,0)
#if GTK_MAJOR_VERSION < 3
-- | Creates a Cairo context for drawing to a 'Drawable'.
--
-- Removed in Gtk3.
renderWithDrawable :: DrawableClass drawable =>
    drawable -- ^ @drawable@ - a 'Drawable'
 -> Render a -- ^ A newly created Cairo context.
 -> IO a
renderWithDrawable drawable m =
  bracket (liftM Cairo.Cairo $ {#call unsafe gdk_cairo_create#} (toDrawable drawable))
          (\context -> do status <- Cairo.Internal.status context
                          Cairo.Internal.destroy context
                          unless (status == Cairo.StatusSuccess) $
                            fail =<< Cairo.Internal.statusToString status)
          (\context -> runReaderT (Cairo.Internal.runRender m) context)
#endif

#if GTK_MAJOR_VERSION >= 3
-- | Creates a Cairo context for drawing to a 'DrawWindow'.
renderWithDrawWindow :: DrawWindowClass drawWindow =>
    drawWindow -- ^ @drawWindow@ - a 'DrawWindow'
 -> Render a -- ^ A newly created Cairo context.
 -> IO a
renderWithDrawWindow drawWindow m =
  bracket (liftM Cairo.Cairo $ {#call unsafe gdk_cairo_create#} (toDrawWindow drawWindow))
          (\context -> do status <- Cairo.Internal.status context
                          Cairo.Internal.destroy context
                          unless (status == Cairo.StatusSuccess) $
                            fail =<< Cairo.Internal.statusToString status)
          (\context -> runReaderT (Cairo.Internal.runRender m) context)

-- | Compute a bounding box in user coordinates covering the area inside
-- the current clip. It rounds the bounding box to integer coordinates.
-- Returns 'Nothing' indicating if a clip area doesn't exist.
getClipRectangle :: Render (Maybe Rectangle)
getClipRectangle = Render $ do
  cr <- ask
  liftIO $ alloca $ \rectPtr -> do
    ok <- {# call unsafe gdk_cairo_get_clip_rectangle #}
      cr
      (castPtr rectPtr)
    if ok /= 0
      then fmap Just (peek rectPtr)
      else return Nothing
#endif

-- | Sets the given pixbuf as the source pattern for the Cairo context. The
-- pattern has an extend mode of 'ExtendNone' and is aligned so that the
-- origin of pixbuf is @(x, y)@.
--
setSourcePixbuf ::
    Pixbuf
 -> Double    -- ^ x
 -> Double    -- ^ y
 -> Render ()
setSourcePixbuf pixbuf pixbufX pixbufY = Render $ do
  cr <- ask
  liftIO $ {# call unsafe gdk_cairo_set_source_pixbuf #}
    cr
    pixbuf
    (realToFrac pixbufX)
    (realToFrac pixbufY)

-- | Adds the given region to the current path of the 'Render' context.
rectangle :: Rectangle -> Render ()
rectangle rect = Render $ do
  cr <- ask
  liftIO $ with rect $ \ rectPtr ->
    {# call unsafe gdk_cairo_rectangle #}
      cr
      (castPtr rectPtr)

-- | Adds the given region to the current path of the 'Render' context.
region :: Region -> Render ()
region region = Render $ do
  cr <- ask
  liftIO $ {# call unsafe gdk_cairo_region #}
    cr
    region

#endif
