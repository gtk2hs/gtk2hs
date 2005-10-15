-- -*-haskell-*-
--  GIMP Toolkit (GTK) Cairo GDK integration
--
--  Author : Duncan Coutts
--
--  Created: 17 August 2005
--
--  Version $Revision: 1.5 $ from $Date: 2005/10/15 16:59:09 $
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
  renderWithDrawable,
  setSourceColor,
  setSourcePixbuf,
  cairoRectangle,
  cairoRegion,
  cairoFontMapNew,
  cairoFontMapSetResolution,
  cairoFontMapGetResolution,
  cairoCreateContext,
  cairoContextSetResolution,
  cairoContextGetResolution,
  cairoContextSetFontOptions,
  cairoContextGetFontOptions,
  cairoUpdateContext,
  cairoCreateLayout,
  cairoUpdateLayout,
  cairoShowGlyphString,
  cairoShowLayoutLine,
  cairoShowLayout,
  cairoGlyphStringPath,
  cairoLayoutLinePath,
  cairoLayoutPath
#endif
  ) where

import Monad	    (liftM,unless)
import Control.Exception    (bracket)

import System.Glib.FFI
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Gdk.Region#} (Region(..))
import Graphics.UI.Gtk.General.Structs (Rectangle(..), Color(..))
import System.Glib.GObject		(makeNewGObject)
{#import Graphics.UI.Gtk.Pango.Types#}
import Graphics.UI.Gtk.Pango.Layout ( layoutSetText )
import Data.IORef

#if GTK_CHECK_VERSION(2,8,0) && defined(ENABLE_CAIRO)
{#import Graphics.Rendering.Cairo.Types#} as Cairo
import Graphics.Rendering.Cairo.Internal as Cairo.Internal
import Graphics.Rendering.Cairo as Cairo
import Control.Monad.Reader
#endif

{# context lib="gdk" prefix="gdk" #}

--------------------
-- Methods

#if GTK_CHECK_VERSION(2,8,0) && defined(ENABLE_CAIRO)
-- | Creates a Cairo context for drawing to drawable.
--
-- * Available since Gtk+ version 2.8
--
renderWithDrawable :: DrawableClass drawable =>
    drawable -- ^ drawable - a 'Drawable'
 -> Render a -- ^ A newly created Cairo context.
 -> IO a
renderWithDrawable drawable m =
  bracket (liftM Cairo.Cairo $ {#call unsafe gdk_cairo_create#} (toDrawable drawable))
          (\context -> do status <- Cairo.Internal.status context
                          Cairo.Internal.destroy context
                          unless (status == StatusSuccess) $
                            fail =<< Cairo.Internal.statusToString status)
          (\context -> runReaderT (runRender m) context)

-- | Sets the specified 'Color' as the source color of @cr@.
--
-- * Available since Gtk+ version 2.8
--
setSourceColor :: Color -> Render ()
setSourceColor (Color red green blue) =
  Cairo.setSourceRGB
    (realToFrac red   / 65535.0)
    (realToFrac green / 65535.0)
    (realToFrac blue  / 65535.0)

-- | Sets the given pixbuf as the source pattern for the Cairo context. The
-- pattern has an extend mode of CAIRO_EXTEND_NONE and is aligned so that the
-- origin of pixbuf is pixbuf_x, pixbuf_y
--
-- * Available since Gtk+ version 2.8
--
setSourcePixbuf :: Pixbuf -> Double -> Double -> Render ()
setSourcePixbuf pixbuf pixbufX pixbufY = Render $ do
  cr <- ask
  liftIO $ {# call unsafe gdk_cairo_set_source_pixbuf #}
    cr
    pixbuf
    (realToFrac pixbufX)
    (realToFrac pixbufY)

-- | Adds the given rectangle to the current path of cr.
--
-- * Available since Gtk+ version 2.8
--
cairoRectangle :: Rectangle -> Render ()
cairoRectangle (Rectangle x y width height) =
  Cairo.rectangle
    (realToFrac x)
    (realToFrac y)
    (realToFrac width)
    (realToFrac height)

-- | Adds the given region to the current path of cr.
--
-- * Available since Gtk+ version 2.8
--
cairoRegion :: Region -> Render ()
cairoRegion region = Render $ do
  cr <- ask
  liftIO $ {# call unsafe gdk_cairo_region #}
    cr
    region


-- | Create a 'PangoFontMap' that contains a list of available fonts.
--
-- * The sole purpose of creating an explicit 'PangoFontMap' is to set
--   a different scaling factor between font sizes (in points, pt) and
--   Cairo units (in pixels). The default is 96dpi (dots per inch) which
--   corresponds to an average screen as output medium. A 10pt font will
--   therefore scale to @10pt * (1/72 pt/inch) * (96 pixel/inch) = 13.3 pixel@.
--
cairoFontMapNew :: IO FontMap
cairoFontMapNew = 
  makeNewGObject mkFontMap $ {#call unsafe pango_cairo_font_map_new#}

-- | Set the scaling factor between font size and Cairo units.
--
-- * Value is in dots per inch (dpi). See 'cairoFontMapNew'.
--
cairoFontMapSetResolution :: Double -> FontMap -> IO ()
cairoFontMapSetResolution dpi (FontMap fm) =
  withForeignPtr fm $ \fmPtr ->
  {#call unsafe pango_cairo_font_map_set_resolution#}
    (castPtr fmPtr) (realToFrac dpi)

-- | Ask for the scaling factor between font size and Cairo units.
--
-- * Value is in dots per inch (dpi). See 'cairoFontMapNew'.
--
cairoFontMapGetResolution :: FontMap -> IO Double
cairoFontMapGetResolution (FontMap fm) = liftM realToFrac $
  withForeignPtr fm $ \fmPtr ->
  {#call unsafe pango_cairo_font_map_get_resolution#} (castPtr fmPtr)

-- | Create a 'PangoContext'.
--
-- * If no 'FontMap' is specified, it uses the default 'FontMap' that
--   has a scaling factor of 96 dpi. See 'cairoFontMapNew'.
--
cairoCreateContext :: Maybe FontMap -> IO PangoContext
cairoCreateContext (Just (FontMap fm)) = makeNewGObject mkPangoContext $
  withForeignPtr fm $ \fmPtr -> -- PangoCairoFontMap /= PangoFontMap
  {#call unsafe pango_cairo_font_map_create_context#} (castPtr fmPtr)
cairoCreateContext Nothing = do
  fmPtr <- {#call pango_cairo_font_map_get_default#}
  makeNewGObject mkPangoContext $
    {#call unsafe pango_cairo_font_map_create_context#} (castPtr fmPtr)

-- | Set the scaling factor of the 'PangoContext'.
--
-- * Supplying zero or a negative value will result in the resolution value
--   of the underlying 'FontMap' to be used. See also 'cairoFonMapNew'.
cairoContextSetResolution :: PangoContext -> Double -> IO ()
cairoContextSetResolution pc dpi =
  {#call unsafe pango_cairo_context_set_resolution#} pc (realToFrac dpi)

-- | Ask for the scaling factor of the 'PangoContext'.
--
-- * A negative value will be returned if no resolution has been set.
--   See 'cairoContextSetResolution'.
--
cairoContextGetResolution :: PangoContext -> IO Double
cairoContextGetResolution pc = liftM realToFrac $
  {#call unsafe pango_cairo_context_get_resolution#} pc

-- | Set Cairo font options.
--
-- * Applied the given font options to the context. Values set through this
--   functions have override those that are applied by 'cairoUpdateContext'.
--
cairoContextSetFontOptions :: PangoContext -> FontOptions -> IO ()
cairoContextSetFontOptions pc fo =
  {#call unsafe pango_cairo_context_set_font_options#} pc fo

-- | Reset Cairo font options.
--
cairoContextResetFontOptions :: PangoContext -> IO ()
cairoContextResetFontOptions pc =
  {#call unsafe pango_cairo_context_set_font_options#} pc
    (Cairo.Internal.FontOptions nullForeignPtr)

-- | Retrieve Cairo font options.
--
cairoContextGetFontOptions :: PangoContext -> IO FontOptions
cairoContextGetFontOptions pc = do
  foPtr <- {#call unsafe pango_cairo_context_get_font_options#} pc
  Cairo.Internal.mkFontOptions foPtr

-- | Update a 'PangoContext' with respect to changes in a 'Render'
--   environment.
--
--  * The 'PangoContext' must have been created with
--    'cairoCreateContext'. Any 'PangoLayout's that have been
--    previously created with this context have to be update using
--    'layoutContextChanged'.
--
cairoUpdateContext :: PangoContext -> Render ()
cairoUpdateContext pc =  Render $ do
  cr <- ask
  liftIO $ {# call unsafe pango_cairo_update_context #} cr pc

-- | Create a 'PangoLayout' within a 'Render' context.
--
-- * This is a convenience function that creates a new 'PangoContext'
--   within this 'Render' context and creates a new 'PangoLayout'.
--   If the transformation or target surface of the 'Render' context
--   change, 'cairoUpdateLayout' has to be called on this layout.
--
cairoCreateLayout :: String -> Render PangoLayout
cairoCreateLayout text = Render $ do
  cr <- ask
  liftIO $ do
    layRaw <- makeNewGObject mkPangoLayoutRaw $
	      {#call unsafe pango_cairo_create_layout#} cr
    textRef <- newIORef undefined
    let pl = (PangoLayout textRef layRaw)
    layoutSetText pl text
    return pl

-- | Propagate changed to the 'Render' context to a 'PangoLayout'.
--
-- * This is a convenience function that calls 'cairoUpdateContext' on the
--   (private) 'PangoContext' of the given layout to propagate changes
--   from the 'Render' context to the 'PangoContext' and then calls
--   'layoutContextChanged' on the layout. This function is necessary for
--   'cairoCreateLayout' since a private 'PangoContext' is created that is
--   not visible to the user.
--
cairoUpdateLayout :: PangoLayout -> Render ()
cairoUpdateLayout (PangoLayout _ lay) = Render $ do
  cr <- ask
  liftIO $ {#call unsafe pango_cairo_update_layout#} cr lay

-- | Draw a glyph string.
--
-- * The origin of the glyphs (the left edge of the baseline) will be drawn
--   at the current point of the cairo context.
--
cairoShowGlyphString :: GlyphItem -> Render ()
cairoShowGlyphString (GlyphItem pi gs) = Render $ do
  cr <- ask
  font <- liftIO $ pangoItemGetFont pi
  liftIO $ {#call unsafe pango_cairo_show_glyph_string#} cr font gs

-- | Draw a 'LayoutLine'.
--
-- * The origin of the glyphs (the left edge of the baseline) will be drawn
--   at the current point of the cairo context.
--
cairoShowLayoutLine :: LayoutLine -> Render ()
cairoShowLayoutLine (LayoutLine _ ll) = Render $ do
  cr <- ask
  liftIO $ {#call unsafe pango_cairo_show_layout_line#} cr ll

-- | Draw a 'PangoLayout'.
--
-- * The origin of the glyphs (the left edge of the baseline) will be drawn
--   at the current point of the cairo context.
--
cairoShowLayout :: PangoLayout -> Render ()
cairoShowLayout (PangoLayout _ lay) = Render $ do
  cr <- ask
  liftIO $ {#call unsafe pango_cairo_show_layout#} cr lay


-- | Draw a glyph string.
--
-- * The origin of the glyphs (the left edge of the line) will be at the
--   current point of the cairo context.
--
cairoGlyphStringPath :: GlyphItem -> Render ()
cairoGlyphStringPath (GlyphItem pi gs) = Render $ do
  cr <- ask
  font <- liftIO $ pangoItemGetFont pi
  liftIO $ {#call unsafe pango_cairo_glyph_string_path#} cr font gs

-- | Draw a 'LayoutLine'.
--
-- * The origin of the glyphs (the left edge of the line) will be at the
--   current point of the cairo context.
--
cairoLayoutLinePath :: LayoutLine -> Render ()
cairoLayoutLinePath (LayoutLine _ ll) = Render $ do
  cr <- ask
  liftIO $ {#call unsafe pango_cairo_layout_line_path#} cr ll

-- | Draw a 'PangoLayout'.
--
-- * The origin of the glyphs (the left edge of the line) will be at the
--   current point of the cairo context.
--
cairoLayoutPath :: PangoLayout -> Render ()
cairoLayoutPath (PangoLayout _ lay) = Render $ do
  cr <- ask
  liftIO $ {#call unsafe pango_cairo_layout_path#} cr lay

#endif

