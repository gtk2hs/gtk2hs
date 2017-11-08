{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Cair Pango integration
--
--  Author : Duncan Coutts, Axel Simon
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
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
--
-- Pango specific functions for redering with Cairo.
--
-- Cairo is a graphics library that supports vector graphics and image
-- compositing that can be used with Pango. The functions in this module provide
-- ways of rendering text in Cairo using Pango.
--
module Graphics.Rendering.Pango.Cairo (
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
  setSourceColor,
  updateContext,
  createLayout,
  updateLayout,
  showGlyphString,
  showLayoutLine,
  showLayout,
  glyphStringPath,
  layoutLinePath,
  layoutPath
  ) where

import Control.Exception    (bracket)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject              (wrapNewGObject, makeNewGObject,
  objectRef, objectUnref)
{#import Graphics.Rendering.Pango.Types#}
{#import Graphics.Rendering.Pango.BasicTypes#}
import Graphics.Rendering.Pango.Structs ( pangoItemGetFont, Color(..) )
{#import Graphics.Rendering.Pango.Layout#} ( layoutSetText )
import Data.IORef

{#import Graphics.Rendering.Cairo.Types#} as Cairo
import qualified Graphics.Rendering.Cairo.Internal as Cairo.Internal
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo.Internal (Render(Render))
import Control.Monad.Reader

{# context lib="gdk" prefix="gdk" #}

--------------------
-- Methods

-- | Sets the specified 'Color' as the source color of the 'Render' context.
--
setSourceColor :: Color -> Render ()
setSourceColor (Color red green blue) =
  Cairo.setSourceRGB
    (realToFrac red   / 65535.0)
    (realToFrac green / 65535.0)
    (realToFrac blue  / 65535.0)

-- cairo_font_map_new cannot be bound due to incorrect memory management
-- in functions like font_map_list_families that create new structures
-- that store the font map without referencing them

-- | Retrieve the default 'Graphics.Rendering.Pango.FontMap' that contains a
--   list of available fonts.
--
-- * One purpose of creating an explicit
--  'Graphics.Rendering.Pango.Font.FontMap' is to set
--   a different scaling factor between font sizes (in points, pt) and
--   Cairo units (in pixels). The default is 96dpi (dots per inch) which
--   corresponds to an average screen as output medium. A 10pt font will
--   therefore scale to
--   @10pt * (1\/72 pt\/inch) * (96 pixel\/inch) = 13.3 pixel@.
--
cairoFontMapGetDefault :: IO FontMap
cairoFontMapGetDefault =
  makeNewGObject mkFontMap $ {#call unsafe pango_cairo_font_map_get_default#}

-- | Set the scaling factor between font size and Cairo units.
--
-- * Value is in dots per inch (dpi). See 'cairoFontMapGetDefault'.
--
cairoFontMapSetResolution :: FontMap -> Double -> IO ()
cairoFontMapSetResolution (FontMap fm) dpi =
  withForeignPtr fm $ \fmPtr ->
  {#call unsafe pango_cairo_font_map_set_resolution#}
    (castPtr fmPtr) (realToFrac dpi)

-- | Ask for the scaling factor between font size and Cairo units.
--
-- * Value is in dots per inch (dpi). See 'cairoFontMapGetDefault'.
--
cairoFontMapGetResolution :: FontMap -> IO Double
cairoFontMapGetResolution (FontMap fm) = liftM realToFrac $
  withForeignPtr fm $ \fmPtr ->
  {#call unsafe pango_cairo_font_map_get_resolution#} (castPtr fmPtr)

-- | Create a 'PangoContext'.
--
-- * If no 'FontMap' is specified, it uses the default 'FontMap' that
--   has a scaling factor of 96 dpi. See 'cairoFontMapGetDefault'.
--
cairoCreateContext :: Maybe FontMap -> IO PangoContext
cairoCreateContext (Just (FontMap fm)) = wrapNewGObject mkPangoContext $
  withForeignPtr fm $ \fmPtr -> -- PangoCairoFontMap /= PangoFontMap
  {#call unsafe pango_cairo_font_map_create_context#} (castPtr fmPtr)
cairoCreateContext Nothing = do
  fmPtr <- {#call unsafe pango_cairo_font_map_get_default#}
  wrapNewGObject mkPangoContext $
    {#call unsafe pango_cairo_font_map_create_context#} (castPtr fmPtr)

-- | Set the scaling factor of the 'PangoContext'.
--
-- * Supplying zero or a negative value will result in the resolution value
--   of the underlying 'FontMap' to be used. See also 'cairoFontMapGetDefault'.
--
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
-- * Apply the given font options to the context. Values set through this
--   functions override those that are set by 'updateContext'.
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
--    'Graphics.Rendering.Pango.Layout.layoutContextChanged'.
--
updateContext :: PangoContext -> Render ()
updateContext pc =  Render $ do
  cr <- ask
  liftIO $ {# call unsafe pango_cairo_update_context #} cr pc

-- | Create a 'PangoLayout' within a 'Render' context.
--
-- * This is a convenience function that creates a new 'PangoContext'
--   within this 'Render' context and creates a new 'PangoLayout'.
--   If the transformation or target surface of the 'Render' context
--   change, 'updateLayout' has to be called on this layout.
--
createLayout :: GlibString string => string -> Render PangoLayout
createLayout text = Render $ do
  cr <- ask
  liftIO $ do
    layRaw <- wrapNewGObject mkPangoLayoutRaw $
              {#call unsafe pango_cairo_create_layout#} cr
    textRef <- newIORef undefined
    let pl = (PangoLayout textRef layRaw)
    layoutSetText pl text
    return pl

-- | Propagate changed to the 'Render' context to a 'PangoLayout'.
--
-- * This is a convenience function that calls 'updateContext' on the
--   (private) 'PangoContext' of the given layout to propagate changes
--   from the 'Render' context to the 'PangoContext' and then calls
--   'Graphics.Rendering.Pango.Layout.layoutContextChanged' on the layout.
--   This function is necessary for
--   'createLayout' since a private 'PangoContext' is created that is
--   not visible to the user.
--
updateLayout :: PangoLayout -> Render ()
updateLayout (PangoLayout _ lay) = Render $ do
  cr <- ask
  liftIO $ {#call unsafe pango_cairo_update_layout#} cr lay

-- | Draw a glyph string.
--
-- * The origin of the glyphs (the left edge of the baseline) will be drawn
--   at the current point of the cairo context.
--
showGlyphString :: GlyphItem -> Render ()
showGlyphString (GlyphItem pi gs) = Render $ do
  cr <- ask
  font <- liftIO $ pangoItemGetFont pi
  liftIO $ {#call unsafe pango_cairo_show_glyph_string#} cr font gs

-- | Draw a 'LayoutLine'.
--
-- * The origin of the glyphs (the left edge of the baseline) will be drawn
--   at the current point of the cairo context.
--
showLayoutLine :: LayoutLine -> Render ()
showLayoutLine (LayoutLine _ ll) = Render $ do
  cr <- ask
  liftIO $ {#call unsafe pango_cairo_show_layout_line#} cr ll

-- | Draw a 'PangoLayout'.
--
-- * The top-left corner of the 'PangoLayout' will be drawn at the current
--   point of the cairo context.
--
showLayout :: PangoLayout -> Render ()
showLayout (PangoLayout _ lay) = Render $ do
  cr <- ask
  liftIO $ {#call unsafe pango_cairo_show_layout#} cr lay


-- | Add the extent of a glyph string to the current path.
--
-- * The origin of the glyphs (the left edge of the line) will be at the
--   current point of the cairo context.
--
glyphStringPath :: GlyphItem -> Render ()
glyphStringPath (GlyphItem pi gs) = Render $ do
  cr <- ask
  font <- liftIO $ pangoItemGetFont pi
  liftIO $ {#call unsafe pango_cairo_glyph_string_path#} cr font gs

-- | Add the extent of a layout line to the current path.
--
-- * The origin of the glyphs (the left edge of the line) will be at the
--   current point of the cairo context.
--
layoutLinePath :: LayoutLine -> Render ()
layoutLinePath (LayoutLine _ ll) = Render $ do
  cr <- ask
  liftIO $ {#call unsafe pango_cairo_layout_line_path#} cr ll

-- | Add the layout to the current path.
--
-- * Adds the top-left corner of the text to the current path. Afterwards,
--   the path position is at the bottom-right corner of the 'PangoLayout'.
--
layoutPath :: PangoLayout -> Render ()
layoutPath (PangoLayout _ lay) = Render $ do
  cr <- ask
  liftIO $ {#call unsafe pango_cairo_layout_path#} cr lay


