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
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- 
-- Gtk specific functions to for redering with Cairo.
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
#if GTK_CHECK_VERSION(2,8,0) && defined(ENABLE_CAIRO)
  -- * Global Cairo settings.
  cairoFontMapGetDefault,
  cairoFontMapSetResolution,
  cairoFontMapGetResolution,
  cairoCreateContext,
  cairoContextSetResolution,
  cairoContextGetResolution,
  cairoContextSetFontOptions,
  cairoContextGetFontOptions,
  -- * Using 'Graphics.UI.Gtk.Gdk.Pixbuf.Pixbuf' functions together with Cairo
  cairoImageSurfaceFromPixbuf,
#if CAIRO_CHECK_VERSION(1,2,0)
  pixbufFromImageSurface,
#endif
  -- * Functions for the 'Render' monad.
  renderWithDrawable,
  setSourceColor,
  setSourcePixbuf,
  region,
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
{#import Graphics.UI.Gtk.Gdk.Region#} (Region(..))
import Graphics.UI.Gtk.General.Structs (Color(..))
import System.Glib.GObject		(constructNewGObject, makeNewGObject,
  objectRef, objectUnref)
{#import Graphics.UI.Gtk.Pango.Types#}
import Graphics.UI.Gtk.Pango.Structs ( pangoItemGetFont )
import Graphics.UI.Gtk.Pango.Layout ( layoutSetText )
{#import Graphics.UI.Gtk.Gdk.Pixbuf#} ( pixbufGetHasAlpha, pixbufGetNChannels,
  pixbufGetColorSpace, pixbufGetWidth, pixbufGetHeight, pixbufGetRowstride,
  Colorspace(..) )
import Data.IORef

#if GTK_CHECK_VERSION(2,8,0) && defined(ENABLE_CAIRO)
{#import Graphics.Rendering.Cairo.Types#} as Cairo
import qualified Graphics.Rendering.Cairo.Internal as Cairo.Internal
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo.Internal (Render(Render))
import Control.Monad.Reader
#endif

{# context lib="gdk" prefix="gdk" #}

--------------------
-- Methods

#if GTK_CHECK_VERSION(2,8,0) && defined(ENABLE_CAIRO)
-- | Treat a 'Graphics.UI.Gtk.Gdk.Pixbuf.Pixbuf' as an image
--   'Graphics..Rendering.Cairo.Surface'.
--
-- * The image data is shared between the two objects. Note that everytime you
--   use 'Graphics.UI.Gtk.Gdk.Pixbuf.Pixbuf' functions on the image, it is
--   necessary to tell Cairo that the image data has changed using
--   'Graphics..Rendering.Cairo.surfaceMarkDirty' since it might cache certain areas of
--   an image.
--
cairoImageSurfaceFromPixbuf :: Pixbuf -> IO Surface
cairoImageSurfaceFromPixbuf pb = do
  alpha <- pixbufGetHasAlpha pb
  chan <- pixbufGetNChannels pb
  cs <- pixbufGetColorSpace pb
  width <- pixbufGetWidth pb
  height <- pixbufGetHeight pb
  stride <- pixbufGetRowstride pb
  cairoFormat <- case (alpha, chan, cs) of
    (True, 4, ColorspaceRgb) -> return FormatARGB32
    (False, 3, ColorspaceRgb) -> return FormatRGB24
    (_, 1, _) -> return FormatA8 -- pixbuf doesn't actually do that
    _ -> error "cairoImageSurfaceFromPixbuf: cannot create cairo context form given format"
  dPtr <- {#call unsafe pixbuf_get_pixels#} pb
  sfPtr <- {#call cairo_image_surface_create_for_data#} dPtr
    (fromIntegral (fromEnum cairoFormat)) (fromIntegral width)
    (fromIntegral height) (fromIntegral stride)
  sf <- mkSurface sfPtr
  let pbPtr = unsafeForeignPtrToPtr (unPixbuf pb)
  objectRef pbPtr
  {#call cairo_surface_set_user_data#} sf (castPtr pbPtr)
    (castPtr pbPtr) objectUnref
  manageSurface sf
  return sf

#if CAIRO_CHECK_VERSION(1,2,0)
-- | Treat an image 'Graphics.Rendering.Cairo.Surface' as a
-- 'Graphics.UI.Gtk.Gdk.Pixbuf.Pixbuf'.
--
-- * The image data is shared between the two objects. Note that everytime you
--   use 'Graphics.UI.Gtk.Gdk.Pixbuf.Pixbuf' functions on the image, it is
--   necessary to tell Cairo that the image data has changed using
--   'Graphics.Rendering.Cairo.surfaceMarkDirty' since it might cache certain
--   areas of an image. This function throws an error if the
--   'Graphics.Rendering.Cairo.Surface' has any other format than
--   'Graphics.Rendering.Cairo.FormatARGB32' or
--   'Graphics.Rendering.Cairo.FormatRGB32' since
--   'Graphics.UI.Gtk.Gdk.Pixbuf.Pixbuf' can currently only handle these two
--   formats.
--
-- * Requires Cairo 1.2 or higher.
--
pixbufFromImageSurface :: Surface -> IO Pixbuf
pixbufFromImageSurface sf = do
  con <- Cairo.Internal.surfaceGetContent sf
  hasAlpha <- case con of
    Cairo.Internal.ContentColor -> return False
    Cairo.Internal.ContentColorAlpha -> return True
    _ -> error ("pixbufFromImageSurface: Pixbufs do not support Cairo format "++show con)    
  width <- Cairo.Internal.imageSurfaceGetWidth sf
  height <- Cairo.Internal.imageSurfaceGetHeight sf
  stride <- Cairo.Internal.imageSurfaceGetStride sf
  dPtr <- Cairo.Internal.imageSurfaceGetData sf
  let (Cairo.Surface sfFPtr) = sf
  let sfPtr = unsafeForeignPtrToPtr sfFPtr
  Cairo.Internal.surfaceReference sf
  fPtrRef <- newIORef nullFunPtr
  fPtr <- mkPixbufDestroyNotify $ \_ _ -> do
    Cairo.Internal.surfaceDestroy sf
    fPtr <- readIORef fPtrRef
    freeHaskellFunPtr fPtr
  writeIORef fPtrRef fPtr
  makeNewGObject mkPixbuf $
    {#call unsafe gdk_pixbuf_new_from_data#} dPtr 0 (fromBool hasAlpha)
      8 (fromIntegral width) (fromIntegral height) (fromIntegral stride)
      fPtr nullPtr
#endif

-- the following should be ifdef'd out as well but then we need to conditionally
-- link in the _stub.o file of that is then only sometimes generated...
{#pointer GdkPixbufDestroyNotify as PixbufDestroyNotify#}

foreign import ccall "wrapper" mkPixbufDestroyNotify ::
  (Ptr () -> Ptr Surface -> IO ()) -> IO PixbufDestroyNotify

-- | Creates a Cairo context for drawing to a 'Drawable'.
--
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

-- | Sets the specified 'Color' as the source color of the 'Render' context.
--
setSourceColor :: Color -> Render ()
setSourceColor (Color red green blue) =
  Cairo.setSourceRGB
    (realToFrac red   / 65535.0)
    (realToFrac green / 65535.0)
    (realToFrac blue  / 65535.0)

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
--
region :: Region -> Render ()
region region = Render $ do
  cr <- ask
  liftIO $ {# call unsafe gdk_cairo_region #}
    cr
    region


-- cairo_font_map_new cannot be bound due to incorrect memory management
-- in functions like font_map_list_families that create new structures
-- that store the font map without referencing them

-- | Retrieve the default 'Graphics.UI.Gtk.Pango.FontMap' that contains a
--   list of available fonts.
--
-- * One purpose of creating an explicit
--  'Graphics.UI.Gtk.Pango.Font.FontMap' is to set
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
-- * Value is in dots per inch (dpi). See 'cairoFontMapNew'.
--
cairoFontMapSetResolution :: FontMap -> Double -> IO ()
cairoFontMapSetResolution (FontMap fm) dpi =
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
cairoCreateContext (Just (FontMap fm)) = constructNewGObject mkPangoContext $
  withForeignPtr fm $ \fmPtr -> -- PangoCairoFontMap /= PangoFontMap
  {#call unsafe pango_cairo_font_map_create_context#} (castPtr fmPtr)
cairoCreateContext Nothing = do
  fmPtr <- {#call unsafe pango_cairo_font_map_get_default#}
  constructNewGObject mkPangoContext $
    {#call unsafe pango_cairo_font_map_create_context#} (castPtr fmPtr)

-- | Set the scaling factor of the 'PangoContext'.
--
-- * Supplying zero or a negative value will result in the resolution value
--   of the underlying 'FontMap' to be used. See also 'cairoFontMapNew'.
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
--    'Graphics.UI.Gtk.Pango.Layout.layoutContextChanged'.
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
createLayout :: String -> Render PangoLayout
createLayout text = Render $ do
  cr <- ask
  liftIO $ do
    layRaw <- constructNewGObject mkPangoLayoutRaw $
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
--   'Graphics.UI.Gtk.Pango.Layout.layoutContextChanged' on the layout.
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

#endif

