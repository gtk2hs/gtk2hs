{-# LANGUAGE CPP, ScopedTypeVariables #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Drawable
--
--  Author : Axel Simon
--
--  Created: 22 September 2002
--
--  Copyright (C) 2002-2005 Axel Simon
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
-- TODO
--
-- if gdk_visuals are implemented, do: get_visual
--
-- if gdk_colormaps are implemented, do: set_colormap, get_colormap
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Drawing primitives.
--
-- This module defines drawing primitives that can operate on 'DrawWindow's
-- and 'Pixmap's.
--
-- This module is empty when built with Gtk3 because GTKDrawable has been
-- removed.
module Graphics.UI.Gtk.Gdk.Drawable (
#if GTK_MAJOR_VERSION < 3
  Drawable,
  DrawableClass,
  castToDrawable, gTypeDrawable,
  toDrawable,
  drawableGetDepth,
  drawableGetSize,
  drawableGetClipRegion,
  drawableGetVisibleRegion,
  drawableGetID,
  Point,
  drawPoint,
  drawPoints,
  drawLine,
  drawLines,
#if GTK_CHECK_VERSION(2,2,0)
  Dither(..),
  drawPixbuf,
#endif
  drawSegments,
  drawRectangle,
  drawArc,
  drawPolygon,
  drawGlyphs,
  drawLayoutLine,
  drawLayoutLineWithColors,
  drawLayout,
  drawLayoutWithColors,
  drawDrawable,
#endif
) where

#if GTK_MAJOR_VERSION < 3
import Control.Monad    (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.General.Structs  (Point, drawableGetID)
import Graphics.Rendering.Pango.Structs
{#import Graphics.Rendering.Pango.Types#}
{#import Graphics.Rendering.Pango.BasicTypes#}
{#import Graphics.UI.Gtk.Types#}
#if GTK_MAJOR_VERSION < 3
{#import Graphics.UI.Gtk.Gdk.Region#}   (Region, makeNewRegion)
#endif
import Graphics.UI.Gtk.Gdk.Enums        (Dither(..))

{# context lib="gtk" prefix="gdk" #}

-- methods

-- | Get the size of pixels.
--
-- * Returns the number of bits which are use to store information on each
--   pixels in this 'Drawable'.
--
drawableGetDepth :: DrawableClass d => d -> IO Int
drawableGetDepth d = liftM fromIntegral $
                     {#call unsafe drawable_get_depth#} (toDrawable d)

-- | Retrieve the size of the 'Drawable'.
--
-- * The result might not be up-to-date if there are still resizing messages
--   to be processed.
--
drawableGetSize :: DrawableClass d => d -> IO (Int, Int)
drawableGetSize d = alloca $ \wPtr -> alloca $ \hPtr -> do
  {#call unsafe drawable_get_size#} (toDrawable d) wPtr hPtr
  (w::{#type gint#}) <- peek wPtr
  (h::{#type gint#}) <- peek hPtr
  return (fromIntegral w, fromIntegral h)

-- | Determine where not to draw.
--
-- * Computes the region of a drawable that potentially can be written
--   to by drawing primitives. This region will not take into account the
--   clip region for the GC, and may also not take into account other
--   factors such as if the window is obscured by other windows, but no
--   area outside of this region will be affected by drawing primitives.
--
drawableGetClipRegion :: DrawableClass d => d -> IO Region
drawableGetClipRegion d = do
  rPtr <- {#call unsafe drawable_get_clip_region#} (toDrawable d)
  makeNewRegion rPtr

-- | Determine what not to redraw.
--
-- * Computes the region of a drawable that is potentially visible.
-- This does not necessarily take into account if the window is obscured
-- by other windows, but no area outside of this region is visible.
--
drawableGetVisibleRegion :: DrawableClass d => d -> IO Region
drawableGetVisibleRegion d = do
  rPtr <- {#call unsafe drawable_get_visible_region#} (toDrawable d)
  makeNewRegion rPtr

-- | Draw a point into a 'Drawable'.
--
drawPoint :: DrawableClass d => d -> GC -> Point -> IO ()
drawPoint d gc (x,y) = {#call unsafe draw_point#} (toDrawable d)
  (toGC gc) (fromIntegral x) (fromIntegral y)


-- | Draw several points into a 'Drawable'.
--
-- * This function is more efficient than calling 'drawPoint' on
--   several points.
--
drawPoints :: DrawableClass d => d -> GC -> [Point] -> IO ()
drawPoints d gc []     = return ()
drawPoints d gc points =
  withArray (concatMap (\(x,y) -> [fromIntegral x, fromIntegral y]) points) $
  \(aPtr :: Ptr {#type gint#}) -> {#call unsafe draw_points#} (toDrawable d)
    (toGC gc) (castPtr aPtr) (fromIntegral (length points))

-- | Draw a line into a 'Drawable'.
--
-- * The parameters are x1, y1, x2, y2.
--
-- * Drawing several separate lines can be done more efficiently by
--   'drawSegments'.
--
drawLine :: DrawableClass d => d -> GC -> Point -> Point -> IO ()
drawLine d gc (x1,y1) (x2,y2) = {#call unsafe draw_line#} (toDrawable d)
  (toGC gc) (fromIntegral x1) (fromIntegral y1) (fromIntegral x2)
  (fromIntegral y2)

-- | Draw several lines.
--
-- * The function uses the current line width, dashing and especially the
--   joining specification in the graphics context (in contrast to
--   'drawSegments'.
--
drawLines :: DrawableClass d => d -> GC -> [Point] -> IO ()
drawLines d gc []     = return ()
drawLines d gc points =
  withArray (concatMap (\(x,y) -> [fromIntegral x, fromIntegral y]) points) $
  \(aPtr :: Ptr {#type gint#}) -> {#call unsafe draw_lines#} (toDrawable d)
    (toGC gc) (castPtr aPtr) (fromIntegral (length points))

#if GTK_CHECK_VERSION(2,2,0)
-- | Render a 'Pixbuf'.
--
-- * Usage:
--   @drawPixbuf d gc pb srcX srcY destX destY srcWidth srcHeight dither xDither yDither@
--   Renders a rectangular portion of a 'Pixbuf' to a
--   'Drawable'. The @srcX@, @srcY@,
--   @srcWidth@ and @srcHeight@ specify what part of the
--   'Pixbuf' should be rendered. The latter two values may be
--   @-1@ in which case the width and height are taken from
--   @pb@. The image is placed at @destX@, @destY@.
--   If you render parts of an image at a time, set @ditherX@ and
--   @ditherY@ to the origin of the image you are rendering.
--
-- * Since 2.2.
--
drawPixbuf :: DrawableClass d => d -> GC -> Pixbuf -> Int -> Int ->
                                 Int -> Int -> Int -> Int -> Dither ->
                                 Int -> Int -> IO ()
drawPixbuf d gc pb srcX srcY destX destY srcWidth srcHeight dither
  xDither yDither = {#call unsafe draw_pixbuf#} (toDrawable d)
    gc pb (fromIntegral srcX) (fromIntegral srcY) (fromIntegral destX)
    (fromIntegral destY) (fromIntegral srcWidth) (fromIntegral srcHeight)
    ((fromIntegral . fromEnum) dither) (fromIntegral xDither)
    (fromIntegral yDither)

#endif

-- | Draw several unconnected lines.
--
-- * This method draws several unrelated lines.
--
drawSegments :: DrawableClass d => d -> GC -> [(Point,Point)] -> IO ()
drawSegments d gc []  = return ()
drawSegments d gc pps = withArray (concatMap (\((x1,y1),(x2,y2)) ->
  [fromIntegral x1, fromIntegral y1, fromIntegral x2, fromIntegral y2])
  pps) $ \(aPtr :: Ptr {#type gint#}) ->
    {#call unsafe draw_segments#} (toDrawable d) (toGC gc)
    (castPtr aPtr) (fromIntegral (length pps))

-- | Draw a rectangular object.
--
-- * Draws a rectangular outline or filled rectangle, using the
--   foreground color and other attributes of the 'GC'.
--
-- * A rectangle drawn filled is 1 pixel smaller in both dimensions
--   than a rectangle outlined. Calling 'drawRectangle' w gc
--   True 0 0 20 20 results in a filled rectangle 20 pixels wide and 20
--   pixels high. Calling 'drawRectangle' d gc False 0 0 20 20
--   results in an outlined rectangle with corners at (0, 0), (0, 20), (20,
--   20), and (20, 0), which makes it 21 pixels wide and 21 pixels high.
--
drawRectangle :: DrawableClass d => d -- ^ drawable
  -> GC -- ^ graphics context
  -> Bool -- ^ filled
  -> Int -- ^ x
  -> Int -- ^ y
  -> Int -- ^ width
  -> Int -- ^ height
  -> IO ()
drawRectangle d gc filled x y width height = {#call unsafe draw_rectangle#}
  (toDrawable d) (toGC gc) (fromBool filled) (fromIntegral x)
  (fromIntegral y) (fromIntegral width) (fromIntegral height)

-- | Draws an arc or a filled 'pie slice'.
--
-- * The arc is defined by the bounding rectangle of the entire
--   ellipse, and the start and end angles of the part of the ellipse to be
--   drawn.
--
-- * The starting angle @aStart@ is relative to the 3 o'clock
--   position, counter-clockwise, in 1\/64ths of a degree. @aEnd@
--   is measured similarly, but relative to @aStart@.
--
drawArc :: DrawableClass d => d -> GC -> Bool -> Int -> Int ->
                                 Int -> Int -> Int -> Int -> IO ()
drawArc d gc filled x y width height aStart aEnd =
  {#call unsafe draw_arc#} (toDrawable d) (toGC gc) (fromBool filled)
  (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)
  (fromIntegral aStart) (fromIntegral aEnd)

-- | Draws an outlined or filled polygon.
--
-- * The polygon is closed automatically, connecting the last point to
--   the first point if necessary.
--
drawPolygon :: DrawableClass d => d -> GC -> Bool -> [Point] -> IO ()
drawPolygon _ _ _ [] = return ()
drawPolygon d gc filled points =
  withArray (concatMap (\(x,y) -> [fromIntegral x, fromIntegral y]) points) $
  \(aPtr::Ptr {#type gint#}) -> {#call unsafe draw_polygon#} (toDrawable d)
  (toGC gc) (fromBool filled) (castPtr aPtr) (fromIntegral (length points))

-- | Draw a segment of text.
--
-- * This function draws a segment of text. These segements are the result
--   of itemizing a string into segments with the same characteristics
--   (font, text direction, etc.) using
--   'Graphics.Rendering.Pango.Rendering.itemize'. Each item is then turned
--   into a shapes by calling 'Graphics.Rendering.Pango.Rendering.shape'.
--   These shapes can then be drawn onto screen using this function.
--   A simpler interface, that also takes care of breaking a paragraph
--   into several lines is a 'Graphics.Rendering.Pango.Layout.LayoutLine'.
--
drawGlyphs :: DrawableClass d => d -> GC -> Int -> Int -> GlyphItem -> IO ()
drawGlyphs d gc x y (GlyphItem pi gs) = do
  font <- pangoItemGetFont pi
  {#call unsafe draw_glyphs#} (toDrawable d) (toGC gc) font
    (fromIntegral x) (fromIntegral y) gs

--
-- | Draw a single line of text.
--
-- * The @x@ coordinate specifies the start of the string,
--   the @y@ coordinate specifies the base line.
--
drawLayoutLine :: DrawableClass d => d -> GC -> Int -> Int -> LayoutLine ->
                                     IO ()
drawLayoutLine d gc x y (LayoutLine _ ll) =
  {#call unsafe draw_layout_line#} (toDrawable d) (toGC gc)
    (fromIntegral x) (fromIntegral y) ll

-- | Draw a single line of text.
--
-- * The @x@ coordinate specifies the start of the string,
--   the @y@ coordinate specifies the base line.
--
-- * If both colors are @Nothing@ this function will behave like
--   'drawLayoutLine' in that it uses the default colors from
--   the graphics context.
--
drawLayoutLineWithColors :: DrawableClass d => d -> GC -> Int -> Int ->
                            LayoutLine -> Maybe Color -> Maybe Color -> IO ()
drawLayoutLineWithColors d gc x y (LayoutLine _ ll) foreground background = let
    withMB :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
    withMB Nothing f = f nullPtr
    withMB (Just x) f = with x f
  in
    withMB foreground $ \fPtr -> withMB background $ \bPtr ->
    {#call unsafe draw_layout_line_with_colors#} (toDrawable d) (toGC gc)
      (fromIntegral x) (fromIntegral y) ll (castPtr fPtr) (castPtr bPtr)


-- | Draw a paragraph of text.
--
-- * The @x@ and @y@ values specify the upper left
--   point of the layout.
--
drawLayout :: DrawableClass d => d -> GC -> Int -> Int -> PangoLayout -> IO ()
drawLayout d gc x y (PangoLayout _ pl) =
  {#call unsafe draw_layout#} (toDrawable d) (toGC gc)
    (fromIntegral x) (fromIntegral y) pl

-- | Draw a paragraph of text.
--
-- * The @x@ and @y@ values specify the upper left
--   point of the layout.
--
-- * If both colors are @Nothing@ this function will behave like
--   'drawLayout' in that it uses the default colors from
--   the graphics context.
--
drawLayoutWithColors :: DrawableClass d => d -> GC -> Int -> Int ->
                        PangoLayout -> Maybe Color -> Maybe Color -> IO ()
drawLayoutWithColors d gc x y (PangoLayout _ pl) foreground background = let
    withMB :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
    withMB Nothing f = f nullPtr
    withMB (Just x) f = with x f
  in
    withMB foreground $ \fPtr -> withMB background $ \bPtr ->
    {#call unsafe draw_layout_with_colors#} (toDrawable d) (toGC gc)
      (fromIntegral x) (fromIntegral y) pl (castPtr fPtr) (castPtr bPtr)


-- | Copies another 'Drawable'.
--
-- * Copies the (width,height) region of the @src@ at coordinates
--   (@xSrc@, @ySrc@) to coordinates (@xDest@,
--   @yDest@) in the @dest@. The @width@ and\/or
--   @height@ may be given as -1, in which case the entire source
--   drawable will be copied.
--
-- * Most fields in @gc@ are not used for this operation, but
--   notably the clip mask or clip region will be honored.  The source and
--   destination drawables must have the same visual and colormap, or
--   errors will result. (On X11, failure to match visual\/colormap results
--   in a BadMatch error from the X server.)  A common cause of this
--   problem is an attempt to draw a bitmap to a color drawable. The way to
--   draw a bitmap is to set the bitmap as a clip mask on your
--   'GC', then use 'drawRectangle' to draw a
--   rectangle clipped to the bitmap.
--
drawDrawable :: (DrawableClass src, DrawableClass dest)
  => dest -- ^ destination drawable
  -> GC -- ^ graphics context
  -> src -- ^ source drawable
  -> Int -- ^ @xSrc@
  -> Int -- ^ @ySrc@
  -> Int -- ^ @xDest@
  -> Int -- ^ @yDest@
  -> Int -- ^ @width@
  -> Int -- ^ @height@
  -> IO ()
drawDrawable dest gc src xSrc ySrc xDest yDest width height =
  {#call unsafe draw_drawable#} (toDrawable dest) (toGC gc)
  (toDrawable src)
  (fromIntegral xSrc) (fromIntegral ySrc) (fromIntegral xDest)
  (fromIntegral yDest) (fromIntegral width) (fromIntegral height)

#endif /* GTK_MAJOR_VERSION < 3 */
