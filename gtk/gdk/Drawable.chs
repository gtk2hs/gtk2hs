{-# OPTIONS -cpp #-}
--  -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Drawable@
--
--  Author : Axel Simon
--  Created: 22 September 2002
--
--  Version $Revision: 1.5 $ from $Date: 2003/05/16 18:45:23 $
--
--  Copyright (c) 2002 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- @description@ --------------------------------------------------------------
--
--  Drawing primitives.
--
-- @documentation@ ------------------------------------------------------------
--
-- * This module defines drawing primitives that can operate on 
--   @ref data DrawWindow@s, @ref data Pixmap@s and 
--   @ref data Bitmap@s.
--
-- @todo@ ---------------------------------------------------------------------
--
-- * if gdk_visuals are implemented, do: get_visual
--
-- * if gdk_colormaps are implemented, do: set_colormap, get_colormap
--
-- * add draw_glyphs if we are desparate
--
--

#include<gtk/gtkversion.h>

module Drawable(
  Drawable,
  DrawableClass,
  castToDrawable,
  drawableGetDepth,
  drawableGetSize,
  drawableGetClipRegion,
  drawableGetVisibleRegion,
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
  drawLayoutLine,
  drawLayoutLineWithColors,
  drawLayout,
  drawLayoutWithColors,
  drawDrawable) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import GObject	(makeNewGObject)
import Structs  (Point)
{#import Hierarchy#}
{#import Region#}	(Region, makeNewRegion)
import Structs		(Color)
{#import PangoTypes#}
import GdkEnums         (Dither(..))

{# context lib="gtk" prefix="gdk" #}

-- methods

-- @method drawableGetDepth@ Get the size of pixels.
--
-- * Returns the number of bits which are use to store information on each
--   pixels in this @ref data Drawable@.
--
drawableGetDepth :: DrawableClass d => d -> IO Int
drawableGetDepth d = liftM fromIntegral $ 
		     {#call unsafe drawable_get_depth#} (toDrawable d)

-- @method drawableGetSize@ Retrieve the size of the @ref type Drawable@.
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

-- @method drawableGetClipRegion@ Determine where not to draw.
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

-- @method drawableGetVisibleRegion@ Determine what not to redraw.
--
-- * Computes the region of a drawable that is potentially visible.
-- This does not necessarily take into account if the window is obscured
-- by other windows, but no area outside of this region is visible.
--
drawableGetVisibleRegion :: DrawableClass d => d -> IO Region
drawableGetVisibleRegion d = do
  rPtr <- {#call unsafe drawable_get_visible_region#} (toDrawable d)
  makeNewRegion rPtr

-- @method drawPoint@ Draw a point into a @ref type Drawable@.
--
drawPoint :: DrawableClass d => d -> GC -> Point -> IO ()
drawPoint d gc (x,y) = {#call unsafe draw_point#} (toDrawable d)
  (toGC gc) (fromIntegral x) (fromIntegral y)


-- @method drawPoints@ Draw several points into a @ref type Drawable@.
--
-- * This function is more efficient than calling @ref method drawPoint@ on
--   several points.
--
drawPoints :: DrawableClass d => d -> GC -> [Point] -> IO ()
drawPoints d gc []     = return ()
drawPoints d gc points = 
  withArray (concatMap (\(x,y) -> [fromIntegral x, fromIntegral y]) points) $
  \(aPtr :: Ptr {#type gint#}) -> {#call unsafe draw_points#} (toDrawable d)
    (toGC gc) (castPtr aPtr) (fromIntegral (length points))

-- @method drawLine@ Draw a line into a @ref type Drawable@.
--
-- * The parameters are x1, y1, x2, y2.
--
-- * Drawing several separate lines can be done more efficiently by
--   @ref method drawSegments@.
--
drawLine :: DrawableClass d => d -> GC -> Point -> Point -> IO ()
drawLine d gc (x1,y1) (x2,y2) = {#call unsafe draw_line#} (toDrawable d)
  (toGC gc) (fromIntegral x1) (fromIntegral y1) (fromIntegral x2) 
  (fromIntegral x2)

-- @method drawLines@ Draw several lines.
--
-- * The function uses the current line width, dashing and especially the
--   joining specification in the graphics context (in contrast to
--   @ref method drawSegments@.
--
drawLines :: DrawableClass d => d -> GC -> [Point] -> IO ()
drawLines d gc []     = return ()
drawLines d gc points =
  withArray (concatMap (\(x,y) -> [fromIntegral x, fromIntegral y]) points) $
  \(aPtr :: Ptr {#type gint#}) -> {#call unsafe draw_lines#} (toDrawable d)
    (toGC gc) (castPtr aPtr) (fromIntegral (length points))

#if GTK_CHECK_VERSION(2,2,0)
-- @method drawPixbuf@ Render a @ref data Pixbuf@.
--
-- * Renders a rectangular portion of a @ref data Pixbuf@ to a
--   @ref data Drawable@. The @ref arg srcX@, @ref arg srcY@,
--   @ref arg srcWidth@ and @ref arg srcHeight@ specify what part of the
--   @ref data Pixbuf@ should be rendered. The latter two values may be
--   @literal -1@ in which case the width and height are taken from
--   @ref arg src@. The image is placed at @ref arg destX@, @ref arg destY@.
--   If you render parts of an image at a time, set @ref arg ditherX@ and
--   @ref arg ditherY@ to the origin of the image you are rendering.
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

-- @method drawSegments@ Draw several unconnected lines.
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

-- @method drawRectangle@ Draw a rectangular object.
--
-- * Draws a rectangular outline or filled rectangle, using the
--   foreground color and other attributes of the @ref type GC@.
--
-- * A rectangle drawn filled is 1 pixel smaller in both dimensions
--   than a rectangle outlined. Calling @ref method drawRectangle@ w gc
--   True 0 0 20 20 results in a filled rectangle 20 pixels wide and 20
--   pixels high. Calling @ref method drawRectangle@ d gc False 0 0 20 20
--   results in an outlined rectangle with corners at (0, 0), (0, 20), (20,
--   20), and (20, 0), which makes it 21 pixels wide and 21 pixels high.
--
drawRectangle :: DrawableClass d => d -> GC -> Bool -> Int -> Int ->
				       Int -> Int -> IO ()
drawRectangle d gc filled x y width height = {#call unsafe draw_rectangle#}
  (toDrawable d) (toGC gc) (fromBool filled) (fromIntegral x)
  (fromIntegral y) (fromIntegral width) (fromIntegral height)

-- @method drawArc@ Draws an arc or a filled 'pie slice'.
--
-- * The arc is defined by the bounding rectangle of the entire
--   ellipse, and the start and end angles of the part of the ellipse to be
--   drawn.
--
-- * The starting angle @ref arg aStart@ is relative to the 3 o'clock
--   position, counter-clockwise, in 1/64ths of a degree. @ref arg aEnd@
--   is measured similarly, but relative to @ref arg aStart@.
--
drawArc :: DrawableClass d => d -> GC -> Bool -> Int -> Int ->
				 Int -> Int -> Int -> Int -> IO ()
drawArc d gc filled x y width height aStart aEnd =
  {#call unsafe draw_arc#} (toDrawable d) (toGC gc) (fromBool filled)
  (fromIntegral x) (fromIntegral y) (fromIntegral width) (fromIntegral height)
  (fromIntegral aStart) (fromIntegral aEnd)

-- @method drawPolygon@ Draws an outlined or filled polygon.
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

-- @method drawLayoutLine@ Draw a single line of text.
--
-- * The @ref arg x@ coordinate specifies the start of the string,
--   the @ref arg y@ coordinate specifies the base line.
--
drawLayoutLine :: DrawableClass d => d -> GC -> Int -> Int -> LayoutLine ->
				     IO ()
drawLayoutLine d gc x y text =
  {#call unsafe draw_layout_line#} (toDrawable d) (toGC gc)
    (fromIntegral x) (fromIntegral y) text

-- @method drawLayoutLineWithColors@ Draw a single line of text.
--
-- * The @ref arg x@ coordinate specifies the start of the string,
--   the @ref arg y@ coordinate specifies the base line.
--
-- * If both colors are @literal Nothing@ this function will behave like
--   @method drawLayoutLine@ in that it uses the default colors from
--   the graphics context.
--
drawLayoutLineWithColors :: DrawableClass d => d -> GC -> Int -> Int ->
			    LayoutLine -> Maybe Color -> Maybe Color -> IO ()
drawLayoutLineWithColors d gc x y text foreground background = let
    withMB :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
    withMB Nothing f = f nullPtr
    withMB (Just x) f = with' x f
  in
    withMB foreground $ \fPtr -> withMB background $ \bPtr ->
    {#call unsafe draw_layout_line_with_colors#} (toDrawable d) (toGC gc)
      (fromIntegral x) (fromIntegral y) text (castPtr fPtr) (castPtr bPtr)


-- @method drawLayout@ Draw a paragraph of text.
--
-- * The @ref arg x@ and @ref arg y@ values specify the upper left
--   point of the layout. 
--
drawLayout :: DrawableClass d => d -> GC -> Int -> Int -> PangoLayout -> IO ()
drawLayout d gc x y text =
  {#call unsafe draw_layout#} (toDrawable d) (toGC gc)
    (fromIntegral x) (fromIntegral y) (toPangoLayout text)

-- @method drawLayoutWithColors@ Draw a paragraph of text.
--
-- * The @ref arg x@ and @ref arg y@ values specify the upper left
--   point of the layout. 
--
-- * If both colors are @literal Nothing@ this function will behave like
--   @method drawLayout@ in that it uses the default colors from
--   the graphics context.
--
drawLayoutWithColors :: DrawableClass d => d -> GC -> Int -> Int ->
			PangoLayout -> Maybe Color -> Maybe Color -> IO ()
drawLayoutWithColors d gc x y text foreground background = let
    withMB :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
    withMB Nothing f = f nullPtr
    withMB (Just x) f = with' x f
  in
    withMB foreground $ \fPtr -> withMB background $ \bPtr ->
    {#call unsafe draw_layout_with_colors#} (toDrawable d) (toGC gc)
      (fromIntegral x) (fromIntegral y) (toPangoLayout text)
      (castPtr fPtr) (castPtr bPtr)


-- @method drawDrawable@ Copies another @ref type Drawable@.
--
-- * Copies the (width,height) region of the @ref arg src@ at coordinates
--   (@ref arg xSrc@, @ref arg ySrc@) to coordinates (@ref arg xDest@,
--   @ref arg yDest@) in the @ref arg dest@. The @ref arg width@ and/or
--   @ref arg height@ may be given as -1, in which case the entire source
--   drawable will be copied.
--
-- * Most fields in @ref arg gc@ are not used for this operation, but
--   notably the clip mask or clip region will be honored.  The source and
--   destination drawables must have the same visual and colormap, or
--   errors will result. (On X11, failure to match visual/colormap results
--   in a BadMatch error from the X server.)  A common cause of this
--   problem is an attempt to draw a bitmap to a color drawable. The way to
--   draw a bitmap is to set the bitmap as a clip mask on your
--   @ref type GC@, then use @ref method drawRectangle@ to draw a 
--   rectangle clipped to the bitmap.
--
drawDrawable :: (DrawableClass src, DrawableClass dest) => 
		dest -> GC -> src -> Int -> Int -> Int -> Int -> 
		Int -> Int -> IO ()
drawDrawable dest gc src xSrc ySrc xDest yDest width height =
  {#call unsafe draw_drawable#} (toDrawable dest) (toGC gc)
  (toDrawable src)
  (fromIntegral xSrc) (fromIntegral ySrc) (fromIntegral xDest)
  (fromIntegral yDest) (fromIntegral width) (fromIntegral height)


