--  -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Region@
--
--  Author : Axel Simon
--  Created: 22 September 2002
--
--  Version $Revision: 1.2 $ from $Date: 2002/11/03 20:35:42 $
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
--  A set of rectangles describing areas to be redrawn.
--
-- @documentation@ ------------------------------------------------------------
--
-- * Regions consist of a set of non-overlapping rectangles. They are used to
--   specify the area of a window which needs updating.
--
-- @todo@ ---------------------------------------------------------------------
--
-- * The Span functions and callbacks are not implemented since retrieving
--   a set of rectangles and working on them within Haskell seems to be easier.
--
module Region(
  makeNewRegion,
  Region,
  regionNew,
  FillRule(..),
  regionPolygon,
  regionCopy,
  regionRectangle,
  regionGetClipbox,
  regionGetRectangles,
  regionEmpty,
  regionEqual,
  regionPointIn,
  OverlapType(..),
  regionRectIn,
  regionOffset,
  regionShrink,
  regionUnionWithRect,
  regionIntersect,
  regionUnion,
  regionSubtract,
  regionXor) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Structs	(Point, Rectangle(..))
import GdkEnums (FillRule(..), OverlapType(..))

{# context lib="gtk" prefix="gdk" #}

{#pointer *GdkRegion as Region foreign newtype #}

-- Construct a region from a pointer.
--
makeNewRegion :: Ptr Region -> IO Region
makeNewRegion rPtr = do
  region <- newForeignPtr rPtr (regionDestroy rPtr)
  return (Region region)

foreign import ccall "gdk_region_destroy" unsafe
  regionDestroy :: Ptr Region -> IO ()

-- @constructor regionNew@ Create an empty region.
--
regionNew :: IO Region
regionNew = do
  rPtr <- {#call unsafe region_new#}
  makeNewRegion rPtr

-- @constructor regionPolygon@ Convert a polygon into a @ref data Region@.
--
regionPolygon :: [Point] -> FillRule -> IO Region
regionPolygon points rule =
  withArray (concatMap (\(x,y) -> [fromIntegral x, fromIntegral y]) points) $
  \(aPtr :: Ptr {#type gint#}) -> do
    rPtr <- {#call unsafe region_polygon#} (castPtr aPtr) 
	    (fromIntegral (length points)) ((fromIntegral.fromEnum) rule)
    makeNewRegion rPtr

-- @method regionCopy@ Copy a @ref data Region@.
--
regionCopy :: Region -> IO Region
regionCopy r = do
  rPtr <- {#call unsafe region_copy#} r
  makeNewRegion rPtr

-- @constructor regionRectangle@ Convert a rectangle to a @ref data Region@.
--
regionRectangle :: Rectangle -> IO Region
regionRectangle rect = withObject rect $ \rectPtr -> do
  regPtr <- {#call unsafe region_rectangle#} (castPtr rectPtr)
  makeNewRegion regPtr

-- @method regionGetClipbox@ Smallest rectangle including the 
-- @ref data Region@.
--
regionGetClipbox :: Region -> IO Rectangle
regionGetClipbox r = alloca $ \rPtr -> do
  {#call unsafe region_get_clipbox#} r (castPtr rPtr)
  peek rPtr

-- @method regionGetRectangles@ Turn the @ref data Region@ into its rectangles.
--
-- * A @ref data Region@ is a set of horizontal bands. Each band
--   consists of one or more rectangles of the same height. No rectangles
--   in a band touch.
--
regionGetRectangles :: Region -> IO [Rectangle]
regionGetRectangles r = 
  alloca $ \(aPtr :: Ptr Rectangle) -> 
  alloca $ \(iPtr :: Ptr {#type gint#}) -> do
    {#call unsafe region_get_rectangles#} r (castPtr aPtr) iPtr
    size <- peek iPtr
    regs <- peekArray (fromIntegral size) aPtr
    {#call unsafe g_free#} (castPtr aPtr)
    return regs

-- @method regionEmpty@ Test if a @ref data Region@ is empty.
--
regionEmpty :: Region -> IO Bool
regionEmpty r = liftM toBool $ {#call unsafe region_empty#} r

-- @method regionEqual@ Compares two @ref data Region@s for equality.
--
regionEqual :: Region -> Region -> IO Bool
regionEqual r1 r2 = liftM toBool $ {#call unsafe region_equal#} r1 r2

-- @method regionPointIn@ Checks if a point it is within a region.
--
regionPointIn :: Region -> Point -> IO Bool
regionPointIn r (x,y) = liftM toBool $ 
  {#call unsafe region_point_in#} r (fromIntegral x) (fromIntegral y)

-- @method regionRectIn@ Check if a rectangle is within a region.
--
regionRectIn :: Region -> Rectangle -> IO OverlapType
regionRectIn reg rect = liftM (toEnum.fromIntegral) $ withObject rect $
  \rPtr -> {#call unsafe region_rect_in#} reg (castPtr rPtr)

-- @method regionOffset@ Move a region.
--
regionOffset :: Region -> Int -> Int -> IO ()
regionOffset r dx dy = 
  {#call unsafe region_offset#} r (fromIntegral dx) (fromIntegral dy)

-- @method regionShrink@ Move a region.
--
-- * Positive values shrink the region, negative values expand it.
--
regionShrink :: Region -> Int -> Int -> IO ()
regionShrink r dx dy = 
  {#call unsafe region_shrink#} r (fromIntegral dx) (fromIntegral dy)

-- @method regionUnionWithRect@ Updates the region to include the rectangle.
--
regionUnionWithRect :: Region -> Rectangle -> IO ()
regionUnionWithRect reg rect = withObject rect $ \rPtr ->
  {#call unsafe region_union_with_rect#} reg (castPtr rPtr)

-- @method regionInterset@ Intersects one region with another.
--
-- * Changes @ref arg reg1@ to include the common areas of @ref arg reg1@
--   and @ref arg reg2@.
--
regionIntersect :: Region -> Region -> IO ()
regionIntersect reg1 reg2 = {#call unsafe region_intersect#} reg1 reg2

-- @method regionInterset@ Unions one region with another.
--
-- * Changes @ref arg reg1@ to include @ref arg reg1@ and @ref arg reg2@.
--
regionUnion :: Region -> Region -> IO ()
regionUnion reg1 reg2 = {#call unsafe region_union#} reg1 reg2

-- @method regionSubtract@ Removes pars of a @ref data Region@.
--
-- * Reduces the region @ref arg reg1@ so that is does not include any areas
--   of @ref arg reg2@.
--
regionSubtract :: Region -> Region -> IO ()
regionSubtract reg1 reg2 = {#call unsafe region_subtract#} reg1 reg2

-- @method regionXor@ XORs two @ref data Region@s.
--
-- * The exclusive or of two regions contains all areas which were not
--   overlapping. In other words, it is the union of the regions minus
--   their intersections.
--
regionXor :: Region -> Region -> IO ()
regionXor reg1 reg2 = {#call unsafe region_xor#} reg1 reg2


