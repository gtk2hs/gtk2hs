-- -*-haskell-*-
--  GIMP Toolkit (GTK) Glyph Storage of Pango
--
--  Author : Axel Simon
--
--  Created: 31 July 2005
--
--  Version $Revision: 1.1 $ from $Date: 2005/08/25 07:22:29 $
--
--  Copyright (C) 2005 Axel Simon
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
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- 
--
module Graphics.UI.Gtk.Pango.GlyphStorage (
  -- * Types
  GlyphItem,

  -- * Methods
  glyphItemExtents,
  glyphItemExtentsRange,
  glyphItemIndexToX,
  glyphItemXToIndex,
  glyphItemGetLogicalWidths,
  glyphItemSplit
  ) where

import Monad	(liftM)
import System.Glib.FFI
{#import Graphics.UI.Gtk.Types#}
import System.Glib.UTFString
import System.Glib.GList                (readGSList)
import System.Glib.GObject              (makeNewGObject)
{#import Graphics.UI.Gtk.Pango.Types#}
import Graphics.UI.Gtk.General.Structs (PangoDirection(..), pangodirToLevel,
					pangoItemRawAnalysis,
					pangoItemRawGetLevel )
import Graphics.UI.Gtk.General.Enums (TextDirection(..))
import Control.Exception ( throwIO,
			   Exception(ArrayException),
			   ArrayException(IndexOutOfBounds) )
{# context lib="pango" prefix="pango" #}


--------------------
-- Methods

-- | Ask for bounding rectangles of this glyph sequence.
--
-- * Compute the logical and ink extents of a glyph string. The
--   logical extend is used for positioning, the ink size is the smallest
--   bounding box that includes all character pixels. The ink size can be
--   smaller or larger that the logical layout.
--
--
glyphItemExtents :: GlyphItem -> IO (PangoRectangle, PangoRectangle)
glyphItemExtents (GlyphItem pi self) = do
  font <- pangoItemGetFont pi
  alloca $ \logPtr -> alloca $ \inkPtr -> do
  {#call unsafe glyph_string_extents#} self font
    (castPtr logPtr) (castPtr inkPtr)
  log <- peek logPtr
  ink <- peek inkPtr
  return (fromRect log, fromRect ink)

-- | Ask for bounding rectangles for a sub-range of a glyph sequence.
--
-- * The returned rectangles are relative to the given sub-range, that is,
--   the result of this function is the same as if 'glyphItemExtents'
--   were called on the sub-string.
--
glyphItemExtentsRange :: GlyphItem -> Int -> Int -> 
			   IO (PangoRectangle, PangoRectangle)

glyphItemExtentsRange (GlyphItem pi@(PangoItem (PangoString uc _ _) _) self)
  start end = do
  font <- pangoItemGetFont pi
  alloca $ \logPtr -> alloca $ \inkPtr -> do
  {#call unsafe glyph_string_extents_range#} self
    (fromIntegral (ofsToUTF start uc)) (fromIntegral (ofsToUTF end uc))
    font (castPtr logPtr) (castPtr inkPtr)
  log <- peek logPtr
  ink <- peek inkPtr
  return (fromRect log, fromRect ink)

-- | Get the horizontal position of a character.
--
-- * Clusters (e.g. \"e\" with an accent modifier) are divided up into equal
--   portions.
--
glyphItemIndexToX :: GlyphItem -- ^ the rendered string
		    -> Int -- ^ the index into the string
		    -> Bool -- ^ return the beginning (@False@) or the end 
			    -- of the character
		    -> IO PangoUnit
glyphItemIndexToX (GlyphItem (PangoItem ps pir) gs) pos beg =
  withPangoItemRaw pir $ \pirPtr -> alloca $ \intPtr -> 
  withPangoString ps $ \uc l strPtr -> do
    {# call unsafe glyph_string_index_to_x #} gs strPtr
      (fromIntegral l) (pangoItemRawAnalysis pirPtr) 
      (fromIntegral (ofsToUTF pos uc)) (fromBool beg) intPtr
    liftM intToPu $ peek intPtr

-- | Get the character at the given horizontal position.
--
-- * The position is clipped to the width of this line.
--
-- * The function returns the position in the string that corresponds
--   to the given horizontal location. Furthermore, if the position lies
--   on the first half of the character, @False@ is returned.
--
glyphItemXToIndex :: GlyphItem -> PangoUnit -> IO (Int, Bool)
glyphItemXToIndex (GlyphItem (PangoItem ps pir) gs) pos =
  withPangoItemRaw pir $ \pirPtr -> alloca $ \intPtr -> 
  alloca $ \boolPtr -> withPangoString ps $ \uc l strPtr -> do
    {# call unsafe pango_glyph_string_x_to_index #} gs strPtr
      (fromIntegral l) (pangoItemRawAnalysis pirPtr) (puToInt pos)
      intPtr boolPtr
    int <- peek intPtr
    bool <- peek boolPtr
    return (ofsFromUTF (fromIntegral int) uc, toBool bool)

-- | Retrieve the width of every character in a string.
--
-- * Note that text of both directions can be mixed in one
--   'GlyphItem', hence, in general, it is not possible to calculate
--   positions of characters with this function. The boolean parameter
--   determines if the returned array starts with the leftmost glyph
--   (@False@) or with the rightmost glyph (@True@). If @Nothing@ is
--   passed in, the direction is taken from the 'GlyphItem'.
--
glyphItemGetLogicalWidths :: GlyphItem -> Maybe Bool -> IO [PangoUnit]
glyphItemGetLogicalWidths (GlyphItem (PangoItem ps pir) gs) mDir = do
  dir <- case mDir of
    Just dir -> return dir
    Nothing -> withPangoItemRaw pir pangoItemRawGetLevel
  withPangoString ps $ \uc l strPtr -> do
    logLen <- {#call unsafe g_utf8_strlen#} strPtr (fromIntegral l)
    allocaArray (fromIntegral logLen) $ \arrPtr -> do
      {# call unsafe pango_glyph_string_get_logical_widths #}
        gs strPtr (fromIntegral l) (fromBool dir) arrPtr
      elems <- peekArray (fromIntegral logLen) arrPtr
      return (map intToPu elems)

-- | Split a 'GlyphItem' at the given index.
--
-- * The given 'GlyphItem' is split at the given index. The index must be
--   at least one and not greater or equal to length, i.e. the item must
--   be split into two non-empty segments. The function throws an
--   'ArrayException' if the index is out of bounds.
--
glyphItemSplit :: GlyphItem -> Int -> IO (GlyphItem, GlyphItem)
glyphItemSplit (GlyphItem (PangoItem ps pir) gs) pos = do
  pirPtr1 <- {#call unsafe pango_item_copy#} pir
  gsrPtr1 <- {#call unsafe glyph_string_copy#} gs
  pir1 <- makeNewPangoItemRaw pirPtr1
  gsr1 <- makeNewGlyphStringRaw gsrPtr1
  allocaBytes {#sizeof PangoGlyphItem#} $ \giPtr1 -> do
    {#set PangoGlyphItem.item#} giPtr1 pirPtr1
    {#set PangoGlyphItem.glyphs#} giPtr1 gsrPtr1
    giPtr2 <- withPangoString ps $ \uc l strPtr ->
      {#call unsafe pango_glyph_item_split#} giPtr1 strPtr
        (fromIntegral (ofsToUTF pos uc))
    if giPtr2==nullPtr then 
       throwIO (ArrayException (IndexOutOfBounds
	 ("Graphics.UI.Gtk.Pango.GlyphStorage."++
	  "glyphItemSplit: cannot split item at index "++show pos))) else do
      pirPtr2 <- {#get PangoGlyphItem.item#} giPtr2
      gsrPtr2 <- {#get PangoGlyphItem.glyphs#} giPtr2
      {#call unsafe g_free#} giPtr2
      pir2 <- makeNewPangoItemRaw pirPtr2
      gsr2 <- makeNewGlyphStringRaw gsrPtr2
      return (GlyphItem (PangoItem ps pir2) gsr2,
	      GlyphItem (PangoItem ps pir1) gsr1)
