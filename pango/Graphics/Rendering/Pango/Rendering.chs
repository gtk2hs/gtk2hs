{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) - text layout functions Rendering
--
--  Author : Axel Simon
--
--  Created: 8 Feburary 2003
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
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Functions to run the rendering pipeline.
--
-- * This module provides elementary rendering functions. For a simpler
--   interface, consider using 'PangoLayout's.
--
-- * The Pango rendering pipeline takes a string of Unicode characters,
--   divides them into sequences of letters that have the same characteristics
--   such as font, size, color, etc. Such a sequence is called 'PangoItem'.
--   Each 'PangoItem' is then converted into one 'GlyphItem', that is
--   an actual sequence of glyphs,
--   where several characters might be turned into legatures or clusters,
--   e.g. an \"e\" and an accent modifier are turned into a single glyph. These
--   'GlyphItem's can then be rendered onto the output device with functions
--   such as 'Graphics.Rendering.Cairo.cairoShowGlyphString'.
--
module Graphics.Rendering.Pango.Rendering (
  -- * 'PangoItem': Partition text into units with similar attributes.
  PangoItem,
  pangoItemize,
  pangoItemGetFontMetrics,
  pangoItemGetFont,
  pangoItemGetLanguage,

  -- * 'GlyphItem': Turn text segments into glyph sequences.
  GlyphItem,
  pangoShape,
  glyphItemExtents,
  glyphItemExtentsRange,
  glyphItemIndexToX,
  glyphItemXToIndex,
  glyphItemGetLogicalWidths,
#if PANGO_VERSION_CHECK(1,2,0)
  glyphItemSplit
#endif
  ) where

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.Rendering.Pango.Structs  ( pangoItemRawAnalysis, intToPu,
  pangoItemRawGetOffset, pangoItemRawGetLength,
  pangoItemGetFont, pangoItemGetLanguage)
{#import Graphics.Rendering.Pango.Types#}       (PangoContext(..), Font(..))
{#import Graphics.Rendering.Pango.BasicTypes#}
{#import Graphics.Rendering.Pango.Enums#}
{#import Graphics.Rendering.Pango.Attributes#}
import Graphics.Rendering.Pango.GlyphStorage
{#import System.Glib.GList#}

{# context lib="pango" prefix="pango" #}

-- | Turn a string into a sequence of glyphs.
--
-- * Partitions the input string into segments with the same text direction
--   and shaping engine. The generated list of items will be in logical order
--   (the start offsets of the items are ascending).
--
pangoItemize :: GlibString string => PangoContext -> string -> [PangoAttribute] -> IO [PangoItem]
pangoItemize pc str attrs = do
  ps <- makeNewPangoString str
  withAttrList ps attrs $ \alPtr -> do
    glist <- withPangoString ps $ \_ l strPtr ->
             {#call unsafe itemize#} pc strPtr 0 l alPtr nullPtr
    piPtrs <- fromGList glist
    piRaws <- mapM makeNewPangoItemRaw piPtrs
    return (map (PangoItem ps) piRaws)


-- | Retrieve the metrics of the font that was chosen to break the given
--   'PangoItem'.
--
pangoItemGetFontMetrics :: PangoItem -> IO FontMetrics
pangoItemGetFontMetrics pi = do
  font <- pangoItemGetFont pi
  lang <- pangoItemGetLanguage pi
  mPtr <- {#call unsafe font_get_metrics#} font lang
  ascent <- {#call unsafe font_metrics_get_ascent#} mPtr
  descent <- {#call unsafe font_metrics_get_descent#} mPtr
  approximate_char_width <-
      {#call unsafe font_metrics_get_approximate_char_width#} mPtr
  approximate_digit_width <-
      {#call unsafe font_metrics_get_approximate_digit_width#} mPtr
#if PANGO_VERSION_CHECK(1,6,0)
  underline_position <-
      {#call unsafe font_metrics_get_underline_position#} mPtr
  underline_thickness <-
      {#call unsafe font_metrics_get_underline_thickness#} mPtr
  strikethrough_position <-
      {#call unsafe font_metrics_get_strikethrough_position#} mPtr
  strikethrough_thickness <-
      {#call unsafe font_metrics_get_strikethrough_thickness#} mPtr
#endif
  return (FontMetrics
          (intToPu ascent)
          (intToPu descent)
          (intToPu approximate_char_width)
          (intToPu approximate_digit_width)
#if PANGO_VERSION_CHECK(1,6,0)
          (intToPu underline_position)
          (intToPu underline_thickness)
          (intToPu strikethrough_position)
          (intToPu strikethrough_thickness)
#endif
         )

-- | Turn a 'PangoItem' into a 'GlyphItem'.
--
-- * Turns a 'PangoItem', that is, sequence of characters with the same
--   attributes such as font, size and color, into a 'GlyphItem' which
--   contains the graphical representation of these characters. 'GlyphItem's
--   can be rendered directly (and several times) onto screens.
--
pangoShape :: PangoItem -> IO GlyphItem
pangoShape pi@(PangoItem ps pir) =
  withPangoString ps $ \_ _ strPtr -> withPangoItemRaw pir $ \pirPtr -> do
  gsPtr <- {#call unsafe glyph_string_new#}
  gs <- makeNewGlyphStringRaw gsPtr
  ofs <- pangoItemRawGetOffset pirPtr
  len <- pangoItemRawGetLength pirPtr
  {#call unsafe shape#} (strPtr `plusPtr` (fromIntegral ofs)) (fromIntegral len)
    (pangoItemRawAnalysis pirPtr) gs
  return (GlyphItem pi gs)
