--  GIMP Toolkit (GTK) - text layout functions Rendering
--
--  Author : Axel Simon
--          
--  Created: 8 Feburary 2003
--
--  Version $Revision: 1.4 $ from $Date: 2004/12/12 11:18:41 $
--
--  Copyright (c) 1999..2003 Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- |
--
-- Functions to run the rendering pipeline.
--
-- * The Pango rendering pipeline takes a string of Unicode characters
--   and converts it into glyphs.  The functions described in this module
--   accomplish various steps of this process.
--
module Rendering(
  PangoContext,
  contextListFamilies,
--  contextLoadFont,
--  contextLoadFontSet,
  contextGetMetrics,
  FontMetrics(..),
  contextSetFontDescription,
  contextGetFontDescription,
  contextSetLanguage,
  contextGetLanguage,
  contextSetTextDir,
  contextGetTextDir,
  TextDirection(..)
  ) where

import Monad    (liftM)
import FFI
import Structs  (pangoScale)
{#import Hierarchy#}
import GObject  (makeNewGObject)
import Enums
{#import PangoTypes#}
import Data.Ratio

{# context lib="pango" prefix="pango" #}


-- | Retrieve a list of all available font families.
--
-- * A font family is the name of the font without further attributes
--   like slant, variant or size.
--
contextListFamilies :: PangoContext -> IO [FontFamily]
contextListFamilies c = alloca $ \sizePtr -> alloca $ \ptrPtr -> do
  {#call unsafe context_list_families#} c ptrPtr sizePtr
  ptr <- peek ptrPtr
  size <- peek sizePtr
  -- c2hs get FontFamily*** wrong as FontFamily**, therefore the cast
  familyPtrs <- peekArray (fromIntegral size) (castPtr ptr)
  fams <- mapM (makeNewGObject mkFontFamily . return) familyPtrs
  {#call unsafe g_free#} (castPtr ptr)
  return fams

-- | Load a font.
--
--contextLoadFont :: PangoContext -> FontDescription -> Language ->
--		   IO (Maybe Font)
--contextLoadFont pc fd l = do
--  fsPtr <- {#call context_load_font#} pc fd l
--  if fsPtr==nullPtr then return Nothing else
--    liftM Just $ makeNewGObject mkFont (return fsPtr)

-- | Load a font set.
--
--contextLoadFontSet :: PangoContext -> FontDescription -> Language ->
--		      IO (Maybe FontSet)
--contextLoadFontSet pc fd l = do
--  fsPtr <- {#call context_load_fontset#} pc fd l
--  if fsPtr==nullPtr then return Nothing else
--    liftM Just $ makeNewGObject mkFontSet (return fsPtr)

-- | Query the metrics of the given font implied by the font description.
--
contextGetMetrics :: PangoContext -> FontDescription -> Language ->
		     IO FontMetrics
contextGetMetrics pc fd l = do
  mPtr <- {#call unsafe context_get_metrics#} pc fd l
  ascend <- liftM fromIntegral $ {#call unsafe font_metrics_get_ascent#} mPtr
  descend <- liftM fromIntegral $ {#call unsafe font_metrics_get_descent#} mPtr
  cWidth <- liftM fromIntegral $
	    {#call unsafe font_metrics_get_approximate_char_width#} mPtr
  dWidth <- liftM fromIntegral $
	    {#call unsafe font_metrics_get_approximate_digit_width#} mPtr
  {#call unsafe font_metrics_unref#} mPtr
  return (FontMetrics
	  (ascend % pangoScale)
	  (descend % pangoScale)
	  (cWidth % pangoScale)
	  (dWidth % pangoScale))

-- | The characteristic measurements of a font.
--
-- * All values are measured in pixels.
--
data FontMetrics = FontMetrics {
  -- | The ascent is the distance from the baseline to the logical top
  --   of a line of text. (The logical top may be above or below the
  --   top of the actual drawn ink. It is necessary to lay out the
  --   text to figure where the ink will be.)
  ascent :: Rational,
  -- | The descent is the distance from the baseline to the logical
  --   bottom of a line of text. (The logical bottom may be above or
  --   below the bottom of the actual drawn ink. It is necessary to
  --   lay out the text to figure where the ink will be.)
  descent :: Rational,
  -- | The approximate character width. This is merely a
  --   representative value useful, for example, for determining the
  --   initial size for a window. Actual characters in text will be
  --   wider and narrower than this.
  approximateCharWidth :: Rational,
  -- | The approximate digit widt. This is merely a representative
  --   value useful, for example, for determining the initial size for
  --   a window. Actual digits in text can be wider and narrower than
  --   this, though this value is generally somewhat more accurate
  --   than @approximateCharWidth@.
  approximateDigitWidth :: Rational
}

-- | Set the default 'FontDescription' of this context.
--
contextSetFontDescription :: PangoContext -> FontDescription -> IO ()
contextSetFontDescription pc fd =
  {#call unsafe context_set_font_description#} pc fd

-- | Get the current 'FontDescription' of this context.
--
contextGetFontDescription :: PangoContext -> IO FontDescription
contextGetFontDescription pc = do
  fdPtrConst <- {#call unsafe context_get_font_description#} pc
  fdPtr <- pango_font_description_copy fdPtrConst
  makeNewFontDescription fdPtr

foreign import ccall unsafe "pango_font_description_copy"
  pango_font_description_copy :: Ptr FontDescription -> 
				 IO (Ptr FontDescription)

-- | Set the default 'Language' of this context.
--
contextSetLanguage :: PangoContext -> Language -> IO ()
contextSetLanguage pc (Language l) = {#call unsafe context_set_language#} pc l

-- | Get the current 'Language' of this context.
--
contextGetLanguage :: PangoContext -> IO Language
contextGetLanguage pc = liftM Language $
			{#call unsafe context_get_language#} pc

-- only used internally
{#enum PangoDirection {underscoreToCase} #}

-- | Set the default text direction of this context.
--
contextSetTextDir :: PangoContext -> TextDirection -> IO ()
contextSetTextDir pc dir =
  {#call unsafe context_set_base_dir#} pc (convert dir)
  where
    convert TextDirNone = fromIntegral (fromEnum DirectionNeutral)
    convert TextDirLtr = fromIntegral (fromEnum DirectionLtr)
    convert TextDirRtl = fromIntegral (fromEnum DirectionRtl)

-- | Get the current text direction of this context.
--
contextGetTextDir :: PangoContext -> IO TextDirection
contextGetTextDir pc = liftM (convert . toEnum . fromIntegral) $
		       {#call unsafe context_get_base_dir#} pc
  where
  convert DirectionLtr = TextDirLtr
  convert DirectionRtl = TextDirRtl
  convert _ = TextDirNone

