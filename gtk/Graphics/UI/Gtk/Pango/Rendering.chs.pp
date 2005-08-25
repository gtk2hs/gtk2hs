-- -*-haskell-*-
--  GIMP Toolkit (GTK) - text layout functions Rendering
--
--  Author : Axel Simon
--
--  Created: 8 Feburary 2003
--
--  Version $Revision: 1.6 $ from $Date: 2005/08/25 13:10:06 $
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
-- * The Pango rendering pipeline takes a string of Unicode characters,
--   divides them into sequences of letters that have the same characteristics
--   such as font, size, color, etc. Such a sequence is called 'PangoItem'.
--   Each 'PangoItem' is then converted into an actual sequence of glyphs,
--   where several characters might me turned into legatures or clusters,
--   like "e" and an accent modifier, are turned into a single glyph. These
--   'GlyphItem's can then be rendered onto the output device with functions
--   such as 'Graphics.UI.Gtk.Cairo.Rendering.showGlyphString'.
--
module Graphics.UI.Gtk.Pango.Rendering (
-- * Types and Methods for Rendering
  PangoItem,
  pangoItemize,
  pangoShape,
  PangoAttribute(..),

-- * Types and Methods for 'PangoContext's
  PangoContext,
  PangoDirection(..),
  contextListFamilies,
--  contextLoadFont,
--  contextLoadFontSet,
  contextGetMetrics,
  FontMetrics(..),
  contextSetFontDescription,
  contextGetFontDescription,
  Language,
  languageFromString,
  contextSetLanguage,
  contextGetLanguage,
  contextSetTextDir,
  contextGetTextDir

  ) where

import Monad    (liftM)
import Data.Ratio

import System.Glib.FFI
import Graphics.UI.Gtk.General.Structs  (pangoScale, PangoDirection(..),
					 setAttrPos, Color(..),
					 pangoItemRawAnalysis )
{#import Graphics.UI.Gtk.Types#}
import System.Glib.GObject  (makeNewGObject)
import Graphics.UI.Gtk.General.Enums
{#import Graphics.UI.Gtk.Pango.Types#}
{#import Graphics.UI.Gtk.Pango.Enums#}
import System.Glib.UTFString ( withUTFString, UTFCorrection )
{#import System.Glib.GList#}
import Data.List ( sortBy )

{# context lib="pango" prefix="pango" #}

-- | Turn a string into a sequence of glyphs.
--
-- * Partitions the input string into segments with the same text direction
--   and shaping engine. The generated list of items will be in logical order
--   (the start offsets of the items are ascending).
--
pangoItemize :: PangoContext -> String -> [PangoAttribute] -> IO [PangoItem]
pangoItemize pc str attrs = do
  ps <- makeNewPangoString str
  withAttrList ps attrs $ \alPtr -> do
    glist <- withPangoString ps $ \_ l strPtr ->
	     {#call unsafe itemize#} pc strPtr 0 l alPtr nullPtr
    piPtrs <- fromGList glist
    piRaws <- mapM makeNewPangoItemRaw piPtrs
    return (map (PangoItem ps) piRaws)

-- | Turn a 'PangoItem' into a 'GlyphItem'.
--
-- * Turns a 'PangoItem', that is, sequence of characters with the same
--   attributes such as font, size and color, into a 'GlyphItem' which
--   contains the graphical representation of these characters. 'GlyphItem's
--   can be rendered directly (and several times) onto screens.
--
pangoShape :: PangoItem -> IO GlyphItem
pangoShape pi@(PangoItem ps pir) =
  withPangoString ps $ \_ l strPtr -> withPangoItemRaw pir $ \pirPtr -> do
  gsPtr <- {#call unsafe glyph_string_new#}
  gs <- makeNewGlyphStringRaw gsPtr
  {#call unsafe shape#} strPtr l (pangoItemRawAnalysis pirPtr) gs
  return (GlyphItem pi gs)

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
  -- c2hs gets FontFamily*** wrong as FontFamily**, therefore the cast
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
	  (ascend % fromIntegral pangoScale)
	  (descend % fromIntegral pangoScale)
	  (cWidth % fromIntegral pangoScale)
	  (dWidth % fromIntegral pangoScale))

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
  -- | The approximate digit width. This is merely a representative
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
contextSetLanguage = {#call unsafe context_set_language#}

-- | Get the current 'Language' of this context.
--
contextGetLanguage :: PangoContext -> IO Language
contextGetLanguage pc = liftM Language $
			{#call unsafe context_get_language#} pc

-- | Set the default text direction of this context.
--
contextSetTextDir :: PangoContext -> PangoDirection -> IO ()
contextSetTextDir pc dir =
  {#call unsafe context_set_base_dir#} pc (fromIntegral (fromEnum dir))

-- | Get the current text direction of this context.
--
contextGetTextDir :: PangoContext -> IO PangoDirection
contextGetTextDir pc = liftM (toEnum . fromIntegral) $
		       {#call unsafe context_get_base_dir#} pc

-- | Attributes for 'PangoItem's.
--
-- * A given attribute is applied from its start position 'paStart' up,
--   but not including the end position, 'paEnd'.
--
data PangoAttribute
  -- | A hint as to what language this piece of text is written in.
  = AttrLanguage { paStart :: Int, paEnd :: Int, paLang :: Language }
  -- | The font family, e.g. "sans serif".
  | AttrFamily { paStart :: Int, paEnd :: Int, paFamily :: String }
  -- | The slant of the current font.
  | AttrStyle { paStart :: Int, paEnd :: Int, paStyle :: FontStyle }
  -- | Weight of font, e.g. 'WeightBold'.
  | AttrWeight { paStart :: Int, paEnd :: Int, paWeight :: Weight }
  -- | 'VariantSmallCaps' will display lower case letters as small
  -- upper case letters (if the font supports this).
  | AttrVariant { paStart :: Int, paEnd :: Int, paVariant :: Variant }
  -- | Stretch or condense the width of the letters.
  | AttrStretch { paStart :: Int, paEnd :: Int, paStretch :: Stretch }
  -- | Specify the size of the font in points.
  | AttrSize { paStart :: Int, paEnd :: Int, paSize :: PangoUnit }
#if PANGO_CHECK_VERSION(1,8,0)
  -- | Specify the size of the font in device units (pixels).
  --
  -- * Available in Pango 1.8.0 and higher.
  --
  | AttrAbsSize { paStart :: Int, paEnd :: Int, paSize :: PangoUnit }
#endif
  -- | Specify several attributes of a font at once.
  | AttrFontDescription { paStart :: Int, paEnd :: Int,
			  paFontDescription :: FontDescription }
  -- | Specify the foreground color.
  | AttrForeground { paStart :: Int, paEnd :: Int, paColor :: Color }
  -- | Specify the background color.
  | AttrBackground { paStart :: Int, paEnd :: Int, paColor :: Color }
  -- | Specify the kind of underline, e.g. 'UnderlineSingle'.
  | AttrUnderline { paStart :: Int, paEnd :: Int, paUnderline :: Underline }
#if PANGO_CHECK_VERSION(1,8,0)
  -- | Specify the color of an underline.
  --
  -- * Available in Pango 1.8.0 and higher.
  --
  | AttrUnderlineColor { paStart :: Int, paEnd :: Int, paColor :: Color }
#endif
  -- | Specify if this piece of text should have a line through it.
  | AttrStrikethrough { paStart :: Int, paEnd :: Int, paStrikethrough :: Bool }
#if PANGO_CHECK_VERSION(1,8,0)
  -- | Specify the color of the strike through line.
  --
  -- * Available in Pango 1.8.0 and higher.
  --
  | AttrStrikethroughColor { paStart :: Int, paEnd :: Int, paColor :: Color }
#endif
  -- | Displace the text vertically. Positive values move the text upwards.
  | AttrRise { paStart :: Int, paEnd :: Int, paRise :: PangoUnit }
#if PANGO_CHECK_VERSION(1,8,0)
  -- | Restrict the amount of what is drawn of the marked shapes.
  --
  -- * Available in Pango 1.8.0 and higher.
  --
  | AttrShape { paStart :: Int, paEnd :: Int, paInk :: PangoRectangle,
		paLogical :: PangoRectangle }
#endif
  -- | Scale the font up (values greater than one) or shrink the font.
  | AttrScale { paStart :: Int, paEnd :: Int, paScale :: Double }
  -- | Determine if a fall back font should be substituted if no matching
  -- font is available.
  | AttrFallback { paStart :: Int, paEnd :: Int, paFallback :: Bool }
  -- | Add extra space between graphemes of the text.
  | AttrLetterSpacing { paStart :: Int, paEnd :: Int, 
			paLetterSpacing :: PangoUnit }
 
-- Attributes
{#pointer *PangoAttrList #}

-- Create an attribute list.
withAttrList :: PangoString -> [PangoAttribute] -> (Ptr () -> IO a) -> IO a
withAttrList _ [] act = act nullPtr
withAttrList (PangoString correct _ _) pas act = do
  alPtr <- {#call unsafe attr_list_new#}
  let pas' = sortBy (\pa1 pa2 -> case compare (paStart pa1) (paStart pa2) of
		     EQ -> compare (paEnd pa1) (paEnd pa2)
		     other -> other) pas
  mapM_ (\pa -> do
	   paPtr <- crAttr correct pa
	   {#call unsafe pango_attr_list_insert#} alPtr (castPtr paPtr)) pas'
  res <- act alPtr
  {#call unsafe attr_list_unref#} alPtr
  return res

-- Create a PangoAttribute.
crAttr :: UTFCorrection -> PangoAttribute -> IO (Ptr ())
crAttr c AttrLanguage { paStart=s, paEnd=e, paLang = lang } =
  setAttrPos c s e $ {#call unsafe attr_language_new#} lang
crAttr c AttrFamily { paStart=s, paEnd=e, paFamily = fam } =
  setAttrPos c s e $ withUTFString fam $ {#call unsafe attr_family_new#}
crAttr c AttrStyle { paStart=s, paEnd=e, paStyle = style } =
  setAttrPos c s e $
  {#call unsafe attr_style_new#} (fromIntegral (fromEnum style))
crAttr c AttrWeight { paStart=s, paEnd=e, paWeight = weight } =
  setAttrPos c s e $
  {#call unsafe attr_weight_new#} (fromIntegral (fromEnum weight))
crAttr c AttrVariant { paStart=s, paEnd=e, paVariant = variant } =
  setAttrPos c s e $
  {#call unsafe attr_variant_new#} (fromIntegral (fromEnum variant))
crAttr c AttrStretch { paStart=s, paEnd=e, paStretch = stretch } =
  setAttrPos c s e $
  {#call unsafe attr_stretch_new#} (fromIntegral (fromEnum stretch))
crAttr c AttrSize { paStart=s, paEnd=e, paSize = pu } =
  setAttrPos c s e $ {#call unsafe attr_size_new#} (puToInt pu)
#if PANGO_CHECK_VERSION(1,8,0)
crAttr c AttrAbsSize { paStart=s, paEnd=e, paSize = pu } =
  setAttrPos c s e $ {#call unsafe attr_size_new_absolute#} (puToInt pu)
#endif
crAttr c AttrFontDescription { paStart=s, paEnd=e, paFontDescription = fd } =
  setAttrPos c s e $ {#call unsafe attr_font_desc_new#} fd
crAttr c AttrForeground { paStart=s, paEnd=e, paColor = Color r g b } =
  setAttrPos c s e $ {#call unsafe attr_foreground_new#}
  (fromIntegral r) (fromIntegral b) (fromIntegral b)
crAttr c AttrBackground { paStart=s, paEnd=e, paColor = Color r g b } =
  setAttrPos c s e $ {#call unsafe attr_background_new#}
  (fromIntegral r) (fromIntegral b) (fromIntegral b)
crAttr c AttrUnderline { paStart=s, paEnd=e, paUnderline = underline } =
  setAttrPos c s e $ do
  {#call unsafe attr_underline_new#} (fromIntegral (fromEnum underline))
#if PANGO_CHECK_VERSION(1,8,0)
crAttr c AttrUnderlineColor {paStart=s, paEnd=e, paColor = Color r g b } =
  setAttrPos c s e $ {#call unsafe attr_underline_color_new#}
  (fromIntegral r) (fromIntegral b) (fromIntegral b)
#endif
crAttr c AttrStrikethrough { paStart=s, paEnd=e, paStrikethrough = st } =
  setAttrPos c s e $ do
  {#call unsafe attr_strikethrough_new#} (fromIntegral (fromEnum st))
#if PANGO_CHECK_VERSION(1,8,0)
crAttr c AttrStrikethroughColor {paStart=s, paEnd=e, paColor = Color r g b } =
  setAttrPos c s e $ {#call unsafe attr_strikethrough_color_new#}
  (fromIntegral r) (fromIntegral b) (fromIntegral b)
#endif
crAttr c AttrRise { paStart=s, paEnd=e, paRise = pu } =
  setAttrPos c s e $ {#call unsafe attr_rise_new#} (puToInt pu)
#if PANGO_CHECK_VERSION(1,8,0)
crAttr c AttrShape { paStart=s, paEnd=e, paInk = rect1, paLogical = rect2 } =
  setAttrPos c s e $ alloca $ \rect1Ptr -> alloca $ \rect2Ptr -> do
    poke rect1Ptr (toRect rect1)
    poke rect2Ptr (toRect rect2)
    {#call unsafe attr_shape_new#} (castPtr rect1Ptr) (castPtr rect2Ptr)
#endif
crAttr c AttrScale { paStart=s, paEnd=e, paScale = scale } =
  setAttrPos c s e $ 
  {#call unsafe attr_scale_new#} (realToFrac scale)
crAttr c AttrFallback { paStart=s, paEnd=e, paFallback = fb } =
  setAttrPos c s e $
  {#call unsafe attr_fallback_new#} (fromBool fb)
crAttr c AttrLetterSpacing { paStart=s, paEnd=e, paLetterSpacing = pu } =
  setAttrPos c s e $
  {#call unsafe attr_letter_spacing_new#} (puToInt pu)
