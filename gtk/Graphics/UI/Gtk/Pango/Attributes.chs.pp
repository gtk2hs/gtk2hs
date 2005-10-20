-- -*-haskell-*-
--  GIMP Toolkit (GTK) - pango text attributes
--
--  Author : Axel Simon
--
--  Created: 20 October 2005
--
--  Version $Revision: 1.1 $ from $Date: 2005/10/20 23:05:25 $
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
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Defines text attributes.
--
module Graphics.UI.Gtk.Pango.Attributes (
  PangoAttribute(..),
  PangoAttrList(..),
  withAttrList
  ) where

import Monad (liftM)
import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.General.Structs  (setAttrPos, Color(..))
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Pango.Types#}
{#import Graphics.UI.Gtk.Pango.Enums#}
import Data.List ( sortBy )

{# context lib="pango" prefix="pango" #}


-- | The characteristic measurements of a font.
--
-- * All values are measured in points, expressed as 'PangoUnit's.
--
-- * The last four fields are only available in Pango 1.6 or higher.
--
data FontMetrics = FontMetrics {
  -- | The ascent is the distance from the baseline to the logical top
  --   of a line of text. (The logical top may be above or below the
  --   top of the actual drawn ink. It is necessary to lay out the
  --   text to figure where the ink will be.)
  ascent :: PangoUnit,
  -- | The descent is the distance from the baseline to the logical
  --   bottom of a line of text. (The logical bottom may be above or
  --   below the bottom of the actual drawn ink. It is necessary to
  --   lay out the text to figure where the ink will be.)
  descent :: PangoUnit,
  -- | The approximate character width. This is merely a
  --   representative value useful, for example, for determining the
  --   initial size for a window. Actual characters in text will be
  --   wider and narrower than this.
  approximateCharWidth :: PangoUnit,
  -- | The approximate digit width. This is merely a representative
  --   value useful, for example, for determining the initial size for
  --   a window. Actual digits in text can be wider and narrower than
  --   this, though this value is generally somewhat more accurate
  --   than 'approximateCharWidth'.
  approximateDigitWidth :: PangoUnit
#if PANGO_CHECK_VERSION(1,6,0)
  ,
  -- | The suggested thickness to draw an underline.
  underlineThickness :: PangoUnit,
  -- | The suggested position to draw the underline. The value returned is
  --   the distance above the baseline of the top of the underline. Since
  --   most fonts have underline positions beneath the baseline, this value
  --   is typically negative.
  underlinePosition :: PangoUnit,
  -- | The suggested thickness to draw for the strikethrough.
  strikethroughThickenss :: PangoUnit,
  -- | The suggested position to draw the strikethrough. The value
  --   returned is the distance above the baseline of the top of the
  --   strikethrough.
  strikethroughPosition :: PangoUnit
#endif
  } deriving Show


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
