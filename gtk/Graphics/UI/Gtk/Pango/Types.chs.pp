-- -*-haskell-*-
--  GIMP Toolkit (GTK) - pango non-GObject types PangoTypes
--
--  Author : Axel Simon
--
--  Created: 9 Feburary 2003
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
-- Define types used in Pango which are not derived from GObject.
--
module Graphics.UI.Gtk.Pango.Types (
  Markup,

  GInt,
  PangoUnit,
  PangoRectangle(PangoRectangle),
  PangoDirection(..),

  PangoString(PangoString),
  makeNewPangoString,
  withPangoString,

  PangoItem(PangoItem),
  PangoItemRaw(PangoItemRaw),
  makeNewPangoItemRaw,
  withPangoItemRaw,

  GlyphItem(GlyphItem),
  GlyphStringRaw(GlyphStringRaw),
  makeNewGlyphStringRaw,

  PangoLayout(PangoLayout),

  LayoutIter(LayoutIter),
  LayoutIterRaw(LayoutIterRaw),
  makeNewLayoutIterRaw,

  LayoutLine(LayoutLine),
  LayoutLineRaw(LayoutLineRaw),
  makeNewLayoutLineRaw,
  FontDescription(FontDescription),
  makeNewFontDescription,
  Language(Language),
  emptyLanguage,
  languageFromString,

  FontMetrics(..),
  FontStyle(..),
  Weight(..),
  Variant(..),
  Stretch(..),
  Underline(..),
#if PANGO_CHECK_VERSION(1,16,0)
  PangoGravity(..),
  PangoGravityHint(..),
#endif
  PangoAttribute(..),
  PangoAttrList,
  CPangoAttribute,
  ) where

import Control.Monad (liftM)
import Data.IORef ( IORef )
import System.Glib.FFI
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#} (Font, PangoLayoutRaw)
import Graphics.UI.Gtk.General.Structs (Color(..))

{# context lib="pango" prefix="pango" #}

-- | Define a synonym for text with embedded markup commands.
--
-- * Markup strings are just simple strings. But it's easier to tell if a
--   method expects text with or without markup.
--
type Markup = String

-- because stupid hsc2hs and c2hs do not agree on the type of gint (Int32 vs
-- CInt)
type GInt = {#type gint#}

-- A pango unit is an internal euclidian metric, that is, a measure for 
-- lengths and position.
--
-- * Deprecated. Replaced by Double.
type PangoUnit = Double

-- | Rectangles describing an area in 'Double's.
--
-- * Specifies x, y, width and height
--
data PangoRectangle = PangoRectangle Double Double Double Double
		      deriving Show

-- | The 'PangoDirection' type represents a direction in the Unicode
-- bidirectional algorithm.
--
-- * The \"weak\" values denote a left-to-right or right-to-left direction
--   only if there is no character with a strong direction in a paragraph.
--   An example is a sequence of special, graphical characters which are
--   neutral with respect to their rendering direction. A fresh
--   'Graphics.UI.Gtk.Pango.Rendering.PangoContext' is by default weakly
--   left-to-right.
--
-- * Not every value in this enumeration makes sense for every usage
--   of 'PangoDirection'; for example, the return value of
--   'unicharDirection' and 'findBaseDir' cannot be 'PangoDirectionWeakLtr'
--   or 'PangoDirectionWeakRtl', since every character is either neutral or
--   has a strong direction; on the other hand 'PangoDirectionNeutral'
--   doesn't make sense to pass to 'log2visGetEmbeddingLevels'.
--
data PangoDirection = PangoDirectionLtr
                    | PangoDirectionRtl
#if PANGO_CHECK_VERSION(1,4,0)
                    | PangoDirectionWeakLtr
                    | PangoDirectionWeakRtl
                    | PangoDirectionNeutral
#endif
                    deriving (Eq,Ord)

-- A string that is stored with each GlyphString, PangoItem
data PangoString = PangoString UTFCorrection CInt (ForeignPtr CChar)

makeNewPangoString :: String -> IO PangoString
makeNewPangoString str = do
  let correct = genUTFOfs str
  (strPtr, len) <- newUTFStringLen str
  let cLen = fromIntegral len
  liftM (PangoString correct cLen) $ newForeignPtr strPtr finalizerFree

withPangoString :: PangoString -> 
		   (UTFCorrection -> CInt -> Ptr CChar -> IO a) -> IO a
withPangoString (PangoString c l ptr) act = withForeignPtr ptr $ \strPtr ->
  act c l strPtr

-- paired with PangoString to create a Haskell GlyphString
{#pointer *PangoGlyphString as GlyphStringRaw foreign newtype #}

makeNewGlyphStringRaw :: Ptr GlyphStringRaw -> IO GlyphStringRaw
makeNewGlyphStringRaw llPtr = do
  liftM GlyphStringRaw $ newForeignPtr llPtr pango_glyph_string_free

foreign import ccall unsafe "&pango_glyph_string_free"
  pango_glyph_string_free :: FinalizerPtr GlyphStringRaw

-- paired with PangoString and UTFCorrection to create a Haskell PangoItem
{#pointer *PangoItem as PangoItemRaw foreign newtype #}

makeNewPangoItemRaw :: Ptr PangoItemRaw -> IO PangoItemRaw
makeNewPangoItemRaw llPtr = do
  liftM PangoItemRaw $ newForeignPtr llPtr pango_item_free

withPangoItemRaw :: PangoItemRaw -> (Ptr PangoItemRaw -> IO a) -> IO a
withPangoItemRaw (PangoItemRaw pir) act = withForeignPtr pir act

foreign import ccall unsafe "&pango_item_free"
  pango_item_free :: FinalizerPtr PangoItemRaw

#if PANGO_CHECK_VERSION(1,2,0)
{#pointer *PangoGlyphItem as GlyphItemRaw #}
#endif

-- With each GlyphString we pair a UTFCorrection
-- and the marshalled UTF8 string. Together, this data
-- enables us to bind all functions that take or return
-- indices into the CString, rather then unicode position. Note that text
-- handling is particularly horrible with UTF8: Several UTF8 bytes can make
-- up one Unicode character (a Haskell Char), and several Unicode characters
-- can form a cluster (e.g. a letter and an accent). We protect the user from
-- UTF8\/Haskell String conversions, but not from clusters.

-- | A sequence of characters that are rendered with the same settings.
--
-- * A preprocessing stage done by 'itemize' splits the input text into
--   several chunks such that each chunk can be rendered with the same
--   font, direction, slant, etc. Some attributes such as the color,
--   underline or strikethrough do not affect a break into several
--   'PangoItem's. See also 'GlyphItem'.
--
data PangoItem = PangoItem PangoString PangoItemRaw

-- | A sequence of glyphs for a chunk of a string.
--
-- * A glyph item contains the graphical representation of a 'PangoItem'.
--   Clusters (like @e@ and an accent modifier) as well as legatures
--   (such as @ffi@ turning into a single letter that omits the dot over the
--   @i@) are usually represented as a single glyph. 
--
data GlyphItem = GlyphItem PangoItem GlyphStringRaw 

-- | A rendered paragraph.
data PangoLayout = PangoLayout (IORef PangoString) PangoLayoutRaw

-- | An iterator to examine a layout.
--
data LayoutIter = LayoutIter (IORef PangoString) LayoutIterRaw

{#pointer *PangoLayoutIter as LayoutIterRaw foreign newtype #}

makeNewLayoutIterRaw :: Ptr LayoutIterRaw -> IO LayoutIterRaw
makeNewLayoutIterRaw liPtr =
  liftM LayoutIterRaw $ newForeignPtr liPtr layout_iter_free

foreign import ccall unsafe "&pango_layout_iter_free"
  layout_iter_free :: FinalizerPtr LayoutIterRaw

-- | A single line in a 'PangoLayout'.
--
data LayoutLine = LayoutLine (IORef PangoString) LayoutLineRaw

{#pointer *PangoLayoutLine as LayoutLineRaw foreign newtype #}

makeNewLayoutLineRaw :: Ptr LayoutLineRaw -> IO LayoutLineRaw
makeNewLayoutLineRaw llPtr = do
  liftM LayoutLineRaw $ newForeignPtr llPtr pango_layout_line_unref

foreign import ccall unsafe "&pango_layout_line_unref"
  pango_layout_line_unref :: FinalizerPtr LayoutLineRaw

-- | A possibly partial description of font(s).
--
{#pointer *PangoFontDescription as FontDescription foreign newtype #}

makeNewFontDescription :: Ptr FontDescription -> IO FontDescription
makeNewFontDescription llPtr = do
  liftM FontDescription $ newForeignPtr llPtr pango_font_description_free

foreign import ccall unsafe "&pango_font_description_free"
  pango_font_description_free :: FinalizerPtr FontDescription

-- | An RFC-3066 language designator to choose scripts.
--
{#pointer* Language newtype#} deriving Eq

instance Show Language where
  show (Language ptr)
    | ptr==nullPtr = ""
    | otherwise = unsafePerformIO $ peekUTFString (castPtr ptr)

-- | Specifying no particular language.
emptyLanguage = Language nullPtr

-- | Take a RFC-3066 format language tag as a string and convert it to a
--  'Language' type that can be efficiently passed around and compared with
--  other language tags.
--
-- * This function first canonicalizes the string by converting it to
--   lowercase, mapping \'_\' to \'-\', and stripping all characters
--   other than letters and \'-\'.
--
languageFromString :: String -> IO Language
languageFromString language = liftM Language $
  withUTFString language {#call language_from_string#}

-- | The characteristic measurements of a font.
--
-- * All values are measured in pixels.
--
-- * In Pango versions before 1.6 only 'ascent', 'descent',
--   'approximateCharWidth' and 'approximateDigitWidth' are available.
--
data FontMetrics = FontMetrics {
  -- | The ascent is the distance from the baseline to the logical top
  --   of a line of text. (The logical top may be above or below the
  --   top of the actual drawn ink. It is necessary to lay out the
  --   text to figure where the ink will be.)
  ascent :: Double,
  -- | The descent is the distance from the baseline to the logical
  --   bottom of a line of text. (The logical bottom may be above or
  --   below the bottom of the actual drawn ink. It is necessary to
  --   lay out the text to figure where the ink will be.)
  descent :: Double,
  -- | The approximate character width. This is merely a
  --   representative value useful, for example, for determining the
  --   initial size for a window. Actual characters in text will be
  --   wider and narrower than this.
  approximateCharWidth :: Double,
  -- | The approximate digit width. This is merely a representative
  --   value useful, for example, for determining the initial size for
  --   a window. Actual digits in text can be wider and narrower than
  --   this, though this value is generally somewhat more accurate
  --   than 'approximateCharWidth'.
  approximateDigitWidth :: Double
#if PANGO_CHECK_VERSION(1,6,0)
  ,
  -- | The suggested thickness to draw an underline.
  underlineThickness :: Double,
  -- | The suggested position to draw the underline. The value returned is
  --   the distance above the baseline of the top of the underline. Since
  --   most fonts have underline positions beneath the baseline, this value
  --   is typically negative.
  underlinePosition :: Double,
  -- | The suggested thickness to draw for the strikethrough.
  strikethroughThickenss :: Double,
  -- | The suggested position to draw the strikethrough. The value
  --   returned is the distance above the baseline of the top of the
  --   strikethrough.
  strikethroughPosition :: Double
#endif
  } deriving Show

-- | The style of a font.
--
-- * 'StyleOblique' is a slanted font like 'StyleItalic', 
--   but in a roman style.
--
{#enum Style as FontStyle {underscoreToCase} deriving (Eq)#}

instance Show FontStyle where
  showsPrec _ StyleNormal	   = shows "normal"
  showsPrec _ StyleOblique	   = shows "oblique"
  showsPrec _ StyleItalic	   = shows "italic"

-- | Define attributes for 'Weight'.
--
{#enum Weight {underscoreToCase} deriving (Eq)#}

instance Show Weight where
  showsPrec _ WeightUltralight	= shows "ultralight"
  showsPrec _ WeightLight	= shows "light"
  showsPrec _ WeightNormal	= shows "normal"
  showsPrec _ WeightBold	= shows "bold"
  showsPrec _ WeightUltrabold 	= shows "ultrabold"
  showsPrec _ WeightHeavy	= shows "heavy"

-- | The variant of a font.
--
-- * The 'VariantSmallCaps' is a version of a font where lower case
--   letters are shown as physically smaller upper case letters.
--
{#enum Variant {underscoreToCase} deriving (Eq)#}

instance Show Variant where
  showsPrec _ VariantNormal       = shows "normal"
  showsPrec _ VariantSmallCaps    = shows "smallcaps"

-- | Define how wide characters are.
--
{#enum Stretch {underscoreToCase} deriving (Eq)#}

instance Show Stretch where
  showsPrec _ StretchUltraCondensed	= shows "ultracondensed"
  showsPrec _ StretchExtraCondensed	= shows "extracondensed"
  showsPrec _ StretchCondensed		= shows "condensed"
  showsPrec _ StretchSemiCondensed	= shows "semicondensed"
  showsPrec _ StretchNormal		= shows "normal"
  showsPrec _ StretchSemiExpanded	= shows "semiexpanded"
  showsPrec _ StretchExpanded		= shows "expanded"
  showsPrec _ StretchExtraExpanded	= shows "extraexpanded"
  showsPrec _ StretchUltraExpanded	= shows "ultraexpanded"

-- | Define attributes for 'Underline'.
--
-- * The squiggly underline for errors is only available in Gtk 2.4 and higher.
--
{#enum Underline {underscoreToCase} deriving (Eq)#}

instance Show Underline where
  showsPrec _ UnderlineNone	= shows "none"
  showsPrec _ UnderlineSingle	= shows "single"
  showsPrec _ UnderlineDouble	= shows "double"
  showsPrec _ UnderlineLow	= shows "low"
#if GTK_CHECK_VERSION(2,4,0)
  showsPrec _ UnderlineError	= shows "error"
#endif

#if PANGO_CHECK_VERSION(1,16,0)
-- |  The 'PangoGravity' type represents the orientation of glyphs in a
-- segment of text. The value 'GravitySouth', for instance, indicates that the
-- text stands upright, i.e. that the base of the letter is directed
-- downwards.
--
-- This is useful when rendering vertical text layouts. In those situations,
-- the layout is rotated using a non-identity 'PangoMatrix', and then glyph
-- orientation is controlled using 'PangoGravity'. Not every value in this
-- enumeration makes sense for every usage of 'Gravity'; for example,
-- 'PangoGravityAuto' only can be passed to 'pangoContextSetBaseGravity' and
-- can only be returned by 'pangoContextGetBaseGravity'.
--
-- * See also: 'PangoGravityHint'
--
-- * Gravity is resolved from the context matrix.
--
-- * Since Pango 1.16
--
{#enum PangoGravity {underscoreToCase} with prefix="" deriving (Eq)#}

instance Show PangoGravity where
  show PangoGravitySouth = "south"
  show PangoGravityEast = "east"
  show PangoGravityNorth = "north"
  show PangoGravityWest = "west"
  show PangoGravityAuto = "auto"
  
-- | The 'PangoGravityHint' defines how horizontal scripts should behave in a
-- vertical context.
--
-- * 'PangoGravityHintNatural': scripts will take their natural gravity based
--   on the base gravity and the script. This is the default.
--
-- * 'PangoGravityHintStrong': always use the base gravity set, regardless of
--   the script.
--
-- * 'PangoGravityHintLine': for scripts not in their natural direction (eg.
--   Latin in East gravity), choose per-script gravity such that every script
--   respects the line progression. This means, Latin and Arabic will take
--   opposite gravities and both flow top-to-bottom for example.
--
{#enum PangoGravityHint {underscoreToCase} with prefix="" deriving (Eq)#}

instance Show PangoGravityHint where
  show PangoGravityHintNatural = "natural"
  show PangoGravityHintStrong = "storng"
  show PangoGravityHintLine = "line"

#endif

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
  | AttrSize { paStart :: Int, paEnd :: Int, paSize :: Double }
#if PANGO_CHECK_VERSION(1,8,0)
  -- | Specify the size of the font in device units (pixels).
  --
  -- * Available in Pango 1.8.0 and higher.
  --
  | AttrAbsSize { paStart :: Int, paEnd :: Int, paSize :: Double }
#endif
  -- | Specify several attributes of a font at once. Note that no deep copy
  --   of the description is made when this attributes is passed to or received
  --   from functions.
    | AttrFontDescription { paStart :: Int, paEnd :: Int,
			  paFontDescription :: FontDescription }
  -- | Specify the foreground color.
  | AttrForeground { paStart :: Int, paEnd :: Int, paColor :: Color }
  -- | Specify the background color.
  | AttrBackground { paStart :: Int, paEnd :: Int, paColor :: Color }
  -- | Specify the kind of underline, e.g. 'UnderlineSingle'.
  | AttrUnderline { paStart :: Int, paEnd :: Int, paUnderline :: Underline }
#if  (defined (WIN32) && PANGO_CHECK_VERSION(1,10,0)) \
 || (!defined (WIN32) && PANGO_CHECK_VERSION(1,8,0))
  -- | Specify the color of an underline.
  --
  -- * Available in Pango 1.8.0 and higher.
  --
  | AttrUnderlineColor { paStart :: Int, paEnd :: Int, paColor :: Color }
#endif
  -- | Specify if this piece of text should have a line through it.
  | AttrStrikethrough { paStart :: Int, paEnd :: Int, paStrikethrough :: Bool }
#if  (defined (WIN32) && PANGO_CHECK_VERSION(1,10,0)) \
 || (!defined (WIN32) && PANGO_CHECK_VERSION(1,8,0))
  -- | Specify the color of the strike through line.
  --
  -- * Available in Pango 1.8.0 and higher.
  --
  | AttrStrikethroughColor { paStart :: Int, paEnd :: Int, paColor :: Color }
#endif
  -- | Displace the text vertically. Positive values move the text upwards.
  | AttrRise { paStart :: Int, paEnd :: Int, paRise :: Double }
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
#if PANGO_CHECK_VERSION(1,4,0)
  -- | Determine if a fall back font should be substituted if no matching
  -- font is available.
  | AttrFallback { paStart :: Int, paEnd :: Int, paFallback :: Bool }
#endif
#if PANGO_CHECK_VERSION(1,6,0)
  -- | Add extra space between graphemes of the text.
  --
  -- * Available in Pango 1.6.0 and higher.
  --
  | AttrLetterSpacing { paStart :: Int, paEnd :: Int, 
			paLetterSpacing :: Double }
#endif
#if PANGO_CHECK_VERSION(1,16,0)
  -- | Sets the gravity field of a font description. The gravity field specifies
  -- how the glyphs should be rotated. If gravity is 'GravityAuto', this
  -- actually unsets the gravity mask on the font description.
  --
  -- * This function is seldom useful to the user. Gravity should normally be
  --   set on a 'PangoContext'.
  --
  -- * Available in Pango 1.16.0 and higher.
  --
  | AttrGravity { paStart :: Int, paEnd :: Int, 
			paGravity :: PangoGravity }

	-- | Set the way horizontal scripts behave in a vertical context.
  --
  -- * Available in Pango 1.16.0 and higher.
  --
	| AttrGravityHint  { paStart :: Int, paEnd :: Int, 
			paGravityHint :: PangoGravityHint }
#endif
  deriving Show

-- dirty hack to make the above showable
instance Show FontDescription where
  show fd = unsafePerformIO $ do
    strPtr <- {#call unsafe font_description_to_string#} fd
    str <- peekUTFString strPtr
    {#call unsafe g_free#} (castPtr strPtr)
    return str

-- Attributes
{#pointer *PangoAttrList #}

{#pointer *PangoAttribute as CPangoAttribute#}

