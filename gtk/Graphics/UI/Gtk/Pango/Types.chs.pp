-- -*-haskell-*-
--  GIMP Toolkit (GTK) - pango non-GObject types PangoTypes
--
--  Author : Axel Simon
--
--  Created: 9 Feburary 2003
--
--  Version $Revision: 1.13 $ from $Date: 2005/10/25 19:51:37 $
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
  PangoUnit(PangoUnit),
  puToInt, puToUInt,
  intToPu, uIntToPu,
  PangoRectangle(PangoRectangle),
  fromRect,
  toRect,

  PangoString(PangoString),
  makeNewPangoString,
  withPangoString,

  PangoItem(PangoItem),
  PangoItemRaw(PangoItemRaw),
  makeNewPangoItemRaw,
  withPangoItemRaw,
  pangoItemGetFont,
  pangoItemGetLanguage,

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

  FontMetrics(..)
  ) where

import Monad (liftM)
import Data.Bits (shiftR)
import Data.Ratio ((%))
import Numeric ( showFFloat )
import Data.IORef ( IORef )
import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.General.Structs ( pangoScale, Rectangle(..),
					 pangoItemRawGetFont,
					 pangoItemRawGetLanguage )
{#import Graphics.UI.Gtk.Types#}

{# context lib="pango" prefix="pango" #}

-- | A pango unit is an internal euclidian metric, that is, a measure for 
--   lengths and position.
--
-- * Converting pango units to integers or rationals results in device units
--   which are pixels for on-screen texts and points (1\/72 inch) for printers.
--   Internally, a pango unit
--   is a device unit scaled by 1024, hence allowing
--   more precise sub-pixel\/sub-point calculations. As a rule of thumb,
--   any pango function that passes sizes or positions as @Int@ expects
--   a measurement in points, all function that take a 'PangoUnit'
--   expect a measures in pixels. However, some functions that take
--   'PangoUnit's or 'PangoRectangle's have convenience variant that
--   take 'Int's as measurements in pixels. A 'PangoUnit's can be converted
--   to and from device units (pixels or points) using @fromIntegral@ and
--   @fromRational@, respectively.
--
newtype PangoUnit = PangoUnit Int
  deriving (Eq, Ord)

puToInt :: PangoUnit -> {#type gint#}
puToInt (PangoUnit u) = fromIntegral u

puToUInt :: PangoUnit -> {#type guint#}
puToUInt (PangoUnit u) = fromIntegral u

intToPu :: {#type gint#} -> PangoUnit
intToPu i = PangoUnit (fromIntegral i)

uIntToPu :: {#type guint#} -> PangoUnit
uIntToPu i = PangoUnit (fromIntegral i)

instance Show PangoUnit where
  showsPrec _ pu = showFFloat (Just 2) (realToFrac pu)

instance Enum PangoUnit where
  succ (PangoUnit u) = PangoUnit (u+1)
  pred (PangoUnit u) = PangoUnit (u-1)
  toEnum u = PangoUnit u
  fromEnum (PangoUnit u) = u

instance Num PangoUnit where
  (PangoUnit u1) + (PangoUnit u2) = PangoUnit (u1+u2)
  (PangoUnit u1) - (PangoUnit u2) = PangoUnit (u1-u2)
  (PangoUnit u1) * (PangoUnit u2) = PangoUnit (u1*u2 `div` pangoScale)
  negate (PangoUnit u) = PangoUnit (-u)
  abs (PangoUnit u) = PangoUnit (abs u)
  signum (PangoUnit u) = PangoUnit (pangoScale * signum u)
  fromInteger i = PangoUnit (fromIntegral i * pangoScale)

instance Real PangoUnit where
  toRational (PangoUnit u) = fromIntegral u % fromIntegral pangoScale

instance Integral PangoUnit where
  (PangoUnit u1) `quot` (PangoUnit u2) = PangoUnit (u1*pangoScale `quot` u2)
  (PangoUnit u1) `rem` (PangoUnit u2) = PangoUnit (u1*pangoScale `rem` u2)
  pu1 `quotRem` pu2 = (pu1 `quot` pu2, pu1 `rem` pu2)
  (PangoUnit u1) `div` (PangoUnit u2) = PangoUnit (u1*pangoScale `div` u2)
  (PangoUnit u1) `mod` (PangoUnit u2) = PangoUnit (u1*pangoScale `mod` u2)
  pu1 `divMod` pu2 = (pu1 `div` pu2, pu1 `mod` pu2)
  toInteger (PangoUnit u) = fromIntegral 
    ((u+(pangoScale `shiftR` 1)) `div` pangoScale)

instance Fractional PangoUnit where
  (PangoUnit u1) / (PangoUnit u2) = PangoUnit (u1*pangoScale `quot` u2)
  fromRational r = PangoUnit (round (r*fromIntegral pangoScale))


-- | Rectangles describing an area in 'PangoUnit's.
--
-- * Specifies x, y, width and height
--
data PangoRectangle = PangoRectangle PangoUnit PangoUnit PangoUnit PangoUnit

-- Cheating functions: We marshal PangoRectangles as Rectangles.
fromRect :: Rectangle -> PangoRectangle
fromRect (Rectangle x y w h) =
  PangoRectangle (PangoUnit x) (PangoUnit y) (PangoUnit w) (PangoUnit h)

toRect :: PangoRectangle -> Rectangle
toRect (PangoRectangle (PangoUnit x) (PangoUnit y) (PangoUnit w) 
	(PangoUnit h)) = Rectangle x y w h
  

-- A string that is stored with each GlyphString, PangoItem
data PangoString = PangoString UTFCorrection CInt (ForeignPtr CChar)

makeNewPangoString :: String -> IO PangoString
makeNewPangoString str = do
  let correct = genUTFOfs str
  (strPtr, len) <- newUTFStringLen str
  let cLen = fromIntegral len
#if __GLASGOW_HASKELL__>=600
  liftM (PangoString correct cLen) $ newForeignPtr strPtr finalizerFree
#else
  liftM (PangoString correct cLen) $ newForeignPtr strPtr free
#endif

withPangoString :: PangoString -> 
		   (UTFCorrection -> CInt -> Ptr CChar -> IO a) -> IO a
withPangoString (PangoString c l ptr) act = withForeignPtr ptr $ \strPtr ->
  act c l strPtr

-- paired with PangoString to create a Haskell GlyphString
{#pointer *PangoGlyphString as GlyphStringRaw foreign newtype #}

makeNewGlyphStringRaw :: Ptr GlyphStringRaw -> IO GlyphStringRaw
makeNewGlyphStringRaw llPtr = do
  liftM GlyphStringRaw $ newForeignPtr llPtr (pango_glyph_string_free llPtr)


#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "&pango_glyph_string_free"
  pango_glyph_string_free' :: FinalizerPtr GlyphStringRaw

pango_glyph_string_free :: Ptr GlyphStringRaw -> FinalizerPtr GlyphStringRaw
pango_glyph_string_free _ = pango_glyph_string_free'

#else

foreign import ccall unsafe "pango_glyph_string_free"
  pango_glyph_string_free :: Ptr GlyphStringRaw -> IO ()

#endif

-- paired with PangoString and UTFCorrection to create a Haskell PangoItem
{#pointer *PangoItem as PangoItemRaw foreign newtype #}

makeNewPangoItemRaw :: Ptr PangoItemRaw -> IO PangoItemRaw
makeNewPangoItemRaw llPtr = do
  liftM PangoItemRaw $ newForeignPtr llPtr (pango_item_free llPtr)

withPangoItemRaw :: PangoItemRaw -> (Ptr PangoItemRaw -> IO a) -> IO a
withPangoItemRaw (PangoItemRaw pir) act = withForeignPtr pir act

#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "&pango_item_free"
  pango_item_free' :: FinalizerPtr PangoItemRaw

pango_item_free :: Ptr PangoItemRaw -> FinalizerPtr PangoItemRaw
pango_item_free _ = pango_item_free'

#else

foreign import ccall unsafe "pango_item_free"
  pango_item_free :: Ptr PangoItemRaw -> IO ()

#endif

-- | Extract the font used for this 'PangoItem'.
--
pangoItemGetFont :: PangoItem -> IO Font
pangoItemGetFont (PangoItem _ (PangoItemRaw pir)) =
  withForeignPtr pir pangoItemRawGetFont

-- | Extract the 'Language' used for this 'PangoItem'.
--
pangoItemGetLanguage :: PangoItem -> IO Language
pangoItemGetLanguage (PangoItem _ (PangoItemRaw pir)) =
  liftM (Language . castPtr) $ withForeignPtr pir pangoItemRawGetLanguage

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
  liftM LayoutIterRaw $ newForeignPtr liPtr (layout_iter_free liPtr)

#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "&pango_layout_iter_free"
  layout_iter_free' :: FinalizerPtr LayoutIterRaw

layout_iter_free :: Ptr LayoutIterRaw -> FinalizerPtr LayoutIterRaw
layout_iter_free _ = layout_iter_free'

#else

foreign import ccall unsafe "pango_layout_iter_free"
  layout_iter_free :: Ptr LayoutIterRaw -> IO ()

#endif

-- | A single line in a 'PangoLayout'.
--
data LayoutLine = LayoutLine (IORef PangoString) LayoutLineRaw

{#pointer *PangoLayoutLine as LayoutLineRaw foreign newtype #}

makeNewLayoutLineRaw :: Ptr LayoutLineRaw -> IO LayoutLineRaw
makeNewLayoutLineRaw llPtr = do
  liftM LayoutLineRaw $ newForeignPtr llPtr (pango_layout_line_unref llPtr)


#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "&pango_layout_line_unref"
  pango_layout_line_unref' :: FinalizerPtr LayoutLineRaw

pango_layout_line_unref :: Ptr LayoutLineRaw -> FinalizerPtr LayoutLineRaw
pango_layout_line_unref _ = pango_layout_line_unref'

#else

foreign import ccall unsafe "pango_layout_line_unref"
  pango_layout_line_unref :: Ptr LayoutLineRaw -> IO ()

#endif

foreign import ccall unsafe "pango_layout_line_ref"
  pango_layout_line_ref :: Ptr LayoutLineRaw -> IO ()

-- | A possibly partial description of font(s).
--
{#pointer *PangoFontDescription as FontDescription foreign newtype #}

makeNewFontDescription :: Ptr FontDescription -> IO FontDescription
makeNewFontDescription llPtr = do
  liftM FontDescription $ newForeignPtr llPtr
	    (pango_font_description_free llPtr)

#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "&pango_font_description_free"
  pango_font_description_free' :: FinalizerPtr FontDescription

pango_font_description_free :: Ptr FontDescription -> 
				FinalizerPtr FontDescription
pango_font_description_free _ = pango_font_description_free'

#else

foreign import ccall unsafe "pango_font_description_free"
  pango_font_description_free :: Ptr FontDescription -> IO ()

#endif

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


