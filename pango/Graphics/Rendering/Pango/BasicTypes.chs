{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
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
module Graphics.Rendering.Pango.BasicTypes (
  GInt,

  Language(Language),
  emptyLanguage,
  languageFromString,

  FontStyle(..),
  Weight(..),
  Variant(..),
  Stretch(..),
  Underline(..),
#if PANGO_VERSION_CHECK(1,16,0)
  PangoGravity(..),
  PangoGravityHint(..),
#endif
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

  PangoAttrList,
  CPangoAttribute,
  ) where

import Control.Monad (liftM)
import Data.IORef ( IORef )
import qualified Data.Text as T (unpack)
import System.Glib.FFI
import System.Glib.UTFString
{#import Graphics.Rendering.Pango.Types#} (Font, PangoLayoutRaw)
-- {#import Graphics.Rendering.Pango.Enums#}

{# context lib="pango" prefix="pango" #}

-- | An RFC-3066 language designator to choose scripts.
--
{#pointer* Language newtype#} deriving Eq

-- | Define the gint that c2hs is the Haskell type.
type GInt = {#type gint#}

instance Show Language where
  show (Language ptr)
    | ptr==nullPtr = ""
    | otherwise = T.unpack . unsafePerformIO $ peekUTFString (castPtr ptr)

-- | Specifying no particular language.
emptyLanguage :: Language
emptyLanguage = Language nullPtr

-- | Take a RFC-3066 format language tag as a string and convert it to a
--  'Language' type that can be efficiently passed around and compared with
--  other language tags.
--
-- * This function first canonicalizes the string by converting it to
--   lowercase, mapping \'_\' to \'-\', and stripping all characters
--   other than letters and \'-\'.
--
languageFromString :: GlibString string => string -> IO Language
languageFromString language = liftM Language $
  withUTFString language {#call language_from_string#}

-- | The style of a font.
--
-- * 'StyleOblique' is a slanted font like 'StyleItalic',
--   but in a roman style.
--
{#enum Style as FontStyle {underscoreToCase} deriving (Eq)#}

instance Show FontStyle where
  showsPrec _ StyleNormal          = shows "normal"
  showsPrec _ StyleOblique         = shows "oblique"
  showsPrec _ StyleItalic          = shows "italic"

-- | Define attributes for 'Weight'.
--
{#enum Weight {underscoreToCase} deriving (Eq)#}

instance Show Weight where
  showsPrec _ WeightUltralight  = shows "ultralight"
  showsPrec _ WeightLight       = shows "light"
  showsPrec _ WeightNormal      = shows "normal"
  showsPrec _ WeightSemibold    = shows "semibold"
  showsPrec _ WeightBold        = shows "bold"
  showsPrec _ WeightUltrabold   = shows "ultrabold"
  showsPrec _ WeightHeavy       = shows "heavy"
#if PANGO_VERSION_CHECK(1,24,0)
  showsPrec _ WeightThin        = shows "thin"
  showsPrec _ WeightBook        = shows "book"
  showsPrec _ WeightMedium      = shows "medium"
  showsPrec _ WeightUltraheavy  = shows "ultraheavy"
#endif

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
  showsPrec _ StretchUltraCondensed     = shows "ultracondensed"
  showsPrec _ StretchExtraCondensed     = shows "extracondensed"
  showsPrec _ StretchCondensed          = shows "condensed"
  showsPrec _ StretchSemiCondensed      = shows "semicondensed"
  showsPrec _ StretchNormal             = shows "normal"
  showsPrec _ StretchSemiExpanded       = shows "semiexpanded"
  showsPrec _ StretchExpanded           = shows "expanded"
  showsPrec _ StretchExtraExpanded      = shows "extraexpanded"
  showsPrec _ StretchUltraExpanded      = shows "ultraexpanded"

-- | Define attributes for 'Underline'.
--
-- * The squiggly underline for errors is only available in Gtk 2.4 and higher.
--
{#enum Underline {underscoreToCase} deriving (Eq)#}

instance Show Underline where
  showsPrec _ UnderlineNone     = shows "none"
  showsPrec _ UnderlineSingle   = shows "single"
  showsPrec _ UnderlineDouble   = shows "double"
  showsPrec _ UnderlineLow      = shows "low"
  showsPrec _ UnderlineError    = shows "error"

#if PANGO_VERSION_CHECK(1,16,0)
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
  show PangoGravityHintStrong = "strong"
  show PangoGravityHintLine = "line"

#endif

-- A string that is stored with each GlyphString, PangoItem
data PangoString = PangoString UTFCorrection CInt (ForeignPtr CChar)

makeNewPangoString :: GlibString string => string -> IO PangoString
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

#if PANGO_VERSION_CHECK(1,2,0)
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

-- Attributes
{#pointer *PangoAttrList #}

{#pointer *PangoAttribute as CPangoAttribute#}

-- dirty hack to make PangoAttribute showable
instance Show FontDescription where
  show fd = unsafePerformIO $ do
    strPtr <- {#call unsafe font_description_to_string#} fd
    str <- peekUTFString strPtr
    {#call unsafe g_free#} (castPtr strPtr)
    return $ T.unpack str


