{-# OPTIONS_HADDOCK hide #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) - text layout functions: Font Descriptions
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
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Functions to manage font descriptions.
--
-- * Font descriptions provide a way to query and state requirements on
--   fonts. This data structure has several fields describing different
--   characteristics of a font. Each of these fields can be set of left
--   unspecified.
--
module Graphics.Rendering.Pango.Description (
  FontDescription,
  fontDescriptionNew,
  fontDescriptionCopy,
  fontDescriptionSetFamily,
  fontDescriptionGetFamily,
  fontDescriptionSetStyle,
  fontDescriptionGetStyle,
  fontDescriptionSetVariant,
  fontDescriptionGetVariant,
  fontDescriptionSetWeight,
  fontDescriptionGetWeight,
  fontDescriptionSetStretch,
  fontDescriptionGetStretch,
  fontDescriptionSetSize,
  fontDescriptionGetSize,
  FontMask(..),
  fontDescriptionUnsetFields,
  fontDescriptionMerge,
  fontDescriptionBetterMatch,
  fontDescriptionFromString,
  fontDescriptionToString
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Flags                (Flags, fromFlags)
import System.Glib.UTFString
{#import Graphics.Rendering.Pango.Types#}
{#import Graphics.Rendering.Pango.Enums#}
import Graphics.Rendering.Pango.Structs ( puToInt, intToPu )
import Graphics.Rendering.Pango.BasicTypes

{# context lib="pango" prefix="pango_font_description" #}

-- | Create a new font description.
--
-- * All field are unset.
--
fontDescriptionNew :: IO FontDescription
fontDescriptionNew = {#call unsafe new#} >>= makeNewFontDescription

-- | Make a deep copy of a font description.
--
fontDescriptionCopy :: FontDescription -> IO FontDescription
fontDescriptionCopy fd = {#call unsafe copy#} fd >>= makeNewFontDescription

-- | Set the font famliy.
--
-- * A font family is a name designating the design of the font (e.g. Sans
--   or Times) without the variant.
--
-- * In some contexts a comma separated list of font families can be used.
--
fontDescriptionSetFamily :: GlibString string => FontDescription -> string -> IO ()
fontDescriptionSetFamily fd family = withUTFString family $ \strPtr ->
  {#call unsafe set_family#} fd strPtr

-- | Get the font family.
--
-- * 'Nothing' is returned if the font family is not set.
--
fontDescriptionGetFamily :: GlibString string => FontDescription -> IO (Maybe string)
fontDescriptionGetFamily fd = do
  strPtr <- {#call unsafe get_family#} fd
  if strPtr==nullPtr then return Nothing else
    liftM Just $ peekUTFString strPtr

-- | Flags denoting which fields in a font description are set.
{#enum PangoFontMask as FontMask {underscoreToCase} deriving(Bounded) #}

instance Flags FontMask

-- | Set the style field.
--
-- * Most fonts will have either a 'StyleItalic' or 'StyleOblique'
--   but rarely both.
--
fontDescriptionSetStyle :: FontDescription -> FontStyle -> IO ()
fontDescriptionSetStyle fd p =
    {#call unsafe set_style#} fd (fromIntegral (fromEnum p))

-- | Get the style field.
fontDescriptionGetStyle :: FontDescription -> IO (Maybe FontStyle)
fontDescriptionGetStyle fd = do
  fields <- {#call unsafe get_set_fields#} fd
  if (fromEnum PangoFontMaskStyle) .&. (fromIntegral fields) /=0
     then liftM (Just . toEnum . fromIntegral) $
              {#call unsafe get_style#} fd
     else return Nothing

-- | Set the variant field.
--
fontDescriptionSetVariant :: FontDescription -> Variant -> IO ()
fontDescriptionSetVariant fd p =
    {#call unsafe set_variant#} fd (fromIntegral (fromEnum p))

-- | Get the variant field.
fontDescriptionGetVariant :: FontDescription -> IO (Maybe Variant)
fontDescriptionGetVariant fd = do
  fields <- {#call unsafe get_set_fields#} fd
  if (fromEnum PangoFontMaskVariant) .&. (fromIntegral fields) /=0
     then liftM (Just . toEnum . fromIntegral) $
              {#call unsafe get_variant#} fd
     else return Nothing

-- | Set the weight field.
--
fontDescriptionSetWeight :: FontDescription -> Weight -> IO ()
fontDescriptionSetWeight fd p =
  {#call unsafe set_weight#} fd (fromIntegral (fromEnum p))

-- | Get the weight field.
fontDescriptionGetWeight :: FontDescription -> IO (Maybe Weight)
fontDescriptionGetWeight fd = do
  fields <- {#call unsafe get_set_fields#} fd
  if (fromEnum PangoFontMaskWeight) .&. (fromIntegral fields) /=0
     then liftM (Just . toEnum . fromIntegral) $
              {#call unsafe get_weight#} fd
     else return Nothing

-- | Set the stretch field.
--
fontDescriptionSetStretch :: FontDescription -> Stretch -> IO ()
fontDescriptionSetStretch fd p =
  {#call unsafe set_stretch#} fd (fromIntegral (fromEnum p))

-- | Get the stretch field.
fontDescriptionGetStretch :: FontDescription -> IO (Maybe Stretch)
fontDescriptionGetStretch fd = do
  fields <- {#call unsafe get_set_fields#} fd
  if (fromEnum PangoFontMaskStretch) .&. (fromIntegral fields) /=0
     then liftM (Just . toEnum . fromIntegral) $
              {#call unsafe get_stretch#} fd
     else return Nothing

-- | Set the size field.
--
-- * The given size is in points (pts). One point is 1\/72 inch.
--
fontDescriptionSetSize :: FontDescription -> Double -> IO ()
fontDescriptionSetSize fd p =
  {#call unsafe set_size#} fd (puToInt p)

-- | Get the size field.
fontDescriptionGetSize :: FontDescription -> IO (Maybe Double)
fontDescriptionGetSize fd = do
  fields <- {#call unsafe get_set_fields#} fd
  if (fromEnum PangoFontMaskSize) .&. (fromIntegral fields) /=0
     then liftM (\x -> Just (intToPu x)) $
              {#call unsafe get_size#} fd
     else return Nothing

-- | Reset fields in a font description.
--
fontDescriptionUnsetFields :: FontDescription -> [FontMask] -> IO ()
fontDescriptionUnsetFields fd mask =
  {#call unsafe unset_fields#} fd (fromIntegral (fromFlags mask))

-- | Merge two font descriptions.
--
-- * Copy fields from the second description to the first. If the boolean
--   parameter is set, existing fields in the first description will be
--   replaced.
--
fontDescriptionMerge :: FontDescription -> FontDescription -> Bool -> IO ()
fontDescriptionMerge fd1 fd2 replace =
  {#call unsafe merge#} fd1 fd2 (fromBool replace)

-- | Determine if two descriptions are simliar.
--
-- * Returns 'True' if the two descriptions only differ in weight or style.
--
fontDescriptionIsMatch :: FontDescription -> FontDescription -> Bool
fontDescriptionIsMatch fdA fdB = unsafePerformIO $ liftM toBool $
  {#call unsafe better_match#} fdA (FontDescription nullForeignPtr) fdB

-- | Determine which of two descriptions matches a given description better.
--
-- * Returns @True@ if the last description is a better match to the first
--   arguement than the middle one.
--
-- * Approximate matching is done on weight and style. If the other
--   attributes do not match, the function returns @False@.
--
fontDescriptionBetterMatch :: FontDescription -> FontDescription ->
                              FontDescription -> Bool
fontDescriptionBetterMatch fd fdA fdB = unsafePerformIO $ liftM toBool $
  {#call unsafe better_match#} fd fdA fdB

-- | Create a font description from a string.
--
-- * The given argument must have the form
--   @[FAMILY-LIST] [STYLE-OPTIONS] [SIZE]@ where @FAMILY_LIST@ is a comma
--   separated list of font families optionally terminated by a comma,
--   @STYLE_OPTIONS@ is a whitespace separated list of words where each
--   word describes one of style, variant, weight or stretch. @SIZE@ is
--   a decimal number giving the size of the font in points. If any of
--   these fields is absent, the resulting 'FontDescription' will have
--   the corresponing fields unset.
--
fontDescriptionFromString :: GlibString string => string -> IO FontDescription
fontDescriptionFromString descr = withUTFString descr $ \strPtr ->
  {#call unsafe from_string#} strPtr >>= makeNewFontDescription

-- | Convert a font description to a string.
--
-- * Creates a string representation of a font description. See
--   'fontDescriptionFromString' for the format of the string.
--
fontDescriptionToString :: GlibString string => FontDescription -> IO string
fontDescriptionToString fd = do
  strPtr <- {#call unsafe to_string#} fd
  str <- peekUTFString strPtr
  {#call unsafe g_free#} (castPtr strPtr)
  return str



