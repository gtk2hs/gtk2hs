{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) - text layout functions: Font
--
--  Author : Axel Simon
--
--  Created: 16 October 2005
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
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Fonts. The selection of an appropriate font to render text becomes a
-- substantial task in the presence of Unicode where a single font does not
-- cover the whole range of possible characters. Pango provides several
-- concepts to find appropriate fonts and to query information about them:
--
-- * 'FontDescription': Font descriptions provide a way to query and state
--   requirements on
--   fonts. This data structure has several fields describing different
--   characteristics of a font. Each of these fields can be set of left
--   unspecified.
--
-- * 'FontMap' : A font map represents the set of fonts available for a
--   particular rendering system. In particular this map defines the
--   relation between font size and pixel size in terms of the output medium.
--
-- * 'FontFamily' : A font family represents a set of fonts that have
--   related faces, that is, their faces share a common design, but differ
--   in slant, weight, width and other aspects.
--
-- * 'FontFace': A face is a specific font where all characteristics are
--   fixed except for the size.
--
-- * 'Font': A font in the underlying rendering system.
--
-- * 'FontMetrics': Information about the font that will be used to render
--   a specific 'Context' or 'PangoItem'.
--
module Graphics.Rendering.Pango.Font (
  -- Functions to manage font descriptions.
  module Graphics.Rendering.Pango.Description,
  -- Font metrics.
  FontMap,
  FontMapClass,
  pangoFontMapListFamilies,
  FontFamily,
  FontFamilyClass,
#if PANGO_VERSION_CHECK(1,4,0)
  pangoFontFamilyIsMonospace,
#endif
  pangoFontFamilyListFaces,
  FontFace,
  FontFaceClass,
#if PANGO_VERSION_CHECK(1,4,0)
  pangoFontFaceListSizes,
#endif
  pangoFontFaceDescribe,
  Font,
  FontClass,
  ) where

import Control.Monad    (liftM)
import qualified Data.Text as T (unpack)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject              (makeNewGObject)
{#import Graphics.Rendering.Pango.BasicTypes#}
{#import Graphics.Rendering.Pango.Types#}
{#import Graphics.Rendering.Pango.Enums#} (FontMetrics)
import Graphics.Rendering.Pango.Description
import Graphics.Rendering.Pango.Structs

{# context lib="pango" prefix="pango" #}


-- | Ask for the different font families that a particular back-end supports.
--
-- * The 'FontMap' can be acquired by calling
--   'Graphics.Rendering.Pango.Cairo.cairoFontMapGetDefault'.
--
pangoFontMapListFamilies :: FontMap -> IO [FontFamily]
pangoFontMapListFamilies fm = alloca $ \arrPtrPtr -> alloca $ \sizePtr -> do
  {#call unsafe font_map_list_families#} fm arrPtrPtr sizePtr
  arrPtr <- peek arrPtrPtr
  size <- peek sizePtr
  ffsPtr <- peekArray (fromIntegral size)
            (castPtr arrPtr::Ptr (Ptr FontFamily)) -- c2hs is wrong here
  {#call unsafe g_free#} (castPtr arrPtr)
  mapM (makeNewGObject mkFontFamily . return . castPtr) ffsPtr

instance Show FontFamily where
  show ff = T.unpack . unsafePerformIO $ do
    strPtr <- {#call unsafe font_family_get_name#} ff
    peekUTFString strPtr

#if PANGO_VERSION_CHECK(1,4,0)
-- | Ask if the given family contains monospace fonts.
--
-- * A monospace font is a font designed for text display where the
--   characters form a regular grid. For Western languages this would
--   mean that the advance width of all characters are the same, but
--   this categorization also includes Asian fonts which include
--   double-width characters: characters that occupy two grid cells.
--
-- * The best way to find out the grid-cell size is to query the members
--   of the according 'FontMetrics' structure.
--
pangoFontFamilyIsMonospace :: FontFamily -> Bool
pangoFontFamilyIsMonospace ff = unsafePerformIO $
  liftM toBool $ {#call unsafe font_family_is_monospace#} ff
#endif

-- | Ask for the faces contained in a particular family.
--
-- * Asks for all font faces in the given family. The faces in a family
--   share a common design, but differ in slant, weight, width and other
--   aspects. For example, the font family "Sans" contains several fonts
--   such as Helvetica and Arial.
--
pangoFontFamilyListFaces :: FontFamily -> IO [FontFace]
pangoFontFamilyListFaces ff = alloca $ \arrPtrPtr -> alloca $ \sizePtr -> do
  {#call unsafe font_family_list_faces#} ff arrPtrPtr sizePtr
  arrPtr <- peek arrPtrPtr
  size <- peek sizePtr
  ffsPtr <- peekArray (fromIntegral size)
            (castPtr arrPtr::Ptr (Ptr FontFace)) -- c2hs is wrong here
  {#call unsafe g_free#} (castPtr arrPtr)
  mapM (makeNewGObject mkFontFace . return . castPtr) ffsPtr

instance Show FontFace where
  show ff = T.unpack . unsafePerformIO $ do
    strPtr <- {#call unsafe font_face_get_face_name#} ff
    peekUTFString strPtr

#if PANGO_VERSION_CHECK(1,4,0)
-- | Ask for available sizes of this font face.
--
-- * List the available sizes for a font. This is only applicable to bitmap
--   fonts since all other fonts can be scaled arbitrarily. For scalable
--   fonts, this function returns @Nothing@. The sizes returned are in
--   ascending order, their unit is points (1\/72 inch).
--
pangoFontFaceListSizes :: FontFace -> IO (Maybe [Double])
pangoFontFaceListSizes ff = alloca $ \arrPtrPtr -> alloca $ \sizePtr -> do
  {#call unsafe font_face_list_sizes#} ff arrPtrPtr sizePtr
  arrPtr <- peek arrPtrPtr
  size <- peek sizePtr
  if arrPtr==nullPtr then return Nothing else do
    sizes <- peekArray (fromIntegral size) arrPtr
    {#call unsafe g_free#} (castPtr arrPtr)
    return (Just (map intToPu sizes))
#endif

-- | Ask for a description of this face.
--
-- * Returns the family, style, variant, weight and stretch of a 'FontFace'.
--   The size field of the resulting font description will be unset.
--
pangoFontFaceDescribe :: FontFace -> IO FontDescription
pangoFontFaceDescribe ff = do
  fdPtr <- {#call unsafe font_face_describe#} ff
  makeNewFontDescription fdPtr
