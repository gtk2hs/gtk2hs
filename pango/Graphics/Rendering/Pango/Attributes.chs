{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) - pango text attributes
--
--  Author : Axel Simon
--
--  Created: 20 October 2005
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
module Graphics.Rendering.Pango.Attributes (
  withAttrList,
  parseMarkup,
  fromAttrList,
  readAttrList
  ) where

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GError
import System.Glib.GList
import Graphics.Rendering.Pango.Structs
{#import Graphics.Rendering.Pango.BasicTypes#}
import Data.List ( sortBy )
import Data.Char ( ord, chr )
import Control.Monad ( liftM )

{# context lib="pango" prefix="pango" #}

foreign import ccall unsafe "pango_attr_list_unref"
  pango_attr_list_unref :: PangoAttrList -> IO ()

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
  pango_attr_list_unref alPtr
  return res

-- Create a PangoAttribute.
crAttr :: UTFCorrection -> PangoAttribute -> IO CPangoAttribute
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
#if PANGO_VERSION_CHECK(1,8,0)
crAttr c AttrAbsSize { paStart=s, paEnd=e, paSize = pu } =
  setAttrPos c s e $ {#call unsafe attr_size_new_absolute#} (puToInt pu)
#endif
crAttr c AttrFontDescription { paStart=s, paEnd=e, paFontDescription = fd } =
  setAttrPos c s e $ {#call unsafe attr_font_desc_new#} fd
crAttr c AttrForeground { paStart=s, paEnd=e, paColor = Color r g b } =
  setAttrPos c s e $ {#call unsafe attr_foreground_new#}
  (fromIntegral r) (fromIntegral g) (fromIntegral b)
crAttr c AttrBackground { paStart=s, paEnd=e, paColor = Color r g b } =
  setAttrPos c s e $ {#call unsafe attr_background_new#}
  (fromIntegral r) (fromIntegral g) (fromIntegral b)
crAttr c AttrUnderline { paStart=s, paEnd=e, paUnderline = underline } =
  setAttrPos c s e $ do
  {#call unsafe attr_underline_new#} (fromIntegral (fromEnum underline))
#if  (defined (WIN32) && PANGO_VERSION_CHECK(1,10,0)) \
 || (!defined (WIN32) && PANGO_VERSION_CHECK(1,8,0))
crAttr c AttrUnderlineColor {paStart=s, paEnd=e, paColor = Color r g b } =
  setAttrPos c s e $ {#call unsafe attr_underline_color_new#}
  (fromIntegral r) (fromIntegral g) (fromIntegral b)
#endif
crAttr c AttrStrikethrough { paStart=s, paEnd=e, paStrikethrough = st } =
  setAttrPos c s e $ do
  {#call unsafe attr_strikethrough_new#} (fromIntegral (fromEnum st))
#if  (defined (WIN32) && PANGO_VERSION_CHECK(1,10,0)) \
 || (!defined (WIN32) && PANGO_VERSION_CHECK(1,8,0))
crAttr c AttrStrikethroughColor {paStart=s, paEnd=e, paColor = Color r g b } =
  setAttrPos c s e $ {#call unsafe attr_strikethrough_color_new#}
  (fromIntegral r) (fromIntegral g) (fromIntegral b)
#endif
crAttr c AttrRise { paStart=s, paEnd=e, paRise = pu } =
  setAttrPos c s e $ {#call unsafe attr_rise_new#} (puToInt pu)
#if PANGO_VERSION_CHECK(1,8,0)
crAttr c AttrShape { paStart=s, paEnd=e, paInk = rect1, paLogical = rect2 } =
  setAttrPos c s e $ alloca $ \rect1Ptr -> alloca $ \rect2Ptr -> do
    poke rect1Ptr rect1
    poke rect2Ptr rect2
    {#call unsafe attr_shape_new#} (castPtr rect1Ptr) (castPtr rect2Ptr)
#endif
crAttr c AttrScale { paStart=s, paEnd=e, paScale = scale } =
  setAttrPos c s e $
  {#call unsafe attr_scale_new#} (realToFrac scale)
#if PANGO_VERSION_CHECK(1,4,0)
crAttr c AttrFallback { paStart=s, paEnd=e, paFallback = fb } =
  setAttrPos c s e $
  {#call unsafe attr_fallback_new#} (fromBool fb)
#endif
#if PANGO_VERSION_CHECK(1,6,0)
crAttr c AttrLetterSpacing { paStart=s, paEnd=e, paLetterSpacing = pu } =
  setAttrPos c s e $
  {#call unsafe attr_letter_spacing_new#} (puToInt pu)
#endif
#if PANGO_VERSION_CHECK(1,16,0)
crAttr c AttrGravity { paStart=s, paEnd=e, paGravity = g } =
  setAttrPos c s e $
  {#call unsafe attr_gravity_new#} (fromIntegral (fromEnum g))
crAttr c AttrGravityHint { paStart=s, paEnd=e, paGravityHint = g } =
  setAttrPos c s e $
  {#call unsafe attr_gravity_hint_new#} (fromIntegral (fromEnum g))
#endif

-- | Parse the marked-up text (see 'Graphics.Rendering.Pango.Markup.Markup'
-- format) to create a plain-text string and an attribute list.
--
-- * The attribute list is a list of lists of attribute. Each list describes
--   the attributes for the same span.
--
-- * If @accelMarker@ is not @'\0'@ (a zero character), the given character
--   will mark the character following it as an accelerator. For example,
--   @accelMarker@ might be an ampersand or underscore. All characters marked
--   as an accelerator will receive a 'UnderlineLow' attribute, and the
--   first character so marked will be returned as @accelChar@. If no
--   accelerator character is found, the @accelMarker@ character itself is
--   returned. Two @accelMarker@ characters following each other produce a
--   single literal @accelMarker@ character.
--
-- * If a parsing error occurs a 'System.Glib.GError.GError' is thrown.
--
parseMarkup ::
     (GlibString markup, GlibString string)
  => markup -- ^ the string containing markup
  -> Char -- ^ @accelMarker@ - the character that prefixes an accelerator
  -> IO ([[PangoAttribute]], Char, string) -- ^ list of attributes, the accelerator character found and the input string
  -- without markup
parseMarkup markup accelMarker = propagateGError $ \errPtr ->
  withUTFStringLen markup $ \(markupPtr,markupLen) ->
  alloca $ \attrListPtr ->
  alloca $ \strPtrPtr ->
  alloca $ \accelPtr -> do
    poke accelPtr (fromIntegral (ord accelMarker))
    success <- {#call unsafe pango_parse_markup#} markupPtr
      (fromIntegral markupLen) (fromIntegral (ord accelMarker))
      (castPtr attrListPtr) strPtrPtr accelPtr errPtr
    if not (toBool success) then return undefined else do
      accel <- peek accelPtr
      strPtr <- peek strPtrPtr
      str <- peekUTFString strPtr
      {#call unsafe g_free#} (castPtr strPtr)
      attrList <- peek attrListPtr
      attrs <- fromAttrList (genUTFOfs str) attrList
      return (attrs, chr (fromIntegral accel), str)

{#pointer *PangoAttrIterator #}

-- | Convert an attribute list into a list of attributes.
fromAttrList :: UTFCorrection -> PangoAttrList -> IO [[PangoAttribute]]
fromAttrList correct attrListPtr = do
  iter <- {#call unsafe pango_attr_list_get_iterator#} attrListPtr
  let readIter = do
        list <- {#call unsafe pango_attr_iterator_get_attrs#} iter
        attrs <- if list==nullPtr then return [] else do
          attrPtrs <- fromGSList list
          mapM (fromAttr correct) attrPtrs
        more <- {#call unsafe pango_attr_iterator_next#} iter
        if toBool more then liftM ((:) attrs) $ readIter else return []
  elems <- readIter
  {#call unsafe pango_attr_iterator_destroy#} iter
  return elems

-- | Extract and delete an attribute.
--
fromAttr :: UTFCorrection -> CPangoAttribute -> IO PangoAttribute
fromAttr correct attrPtr = do
  attr <- readAttr correct attrPtr
  {#call unsafe pango_attribute_destroy#} attrPtr
  return attr

readAttrList :: UTFCorrection -> PangoAttrList -> IO [[PangoAttribute]]
readAttrList correct attrListPtr = do
  elems <- fromAttrList correct attrListPtr
  pango_attr_list_unref attrListPtr
  return elems
