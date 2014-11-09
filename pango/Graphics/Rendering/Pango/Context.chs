{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) - text layout functions Context
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
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- This module defines 'PangoContext's,
-- an environment that provides information on available fonts,
-- internationalization and output capabilities of the medium. Given
-- such a context, text can be rendered into strings of glyphs (see
-- 'Graphics.Rendering.Pango.Rendering') or, at a more abstract level, using
-- layouts (see 'Graphics.Rendering.Pango.Layout.Layout').
--
-- * A 'PangoContext' is a prerequisite for all text rendering functions.
--   A context can be created from scratch or, more conveniently, by using
--   default settings that are already used in the application. When text
--   is rendered through Gdk, use
--   'Graphics.UI.Gtk.Abstract.Widget.widgetCreatePangoContext', if you use
--   the Cairo rendering engine, a new context can be acquired using
--   'Graphics.Rendering.Pango.Cairo.cairoCreateContext'.
--
-- * The properties of a 'PangoContext' can be changed which, in turn, has
--   an effect on how text is rendered. To reflect such a change in the
--   rendered text, call 'Graphics.Rendering.Pango.Layout.layoutContextChanged'.
--
module Graphics.Rendering.Pango.Context (
-- * Types and Methods for 'PangoContext's
  PangoContext,
  PangoContextClass,
  contextListFamilies,
--  contextLoadFont,
--  contextLoadFontSet,
  contextGetMetrics,
  contextSetFontDescription,
  contextGetFontDescription,
  Language,
  emptyLanguage,
  languageFromString,
  contextSetLanguage,
  contextGetLanguage,
  contextSetTextDir,
  contextGetTextDir,
#if PANGO_VERSION_CHECK(1,16,0)
  contextSetTextGravity,
  contextGetTextGravity,
  contextSetTextGravityHint,
  contextGetTextGravityHint,
#endif
#if PANGO_VERSION_CHECK(1,6,0)
  contextGetMatrix,
  contextSetMatrix,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import Graphics.Rendering.Pango.Enums
import Graphics.Rendering.Pango.Structs
import System.Glib.GObject  (makeNewGObject)
{#import Graphics.Rendering.Pango.Types#}
{#import Graphics.Rendering.Pango.BasicTypes#}
{#import Graphics.Rendering.Pango.Enums#} ( FontMetrics(..) )
{#import Graphics.Rendering.Cairo.Matrix#}


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
  -- c2hs gets FontFamily*** wrong as FontFamily**, therefore the cast
  familyPtrs <- peekArray (fromIntegral size) (castPtr ptr)
  fams <- mapM (makeNewGObject mkFontFamily . return) familyPtrs
  {#call unsafe g_free#} (castPtr ptr)
  return fams

-- | Query the metrics of the given font implied by the font description.
--
contextGetMetrics :: PangoContext -> FontDescription -> Language ->
                     IO FontMetrics
contextGetMetrics pc fd l = do
  mPtr <- {#call unsafe context_get_metrics#} pc fd l
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
          (intToPu underline_thickness)
          (intToPu underline_position)
          (intToPu strikethrough_thickness)
          (intToPu strikethrough_position)
#endif
         )

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

#if PANGO_VERSION_CHECK(1,16,0)
-- | Set the text gravity of this context. If the given value is
-- 'PangoGravityAuto' then the gravity is derived from the current rotation
-- matrix.
--
contextSetTextGravity :: PangoContext -> PangoGravity -> IO ()
contextSetTextGravity pc gravity =
  {#call unsafe context_set_base_gravity#} pc (fromIntegral (fromEnum gravity))

-- | Get the current text gravity of this context.
--
contextGetTextGravity :: PangoContext -> IO PangoGravity
contextGetTextGravity pc = liftM (toEnum . fromIntegral) $
                       {#call unsafe context_get_base_gravity#} pc

-- | Set the text gravity hint of this context.
--
contextSetTextGravityHint :: PangoContext -> PangoGravityHint -> IO ()
contextSetTextGravityHint pc gravity =
  {#call unsafe context_set_gravity_hint#} pc (fromIntegral (fromEnum gravity))

-- | Get the current text gravity of this context.
--
contextGetTextGravityHint :: PangoContext -> IO PangoGravityHint
contextGetTextGravityHint pc = liftM (toEnum . fromIntegral) $
                       {#call unsafe context_get_gravity_hint#} pc
#endif

#if PANGO_VERSION_CHECK(1,6,0)
-- | Gets the transformation matrix that will be applied when rendering with
-- this context.
--
-- * Since Pango 1.6
contextGetMatrix :: PangoContext -> IO Matrix
contextGetMatrix pc = do
  matPtr <- {#call unsafe context_get_matrix#} pc
  if matPtr==nullPtr then return identity else peek (castPtr matPtr)

-- | Sets the transformation matrix that will be applied when rendering with
-- this context. Note that any metrics reported by other functions are in user
-- space coordinates before the application of the matrix, not device-space
-- coordinates after the application of the matrix. So, they don't scale with
-- the matrix, though they may change slightly for different matrices,
-- depending on how the text is fit to the pixel grid.
--
-- * Since Pango 1.6
contextSetMatrix :: PangoContext -> Matrix -> IO ()
contextSetMatrix pc mat
  | mat==identity = {#call unsafe context_set_matrix#} pc nullPtr
  | otherwise = with mat $ \matPtr ->
                {#call unsafe context_set_matrix#} pc (castPtr matPtr)
#endif
