-- -*-haskell-*-
--  GIMP Toolkit (GTK) - text layout functions Context
--
--  Author : Axel Simon
--
--  Created: 16 October 2005
--
--  Version $Revision: 1.2 $ from $Date: 2005/10/17 22:52:50 $
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
-- 'Graphics.UI.Gtk.Pango.Rendering') or, at a more abstract level, using
-- layouts (see 'Graphics.UI.Gtk.Pango.Layout').
--
-- * A 'PangoContext' is a prerequisite for all text rendering functions.
--   A context can be created from scratch or, more conveniently, by using
--   default settings that are already used in the application. When text
--   is rendered through Gdk, use
--   'Graphics.UI.Gtk.Abstract.Widget.widgetCreatePangoContext', if you use
--   the Cairo rendering engine, a new context can be acquired using
--   'Graphics.UI.Gtk.Cairo.cairoCreateContext'.
--
-- * The properties of a 'PangoContext' can be changed which, in turn, has
--   an effect on how text is rendered. To reflect such a change in the
--   rendered text, call 'Graphics.UI.Gtk.Pango.Layout.layoutContextChanged'.
--
module Graphics.UI.Gtk.Pango.Context (
-- * Types and Methods for 'PangoContext's
  PangoContext,
  PangoDirection(..),
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
  contextGetTextDir
  ) where

import Monad    (liftM)
import Data.Ratio

import System.Glib.FFI
import Graphics.UI.Gtk.General.Structs  (pangoScale, PangoDirection(..))
{#import Graphics.UI.Gtk.Types#}
import System.Glib.GObject  (makeNewGObject)
import Graphics.UI.Gtk.General.Enums
{#import Graphics.UI.Gtk.Pango.Types#}
{#import Graphics.UI.Gtk.Pango.Enums#}
import System.Glib.UTFString ( withUTFString, UTFCorrection )
{#import System.Glib.GList#}
import Graphics.UI.Gtk.Pango.Font ( FontMetrics(..) )

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
#if PANGO_CHECK_VERSION(1,6,0)
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
#if PANGO_CHECK_VERSION(1,6,0)
	  (intToPu underline_position)
	  (intToPu underline_thickness)
	  (intToPu strikethrough_position)
	  (intToPu strikethrough_thickness)
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

