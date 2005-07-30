-- -*-haskell-*-
--  GIMP Toolkit (GTK) - pango non-GObject types PangoTypes
--
--  Author : Axel Simon
--
--  Created: 9 Feburary 2003
--
--  Version $Revision: 1.5 $ from $Date: 2005/07/30 17:32:05 $
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
  LayoutIter(LayoutIter),
  layout_iter_free,
  LayoutLine(LayoutLine),
  makeNewLayoutLine,
  FontDescription(FontDescription),
  makeNewFontDescription,
  Language(Language),
  emptyLanguage,
  languageFromString
  ) where

import Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString

{# context lib="pango" prefix="pango" #}

-- entry PangoLayout

-- | An iterator to examine a layout.
--
{#pointer *PangoLayoutIter as LayoutIter foreign newtype #}


#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "&pango_layout_iter_free"
  layout_iter_free' :: FinalizerPtr LayoutIter

layout_iter_free :: Ptr LayoutIter -> FinalizerPtr LayoutIter
layout_iter_free _ = layout_iter_free'

#else

foreign import ccall unsafe "pango_layout_iter_free"
  layout_iter_free :: Ptr LayoutIter -> IO ()

#endif

-- | A single line in a 'PangoLayout'.
--
{#pointer *PangoLayoutLine as LayoutLine foreign newtype #}

makeNewLayoutLine :: Ptr LayoutLine -> IO LayoutLine
makeNewLayoutLine llPtr = do
  liftM LayoutLine $ newForeignPtr llPtr (pango_layout_line_unref llPtr)


#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "&pango_layout_line_unref"
  pango_layout_line_unref' :: FinalizerPtr LayoutLine

pango_layout_line_unref :: Ptr LayoutLine -> FinalizerPtr LayoutLine
pango_layout_line_unref _ = pango_layout_line_unref'

#else

foreign import ccall unsafe "pango_layout_line_unref"
  pango_layout_line_unref :: Ptr LayoutLine -> IO ()

#endif

foreign import ccall unsafe "pango_layout_line_ref"
  pango_layout_line_ref :: Ptr LayoutLine -> IO ()

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

-- | A Language designator to choose fonts.
--
{#pointer* Language newtype#} deriving Eq

instance Show Language where
  show (Language ptr)
    | ptr==nullPtr = ""
    | otherwise = unsafePerformIO $ peekUTFString (castPtr ptr)

-- | Specifying no particular language.
emptyLanguage = Language nullPtr

languageFromString :: String -> IO Language
languageFromString language = liftM Language $
  withUTFString language {#call language_from_string#}
