{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  Pango - a library for typesetting Unicode text
--
--  Author : Axel Simon
--
--  Copyright (C) 2001-2010 Axel Simon
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
-- Pango is a library for laying out and rendering of text, with an emphasis
-- on internationalization. Pango can be used anywhere that text layout is
-- needed, though most of the work on Pango so far has been done in the
-- context of the GTK+ widget toolkit. Pango forms the core of text and font
-- handling for GTK+-2.x.
--
-- Pango is designed to be modular; the core Pango layout engine can be used
-- with different font backends. There are three basic backends:
--
-- * Client side fonts using the FreeType and fontconfig libraries.
--
-- * Native fonts on Microsoft Windows using Uniscribe for complex-text handling.
--
-- * Native fonts on MacOS X using ATSUI for complex-text handling.
--
-- The integration of Pango
-- with Cairo <http://cairographics.org/> provides a complete solution with
-- high quality text handling and graphics rendering.
--
-- Dynamically loaded modules then handle text layout for particular
-- combinations of script and font backend. Pango ships with a wide selection
-- of modules, including modules for Hebrew, Arabic, Hangul, Thai, and a
-- number of Indic scripts. Virtually all of the world's major scripts are
-- supported.
--
-- As well as the low level layout rendering routines, Pango includes
-- 'PangoLayout', a high level driver for laying out entire blocks of text, and
-- routines to assist in editing internationalized text.
--
-- Pango depends on 2.x series of the GLib library.
--
-- This module only re-exports the parts of the Pango library that are relevant for
-- text rendering (as opposed to integration with other libraries).
--
module Graphics.Rendering.Pango (
  module Graphics.Rendering.Pango.Context,
  module Graphics.Rendering.Pango.Layout,
  module Graphics.Rendering.Pango.Rendering,
  module Graphics.Rendering.Pango.Markup,
  module Graphics.Rendering.Pango.Font,
  module Graphics.Rendering.Pango.Enums,
  module Graphics.Rendering.Pango.Cairo,
  ) where

import Graphics.Rendering.Pango.Font
import Graphics.Rendering.Pango.Enums hiding (
  Language,
  emptyLanguage,
  languageFromString,
  )
import Graphics.Rendering.Pango.Context
import Graphics.Rendering.Pango.Layout hiding (
  PangoRectangle(..),
#if PANGO_VERSION_CHECK(1,6,0)
  EllipsizeMode(..),
#endif
  )
import Graphics.Rendering.Pango.Rendering
import Graphics.Rendering.Pango.Markup
import Graphics.Rendering.Pango.Cairo
