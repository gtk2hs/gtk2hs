{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceStyleScheme
--
--  Author : Peter Gavin
--  derived from sourceview bindings by Axel Simon and Duncan Coutts
--
--  Created: 18 December 2008
--
--  Copyright (C) 2004-2008 Peter Gavin, Duncan Coutts, Axel Simon
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
module Graphics.UI.Gtk.SourceView.SourceStyleScheme (
  SourceStyleScheme,
  castToSourceStyleScheme,
  sourceStyleSchemeGetId,
  sourceStyleSchemeGetName,
  sourceStyleSchemeGetDescription,
  sourceStyleSchemeGetAuthors,
  sourceStyleSchemeGetFilename,
  sourceStyleSchemeGetStyle,
  sourceStyleSchemeDescription,
  sourceStyleSchemeFilename,
  sourceStyleSchemeId,
  sourceStyleSchemeName,
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject	(constructNewGObject)
import System.Glib.Attributes
{#import System.Glib.Properties#}
{#import Graphics.UI.Gtk.SourceView.Types#}
import Graphics.UI.Gtk.SourceView.SourceStyle
{#import Graphics.UI.Gtk.SourceView.SourceStyle.Internal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | 
-- 
sourceStyleSchemeGetId :: SourceStyleScheme -> IO String
sourceStyleSchemeGetId ss =
  {#call source_style_scheme_get_id#} ss >>= peekUTFString

-- | 
-- 
sourceStyleSchemeGetName :: SourceStyleScheme -> IO String
sourceStyleSchemeGetName ss =
  {#call source_style_scheme_get_name#} ss >>= peekUTFString

-- | 
-- 
sourceStyleSchemeGetDescription :: SourceStyleScheme -> IO String
sourceStyleSchemeGetDescription ss =
  {#call source_style_scheme_get_description#} ss >>= peekUTFString

-- |
--
sourceStyleSchemeGetAuthors :: SourceStyleScheme -> IO [String]
sourceStyleSchemeGetAuthors ss =
  {#call source_style_scheme_get_authors#} ss >>= peekUTFStringArray0

-- | 
-- 
sourceStyleSchemeGetFilename :: SourceStyleScheme -> IO String
sourceStyleSchemeGetFilename ss =
  {#call source_style_scheme_get_filename#} ss >>= peekUTFString

-- | 
-- 
sourceStyleSchemeGetStyle :: SourceStyleScheme -> String -> IO SourceStyle
sourceStyleSchemeGetStyle ss id = do
  styleObj <- makeNewGObject mkSourceStyleObject $
              withUTFString id ({#call source_style_scheme_get_style#} ss)
  sourceStyleFromObject styleObj

-- |
--
sourceStyleSchemeDescription :: ReadAttr SourceStyleScheme String
sourceStyleSchemeDescription = readAttrFromStringProperty "description"

-- |
--
sourceStyleSchemeFilename :: ReadAttr SourceStyleScheme FilePath
sourceStyleSchemeFilename = readAttrFromStringProperty "filename"

-- |
--
sourceStyleSchemeId :: ReadAttr SourceStyleScheme String
sourceStyleSchemeId = readAttrFromStringProperty "id"

-- |
--
sourceStyleSchemeName :: ReadAttr SourceStyleScheme String
sourceStyleSchemeName = readAttrFromStringProperty "name"
