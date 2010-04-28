{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SourceView
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
module Graphics.UI.Gtk.SourceView.SourceLanguage (
  SourceLanguage,
  SourceLanguageClass,
  castToSourceLanguage,
  sourceLanguageGetId,
  sourceLanguageGetName,
  sourceLanguageGetSection,
  sourceLanguageGetHidden,
  sourceLanguageGetMetadata,
  sourceLanguageGetMimeTypes,
  sourceLanguageGetGlobs,
  sourceLanguageHidden,
  sourceLanguageId,
  sourceLanguageName,
  sourceLanguageSection
  ) where

import Control.Monad	(liftM)
import Data.Maybe (fromMaybe)

import System.Glib.FFI
import System.Glib.UTFString
{#import System.Glib.Properties#}
import System.Glib.Attributes
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import Graphics.UI.Gtk.SourceView.Signals#}
{#import Graphics.UI.Gtk.SourceView.SourceStyleScheme#}

{# context lib="gtk" prefix="gtk" #}


-- methods

-- | 
--
sourceLanguageGetId :: SourceLanguage -> IO String
sourceLanguageGetId sl =
  {#call unsafe source_language_get_id#} sl >>= peekUTFString

-- | 
--
sourceLanguageGetName :: SourceLanguage -> IO String
sourceLanguageGetName sl =
  {#call unsafe source_language_get_name#} sl >>= peekUTFString

-- | 
--
sourceLanguageGetSection :: SourceLanguage -> IO String
sourceLanguageGetSection sl =
  {#call unsafe source_language_get_section#} sl >>= peekUTFString

-- |
--
sourceLanguageGetHidden :: SourceLanguage -> IO Bool
sourceLanguageGetHidden sl = liftM toBool $
  {#call unsafe source_language_get_hidden#} sl

-- |
--
sourceLanguageGetMetadata :: SourceLanguage -> String -> IO String
sourceLanguageGetMetadata sl name = do
  withUTFString name ({#call unsafe source_language_get_metadata#} sl) >>= peekUTFString

-- |
--
sourceLanguageGetMimeTypes :: SourceLanguage -> IO [String]
sourceLanguageGetMimeTypes sl = do
  mimeTypesArray <- {#call unsafe source_language_get_mime_types#} sl
  mimeTypes <- liftM (fromMaybe []) $ maybePeek peekUTFStringArray0 mimeTypesArray
  {# call g_strfreev #} mimeTypesArray
  return mimeTypes

-- |
--
sourceLanguageGetGlobs :: SourceLanguage -> IO [String]
sourceLanguageGetGlobs sl = do
  globsArray <- {#call unsafe source_language_get_globs#} sl
  globs <- liftM (fromMaybe []) $ maybePeek peekUTFStringArray0 globsArray
  {# call g_strfreev #} globsArray
  return globs

-- |
--
sourceLanguageHidden :: ReadAttr SourceLanguage Bool
sourceLanguageHidden = readAttrFromBoolProperty "hidden"

-- |
--
sourceLanguageId :: ReadAttr SourceLanguage String
sourceLanguageId = readAttrFromStringProperty "id"

-- |
--
sourceLanguageName :: ReadAttr SourceLanguage String
sourceLanguageName = readAttrFromStringProperty "name"

-- |
--
sourceLanguageSection :: ReadAttr SourceLanguage String
sourceLanguageSection = readAttrFromStringProperty "section"
