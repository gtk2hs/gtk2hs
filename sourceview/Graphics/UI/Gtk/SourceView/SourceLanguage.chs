-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SourceView
--
--  Author : Duncan Coutts
--  derived from GtkTextView bindings by Axel Simon
--          
--  Created: 14 October 2003
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- |
--
module Graphics.UI.Gtk.SourceView.SourceLanguage (
  SourceLanguage,
  sourceLanguageGetName,
  sourceLanguageGetSection,
  sourceLanguageGetTags,
  sourceLanguageGetEscapeChar,
  sourceLanguageGetMimeTypes,
  sourceLanguageSetMimeTypes,
  sourceLanguageGetStyleScheme,
  sourceLanguageSetStyleScheme,
  sourceLanguageGetTagStyle,
  sourceLanguageSetTagStyle,
  sourceLanguageGetTagDefaultStyle
) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList		(fromGSList, toGSList)
import System.Glib.GObject              (makeNewGObject)
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.SourceView.SourceStyleScheme
import Graphics.UI.Gtk.SourceView.SourceTagStyle

{# context lib="gtk" prefix="gtk" #}


-- methods

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
sourceLanguageGetTags :: SourceLanguage -> IO [SourceTag]
sourceLanguageGetTags sl = do
  gList <- {#call unsafe source_language_get_tags#} sl
  wList <- fromGSList gList
  mapM (makeNewGObject mkSourceTag) (map return wList)

-- | 
--
sourceLanguageGetEscapeChar :: SourceLanguage -> IO Char
sourceLanguageGetEscapeChar sl = liftM (toEnum . fromEnum) $
  {#call unsafe source_language_get_escape_char#} sl

sourceLanguageGetMimeTypes :: SourceLanguage -> IO [String]
sourceLanguageGetMimeTypes sl = do
  mimeTypesList <- {#call unsafe source_language_get_mime_types#} sl
  mimeTypesPtrs <- fromGSList mimeTypesList
  mapM peekUTFString mimeTypesPtrs

sourceLanguageSetMimeTypes :: SourceLanguage -> [String] -> IO ()
sourceLanguageSetMimeTypes sl mimeTypes = do
  mimeTypesPtrs <- mapM newUTFString mimeTypes
  mimeTypesList <- toGSList mimeTypesPtrs
  {#call unsafe source_language_set_mime_types#} sl mimeTypesList
  {#call unsafe g_slist_free#} mimeTypesList

-- | 
--
sourceLanguageGetStyleScheme :: SourceLanguage -> IO SourceStyleScheme
sourceLanguageGetStyleScheme sl = makeNewGObject mkSourceStyleScheme $
  {#call unsafe source_language_get_style_scheme#} sl

-- | 
--
sourceLanguageSetStyleScheme :: SourceLanguage -> SourceStyleScheme -> IO ()
sourceLanguageSetStyleScheme sl ss =
  {#call unsafe source_language_set_style_scheme#} sl ss

-- | 
--
sourceLanguageGetTagStyle :: SourceLanguage -> String -> IO SourceTagStyle
sourceLanguageGetTagStyle sl tag =
  withCString  tag  $ \strPtr1 -> do
  sts <- {#call unsafe source_language_get_tag_style#} sl strPtr1
  peek (castPtr sts)

-- | 
--
sourceLanguageSetTagStyle :: SourceLanguage -> String -> SourceTagStyle -> IO ()
sourceLanguageSetTagStyle sl tag sts =  
  withCString  tag  $ \strPtr1 ->
  alloca            $ \sts' -> do
  poke sts' sts
  {#call unsafe source_language_set_tag_style#} sl strPtr1 (castPtr sts')

-- | 
--
sourceLanguageGetTagDefaultStyle :: SourceLanguage -> String -> IO SourceTagStyle
sourceLanguageGetTagDefaultStyle sl tag =
  withCString  tag  $ \strPtr1 -> do
  sts <- {#call unsafe source_language_get_tag_default_style#} sl strPtr1
  peek (castPtr sts)
