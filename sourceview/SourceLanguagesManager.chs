-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry SourceLanguagesManager@
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
-- @description@ --------------------------------------------------------------
--
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
--
--
module SourceLanguagesManager (
  SourceLanguagesManager,
  sourceLanguagesManagerNew,
  sourceLanguagesManagerGetAvailableLanguages,
  sourceLanguagesManagerGetLanguageFromMimeType,
) where

import Monad	(liftM)
import FFI
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import SourceViewType#}
{#import Signal#}
import GList	(readGSList)

{# context lib="gtk" prefix="gtk" #}


-- methods

-- @constructor sourceLanguagesManagerNew@ Create a new @ref type SourceLanguagesManager@
--
sourceLanguagesManagerNew :: IO SourceLanguagesManager
sourceLanguagesManagerNew = makeNewGObject mkSourceLanguagesManager
  {#call source_languages_manager_new#} 

-- @method sourceLanguagesManagerGetAvailableLanguages@
-- 
sourceLanguagesManagerGetAvailableLanguages :: SourceLanguagesManager -> IO [SourceLanguage]
sourceLanguagesManagerGetAvailableLanguages lm = do
  gList <- {#call source_languages_manager_get_available_languages#} lm
  wList <- readGSList gList
  mapM (makeNewGObject mkSourceLanguage) (map return wList)

-- @method sourceLanguagesManagerGetLanguageFromMimeType@
-- 
sourceLanguagesManagerGetLanguageFromMimeType :: SourceLanguagesManager -> String -> IO (Maybe SourceLanguage)
sourceLanguagesManagerGetLanguageFromMimeType lm mimeType = do
  langPtr <- withCString mimeType $ \strPtr ->
             {#call source_languages_manager_get_language_from_mime_type#} lm strPtr
  if langPtr==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkSourceLanguage (return langPtr)

{-
-- @method sourceLanguagesManagerGetLangFilesDirs@
-- 
sourceLanguagesManagerGetLangFilesDirs :: SourceLanguagesManager -> IO [FilePath]
sourceLanguagesManagerGetLangFilesDirs lm = do
  gList <- {#call source_languages_manager_get_lang_files_dirs#} lm
  wList <- readGSList gList
  return the wList, converted to strings
-}
