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
module Graphics.UI.Gtk.SourceView.SourceLanguageManager (
  SourceLanguageManager,
  SourceLanguageManagerClass,
  castToSourceLanguageManager,
  sourceLanguageManagerNew,
  sourceLanguageManagerGetDefault,
  sourceLanguageManagerSetSearchPath,
  sourceLanguageManagerGetSearchPath,
  sourceLanguageManagerGetLanguageIds,
  sourceLanguageManagerGetLanguage,
  ) where

import Control.Monad	(liftM)
import Data.Maybe (fromMaybe)

import System.Glib.FFI
import System.Glib.GObject (constructNewGObject, makeNewGObject)
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

{# enum SourceSmartHomeEndType {underscoreToCase} deriving (Eq, Bounded, Show, Read) #}

-- |
--
sourceLanguageManagerNew :: IO SourceLanguageManager
sourceLanguageManagerNew = constructNewGObject mkSourceLanguageManager $ liftM castPtr
  {#call unsafe source_language_manager_new#}

-- |
--
sourceLanguageManagerGetDefault :: IO SourceLanguageManager
sourceLanguageManagerGetDefault = makeNewGObject mkSourceLanguageManager $ liftM castPtr
  {#call unsafe source_language_manager_new#}

-- |
--
sourceLanguageManagerSetSearchPath :: SourceLanguageManager -> Maybe [String] -> IO ()
sourceLanguageManagerSetSearchPath slm dirs =
  maybeWith withUTFStringArray0 dirs $ \dirsPtr -> do
    {#call unsafe source_language_manager_set_search_path#} slm dirsPtr

-- |
--
sourceLanguageManagerGetSearchPath :: SourceLanguageManager -> IO [String]
sourceLanguageManagerGetSearchPath slm = do
  dirsPtr <- {#call unsafe source_language_manager_get_search_path#} slm
  peekUTFStringArray0 dirsPtr

-- |
--
sourceLanguageManagerGetLanguageIds :: SourceLanguageManager -> IO [String]
sourceLanguageManagerGetLanguageIds slm = do
  idsPtr <- {#call unsafe source_language_manager_get_language_ids#} slm
  liftM (fromMaybe []) $ maybePeek peekUTFStringArray0 idsPtr

-- |
--
sourceLanguageManagerGetLanguage :: SourceLanguageManager -> String -> IO SourceLanguage
sourceLanguageManagerGetLanguage slm id = makeNewGObject mkSourceLanguage $ liftM castPtr $
  withUTFString id ({#call unsafe source_language_manager_get_language#} slm)

#if GTKSOURCEVIEW2_CHECK_VERSION(2,4,0)
-- |
--
sourceLanguageManagerGuessLanguage slm filename contentType = makeNewGObject mkSourceLanguage $ liftM castPtr $
  withUTFString filename $ \cFilename ->
  withUTFString contentType $ \cContentType ->
  {#call unsafe source_language_manager_guess_language#} slm cFilename cContentType
#endif

-- |
--
sourceLanguageManagerLanguageIds :: ReadAttr SourceLanguageManager [String]
sourceLanguageManagerLanguageIds =
  readAttrFromBoxedOpaqueProperty (liftM (fromMaybe []) . maybePeek peekUTFStringArray0 . castPtr)
                                  "language-ids" {#call pure g_strv_get_type#}

-- |
--
sourceLanguageManagerSearchPath :: ReadWriteAttr SourceLanguageManager [String] (Maybe [String])
sourceLanguageManagerSearchPath =
  newAttr (objectGetPropertyBoxedOpaque (peekUTFStringArray0 . castPtr) gtype "search-path")
          (objectSetPropertyBoxedOpaque (\dirs f -> maybeWith withUTFStringArray0 dirs (f . castPtr)) gtype "search-path")
  where gtype = {#call pure g_strv_get_type#}
