{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceMark
--
--  Author : Duncan Coutts
--  derived from GtkTextView bindings by Axel Simon
--
--  Created: 26 October 2003
--
--  Copyright (C) 2003-2005 Duncan Coutts, Axel Simon
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
module Graphics.UI.Gtk.SourceView.SourceMark (
  SourceMark,
  castToSourceMark,
  sourceMarkGetCategory,
  sourceMarkNext,
  sourceMarkPrev
) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject	(makeNewGObject)
{#import Graphics.UI.Gtk.SourceView.Types#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | 
-- 
sourceMarkGetCategory :: SourceMark -> IO String
sourceMarkGetCategory mark = do
  strPtr <- {#call unsafe source_mark_get_category#} mark
  markType <- peekUTFString strPtr
  {#call unsafe g_free#} (castPtr strPtr)
  return markType

-- | 
-- 
sourceMarkNext :: SourceMark -> String -> IO SourceMark
sourceMarkNext mark category = makeNewGObject mkSourceMark $
  withUTFString category $ {#call unsafe source_mark_next#} mark

-- | 
-- 
sourceMarkPrev :: SourceMark -> String -> IO SourceMark
sourceMarkPrev mark category = makeNewGObject mkSourceMark $
  withUTFString category $ {#call unsafe source_mark_prev#} mark

