-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceMarker
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
module Graphics.UI.Gtk.SourceView.SourceMarker (
  SourceMarker,
  castToSourceMarker,
  sourceMarkerSetMarkerType,
  sourceMarkerGetMarkerType,
  sourceMarkerGetLine,
  sourceMarkerGetName,
  sourceMarkerGetBuffer,
  sourceMarkerNext,
  sourceMarkerPrev
) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject	(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.SourceView.Types#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | 
-- 
sourceMarkerSetMarkerType :: SourceMarker -> String -> IO ()
sourceMarkerSetMarkerType mark markType =
  withCString markType $ \strPtr1 ->
  {#call unsafe source_marker_set_marker_type#} mark strPtr1

-- | 
-- 
sourceMarkerGetMarkerType :: SourceMarker -> IO String
sourceMarkerGetMarkerType mark = do
  strPtr <- {#call unsafe source_marker_get_marker_type#} mark
  markType <- peekUTFString strPtr
  {#call unsafe g_free#} (castPtr strPtr)
  return markType

-- | 
-- 
sourceMarkerGetLine :: SourceMarker -> IO Int
sourceMarkerGetLine mark = liftM fromIntegral $
  {#call unsafe source_marker_get_line#} mark

-- | 
-- 
sourceMarkerGetName :: SourceMarker -> IO String
sourceMarkerGetName mark =
  {#call unsafe source_marker_get_name#} mark >>= peekUTFString

-- | 
-- 
sourceMarkerGetBuffer :: SourceMarker -> IO SourceBuffer
sourceMarkerGetBuffer mark = makeNewGObject mkSourceBuffer $
  {#call unsafe source_marker_get_buffer#} mark

-- | 
-- 
sourceMarkerNext :: SourceMarker -> IO SourceMarker
sourceMarkerNext mark = makeNewGObject mkSourceMarker $
  {#call unsafe source_marker_next#} mark

-- | 
-- 
sourceMarkerPrev :: SourceMarker -> IO SourceMarker
sourceMarkerPrev mark = makeNewGObject mkSourceMarker $
  {#call unsafe source_marker_prev#} mark
