-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SourceView
--
--  Author : Duncan Coutts
--  derived from GtkTextView bindings by Axel Simon
--
--  Created: 14 October 2003
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
module Graphics.UI.Gtk.SourceView.SourceView (
  SourceView,
  SourceViewClass,
  castToSourceView,
  sourceViewNew,
  sourceViewNewWithBuffer,
  sourceViewSetShowLineNumbers,
  sourceViewGetShowLineNumbers,
  sourceViewSetShowLineMarkers,
  sourceViewGetShowLineMarkers,
  sourceViewSetTabsWidth,
  sourceViewGetTabsWidth,
  sourceViewSetAutoIndent,
  sourceViewGetAutoIndent,
  sourceViewSetInsertSpacesInsteadOfTabs,
  sourceViewGetInsertSpacesInsteadOfTabs,
  sourceViewSetShowMargin,
  sourceViewGetShowMargin,
  sourceViewSetMargin,
  sourceViewGetMargin,
  sourceViewSetMarkerPixbuf,
  sourceViewGetMarkerPixbuf,
  sourceViewSetSmartHomeEnd,
  sourceViewGetSmartHomeEnd,
) where

import Control.Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import System.Glib.GObject		(constructNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.SourceView.SourceBuffer

{# context lib="gtk" prefix="gtk" #}


-- methods

-- | Create a new 'SourceView' widget with a default 'SourceBuffer'.
--
sourceViewNew :: IO SourceView
sourceViewNew = makeNewObject mkSourceView $ liftM castPtr 
  {#call unsafe source_view_new#}

-- | Create a new 'SourceView'
-- widget with the given 'SourceBuffer'.
--
sourceViewNewWithBuffer :: SourceBuffer -> IO SourceView
sourceViewNewWithBuffer sb = makeNewObject mkSourceView $ liftM castPtr $
  {#call source_view_new_with_buffer#} sb

-- | 
--
sourceViewSetShowLineNumbers :: SourceViewClass sv => sv -> Bool -> IO ()
sourceViewSetShowLineNumbers sv newVal =
  {#call source_view_set_show_line_numbers#} (toSourceView sv) (fromBool newVal)
  
-- | 
--
sourceViewGetShowLineNumbers :: SourceViewClass sv => sv -> IO Bool 
sourceViewGetShowLineNumbers sv = liftM toBool $
  {#call unsafe source_view_get_show_line_numbers#} (toSourceView sv)

-- | 
--
sourceViewSetShowLineMarkers :: SourceViewClass sv => sv -> Bool -> IO ()
sourceViewSetShowLineMarkers sv newVal =
  {#call source_view_set_show_line_markers#} (toSourceView sv) (fromBool newVal)
  
-- | 
--
sourceViewGetShowLineMarkers :: SourceViewClass sv => sv -> IO Bool 
sourceViewGetShowLineMarkers sv = liftM toBool $
  {#call unsafe source_view_get_show_line_markers#} (toSourceView sv)

-- | 
--
sourceViewSetTabsWidth :: SourceViewClass sv => sv -> Int -> IO ()
sourceViewSetTabsWidth sv width =
  {#call source_view_set_tabs_width#} (toSourceView sv) (fromIntegral width)
  
-- | 
--
sourceViewGetTabsWidth :: SourceViewClass sv => sv -> IO Int 
sourceViewGetTabsWidth sv = liftM fromIntegral $
  {#call unsafe source_view_get_tabs_width#} (toSourceView sv)

-- | 
--
sourceViewSetAutoIndent :: SourceViewClass sv => sv -> Bool -> IO ()
sourceViewSetAutoIndent sv newVal =
  {#call source_view_set_auto_indent#} (toSourceView sv) (fromBool newVal)
  
-- | 
--
sourceViewGetAutoIndent :: SourceViewClass sv => sv -> IO Bool 
sourceViewGetAutoIndent sv = liftM toBool $
  {#call unsafe source_view_get_auto_indent#} (toSourceView sv)

-- | 
--
sourceViewSetInsertSpacesInsteadOfTabs :: SourceViewClass sv => sv -> Bool -> IO ()
sourceViewSetInsertSpacesInsteadOfTabs sv newVal =
  {#call source_view_set_insert_spaces_instead_of_tabs#} (toSourceView sv) (fromBool newVal)
  
-- | 
--
sourceViewGetInsertSpacesInsteadOfTabs :: SourceViewClass sv => sv -> IO Bool 
sourceViewGetInsertSpacesInsteadOfTabs sv = liftM toBool $
  {#call unsafe source_view_get_insert_spaces_instead_of_tabs#} (toSourceView sv)

-- | 
--
sourceViewSetShowMargin :: SourceViewClass sv => sv -> Bool -> IO ()
sourceViewSetShowMargin sv newVal =
  {#call source_view_set_show_margin#} (toSourceView sv) (fromBool newVal)
  
-- | 
--
sourceViewGetShowMargin :: SourceViewClass sv => sv -> IO Bool 
sourceViewGetShowMargin sv = liftM toBool $
  {#call unsafe source_view_get_show_margin#} (toSourceView sv)

-- | 
--
sourceViewSetMargin :: SourceViewClass sv => sv -> Int -> IO ()
sourceViewSetMargin sv margin =
  {#call source_view_set_margin#} (toSourceView sv) (fromIntegral margin)
  
-- | 
--
sourceViewGetMargin :: SourceViewClass sv => sv -> IO Int 
sourceViewGetMargin sv = liftM fromIntegral $
  {#call unsafe source_view_get_margin#} (toSourceView sv)

-- | 
--
sourceViewSetMarkerPixbuf :: SourceViewClass sv => sv -> String -> Pixbuf -> IO ()
sourceViewSetMarkerPixbuf sv markerType marker = withCString markerType $ \strPtr ->
  {#call source_view_set_marker_pixbuf#} (toSourceView sv) strPtr marker
  
-- | 
--
sourceViewGetMarkerPixbuf :: SourceViewClass sv => sv -> String -> IO Pixbuf 
sourceViewGetMarkerPixbuf sv markerType = withCString markerType $ \strPtr ->
  constructNewGObject mkPixbuf $
  {#call unsafe source_view_get_marker_pixbuf#} (toSourceView sv) strPtr

-- | 
--
sourceViewSetSmartHomeEnd :: SourceViewClass sv => sv -> Bool -> IO ()
sourceViewSetSmartHomeEnd sv newVal =
  {#call source_view_set_smart_home_end#} (toSourceView sv) (fromBool newVal)
  
-- | 
--
sourceViewGetSmartHomeEnd :: SourceViewClass sv => sv -> IO Bool 
sourceViewGetSmartHomeEnd sv = liftM toBool $
  {#call unsafe source_view_get_smart_home_end#} (toSourceView sv)
