-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget SourceView@
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
module SourceView (
  SourceView,
  SourceViewClass,
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
  module SourceBuffer,
  module SourceLanguage,
  module SourceLanguagesManager,
  module SourceTag,
  module SourceTagTable,
  module SourceTagStyle,
  module SourceStyleScheme
) where

import Monad	(liftM)
import FFI
import Object	(makeNewObject)
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import SourceViewType#}
{#import Signal#}
import SourceBuffer
import SourceLanguage
import SourceLanguagesManager
import SourceStyleScheme
import SourceTag
import SourceTagTable
import SourceTagStyle

{# context lib="gtk" prefix="gtk" #}


-- methods

-- @constructor sourceViewNew@ Create a new @ref type SourceView@ widget with a
-- default @ref type SourceBuffer@.
--
sourceViewNew :: IO SourceView
sourceViewNew = makeNewGObject mkSourceView $ liftM castPtr 
  {#call unsafe source_view_new#}

-- @constructor sourceViewNewWithBuffer@ Create a new @ref type SourceView@
-- widget with the given @ref type SourceBuffer@.
--
sourceViewNewWithBuffer :: SourceBuffer -> IO SourceView
sourceViewNewWithBuffer sb = makeNewGObject mkSourceView $ liftM castPtr $
  {#call unsafe source_view_new_with_buffer#} sb

-- @method sourceViewSetShowLineNumbers@
--
sourceViewSetShowLineNumbers :: SourceViewClass sv => sv -> Bool -> IO ()
sourceViewSetShowLineNumbers sv newVal =
  {#call unsafe source_view_set_show_line_numbers#} (toSourceView sv) (fromBool newVal)
  
-- @method sourceViewGetShowLineNumbers@
--
sourceViewGetShowLineNumbers :: SourceViewClass sv => sv -> IO Bool 
sourceViewGetShowLineNumbers sv = liftM toBool $
  {#call unsafe source_view_get_show_line_numbers#} (toSourceView sv)

-- @method sourceViewSetShowLineMarkers@
--
sourceViewSetShowLineMarkers :: SourceViewClass sv => sv -> Bool -> IO ()
sourceViewSetShowLineMarkers sv newVal =
  {#call unsafe source_view_set_show_line_markers#} (toSourceView sv) (fromBool newVal)
  
-- @method sourceViewGetShowLineMarkers@
--
sourceViewGetShowLineMarkers :: SourceViewClass sv => sv -> IO Bool 
sourceViewGetShowLineMarkers sv = liftM toBool $
  {#call unsafe source_view_get_show_line_markers#} (toSourceView sv)

-- @method sourceViewSetTabsWidth@
--
sourceViewSetTabsWidth :: SourceViewClass sv => sv -> Int -> IO ()
sourceViewSetTabsWidth sv width =
  {#call unsafe source_view_set_tabs_width#} (toSourceView sv) (fromIntegral width)
  
-- @method sourceViewGetTabsWidth@
--
sourceViewGetTabsWidth :: SourceViewClass sv => sv -> IO Int 
sourceViewGetTabsWidth sv = liftM fromIntegral $
  {#call unsafe source_view_get_tabs_width#} (toSourceView sv)

-- @method sourceViewSetAutoIndent@
--
sourceViewSetAutoIndent :: SourceViewClass sv => sv -> Bool -> IO ()
sourceViewSetAutoIndent sv newVal =
  {#call unsafe source_view_set_auto_indent#} (toSourceView sv) (fromBool newVal)
  
-- @method sourceViewGetAutoIndent@
--
sourceViewGetAutoIndent :: SourceViewClass sv => sv -> IO Bool 
sourceViewGetAutoIndent sv = liftM toBool $
  {#call unsafe source_view_get_auto_indent#} (toSourceView sv)

-- @method sourceViewSetInsertSpacesInsteadOfTabs@
--
sourceViewSetInsertSpacesInsteadOfTabs :: SourceViewClass sv => sv -> Bool -> IO ()
sourceViewSetInsertSpacesInsteadOfTabs sv newVal =
  {#call unsafe source_view_set_insert_spaces_instead_of_tabs#} (toSourceView sv) (fromBool newVal)
  
-- @method sourceViewGetInsertSpacesInsteadOfTabs@
--
sourceViewGetInsertSpacesInsteadOfTabs :: SourceViewClass sv => sv -> IO Bool 
sourceViewGetInsertSpacesInsteadOfTabs sv = liftM toBool $
  {#call unsafe source_view_get_insert_spaces_instead_of_tabs#} (toSourceView sv)

-- @method sourceViewSetShowMargin@
--
sourceViewSetShowMargin :: SourceViewClass sv => sv -> Bool -> IO ()
sourceViewSetShowMargin sv newVal =
  {#call unsafe source_view_set_show_margin#} (toSourceView sv) (fromBool newVal)
  
-- @method sourceViewGetShowMargin@
--
sourceViewGetShowMargin :: SourceViewClass sv => sv -> IO Bool 
sourceViewGetShowMargin sv = liftM toBool $
  {#call unsafe source_view_get_show_margin#} (toSourceView sv)

-- @method sourceViewSetMargin@
--
sourceViewSetMargin :: SourceViewClass sv => sv -> Int -> IO ()
sourceViewSetMargin sv margin =
  {#call unsafe source_view_set_margin#} (toSourceView sv) (fromIntegral margin)
  
-- @method sourceViewGetMargin@
--
sourceViewGetMargin :: SourceViewClass sv => sv -> IO Int 
sourceViewGetMargin sv = liftM fromIntegral $
  {#call unsafe source_view_get_margin#} (toSourceView sv)

-- @method sourceViewSetMarkerPixbuf@
--
sourceViewSetMarkerPixbuf :: SourceViewClass sv => sv -> String -> Pixbuf -> IO ()
sourceViewSetMarkerPixbuf sv markerType marker = withCString markerType $ \strPtr ->
  {#call unsafe source_view_set_marker_pixbuf#} (toSourceView sv) strPtr marker
  
-- @method sourceViewGetMarkerPixbuf@
--
sourceViewGetMarkerPixbuf :: SourceViewClass sv => sv -> String -> IO Pixbuf 
sourceViewGetMarkerPixbuf sv markerType = withCString markerType $ \strPtr ->
  makeNewGObject mkPixbuf $
  {#call unsafe source_view_get_marker_pixbuf#} (toSourceView sv) strPtr

-- @method sourceViewSetSmartHomeEnd@
--
sourceViewSetSmartHomeEnd :: SourceViewClass sv => sv -> Bool -> IO ()
sourceViewSetSmartHomeEnd sv newVal =
  {#call unsafe source_view_set_smart_home_end#} (toSourceView sv) (fromBool newVal)
  
-- @method sourceViewGetSmartHomeEnd@
--
sourceViewGetSmartHomeEnd :: SourceViewClass sv => sv -> IO Bool 
sourceViewGetSmartHomeEnd sv = liftM toBool $
  {#call unsafe source_view_get_smart_home_end#} (toSourceView sv)
