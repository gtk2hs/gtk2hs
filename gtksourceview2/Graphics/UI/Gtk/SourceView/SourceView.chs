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
module Graphics.UI.Gtk.SourceView.SourceView (
  SourceView,
  SourceViewClass,
  SourceSmartHomeEndType(..),
  castToSourceView,
  sourceViewNew,
  sourceViewNewWithBuffer,
  sourceViewSetAutoIndent,
  sourceViewGetAutoIndent,
  sourceViewSetIndentOnTab,
  sourceViewGetIndentOnTab,
  sourceViewSetIndentWidth,
  sourceViewGetIndentWidth,
  sourceViewSetInsertSpacesInsteadOfTabs,
  sourceViewGetInsertSpacesInsteadOfTabs,
  sourceViewSetSmartHomeEnd,
  sourceViewGetSmartHomeEnd,
  sourceViewSetHighlightCurrentLine,
  sourceViewGetHighlightCurrentLine,
  sourceViewSetShowLineMarks,
  sourceViewGetShowLineMarks,
  sourceViewSetShowLineNumbers,
  sourceViewGetShowLineNumbers,
  sourceViewSetShowRightMargin,
  sourceViewGetShowRightMargin,
  sourceViewSetRightMarginPosition,
  sourceViewGetRightMarginPosition,
  sourceViewSetTabWidth,
  sourceViewGetTabWidth,
  sourceViewSetMarkCategoryPixbuf,
  sourceViewGetMarkCategoryPixbuf,
  sourceViewAutoIndent,
  sourceViewHighlightCurrentLine,
  sourceViewIndentOnTab,
  sourceViewIndentWidth,
  sourceViewInsertSpacesInsteadOfTabs,
  sourceViewRightMarginPosition,
  sourceViewShowLineNumbers,
  sourceViewShowRightMargin,
  sourceViewSmartHomeEnd,
  sourceViewTabWidth,
  sourceViewUndo,
  sourceViewRedo
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
{#import System.Glib.Properties#}
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import Graphics.UI.Gtk.SourceView.Signals#}

{# context lib="gtk" prefix="gtk" #}

{# enum SourceSmartHomeEndType {underscoreToCase} deriving (Eq, Bounded, Show, Read) #}

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
sourceViewSetIndentOnTab :: SourceViewClass sv => sv -> Bool -> IO ()
sourceViewSetIndentOnTab sv newVal =
  {#call source_view_set_indent_on_tab#} (toSourceView sv) (fromBool newVal)
  
-- | 
--
sourceViewGetIndentOnTab :: SourceViewClass sv => sv -> IO Bool 
sourceViewGetIndentOnTab sv = liftM toBool $
  {#call unsafe source_view_get_indent_on_tab#} (toSourceView sv)

-- | 
--
sourceViewSetIndentWidth :: SourceViewClass sv => sv -> Int -> IO ()
sourceViewSetIndentWidth sv newVal =
  {#call source_view_set_indent_width#} (toSourceView sv) (fromIntegral newVal)

-- | 
--
sourceViewGetIndentWidth :: SourceViewClass sv => sv -> IO Int
sourceViewGetIndentWidth sv = liftM fromIntegral $
  {#call unsafe source_view_get_indent_width#} (toSourceView sv)

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
sourceViewSetSmartHomeEnd :: SourceViewClass sv => sv -> SourceSmartHomeEndType -> IO ()
sourceViewSetSmartHomeEnd sv newVal =
  {#call source_view_set_smart_home_end#} (toSourceView sv) (fromIntegral $ fromEnum newVal)
  
-- | 
--
sourceViewGetSmartHomeEnd :: SourceViewClass sv => sv -> IO SourceSmartHomeEndType
sourceViewGetSmartHomeEnd sv = liftM (toEnum . fromIntegral) $
  {#call unsafe source_view_get_smart_home_end#} (toSourceView sv)

-- | 
--
sourceViewSetHighlightCurrentLine :: SourceViewClass sv => sv -> Bool -> IO ()
sourceViewSetHighlightCurrentLine sv newVal =
  {#call source_view_set_highlight_current_line#} (toSourceView sv) (fromBool newVal)
  
-- | 
--
sourceViewGetHighlightCurrentLine :: SourceViewClass sv => sv -> IO Bool 
sourceViewGetHighlightCurrentLine sv = liftM toBool $
  {#call unsafe source_view_get_highlight_current_line#} (toSourceView sv)

-- | 
--
sourceViewSetShowLineMarks :: SourceViewClass sv => sv -> Bool -> IO ()
sourceViewSetShowLineMarks sv newVal =
  {#call source_view_set_show_line_marks#} (toSourceView sv) (fromBool newVal)
  
-- | 
--
sourceViewGetShowLineMarks :: SourceViewClass sv => sv -> IO Bool 
sourceViewGetShowLineMarks sv = liftM toBool $
  {#call unsafe source_view_get_show_line_marks#} (toSourceView sv)

--- | 
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
sourceViewSetShowRightMargin :: SourceViewClass sv => sv -> Bool -> IO ()
sourceViewSetShowRightMargin sv newVal =
  {#call source_view_set_show_right_margin#} (toSourceView sv) (fromBool newVal)
  
-- | 
--
sourceViewGetShowRightMargin :: SourceViewClass sv => sv -> IO Bool
sourceViewGetShowRightMargin sv = liftM toBool $
  {#call source_view_get_show_right_margin#} (toSourceView sv)
  
-- | 
--
sourceViewSetRightMarginPosition :: SourceViewClass sv => sv -> Word -> IO ()
sourceViewSetRightMarginPosition sv margin =
  {#call source_view_set_right_margin_position#} (toSourceView sv) (fromIntegral margin)
  
-- | 
--
sourceViewGetRightMarginPosition :: SourceViewClass sv => sv -> IO Int 
sourceViewGetRightMarginPosition sv = liftM fromIntegral $
  {#call unsafe source_view_get_right_margin_position#} (toSourceView sv)

-- | 
--
sourceViewSetTabWidth :: SourceViewClass sv => sv -> Int -> IO ()
sourceViewSetTabWidth sv width =
  {#call source_view_set_tab_width#} (toSourceView sv) (fromIntegral width)
  
-- | 
--
sourceViewGetTabWidth :: SourceViewClass sv => sv -> IO Int 
sourceViewGetTabWidth sv = liftM fromIntegral $
  {#call unsafe source_view_get_tab_width#} (toSourceView sv)

-- |
--
sourceViewSetMarkCategoryPriority :: SourceViewClass sv => sv -> String -> Int -> IO ()
sourceViewSetMarkCategoryPriority sv markerType priority = withCString markerType $ \strPtr ->
  {#call source_view_set_mark_category_priority#} (toSourceView sv) strPtr (fromIntegral priority)

-- |
--
sourceViewGetMarkCategoryPriority :: SourceViewClass sv => sv -> String -> IO Int
sourceViewGetMarkCategoryPriority sv markerType = withCString markerType $ \strPtr ->
  liftM fromIntegral $
  {#call unsafe source_view_get_mark_category_priority#} (toSourceView sv) strPtr

--- |
--
sourceViewSetMarkCategoryPixbuf :: SourceViewClass sv => sv -> String -> Pixbuf -> IO ()
sourceViewSetMarkCategoryPixbuf sv markerType marker = withCString markerType $ \strPtr ->
  {#call source_view_set_mark_category_pixbuf#} (toSourceView sv) strPtr marker

-- |
--
sourceViewGetMarkCategoryPixbuf :: SourceViewClass sv => sv -> String -> IO Pixbuf
sourceViewGetMarkCategoryPixbuf sv markerType = withCString markerType $ \strPtr ->
  constructNewGObject mkPixbuf $
  {#call unsafe source_view_get_mark_category_pixbuf#} (toSourceView sv) strPtr

-- |
--
sourceViewAutoIndent :: SourceViewClass sv => Attr sv Bool
sourceViewAutoIndent = newAttrFromBoolProperty "auto-indent"

-- |
--
sourceViewHighlightCurrentLine :: SourceViewClass sv => Attr sv Bool
sourceViewHighlightCurrentLine = newAttrFromBoolProperty "highlight-current-line"

-- |
--
sourceViewIndentOnTab :: SourceViewClass sv => Attr sv Bool
sourceViewIndentOnTab = newAttrFromBoolProperty "indent-on-tab"

-- |
--
sourceViewIndentWidth :: SourceViewClass sv => Attr sv Int
sourceViewIndentWidth = newAttrFromIntProperty "indent-width"

-- |
--
sourceViewInsertSpacesInsteadOfTabs :: SourceViewClass sv => Attr sv Bool
sourceViewInsertSpacesInsteadOfTabs = newAttrFromBoolProperty "insert-spaces-instead-of-tabs"

-- |
--
sourceViewRightMarginPosition :: SourceViewClass sv => Attr sv Int
sourceViewRightMarginPosition = newAttrFromUIntProperty "right-margin-position"

-- |
--
sourceViewShowLineNumbers :: SourceViewClass sv => Attr sv Bool
sourceViewShowLineNumbers = newAttrFromBoolProperty "show-line-numbers"

-- |
--
sourceViewShowRightMargin :: SourceViewClass sv => Attr sv Bool
sourceViewShowRightMargin = newAttrFromBoolProperty "show-right-margin"

-- |
--
sourceViewSmartHomeEnd :: SourceViewClass sv => Attr sv SourceSmartHomeEndType
sourceViewSmartHomeEnd = newAttrFromEnumProperty "smart-home-end" {#call fun gtk_source_smart_home_end_type_get_type#}

-- |
--
sourceViewTabWidth :: SourceViewClass sv => Attr sv Int
sourceViewTabWidth = newAttrFromUIntProperty "tab-width"

-- |
--
sourceViewUndo :: SourceViewClass sv => Signal sv (IO ())
sourceViewUndo = Signal $ connect_NONE__NONE "undo"

-- |
--
sourceViewRedo :: SourceViewClass sv => Signal sv (IO ())
sourceViewRedo = Signal $ connect_NONE__NONE "redo"
