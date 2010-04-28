{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceBuffer
--
--  Author : Peter Gavin
--  derived from sourceview bindings by Axel Simon and Duncan Coutts
--
--  Created: 18 December 2008
--
--  Copyright (C) 2003-2008 Peter Gavin, Duncan Coutts, Axel Simon
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
module Graphics.UI.Gtk.SourceView.SourceBuffer (
  SourceBuffer,
  SourceBufferClass,
  castToSourceBuffer,
  sourceBufferNew,
  sourceBufferNewWithLanguage,
  sourceBufferSetHighlightSyntax,
  sourceBufferGetHighlightSyntax,
  sourceBufferSetLanguage,
  sourceBufferGetLanguage,
  sourceBufferSetHighlightMatchingBrackets,
  sourceBufferGetHighlightMatchingBrackets,
  sourceBufferSetStyleScheme,
  sourceBufferGetStyleScheme,
  sourceBufferSetMaxUndoLevels,
  sourceBufferGetMaxUndoLevels,
  sourceBufferGetCanUndo,
  sourceBufferGetCanRedo,
  sourceBufferUndo,
  sourceBufferRedo,
  sourceBufferBeginNotUndoableAction,
  sourceBufferEndNotUndoableAction,
  sourceBufferCreateSourceMark,
  sourceBufferEnsureHighlight,
  sourceBufferCanRedo,
  sourceBufferCanUndo,
  sourceBufferHighlightMatchingBrackets,
  sourceBufferHighlightSyntax,
  sourceBufferLanguage,
  sourceBufferSourceStyleScheme,
  sourceBufferHighlightUpdated
  ) where

import Control.Monad	(liftM)
import Data.Maybe    (fromMaybe)

import System.Glib.FFI
import System.Glib.GObject              (constructNewGObject,
					 makeNewGObject)
{#import System.Glib.Properties#}
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import Graphics.UI.Gtk.SourceView.Signals#}
import Graphics.UI.GtkInternals
{#import Graphics.UI.Gtk.Multiline.TextIter#}

{#import Graphics.UI.Gtk.SourceView.SourceMark#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new 'SourceBuffer', possibly
-- taking a 'TextTagTable'.
--
sourceBufferNew :: Maybe TextTagTable -> IO SourceBuffer
sourceBufferNew tt = constructNewGObject mkSourceBuffer $
  {#call unsafe source_buffer_new#} 
  (fromMaybe (TextTagTable nullForeignPtr) tt)

-- | Create a new 'SourceBuffer'
-- with a 'SourceLanguage'.
--
sourceBufferNewWithLanguage :: SourceLanguage -> IO SourceBuffer
sourceBufferNewWithLanguage lang = constructNewGObject mkSourceBuffer $
  {#call unsafe source_buffer_new_with_language#} lang

-- | 
--
sourceBufferSetHighlightSyntax :: SourceBuffer -> Bool -> IO ()
sourceBufferSetHighlightSyntax sb newVal =
  {#call unsafe source_buffer_set_highlight_syntax#} sb (fromBool newVal)
  
-- | 
--
sourceBufferGetHighlightSyntax :: SourceBuffer -> IO Bool 
sourceBufferGetHighlightSyntax sb = liftM toBool $
  {#call unsafe source_buffer_get_highlight_syntax#} sb

-- | 
--
sourceBufferSetLanguage :: SourceBuffer -> SourceLanguage -> IO ()
sourceBufferSetLanguage sb lang =
  {#call unsafe source_buffer_set_language#} sb lang
  
-- | 
--
sourceBufferGetLanguage :: SourceBuffer -> IO SourceLanguage
sourceBufferGetLanguage sb = makeNewGObject mkSourceLanguage $
  {#call unsafe source_buffer_get_language#} sb

-- | 
--
sourceBufferSetHighlightMatchingBrackets :: SourceBuffer -> Bool -> IO ()
sourceBufferSetHighlightMatchingBrackets sb newVal =
  {#call unsafe source_buffer_set_highlight_matching_brackets#} sb (fromBool newVal)
  
-- | 
--
sourceBufferGetHighlightMatchingBrackets :: SourceBuffer -> IO Bool 
sourceBufferGetHighlightMatchingBrackets sb = liftM toBool $
  {#call unsafe source_buffer_get_highlight_matching_brackets#} sb

-- |
-- 
sourceBufferSetStyleScheme :: SourceBuffer -> SourceStyleScheme -> IO ()
sourceBufferSetStyleScheme sb sss =
    {#call unsafe source_buffer_set_style_scheme#} sb sss

-- |
-- 
sourceBufferGetStyleScheme :: SourceBuffer -> IO SourceStyleScheme
sourceBufferGetStyleScheme sb = makeNewGObject mkSourceStyleScheme $
  {#call unsafe source_buffer_get_style_scheme#} sb

-- | 
--
sourceBufferSetMaxUndoLevels :: SourceBuffer -> Int -> IO ()
sourceBufferSetMaxUndoLevels sb newVal =
  {#call unsafe source_buffer_set_max_undo_levels#} sb (fromIntegral newVal)
  
-- | 
--
sourceBufferGetMaxUndoLevels :: SourceBuffer -> IO Int
sourceBufferGetMaxUndoLevels sb = liftM fromIntegral $
  {#call unsafe source_buffer_get_max_undo_levels#} sb

-- | 
--
sourceBufferGetCanUndo :: SourceBuffer -> IO Bool
sourceBufferGetCanUndo sb = liftM toBool $
  {#call unsafe source_buffer_can_undo#} sb
  
-- | 
--
sourceBufferGetCanRedo :: SourceBuffer -> IO Bool
sourceBufferGetCanRedo sb = liftM toBool $
  {#call unsafe source_buffer_can_redo#} sb

-- | 
--
sourceBufferUndo :: SourceBuffer -> IO ()
sourceBufferUndo sb =
  {#call source_buffer_undo#} sb
  
-- | 
--
sourceBufferRedo :: SourceBuffer -> IO ()
sourceBufferRedo sb =
  {#call source_buffer_redo#} sb

-- | 
--
sourceBufferBeginNotUndoableAction :: SourceBuffer -> IO ()
sourceBufferBeginNotUndoableAction sb =
  {#call source_buffer_begin_not_undoable_action#} sb
  
-- | 
--
sourceBufferEndNotUndoableAction :: SourceBuffer -> IO ()
sourceBufferEndNotUndoableAction sb =
  {#call source_buffer_end_not_undoable_action#} sb

-- | Creates a marker in the buffer of the given type.
--
-- *  A marker is
--    semantically very similar to a 'Graphics.UI.Gtk.Multiline.TextMark',
--    except it has a type
--    which is used by the 'SourceView' displaying the buffer to show a
--    pixmap on the left margin, at the line the marker is in.  Because
--    of this, a marker is generally associated to a line and not a
--    character position.  Markers are also accessible through a position
--    or range in the buffer.
--
-- *  Markers are implemented using 'Graphics.UI.Gtk.Multiline.TextMark',
--    so all characteristics
--    and restrictions to marks apply to markers too.  These includes
--    life cycle issues and 'Graphics.UI.Gtk.Multiline.TextMark.onMarkSet'
--    and 'Graphics.UI.Gtk.Multiline.TextMark.onMarkDeleted' signal
--    emissions.
--
-- *  Like a 'Graphics.UI.Gtk.Multiline.TextMark', a 'SourceMarker'
--    can be anonymous if the
--    passed name is @Nothing@.  Also, the buffer owns the markers so you
--    shouldn't unreference it.

sourceBufferCreateSourceMark :: SourceBuffer -- the buffer
                         -> Maybe String -- the name of the mark
                         -> String -- the category of the mark
                         -> TextIter -> IO SourceMark
sourceBufferCreateSourceMark sb name category iter =
  makeNewGObject mkSourceMark $
  maybeWith withCString name       $ \strPtr1 ->
  withCString category $ \strPtr2 ->
  {#call source_buffer_create_source_mark#} sb strPtr1 strPtr2 iter

-- |
-- 
sourceBufferEnsureHighlight :: SourceBuffer -> TextIter -> TextIter -> IO ()
sourceBufferEnsureHighlight sb start end =
    {#call source_buffer_ensure_highlight#} sb start end

-- |
-- 
sourceBufferCanRedo :: ReadAttr SourceBuffer Bool
sourceBufferCanRedo = readAttrFromBoolProperty "can-redo"

-- |
-- 
sourceBufferCanUndo :: ReadAttr SourceBuffer Bool
sourceBufferCanUndo = readAttrFromBoolProperty "can-undo"

-- |
--
sourceBufferHighlightMatchingBrackets :: Attr SourceBuffer Bool
sourceBufferHighlightMatchingBrackets = newAttrFromBoolProperty "highlight-matching-brackets"

-- |
--
sourceBufferHighlightSyntax :: Attr SourceBuffer Bool
sourceBufferHighlightSyntax = newAttrFromBoolProperty "highlight-matching-brackets"

-- |
-- 
sourceBufferLanguage :: Attr SourceBuffer (Maybe SourceLanguage)
sourceBufferLanguage = newAttrFromMaybeObjectProperty "language" gTypeSourceLanguage

-- |
--
sourceBufferMaxUndoLevels :: Attr SourceBuffer Int
sourceBufferMaxUndoLevels = newAttrFromIntProperty "max-undo-levels"

-- |
-- 
sourceBufferSourceStyleScheme :: Attr SourceBuffer (Maybe SourceStyleScheme)
sourceBufferSourceStyleScheme = newAttrFromMaybeObjectProperty "style-scheme" gTypeSourceStyleScheme

-- |
--
sourceBufferHighlightUpdated :: Signal SourceBuffer (TextIter -> TextIter -> IO ())
sourceBufferHighlightUpdated = Signal $ connect_BOXED_BOXED__NONE "highlight-updated" mkTextIterCopy mkTextIterCopy
