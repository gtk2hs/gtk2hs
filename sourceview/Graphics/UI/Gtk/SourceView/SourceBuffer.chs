-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceBuffer
--
--  Author : Duncan Coutts
--  derived from GtkTextView bindings by Axel Simon
--
--  Created: 15 October 2003
--
--  Version $Revision: 1.4 $ from $Date: 2005/11/26 16:00:22 $
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
module Graphics.UI.Gtk.SourceView.SourceBuffer (
  SourceBuffer,
  SourceBufferClass,
  castToSourceBuffer,
  sourceBufferNew,
  sourceBufferNewWithLanguage,
  sourceBufferSetCheckBrackets,
  sourceBufferGetCheckBrackets,
  sourceBufferSetBracketsMatchStyle,
  sourceBufferSetHighlight,
  sourceBufferGetHighlight,
  sourceBufferSetMaxUndoLevels,
  sourceBufferGetMaxUndoLevels,
  sourceBufferSetLanguage,
  sourceBufferGetLanguage,
  sourceBufferSetEscapeChar,
  sourceBufferGetEscapeChar,
  sourceBufferCanUndo,
  sourceBufferCanRedo,
  sourceBufferUndo,
  sourceBufferRedo,
  sourceBufferBeginNotUndoableAction,
  sourceBufferEndNotUndoableAction,
  sourceBufferCreateMarker,
  sourceBufferMoveMarker,
  sourceBufferDeleteMarker,
  sourceBufferGetMarker,
  sourceBufferGetMarkersInRegion,
  sourceBufferGetFirstMarker,
  sourceBufferGetLastMarker,
  sourceBufferGetIterAtMarker,
  sourceBufferGetNextMarker,
  sourceBufferGetPrevMarker
) where

import Monad	(liftM)
import Maybe    (fromMaybe)

import System.Glib.FFI
import System.Glib.GList		(fromGSList)
import System.Glib.GObject              (constructNewGObject,
					 makeNewGObject)
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.SourceView.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.SourceView.SourceTagStyle
import Graphics.UI.Gtk.SourceView.SourceMarker
{#import Graphics.UI.Gtk.Multiline.Types#}
{#import Graphics.UI.Gtk.Multiline.TextIter#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new 'SourceBuffer', possibly
-- taking a 'SourceTagTable'.
--
sourceBufferNew :: Maybe SourceTagTable -> IO SourceBuffer
sourceBufferNew tt = constructNewGObject mkSourceBuffer $
  {#call unsafe source_buffer_new#} 
  (fromMaybe (mkSourceTagTable nullForeignPtr) tt)

-- | Create a new 'SourceBuffer'
-- with a 'SourceLanguage'.
--
sourceBufferNewWithLanguage :: SourceLanguage -> IO SourceBuffer
sourceBufferNewWithLanguage lang = constructNewGObject mkSourceBuffer $
  {#call unsafe source_buffer_new_with_language#} lang

-- | 
--
sourceBufferSetCheckBrackets :: SourceBuffer -> Bool -> IO ()
sourceBufferSetCheckBrackets sb newVal =
  {#call unsafe source_buffer_set_check_brackets#} sb (fromBool newVal)
  
-- | 
--
sourceBufferGetCheckBrackets :: SourceBuffer -> IO Bool 
sourceBufferGetCheckBrackets sb = liftM toBool $
  {#call unsafe source_buffer_get_check_brackets#} sb

-- | 
--
sourceBufferSetBracketsMatchStyle :: SourceBuffer -> SourceTagStyle -> IO () 
sourceBufferSetBracketsMatchStyle sb ts =
  alloca $ \tsPtr -> do
  poke tsPtr ts
  {#call unsafe source_buffer_set_bracket_match_style#} sb (castPtr tsPtr)

-- | 
--
sourceBufferSetHighlight :: SourceBuffer -> Bool -> IO ()
sourceBufferSetHighlight sb newVal =
  {#call unsafe source_buffer_set_highlight#} sb (fromBool newVal)
  
-- | 
--
sourceBufferGetHighlight :: SourceBuffer -> IO Bool 
sourceBufferGetHighlight sb = liftM toBool $
  {#call unsafe source_buffer_get_highlight#} sb

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
sourceBufferSetEscapeChar :: SourceBuffer -> Char -> IO ()
sourceBufferSetEscapeChar sb char =
  {#call unsafe source_buffer_set_escape_char#} sb ((toEnum . fromEnum) char)
  
-- | 
--
sourceBufferGetEscapeChar :: SourceBuffer -> IO Char
sourceBufferGetEscapeChar sb = liftM (toEnum . fromEnum) $
  {#call unsafe source_buffer_get_escape_char#} sb

-- | 
--
sourceBufferCanUndo :: SourceBuffer -> IO Bool
sourceBufferCanUndo sb = liftM toBool $
  {#call unsafe source_buffer_can_undo#} sb
  
-- | 
--
sourceBufferCanRedo :: SourceBuffer -> IO Bool
sourceBufferCanRedo sb = liftM toBool $
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

sourceBufferCreateMarker :: SourceBuffer -- the buffer
			 -> Maybe String -- the name of the marker
			 -> String -- the type of the marker
			 -> TextIter -> IO SourceMarker
sourceBufferCreateMarker sb name markerType iter =
  constructNewGObject mkSourceMarker $
  maybeWith withCString name       $ \strPtr1 ->
  withCString markerType $ \strPtr2 ->
  {#call source_buffer_create_marker#} sb strPtr1 strPtr2 iter

-- | 
--
sourceBufferMoveMarker :: SourceBuffer -> SourceMarker -> TextIter -> IO ()
sourceBufferMoveMarker sb mark iter =
  {#call source_buffer_move_marker#} sb mark iter

-- | 
--
sourceBufferDeleteMarker :: SourceBuffer -> SourceMarker -> IO ()
sourceBufferDeleteMarker sb mark =
  {#call source_buffer_delete_marker#} sb mark

-- | 
--
sourceBufferGetMarker :: SourceBuffer -> String -> IO (Maybe SourceMarker)
sourceBufferGetMarker sb name =
  maybeNull (makeNewGObject mkSourceMarker) $
  withCString name $ \strPtr1 ->
  {#call unsafe source_buffer_get_marker#} sb strPtr1

-- | Returns an /ordered/ (by position) list of 'SourceMarker's inside the
--   region delimited by the two 'TextIter's. The iterators may be in any
--   order.
--
sourceBufferGetMarkersInRegion :: SourceBuffer -> TextIter -> TextIter -> IO [SourceMarker]
sourceBufferGetMarkersInRegion sb begin end = do
  gList <- {#call unsafe source_buffer_get_markers_in_region#} sb begin end
  wList <- fromGSList gList
  mapM (makeNewGObject mkSourceMarker) (map return wList)

-- | Returns the first (nearest to the top of the buffer) marker.
--
sourceBufferGetFirstMarker :: SourceBuffer -> IO (Maybe SourceMarker)
sourceBufferGetFirstMarker sb =
  maybeNull (makeNewGObject mkSourceMarker) $
  {#call unsafe source_buffer_get_first_marker#} sb

-- | Returns the last (nearest to the bottom of the buffer) marker.
--
sourceBufferGetLastMarker :: SourceBuffer -> IO (Maybe SourceMarker)
sourceBufferGetLastMarker sb =
  maybeNull (makeNewGObject mkSourceMarker) $
  {#call unsafe source_buffer_get_last_marker#} sb

-- | 
--
sourceBufferGetIterAtMarker :: SourceBuffer -> SourceMarker -> IO TextIter
sourceBufferGetIterAtMarker sb mark = do
  iter <- makeEmptyTextIter
  {#call unsafe source_buffer_get_iter_at_marker#} sb iter mark
  return iter

-- | Returns the nearest marker to the right of the given iterator.
-- If there are
-- multiple markers at the same position, this function will always
-- return the first one (from the internal linked list), even if
-- starting the search exactly at its location.  You can get the
-- others using 'sourceMarkerNext'.
--
sourceBufferGetNextMarker :: SourceBuffer -> TextIter -> IO (Maybe SourceMarker)
sourceBufferGetNextMarker sb iter = maybeNull (makeNewGObject mkSourceMarker) $
  {#call unsafe source_buffer_get_next_marker#} sb iter

-- | Returns the nearest marker to the left of the given iterator.
-- If there are
-- multiple markers at the same position, this function will always
-- return the last one (from the internal linked list), even if
-- starting the search exactly at its location.  You can get the
-- others using 'sourceMarkerPrev'.
--
sourceBufferGetPrevMarker :: SourceBuffer -> TextIter -> IO (Maybe SourceMarker)
sourceBufferGetPrevMarker sb iter = maybeNull (makeNewGObject mkSourceMarker) $
  {#call unsafe source_buffer_get_prev_marker#} sb iter
