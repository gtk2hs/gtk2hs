-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry SourceBuffer@
--
--  Author : Duncan Coutts
--  derived from GtkTextView bindings by Axel Simon
--          
--  Created: 15 October 2003
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
module SourceBuffer (
  SourceBuffer,
  SourceBufferClass,
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
import FFI
import Object	(makeNewObject)
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import SourceViewType#}
{#import Signal#}
import SourceTagStyle
import SourceMarker
{#import TextIter#}
import GList	(fromGSList)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor sourceBufferNew@ Create a new @ref type SourceBuffer@, possibly
-- taking a @ref type SourceTagTable@.
--
sourceBufferNew :: Maybe SourceTagTable -> IO SourceBuffer
sourceBufferNew tt = makeNewGObject mkSourceBuffer $
  {#call unsafe source_buffer_new#} 
  (fromMaybe (mkSourceTagTable nullForeignPtr) tt)

-- @constructor sourceBufferNewWithLanguage@ Create a new @ref type SourceBuffer@
-- with a @ref type SourceLanguage@.
--
sourceBufferNewWithLanguage :: SourceLanguage -> IO SourceBuffer
sourceBufferNewWithLanguage lang = makeNewGObject mkSourceBuffer $
  {#call unsafe source_buffer_new_with_language#} lang

-- @method sourceBufferSetCheckBrackets@
--
sourceBufferSetCheckBrackets :: SourceBuffer -> Bool -> IO ()
sourceBufferSetCheckBrackets sb newVal =
  {#call unsafe source_buffer_set_check_brackets#} sb (fromBool newVal)
  
-- @method sourceBufferGetCheckBrackets@
--
sourceBufferGetCheckBrackets :: SourceBuffer -> IO Bool 
sourceBufferGetCheckBrackets sb = liftM toBool $
  {#call unsafe source_buffer_get_check_brackets#} sb

-- @method sourceBufferSetBracketsMatchStyle@
--
sourceBufferSetBracketsMatchStyle :: SourceBuffer -> SourceTagStyle -> IO () 
sourceBufferSetBracketsMatchStyle sb ts =
  alloca $ \tsPtr -> do
  poke tsPtr ts
  {#call unsafe source_buffer_set_bracket_match_style#} sb (castPtr tsPtr)

-- @method sourceBufferSetHighlight@
--
sourceBufferSetHighlight :: SourceBuffer -> Bool -> IO ()
sourceBufferSetHighlight sb newVal =
  {#call unsafe source_buffer_set_highlight#} sb (fromBool newVal)
  
-- @method sourceBufferGetHighlight@
--
sourceBufferGetHighlight :: SourceBuffer -> IO Bool 
sourceBufferGetHighlight sb = liftM toBool $
  {#call unsafe source_buffer_get_highlight#} sb

-- @method sourceBufferSetMaxUndoLevels@
--
sourceBufferSetMaxUndoLevels :: SourceBuffer -> Int -> IO ()
sourceBufferSetMaxUndoLevels sb newVal =
  {#call unsafe source_buffer_set_max_undo_levels#} sb (fromIntegral newVal)
  
-- @method sourceBufferGetMaxUndoLevels@
--
sourceBufferGetMaxUndoLevels :: SourceBuffer -> IO Int
sourceBufferGetMaxUndoLevels sb = liftM fromIntegral $
  {#call unsafe source_buffer_get_max_undo_levels#} sb

-- @method sourceBufferSetLanguage@
--
sourceBufferSetLanguage :: SourceBuffer -> SourceLanguage -> IO ()
sourceBufferSetLanguage sb lang =
  {#call unsafe source_buffer_set_language#} sb lang
  
-- @method sourceBufferGetLanguage@
--
sourceBufferGetLanguage :: SourceBuffer -> IO SourceLanguage
sourceBufferGetLanguage sb = makeNewGObject mkSourceLanguage $
  {#call unsafe source_buffer_get_language#} sb

-- @method sourceBufferSetEscapeChar@
--
sourceBufferSetEscapeChar :: SourceBuffer -> Char -> IO ()
sourceBufferSetEscapeChar sb char =
  {#call unsafe source_buffer_set_escape_char#} sb ((toEnum . fromEnum) char)
  
-- @method sourceBufferGetEscapeChar@
--
sourceBufferGetEscapeChar :: SourceBuffer -> IO Char
sourceBufferGetEscapeChar sb = liftM (toEnum . fromEnum) $
  {#call unsafe source_buffer_get_escape_char#} sb

-- @method sourceBufferCanUndo@
--
sourceBufferCanUndo :: SourceBuffer -> IO Bool
sourceBufferCanUndo sb = liftM toBool $
  {#call unsafe source_buffer_can_undo#} sb
  
-- @method sourceBufferCanRedo@
--
sourceBufferCanRedo :: SourceBuffer -> IO Bool
sourceBufferCanRedo sb = liftM toBool $
  {#call unsafe source_buffer_can_redo#} sb

-- @method sourceBufferUndo@
--
sourceBufferUndo :: SourceBuffer -> IO ()
sourceBufferUndo sb =
  {#call source_buffer_undo#} sb
  
-- @method sourceBufferRedo@
--
sourceBufferRedo :: SourceBuffer -> IO ()
sourceBufferRedo sb =
  {#call source_buffer_redo#} sb

-- @method sourceBufferBeginNotUndoableAction@
--
sourceBufferBeginNotUndoableAction :: SourceBuffer -> IO ()
sourceBufferBeginNotUndoableAction sb =
  {#call source_buffer_begin_not_undoable_action#} sb
  
-- @method sourceBufferEndNotUndoableAction@
--
sourceBufferEndNotUndoableAction :: SourceBuffer -> IO ()
sourceBufferEndNotUndoableAction sb =
  {#call source_buffer_end_not_undoable_action#} sb

-- @method sourceBufferCreateMarker@
--
sourceBufferCreateMarker :: SourceBuffer -> String -> String -> TextIter -> IO SourceMarker
sourceBufferCreateMarker sb name markerType iter =
  makeNewGObject mkSourceMarker $
  withCString name       $ \strPtr1 ->
  withCString markerType $ \strPtr2 ->
  {#call source_buffer_create_marker#} sb strPtr1 strPtr2 iter

-- @method sourceBufferMoveMarker@
--
sourceBufferMoveMarker :: SourceBuffer -> SourceMarker -> TextIter -> IO ()
sourceBufferMoveMarker sb mark iter =
  {#call source_buffer_move_marker#} sb mark iter

-- @method sourceBufferDeleteMarker@
--
sourceBufferDeleteMarker :: SourceBuffer -> SourceMarker -> IO ()
sourceBufferDeleteMarker sb mark =
  {#call source_buffer_delete_marker#} sb mark

-- @method sourceBufferGetMarker@
--
sourceBufferGetMarker :: SourceBuffer -> String -> IO SourceMarker
sourceBufferGetMarker sb name =
  makeNewGObject mkSourceMarker $
  withCString name $ \strPtr1 ->
  {#call unsafe source_buffer_get_marker#} sb strPtr1

-- @method sourceBufferGetMarkersInRegion@
--
sourceBufferGetMarkersInRegion :: SourceBuffer -> TextIter -> TextIter -> IO [SourceMarker]
sourceBufferGetMarkersInRegion sb begin end = do
  gList <- {#call unsafe source_buffer_get_markers_in_region#} sb begin end
  wList <- fromGSList gList
  mapM (makeNewGObject mkSourceMarker) (map return wList)

-- @method sourceBufferGetFirstMarker@
--
sourceBufferGetFirstMarker :: SourceBuffer -> IO SourceMarker
sourceBufferGetFirstMarker sb =
  makeNewGObject mkSourceMarker $
  {#call unsafe source_buffer_get_first_marker#} sb

-- @method sourceBufferGetLastMarker@
--
sourceBufferGetLastMarker :: SourceBuffer -> IO SourceMarker
sourceBufferGetLastMarker sb =
  makeNewGObject mkSourceMarker $
  {#call unsafe source_buffer_get_last_marker#} sb

-- @method sourceBufferGetIterAtMarker@
--
sourceBufferGetIterAtMarker :: SourceBuffer -> SourceMarker -> IO TextIter
sourceBufferGetIterAtMarker sb mark = do
  iter <- makeEmptyTextIter
  {#call unsafe source_buffer_get_iter_at_marker#} sb iter mark
  return iter

-- @method sourceBufferGetNextMarker@
--
sourceBufferGetNextMarker :: SourceBuffer -> TextIter -> IO (Maybe SourceMarker)
sourceBufferGetNextMarker sb iter = do
  markPtr <- {#call unsafe source_buffer_get_next_marker#} sb iter
  if markPtr==nullPtr then return Nothing
                      else liftM Just $ makeNewGObject mkSourceMarker (return markPtr)

-- @method sourceBufferGetPrevMarker@
--
sourceBufferGetPrevMarker :: SourceBuffer -> TextIter -> IO (Maybe SourceMarker)
sourceBufferGetPrevMarker sb iter = do
  markPtr <- {#call unsafe source_buffer_get_prev_marker#} sb iter
  if markPtr==nullPtr then return Nothing
                      else liftM Just $ makeNewGObject mkSourceMarker (return markPtr)
