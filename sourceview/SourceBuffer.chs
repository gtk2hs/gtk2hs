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
--  sourceBufferSetBracketsMatchStyle,
  sourceBufferSetHighlight,
  sourceBufferGetHighlight
{-
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
-}
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
import GList	(fromGList)

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

{-
-- @method sourceBufferSetBracketsMatchStyle@
--
sourceBufferSetBracketsMatchStyle :: SourceBuffer -> SourceTagStyle -> IO () 
sourceBufferSetBracketsMatchStyle sb ts = liftM toBool $
  {#call unsafe source_buffer_set_bracket_match_style#} sb ts
-}

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
