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
  sourceViewSetCheckBrackets,
  sourceViewGetCheckBrackets,
  sourceBufferSetHighlight,
  sourceBufferGetHighlight
) where

import Monad	(liftM)
import Maybe    (fromMaybe)
import FFI
import Object	(makeNewObject)
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import SourceViewType#}
{#import Signal#}
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

-- @method sourceViewSetCheckBrackets@
--
sourceViewSetCheckBrackets :: SourceBuffer -> Bool -> IO ()
sourceViewSetCheckBrackets sb newVal =
  {#call unsafe source_buffer_set_check_brackets#} sb (fromBool newVal)
  
-- @method sourceViewGetCheckBrackets@
--
sourceViewGetCheckBrackets :: SourceBuffer -> IO Bool 
sourceViewGetCheckBrackets sb = liftM toBool $
  {#call unsafe source_buffer_get_check_brackets#} sb

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
