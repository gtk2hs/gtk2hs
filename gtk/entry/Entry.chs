-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Entry@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.4 $ from $Date: 2002/07/17 16:09:05 $
--
--  Copyright (c) 1999..2002 Axel Simon
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
-- * This widget lets the user enter a single line of text.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
--
-- * A couple of signals are not bound because I could not figure out what
--   they mean. Some of them do not seem to be emitted at all.
--
module Entry(
  Entry,
  EntryClass,
  castToEntry,
  entrySelectRegion,
  entryGetSelectionBounds,
  entryInsertText,
  entryDeleteText,
  entryGetChars,
  entryCutClipboard,
  entryCopyClipboard,
  entryPasteClipboard,
  entryDeleteSelection,
  entrySetEditable,
  entryNew,
  entrySetText,
  entryGetText,
  entryAppendText,
  entryPrependText,
  entrySetVisibility,
  entrySetInvisibleChar,
  entrySetMaxLength,
  entryGetActivatesDefault,
  entrySetActivatesDefault,
  entryGetHasFrame,
  entrySetHasFrame,
  entryGetWidthChars,
  entrySetWidthChars,
  onEntryActivate,
  afterEntryActivate,
  onEntryChanged,
  afterEntryChanged,
  onCopyClipboard,
  afterCopyClipboard,
  onCutClipboard,
  afterCutClipboard,
  onPasteClipboard,
  afterPasteClipboard,
  onDeleteText,
  afterDeleteText,
  onInsertAtCursor,
  afterInsertAtCursor,
  onToggleOverwrite,
  afterToggleOverwrite
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Char	(ord)

{# context lib="gtk" prefix="gtk" #}

-- methods originating in the Editable base class which is not really a base
-- class of in the Gtk Hierarchy (it is non-existant). I renamed
{#pointer *Editable foreign#}

toEditable :: EntryClass ed => ed -> Editable
toEditable = castForeignPtr.unEntry.toEntry

-- @method entrySelectRegion@ Select a span of text.
--
-- * A negative @ref arg end@ position will make the selection extend to the
--   end of the buffer.
--
-- * Calling this function with @ref arg start@=1 and @ref arg end@=4 it will
--   mark "ask" in the string "Haskell". (FIXME: verify)
--
entrySelectRegion :: EntryClass ed => ed -> Int -> Int -> IO ()
entrySelectRegion ed start end = {#call editable_select_region#}
  (toEditable ed) (fromIntegral start) (fromIntegral end)

-- @method entryGetSelectionBounds@ Get the span of the current selection.
--
-- * The returned tuple is not ordered. The second index represents the
--   position of the cursor. The first index is the other end of the
--   selection. If both numbers are equal there is in fact no selection.
--
entryGetSelectionBounds :: EntryClass ed => ed -> IO (Int,Int)
entryGetSelectionBounds ed = alloca $ \startPtr -> alloca $ \endPtr -> do
  {#call unsafe editable_get_selection_bounds#} (toEditable ed) startPtr endPtr
  start <- liftM fromIntegral $ peek startPtr
  end	<- liftM fromIntegral $ peek endPtr
  return (start,end)

-- @method entryInsertText@ Insert new text at the specified position.
--
-- * If the position is invalid the text will be inserted at the end of the
--   buffer. The returned value reflects the actual insertion point.
--
entryInsertText :: EntryClass ed => ed -> String -> Int -> IO Int
entryInsertText ed str pos = withObject (fromIntegral pos) $ \posPtr ->
  withCStringLen str $ \(strPtr,len) -> do
    {#call editable_insert_text#} (toEditable ed) strPtr (fromIntegral len) 
      posPtr
    liftM fromIntegral $ peek posPtr

-- @method entryDeleteText@ Delete a given range of text.
--
-- * If the @ref arg end@ position is invalid, it is set to the lenght of the
--   buffer.
--
-- * @ref arg start@ is restricted to 0..@end.
--
entryDeleteText :: EntryClass ed => ed -> Int -> Int -> IO ()
entryDeleteText ed start end = {#call editable_delete_text#} (toEditable ed)
  (fromIntegral start) (fromIntegral end)

-- @method entryGetChars@ Retrieve a range of characters.
--
-- * Set @ref arg end@ to a negative value to reach the end of the buffer.
--
entryGetChars :: EntryClass ed => ed -> Int -> Int -> IO String
entryGetChars ed start end = do
  strPtr <- {#call unsafe editable_get_chars#} (toEditable ed) 
    (fromIntegral start) (fromIntegral end)
  str <- peekCString strPtr
  {#call unsafe g_free#} (castPtr strPtr)
  return str

-- @method entryCutClipboard@ Cut the selected characters to the Clipboard.
--
entryCutClipboard :: EntryClass ed => ed -> IO ()
entryCutClipboard  = {#call editable_cut_clipboard#}.toEditable

-- @method entryCopyClipboard@ Copy the selected characters to the Clipboard.
--
entryCopyClipboard :: EntryClass ed => ed -> IO ()
entryCopyClipboard  = {#call editable_copy_clipboard#}.toEditable

-- @method entryPasteClipboard@ Paste the selected characters to the
-- Clipboard.
--
entryPasteClipboard :: EntryClass ed => ed -> IO ()
entryPasteClipboard  = {#call editable_paste_clipboard#}.toEditable

-- @method entryDeleteSelection@ Delete the current selection.
--
entryDeleteSelection :: EntryClass ed => ed -> IO ()
entryDeleteSelection  = {#call editable_delete_selection#}.toEditable

-- @method entrySetPosition@ Set the cursor to a specific position.
--
entrySetPosition :: EntryClass ed => ed -> Int -> IO ()
entrySetPosition ed pos = 
  {#call editable_set_position#} (toEditable ed) (fromIntegral pos)

-- @method entryGetPosition@ Get the current cursor position.
--
entryGetPosition :: EntryClass ed => ed -> IO Int
entryGetPosition ed = liftM fromIntegral $
  {#call unsafe editable_get_position#} (toEditable ed)

-- @method entrySetEditable@ Make an @ref type Entry@ insensitive.
--
-- * Called with False will make the text uneditable.
--
entrySetEditable :: EntryClass ed => ed -> Bool -> IO ()
entrySetEditable ed isEditable = {#call editable_set_editable#}
  (toEditable ed) (fromBool isEditable)


-- methods

-- @constructor entryNew@ Create a new @ref type Entry@ widget.
--
entryNew :: IO Entry
entryNew  = makeNewObject mkEntry $ liftM castPtr $ {#call unsafe entry_new#}

-- @method entrySetText@ Set the text of the @ref type Entry@ widget.
--
entrySetText :: EntryClass ec => ec -> String -> IO ()
entrySetText ec str = withCString str $ {#call entry_set_text#} (toEntry ec)

-- @method entryGetText@ Get the text of the @ref type Entry@ widget.
--
entryGetText :: EntryClass ec => ec -> IO String
entryGetText ec = {#call entry_get_text#} (toEntry ec) >>= peekCString

-- @method entryAppendText@ Append to the text of the @ref type Entry@ widget.
--
entryAppendText :: EntryClass ec => ec -> String -> IO ()
entryAppendText ec str = 
  withCString str $ {#call entry_append_text#} (toEntry ec)

-- @method entryPrependText@ Prepend the text of the @ref type Entry@ widget.
--
entryPrependText :: EntryClass ec => ec -> String -> IO ()
entryPrependText ec str = 
  withCString str $ {#call entry_prepend_text#} (toEntry ec)

-- @method entrySetVisibility@ Set whether to use password mode (display stars
-- instead of the text).
--
-- * The replacement character can be changed with
--   @ref method entrySetInvisibleChar@.
--
entrySetVisibility :: EntryClass ec => ec -> Bool -> IO ()
entrySetVisibility ec visible =
  {#call entry_set_visibility#} (toEntry ec) (fromBool visible)

-- @method entrySetInvisibleChar@ Set the replacement character for invisible
-- text.
--
entrySetInvisibleChar :: EntryClass ec => ec -> Char -> IO ()
entrySetInvisibleChar ec ch =
  {#call unsafe entry_set_invisible_char#} (toEntry ec) ((fromIntegral.ord) ch)

-- @method entrySetMaxLength@ Sets a maximum length the text may grow to.
--
-- * A negative number resets the restriction.
--
entrySetMaxLength :: EntryClass ec => ec -> Int -> IO ()
entrySetMaxLength ec max = 
  {#call entry_set_max_length#} (toEntry ec) (fromIntegral max)

-- @method entryGetActivatesDefault@ Query whether pressing return will
-- activate the default widget.
--
entryGetActivatesDefault :: EntryClass ec => ec -> IO Bool
entryGetActivatesDefault ec = liftM toBool $
  {#call unsafe entry_get_activates_default#} (toEntry ec)

-- @method entrySetActivatesDefault@ Specify if pressing return will activate
-- the default widget.
--
-- * This setting is useful in @ref arg Dialog@ boxes where enter should press
--   the default button.
--
entrySetActivatesDefault :: EntryClass ec => ec -> Bool -> IO ()
entrySetActivatesDefault ec setting = {#call entry_set_activates_default#}
  (toEntry ec) (fromBool setting)

-- @method entryGetHasFrame@ Query if the text @ref type Entry@ is displayed
-- with a frame around it.
--
entryGetHasFrame :: EntryClass ec => ec -> IO Bool
entryGetHasFrame ec = liftM toBool $
  {#call unsafe entry_get_has_frame#} (toEntry ec)

-- @method entrySetHasFrame@ Specifies whehter the @ref type Entry@ should be
-- in an etched-in frame.
--
entrySetHasFrame :: EntryClass ec => ec -> Bool -> IO ()
entrySetHasFrame ec setting = {#call entry_set_has_frame#}
  (toEntry ec) (fromBool setting)

-- @method entryGetWidthChars@ Retrieve the number of characters the widget
-- should ask for.
--
entryGetWidthChars :: EntryClass ec => ec -> IO Int
entryGetWidthChars ec = liftM fromIntegral $ 
  {#call unsafe entry_get_width_chars#} (toEntry ec)

-- @method entrySetWidthChars@ Specifies how large the @ref type Entry@ should
-- be in characters.
--
-- * This setting is only considered when the widget formulates its size
--   request. Make sure that it is not mapped (shown) before you change this
--   value.
--
entrySetWidthChars :: EntryClass ec => ec -> Int -> IO ()
entrySetWidthChars ec setting = {#call entry_set_width_chars#}
  (toEntry ec) (fromIntegral setting)


-- signals

-- @signal connectToEntryActivate@ Emitted when the user presses return within
-- the @ref arg Entry@ field.
--
onEntryActivate, afterEntryActivate :: EntryClass ec => ec -> IO () ->
                                       IO (ConnectId ec)
onEntryActivate = connect_NONE__NONE "activate" False
afterEntryActivate = connect_NONE__NONE "activate" True

-- @signal connectToEntryChanged@ Emitted when the settings of the
-- @ref arg Entry@ widget changes.
--
onEntryChanged, afterEntryChanged :: EntryClass ec => ec -> IO () ->
                                     IO (ConnectId ec)
onEntryChanged = connect_NONE__NONE "changed" False
afterEntryChanged = connect_NONE__NONE "changed" True

-- @signal connectToCopyClipboard@ Emitted when the current selection has been
-- copied to the clipboard.
--
onCopyClipboard, afterCopyClipboard :: EntryClass ec => ec -> IO () ->
                                       IO (ConnectId ec)
onCopyClipboard = connect_NONE__NONE "copy_clipboard" False
afterCopyClipboard = connect_NONE__NONE "copy_clipboard" True

-- @signal connectToCutClipboard@ Emitted when the current selection has been
-- cut to the clipboard.
--
onCutClipboard, afterCutClipboard :: EntryClass ec => ec -> IO () ->
                                     IO (ConnectId ec)
onCutClipboard = connect_NONE__NONE "cut_clipboard" False
afterCutClipboard = connect_NONE__NONE "cut_clipboard" True

-- @signal connectToPasteClipboard@ Emitted when the current selection has
-- been pasted from the clipboard.
--
onPasteClipboard, afterPasteClipboard :: EntryClass ec => ec -> IO () ->
                                         IO (ConnectId ec)
onPasteClipboard = connect_NONE__NONE "paste_clipboard" False
afterPasteClipboard = connect_NONE__NONE "paste_clipboard" True

-- @signal connectToDeleteText@ Emitted when a piece of text is deleted from
-- the @ref arg Entry@.
--
onDeleteText, afterDeleteText :: EntryClass ec => ec ->
                                 (Int -> Int -> IO ()) -> IO (ConnectId ec)
onDeleteText = connect_INT_INT__NONE "delete_text" False
afterDeleteText = connect_INT_INT__NONE "delete_text" True

-- @signal connectToInsertAtCursor@ Emitted when a piece of text is inserted
-- at the cursor position.
--
onInsertAtCursor, afterInsertAtCursor :: EntryClass ec => ec ->
                                         (String -> IO ()) ->
                                         IO (ConnectId ec)
onInsertAtCursor = connect_STRING__NONE "insert_at_cursor" False
afterInsertAtCursor = connect_STRING__NONE "insert_at_cursor" True

-- @signal connectToToggleOverwrite@ Emitted when the user changes from
-- overwriting to inserting.
--
onToggleOverwrite, afterToggleOverwrite :: EntryClass ec => ec -> IO () ->
                                           IO (ConnectId ec)
onToggleOverwrite = connect_NONE__NONE "toggle_overwrite" False
afterToggleOverwrite = connect_NONE__NONE "toggle_overwrite" True
