-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Entry
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/04 14:02:30 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
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
--- DESCRIPTION ---------------------------------------------------------------
--
-- * This widget lets the user enter a single line of text.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------
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
  connectToEntryActivate,
  connectToEntryChanged,
  connectToCopyClipboard,
  connectToCutClipboard,
  connectToPasteClipboard,
  connectToDeleteText,
  connectToInsertAtCursor,
  connectToToggleOverwrite
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

-- Select a span of text. (EXPORTED)
--
-- * A negative @end position will make the selection extend to the end
--   of the buffer.
--
-- * Calling this function with @start=1 and @end=4 it will mark "ask" in
--   the string "Haskell". (FIXME: verify)
--
entrySelectRegion :: EntryClass ed => Int -> Int -> ed -> IO ()
entrySelectRegion start end ed = {#call editable_select_region#}
  (toEditable ed) (fromIntegral start) (fromIntegral end)

-- Get the span of the current selection. (EXPORTED)
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

-- Insert new text at the specified position. (EXPORTED)
--
-- * If the position is invalid the text will be inserted at the end
--   of the buffer. The returned value reflects the actual insertion point.
--
entryInsertText :: EntryClass ed => String -> Int -> ed -> IO Int
entryInsertText str pos ed = withObject (fromIntegral pos) $ \posPtr ->
  withCStringLen str $ \(strPtr,len) -> do
    {#call editable_insert_text#} (toEditable ed) strPtr (fromIntegral len) 
      posPtr
    liftM fromIntegral $ peek posPtr

-- Delete a given range of text. (EXPORTED)
--
-- * If the @end position is invalid, it is set to the lenght of the buffer.
--
-- * @start is restricted to 0..@end.
--
entryDeleteText :: EntryClass ed => Int -> Int -> ed -> IO ()
entryDeleteText start end ed = {#call editable_delete_text#} (toEditable ed)
  (fromIntegral start) (fromIntegral end)

-- Retrieve a range of characters. (EXPORTED)
--
-- * Set @end to a negative value to reach the end of the buffer.
--
entryGetChars :: EntryClass ed => Int -> Int -> ed -> IO String
entryGetChars start end ed = do
  strPtr <- {#call unsafe editable_get_chars#} (toEditable ed) 
    (fromIntegral start) (fromIntegral end)
  str <- peekCString strPtr
  {#call unsafe g_free#} (castPtr strPtr)
  return str

-- Cut the selected characters to the Clipboard. (EXPORTED)
--
entryCutClipboard :: EntryClass ed => ed -> IO ()
entryCutClipboard = {#call editable_cut_clipboard#}.toEditable

-- Copy the selected characters to the Clipboard. (EXPORTED)
--
entryCopyClipboard :: EntryClass ed => ed -> IO ()
entryCopyClipboard = {#call editable_copy_clipboard#}.toEditable

-- Paste the selected characters to the Clipboard. (EXPORTED)
--
entryPasteClipboard :: EntryClass ed => ed -> IO ()
entryPasteClipboard = {#call editable_paste_clipboard#}.toEditable

-- Delete the current selection. (EXPORTED)
--
entryDeleteSelection :: EntryClass ed => ed -> IO ()
entryDeleteSelection = {#call editable_delete_selection#}.toEditable

-- Set the cursor to a specific position. (EXPORTED)
--
entrySetPosition :: EntryClass ed => Int -> ed -> IO ()
entrySetPosition pos ed = 
  {#call editable_set_position#} (toEditable ed) (fromIntegral pos)

-- Get the current cursor position. (EXPORTED)
--
entryGetPosition :: EntryClass ed => ed -> IO Int
entryGetPosition ed = liftM fromIntegral $
  {#call unsafe editable_get_position#} (toEditable ed)

-- Make an @Entry insensitive. (EXPORTED)
--
-- * Called with False will make the text uneditable.
--
entrySetEditable :: EntryClass ed => Bool -> ed -> IO ()
entrySetEditable isEditable ed = {#call editable_set_editable#}
  (toEditable ed) (fromBool isEditable)


-- methods

-- Create a new @Entry widget. (EXPORTED)
--
entryNew :: IO Entry
entryNew = makeNewObject mkEntry $ liftM castPtr $ {#call unsafe entry_new#}



-- Set the text of the @Entry widget. (EXPORTED)
--
entrySetText :: EntryClass ec => String -> ec -> IO ()
entrySetText str ec = withCString str $ {#call entry_set_text#} (toEntry ec)

-- Append to the text of the @Entry widget. (EXPORTED)
--
entryAppendText :: EntryClass ec => String -> ec -> IO ()
entryAppendText str ec = 
  withCString str $ {#call entry_append_text#} (toEntry ec)

-- Prepend the text of the @Entry widget. (EXPORTED)
--
entryPrependText :: EntryClass ec => String -> ec -> IO ()
entryPrependText str ec = 
  withCString str $ {#call entry_prepend_text#} (toEntry ec)

-- Set whether to use password mode (display stars instead of the text). 
-- (EXPORTED)
--
-- * The replacement character can be changed with @entrySetInvisibleChar.
--
entrySetVisibility :: EntryClass ec => Bool -> ec -> IO ()
entrySetVisibility visible ec =
  {#call entry_set_visibility#} (toEntry ec) (fromBool visible)

-- Set the replacement character for invisible text. (EXPORTED)
--
entrySetInvisibleChar :: EntryClass ec => Char -> ec -> IO ()
entrySetInvisibleChar ch ec =
  {#call unsafe entry_set_invisible_char#} (toEntry ec) ((fromIntegral.ord) ch)

-- Sets a maximum length the text may grow to. (EXPORTED)
--
-- * A negative number resets the restriction.
--
entrySetMaxLength :: EntryClass ec => Int -> ec -> IO ()
entrySetMaxLength max ec = 
  {#call entry_set_max_length#} (toEntry ec) (fromIntegral max)

-- Query whether pressing return will activate the default widget. (EXPORTED)
--
entryGetActivatesDefault :: EntryClass ec => ec -> IO Bool
entryGetActivatesDefault ec = liftM toBool $
  {#call unsafe entry_get_activates_default#} (toEntry ec)

-- Specify if pressing return will activate the default widget. (EXPORTED)
--
-- * This setting is useful in @Dialog boxes where enter should press
--   the default button.
--
entrySetActivatesDefault :: EntryClass ec => Bool -> ec -> IO ()
entrySetActivatesDefault setting ec = {#call entry_set_activates_default#}
  (toEntry ec) (fromBool setting)

-- Query if the text @Entry is displayed with a frame around it. (EXPORTED)
--
entryGetHasFrame :: EntryClass ec => ec -> IO Bool
entryGetHasFrame ec = liftM toBool $
  {#call unsafe entry_get_has_frame#} (toEntry ec)

-- Specifies whehter the @Entry should be in an etched-in frame. (EXPORTED)
--
--
entrySetHasFrame :: EntryClass ec => Bool -> ec -> IO ()
entrySetHasFrame setting ec = {#call entry_set_has_frame#}
  (toEntry ec) (fromBool setting)

-- Retrieve the number of characters the widget should ask for. (EXPORTED)
--
entryGetWidthChars :: EntryClass ec => ec -> IO Int
entryGetWidthChars ec = liftM fromIntegral $ 
  {#call unsafe entry_get_width_chars#} (toEntry ec)

-- Specifies how large the @Entry should be in characters. (EXPORTED)
--
-- * This setting is only considered when the widget formulates its size
--   request. Make sure that it is not mapped (shown) before you change
--   this value.
--
entrySetWidthChars :: EntryClass ec => Int -> ec -> IO ()
entrySetWidthChars setting ec = {#call entry_set_width_chars#}
  (toEntry ec) (fromIntegral setting)


-- signals

-- Emitted when the user presses return within the @Entry field. (EXPORTED)
--
connectToEntryActivate :: EntryClass ec =>
  IO () -> ConnectAfter -> ec -> IO (ConnectId ec)
connectToEntryActivate = connect_NONE__NONE "activate"

-- Emitted when the settings of the @Entry widget changes. (EXPORTED)
--
connectToEntryChanged :: EntryClass ec =>
  IO () -> ConnectAfter -> ec -> IO (ConnectId ec)
connectToEntryChanged = connect_NONE__NONE "changed"

-- Emitted when the current selection has been copied to the clipboard.
-- (EXPORTED)
--
connectToCopyClipboard :: EntryClass ec =>
  IO () -> ConnectAfter -> ec -> IO (ConnectId ec)
connectToCopyClipboard = connect_NONE__NONE "copy_clipboard"

-- Emitted when the current selection has been cut to the clipboard.
-- (EXPORTED)
--
connectToCutClipboard :: EntryClass ec =>
  IO () -> ConnectAfter -> ec -> IO (ConnectId ec)
connectToCutClipboard = connect_NONE__NONE "cut_clipboard"

-- Emitted when the current selection has been pasted from the clipboard.
-- (EXPORTED)
--
connectToPasteClipboard :: EntryClass ec =>
  IO () -> ConnectAfter -> ec -> IO (ConnectId ec)
connectToPasteClipboard = connect_NONE__NONE "paste_clipboard"

-- Emitted when a piece of text is deleted from the @Entry. (EXPORTED)
--
connectToDeleteText :: EntryClass ec =>
  (Int -> Int -> IO ()) -> ConnectAfter -> ec -> IO (ConnectId ec)
connectToDeleteText = connect_INT_INT__NONE "delete_text"

-- Emitted when a piece of text is inserted at the cursor position. (EXPORTED)
--
connectToInsertAtCursor :: EntryClass ec =>
  (String -> IO ()) -> ConnectAfter -> ec -> IO (ConnectId ec)
connectToInsertAtCursor = connect_STRING__NONE "insert_at_cursor"

-- Emitted when the user changes from overwriting to inserting. (EXPORTED)
--
connectToToggleOverwrite :: EntryClass ec =>
  IO () -> ConnectAfter -> ec -> IO (ConnectId ec)
connectToToggleOverwrite = connect_NONE__NONE "toggle_overwrite"
