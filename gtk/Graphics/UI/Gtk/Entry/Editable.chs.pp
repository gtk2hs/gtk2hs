-- -*-haskell-*-
--  GIMP Toolkit (GTK) Interface Editable
--
--  Author : Axel Simon, Duncan Coutts
--          
--  Created: 30 July 2004
--  split off from Entry.chs
--
--  Copyright (c) 1999..2002 Axel Simon
--  modified 2004 Duncan Coutts
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
-- |
--
-- * This is an interface for simple single-line text editing widgets. It is
--   implemented by "Entry" and "SpinButton".
--
-- * TODO
--
-- * Find out if \"insert-text\" signal is useful and how to bind it. It is
--   tricky because it has an in-out parameter.
--
module Graphics.UI.Gtk.Entry.Editable (
  -- * Data types
  Editable,
  EditableClass,
  castToEditable,

  -- * Methods
  editableSelectRegion,
  editableGetSelectionBounds,
  editableInsertText,
  editableDeleteText,
  editableGetChars,
  editableCutClipboard,
  editableCopyClipboard,
  editablePasteClipboard,
  editableDeleteSelection,
  editableSetEditable,
  editableGetEditable,
  editableSetPosition,
  editableGetPosition,
  
  -- * Signals
  onEditableChanged,
  afterEditableChanged,
  onDeleteText,
  afterDeleteText,
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import System.Glib.GObject (makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

-- | Select a span of text.
--
-- * A negative @end@ position will make the selection extend to the
--   end of the buffer.
--
-- * Calling this function with @start@=1 and @end@=4 it will
--   mark \"ask\" in the string \"Haskell\". (FIXME: verify)
--
editableSelectRegion :: EditableClass ed => ed -> Int -> Int -> IO ()
editableSelectRegion ed start end =
  {#call editable_select_region#} (toEditable ed)
    (fromIntegral start) (fromIntegral end)

-- | Get the span of the current selection.
--
-- * The returned tuple is not ordered. The second index represents the
--   position of the cursor. The first index is the other end of the
--   selection. If both numbers are equal there is in fact no selection.
--
editableGetSelectionBounds :: EditableClass ed => ed -> IO (Int,Int)
editableGetSelectionBounds ed = alloca $ \startPtr -> alloca $ \endPtr -> do
  {#call unsafe editable_get_selection_bounds#} (toEditable ed) startPtr endPtr
  start <- liftM fromIntegral $ peek startPtr
  end	<- liftM fromIntegral $ peek endPtr
  return (start,end)

-- | Insert new text at the specified position.
--
-- * If the position is invalid the text will be inserted at the end of the
--   buffer. The returned value reflects the actual insertion point.
--
editableInsertText :: EditableClass ed => ed -> String -> Int -> IO Int
editableInsertText ed str pos = withObject (fromIntegral pos) $ \posPtr ->
  withUTFStringLen str $ \(strPtr,len) -> do
    {#call editable_insert_text#} (toEditable ed) strPtr (fromIntegral len) 
      posPtr
    liftM fromIntegral $ peek posPtr

-- | Delete a given range of text.
--
-- * If the @end@ position is invalid, it is set to the lenght of the
--   buffer.
--
-- * @start@ is restricted to 0..@end@.
--
editableDeleteText :: EditableClass ed => ed -> Int -> Int -> IO ()
editableDeleteText ed start end = {#call editable_delete_text#} (toEditable ed)
  (fromIntegral start) (fromIntegral end)

-- | Retrieve a range of characters.
--
-- * Set @end@ to a negative value to reach the end of the buffer.
--
editableGetChars :: EditableClass ed => ed -> Int -> Int -> IO String
editableGetChars ed start end = do
  strPtr <- {#call unsafe editable_get_chars#} (toEditable ed) 
    (fromIntegral start) (fromIntegral end)
  str <- peekUTFString strPtr
  {#call unsafe g_free#} (castPtr strPtr)
  return str

-- | Cut the selected characters to the Clipboard.
--
editableCutClipboard :: EditableClass ed => ed -> IO ()
editableCutClipboard  = {#call editable_cut_clipboard#}.toEditable

-- | Copy the selected characters to the Clipboard.
--
editableCopyClipboard :: EditableClass ed => ed -> IO ()
editableCopyClipboard  = {#call editable_copy_clipboard#}.toEditable

-- | Paste the selected characters to the
-- Clipboard.
--
editablePasteClipboard :: EditableClass ed => ed -> IO ()
editablePasteClipboard  = {#call editable_paste_clipboard#}.toEditable

-- | Delete the current selection.
--
editableDeleteSelection :: EditableClass ed => ed -> IO ()
editableDeleteSelection  = {#call editable_delete_selection#}.toEditable

-- | Set the cursor to a specific position.
--
editableSetPosition :: EditableClass ed => ed -> Int -> IO ()
editableSetPosition ed pos = 
  {#call editable_set_position#} (toEditable ed) (fromIntegral pos)

-- | Get the current cursor position.
--
editableGetPosition :: EditableClass ed => ed -> IO Int
editableGetPosition ed = liftM fromIntegral $
  {#call unsafe editable_get_position#} (toEditable ed)

-- | Make the widget insensitive.
--
-- * Called with False will make the text uneditable.
--
editableSetEditable :: EditableClass ed => ed -> Bool -> IO ()
editableSetEditable ed isEditable = {#call editable_set_editable#}
  (toEditable ed) (fromBool isEditable)

-- | Retrieves whether the text is editable.
--
editableGetEditable :: EditableClass ed => ed -> IO Bool
editableGetEditable ed =
  liftM toBool $ {#call editable_get_editable#} (toEditable ed)
  

-- signals

-- | Emitted when the settings of the 'Editable' widget changes.
--
onEditableChanged, afterEditableChanged :: EditableClass ec => ec -> IO () ->
                                     IO (ConnectId ec)
onEditableChanged = connect_NONE__NONE "changed" False
afterEditableChanged = connect_NONE__NONE "changed" True

-- | Emitted when a piece of text is deleted from the 'Editable' widget.
--
onDeleteText, afterDeleteText :: EditableClass ec => ec ->
                                 (Int -> Int -> IO ()) -> IO (ConnectId ec)
onDeleteText = connect_INT_INT__NONE "delete_text" False
afterDeleteText = connect_INT_INT__NONE "delete_text" True

