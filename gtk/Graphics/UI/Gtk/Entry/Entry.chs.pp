-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Entry
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/25 01:11:33 $
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- TODO
--
-- A couple of signals are not bound because I could not figure out what
--   they mean. Some of them do not seem to be emitted at all.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A single line text entry field.
--
module Graphics.UI.Gtk.Entry.Entry (
-- * Description
-- 
-- | The 'Entry' widget is a single line text entry widget. A fairly large set
-- of key bindings are supported by default. If the entered text is longer than
-- the allocation of the widget, the widget will scroll so that the cursor
-- position is visible.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----Entry
-- |                     +----'SpinButton'
-- @

-- * Types
  Entry,
  EntryClass,
  castToEntry,

-- * Constructors
  entryNew,

-- * Methods
  entrySetText,
  entryGetText,
#ifndef DISABLE_DEPRECATED
  entryAppendText,
  entryPrependText,
#endif
  entrySetVisibility,
  entryGetVisibility,
  entrySetInvisibleChar,
  entryGetInvisibleChar,
  entrySetMaxLength,
  entryGetActivatesDefault,
  entrySetActivatesDefault,
  entryGetHasFrame,
  entrySetHasFrame,
  entryGetWidthChars,
  entrySetWidthChars,
#if GTK_CHECK_VERSION(2,4,0)
  entrySetAlignment,
  entryGetAlignment,
  entrySetCompletion,
  entryGetCompletion,
#endif

-- * Signals
  onEntryActivate,
  afterEntryActivate,
  onCopyClipboard,
  afterCopyClipboard,
  onCutClipboard,
  afterCutClipboard,
  onPasteClipboard,
  afterPasteClipboard,
  onInsertAtCursor,
  afterInsertAtCursor,
  onToggleOverwrite,
  afterToggleOverwrite
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import System.Glib.GObject (makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Char	(ord, chr)

{# context lib="gtk" prefix="gtk" #}

-- GtkEntry implements the GtkEditable interface
instance EditableClass Entry

--------------------
-- Constructors

-- | Create a new 'Entry' widget.
--
entryNew :: IO Entry
entryNew  = makeNewObject mkEntry $ liftM castPtr $ {#call unsafe entry_new#}

--------------------
-- Methods

-- | Set the text of the 'Entry' widget.
--
entrySetText :: EntryClass ec => ec -> String -> IO ()
entrySetText ec str = withUTFString str $ {#call entry_set_text#} (toEntry ec)

-- | Get the text of the 'Entry' widget.
--
entryGetText :: EntryClass ec => ec -> IO String
entryGetText ec = {#call entry_get_text#} (toEntry ec) >>= peekUTFString

#ifndef DISABLE_DEPRECATED
-- | Append to the text of the 'Entry' widget.
--
entryAppendText :: EntryClass ec => ec -> String -> IO ()
entryAppendText ec str = 
  withUTFString str $ {#call entry_append_text#} (toEntry ec)

-- | Prepend the text of the 'Entry' widget.
--
entryPrependText :: EntryClass ec => ec -> String -> IO ()
entryPrependText ec str = 
  withUTFString str $ {#call entry_prepend_text#} (toEntry ec)
#endif

-- | Set whether to use password mode (display stars instead of the text).
--
-- * The replacement character can be changed with 'entrySetInvisibleChar'.
--
entrySetVisibility :: EntryClass ec => ec -> Bool -> IO ()
entrySetVisibility ec visible =
  {#call entry_set_visibility#} (toEntry ec) (fromBool visible)

-- | Get whether widget is in password mode.
--
entryGetVisibility :: EntryClass ec => ec -> IO Bool
entryGetVisibility ec =
  liftM toBool $ {#call entry_get_visibility#} (toEntry ec)

-- | Set the replacement character for invisible text.
--
entrySetInvisibleChar :: EntryClass ec => ec -> Char -> IO ()
entrySetInvisibleChar ec ch =
  {#call unsafe entry_set_invisible_char#} (toEntry ec) ((fromIntegral.ord) ch)

-- | Get the current replacement character for invisible text,
-- or 0 if not in password mode.
--
entryGetInvisibleChar :: EntryClass ec => ec -> IO Char
entryGetInvisibleChar ec = liftM (chr.fromIntegral) $
  {#call unsafe entry_get_invisible_char#} (toEntry ec)

-- | Sets a maximum length the text may grow to.
--
-- * A negative number resets the restriction.
--
entrySetMaxLength :: EntryClass ec => ec -> Int -> IO ()
entrySetMaxLength ec max = 
  {#call entry_set_max_length#} (toEntry ec) (fromIntegral max)

-- | Gets a maximum length the text is allowed to grow to.
--
entryGetMaxLength :: EntryClass ec => ec -> IO Int
entryGetMaxLength ec =
  liftM fromIntegral $ {#call unsafe entry_get_max_length#} (toEntry ec)

-- | Query whether pressing return will activate the default widget.
--
entryGetActivatesDefault :: EntryClass ec => ec -> IO Bool
entryGetActivatesDefault ec = liftM toBool $
  {#call unsafe entry_get_activates_default#} (toEntry ec)

-- | Specify if pressing return will activate
-- the default widget.
--
-- * This setting is useful in 'Dialog' boxes where enter should press
--   the default button.
--
entrySetActivatesDefault :: EntryClass ec => ec -> Bool -> IO ()
entrySetActivatesDefault ec setting = {#call entry_set_activates_default#}
  (toEntry ec) (fromBool setting)

-- | Query if the text 'Entry' is displayed with a frame around it.
--
entryGetHasFrame :: EntryClass ec => ec -> IO Bool
entryGetHasFrame ec = liftM toBool $
  {#call unsafe entry_get_has_frame#} (toEntry ec)

-- | Specifies whehter the 'Entry' should be in an etched-in frame.
--
entrySetHasFrame :: EntryClass ec => ec -> Bool -> IO ()
entrySetHasFrame ec setting = {#call entry_set_has_frame#}
  (toEntry ec) (fromBool setting)

-- | Retrieve the number of characters the widget should ask for.
--
entryGetWidthChars :: EntryClass ec => ec -> IO Int
entryGetWidthChars ec = liftM fromIntegral $ 
  {#call unsafe entry_get_width_chars#} (toEntry ec)

-- | Specifies how large the 'Entry' should be in characters.
--
-- * This setting is only considered when the widget formulates its size
--   request. Make sure that it is not mapped (shown) before you change this
--   value.
--
entrySetWidthChars :: EntryClass ec => ec -> Int -> IO ()
entrySetWidthChars ec setting = {#call entry_set_width_chars#}
  (toEntry ec) (fromIntegral setting)

#if GTK_CHECK_VERSION(2,4,0)
-- | Sets the alignment for the contents of the entry. This controls the
-- horizontal positioning of the contents when the displayed text is shorter
-- than the width of the entry.
--
-- * Since gtk 2.4
--
entrySetAlignment :: EntryClass ec => ec -> Float -> IO ()
entrySetAlignment ec xalign =
  {#call entry_set_alignment#} (toEntry ec) (realToFrac xalign)

-- | Gets the value set by 'entrySetAlignment'.
--
-- * Since gtk 2.4
--
entryGetAlignment :: EntryClass ec => ec -> IO Float
entryGetAlignment ec =
  liftM realToFrac $ {#call unsafe entry_get_alignment#} (toEntry ec)

-- | Sets the auxiliary completion object to use with the entry. All further
-- configuration of the completion mechanism is done on completion using the
-- "EntryCompletion" API.
--
-- * Since gtk 2.4
--
entrySetCompletion :: EntryClass ec => ec -> EntryCompletion -> IO ()
entrySetCompletion ec completion = {#call gtk_entry_set_completion#}
  (toEntry ec) completion

-- | Returns the auxiliary completion object currently in use by the entry.
--
-- * Since gtk 2.4
--
entryGetCompletion :: EntryClass ec => ec -> IO EntryCompletion
entryGetCompletion ec =
  makeNewGObject mkEntryCompletion $
  {#call gtk_entry_get_completion#} (toEntry ec)
#endif

--------------------
-- Signals

-- | Emitted when the user presses return within
-- the 'Entry' field.
--
onEntryActivate, afterEntryActivate :: EntryClass ec => ec -> IO () ->
                                       IO (ConnectId ec)
onEntryActivate = connect_NONE__NONE "activate" False
afterEntryActivate = connect_NONE__NONE "activate" True

-- | Emitted when the settings of the
-- 'Entry' widget changes.
--
onEntryChanged, afterEntryChanged :: EntryClass ec => ec -> IO () ->
                                     IO (ConnectId ec)
onEntryChanged = connect_NONE__NONE "changed" False
afterEntryChanged = connect_NONE__NONE "changed" True

-- | Emitted when the current selection has been
-- copied to the clipboard.
--
onCopyClipboard, afterCopyClipboard :: EntryClass ec => ec -> IO () ->
                                       IO (ConnectId ec)
onCopyClipboard = connect_NONE__NONE "copy_clipboard" False
afterCopyClipboard = connect_NONE__NONE "copy_clipboard" True

-- | Emitted when the current selection has been
-- cut to the clipboard.
--
onCutClipboard, afterCutClipboard :: EntryClass ec => ec -> IO () ->
                                     IO (ConnectId ec)
onCutClipboard = connect_NONE__NONE "cut_clipboard" False
afterCutClipboard = connect_NONE__NONE "cut_clipboard" True

-- | Emitted when the current selection has
-- been pasted from the clipboard.
--
onPasteClipboard, afterPasteClipboard :: EntryClass ec => ec -> IO () ->
                                         IO (ConnectId ec)
onPasteClipboard = connect_NONE__NONE "paste_clipboard" False
afterPasteClipboard = connect_NONE__NONE "paste_clipboard" True

-- | Emitted when a piece of text is deleted from
-- the 'Entry'.
--
onDeleteText, afterDeleteText :: EntryClass ec => ec ->
                                 (Int -> Int -> IO ()) -> IO (ConnectId ec)
onDeleteText = connect_INT_INT__NONE "delete_text" False
afterDeleteText = connect_INT_INT__NONE "delete_text" True

-- | Emitted when a piece of text is inserted
-- at the cursor position.
--
onInsertAtCursor, afterInsertAtCursor :: EntryClass ec => ec ->
                                         (String -> IO ()) ->
                                         IO (ConnectId ec)
onInsertAtCursor = connect_STRING__NONE "insert_at_cursor" False
afterInsertAtCursor = connect_STRING__NONE "insert_at_cursor" True

-- | Emitted when the user changes from
-- overwriting to inserting.
--
onToggleOverwrite, afterToggleOverwrite :: EntryClass ec => ec -> IO () ->
                                           IO (ConnectId ec)
onToggleOverwrite = connect_NONE__NONE "toggle_overwrite" False
afterToggleOverwrite = connect_NONE__NONE "toggle_overwrite" True
