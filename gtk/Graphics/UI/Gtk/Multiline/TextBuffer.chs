{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) TextBuffer
--
--  Author : Axel Simon, Andy Stewart
--
--  Created: 23 February 2002
--
--  Copyright (C) 2001-2005 Axel Simon
--  Copyright (C) 2009 Andy Stewart
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
-- NOTES
--
-- Below `variadic` functions can't support by FFI:
--     gtk_text_buffer_insert_with_tags
--     gtk_text_buffer_insert_with_tags_by_name
--     gtk_text_buffer_create_tag
-- But above functions is not essential, we can use other functions do same work.
-- Example:
--
--     gtk_text_buffer_insert_with_tags equivalent to calling textBufferInsert,
--     then textBufferApplyTag on the inserted text.
--
--     gtk_text_buffer_insert_with_tags_by_name same as gtk_text_buffer_insert_with_tags,
--     just use textTagName handle tag name.
--
--     gtk_text_buffer_create_tag Equivalent to calling textTagNew
--     and then adding the tag to the buffer's tag table.
--
-- The following functions do not make sense due to Haskell's wide character
--   representation of Unicode:
--     gtk_text_buffer_get_iter_at_line_index
--
-- The function gtk_text_buffer_get_selection_bounds is only used to test
--   if there is a selection  (see 'textBufferHasSelection').
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Stores attributed text for display in a 'TextView'
--
module Graphics.UI.Gtk.Multiline.TextBuffer (
-- * Detail
--
-- | You may wish to begin by reading the text widget conceptual overview
-- which gives an overview of all the objects and data types related to the
-- text widget and how they work together.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----TextBuffer
-- @

-- * Types
  TextBuffer,
  TextBufferClass,
  castToTextBuffer, gTypeTextBuffer,
  toTextBuffer,

-- * Constructors
  textBufferNew,

-- * Methods
  textBufferGetLineCount,
  textBufferGetCharCount,
  textBufferGetTagTable,
  textBufferInsert,
  textBufferInsertByteString,
  textBufferInsertAtCursor,
  textBufferInsertByteStringAtCursor,
  textBufferInsertInteractive,
  textBufferInsertByteStringInteractive,
  textBufferInsertInteractiveAtCursor,
  textBufferInsertByteStringInteractiveAtCursor,
  textBufferInsertRange,
  textBufferInsertRangeInteractive,
  textBufferDelete,
  textBufferDeleteInteractive,
  textBufferSetByteString,
  textBufferGetByteString,
  textBufferGetByteStringSlice,
  textBufferSetText,
  textBufferGetText,
  textBufferGetSlice,
  textBufferInsertPixbuf,
  textBufferCreateMark,
#if GTK_CHECK_VERSION(2,12,0)
  textBufferAddMark,
#endif
  textBufferMoveMark,
  textBufferMoveMarkByName,
  textBufferDeleteMark,
  textBufferDeleteMarkByName,
  textBufferGetMark,
  textBufferGetInsert,
  textBufferGetSelectionBound,
  textBufferPlaceCursor,
  textBufferApplyTag,
  textBufferRemoveTag,
  textBufferApplyTagByName,
  textBufferRemoveTagByName,
  textBufferRemoveAllTags,
  textBufferGetIterAtLineOffset,
  textBufferGetIterAtOffset,
  textBufferGetIterAtLine,
  textBufferGetIterAtMark,
  textBufferGetStartIter,
  textBufferGetEndIter,
  textBufferGetModified,
  textBufferSetModified,
  textBufferDeleteSelection,
  textBufferHasSelection,
  textBufferGetSelectionBounds,
#if GTK_CHECK_VERSION(2,4,0)
  textBufferSelectRange,
#endif
  textBufferGetBounds,
  textBufferBeginUserAction,
  textBufferEndUserAction,
#if GTK_CHECK_VERSION(2,6,0)
  textBufferBackspace,
#endif
  textBufferInsertChildAnchor,
  textBufferCreateChildAnchor,
  textBufferGetIterAtChildAnchor,
#if GTK_CHECK_VERSION(2,2,0)
  textBufferPasteClipboard,
  textBufferPasteClipboardAtCursor,
  textBufferCopyClipboard,
  textBufferCutClipboard,
#endif
  textBufferAddSelectionClipboard,
  textBufferRemoveSelectionClipboard,

-- * Attributes
  textBufferTagTable,
#if GTK_CHECK_VERSION(2,8,0)
  textBufferText,
#endif
  textBufferModified,

-- * Signals
  applyTag,
  beginUserAction,
  bufferChanged,
  deleteRange,
  endUserAction,
  insertPixbuf,
  insertChildAnchor,
  bufferInsertText,
  markDeleted,
  markSet,
  modifiedChanged,
  pasteDone,
  removeTag,

-- * Deprecated
#ifndef DISABLE_DEPRECATED
  onApplyTag,
  afterApplyTag,
  onBeginUserAction,
  afterBeginUserAction,
  onBufferChanged,
  afterBufferChanged,
  onDeleteRange,
  afterDeleteRange,
  onEndUserAction,
  afterEndUserAction,
  onInsertPixbuf,
  afterInsertPixbuf,
  onBufferInsertText,
  afterBufferInsertText,
  onMarkDeleted,
  afterMarkDeleted,
  onMarkSet,
  afterMarkSet,
  onModifiedChanged,
  afterModifiedChanged,
  onRemoveTag,
  afterRemoveTag
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackCStringFinalizer)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.Multiline.Types#}
import Graphics.UI.Gtk.Multiline.TextMark       (MarkName)
import Graphics.UI.Gtk.Multiline.TextTag        (TagName)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new text buffer.
--
textBufferNew ::
    Maybe TextTagTable -- ^ @table@ - a tag table, or @Nothing@ to create a
                       -- new one
 -> IO TextBuffer
textBufferNew table =
  wrapNewGObject mkTextBuffer $
  {# call unsafe text_buffer_new #}
    (maybe (TextTagTable nullForeignPtr) toTextTagTable table)

--------------------
-- Methods

-- | Obtains the number of lines in the buffer. This value is cached, so the
-- function is very fast.
--
textBufferGetLineCount :: TextBufferClass self => self -> IO Int
textBufferGetLineCount self =
  liftM fromIntegral $
  {# call unsafe text_buffer_get_line_count #}
    (toTextBuffer self)

-- | Gets the number of characters in the buffer. The character count is
-- cached, so this function is very fast.
--
textBufferGetCharCount :: TextBufferClass self => self -> IO Int
textBufferGetCharCount self =
  liftM fromIntegral $
  {# call unsafe text_buffer_get_char_count #}
    (toTextBuffer self)

-- | Get the 'TextTagTable' associated with this buffer.
--
textBufferGetTagTable :: TextBufferClass self => self -> IO TextTagTable
textBufferGetTagTable self =
  makeNewGObject mkTextTagTable $
  {# call unsafe text_buffer_get_tag_table #}
    (toTextBuffer self)

-- | Inserts @text@ at position @iter@. Emits the
-- 'insertText' signal; insertion actually occurs in the default handler for
-- the signal. @iter@ is invalidated when insertion occurs (because the buffer
-- contents change).
--
textBufferInsert :: (TextBufferClass self, GlibString string) => self
 -> TextIter -- ^ @iter@ - a position in the buffer
 -> string   -- ^ @text@ - text to insert
 -> IO ()
textBufferInsert self iter text =
  withUTFStringLen text $ \(textPtr, len) ->
  {# call text_buffer_insert #}
    (toTextBuffer self)
    iter
    textPtr
    (fromIntegral len)

-- | Inserts @text@ at position @iter@. Similar
-- to 'textBufferInsert' but uses 'ByteString' buffers.
--
-- * The passed-in buffer must contain a valid UTF-8 encoded string.
--
textBufferInsertByteString :: TextBufferClass self => self
 -> TextIter -- ^ @iter@ - a position in the buffer
 -> ByteString   -- ^ @text@ - text to insert
 -> IO ()
textBufferInsertByteString self iter text =
  unsafeUseAsCStringLen text $ \(textPtr, len) ->
  {# call text_buffer_insert #}
    (toTextBuffer self)
    iter
    textPtr
    (fromIntegral len)

-- | Simply calls 'textBufferInsert', using the current cursor position as the
-- insertion point.
--
textBufferInsertAtCursor :: (TextBufferClass self, GlibString string) => self -> string -> IO ()
textBufferInsertAtCursor self text =
  withUTFStringLen text $ \(textPtr, len) ->
  {# call text_buffer_insert_at_cursor #}
    (toTextBuffer self)
    textPtr
    (fromIntegral len)

-- | Simply calls 'textBufferInsert', using the current cursor position as the
-- insertion point. Similar to 'textBufferInsertAtCursor' but uses 'ByteString' buffers.
--
-- * The passed-in buffer must contain a valid UTF-8 encoded string.
--
textBufferInsertByteStringAtCursor :: TextBufferClass self => self -> ByteString -> IO ()
textBufferInsertByteStringAtCursor self text =
  unsafeUseAsCStringLen text $ \(textPtr, len) ->
  {# call text_buffer_insert_at_cursor #}
    (toTextBuffer self)
    textPtr
    (fromIntegral len)

-- | Like 'textBufferInsert', but the insertion will not occur if @iter@ is at
-- a non-editable location in the buffer. Usually you want to prevent
-- insertions at ineditable locations if the insertion results from a user
-- action (is interactive).
--
-- If no tag is at the specified position, use the default value @def@ to
-- decide if the text should be inserted. This value could be set to the result
-- of 'Graphics.UI.Gtk.Multiline.TextView.textViewGetEditable'.
--
textBufferInsertInteractive :: (TextBufferClass self, GlibString string) => self
 -> TextIter -- ^ @iter@ - a position in @buffer@
 -> string   -- ^ @text@ - the text to insert
 -> Bool     -- ^ @defaultEditable@ - default editability of buffer
 -> IO Bool  -- ^ returns whether text was actually inserted
textBufferInsertInteractive self iter text defaultEditable =
  liftM toBool $
  withUTFStringLen text $ \(textPtr, len) ->
  {# call text_buffer_insert_interactive #}
    (toTextBuffer self)
    iter
    textPtr
    (fromIntegral len)
    (fromBool defaultEditable)

-- | Similar to 'textBufferInsertInteractive' but uses 'ByteString' buffers.
--
-- * The passed-in buffer must contain a valid UTF-8 encoded string.
--
textBufferInsertByteStringInteractive :: TextBufferClass self => self
 -> TextIter -- ^ @iter@ - a position in @buffer@
 -> ByteString   -- ^ @text@ - the text to insert
 -> Bool     -- ^ @defaultEditable@ - default editability of buffer
 -> IO Bool  -- ^ returns whether text was actually inserted
textBufferInsertByteStringInteractive self iter text defaultEditable =
  liftM toBool $
  unsafeUseAsCStringLen text $ \(textPtr, len) ->
  {# call text_buffer_insert_interactive #}
    (toTextBuffer self)
    iter
    textPtr
    (fromIntegral len)
    (fromBool defaultEditable)

-- | Calls 'textBufferInsertInteractive' at the cursor position.
--
textBufferInsertInteractiveAtCursor :: (TextBufferClass self, GlibString string) => self
 -> string  -- ^ @text@ - the text to insert
 -> Bool    -- ^ @defaultEditable@ - default editability of buffer
 -> IO Bool -- ^ returns whether text was actually inserted
textBufferInsertInteractiveAtCursor self text defaultEditable =
  liftM toBool $
  withUTFStringLen text $ \(textPtr, len) ->
  {# call text_buffer_insert_interactive_at_cursor #}
    (toTextBuffer self)
    textPtr
    (fromIntegral len)
    (fromBool defaultEditable)

-- | Similar to 'textBufferInsertInteractiveAtCursor' but uses 'ByteString' buffers.
--
-- * The passed-in buffer must contain a valid UTF-8 encoded string.
--
textBufferInsertByteStringInteractiveAtCursor :: TextBufferClass self => self
 -> ByteString  -- ^ @text@ - the text to insert
 -> Bool    -- ^ @defaultEditable@ - default editability of buffer
 -> IO Bool -- ^ returns whether text was actually inserted
textBufferInsertByteStringInteractiveAtCursor self text defaultEditable =
  liftM toBool $
  unsafeUseAsCStringLen text $ \(textPtr, len) ->
  {# call text_buffer_insert_interactive_at_cursor #}
    (toTextBuffer self)
    textPtr
    (fromIntegral len)
    (fromBool defaultEditable)

-- | Copies text, tags, and pixbufs between @start@ and @end@ (the order of
-- @start@ and @end@ doesn't matter) and inserts the copy at @iter@. Used
-- instead of simply getting\/inserting text because it preserves images and
-- tags. If @start@ and @end@ are in a different buffer from @buffer@, the two
-- buffers must share the same tag table.
--
-- Implemented via emissions of the insert-text and 'applyTag' signals, so
-- expect those.
--
textBufferInsertRange :: TextBufferClass self => self
 -> TextIter -- ^ @iter@ - a position in the buffer
 -> TextIter -- ^ @start@ - a position in a 'TextBuffer'
 -> TextIter -- ^ @end@ - another position in the same buffer as @start@
 -> IO ()
textBufferInsertRange self iter start end =
  {# call text_buffer_insert_range #}
    (toTextBuffer self)
    iter
    start
    end

-- | Same as 'textBufferInsertRange', but does nothing if the insertion point
-- isn't editable. The @defaultEditable@ parameter indicates whether the text
-- is editable at @iter@ if no tags enclosing @iter@ affect editability.
-- Typically the result of
-- 'Graphics.UI.Gtk.Multiline.TextView.textViewGetEditable' is appropriate here.
--
textBufferInsertRangeInteractive :: TextBufferClass self => self
 -> TextIter -- ^ @iter@ - a position in the buffer
 -> TextIter -- ^ @start@ - a position in a 'TextBuffer'
 -> TextIter -- ^ @end@ - another position in the same buffer as @start@
 -> Bool     -- ^ @defaultEditable@ - default editability of the buffer
 -> IO Bool  -- ^ returns whether an insertion was possible at @iter@
textBufferInsertRangeInteractive self iter start end defaultEditable =
  liftM toBool $
  {# call text_buffer_insert_range_interactive #}
    (toTextBuffer self)
    iter
    start
    end
    (fromBool defaultEditable)

-- | Deletes text between @start@ and @end@. The order of @start@ and @end@ is
-- not actually relevant; 'textBufferDelete' will reorder them. This function
-- actually emits the 'deleteRange' signal, and the default handler of that
-- signal deletes the text. Because the buffer is modified, all outstanding
-- iterators become invalid after calling this function; however, the @start@
-- and @end@ will be re-initialized to point to the location where text was
-- deleted.
--
textBufferDelete :: TextBufferClass self => self
 -> TextIter -- ^ @start@ - a position in @buffer@
 -> TextIter -- ^ @end@ - another position in @buffer@
 -> IO ()
textBufferDelete self start end =
  {# call text_buffer_delete #}
    (toTextBuffer self)
    start
    end

-- | Deletes all /editable/ text in the given range. Calls 'textBufferDelete'
-- for each editable sub-range of [@start@,@end@). @start@ and @end@ are
-- revalidated to point to the location of the last deleted range, or left
-- untouched if no text was deleted.
--
textBufferDeleteInteractive :: TextBufferClass self => self
 -> TextIter -- ^ @startIter@ - start of range to delete
 -> TextIter -- ^ @endIter@ - end of range
 -> Bool     -- ^ @defaultEditable@ - whether the buffer is editable by
             -- default
 -> IO Bool  -- ^ returns whether some text was actually deleted
textBufferDeleteInteractive self startIter endIter defaultEditable =
  liftM toBool $
  {# call text_buffer_delete_interactive #}
    (toTextBuffer self)
    startIter
    endIter
    (fromBool defaultEditable)

-- | Deletes current contents of @buffer@, and inserts @text@ instead.
--
textBufferSetText :: (TextBufferClass self, GlibString string) => self
 -> string -- ^ @text@ - text to insert
 -> IO ()
textBufferSetText self text =
  withUTFStringLen text $ \(textPtr, len) ->
  {# call text_buffer_set_text #}
    (toTextBuffer self)
    textPtr
    (fromIntegral len)

-- | Returns the text in the range [@start@,@end@). Excludes undisplayed text
-- (text marked with tags that set the invisibility attribute) if
-- @includeHiddenChars@ is @False@. Does not include characters representing
-- embedded images, so character indexes into the returned string do
-- /not/ correspond to character indexes into the buffer. Contrast
-- with 'textBufferGetSlice'.
--
textBufferGetText :: (TextBufferClass self, GlibString string) => self
 -> TextIter  -- ^ @start@ - start of a range
 -> TextIter  -- ^ @end@ - end of a range
 -> Bool      -- ^ @includeHiddenChars@ - whether to include invisible text
 -> IO string
textBufferGetText self start end includeHiddenChars =
  {# call unsafe text_buffer_get_text #}
    (toTextBuffer self)
    start
    end
    (fromBool includeHiddenChars)
  >>= readUTFString

-- | Returns the text in the range [@start@,@end@). Excludes undisplayed text
-- (text marked with tags that set the invisibility attribute) if
-- @includeHiddenChars@ is @False@. The returned string includes a
-- @(chr 0xFFFC)@ character whenever the buffer contains embedded images, so
-- character indexes into the returned string /do/ correspond to
-- character indexes into the buffer. Contrast with 'textBufferGetText'. Note
-- that @(chr 0xFFFC)@ can occur in normal text as well, so it is not a reliable
-- indicator that a pixbuf or widget is in the buffer.
--
textBufferGetSlice :: (TextBufferClass self, GlibString string) => self
 -> TextIter  -- ^ @start@ - start of a range
 -> TextIter  -- ^ @end@ - end of a range
 -> Bool      -- ^ @includeHiddenChars@ - whether to include invisible text
 -> IO string
textBufferGetSlice self start end includeHiddenChars =
  {# call unsafe text_buffer_get_slice #}
    (toTextBuffer self)
    start
    end
    (fromBool includeHiddenChars)
  >>= readUTFString

-- | Deletes current contents of @buffer@, and inserts @text@ instead. Similar
-- to 'textBufferSetText' but uses 'ByteString' buffers.
--
-- * The passed-in buffer must contain a valid UTF-8 encoded string.
--
textBufferSetByteString :: TextBufferClass self => self
 -> ByteString -- ^ @text@ - text to insert
 -> IO ()
textBufferSetByteString self text =
  unsafeUseAsCStringLen text $ \(textPtr, len) ->
  {# call text_buffer_set_text #}
    (toTextBuffer self)
    textPtr
    (fromIntegral len)

-- | Returns the text in the range [@start@,@end@). Similar to
-- `textBufferGetText` but uses 'ByteString' buffers.
--
-- * The returned buffer is a UTF-8 encoded string.
--
textBufferGetByteString :: TextBufferClass self => self
 -> TextIter  -- ^ @start@ - start of a range
 -> TextIter  -- ^ @end@ - end of a range
 -> Bool      -- ^ @includeHiddenChars@ - whether to include invisible text
 -> IO ByteString
textBufferGetByteString self start end includeHiddenChars = do
  sPtr <- {# call unsafe text_buffer_get_text #}
    (toTextBuffer self)
    start
    end
    (fromBool includeHiddenChars)
  sLen <- lengthArray0 0 sPtr
  unsafePackCStringFinalizer (castPtr sPtr) (fromIntegral sLen)
    ({#call unsafe g_free#} (castPtr sPtr))

-- | Returns the text in the range [@start@,@end@). Similar to
-- `textBufferGetSlice` but uses 'ByteString' buffers.
--
-- * The returned buffer is a UTF-8 encoded string.
--
textBufferGetByteStringSlice :: TextBufferClass self => self
 -> TextIter  -- ^ @start@ - start of a range
 -> TextIter  -- ^ @end@ - end of a range
 -> Bool      -- ^ @includeHiddenChars@ - whether to include invisible text
 -> IO ByteString
textBufferGetByteStringSlice self start end includeHiddenChars = do
  sPtr <- {# call unsafe text_buffer_get_slice #}
    (toTextBuffer self)
    start
    end
    (fromBool includeHiddenChars)
  sLen <- lengthArray0 0 sPtr
  unsafePackCStringFinalizer (castPtr sPtr) (fromIntegral sLen)
    ({#call unsafe g_free#} (castPtr sPtr))

-- | Inserts an image into the text buffer at @iter@. The image will be
-- counted as one character in character counts, and when obtaining the buffer
-- contents as a string, will be represented by the Unicode \"object
-- replacement character\" @(chr 0xFFFC)@. Note that the \"slice\" variants for
-- obtaining portions of the buffer as a string include this character for
-- pixbufs, but the \"text\" variants do not. e.g. see 'textBufferGetSlice' and
-- 'textBufferGetText'.
--
textBufferInsertPixbuf :: TextBufferClass self => self
 -> TextIter -- ^ @iter@ - location to insert the pixbuf
 -> Pixbuf   -- ^ @pixbuf@ - a 'Pixbuf'
 -> IO ()
textBufferInsertPixbuf self iter pixbuf =
  {# call text_buffer_insert_pixbuf #}
    (toTextBuffer self)
    iter
    pixbuf

-- | Creates a mark at position @where@. If @markName@ is @Nothing@, the mark
-- is anonymous; otherwise, the mark can be retrieved by name using
-- 'textBufferGetMark'. If a mark has left gravity, and text is inserted at the
-- mark's current location, the mark will be moved to the left of the
-- newly-inserted text. If the mark has right gravity (@leftGravity@ =
-- @False@), the mark will end up on the right of newly-inserted text. The
-- standard left-to-right cursor is a mark with right gravity (when you type,
-- the cursor stays on the right side of the text you're typing).
--
-- Emits the 'markSet' signal as notification of the mark's initial
-- placement.
--
textBufferCreateMark :: TextBufferClass self => self
 -> Maybe MarkName -- ^ @markName@ - name for mark, or @Nothing@
 -> TextIter     -- ^ @where@ - location to place mark
 -> Bool         -- ^ @leftGravity@ - whether the mark has left gravity
 -> IO TextMark  -- ^ returns the new 'TextMark' object
textBufferCreateMark self markName where_ leftGravity =
  makeNewGObject mkTextMark $
  maybeWith withUTFString markName $ \markNamePtr ->
  {# call text_buffer_create_mark #}
    (toTextBuffer self)
    markNamePtr
    where_
    (fromBool leftGravity)

#if GTK_CHECK_VERSION(2,12,0)
-- | Adds the mark at position given by the 'TextIter'.
-- The mark may not be added to any other buffer.
--
-- Emits the 'markSet' signal as notification of the mark's initial placement.
--
textBufferAddMark :: TextBufferClass self => self
 -> TextMark  -- ^ @mark@ the mark to add
 -> TextIter  -- ^ @iter@ location to place mark
 -> IO ()
textBufferAddMark self mark iter =
  {# call text_buffer_add_mark #} (toTextBuffer self) (toTextMark mark) iter
#endif

-- | Moves @mark@ to the new location @where@. Emits the 'markSet' signal
-- as notification of the move.
--
textBufferMoveMark :: (TextBufferClass self, TextMarkClass mark) => self
 -> mark     -- ^ @mark@ - a 'TextMark'
 -> TextIter -- ^ @where@ - new location for @mark@ in the buffer
 -> IO ()
textBufferMoveMark self mark where_ =
  {# call text_buffer_move_mark #}
    (toTextBuffer self)
    (toTextMark mark)
    where_

-- | Moves the mark named @name@ (which must exist) to location @where@. See
-- 'textBufferMoveMark' for details.
--
textBufferMoveMarkByName :: TextBufferClass self => self
 -> MarkName   -- ^ @name@ - name of a mark
 -> TextIter -- ^ @where@ - new location for mark
 -> IO ()
textBufferMoveMarkByName self name where_ =
  withUTFString name $ \namePtr ->
  {# call text_buffer_move_mark_by_name #}
    (toTextBuffer self)
    namePtr
    where_

-- | Deletes @mark@, so that it's no longer located anywhere in the buffer.
-- Most operations on @mark@ become invalid. There is no way to undelete a
-- mark. 'Graphics.UI.Gtk.Multiline.TextMark.textMarkGetDeleted' will
-- return @True@ after this function has been
-- called on a mark; 'Graphics.UI.Gtk.Multiline.TextMark.textMarkGetDeleted'
-- indicates that a mark no longer
-- belongs to a buffer. The 'markDeleted' signal will be emitted as
-- notification after the mark is deleted.
--
textBufferDeleteMark :: (TextBufferClass self, TextMarkClass mark) => self
 -> mark  -- ^ @mark@ - a 'TextMark' in the buffer
 -> IO ()
textBufferDeleteMark self mark =
  {# call text_buffer_delete_mark #}
    (toTextBuffer self)
    (toTextMark mark)

-- | Deletes the mark named @name@; the mark must exist. See
-- 'textBufferDeleteMark' for details.
--
textBufferDeleteMarkByName :: TextBufferClass self => self
 -> MarkName -- ^ @name@ - name of a mark in @buffer@
 -> IO ()
textBufferDeleteMarkByName self name =
  withUTFString name $ \namePtr ->
  {# call text_buffer_delete_mark_by_name #}
    (toTextBuffer self)
    namePtr

-- | Returns the mark named @name@ in the buffer, or @Nothing@ if no such
-- mark exists in the buffer.
--
textBufferGetMark :: TextBufferClass self => self
 -> MarkName            -- ^ @name@ - a mark name
 -> IO (Maybe TextMark) -- ^ returns a 'TextMark', or @Nothing@
textBufferGetMark self name =
  maybeNull (makeNewGObject mkTextMark) $
  withUTFString name $ \namePtr ->
  {# call unsafe text_buffer_get_mark #}
    (toTextBuffer self)
    namePtr

-- | Returns the mark that represents the cursor (insertion point). Equivalent
-- to calling @liftM fromJust $ textBufferGetMark \"insert\"@, but very
-- slightly more efficient, and involves less typing.
--
textBufferGetInsert :: TextBufferClass self => self -> IO TextMark
textBufferGetInsert self =
  makeNewGObject mkTextMark $
  {# call unsafe text_buffer_get_insert #}
    (toTextBuffer self)

-- | Returns the mark that represents the selection bound. Equivalent to
-- calling @liftM fromJust $ textBufferGetMark \"selection_bound\"@, but
-- very slightly more efficient, and involves less typing.
--
-- The currently-selected text in @buffer@ is the region between the
-- \"selection_bound\" and \"insert\" marks. If \"selection_bound\" and
-- \"insert\" are in the same place, then there is no current selection.
-- 'textBufferGetSelectionBounds' is another convenient function for handling
-- the selection, if you just want to know whether there's a selection and what
-- its bounds are.
--
textBufferGetSelectionBound :: TextBufferClass self => self -> IO TextMark
textBufferGetSelectionBound self =
  makeNewGObject mkTextMark $
  {# call unsafe text_buffer_get_selection_bound #}
    (toTextBuffer self)

-- | This function moves the \"insert\" and \"selection_bound\" marks
-- simultaneously. If you move them to the same place in two steps with
-- 'textBufferMoveMark', you will temporarily select a region in between their
-- old and new locations, which can be pretty inefficient since the
-- temporarily-selected region will force stuff to be recalculated. This
-- function moves them as a unit, which can be optimized.
--
textBufferPlaceCursor :: TextBufferClass self => self
 -> TextIter -- ^ @where@ - where to put the cursor
 -> IO ()
textBufferPlaceCursor self where_ =
  {# call text_buffer_place_cursor #}
    (toTextBuffer self)
    where_

-- | Emits the 'applyTag' signal on the buffer. The default handler for the
-- signal applies @tag@ to the given range. @start@ and @end@ do not have to be
-- in order.
--
textBufferApplyTag :: (TextBufferClass self, TextTagClass tag) => self
 -> tag      -- ^ @tag@ - a 'TextTag'
 -> TextIter -- ^ @start@ - one bound of range to be tagged
 -> TextIter -- ^ @end@ - other bound of range to be tagged
 -> IO ()
textBufferApplyTag self tag start end =
  {# call text_buffer_apply_tag #}
    (toTextBuffer self)
    (toTextTag tag)
    start
    end

-- | Emits the 'removeTag' signal. The default handler for the signal
-- removes all occurrences of @tag@ from the given range. @start@ and @end@
-- don't have to be in order.
--
textBufferRemoveTag :: (TextBufferClass self, TextTagClass tag) => self
 -> tag      -- ^ @tag@ - a 'TextTag'
 -> TextIter -- ^ @start@ - one bound of range to be untagged
 -> TextIter -- ^ @end@ - other bound of range to be untagged
 -> IO ()
textBufferRemoveTag self tag start end =
  {# call text_buffer_remove_tag #}
    (toTextBuffer self)
    (toTextTag tag)
    start
    end

-- | Calls 'Graphics.UI.Gtk.Multiline.TextTagTable.textTagTableLookup' on the
--   buffer's tag table to get a 'TextTag', then calls 'textBufferApplyTag'.
--
textBufferApplyTagByName :: TextBufferClass self => self
 -> TagName  -- ^ @name@ - name of a named 'TextTag'
 -> TextIter -- ^ @start@ - one bound of range to be tagged
 -> TextIter -- ^ @end@ - other bound of range to be tagged
 -> IO ()
textBufferApplyTagByName self name start end =
  withUTFString name $ \namePtr ->
  {# call text_buffer_apply_tag_by_name #}
    (toTextBuffer self)
    namePtr
    start
    end

-- | Calls 'Graphics.UI.Gtk.Multiline.TextTagTable.textTagTableLookup' on the
--   buffer's tag table to get a 'TextTag', then calls 'textBufferRemoveTag'.
--
textBufferRemoveTagByName :: TextBufferClass self => self
 -> TagName  -- ^ @name@ - name of a 'TextTag'
 -> TextIter -- ^ @start@ - one bound of range to be untagged
 -> TextIter -- ^ @end@ - other bound of range to be untagged
 -> IO ()
textBufferRemoveTagByName self name start end =
  withUTFString name $ \namePtr ->
  {# call text_buffer_remove_tag_by_name #}
    (toTextBuffer self)
    namePtr
    start
    end

-- | Removes all tags in the range between @start@ and @end@. Be careful with
-- this function; it could remove tags added in code unrelated to the code
-- you\'re currently writing. That is, using this function is probably a bad
-- idea if you have two or more unrelated code sections that add tags.
--
textBufferRemoveAllTags :: TextBufferClass self => self
 -> TextIter -- ^ @start@ - one bound of range to be untagged
 -> TextIter -- ^ @end@ - other bound of range to be untagged
 -> IO ()
textBufferRemoveAllTags self start end =
  {# call text_buffer_remove_all_tags #}
    (toTextBuffer self)
    start
    end

-- | Obtains an iterator pointing to @charOffset@ within the given line. The
-- @charOffset@ must exist, offsets off the end of the line are not allowed.
--
textBufferGetIterAtLineOffset :: TextBufferClass self => self
 -> Int      -- ^ @lineNumber@ - line number counting from 0
 -> Int      -- ^ @charOffset@ - char offset from start of line
 -> IO TextIter
textBufferGetIterAtLineOffset self lineNumber charOffset = do
  iter <- makeEmptyTextIter
  {# call unsafe text_buffer_get_iter_at_line_offset #}
    (toTextBuffer self)
    iter
    (fromIntegral lineNumber)
    (fromIntegral charOffset)
  return iter

-- | Creates an iterator pointing to a position @charOffset@ chars from the
-- start of the entire buffer. If @charOffset@ is -1 or greater than the number
-- of characters in the buffer, the end iterator is returned, that is the
-- iterator one past the last valid character in the buffer.
--
textBufferGetIterAtOffset :: TextBufferClass self => self
 -> Int      -- ^ @charOffset@ - char offset from start of buffer (counting
             -- from 0) or -1
 -> IO TextIter
textBufferGetIterAtOffset self charOffset = do
  iter <- makeEmptyTextIter
  {# call unsafe text_buffer_get_iter_at_offset #}
    (toTextBuffer self)
    iter
    (fromIntegral charOffset)
  return iter

-- | Create an iterator at a specific line.
--
textBufferGetIterAtLine :: TextBufferClass self => self
 -> Int      -- ^ @lineNumber@ - line number counting from 0
 -> IO TextIter
textBufferGetIterAtLine self lineNumber = do
  iter <- makeEmptyTextIter
  {# call unsafe text_buffer_get_iter_at_line #}
    (toTextBuffer self)
    iter
    (fromIntegral lineNumber)
  return iter


-- | Create an iterator from a mark.
--
textBufferGetIterAtMark :: (TextBufferClass self, TextMarkClass mark) => self
 -> mark     -- ^ @mark@ - a 'TextMark' in the buffer
 -> IO TextIter
textBufferGetIterAtMark self mark = do
  iter <- makeEmptyTextIter
  {# call unsafe text_buffer_get_iter_at_mark #}
    (toTextBuffer self)
    iter
    (toTextMark mark)
  return iter

-- | Create an iterator at the first position in the text buffer. This is
-- the same as using 'textBufferGetIterAtOffset' to get the iter at character
-- offset 0.
--
textBufferGetStartIter :: TextBufferClass self => self -> IO TextIter
textBufferGetStartIter self = do
  iter <- makeEmptyTextIter
  {# call unsafe text_buffer_get_start_iter #}
    (toTextBuffer self)
    iter
  return iter

-- | Returns the \"end iterator,\" one past the last valid
-- character in the text buffer. If dereferenced with
-- 'Graphics.UI.Gtk.Multiline.TextIter.textIterGetChar', the
-- end iterator has a character value of 0. The entire buffer lies in the range
-- from the first position in the buffer (call 'textBufferGetStartIter' to get
-- character position 0) to the end iterator.
--
textBufferGetEndIter :: TextBufferClass self => self -> IO TextIter
textBufferGetEndIter self = do
  iter <- makeEmptyTextIter
  {# call unsafe text_buffer_get_end_iter #}
    (toTextBuffer self)
    iter
  return iter

-- | Indicates whether the buffer has been modified since the last call to
-- 'textBufferSetModified' set the modification flag to @False@. Used for
-- example to enable a \"save\" function in a text editor.
--
-- It is often more convenient to use 'onModifiedChanged'.
--
textBufferGetModified :: TextBufferClass self => self
 -> IO Bool -- ^ returns @True@ if the buffer has been modified
textBufferGetModified self =
  liftM toBool $
  {# call unsafe text_buffer_get_modified #}
    (toTextBuffer self)

-- | Used to keep track of whether the buffer has been modified since the last
-- time it was saved. Whenever the buffer is saved to disk, call
-- @'textBufferSetModified' buffer False@. When the buffer is
-- modified, it will automatically toggled on the modified bit again. When the
-- modified bit flips, the buffer emits a 'modifiedChanged' signal.
--
textBufferSetModified :: TextBufferClass self => self -> Bool -> IO ()
textBufferSetModified self setting =
  {# call text_buffer_set_modified #}
    (toTextBuffer self)
    (fromBool setting)

-- | Deletes the range between the \"insert\" and \"selection_bound\" marks,
-- that is, the currently-selected text. If @interactive@ is @True@, the
-- editability of the selection will be considered (users can't delete
-- uneditable text).
--
textBufferDeleteSelection :: TextBufferClass self => self
 -> Bool    -- ^ @interactive@ - whether the deletion is caused by user
            -- interaction
 -> Bool    -- ^ @defaultEditable@ - whether the buffer is editable by default
 -> IO Bool -- ^ returns whether there was a non-empty selection to delete
textBufferDeleteSelection self interactive defaultEditable =
  liftM toBool $
  {# call text_buffer_delete_selection #}
    (toTextBuffer self)
    (fromBool interactive)
    (fromBool defaultEditable)

-- | Check if a selection exists.
--
textBufferHasSelection :: TextBufferClass self => self -> IO Bool
textBufferHasSelection self =
  liftM toBool $
  {# call unsafe text_buffer_get_selection_bounds #}
    (toTextBuffer self)
    (TextIter nullForeignPtr)
    (TextIter nullForeignPtr)

-- | Returns the bounds of the selection (if the selection has length 0, then
-- @start@ and @end@ will be the same). @start@ and @end@ will be in ascending
-- order.
--
textBufferGetSelectionBounds :: TextBufferClass self => self
 -> IO (TextIter, TextIter) -- ^ @(start, end)@ returns the selection start and
                            -- end iterators
textBufferGetSelectionBounds self = do
  start <- makeEmptyTextIter
  end <- makeEmptyTextIter
  {# call unsafe text_buffer_get_selection_bounds #}
    (toTextBuffer self)
    start
    end
  return (start, end)

-- | Called to indicate that the buffer operations between here and a call to
-- 'textBufferEndUserAction' are part of a single user-visible operation. The
-- operations between 'textBufferBeginUserAction' and 'textBufferEndUserAction'
-- can then be grouped when creating an undo stack. 'TextBuffer' maintains a
-- count of calls to 'textBufferBeginUserAction' that have not been closed with
-- a call to 'textBufferEndUserAction', and emits the 'beginUserAction' and
-- 'endUserAction' signals only for the outermost pair of calls. This
-- allows you to build user actions from other user actions.
--
-- The \"interactive\" buffer mutation functions, such as
-- 'textBufferInsertInteractive', automatically call begin\/end user action
-- around the buffer operations they perform, so there's no need to add extra
-- calls if you user action consists solely of a single call to one of those
-- functions.
--
textBufferBeginUserAction :: TextBufferClass self => self -> IO ()
textBufferBeginUserAction self =
  {# call text_buffer_begin_user_action #}
    (toTextBuffer self)

-- | Should be paired with a call to 'textBufferBeginUserAction'. See that
-- function for a full explanation.
--
textBufferEndUserAction :: TextBufferClass self => self -> IO ()
textBufferEndUserAction self =
  {# call text_buffer_end_user_action #}
    (toTextBuffer self)

#if GTK_CHECK_VERSION(2,6,0)
-- | Performs the appropriate action as if the user hit the delete key with
-- the cursor at the position specified by @iter@. In the normal case a single
-- character will be deleted, but when combining accents are involved, more
-- than one character can be deleted, and when precomposed character and accent
-- combinations are involved, less than one character will be deleted.
--
-- Because the buffer is modified, all outstanding iterators become invalid
-- after calling this function; however, the @iter@ will be re-initialized to
-- point to the location where text was deleted.
--
-- * Available since Gtk+ version 2.6
--
textBufferBackspace :: TextBufferClass self => self
 -> TextIter -- ^ @iter@ - a position in @buffer@
 -> Bool     -- ^ @interactive@ - whether the deletion is caused by user
             -- interaction
 -> Bool     -- ^ @defaultEditable@ - whether the buffer is editable by
             -- default
 -> IO Bool  -- ^ returns @True@ if the buffer was modified
textBufferBackspace self iter interactive defaultEditable =
  liftM toBool $
  {# call gtk_text_buffer_backspace #}
    (toTextBuffer self)
    iter
    (fromBool interactive)
    (fromBool defaultEditable)
#endif

-- | Inserts a child widget anchor into the text buffer at @iter@. The anchor
-- will be counted as one character in character counts, and when obtaining the
-- buffer contents as a string, will be represented by the Unicode \"object
-- replacement character\" @(chr 0xFFFC)@. Note that the \"slice\" variants for
-- obtaining portions of the buffer as a string include this character for
-- child anchors, but the \"text\" variants do not. e.g. see
-- 'textBufferGetSlice' and 'textBufferGetText'. Consider
-- 'textBufferCreateChildAnchor' as a more convenient alternative to this
-- function.
--
textBufferInsertChildAnchor :: TextBufferClass self => self
 -> TextIter        -- ^ @iter@ - location to insert the anchor
 -> TextChildAnchor -- ^ @anchor@ - a 'TextChildAnchor'
 -> IO ()
textBufferInsertChildAnchor self iter anchor =
  {# call gtk_text_buffer_insert_child_anchor #}
    (toTextBuffer self)
    iter
    anchor

-- | This is a convenience function which simply creates a child anchor with
-- 'Graphics.UI.Gtk.Multiline.TextView.textBufferChildAnchorNew' and inserts
-- it into the buffer with 'textBufferInsertChildAnchor'.
--
textBufferCreateChildAnchor :: TextBufferClass self => self
 -> TextIter           -- ^ @iter@ - location in the buffer
 -> IO TextChildAnchor -- ^ returns the created child anchor
textBufferCreateChildAnchor self iter =
  makeNewGObject mkTextChildAnchor $
  {# call gtk_text_buffer_create_child_anchor #}
    (toTextBuffer self)
    iter

#if GTK_CHECK_VERSION(2,4,0)
-- | This function moves the \"insert\" and \"selection_bound\" marks
-- simultaneously. If you move them in two steps with 'textBufferMoveMark', you
-- will temporarily select a region in between their old and new locations,
-- which can be pretty inefficient since the temporarily-selected region will
-- force stuff to be recalculated. This function moves them as a unit, which
-- can be optimized.
--
-- * Available since Gtk+ version 2.4
--
textBufferSelectRange :: TextBufferClass self => self
 -> TextIter -- ^ @ins@ - where to put the \"insert\" mark
 -> TextIter -- ^ @bound@ - where to put the \"selection_bound\" mark
 -> IO ()
textBufferSelectRange self ins bound =
  {# call gtk_text_buffer_select_range #}
    (toTextBuffer self)
    ins
    bound
#endif

-- | Obtains the location of @anchor@ within @buffer@.
--
textBufferGetIterAtChildAnchor :: TextBufferClass self => self
 -> TextIter        -- ^ @iter@ - an iterator to be initialized
 -> TextChildAnchor -- ^ @anchor@ - a child anchor that appears in @buffer@
 -> IO ()
textBufferGetIterAtChildAnchor self iter anchor =
  {# call gtk_text_buffer_get_iter_at_child_anchor #}
    (toTextBuffer self)
    iter
    anchor

-- | Retrieves the first and last iterators in the buffer, i.e. the entire
-- buffer lies within the range @[start,end)@.
--
textBufferGetBounds :: TextBufferClass self => self
 -> IO (TextIter, TextIter) -- ^  return the first and last iterators in the buffer
textBufferGetBounds self = do
  start <- makeEmptyTextIter
  end   <- makeEmptyTextIter
  {#call unsafe text_buffer_get_bounds #}
    (toTextBuffer self)
    start
    end
  return (start, end)

#if GTK_CHECK_VERSION(2,2,0)
-- | Pastes the contents of a clipboard at the given @location@.
-- (Note: pasting is asynchronous, that is,
-- we'll ask for the paste data and return, and at some point later
-- after the main loop runs, the paste data will be inserted.)
textBufferPasteClipboard :: TextBufferClass self => self
  -> Clipboard  -- ^ @clipboard@ -      the GtkClipboard to paste from
  -> TextIter   -- ^ @location@ -       location to insert pasted text
  -> Bool       -- ^ @defaultEditable@ -   whether the buffer is editable by default
  -> IO ()
textBufferPasteClipboard self clipboard overrideLocation defaultEditable =
  {# call gtk_text_buffer_paste_clipboard #}
    (toTextBuffer self)
    clipboard
    overrideLocation
    (fromBool defaultEditable)

-- | Pastes the contents of a clipboard at the insertion point.
-- (Note: pasting is asynchronous, that is,
-- we'll ask for the paste data and return, and at some point later
-- after the main loop runs, the paste data will be inserted.)
textBufferPasteClipboardAtCursor :: TextBufferClass self => self
  -> Clipboard  -- ^ @clipboard@ -      the GtkClipboard to paste from
  -> Bool       -- ^ @defaultEditable@ -   whether the buffer is editable by default
  -> IO ()
textBufferPasteClipboardAtCursor self clipboard defaultEditable =
  {# call gtk_text_buffer_paste_clipboard #}
    (toTextBuffer self)
    clipboard
    (TextIter nullForeignPtr)
    (fromBool defaultEditable)

-- | Copies the currently-selected text to a clipboard.
textBufferCopyClipboard :: TextBufferClass self => self
  -> Clipboard -- ^ @clipboard@ -       the GtkClipboard object to copy to
  -> IO ()
textBufferCopyClipboard self clipboard =
  {# call gtk_text_buffer_copy_clipboard #}
    (toTextBuffer self)
    clipboard

-- | Copies the currently-selected text to a clipboard,
-- then deletes said text if it's editable.
textBufferCutClipboard :: TextBufferClass self => self
  -> Clipboard  -- ^ @clipboard@ -      the GtkClipboard object to cut to
  -> Bool       -- ^ @defaultEditable@ -   whether the buffer is editable by default
  -> IO ()
textBufferCutClipboard self clipboard defaultEditable =
  {# call gtk_text_buffer_cut_clipboard #}
    (toTextBuffer self)
    clipboard
    (fromBool defaultEditable)
#endif

-- | Adds clipboard to the list of clipboards in which the selection contents of @self@ are available.
-- In most cases, @clipboard@ will be the 'Clipboard' of type 'selectionPrimary' for a view of @self@.
--
textBufferAddSelectionClipboard :: TextBufferClass self => self
 -> Clipboard  -- ^ @clipboard@ -       the 'Clipboard' object to add
 -> IO ()
textBufferAddSelectionClipboard self clipboard =
  {# call text_buffer_add_selection_clipboard #} (toTextBuffer self) clipboard

-- | Removes a 'Clipboard' added with 'textBufferAddSelectionClipboard'.
--
textBufferRemoveSelectionClipboard :: TextBufferClass self => self
 -> Clipboard  -- ^ @clipboard@ -        the 'Clipboard' object to remove
 -> IO ()
textBufferRemoveSelectionClipboard self clipboard =
  {# call text_buffer_remove_selection_clipboard #} (toTextBuffer self) clipboard

--------------------
-- Attributes

-- | Text Tag Table.
--
textBufferTagTable :: (TextBufferClass self, TextTagTableClass textTagTable)
 => ReadWriteAttr self TextTagTable textTagTable
textBufferTagTable = newAttrFromObjectProperty "tag-table"
  {# call pure unsafe gtk_text_tag_table_get_type #}

#if GTK_CHECK_VERSION(2,8,0)
-- | The text content of the buffer. Without child widgets and images, see
-- 'textBufferGetText' for more information.
--
-- Default value: \"\"
--
textBufferText :: (TextBufferClass self, GlibString string) => Attr self string
textBufferText = newAttrFromStringProperty "text"
#endif

-- | The \'modified\' property. See 'textBufferGetModified' and
-- 'textBufferSetModified'
--
textBufferModified :: TextBufferClass self => Attr self Bool
textBufferModified = newAttr
  textBufferGetModified
  textBufferSetModified

--------------------
-- Signals

-- | A 'TextTag' was applied to a region of text.
--
applyTag :: TextBufferClass self => Signal self (TextTag -> TextIter -> TextIter -> IO ())
applyTag = Signal (connect_OBJECT_BOXED_BOXED__NONE "apply-tag" mkTextIterCopy mkTextIterCopy)

-- | A new atomic user action is started.
--
-- * Together with 'endUserAction' these signals can be
--   used to build an undo stack.
--
beginUserAction :: TextBufferClass self => Signal self (IO ())
beginUserAction = Signal (connect_NONE__NONE "begin-user-action")

-- | Emitted when the contents of the buffer change.
--
bufferChanged :: TextBufferClass self => Signal self (IO ())
bufferChanged = Signal (connect_NONE__NONE "changed")

-- | A range of text is about to be deleted.
--
deleteRange :: TextBufferClass self => Signal self (TextIter -> TextIter -> IO ())
deleteRange = Signal (connect_BOXED_BOXED__NONE "delete-range" mkTextIterCopy mkTextIterCopy)

-- | An atomic action has ended.
--
-- * see 'beginUserAction'
--
endUserAction :: TextBufferClass self => Signal self (IO ())
endUserAction = Signal (connect_NONE__NONE "end-user-action")

-- | A 'Pixbuf' is inserted into the buffer.
--
-- * See note in 'bufferInsertText'.
--
insertPixbuf :: TextBufferClass self => Signal self (TextIter -> Pixbuf -> IO ())
insertPixbuf = Signal (connect_BOXED_OBJECT__NONE "insert-pixbuf" mkTextIterCopy)

-- | The 'insertChildAnchor' signal is emitted to insert a 'TextChildAnchor' in a 'TextBuffer'.
-- Insertion actually occurs in the default handler.
--
-- * See note in 'bufferInsertText'.
--
insertChildAnchor :: TextBufferClass self => Signal self (TextIter -> TextChildAnchor -> IO ())
insertChildAnchor = Signal (connect_BOXED_OBJECT__NONE "insert-child-anchor" mkTextIterCopy)

-- | Some text is inserted. Insertion actually occurs in the default handler.
--
-- * The function connected to this handler may not modify the buffer since
--   this would invalidate the iterator. If this function replaces the
--   default handler, it needs to stop the emission of this signal in order
--   to prevent the default handler from running. If additional text should
--   be inserted, this can be done using the 'after' function to connect.
--
bufferInsertText :: (TextBufferClass self, GlibString string) => Signal self (TextIter -> string -> IO ())
bufferInsertText = Signal $ \after obj handler ->
  connect_BOXED_PTR_INT__NONE "insert-text" mkTextIterCopy after obj
  (\iter strPtr strLen -> peekUTFStringLen (strPtr, strLen) >>= handler iter)

-- | A 'TextMark' within the buffer was deleted.
--
markDeleted :: TextBufferClass self => Signal self (TextMark -> IO ())
markDeleted = Signal (connect_OBJECT__NONE "mark-deleted")

-- | A 'TextMark' was inserted into the buffer.
--
markSet :: TextBufferClass self => Signal self (TextIter -> TextMark -> IO ())
markSet = Signal (connect_BOXED_OBJECT__NONE "mark-set" mkTextIterCopy)

modifiedChanged :: TextBufferClass self => Signal self (IO ())
modifiedChanged = Signal (connect_NONE__NONE "modified-changed")

-- | The 'pasteDone' signal is emitted after paste operation has been completed.
-- This is useful to properly scroll the view to the end of the pasted text.
-- See 'textBufferPasteClipboard' for more details.
pasteDone :: TextBufferClass self => Signal self (Clipboard -> IO ())
pasteDone = Signal (connect_OBJECT__NONE "paste-done")

-- | The textbuffer has changed.
--
removeTag :: TextBufferClass self => Signal self (TextTag -> TextIter -> TextIter -> IO ())
removeTag = Signal (connect_OBJECT_BOXED_BOXED__NONE "remove-tag" mkTextIterCopy mkTextIterCopy)

--------------------
-- Deprecated Signals and Events

#ifndef DISABLE_DEPRECATED

-- | A 'TextTag' was applied to a region of text.
--
onApplyTag, afterApplyTag :: TextBufferClass self => self
 -> (TextTag -> TextIter -> TextIter -> IO ())
 -> IO (ConnectId self)
onApplyTag = connect_OBJECT_BOXED_BOXED__NONE "apply-tag"
  mkTextIterCopy mkTextIterCopy False
afterApplyTag = connect_OBJECT_BOXED_BOXED__NONE "apply-tag"
  mkTextIterCopy mkTextIterCopy True

-- | A new atomic user action is started.
--
-- * Together with 'onEndUserAction' these signals can be
--   used to build an undo stack.
--
onBeginUserAction, afterBeginUserAction :: TextBufferClass self => self
 -> IO ()
 -> IO (ConnectId self)
onBeginUserAction = connect_NONE__NONE "begin-user-action" False
afterBeginUserAction = connect_NONE__NONE "begin-user-action" True

--- renamed from Changed to BufferChanged, since the former conflicts with TreeSelection
-- | Emitted when the contents of the buffer change.
--
onBufferChanged, afterBufferChanged :: TextBufferClass self => self
 -> IO ()
 -> IO (ConnectId self)
onBufferChanged = connect_NONE__NONE "changed" False
afterBufferChanged = connect_NONE__NONE "changed" True

-- | A range of text is about to be deleted.
--
onDeleteRange, afterDeleteRange :: TextBufferClass self => self
 -> (TextIter -> TextIter -> IO ())
 -> IO (ConnectId self)
onDeleteRange = connect_BOXED_BOXED__NONE "delete-range"
  mkTextIterCopy mkTextIterCopy False
afterDeleteRange = connect_BOXED_BOXED__NONE "delete-range"
  mkTextIterCopy mkTextIterCopy True

-- | An atomic action has ended.
--
-- * see 'onBeginUserAction'
--
onEndUserAction, afterEndUserAction :: TextBufferClass self => self
 -> IO ()
 -> IO (ConnectId self)
onEndUserAction = connect_NONE__NONE "end-user-action" False
afterEndUserAction = connect_NONE__NONE "end-user-action" True

-- | A 'Pixbuf' is inserted into the
-- buffer.
--
onInsertPixbuf, afterInsertPixbuf :: TextBufferClass self => self
 -> (TextIter -> Pixbuf -> IO ())
 -> IO (ConnectId self)
onInsertPixbuf = connect_BOXED_OBJECT__NONE "insert-pixbuf" mkTextIterCopy False
afterInsertPixbuf = connect_BOXED_OBJECT__NONE "insert-pixbuf" mkTextIterCopy True

-- | Some text was inserted.
--
onBufferInsertText, afterBufferInsertText :: (TextBufferClass self, GlibString string) => self
 -> (TextIter -> string -> IO ())
 -> IO (ConnectId self)
onBufferInsertText self user =
  connect_BOXED_PTR_INT__NONE "insert-text" mkTextIterCopy False self $
    \iter strP strLen -> do
      str <- peekUTFStringLen (strP,strLen)
      user iter str
afterBufferInsertText self user =
  connect_BOXED_PTR_INT__NONE "insert-text" mkTextIterCopy True self $
    \iter strP strLen -> do
      str <- peekUTFStringLen (strP,strLen)
      user iter str

-- | A 'TextMark' within the buffer was deleted.
--
onMarkDeleted, afterMarkDeleted :: TextBufferClass self => self
 -> (TextMark -> IO ())
 -> IO (ConnectId self)
onMarkDeleted = connect_OBJECT__NONE "mark-deleted" False
afterMarkDeleted = connect_OBJECT__NONE "mark-deleted" True

-- | A 'TextMark' was inserted into the buffer.
--
onMarkSet, afterMarkSet :: TextBufferClass self => self ->
                           (TextIter -> TextMark -> IO ()) ->
                           IO (ConnectId self)
onMarkSet = connect_BOXED_OBJECT__NONE "mark-set" mkTextIterCopy False
afterMarkSet = connect_BOXED_OBJECT__NONE "mark-set" mkTextIterCopy True

-- | The textbuffer has changed.
--
onModifiedChanged, afterModifiedChanged :: TextBufferClass self => self
 -> IO ()
 -> IO (ConnectId self)
onModifiedChanged = connect_NONE__NONE "modified-changed" False
afterModifiedChanged = connect_NONE__NONE "modified-changed" True

-- | A 'TextTag' was removed.
--
onRemoveTag, afterRemoveTag :: TextBufferClass self => self
 -> (TextTag -> TextIter -> TextIter -> IO ())
 -> IO (ConnectId self)
onRemoveTag = connect_OBJECT_BOXED_BOXED__NONE "remove-tag"
  mkTextIterCopy mkTextIterCopy False
afterRemoveTag = connect_OBJECT_BOXED_BOXED__NONE "remove-tag"
  mkTextIterCopy mkTextIterCopy True

#endif
