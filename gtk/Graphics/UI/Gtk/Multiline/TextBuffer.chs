-- -*-haskell-*-
--  GIMP Toolkit (GTK) TextBuffer
--
--  Author : Axel Simon
--
--  Created: 23 February 2002
--
--  Version $Revision: 1.4 $ from $Date: 2005/02/25 22:53:42 $
--
--  Copyright (C) 2001-2005 Axel Simon
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
-- The functionality of inserting widgets (child anchors) is not implemented
--   since there will probably some changes before the final release. The
--   following functions are not bound:
--     gtk_text_buffer_insert_child_anchor
--     gtk_text_buffer_create_child_anchor
--     gtk_text_buffer_get_iter_at_anchor
--     connectToInsertChildAnchor
--     
-- Check 'textBufferGetInsert', in case there is no cursor in 
--   the editor,
--   is there a mark called \"insert\"? If not, the function needs to return
--   Maybe TextMark. The same holds for 
--   'textBufferGetSelectionBound'.
--
-- If Clipboards are bound, then these functions need to be bound as well:
--     gtk_text_buffer_paste_clipboard
--     gtk_text_buffer_copy_clipboard
--     gtk_text_buffer_cut_clipboard
--     gtk_text_buffer_add_selection_clipboard
--     gtk_text_buffer_remove_selection_clipboard
--
-- NOTES
--
-- The following convenience functions are omitted: 
--     gtk_text_buffer_insert_with_tags
--     gtk_text_buffer_insert_with_tags_by_name
--     gtk_text_buffer_create_tag
--     gtk_text_buffer_get_bounds
--     gtk_text_buffer_get_selection_bounds
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
-- * Description
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
  castToTextBuffer,

-- * Constructors
  textBufferNew,

-- * Methods
  textBufferGetLineCount,
  textBufferGetCharCount,
  textBufferGetTagTable,
  textBufferInsert,
  textBufferInsertAtCursor,
  textBufferInsertInteractive,
  textBufferInsertInteractiveAtCursor,
  textBufferInsertRange,
  textBufferInsertRangeInteractive,
  textBufferDelete,
  textBufferDeleteInteractive,
  textBufferSetText,
  textBufferGetText,
  textBufferGetSlice,
  textBufferInsertPixbuf,
  textBufferCreateMark,
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
  textBufferBeginUserAction,
  textBufferEndUserAction,

-- * Signals
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
  onInsertText,
  afterInsertText,
  onMarkDeleted,
  afterMarkDeleted,
  onMarkSet,
  afterMarkSet,
  onModifiedChanged,
  afterModifiedChanged,
  onRemoveTag,
  afterRemoveTag
  ) where

import Monad	(liftM)
import Maybe	(fromMaybe)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject			(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.Multiline.TextIter#}
import Graphics.UI.Gtk.Multiline.TextMark	(TextMark, MarkName)
import Graphics.UI.Gtk.Multiline.TextTag	(TextTag, TagName)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new text buffer, possibly taking a
-- table of 'TextTag'.
--
textBufferNew :: Maybe TextTagTable -> IO TextBuffer
textBufferNew tt = makeNewGObject mkTextBuffer $ liftM castPtr $
  {#call unsafe text_buffer_new#} 
  (fromMaybe (mkTextTagTable nullForeignPtr) tt)

--------------------
-- Methods

-- | Obtain the number of lines in the buffer.
--
textBufferGetLineCount :: TextBufferClass tb => tb -> IO Int
textBufferGetLineCount tb = liftM fromIntegral $ 
  {#call unsafe text_buffer_get_line_count#} (toTextBuffer tb)

-- | Obtain the number of characters in the
-- buffer.
--
-- * Note that the comment in the Gtk+ documentation about bytes and chars
--   does not hold because Haskell uses 31-bit characters and not UTF8.
--
textBufferGetCharCount :: TextBufferClass tb => tb -> IO Int
textBufferGetCharCount tb = liftM fromIntegral $
  {#call unsafe text_buffer_get_char_count#} (toTextBuffer tb)

-- | Extract the tag table that is associated
-- with this text buffer.
--
textBufferGetTagTable :: TextBufferClass tb => tb -> IO TextTagTable
textBufferGetTagTable tb = makeNewGObject mkTextTagTable $ liftM castPtr $
  {#call unsafe text_buffer_get_tag_table#} (toTextBuffer tb)

-- | Insert text at the position specified by the
-- 'TextIter'.
--
textBufferInsert :: TextBufferClass tb => tb -> TextIter -> String -> IO ()
textBufferInsert tb iter str = withUTFStringLen str $ \(cStr, len) ->
  {#call text_buffer_insert#} (toTextBuffer tb) iter cStr (fromIntegral len)

-- | Insert text at the cursor.
--
textBufferInsertAtCursor :: TextBufferClass tb => tb -> String -> IO ()
textBufferInsertAtCursor tb str = withUTFStringLen str $ \(cStr, len) ->
  {#call text_buffer_insert_at_cursor#} (toTextBuffer tb) cStr
    (fromIntegral len)

-- | Insert text at the 'TextIter'
-- only if a normal user would be able to do so as well.
--
-- * Insert the text obeying special editable or non-editable Tags.
--
-- * If no tag is at the specified position, use the default value
--   @def@ to decide if the text should be inserted. This value could
--   be set to the result of 'textViewGetEditable'.
--
textBufferInsertInteractive :: TextBufferClass tb => tb -> TextIter ->
						     String -> Bool ->
                               			     IO Bool
textBufferInsertInteractive tb iter str def = withUTFStringLen str $ 
  \(cStr, len) -> liftM toBool $ {#call text_buffer_insert_interactive#} 
    (toTextBuffer tb) iter cStr (fromIntegral len) (fromBool def)

-- | Insert text at cursor only if
-- a normal user would be able to do so as well.
--
textBufferInsertInteractiveAtCursor :: TextBufferClass tb => tb -> String ->
							     Bool -> IO Bool
textBufferInsertInteractiveAtCursor tb str def = withUTFStringLen str $ 
  \(cStr, len) -> liftM toBool $ 
  {#call text_buffer_insert_interactive_at_cursor #} (toTextBuffer tb) cStr 
    (fromIntegral len) (fromBool def)

-- | Copy text between the two
-- 'TextIter' @start@ and @end@ to another location
-- @ins@.
--

--
textBufferInsertRange :: TextBufferClass tb => tb -> TextIter -> TextIter ->
					       TextIter -> IO ()
textBufferInsertRange tb ins start end = {#call text_buffer_insert_range#}
  (toTextBuffer tb) ins start end

-- | Copy text as
-- 'textBufferInsertRange' does, but obey editable and non-editable
-- tags.
--
-- * Insert the text obeying special editable or non-editable Tags.
--
-- * If no tag is at the specified position, use the default value
--   @def@ to decide if the text should be inserted. This value could
--   be set to the result of 'textViewGetEditable'.
--
textBufferInsertRangeInteractive :: TextBufferClass tb => tb -> TextIter ->
				    TextIter -> TextIter -> Bool -> IO Bool
textBufferInsertRangeInteractive tb ins start end def = liftM toBool $
  {#call text_buffer_insert_range_interactive#} (toTextBuffer tb) ins start
    end (fromBool def)


-- | Delete some text.
--
textBufferDelete :: TextBufferClass tb => tb -> TextIter -> TextIter -> IO ()
textBufferDelete tb start end = {#call text_buffer_delete#} (toTextBuffer tb)
				start end

-- | Delete some text but obey editable and
-- non-editable tags.
--
textBufferDeleteInteractive :: TextBufferClass tb => tb -> TextIter ->
						     TextIter -> Bool ->
                               			     IO Bool
textBufferDeleteInteractive tb start end def = liftM toBool $
  {#call text_buffer_delete_interactive#} (toTextBuffer tb) start end
    (fromBool def)

-- | Replace the text in the current
-- 'TextBuffer'.
--
textBufferSetText :: TextBufferClass tb => tb -> String -> IO ()
textBufferSetText tb str = withUTFStringLen str $ \(cStr, len) ->
  {#call text_buffer_set_text#} (toTextBuffer tb) cStr (fromIntegral len)

-- | Extract all the text between @start@ and
-- @end@ from a 'TextBuffer'.
--
-- * The @start@ position is included, @end@ is not.
--
-- * If @incl@ is True, text tagged with the invisible attribute is
--   also returned.
--
-- * Characters representing embedded images are not included. (So offsets
--   within the returned text are different from the Buffer itself.)
--
textBufferGetText :: TextBufferClass tb => tb -> TextIter -> TextIter -> 
					   Bool -> IO String
textBufferGetText tb start end incl = {#call unsafe text_buffer_get_text#} 
  (toTextBuffer tb) start end (fromBool incl) >>= peekUTFString

-- | Extract text and special characters between
-- @start@ and @end@.
--
-- * As opposed to 'textBufferGetText', this function returns 
--   @(chr 0xFFFC)@ for images, so offsets within the returned 
--   string correspond to offsets in the 'TextBuffer'. Note the
--   @(chr 0xFFFC)@ can occur in normal text without images as well.
--
textBufferGetSlice :: TextBufferClass tb => tb -> TextIter -> TextIter ->
					    Bool -> IO String
textBufferGetSlice tb start end incl = {#call unsafe text_buffer_get_slice#}
  (toTextBuffer tb) start end (fromBool incl) >>= peekUTFString

-- | Insert an image into the
-- 'TextBuffer'.
--
-- * See 'textBufferGetSlice' and 'textBufferGetText'.
--
textBufferInsertPixbuf :: TextBufferClass tb => tb -> TextIter -> Pixbuf ->
						IO ()
textBufferInsertPixbuf tb pos img = 
  {#call text_buffer_insert_pixbuf#} (toTextBuffer tb) pos img

-- | Create a 'TextMark' from an
-- iterator.
--
-- * Pass @Nothing@ as mark name for an anonymous 
--   'TextMark'.
--
-- * Set @gravity@ to True if the mark should keep left.
--
textBufferCreateMark :: TextBufferClass tb => tb -> Maybe MarkName -> 
					      TextIter -> Bool ->
                        		      IO TextMark
textBufferCreateMark tb Nothing iter gravity = makeNewGObject mkTextMark $
  {#call unsafe text_buffer_create_mark#} (toTextBuffer tb) nullPtr iter
    (fromBool gravity)
textBufferCreateMark tb (Just name) iter gravity = 
  makeNewGObject mkTextMark $ withUTFString name $ \cStr ->
  {#call unsafe text_buffer_create_mark#} (toTextBuffer tb) cStr iter
    (fromBool gravity)

-- | Move a mark.
--
-- * Emits \"mark_set\".
--
textBufferMoveMark :: TextBufferClass tb => tb -> TextMark -> TextIter -> IO ()
textBufferMoveMark tb tm iter =
  {#call text_buffer_move_mark#} (toTextBuffer tb) tm iter

-- | Move a named mark.
--
-- * The mark should exist (otherwise a nasty warning is generated).
--
textBufferMoveMarkByName :: TextBufferClass tb => tb -> MarkName ->
						  TextIter -> IO ()
textBufferMoveMarkByName tb name iter = withUTFString name $ \cStr ->
  {#call text_buffer_move_mark_by_name#} (toTextBuffer tb) cStr iter

-- | Delete a mark.
--
-- * This renders the 'TextMark' @tm@ unusable forever.
--
textBufferDeleteMark :: TextBufferClass tb => tb -> TextMark -> IO ()
textBufferDeleteMark tb tm =
  {#call text_buffer_delete_mark#} (toTextBuffer tb) tm

-- | Delete a mark by name.
--
-- * The mark should exist (otherwise a nasty warning is generated).
--
textBufferDeleteMarkByName :: TextBufferClass tb => tb -> MarkName -> IO ()
textBufferDeleteMarkByName tb name = withUTFString name $ \cStr ->
  {#call text_buffer_delete_mark_by_name#} (toTextBuffer tb) cStr

-- | Retrieve a 'TextMark' by name.
--
textBufferGetMark :: TextBufferClass tb => tb -> MarkName ->
					   IO (Maybe TextMark)
textBufferGetMark tb name = do
  tm <- withUTFString name $ \cStr -> 
    {#call unsafe text_buffer_get_mark#} (toTextBuffer tb) cStr
  if tm==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkTextMark (return tm)

-- | Get the current cursor position.
--
-- * This is equivalent to liftM unJust $ textBufferGetMark \"insert\"
--
textBufferGetInsert :: TextBufferClass tb => tb -> IO TextMark
textBufferGetInsert tb = makeNewGObject mkTextMark $
  {#call unsafe text_buffer_get_insert#} (toTextBuffer tb)

-- | Get a 'TextMark' for the
-- other side of a selection.
--
textBufferGetSelectionBound :: TextBufferClass tb => tb -> IO TextMark
textBufferGetSelectionBound tb = makeNewGObject mkTextMark $
  {#call unsafe text_buffer_get_selection_bound#} (toTextBuffer tb)

-- | Place the cursor.
--
-- * This is faster than moving the \"insert\" and the \"selection_bound\" marks
--   in sequence since it avoids generating a transient selection.
--
textBufferPlaceCursor :: TextBufferClass tb => tb -> TextIter -> IO ()
textBufferPlaceCursor tb iter =
  {#call text_buffer_place_cursor#} (toTextBuffer tb) iter

-- | Tag a range of text.
--
textBufferApplyTag :: TextBufferClass tb => tb -> TextTag -> TextIter ->
						  TextIter -> IO ()
textBufferApplyTag tb tag start end = 
  {#call text_buffer_apply_tag#} (toTextBuffer tb) tag start end

-- | Remove a tag from a range of text.
--
textBufferRemoveTag :: TextBufferClass tb => tb -> TextTag -> TextIter ->
					     TextIter -> IO ()
textBufferRemoveTag tb tag start end =
  {#call text_buffer_remove_tag#} (toTextBuffer tb) tag start end

-- | Apply a tag that is specified by name.
--
textBufferApplyTagByName :: TextBufferClass tb => tb -> TagName ->
		      				  TextIter -> TextIter ->
                            			  IO () 
textBufferApplyTagByName tb tname start end = withUTFString tname $ \cStr ->
  {#call text_buffer_apply_tag_by_name#} (toTextBuffer tb) cStr start end

-- | Remove a tag from a range of text.
--
textBufferRemoveTagByName :: TextBufferClass tb => tb -> TagName -> 
						   TextIter -> TextIter ->
                             			   IO ()
textBufferRemoveTagByName tb tname start end = withUTFString tname $ \cStr ->
  {#call text_buffer_remove_tag_by_name#} (toTextBuffer tb) cStr start end

-- | Remove all tags within a range.
-- 
-- * Be careful with this function; it could remove tags added in code
--   unrelated to the code you're currently writing. That is, using this
--   function is probably a bad idea if you have two or more unrelated code
--   sections that add tags.
--

--
textBufferRemoveAllTags :: TextBufferClass tb => tb -> TextIter -> 
						 TextIter -> IO ()
textBufferRemoveAllTags tb start end =
  {#call text_buffer_remove_all_tags#} (toTextBuffer tb) start end

-- | Create an iterator at a specific
-- line and offset.
--
-- * The @line@ and @offset@ arguments must be valid.
--
textBufferGetIterAtLineOffset :: TextBufferClass tb => tb -> Int -> Int -> 
						       IO TextIter
textBufferGetIterAtLineOffset tb line offset = do
  iter <- makeEmptyTextIter
  {#call unsafe text_buffer_get_iter_at_line_offset#}
    (toTextBuffer tb) iter (fromIntegral line) (fromIntegral offset)
  return iter

-- | Create an iterator at a specific offset.
--
-- * The @offset@ arguments must be valid, starting from the first
--   character in the buffer.
--
textBufferGetIterAtOffset :: TextBufferClass tb => tb -> Int -> IO TextIter
textBufferGetIterAtOffset tb offset = do
  iter <- makeEmptyTextIter
  {#call unsafe text_buffer_get_iter_at_offset#}
    (toTextBuffer tb) iter (fromIntegral offset)
  return iter
  
-- | Create an iterator at a specific line.
--
-- * The @line@ arguments must be valid.
--
textBufferGetIterAtLine :: TextBufferClass tb => Int -> tb -> IO TextIter
textBufferGetIterAtLine line tb = do
  iter <- makeEmptyTextIter
  {#call unsafe text_buffer_get_iter_at_line#}
    (toTextBuffer tb) iter (fromIntegral line)
  return iter


-- | Create an iterator from a mark.
--
textBufferGetIterAtMark :: TextBufferClass tb => tb -> TextMark -> IO TextIter
textBufferGetIterAtMark tb tm = do
  iter <- makeEmptyTextIter
  {#call unsafe text_buffer_get_iter_at_mark#} (toTextBuffer tb) iter tm
  return iter


-- | Create an iterator at the beginning of the
-- buffer.
--
textBufferGetStartIter :: TextBufferClass tb => tb -> IO TextIter
textBufferGetStartIter tb = do
  iter <- makeEmptyTextIter
  {#call unsafe text_buffer_get_start_iter#} (toTextBuffer tb) iter
  return iter

-- | Create an iterator at the end of the buffer.
--
-- * The iterator represents the position after the last character in the
--   buffer.
--
textBufferGetEndIter :: TextBufferClass tb => tb -> IO TextIter
textBufferGetEndIter tb = do
  iter <- makeEmptyTextIter
  {#call unsafe text_buffer_get_end_iter#} (toTextBuffer tb) iter
  return iter


-- | Query if the buffer was modified.
--
-- * This flag is reset by calling 'textBufferSetModified'.
--
-- * It is usually more convenient to use
--   @\"connectToModifiedChanged\"@.
--
textBufferGetModified :: TextBufferClass tb => tb -> IO Bool
textBufferGetModified tb = liftM toBool $
  {#call unsafe text_buffer_get_modified#} (toTextBuffer tb)

-- | Set the \"buffer-is-modified\" flag.
--
textBufferSetModified :: TextBufferClass tb => tb -> Bool -> IO ()
textBufferSetModified tb isModified =
  {#call text_buffer_set_modified#} (toTextBuffer tb) (fromBool isModified)

-- | Delete the current selection.
--
-- * The @interactive@ flag determines if this function is invoked on
--   behalf of the user (i.e. if we honour editable\/non-editable tags).
--
-- * See 'textBufferInsertAtCursor' for information on
--   @def@.
--
-- * The function returns True if a non-empty selection was deleted.
--
textBufferDeleteSelection :: TextBufferClass tb => tb -> Bool -> Bool ->
						   IO Bool
textBufferDeleteSelection tb interactive def = liftM toBool $
  {#call text_buffer_delete_selection#} (toTextBuffer tb)
  (fromBool interactive) (fromBool def)

-- | Check if a selection exists.
--
textBufferHasSelection :: TextBufferClass tb => tb -> IO Bool
textBufferHasSelection tb = liftM toBool $
  {#call unsafe text_buffer_get_selection_bounds#} 
  (toTextBuffer tb) (TextIter nullForeignPtr) (TextIter nullForeignPtr)

-- | Start a new atomic user action.
--
-- * Called to indicate that the buffer operations between here and a call to
--   'textBufferEndUserAction' are part of a single user-visible
--   operation. The operations between 'textBufferBeginUserAction'
--   and 'textBufferEndUserAction' can then be grouped when
--   creating an undo stack. 'TextBuffer' objects maintains a count
--   of calls to 'textBufferBeginUserAction' that have not been
--   closed with a call to 'textBufferEndUserAction', and emits the
--   \"begin_user_action\" and \"end_user_action\" signals only for the outermost
--   pair of calls. This allows you to build user actions from other user
--   actions. The \"interactive\" buffer mutation functions, such as
--   'textBufferInsertInteractive', automatically call begin\/end
--   user action around the buffer operations they perform, so there's no need
--   to add extra calls if you user action consists solely of a single call to
--   one of those functions.
--
textBufferBeginUserAction :: TextBufferClass tb => tb -> IO ()
textBufferBeginUserAction  = {#call text_buffer_begin_user_action#} .
			     toTextBuffer

-- | End an atomic user action.
--
textBufferEndUserAction :: TextBufferClass tb => tb -> IO ()
textBufferEndUserAction  = {#call text_buffer_end_user_action#} .
			   toTextBuffer

--------------------
-- Signals

-- | A 'TextTag' was applied to a region of
-- text.
--
onApplyTag, afterApplyTag :: TextBufferClass tb => tb ->
                             (TextTag -> TextIter -> TextIter -> IO ()) ->
                             IO (ConnectId tb)
onApplyTag = connect_OBJECT_BOXED_BOXED__NONE "apply-tag" 
  mkTextIter mkTextIter False
afterApplyTag = connect_OBJECT_BOXED_BOXED__NONE "apply-tag" 
  mkTextIter mkTextIter True

-- | A new atomic user action is started.
--
-- * Together with 'connectToEndUserAction' these signals can be
--   used to build an undo stack.
--
onBeginUserAction, afterBeginUserAction :: TextBufferClass tb => tb -> IO () ->
                                           IO (ConnectId tb)
onBeginUserAction = connect_NONE__NONE "begin_user_action" False
afterBeginUserAction = connect_NONE__NONE "begin_user_action" True

--- renamed from Changed to BufferChanged, since the former conflicts with TreeSelection
-- | Emitted when the contents of the buffer change.
--
onBufferChanged, afterBufferChanged :: TextBufferClass tb => tb -> IO () ->
                           IO (ConnectId tb)
onBufferChanged = connect_NONE__NONE "changed" False
afterBufferChanged = connect_NONE__NONE "changed" True

-- | A range of text is about to be deleted.
--
onDeleteRange, afterDeleteRange :: TextBufferClass tb => tb ->
                                   (TextIter -> TextIter -> IO ()) ->
                                   IO (ConnectId tb)
onDeleteRange = connect_BOXED_BOXED__NONE "delete_range"
  mkTextIter mkTextIter False
afterDeleteRange = connect_BOXED_BOXED__NONE "delete_range"
  mkTextIter mkTextIter True

-- | An atomic action has ended.
--
-- * see 'connectToBeginUserAction'
--
onEndUserAction, afterEndUserAction :: TextBufferClass tb => tb -> IO () ->
                                       IO (ConnectId tb)
onEndUserAction = connect_NONE__NONE "end_user_action" False
afterEndUserAction = connect_NONE__NONE "end_user_action" True

-- | A widgets is inserted into the buffer.
--connectToInsertChildAnchor :: TextBufferClass tb =>
-- (TextIter -> TextChildAnchor -> IO ()) -> ConnectAfter -> tb -> 
--  IO (ConnectId tb)
--connectToInsertChildAnchor = connect_BOXED_OBJECT__NONE "insert_child_anchor"
--  mkTextIter

-- | A 'Pixbuf' is inserted into the
-- buffer.
--
onInsertPixbuf, afterInsertPixbuf :: TextBufferClass tb => tb ->
                                     (TextIter -> Pixbuf -> IO ()) ->
                                     IO (ConnectId tb)
onInsertPixbuf = connect_BOXED_OBJECT__NONE "insert_pixbuf" mkTextIter False
afterInsertPixbuf = connect_BOXED_OBJECT__NONE "insert_pixbuf" mkTextIter True

-- | Some text was inserted.
--
onInsertText, afterInsertText :: TextBufferClass tb => tb ->
                                 (TextIter -> String -> IO ()) ->
                                 IO (ConnectId tb)
onInsertText tb user = 
  connect_BOXED_PTR_INT__NONE "insert_text" mkTextIter False tb $
    \iter strP strLen -> do
      str <- peekUTFStringLen (strP,strLen)
      user iter str 
afterInsertText tb user = 
  connect_BOXED_PTR_INT__NONE "insert_text" mkTextIter True tb $
    \iter strP strLen -> do
      str <- peekUTFStringLen (strP,strLen)
      user iter str 

-- | A 'TextMark' within the buffer was
-- deleted.
--
onMarkDeleted, afterMarkDeleted :: TextBufferClass tb => tb ->
                                   (TextMark -> IO ()) -> IO (ConnectId tb)
onMarkDeleted = connect_OBJECT__NONE "mark_deleted" False
afterMarkDeleted = connect_OBJECT__NONE "mark_deleted" True

-- | A 'TextMark' was inserted into the
-- buffer.
--
onMarkSet, afterMarkSet :: TextBufferClass tb => tb ->
                           (TextIter -> TextMark -> IO ()) ->
                           IO (ConnectId tb)
onMarkSet = connect_BOXED_OBJECT__NONE "mark_set" mkTextIter False
afterMarkSet = connect_BOXED_OBJECT__NONE "mark_set" mkTextIter True

-- | The textbuffer has changed.
--
onModifiedChanged, afterModifiedChanged :: TextBufferClass tb => tb -> IO () ->
                                           IO (ConnectId tb)
onModifiedChanged = connect_NONE__NONE "modified_changed" False
afterModifiedChanged = connect_NONE__NONE "modified_changed" True

-- | A 'TextTag' was removed.
--
onRemoveTag, afterRemoveTag :: TextBufferClass tb => tb ->
                               (TextTag -> TextIter -> TextIter -> IO ()) ->
                               IO (ConnectId tb)
onRemoveTag = connect_OBJECT_BOXED_BOXED__NONE "remove_tag" 
  mkTextIter mkTextIter False
afterRemoveTag = connect_OBJECT_BOXED_BOXED__NONE "remove_tag" 
  mkTextIter mkTextIter True

