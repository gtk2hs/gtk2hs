-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry TextBuffer@
--
--  Author : Axel Simon
--          
--  Created: 23 February 2002
--
--  Version $Revision: 1.4 $ from $Date: 2002/08/05 16:41:34 $
--
--  Copyright (c) [2001..2002] Axel Simon
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
-- * This storage object holds text to be displayed by one or more 
--   @ref data TextView@ widgets.
--
-- @documentation@ ------------------------------------------------------------
--
-- * See "Text Widget Overview" in the Gtk+ docs.
--
-- * The following convenience functions are omitted: 
--     gtk_text_buffer_insert_with_tags
--     gtk_text_buffer_insert_with_tags_by_name
--     gtk_text_buffer_create_tag
--     gtk_text_buffer_get_bounds
--     gtk_text_buffer_get_selection_bounds
--
-- * The following functions do not make sense due to Haskell's wide character
--   representation of Unicode:
--     gtk_text_buffer_get_iter_at_line_index
--
-- * The function gtk_text_buffer_get_selection_bounds is only used to test
--   if there is a selection  (see @ref method textBufferHasSelection@).
--
-- @todo@ ---------------------------------------------------------------------
--
-- * The functionality of inserting widgets (child anchors) is not implemented
--   since there will probably some changes before the final release. The
--   following functions are not bound:
--     gtk_text_buffer_insert_child_anchor
--     gtk_text_buffer_create_child_anchor
--     gtk_text_buffer_get_iter_at_anchor
--     connectToInsertChildAnchor
--     
-- * Check @ref method textBufferGetInsert@, in case there is no cursor in 
--   the editor,
--   is there a mark called "insert"? If not, the function needs to return
--   Maybe TextMark. The same holds for 
--   @ref method textBufferGetSelectionBound@.
--
-- * If Clipboards are bound, then these functions need to be bound as well:
--     gtk_text_buffer_paste_clipboard
--     gtk_text_buffer_copy_clipboard
--     gtk_text_buffer_cut_clipboard
--     gtk_text_buffer_add_selection_clipboard
--     gtk_text_buffer_remove_selection_clipboard
--
--
module TextBuffer(
  TextBuffer,
  TextBufferClass,
  castToTextBuffer,
  textBufferNew,
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
  textBufferEndUserAction
  ) where

import Monad	(liftM)
import Maybe	(fromMaybe)
import Foreign
import UTFCForeign
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import Signal#}
{#import TextIter#}
import Structs	(nullForeignPtr)
import TextMark	(TextMark, MarkName)
import TextTag	(TextTag, TagName)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor textBufferNew@ Create a new text buffer, possibly taking a
-- table of @ref type TextTag@.
--
textBufferNew :: Maybe TextTagTable -> IO TextBuffer
textBufferNew tt = makeNewGObject mkTextBuffer $ liftM castPtr $
  {#call unsafe text_buffer_new#} 
  (fromMaybe (mkTextTagTable nullForeignPtr) tt)

-- @method textBufferGetLineCount@ Obtain the number of lines in the buffer.
--
textBufferGetLineCount :: TextBuffer -> IO Int
textBufferGetLineCount tb = liftM fromIntegral $ 
  {#call unsafe text_buffer_get_line_count#} tb

-- @method textBufferGetCharCount@ Obtain the number of characters in the
-- buffer.
--
-- * Note that the comment in the Gtk+ documentation about bytes and chars
--   does not hold because Haskell uses 31-bit characters and not UTF8.
--
textBufferGetCharCount :: TextBuffer -> IO Int
textBufferGetCharCount tb = liftM fromIntegral $
  {#call unsafe text_buffer_get_line_count#} tb

-- @method textBufferGetTagTable@ Extract the tag table that is associated
-- with this text buffer.
--
textBufferGetTagTable :: TextBuffer -> IO TextTagTable
textBufferGetTagTable tb = makeNewGObject mkTextTagTable $ liftM castPtr $
  {#call unsafe text_buffer_get_tag_table#} tb

-- @method textBufferInsert@ Insert text at the position specified by the
-- @ref type TextIter@.
--
textBufferInsert :: TextBuffer -> TextIter -> String -> IO ()
textBufferInsert tb iter str = withCStringLen str $ \(cStr, len) ->
  {#call text_buffer_insert#} tb iter cStr (fromIntegral len)

-- @method textBufferInsertAtCursor@ Insert text at the cursor.
--
textBufferInsertAtCursor :: TextBuffer -> String -> IO ()
textBufferInsertAtCursor tb str = withCStringLen str $ \(cStr, len) ->
  {#call text_buffer_insert_at_cursor#} tb cStr (fromIntegral len)

-- @method textBufferInsertInteractive@ Insert text at the @ref type TextIter@
-- only if a normal user would be able to do so as well.
--
-- * Insert the text obeying special editable or non-editable Tags.
--
-- * If no tag is at the specified position, use the default value
--   @ref arg def@ to decide if the text should be inserted. This value could
--   be set to the result of @ref method textViewGetEditable@.
--
textBufferInsertInteractive :: TextBuffer -> TextIter -> String -> Bool ->
                               IO Bool
textBufferInsertInteractive tb iter str def = withCStringLen str $ 
  \(cStr, len) -> liftM toBool $ {#call text_buffer_insert_interactive#} 
    tb iter cStr (fromIntegral len) (fromBool def)

-- @method textBufferInsertInteractiveAtCursor@ Insert text at cursor only if
-- a normal user would be able to do so as well.
--
textBufferInsertInteractiveAtCursor :: TextBuffer -> String -> Bool -> IO Bool
textBufferInsertInteractiveAtCursor tb str def = withCStringLen str $ 
  \(cStr, len) -> liftM toBool $ 
  {#call text_buffer_insert_interactive_at_cursor #} tb cStr 
    (fromIntegral len) (fromBool def)

-- @method textBufferInsertRange@ Copy text between the two
-- @ref type TextIter@ @ref arg start@ and @ref arg end@ to another location
-- @ref arg ins@.
--
-- *  @literal@
-- 

--
textBufferInsertRange :: TextBuffer -> TextIter -> TextIter -> TextIter ->
                         IO ()
textBufferInsertRange tb ins start end = {#call text_buffer_insert_range#}
  tb ins start end

-- @method textBufferInsertRangeInteractive@ Copy text as
-- @ref method textBufferInsertRange@ does, but obey editable and non-editable
-- tags.
--
-- * Insert the text obeying special editable or non-editable Tags.
--
-- * If no tag is at the specified position, use the default value
--   @ref arg def@ to decide if the text should be inserted. This value could
--   be set to the result of @ref method textViewGetEditable@.
--
textBufferInsertRangeInteractive :: TextBuffer -> TextIter -> TextIter ->
                                    TextIter -> Bool -> IO Bool
textBufferInsertRangeInteractive tb ins start end def = liftM toBool $
  {#call text_buffer_insert_range_interactive#} tb ins start end (fromBool def)


-- @method textBufferDelete@ Delete some text.
--
textBufferDelete :: TextBuffer -> TextIter -> TextIter -> IO ()
textBufferDelete tb start end = {#call text_buffer_delete#} tb start end

-- @method textBufferDeleteInteractive@ Delete some text but obey editable and
-- non-editable tags.
--
textBufferDeleteInteractive :: TextBuffer -> TextIter -> TextIter -> Bool ->
                               IO Bool
textBufferDeleteInteractive tb start end def = liftM toBool $
  {#call text_buffer_delete_interactive#} tb start end (fromBool def)

-- @method textBufferSetText@ Replace the text in the current
-- @ref type TextBuffer@.
--
textBufferSetText :: TextBuffer -> String -> IO ()
textBufferSetText tb str = withCStringLen str $ \(cStr, len) ->
  {#call text_buffer_set_text#} tb cStr (fromIntegral len)

-- @method textBufferGetText@ Extract all the text between @ref arg start@ and
-- @ref arg end@ from a @ref type TextBuffer@.
--
-- * The @ref arg start@ position is included, @ref arg end@ is not.
--
-- * If @ref arg incl@ is True, text tagged with the invisible attribute is
--   also returned.
--
-- * Characters representing embedded images are not included. (So offsets
--   within the returned text are different from the Buffer itself.)
--
textBufferGetText :: TextBuffer -> TextIter -> TextIter -> Bool -> IO String
textBufferGetText tb start end incl = {#call unsafe text_buffer_get_text#} 
  tb start end (fromBool incl) >>= peekCString

-- @method textBufferGetSlice@ Extract text and special characters between
-- @ref arg start@ and @ref arg end@.
--
-- * As opposed to @ref method textBufferGetText@, this function returns (chr
--   0xFFFC) for images, so offsets within the returned string correspond to
--   offsets in the @ref type TextBuffer@. Note the (chr 0xFFFC) can occur in
--   normal text without images as well.
--
textBufferGetSlice :: TextBuffer -> TextIter -> TextIter -> Bool -> IO String
textBufferGetSlice tb start end incl = {#call unsafe text_buffer_get_slice#}
  tb start end (fromBool incl) >>= peekCString

-- @method textBufferInsertPixbuf@ Insert an image into the
-- @ref type TextBuffer@.
--
-- * See @ref method textBufferGetSlice@ and @ref method textBufferGetText@.
--
textBufferInsertPixbuf :: TextBuffer -> TextIter -> GdkPixbuf -> IO ()
textBufferInsertPixbuf tb pos img = 
  {#call text_buffer_insert_pixbuf#} tb pos img

-- @method textBufferCreateMark@ Create a @ref type TextMark@ from an
-- iterator.
--
-- * Pass @ref arg Nothing@ as mark name for an anonymous @ref type TextMark@.
--
-- * Set @ref arg gravity@ to True if the mark should keep left.
--
textBufferCreateMark :: TextBuffer -> Maybe MarkName -> TextIter -> Bool ->
                        IO TextMark
textBufferCreateMark tb Nothing iter gravity = makeNewGObject mkTextMark $
  {#call unsafe text_buffer_create_mark#} tb nullPtr iter (fromBool gravity)
textBufferCreateMark tb (Just name) iter gravity = 
  makeNewGObject mkTextMark $ withCString name $ \cStr ->
  {#call unsafe text_buffer_create_mark#} tb cStr iter (fromBool gravity)

-- @method textBufferMoveMark@ Move a mark.
--
-- * Emits "mark_set".
--
textBufferMoveMark :: TextBuffer -> TextMark -> TextIter -> IO ()
textBufferMoveMark tb tm iter = {#call text_buffer_move_mark#} tb tm iter

-- @method textBufferMoveMarkByName@ Move a named mark.
--
-- * The mark should exist (otherwise a nasty warning is generated).
--
textBufferMoveMarkByName :: TextBuffer -> MarkName -> TextIter -> IO ()
textBufferMoveMarkByName tb name iter = withCString name $ \cStr ->
  {#call text_buffer_move_mark_by_name#} tb cStr iter

-- @method textBufferDeleteMark@ Delete a mark.
--
-- * This renders the @ref type TextMark@ @ref arg tm@ unusable forever.
--
textBufferDeleteMark :: TextBuffer -> TextMark -> IO ()
textBufferDeleteMark tb tm = {#call text_buffer_delete_mark#} tb tm

-- @method textBufferDeleteMarkByName@ Delete a mark by name.
--
-- * The mark should exist (otherwise a nasty warning is generated).
--
textBufferDeleteMarkByName :: TextBuffer -> MarkName -> IO ()
textBufferDeleteMarkByName tb name = withCString name $ \cStr ->
  {#call text_buffer_delete_mark_by_name#} tb cStr

-- @method textBufferGetMark@ Retrieve a @ref type TextMark@ by name.
--
textBufferGetMark :: TextBuffer -> MarkName -> IO (Maybe TextMark)
textBufferGetMark tb name = do
  tm <- withCString name $ \cStr -> 
    {#call unsafe text_buffer_get_mark#} tb cStr
  if tm==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkTextMark (return tm)

-- @method textBufferGetInsert@ Get the current cursor position.
--
-- * This is equivalent to liftM unJust $ textBufferGetMark "insert"
--
textBufferGetInsert :: TextBuffer -> IO TextMark
textBufferGetInsert tb = makeNewGObject mkTextMark $
  {#call unsafe text_buffer_get_insert#} tb

-- @method textBufferGetSelectionBound@ Get a @ref type TextMark@ for the
-- other side of a selection.
--
textBufferGetSelectionBound :: TextBuffer -> IO TextMark
textBufferGetSelectionBound tb = makeNewGObject mkTextMark $
  {#call unsafe text_buffer_get_selection_bound#} tb

-- @method textBufferPlaceCursor@ Place the cursor.
--
-- * This is faster than moving the "insert" and the "selection_bound" marks
--   in sequence since it avoids generating a transient selection.
--
textBufferPlaceCursor :: TextBuffer -> TextIter -> IO ()
textBufferPlaceCursor tb iter = {#call text_buffer_place_cursor#} tb iter

-- @method textBufferApplyTag@ Tag a range of text.
--
textBufferApplyTag :: TextBuffer -> TextTag -> TextIter -> TextIter -> IO ()
textBufferApplyTag tb tag start end = 
  {#call text_buffer_apply_tag#} tb tag start end

-- @method textBufferRemoveTag@ Remove a tag from a range of text.
--
textBufferRemoveTag :: TextBuffer -> TextTag -> TextIter -> TextIter -> IO ()
textBufferRemoveTag tb tag start end =
  {#call text_buffer_remove_tag#} tb tag start end

-- @method textBufferApplyTagByName@ Apply a tag that is specified by name.
--
textBufferApplyTagByName :: TextBuffer -> TagName -> TextIter -> TextIter ->
                            IO () 
textBufferApplyTagByName tb tname start end = withCString tname $ \cStr ->
  {#call text_buffer_apply_tag_by_name#} tb cStr start end

-- @method textBufferRemoveTagByName@ Remove a tag from a range of text.
--
textBufferRemoveTagByName :: TextBuffer -> TagName -> TextIter -> TextIter ->
                             IO ()
textBufferRemoveTagByName tb tname start end = withCString tname $ \cStr ->
  {#call text_buffer_remove_tag_by_name#} tb cStr start end

-- @method textBufferRemoveAllTags@ Remove all tags within a range.
--
-- *  @literal@
-- 
-- * Be careful with this function; it could remove tags added in code
--   unrelated to the code you're currently writing. That is, using this
--   function is probably a bad idea if you have two or more unrelated code
--   sections that add tags.
--

--
textBufferRemoveAllTags :: TextBuffer -> TextIter -> TextIter -> IO ()
textBufferRemoveAllTags tb start end =
  {#call text_buffer_remove_all_tags#} tb start end

-- @method textBufferGetIterAtLineOffset@ Create an iterator at a specific
-- line and offset.
--
-- * The @ref arg line@ and @ref arg offset@ arguments must be valid.
--
textBufferGetIterAtLineOffset :: TextBuffer -> Int -> Int -> IO TextIter
textBufferGetIterAtLineOffset tb line offset = do
  iter <- makeEmptyTextIter
  {#call unsafe text_buffer_get_iter_at_line_offset#}
    tb iter (fromIntegral line) (fromIntegral offset)
  return iter

-- @method textBufferGetIterAtOffset@ Create an iterator at a specific offset.
--
-- * The @ref arg offset@ arguments must be valid, starting from the first
--   character in the buffer.
--
textBufferGetIterAtOffset :: TextBuffer -> Int -> IO TextIter
textBufferGetIterAtOffset tb offset = do
  iter <- makeEmptyTextIter
  {#call unsafe text_buffer_get_iter_at_offset#}
    tb iter (fromIntegral offset)
  return iter
  
-- @method textBufferGetIterAtLine@ Create an iterator at a specific line.
--
-- * The @ref arg line@ arguments must be valid.
--
textBufferGetIterAtLine :: Int -> TextBuffer -> IO TextIter
textBufferGetIterAtLine line tb = do
  iter <- makeEmptyTextIter
  {#call unsafe text_buffer_get_iter_at_line#}
    tb iter (fromIntegral line)
  return iter


-- @method textBufferGetIterAtMark@ Create an iterator from a mark.
--
textBufferGetIterAtMark :: TextBuffer -> TextMark -> IO TextIter
textBufferGetIterAtMark tb tm = do
  iter <- makeEmptyTextIter
  {#call unsafe text_buffer_get_iter_at_mark#} tb iter tm
  return iter


-- @method textBufferGetStartIter@ Create an iterator at the beginning of the
-- buffer.
--
textBufferGetStartIter :: TextBuffer -> IO TextIter
textBufferGetStartIter tb = do
  iter <- makeEmptyTextIter
  {#call unsafe text_buffer_get_start_iter#} tb iter
  return iter

-- @method textBufferGetEndIter@ Create an iterator at the end of the buffer.
--
-- * The iterator represents the position after the last character in the
--   buffer.
--
textBufferGetEndIter :: TextBuffer -> IO TextIter
textBufferGetEndIter tb = do
  iter <- makeEmptyTextIter
  {#call unsafe text_buffer_get_end_iter#} tb iter
  return iter


-- @method textBufferGetModified@ Query if the buffer was modified.
--
-- * This flag is reset by calling @ref method textBufferSetModified@.
--
-- * It is usually more convenient to use
--   @ref signal connectToModifiedChanged@.
--
textBufferGetModified :: TextBuffer -> IO Bool
textBufferGetModified tb = liftM toBool $
  {#call unsafe text_buffer_get_modified#} tb

-- @method textBufferSetModified@ Set the "buffer-is-modified" flag.
--
textBufferSetModified :: TextBuffer -> Bool -> IO ()
textBufferSetModified tb isModified =
  {#call text_buffer_set_modified#} tb (fromBool isModified)

-- @method textBufferDeleteSelection@ Delete the current selection.
--
-- * The @ref arg interactive@ flag determines if this function is invoked on
--   behalf of the user (i.e. if we honour editable/non-editable tags).
--
-- * See @ref method textBufferInsertAtCursor@ for information on
--   @ref arg def@.
--
-- * The function returns True if a non-empty selection was deleted.
--
textBufferDeleteSelection :: TextBuffer -> Bool -> Bool -> IO Bool
textBufferDeleteSelection tb interactive def = liftM toBool $
  {#call text_buffer_delete_selection#} tb (fromBool interactive) 
  (fromBool def)

-- @method textBufferHasSelection@ Check if a selection exists.
--
textBufferHasSelection :: TextBuffer -> IO Bool
textBufferHasSelection tb = liftM toBool $
  {#call unsafe text_buffer_get_selection_bounds#} 
  tb (TextIter nullForeignPtr) (TextIter nullForeignPtr)

-- @method textBufferBeginUserAction@ Start a new atomic user action.
--
-- * Called to indicate that the buffer operations between here and a call to
--   @ref method textBufferEndUserAction@ are part of a single user-visible
--   operation. The operations between @ref method textBufferBeginUserAction@
--   and @ref method textBufferEndUserAction@ can then be grouped when
--   creating an undo stack. @ref type TextBuffer@ objects maintains a count
--   of calls to @ref method textBufferBeginUserAction@ that have not been
--   closed with a call to @ref method textBufferEndUserAction@, and emits the
--   "begin_user_action" and "end_user_action" signals only for the outermost
--   pair of calls. This allows you to build user actions from other user
--   actions. The "interactive" buffer mutation functions, such as
--   @ref method textBufferInsertInteractive@, automatically call begin/end
--   user action around the buffer operations they perform, so there's no need
--   to add extra calls if you user action consists solely of a single call to
--   one of those functions.
--
textBufferBeginUserAction :: TextBuffer -> IO ()
textBufferBeginUserAction  = {#call text_buffer_begin_user_action#}

-- @method textBufferEndUserAction@ End an atomic user action.
--
textBufferEndUserAction :: TextBuffer -> IO ()
textBufferEndUserAction  = {#call text_buffer_end_user_action#}


-- callbacks

-- @signal connectToApplyTag@ A @ref arg TextTag@ was applied to a region of
-- text.
--
onApplyTag, afterApplyTag :: TextBufferClass tb => tb ->
                             (TextTag -> TextIter -> TextIter -> IO ()) ->
                             IO (ConnectId tb)
onApplyTag = connect_OBJECT_BOXED_BOXED__NONE "apply-tag" 
  mkTextIter mkTextIter False
afterApplyTag = connect_OBJECT_BOXED_BOXED__NONE "apply-tag" 
  mkTextIter mkTextIter True

-- @signal connectToBeginUserAction@ A new atomic user action is started.
--
-- * Together with @ref method connectToEndUserAction@ these signals can be
--   used to build an undo stack.
--
onBeginUserAction, afterBeginUserAction :: TextBufferClass tb => tb -> IO () ->
                                           IO (ConnectId tb)
onBeginUserAction = connect_NONE__NONE "begin_user_action" False
afterBeginUserAction = connect_NONE__NONE "begin_user_action" True

-- @signal connectToChanged@ Emitted when the contents of the buffer change.
--
onChanged, afterChanged :: TextBufferClass tb => tb -> IO () ->
                           IO (ConnectId tb)
onChanged = connect_NONE__NONE "changed" False
afterChanged = connect_NONE__NONE "changed" True

-- @signal connectToDeleteRange@ A range of text is about to be deleted.
--
onDeleteRange, afterDeleteRange :: TextBufferClass tb => tb ->
                                   (TextIter -> TextIter -> IO ()) ->
                                   IO (ConnectId tb)
onDeleteRange = connect_BOXED_BOXED__NONE "delete_range"
  mkTextIter mkTextIter False
afterDeleteRange = connect_BOXED_BOXED__NONE "delete_range"
  mkTextIter mkTextIter True

-- @signal connectToEndUserAction@ An atomic action has ended.
--
-- * see @ref method connectToBeginUserAction@
--
onEndUserAction, afterEndUserAction :: TextBufferClass tb => tb -> IO () ->
                                       IO (ConnectId tb)
onEndUserAction = connect_NONE__NONE "end_user_action" False
afterEndUserAction = connect_NONE__NONE "end_user_action" True

-- @signal connectToInsertChildAnchor@ A widgets is inserted into the buffer.
--connectToInsertChildAnchor :: TextBufferClass tb =>
--  (TextIter -> TextChildAnchor -> IO ()) -> ConnectAfter -> tb -> 
--  IO (ConnectId tb)
--connectToInsertChildAnchor = connect_BOXED_OBJECT__NONE "insert_child_anchor"
--  mkTextIter



-- @signal connectToInsertPixbuf@ A @ref arg Pixbuf@ is inserted into the
-- buffer.
--
onInsertPixbuf, afterInsertPixbuf :: TextBufferClass tb => tb ->
                                     (TextIter -> GdkPixbuf -> IO ()) ->
                                     IO (ConnectId tb)
onInsertPixbuf = connect_BOXED_OBJECT__NONE "insert_pixbuf" mkTextIter False
afterInsertPixbuf = connect_BOXED_OBJECT__NONE "insert_pixbuf" mkTextIter True

-- @signal connectToInsertText@ Some text was inserted.
--
onInsertText, afterInsertText :: TextBufferClass tb => tb ->
                                 (TextIter -> String -> IO ()) ->
                                 IO (ConnectId tb)
onInsertText tb user = 
  connect_BOXED_PTR_INT__NONE "insert_text" mkTextIter False tb $
    \iter strP strLen -> do
      str <- peekCStringLen (strP,strLen)
      user iter str 
afterInsertText tb user = 
  connect_BOXED_PTR_INT__NONE "insert_text" mkTextIter True tb $
    \iter strP strLen -> do
      str <- peekCStringLen (strP,strLen)
      user iter str 

-- @signal connectToMarkDeleted@ A @ref arg TextMark@ within the buffer was
-- deleted.
--
onMarkDeleted, afterMarkDeleted :: TextBufferClass tb => tb ->
                                   (TextMark -> IO ()) -> IO (ConnectId tb)
onMarkDeleted = connect_OBJECT__NONE "mark_deleted" False
afterMarkDeleted = connect_OBJECT__NONE "mark_deleted" True

-- @signal connectToMarkSet@ A @ref arg TextMark@ was inserted into the
-- buffer.
--
onMarkSet, afterMarkSet :: TextBufferClass tb => tb ->
                           (TextIter -> TextMark -> IO ()) ->
                           IO (ConnectId tb)
onMarkSet = connect_BOXED_OBJECT__NONE "mark_set" mkTextIter False
afterMarkSet = connect_BOXED_OBJECT__NONE "mark_set" mkTextIter True

-- @signal connectToModifiedChanged@ The textbuffer has changed.
--
onModifiedChanged, afterModifiedChanged :: TextBufferClass tb => tb -> IO () ->
                                           IO (ConnectId tb)
onModifiedChanged = connect_NONE__NONE "modified_changed" False
afterModifiedChanged = connect_NONE__NONE "modified_changed" True

-- @signal connectToRemoveTag@ A @ref arg TextTag@ was removed.
--
onRemoveTag, afterRemoveTag :: TextBufferClass tb => tb ->
                               (TextTag -> TextIter -> TextIter -> IO ()) ->
                               IO (ConnectId tb)
onRemoveTag = connect_OBJECT_BOXED_BOXED__NONE "remove_tag" 
  mkTextIter mkTextIter False
afterRemoveTag = connect_OBJECT_BOXED_BOXED__NONE "remove_tag" 
  mkTextIter mkTextIter True

