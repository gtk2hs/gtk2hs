{-# OPTIONS -cpp #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry TextIter TextBuffer@
--
--  Author : Axel Simon
--          
--  Created: 23 February 2002
--
--  Version $Revision: 1.10 $ from $Date: 2003/10/21 21:28:53 $
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
--   * An iterator is an abstract datatype representing a pointer into a 
--     @ref data TextBuffer@.
--
-- @documentation@ ------------------------------------------------------------
--
-- * The following functions do not make sense due to Haskell's wide character
--   representation of Unicode:
--     gtk_text_iter_get_line_index
--     gtk_text_iter_get_visible_line_index
--     gtk_text_iter_get_bytes_in_line
--     gtk_text_iter_set_line_index
--     gtk_text_iter_set_visible_line_index
--
-- * The functions gtk_text_iter_in_range and gtk_text_iter_order are not bound
--   because they are only convenience functions which can replaced by calls
--   to textIterCompare.
--
-- * All offsets are counted from 0.
--
-- @todo@ ---------------------------------------------------------------------
--
-- * Bind the following function when GSList is bound:
--     gtk_text_iter_get_marks
--     gtk_text_iter_get_toggled_tags
--     gtk_text_iter_get_tags
--
-- * Bind the following functions when we are sure about anchors 
--   (see @ref data TextBuffer@):
--     gtk_text_iter_get_anchor
--
-- * Bind TextAttribute functions when I am clear how to model them. 
--     gtk_text_iter_get_attribute
--
-- * Forward exceptions in the two callback functions.
--
module TextIter(
  TextIter(TextIter),
  mkTextIter,
  makeEmptyTextIter,	-- for internal use only
  textIterGetBuffer,
  textIterCopy,
  textIterGetOffset,
  textIterGetLine,
  textIterGetLineOffset,
  textIterGetVisibleLineOffset,
  textIterGetChar,
  textIterGetSlice,
  textIterGetText,
  textIterGetVisibleSlice,
  textIterGetVisibleText,
  textIterGetPixbuf,
  textIterBeginsTag,
  textIterEndsTag,
  textIterTogglesTag,
  textIterHasTag,
  textIterEditable,
  textIterCanInsert,
  textIterStartsWord,
  textIterEndsWord,
  textIterInsideWord,
  textIterStartsLine,
  textIterEndsLine,
  textIterStartsSentence,
  textIterEndsSentence,
  textIterInsideSentence,
  textIterIsCursorPosition,
  textIterGetCharsInLine,
  textIterIsEnd,
  textIterIsStart,
  textIterForwardChar,
  textIterBackwardChar,
  textIterForwardChars,
  textIterBackwardChars,
  textIterForwardLine,
  textIterBackwardLine,
  textIterForwardLines,
  textIterBackwardLines,
  textIterForwardWordEnds,
  textIterBackwardWordStarts,
  textIterForwardWordEnd,
  textIterBackwardWordStart,
  textIterForwardCursorPosition,
  textIterBackwardCursorPosition,
  textIterForwardCursorPositions,
  textIterBackwardCursorPositions,
  textIterForwardSentenceEnds,
  textIterBackwardSentenceStarts,
  textIterForwardSentenceEnd,
  textIterBackwardSentenceStart,
  textIterSetOffset,
  textIterSetLine,
  textIterSetLineOffset,
  textIterSetVisibleLineOffset,
  textIterForwardToEnd,
  textIterForwardToLineEnd,
  textIterForwardToTagToggle,
  textIterBackwardToTagToggle,
  textIterForwardFindChar,
  textIterBackwardFindChar,
  textIterForwardSearch,
  textIterBackwardSearch,
  textIterEqual,
  textIterCompare
  ) where

import Monad	(liftM)
import Maybe	(fromMaybe)
import Char	(chr)
import FFI
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import Signal#}
import Structs  (textIterSize)
import Enums	(TextSearchFlags, Flags(fromFlags))

{# context lib="gtk" prefix="gtk" #}

-- methods

{#pointer *TextIter foreign newtype #}

-- Create a TextIter from a pointer.
--
mkTextIter :: Ptr TextIter -> IO TextIter
mkTextIter iterPtr = liftM TextIter $ 
  newForeignPtr iterPtr (text_iter_free iterPtr)

#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "&gtk_text_iter_free"
  text_iter_free' :: FinalizerPtr TextIter

text_iter_free :: Ptr TextIter -> FinalizerPtr TextIter
text_iter_free _ = text_iter_free'

#elif __GLASGOW_HASKELL__>=504

foreign import ccall unsafe "gtk_text_iter_free"
  text_iter_free :: Ptr TextIter -> IO ()

#else

foreign import ccall "gtk_text_iter_free" unsafe
  text_iter_free :: Ptr TextIter -> IO ()

#endif

-- Allocate memory to be filled with a TextIter.
--
makeEmptyTextIter :: IO TextIter
makeEmptyTextIter = do
  iterPtr <- mallocBytes textIterSize
  liftM TextIter $ newForeignPtr iterPtr (text_iter_free iterPtr)

-- @method textIterGetBuffer@ Return the @ref type TextBuffer@ this iterator
-- is associated with.
--
textIterGetBuffer :: TextIter -> IO TextBuffer
textIterGetBuffer ti = makeNewGObject mkTextBuffer $
  {#call unsafe text_iter_get_buffer#} ti

-- @method textIterCopy@ Copy the iterator.
--
textIterCopy :: TextIter -> IO TextIter
textIterCopy ti = do
  iterPtr <- {#call unsafe text_iter_copy#} ti
  liftM TextIter $ newForeignPtr iterPtr (text_iter_free iterPtr)

-- @method textIterGetOffset@ Extract the offset relative to the beginning of
-- the buffer.
--
textIterGetOffset :: TextIter -> IO Int
textIterGetOffset ti = liftM fromIntegral $
  {#call unsafe text_iter_get_offset#} ti

-- @method textIterGetLine@ Extract the line of the buffer.
--
textIterGetLine :: TextIter -> IO Int
textIterGetLine ti = liftM fromIntegral $
  {#call unsafe text_iter_get_line#} ti

-- @method textIterGetLineOffset@ Extract the offset relative to the beginning
-- of the line.
--
textIterGetLineOffset :: TextIter -> IO Int
textIterGetLineOffset ti = liftM fromIntegral $
  {#call unsafe text_iter_get_line_offset#} ti

-- @method textIterGetVisibleLineOffset@ Extract the offset relative to the
-- beginning of the line skipping invisible parts of the line.
--
textIterGetVisibleLineOffset :: TextIter -> IO Int
textIterGetVisibleLineOffset ti = liftM fromIntegral $
  {#call unsafe text_iter_get_visible_line_offset#} ti

-- @method textIterGetChar@ Return the character at this iterator.
--
textIterGetChar :: TextIter -> IO (Maybe Char)
textIterGetChar ti = do
  (res::Int) <- liftM fromIntegral $ {#call unsafe text_iter_get_char#} ti
  return $ if res==0 then Nothing else Just (chr res)

-- @method textIterGetSlice@ Return the text in a given range.
--
-- * Pictures (and other objects) are represented by 0xFFFC.
--
textIterGetSlice :: TextIter -> TextIter -> IO String
textIterGetSlice end start = do
  cStr <- {#call text_iter_get_slice#} start end
  str <- peekUTFString cStr
  {#call unsafe g_free#} (castPtr cStr)
  return str

-- @method textIterGetText@ Return the text in a given range.
--
-- * Pictures (and other objects) are stripped form the output.
--
textIterGetText :: TextIter -> TextIter -> IO String
textIterGetText start end = do
  cStr <- {#call text_iter_get_text#} start end
  str <- peekUTFString cStr
  {#call unsafe g_free#} (castPtr cStr)
  return str

-- @method textIterGetVisibleSlice@ Return the visible text in a given range.
--
-- * Pictures (and other objects) are represented by 0xFFFC.
--
textIterGetVisibleSlice :: TextIter -> TextIter -> IO String
textIterGetVisibleSlice start end = do
  cStr <- {#call text_iter_get_visible_slice#} start end
  str <- peekUTFString cStr
  {#call unsafe g_free#} (castPtr cStr)
  return str

-- @method textIterGetVisibleText@ Return the visible text in a given range.
--
-- * Pictures (and other objects) are stripped form the output.
--
textIterGetVisibleText :: TextIter -> TextIter -> IO String
textIterGetVisibleText start end = do
  cStr <- {#call text_iter_get_visible_text#} start end
  str <- peekUTFString cStr
  {#call unsafe g_free#} (castPtr cStr)
  return str

-- @method textIterGetPixbuf@ Get the @ref data Pixbuf@ under the iterator.
--
textIterGetPixbuf :: TextIter -> IO (Maybe Pixbuf)
textIterGetPixbuf it = do
  pbPtr <- {#call unsafe text_iter_get_pixbuf#} it
  if pbPtr==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkPixbuf (return pbPtr)


-- @method textIterBeginsTag@ Query whether a @ref type TextIter@ is at the
-- start of a @ref type TextTag@.
--
textIterBeginsTag :: TextIter -> TextTag -> IO Bool
textIterBeginsTag ti tt = liftM toBool $
  {#call unsafe text_iter_begins_tag#} ti tt


-- @method textIterEndsTag@ Query whether a @ref type TextIter@ is at the end
-- of a @ref type TextTag@.
--
textIterEndsTag :: TextIter -> TextTag -> IO Bool
textIterEndsTag ti tt = liftM toBool $
  {#call unsafe text_iter_ends_tag#} ti tt

-- @method textIterTogglesTag@ Query if the @ref type TextIter@ is at the
-- beginning or the end of a @ref type TextTag@.
--
textIterTogglesTag :: TextIter -> TextTag -> IO Bool
textIterTogglesTag ti tt = liftM toBool $
  {#call unsafe text_iter_toggles_tag#} ti tt

-- @method textIterHasTag@ Check if @ref type TextIter@ is within a range
-- tagged with tag.
--
textIterHasTag :: TextIter -> TextTag -> IO Bool
textIterHasTag ti tt = liftM toBool $
  {#call unsafe text_iter_has_tag#} ti tt

-- @method textIterEditable@ Check if @ref type TextIter@ is within an
-- editable region.
--
-- * If no tags that affect editability are attached to the current position
--   @ref arg def@ will be returned.
--
-- * This function cannot be used to decide whether text can be inserted at
--   @ref type TextIter@. Use the @ref method textIterCanInsert@ function for
--   this purpose.
--
textIterEditable :: TextIter -> Bool -> IO Bool
textIterEditable ti def = liftM toBool $ 
  {#call unsafe text_iter_editable#} ti (fromBool def)

-- @method textIterCanInsert@ Check if new text can be inserted at
-- @ref type TextIter@.
--
-- * Use @ref method textBufferInsertInteractive@ if you want to insert text
--   depending on the current editable status.
--
textIterCanInsert :: TextIter -> Bool -> IO Bool
textIterCanInsert ti def = liftM toBool $ 
  {#call unsafe text_iter_can_insert#} ti (fromBool def)

-- @method textIterStartsWord@ Determine if @ref type TextIter@ begins a new
-- natural-language word.
--
textIterStartsWord :: TextIter -> IO Bool
textIterStartsWord ti = liftM toBool $ {#call unsafe text_iter_starts_word#} ti


-- @method textIterEndsWord@ Determine if @ref type TextIter@ ends a new
-- natural-language word.
--
textIterEndsWord :: TextIter -> IO Bool
textIterEndsWord ti = liftM toBool $ {#call unsafe text_iter_ends_word#} ti

-- @method textIterInsideWord@ Determine if @ref type TextIter@ is inside a
-- word.
--
textIterInsideWord :: TextIter -> IO Bool
textIterInsideWord ti = liftM toBool $ {#call unsafe text_iter_inside_word#} ti

-- @method textIterStartsLine@ Determine if @ref type TextIter@ begins a new
-- line.
--
textIterStartsLine :: TextIter -> IO Bool
textIterStartsLine ti = liftM toBool $ {#call unsafe text_iter_starts_line#} ti

-- @method textIterEndsLine@ Determine if @ref type TextIter@ point to the
-- beginning of a line delimiter.
--
-- * Returns False if @ref type TextIter@ points to the \n in a \r\n sequence.
--
textIterEndsLine :: TextIter -> IO Bool
textIterEndsLine ti = liftM toBool $ {#call unsafe text_iter_ends_line#} ti

-- @method textIterStartsSentence@ Determine if @ref type TextIter@ starts a
-- sentence.
--
textIterStartsSentence :: TextIter -> IO Bool
textIterStartsSentence ti = liftM toBool $ 
  {#call unsafe text_iter_starts_sentence#} ti

-- @method textIterEndsSentence@ Determine if @ref type TextIter@ ends a
-- sentence.
--
textIterEndsSentence :: TextIter -> IO Bool
textIterEndsSentence ti = liftM toBool $ 
  {#call unsafe text_iter_ends_sentence#} ti

-- @method textIterInsideSentence@ Determine if @ref type TextIter@ is inside
-- a sentence.
--
textIterInsideSentence :: TextIter -> IO Bool
textIterInsideSentence ti = liftM toBool $ 
  {#call unsafe text_iter_inside_sentence#} ti

-- @method textIterIsCursorPosition@ Determine if @ref type TextIter@ is at a
-- cursor position.
--
textIterIsCursorPosition :: TextIter -> IO Bool
textIterIsCursorPosition ti = liftM toBool $ 
  {#call unsafe text_iter_is_cursor_position#} ti

-- @method textIterGetCharsInLine@ Return number of characters in this line.
--
-- * The return value includes delimiters.
--
textIterGetCharsInLine :: TextIter -> IO Int
textIterGetCharsInLine ti = liftM fromIntegral $
  {#call unsafe text_iter_get_chars_in_line#} ti

-- @method textIterGetAttributes@ Get the text attributes at the iterator.
--
-- * The @ref arg ta@ argument gives the default values if no specific 
--   attributes are set at that specific location.
--
-- * The function returns @literal Nothing@ if the text at the iterator has 
--   the same attributes.
textIterGetAttributes = undefined

-- @method textIterIsEnd@ Determine if @ref type TextIter@ is at the end of
-- the buffer.
--
textIterIsEnd :: TextIter -> IO Bool
textIterIsEnd ti = liftM toBool $ 
  {#call unsafe text_iter_is_end#} ti

-- @method textIterIsStart@ Determine if @ref type TextIter@ is at the
-- beginning of the buffer.
--
textIterIsStart :: TextIter -> IO Bool
textIterIsStart ti = liftM toBool $ 
  {#call unsafe text_iter_is_start#} ti

-- @method textIterForwardChar@ Move @ref type TextIter@ forwards.
--
-- * Retuns True if the iterator is pointing to a character.
--
textIterForwardChar :: TextIter -> IO Bool
textIterForwardChar ti = liftM toBool $ 
  {#call unsafe text_iter_forward_char#} ti

-- @method textIterBackwardChar@ Move @ref type TextIter@ backwards.
--
-- * Retuns True if the movement was possible.
--
textIterBackwardChar :: TextIter -> IO Bool
textIterBackwardChar ti = liftM toBool $ 
  {#call unsafe text_iter_backward_char#} ti

-- @method textIterForwardChars@ Move @ref type TextIter@ forwards by
-- @ref arg n@ characters.
--
-- * Retuns True if the iterator is pointing to a new character (and False if
--   the iterator points to a picture or has not moved).
--
textIterForwardChars :: TextIter -> Int -> IO Bool
textIterForwardChars ti n = liftM toBool $ 
  {#call unsafe text_iter_forward_chars#} ti (fromIntegral n)

-- @method textIterBackwardChars@ Move @ref type TextIter@ backwards by
-- @ref arg n@ characters.
--
-- * Retuns True if the iterator is pointing to a new character (and False if
--   the iterator points to a picture or has not moved).
--
textIterBackwardChars :: TextIter -> Int -> IO Bool
textIterBackwardChars ti n = liftM toBool $ 
  {#call unsafe text_iter_backward_chars#} ti (fromIntegral n)


-- @method textIterForwardLine@ Move @ref type TextIter@ forwards.
--
-- * Retuns True if the iterator is pointing to a new line (and False if the
--   iterator points to a picture or has not moved).
--
-- * If @ref type TextIter@ is on the first line, it will be moved to the
--   beginning of the buffer.
--
textIterForwardLine :: TextIter -> IO Bool
textIterForwardLine ti = liftM toBool $ 
  {#call unsafe text_iter_forward_line#} ti

-- @method textIterBackwardLine@ Move @ref type TextIter@ backwards.
--
-- * Retuns True if the iterator is pointing to a new line (and False if the
--   iterator points to a picture or has not moved).
--
-- * If @ref type TextIter@ is on the first line, it will be moved to the end
--   of the buffer.
--
textIterBackwardLine :: TextIter -> IO Bool
textIterBackwardLine ti = liftM toBool $ 
  {#call unsafe text_iter_backward_line#} ti


-- @method textIterForwardLines@ Move @ref type TextIter@ forwards by
-- @ref arg n@ lines.
--
-- * Retuns True if the iterator is pointing to a new line (and False if the
--   iterator points to a picture or has not moved).
--
-- * If @ref type TextIter@ is on the first line, it will be moved to the
--   beginning of the buffer.
--
-- * @ref arg n@ can be negative.
--
textIterForwardLines :: TextIter -> Int -> IO Bool
textIterForwardLines ti n = liftM toBool $ 
  {#call unsafe text_iter_forward_lines#} ti (fromIntegral n)

-- @method textIterBackwardLines@ Move @ref type TextIter@ backwards by
-- @ref arg n@ lines.
--
-- * Retuns True if the iterator is pointing to a new line (and False if the
--   iterator points to a picture or has not moved).
--
-- * If @ref type TextIter@ is on the first line, it will be moved to the end
--   of the buffer.
--
-- * @ref arg n@ can be negative.
--
textIterBackwardLines :: TextIter -> Int -> IO Bool
textIterBackwardLines ti n = liftM toBool $ 
  {#call unsafe text_iter_backward_lines#} ti (fromIntegral n)

-- @method textIterForwardWordEnds@ Move @ref type TextIter@ forwards by
-- @ref arg n@ word ends.
--
-- * Retuns True if the iterator is pointing to a new word end.
--
textIterForwardWordEnds :: TextIter -> Int -> IO Bool
textIterForwardWordEnds ti n = liftM toBool $ 
  {#call unsafe text_iter_forward_word_ends#} ti (fromIntegral n)

-- @method textIterBackwardWordStarts@ Move @ref type TextIter@ backwards by
-- @ref arg n@ word beginnings.
--
-- * Retuns True if the iterator is pointing to a new word start.
--
textIterBackwardWordStarts :: TextIter -> Int -> IO Bool
textIterBackwardWordStarts ti n = liftM toBool $ 
  {#call unsafe text_iter_backward_word_starts#} ti (fromIntegral n)

-- @method textIterForwardWordEnd@ Move @ref type TextIter@ forwards to the
-- next word end.
--
-- * Retuns True if the iterator has moved to a new word end.
--
textIterForwardWordEnd :: TextIter -> IO Bool
textIterForwardWordEnd ti = liftM toBool $ 
  {#call unsafe text_iter_forward_word_end#} ti

-- @method textIterBackwardWordStart@ Move @ref type TextIter@ backwards to
-- the next word beginning.
--
-- * Retuns True if the iterator has moved to a new word beginning.
--
textIterBackwardWordStart :: TextIter -> IO Bool
textIterBackwardWordStart ti = liftM toBool $ 
  {#call unsafe text_iter_backward_word_start#} ti

-- @method textIterForwardCursorPosition@ Move @ref type TextIter@ forwards to
-- the next cursor position.
--
-- * Some characters are composed of two Unicode codes. This function ensures
--   that @ref type TextIter@ does not point inbetween such double characters.
--
-- * Returns True if @ref type TextIter@ moved and points to a character (not
--   to an object).
--
textIterForwardCursorPosition :: TextIter -> IO Bool
textIterForwardCursorPosition ti = liftM toBool $
  {#call unsafe text_iter_forward_cursor_position#} ti

-- @method textIterBackwardCursorPosition@ Move @ref type TextIter@ backwards
-- to the next cursor position.
--
-- * Some characters are composed of two Unicode codes. This function ensures
--   that @ref type TextIter@ does not point inbetween such double characters.
--
-- * Returns True if @ref type TextIter@ moved and points to a character (not
--   to an object).
--
textIterBackwardCursorPosition :: TextIter -> IO Bool
textIterBackwardCursorPosition ti = liftM toBool $
  {#call unsafe text_iter_backward_cursor_position#} ti

-- @method textIterForwardCursorPositions@ Move @ref type TextIter@ forwards
-- by @ref arg n@ cursor positions.
--
-- * Returns True if @ref type TextIter@ moved and points to a character (not
--   to an object).
--
textIterForwardCursorPositions :: TextIter -> Int -> IO Bool
textIterForwardCursorPositions ti n = liftM toBool $ 
  {#call unsafe text_iter_forward_cursor_positions#} ti (fromIntegral n)

-- @method textIterBackwardCursorPositions@ Move @ref type TextIter@ backwards
-- by @ref arg n@ cursor positions.
--
-- * Returns True if @ref type TextIter@ moved and points to a character (not
--   to an object).
--
textIterBackwardCursorPositions :: TextIter -> Int -> IO Bool
textIterBackwardCursorPositions ti n = liftM toBool $ 
  {#call unsafe text_iter_backward_cursor_positions#} ti (fromIntegral n)


-- @method textIterForwardSentenceEnds@ Move @ref type TextIter@ forwards by
-- @ref arg n@ sentence ends.
--
-- * Retuns True if the iterator is pointing to a new sentence end.
--
textIterForwardSentenceEnds :: TextIter -> Int -> IO Bool
textIterForwardSentenceEnds ti n = liftM toBool $ 
  {#call unsafe text_iter_forward_sentence_ends#} ti (fromIntegral n)

-- @method textIterBackwardSentenceStarts@ Move @ref type TextIter@ backwards
-- by @ref arg n@ sentence beginnings.
--
-- * Retuns True if the iterator is pointing to a new sentence start.
--
textIterBackwardSentenceStarts :: TextIter -> Int -> IO Bool
textIterBackwardSentenceStarts ti n = liftM toBool $ 
  {#call unsafe text_iter_backward_sentence_starts#} ti (fromIntegral n)

-- @method textIterForwardSentenceEnd@ Move @ref type TextIter@ forwards to
-- the next sentence end.
--
-- * Retuns True if the iterator has moved to a new sentence end.
--
textIterForwardSentenceEnd :: TextIter -> IO Bool
textIterForwardSentenceEnd ti = liftM toBool $ 
  {#call unsafe text_iter_forward_sentence_end#} ti

-- @method textIterBackwardSentenceStart@ Move @ref type TextIter@ backwards
-- to the next sentence beginning.
--
-- * Retuns True if the iterator has moved to a new sentence beginning.
--
textIterBackwardSentenceStart :: TextIter -> IO Bool
textIterBackwardSentenceStart ti = liftM toBool $ 
  {#call unsafe text_iter_backward_sentence_start#} ti

-- @method textIterSetOffset@ Set @ref type TextIter@ to an offset within the
-- buffer.
--
textIterSetOffset :: TextIter -> Int -> IO ()
textIterSetOffset ti n = 
  {#call unsafe text_iter_set_offset#} ti (fromIntegral n)

-- @method textIterSetLine@ Set @ref type TextIter@ to a line within the
-- buffer.
--
textIterSetLine :: TextIter -> Int -> IO ()
textIterSetLine ti n = 
  {#call unsafe text_iter_set_line#} ti (fromIntegral n)

-- @method textIterSetLineOffset@ Set @ref type TextIter@ to an offset within
-- the line.
--
textIterSetLineOffset :: TextIter -> Int -> IO ()
textIterSetLineOffset ti n = 
  {#call unsafe text_iter_set_line_offset#} ti (fromIntegral n)

-- @method textIterSetVisibleLineOffset@ Set @ref type TextIter@ to an visible
-- character within the line.
--
textIterSetVisibleLineOffset :: TextIter -> Int -> IO ()
textIterSetVisibleLineOffset ti n = 
  {#call unsafe text_iter_set_visible_line_offset#} ti (fromIntegral n)

-- @method textIterForwardToEnd@ Moves @ref type TextIter@ to the end of the
-- buffer.
--
textIterForwardToEnd :: TextIter -> IO ()
textIterForwardToEnd ti = {#call unsafe text_iter_forward_to_end#} ti

-- @method textIterForwardToLineEnd@ Moves @ref type TextIter@ to the end of
-- the line.
--
-- * Returns True if @ref type TextIter@ moved to a new location which is not
--   the buffer end iterator.
--
textIterForwardToLineEnd :: TextIter -> IO Bool
textIterForwardToLineEnd ti = liftM toBool $
  {#call unsafe text_iter_forward_to_line_end#} ti

-- @method textIterForwardToTagToggle@ Moves @ref type TextIter@ forward to
-- the next change of a @ref type TextTag@.
--
-- * If Nothing is supplied, any @ref type TextTag@ will be matched.
--
-- * Returns True if there was a tag toggle after @ref type TextIter@.
--
textIterForwardToTagToggle :: TextIter -> Maybe TextTag -> IO Bool
textIterForwardToTagToggle ti tt = liftM toBool $
  {#call unsafe text_iter_forward_to_tag_toggle#} ti 
    (fromMaybe (mkTextTag nullForeignPtr) tt)

-- @method textIterBackwardToTagToggle@ Moves @ref type TextIter@ backward to
-- the next change of a @ref type TextTag@.
--
-- * If Nothing is supplied, any @ref type TextTag@ will be matched.
--
-- * Returns True if there was a tag toggle before @ref type TextIter@.
--
textIterBackwardToTagToggle :: TextIter -> Maybe TextTag -> IO Bool
textIterBackwardToTagToggle ti tt = liftM toBool $
  {#call unsafe text_iter_backward_to_tag_toggle#} ti 
    (fromMaybe (mkTextTag nullForeignPtr) tt)

-- Setup a callback for a predicate function.
--
type TextCharPredicateCB = Char -> Bool

{#pointer TextCharPredicate#}

#if __GLASGOW_HASKELL__>=600

foreign import ccall "wrapper" mkTextCharPredicate ::
  ({#type gunichar#} -> Ptr () -> {#type gboolean#}) -> IO TextCharPredicate

#else

foreign export dynamic mkTextCharPredicate ::
  ({#type gunichar#} -> Ptr () -> {#type gboolean#}) -> IO TextCharPredicate

#endif

-- @method textIterForwardFindChar@ Move @ref type TextIter@ forward until a
-- predicate function returns True.
--
-- * If @ref arg pred@ returns True before @ref arg limit@ is reached, the
--   search is stopped and the return value is True.
--
-- * If @ref arg limit@ is Nothing, the search stops at the end of the buffer.
--
textIterForwardFindChar :: TextIter -> (Char -> Bool) -> Maybe TextIter ->
                           IO Bool
textIterForwardFindChar ti pred limit = do
  fPtr <- mkTextCharPredicate (\c _ -> fromBool $ pred (chr (fromIntegral c)))
  res <- liftM toBool $ {#call text_iter_forward_find_char#} 
    ti fPtr nullPtr (fromMaybe (TextIter nullForeignPtr) limit)
  freeHaskellFunPtr fPtr
  return res

-- @method textIterBackwardFindChar@ Move @ref type TextIter@ backward until a
-- predicate function returns True.
--
-- * If @ref arg pred@ returns True before @ref arg limit@ is reached, the
--   search is stopped and the return value is True.
--
-- * If @ref arg limit@ is Nothing, the search stops at the end of the buffer.
--
textIterBackwardFindChar :: TextIter -> (Char -> Bool) -> Maybe TextIter ->
                            IO Bool
textIterBackwardFindChar ti pred limit = do
  fPtr <- mkTextCharPredicate (\c _ -> fromBool $ pred (chr (fromIntegral c)))
  res <- liftM toBool $ {#call text_iter_backward_find_char#} 
    ti fPtr nullPtr (fromMaybe (TextIter nullForeignPtr) limit)
  freeHaskellFunPtr fPtr
  return res

-- @method textIterForwardSearch@ Search forward for a specific string.
--
-- * If specified, the last character which is tested against that start of
--   the search pattern will be @ref arg limit@.
--
-- * @ref type TextSearchFlags@ may be empty.
--
-- * Returns the start and end position of the string found.
--
textIterForwardSearch :: TextIter -> String -> [TextSearchFlags] ->
                         Maybe TextIter -> IO (Maybe (TextIter, TextIter))
textIterForwardSearch ti str flags limit = do
  start  <- makeEmptyTextIter
  end <- makeEmptyTextIter
  found <- liftM toBool $ withUTFString str $ \cStr ->
    {#call unsafe text_iter_forward_search#} ti cStr 
    ((fromIntegral.fromFlags) flags) start end 
    (fromMaybe (TextIter nullForeignPtr) limit)
  return $ if found then Just (start,end) else Nothing

-- @method textIterBackwardSearch@ Search backward for a specific string.
--
-- * If specified, the last character which is tested against that start of
--   the search pattern will be @ref arg limit@.
--
-- * @ref type TextSearchFlags@ my be empty.
--
-- * Returns the start and end position of the string found.
--
textIterBackwardSearch :: TextIter -> String -> [TextSearchFlags] ->
                          Maybe TextIter -> IO (Maybe (TextIter, TextIter))
textIterBackwardSearch ti str flags limit = do
  start  <- makeEmptyTextIter
  end <- makeEmptyTextIter
  found <- liftM toBool $ withUTFString str $ \cStr ->
    {#call unsafe text_iter_backward_search#} ti cStr 
    ((fromIntegral.fromFlags) flags) start end 
    (fromMaybe (TextIter nullForeignPtr) limit)
  return $ if found then Just (start,end) else Nothing

-- @method textIterEqual@ Compare two @ref type TextIter@ for equality.
--
-- * @ref type TextIter@ could be in class Eq and Ord if there is a guarantee
--   that each iterator is copied before it is modified in place. This is done
--   the next abstraction layer.
--
textIterEqual :: TextIter -> TextIter -> IO Bool
textIterEqual ti2 ti1 = liftM toBool $ {#call unsafe text_iter_equal#} ti1 ti2

-- @method textIterCompare@ Compare two @ref type TextIter@.
--
-- * @ref type TextIter@ could be in class Eq and Ord if there is a guarantee
--   that each iterator is copied before it is modified in place. This could
--   be done the next abstraction layer.
--
textIterCompare :: TextIter -> TextIter -> IO Ordering
textIterCompare ti2 ti1 = do
  res <- {#call unsafe text_iter_compare#} ti1 ti2
  return $ case res of
    (-1)   -> LT
    0	   -> EQ
    1	   -> GT


