-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: TextIter abstract datatype
--
--  Author : Axel Simon
--          
--  Created: 23 February 2002
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
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
--   * An iterator is an abstract datatype representing a pointer into a 
--     @TextBuffer.
--
--- DOCU ----------------------------------------------------------------------
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
--- TODO ----------------------------------------------------------------------
--
-- * Bind the following function when GSList is bound:
--     gtk_text_iter_get_marks
--     gtk_text_iter_get_toggled_tags
--     gtk_text_iter_get_tags
--
-- * Bind the following functions when we are sure about anchors 
--   (see @TextBuffer):
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
import Foreign
import CForeign
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import Signal#}
import Structs  (nullForeignPtr, textIterSize)
import Enums	(TextSearchFlags, Flags(fromFlags))

{# context lib="gtk" prefix="gtk" #}

-- methods

{#pointer *TextIter foreign newtype #}

-- Create a @TextIter from a pointer.
--
mkTextIter :: Ptr TextIter -> IO TextIter
mkTextIter iterPtr = liftM TextIter $ 
  newForeignPtr iterPtr (textIterFree iterPtr)

-- Allocate memory to be filled with a @TextIter.
--
makeEmptyTextIter :: IO TextIter
makeEmptyTextIter = do
  iterPtr <- mallocBytes textIterSize
  liftM TextIter $ newForeignPtr iterPtr (textIterFree iterPtr)

-- Free a @TextIter pointer.
--
foreign import ccall "gtk_text_iter_free" unsafe 
  textIterFree :: Ptr TextIter -> IO ()


-- Return the @TextBuffer this iterator is associated with. (EXPORTED)
--
textIterGetBuffer :: TextIter -> IO TextBuffer
textIterGetBuffer ti = makeNewGObject mkTextBuffer $
  {#call unsafe text_iter_get_buffer#} ti

-- Copy the iterator. (EXPORTED)
--
textIterCopy :: TextIter -> IO TextIter
textIterCopy ti = do
  iterPtr <- {#call unsafe text_iter_copy#} ti
  liftM TextIter $ newForeignPtr iterPtr (textIterFree iterPtr)

-- Extract the offset relative to the beginning of the buffer. (EXPORTED)
--
textIterGetOffset :: TextIter -> IO Int
textIterGetOffset ti = liftM fromIntegral $
  {#call unsafe text_iter_get_offset#} ti

-- Extract the line of the buffer. (EXPORTED)
--
textIterGetLine :: TextIter -> IO Int
textIterGetLine ti = liftM fromIntegral $
  {#call unsafe text_iter_get_line#} ti

-- Extract the offset relative to the beginning of the line. (EXPORTED)
--
textIterGetLineOffset :: TextIter -> IO Int
textIterGetLineOffset ti = liftM fromIntegral $
  {#call unsafe text_iter_get_line_offset#} ti

-- Extract the offset relative to the beginning of the line skipping invisible
-- parts of the line. (EXPORTED)
--
textIterGetVisibleLineOffset :: TextIter -> IO Int
textIterGetVisibleLineOffset ti = liftM fromIntegral $
  {#call unsafe text_iter_get_visible_line_offset#} ti

-- Return the character at this iterator. (EXPORTED)
--
textIterGetChar :: TextIter -> IO (Maybe Char)
textIterGetChar ti = do
  (res::Int) <- liftM fromIntegral $ {#call unsafe text_iter_get_char#} ti
  return $ if res==0 then Nothing else Just (chr res)

-- Return the text in a given range. (EXPORTED)
--
-- * Pictures (and other objects) are represented by 0xFFFC.
--
textIterGetSlice :: TextIter -> TextIter -> IO String
textIterGetSlice start end = do
  cStr <- {#call text_iter_get_slice#} start end
  str <- peekCString cStr
  {#call unsafe g_free#} (castPtr cStr)
  return str

-- Return the text in a given range. (EXPORTED)
--
-- * Pictures (and other objects) are stripped form the output.
--
textIterGetText :: TextIter -> TextIter -> IO String
textIterGetText start end = do
  cStr <- {#call text_iter_get_text#} start end
  str <- peekCString cStr
  {#call unsafe g_free#} (castPtr cStr)
  return str

-- Return the visible text in a given range. (EXPORTED)
--
-- * Pictures (and other objects) are represented by 0xFFFC.
--
textIterGetVisibleSlice :: TextIter -> TextIter -> IO String
textIterGetVisibleSlice start end = do
  cStr <- {#call text_iter_get_visible_slice#} start end
  str <- peekCString cStr
  {#call unsafe g_free#} (castPtr cStr)
  return str

-- Return the visible text in a given range. (EXPORTED)
--
-- * Pictures (and other objects) are stripped form the output.
--
textIterGetVisibleText :: TextIter -> TextIter -> IO String
textIterGetVisibleText start end = do
  cStr <- {#call text_iter_get_visible_text#} start end
  str <- peekCString cStr
  {#call unsafe g_free#} (castPtr cStr)
  return str

-- Get the @GdkPixbuf under the iterator. (EXPORTED)
--
textIterGetPixbuf :: TextIter -> IO (Maybe GdkPixbuf)
textIterGetPixbuf it = do
  pbPtr <- {#call unsafe text_iter_get_pixbuf#} it
  if pbPtr==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkGdkPixbuf (return pbPtr)


-- Query whether a @TextIter is at the start of a @TextTag. (EXPORTED)
--
textIterBeginsTag :: TextTag -> TextIter -> IO Bool
textIterBeginsTag tt ti = liftM toBool $
  {#call unsafe text_iter_begins_tag#} ti tt


-- Query whether a @TextIter is at the end of a @TextTag. (EXPORTED)
--
textIterEndsTag :: TextTag -> TextIter -> IO Bool
textIterEndsTag tt ti = liftM toBool $
  {#call unsafe text_iter_ends_tag#} ti tt

-- Query if the @TextIter is at the beginning or the end of a @TextTag. 
-- (EXPORTED)
--
textIterTogglesTag :: TextTag -> TextIter -> IO Bool
textIterTogglesTag tt ti = liftM toBool $
  {#call unsafe text_iter_toggles_tag#} ti tt

-- Check if @TextIter is within a range tagged with tag. (EXPORTED)
--
textIterHasTag :: TextTag -> TextIter -> IO Bool
textIterHasTag tt ti = liftM toBool $
  {#call unsafe text_iter_has_tag#} ti tt

-- Check if @TextIter is within an editable region. (EXPORTED)
--
-- * If no tags that affect editability are attached to the current position
--   @def will be returned.
--
-- * This function cannot be used to decide whether text can be inserted at
--   @TextIter. Use the @textIterCanInsert function for this purpose.
--
textIterEditable :: Bool -> TextIter -> IO Bool
textIterEditable def ti = liftM toBool $ 
  {#call unsafe text_iter_editable#} ti (fromBool def)

-- Check if new text can be inserted at @TextIter. (EXPORTED)
--
-- * Use @textBufferInsertInteractive if you want to insert text depending
--   on the current editable status.
--
textIterCanInsert :: Bool -> TextIter -> IO Bool
textIterCanInsert def ti = liftM toBool $ 
  {#call unsafe text_iter_can_insert#} ti (fromBool def)

-- Determine if @TextIter begins a new natural-language word. (EXPORTED)
--
textIterStartsWord :: TextIter -> IO Bool
textIterStartsWord ti = liftM toBool $ {#call unsafe text_iter_starts_word#} ti


-- Determine if @TextIter ends a new natural-language word. (EXPORTED)
--
textIterEndsWord :: TextIter -> IO Bool
textIterEndsWord ti = liftM toBool $ {#call unsafe text_iter_ends_word#} ti

-- Determine if @TextIter is inside a word. (EXPORTED)
--
textIterInsideWord :: TextIter -> IO Bool
textIterInsideWord ti = liftM toBool $ {#call unsafe text_iter_inside_word#} ti

-- Determine if @TextIter begins a new line. (EXPORTED)
--
textIterStartsLine :: TextIter -> IO Bool
textIterStartsLine ti = liftM toBool $ {#call unsafe text_iter_starts_line#} ti

-- Determine if @TextIter point to the beginning of a line delimiter. 
-- (EXPORTED)
--
-- * Returns False if @TextIter points to the \n in a \r\n sequence.
--
textIterEndsLine :: TextIter -> IO Bool
textIterEndsLine ti = liftM toBool $ {#call unsafe text_iter_ends_line#} ti

-- Determine if @TextIter starts a sentence. (EXPORTED)
--
textIterStartsSentence :: TextIter -> IO Bool
textIterStartsSentence ti = liftM toBool $ 
  {#call unsafe text_iter_starts_sentence#} ti

-- Determine if @TextIter ends a sentence. (EXPORTED)
--
textIterEndsSentence :: TextIter -> IO Bool
textIterEndsSentence ti = liftM toBool $ 
  {#call unsafe text_iter_ends_sentence#} ti

-- Determine if @TextIter is inside a sentence. (EXPORTED)
--
textIterInsideSentence :: TextIter -> IO Bool
textIterInsideSentence ti = liftM toBool $ 
  {#call unsafe text_iter_inside_sentence#} ti

-- Determine if @TextIter is at a cursor position. (EXPORTED)
--
textIterIsCursorPosition :: TextIter -> IO Bool
textIterIsCursorPosition ti = liftM toBool $ 
  {#call unsafe text_iter_is_cursor_position#} ti

-- Return number of characters in this line. (EXPORTED)
--
-- * The return value includes delimiters.
--
textIterGetCharsInLine :: TextIter -> IO Int
textIterGetCharsInLine ti = liftM fromIntegral $
  {#call unsafe text_iter_get_chars_in_line#} ti

-- Get the text attributes at the iterator. (EXPORTED)
--
-- * The @ta argument gives the default values if no specific attributes
--   are set at that specific location.
--
-- * The function returns Nothing if the text at the iterator has the same
--   attributes.
--

-- Determine if @TextIter is at the end of the buffer. (EXPORTED)
--
textIterIsEnd :: TextIter -> IO Bool
textIterIsEnd ti = liftM toBool $ 
  {#call unsafe text_iter_is_end#} ti

-- Determine if @TextIter is at the beginning of the buffer. (EXPORTED)
--
textIterIsStart :: TextIter -> IO Bool
textIterIsStart ti = liftM toBool $ 
  {#call unsafe text_iter_is_start#} ti

-- Move @TextIter forwards. (EXPORTED)
--
-- * Retuns True if the iterator is pointing to a character.
--
textIterForwardChar :: TextIter -> IO Bool
textIterForwardChar ti = liftM toBool $ 
  {#call unsafe text_iter_forward_char#} ti

-- Move @TextIter backwards. (EXPORTED)
--
-- * Retuns True if the movement was possible.
--
textIterBackwardChar :: TextIter -> IO Bool
textIterBackwardChar ti = liftM toBool $ 
  {#call unsafe text_iter_backward_char#} ti

-- Move @TextIter forwards by @n characters. (EXPORTED)
--
-- * Retuns True if the iterator is pointing to a new character (and False if
--   the iterator points to a picture or has not moved).
--
textIterForwardChars :: Int -> TextIter -> IO Bool
textIterForwardChars n ti = liftM toBool $ 
  {#call unsafe text_iter_forward_chars#} ti (fromIntegral n)

-- Move @TextIter backwards by @n characters. (EXPORTED)
--
-- * Retuns True if the iterator is pointing to a new character (and False if
--   the iterator points to a picture or has not moved).
--
textIterBackwardChars :: Int -> TextIter -> IO Bool
textIterBackwardChars n ti = liftM toBool $ 
  {#call unsafe text_iter_backward_chars#} ti (fromIntegral n)


-- Move @TextIter forwards. (EXPORTED)
--
-- * Retuns True if the iterator is pointing to a new line (and False if
--   the iterator points to a picture or has not moved).
--
-- * If @TextIter is on the first line, it will be moved to the beginning of
--   the buffer.
--
textIterForwardLine :: TextIter -> IO Bool
textIterForwardLine ti = liftM toBool $ 
  {#call unsafe text_iter_forward_line#} ti

-- Move @TextIter backwards. (EXPORTED)
--
-- * Retuns True if the iterator is pointing to a new line (and False if
--   the iterator points to a picture or has not moved).
--
-- * If @TextIter is on the first line, it will be moved to the end of
--   the buffer.
--
textIterBackwardLine :: TextIter -> IO Bool
textIterBackwardLine ti = liftM toBool $ 
  {#call unsafe text_iter_backward_line#} ti


-- Move @TextIter forwards by @n lines. (EXPORTED)
--
-- * Retuns True if the iterator is pointing to a new line (and False if
--   the iterator points to a picture or has not moved).
--
-- * If @TextIter is on the first line, it will be moved to the beginning of
--   the buffer.
--
-- * @n can be negative.
--
textIterForwardLines :: Int -> TextIter -> IO Bool
textIterForwardLines n ti = liftM toBool $ 
  {#call unsafe text_iter_forward_lines#} ti (fromIntegral n)

-- Move @TextIter backwards by @n lines. (EXPORTED)
--
-- * Retuns True if the iterator is pointing to a new line (and False if
--   the iterator points to a picture or has not moved).
--
-- * If @TextIter is on the first line, it will be moved to the end of
--   the buffer.
--
-- * @n can be negative.
--
textIterBackwardLines :: Int -> TextIter -> IO Bool
textIterBackwardLines n ti = liftM toBool $ 
  {#call unsafe text_iter_backward_lines#} ti (fromIntegral n)

-- Move @TextIter forwards by @n word ends. (EXPORTED)
--
-- * Retuns True if the iterator is pointing to a new word end.
--
textIterForwardWordEnds :: Int -> TextIter -> IO Bool
textIterForwardWordEnds n ti = liftM toBool $ 
  {#call unsafe text_iter_forward_word_ends#} ti (fromIntegral n)

-- Move @TextIter backwards by @n word beginnings. (EXPORTED)
--
-- * Retuns True if the iterator is pointing to a new word start.
--
textIterBackwardWordStarts :: Int -> TextIter -> IO Bool
textIterBackwardWordStarts n ti = liftM toBool $ 
  {#call unsafe text_iter_backward_word_starts#} ti (fromIntegral n)

-- Move @TextIter forwards to the next word end. (EXPORTED)
--
-- * Retuns True if the iterator has moved to a new word end.
--
textIterForwardWordEnd :: TextIter -> IO Bool
textIterForwardWordEnd ti = liftM toBool $ 
  {#call unsafe text_iter_forward_word_end#} ti

-- Move @TextIter backwards to the next word beginning. (EXPORTED)
--
-- * Retuns True if the iterator has moved to a new word beginning.
--
textIterBackwardWordStart :: TextIter -> IO Bool
textIterBackwardWordStart ti = liftM toBool $ 
  {#call unsafe text_iter_backward_word_start#} ti

-- Move @TextIter forwards to the next cursor position. (EXPORTED)
--
-- * Some characters are composed of two Unicode codes. This function ensures
--   that @TextIter does not point inbetween such double characters.
--
-- * Returns True if @TextIter moved and points to a character (not to
--   an object).
--
textIterForwardCursorPosition :: TextIter -> IO Bool
textIterForwardCursorPosition ti = liftM toBool $
  {#call unsafe text_iter_forward_cursor_position#} ti

-- Move @TextIter backwards to the next cursor position. (EXPORTED)
--
-- * Some characters are composed of two Unicode codes. This function ensures
--   that @TextIter does not point inbetween such double characters.
--
-- * Returns True if @TextIter moved and points to a character (not to
--   an object).
--
textIterBackwardCursorPosition :: TextIter -> IO Bool
textIterBackwardCursorPosition ti = liftM toBool $
  {#call unsafe text_iter_backward_cursor_position#} ti

-- Move @TextIter forwards by @n cursor positions. (EXPORTED)
--
-- * Returns True if @TextIter moved and points to a character (not to
--   an object).
--
textIterForwardCursorPositions :: Int -> TextIter -> IO Bool
textIterForwardCursorPositions n ti = liftM toBool $ 
  {#call unsafe text_iter_forward_cursor_positions#} ti (fromIntegral n)

-- Move @TextIter backwards by @n cursor positions. (EXPORTED)
--
-- * Returns True if @TextIter moved and points to a character (not to
--   an object).
--
textIterBackwardCursorPositions :: Int -> TextIter -> IO Bool
textIterBackwardCursorPositions n ti = liftM toBool $ 
  {#call unsafe text_iter_backward_cursor_positions#} ti (fromIntegral n)


-- Move @TextIter forwards by @n sentence ends. (EXPORTED)
--
-- * Retuns True if the iterator is pointing to a new sentence end.
--
textIterForwardSentenceEnds :: Int -> TextIter -> IO Bool
textIterForwardSentenceEnds n ti = liftM toBool $ 
  {#call unsafe text_iter_forward_sentence_ends#} ti (fromIntegral n)

-- Move @TextIter backwards by @n sentence beginnings. (EXPORTED)
--
-- * Retuns True if the iterator is pointing to a new sentence start.
--
textIterBackwardSentenceStarts :: Int -> TextIter -> IO Bool
textIterBackwardSentenceStarts n ti = liftM toBool $ 
  {#call unsafe text_iter_backward_sentence_starts#} ti (fromIntegral n)

-- Move @TextIter forwards to the next sentence end. (EXPORTED)
--
-- * Retuns True if the iterator has moved to a new sentence end.
--
textIterForwardSentenceEnd :: TextIter -> IO Bool
textIterForwardSentenceEnd ti = liftM toBool $ 
  {#call unsafe text_iter_forward_sentence_end#} ti

-- Move @TextIter backwards to the next sentence beginning. (EXPORTED)
--
-- * Retuns True if the iterator has moved to a new sentence beginning.
--
textIterBackwardSentenceStart :: TextIter -> IO Bool
textIterBackwardSentenceStart ti = liftM toBool $ 
  {#call unsafe text_iter_backward_sentence_start#} ti

-- Set @TextIter to an offset within the buffer. (EXPORTED)
--
textIterSetOffset :: Int -> TextIter -> IO ()
textIterSetOffset n ti = 
  {#call unsafe text_iter_set_offset#} ti (fromIntegral n)

-- Set @TextIter to a line within the buffer. (EXPORTED)
--
textIterSetLine :: Int -> TextIter -> IO ()
textIterSetLine n ti = 
  {#call unsafe text_iter_set_line#} ti (fromIntegral n)

-- Set @TextIter to an offset within the line. (EXPORTED)
--
textIterSetLineOffset :: Int -> TextIter -> IO ()
textIterSetLineOffset n ti = 
  {#call unsafe text_iter_set_line_offset#} ti (fromIntegral n)

-- Set @TextIter to an visible character within the line. (EXPORTED)
--
textIterSetVisibleLineOffset :: Int -> TextIter -> IO ()
textIterSetVisibleLineOffset n ti = 
  {#call unsafe text_iter_set_visible_line_offset#} ti (fromIntegral n)

-- Moves @TextIter to the end of the buffer. (EXPORTED)
--
textIterForwardToEnd :: TextIter -> IO ()
textIterForwardToEnd ti = {#call unsafe text_iter_forward_to_end#} ti

-- Moves @TextIter to the end of the line. (EXPORTED)
--
-- * Returns True if @TextIter moved to a new location which is not the
--   buffer end iterator.
--
textIterForwardToLineEnd :: TextIter -> IO Bool
textIterForwardToLineEnd ti = liftM toBool $
  {#call unsafe text_iter_forward_to_line_end#} ti

-- Moves @TextIter forward to the next change of a @TextTag. (EXPORTED)
--
-- * If Nothing is supplied, any @TextTag will be matched.
--
-- * Returns True if there was a tag toggle after @TextIter.
--
textIterForwardToTagToggle :: Maybe TextTag -> TextIter -> IO Bool
textIterForwardToTagToggle tt ti = liftM toBool $
  {#call unsafe text_iter_forward_to_tag_toggle#} ti 
    (fromMaybe (mkTextTag nullForeignPtr) tt)

-- Moves @TextIter backward to the next change of a @TextTag. (EXPORTED)
--
-- * If Nothing is supplied, any @TextTag will be matched.
--
-- * Returns True if there was a tag toggle before @TextIter.
--
textIterBackwardToTagToggle :: Maybe TextTag -> TextIter -> IO Bool
textIterBackwardToTagToggle tt ti = liftM toBool $
  {#call unsafe text_iter_backward_to_tag_toggle#} ti 
    (fromMaybe (mkTextTag nullForeignPtr) tt)

-- Setup a callback for a predicate function.
--
type TextCharPredicateCB = Char -> Bool

{#pointer TextCharPredicate#}

foreign export dynamic mkTextCharPredicate ::
  ({#type gunichar#} -> Ptr () -> {#type gboolean#}) -> IO TextCharPredicate

-- Move @TextIter forward until a predicate function returns True. (EXPORTED)
--
-- * If @pred returns True before @limit is reached, the search is stopped
--   and the return value is True.
--
-- * If @limit is Nothing, the search stops at the end of the buffer.
--
textIterForwardFindChar :: (Char -> Bool) -> Maybe TextIter -> TextIter -> 
			   IO Bool
textIterForwardFindChar pred limit ti = do
  fPtr <- mkTextCharPredicate (\c _ -> fromBool $ pred (chr (fromIntegral c)))
  res <- liftM toBool $ {#call text_iter_forward_find_char#} 
    ti fPtr nullPtr (fromMaybe (TextIter nullForeignPtr) limit)
  freeHaskellFunPtr fPtr
  return res

-- Move @TextIter backward until a predicate function returns True. (EXPORTED)
--
-- * If @pred returns True before @limit is reached, the search is stopped
--   and the return value is True.
--
-- * If @limit is Nothing, the search stops at the end of the buffer.
--
textIterBackwardFindChar :: (Char -> Bool) -> Maybe TextIter -> TextIter -> 
			   IO Bool
textIterBackwardFindChar pred limit ti = do
  fPtr <- mkTextCharPredicate (\c _ -> fromBool $ pred (chr (fromIntegral c)))
  res <- liftM toBool $ {#call text_iter_backward_find_char#} 
    ti fPtr nullPtr (fromMaybe (TextIter nullForeignPtr) limit)
  freeHaskellFunPtr fPtr
  return res

-- Search forward for a specific string. (EXPORTED)
--
-- * If specified, the last character which is tested against that start
--   of the search pattern will be @limit.
--
-- * @TextSearchFlags may be empty.
--
-- * Returns the start and end position of the string found.
--
textIterForwardSearch :: String -> [TextSearchFlags] -> Maybe TextIter ->
			 TextIter -> IO (Maybe (TextIter, TextIter))
textIterForwardSearch str flags limit ti = do
  start  <- makeEmptyTextIter
  end <- makeEmptyTextIter
  found <- liftM toBool $ withCString str $ \cStr ->
    {#call unsafe text_iter_forward_search#} ti cStr 
    ((fromIntegral.fromFlags) flags) start end 
    (fromMaybe (TextIter nullForeignPtr) limit)
  return $ if found then Just (start,end) else Nothing

-- Search backward for a specific string. (EXPORTED)
--
-- * If specified, the last character which is tested against that start
--   of the search pattern will be @limit.
--
-- * @TextSearchFlags my be empty.
--
-- * Returns the start and end position of the string found.
--
textIterBackwardSearch :: String -> [TextSearchFlags] -> Maybe TextIter ->
			 TextIter -> IO (Maybe (TextIter, TextIter))
textIterBackwardSearch str flags limit ti = do
  start  <- makeEmptyTextIter
  end <- makeEmptyTextIter
  found <- liftM toBool $ withCString str $ \cStr ->
    {#call unsafe text_iter_backward_search#} ti cStr 
    ((fromIntegral.fromFlags) flags) start end 
    (fromMaybe (TextIter nullForeignPtr) limit)
  return $ if found then Just (start,end) else Nothing

-- Compare two @TextIter for equality. (EXPORTED)
--
-- * @TextIter could be in class Eq and Ord if there is a guarantee that
--   each iterator is copied before it is modified in place. This is
--   done the next abstraction layer.
--
textIterEqual :: TextIter -> TextIter -> IO Bool
textIterEqual ti1 ti2 = liftM toBool $ {#call unsafe text_iter_equal#} ti1 ti2

-- Compare two @TextIter. (EXPORTED)
--
-- * @TextIter could be in class Eq and Ord if there is a guarantee that
--   each iterator is copied before it is modified in place. This could be
--   done the next abstraction layer.
--
textIterCompare :: TextIter -> TextIter -> IO Ordering
textIterCompare ti1 ti2 = do
  res <- {#call unsafe text_iter_compare#} ti1 ti2
  return $ case res of
    (-1)   -> LT
    0	   -> EQ
    1	   -> GT


