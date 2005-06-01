-- -*-haskell-*-
--  GIMP Toolkit (GTK) TextIter TextBuffer
--
--  Author : Axel Simon
--
--  Created: 23 February 2002
--
--  Version $Revision: 1.5 $ from $Date: 2005/06/01 19:07:49 $
--
--  Copyright (C) 2002-2005 Axel Simon
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
-- The following functions do not make sense due to Haskell's wide character
--   representation of Unicode:
--     gtk_text_iter_get_line_index
--     gtk_text_iter_get_visible_line_index
--     gtk_text_iter_get_bytes_in_line
--     gtk_text_iter_set_line_index
--     gtk_text_iter_set_visible_line_index
--
-- The functions gtk_text_iter_in_range and gtk_text_iter_order are not bound
--   because they are only convenience functions which can replaced by calls
--   to textIterCompare.
--
-- All offsets are counted from 0.
--
-- TODO
--
-- Bind the following function when GSList is bound:
--     gtk_text_iter_get_marks
--     gtk_text_iter_get_toggled_tags
--     gtk_text_iter_get_tags
--
-- Bind the following functions when we are sure about anchors 
--   (see 'TextBuffer'):
--     gtk_text_iter_get_anchor
--
-- Bind TextAttribute functions when I am clear how to model them. 
--     gtk_text_iter_get_attribute
--
-- Forward exceptions in the two callback functions.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- An iterator is an abstract datatype representing a pointer into a 
-- 'TextBuffer'.
--
module Graphics.UI.Gtk.Multiline.TextIter (

-- * Types
  TextIter(TextIter),

-- * Methods
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
  textIterCompare,

-- * Attributes
  textIterVisibleLineOffset,
  textIterOffset,
  textIterLineOffset,
  textIterLine,
  ) where

import Monad	(liftM)
import Maybe	(fromMaybe)
import Char	(chr)

import System.Glib.FFI
import System.Glib.Flags		(fromFlags)
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.GObject		(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Structs	(textIterSize)
import Graphics.UI.Gtk.General.Enums	(TextSearchFlags)

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

#else

foreign import ccall unsafe "gtk_text_iter_free"
  text_iter_free :: Ptr TextIter -> IO ()

#endif

-- Allocate memory to be filled with a TextIter.
--
makeEmptyTextIter :: IO TextIter
makeEmptyTextIter = do
  iterPtr <- mallocBytes textIterSize
  liftM TextIter $ newForeignPtr iterPtr (text_iter_free iterPtr)

-- | Return the 'TextBuffer' this iterator
-- is associated with.
--
textIterGetBuffer :: TextIter -> IO TextBuffer
textIterGetBuffer ti = makeNewGObject mkTextBuffer $
  {#call unsafe text_iter_get_buffer#} ti

-- | Copy the iterator.
--
textIterCopy :: TextIter -> IO TextIter
textIterCopy ti = do
  iterPtr <- {#call unsafe text_iter_copy#} ti
  liftM TextIter $ newForeignPtr iterPtr (text_iter_free iterPtr)

-- | Extract the offset relative to the beginning of
-- the buffer.
--
textIterGetOffset :: TextIter -> IO Int
textIterGetOffset ti = liftM fromIntegral $
  {#call unsafe text_iter_get_offset#} ti

-- | Extract the line of the buffer.
--
textIterGetLine :: TextIter -> IO Int
textIterGetLine ti = liftM fromIntegral $
  {#call unsafe text_iter_get_line#} ti

-- | Extract the offset relative to the beginning
-- of the line.
--
textIterGetLineOffset :: TextIter -> IO Int
textIterGetLineOffset ti = liftM fromIntegral $
  {#call unsafe text_iter_get_line_offset#} ti

-- | Extract the offset relative to the
-- beginning of the line skipping invisible parts of the line.
--
textIterGetVisibleLineOffset :: TextIter -> IO Int
textIterGetVisibleLineOffset ti = liftM fromIntegral $
  {#call unsafe text_iter_get_visible_line_offset#} ti

-- | Return the character at this iterator.
--
textIterGetChar :: TextIter -> IO (Maybe Char)
textIterGetChar ti = do
  res <- liftM fromIntegral $ {#call unsafe text_iter_get_char#} ti
  return $ if res==0 then Nothing else Just (chr res)

-- | Return the text in a given range.
--
-- * Pictures (and other objects) are represented by 0xFFFC.
--
textIterGetSlice :: TextIter -> TextIter -> IO String
textIterGetSlice end start = do
  cStr <- {#call text_iter_get_slice#} start end
  str <- peekUTFString cStr
  {#call unsafe g_free#} (castPtr cStr)
  return str

-- | Return the text in a given range.
--
-- * Pictures (and other objects) are stripped form the output.
--
textIterGetText :: TextIter -> TextIter -> IO String
textIterGetText start end = do
  cStr <- {#call text_iter_get_text#} start end
  str <- peekUTFString cStr
  {#call unsafe g_free#} (castPtr cStr)
  return str

-- | Return the visible text in a given range.
--
-- * Pictures (and other objects) are represented by 0xFFFC.
--
textIterGetVisibleSlice :: TextIter -> TextIter -> IO String
textIterGetVisibleSlice start end = do
  cStr <- {#call text_iter_get_visible_slice#} start end
  str <- peekUTFString cStr
  {#call unsafe g_free#} (castPtr cStr)
  return str

-- | Return the visible text in a given range.
--
-- * Pictures (and other objects) are stripped form the output.
--
textIterGetVisibleText :: TextIter -> TextIter -> IO String
textIterGetVisibleText start end = do
  cStr <- {#call text_iter_get_visible_text#} start end
  str <- peekUTFString cStr
  {#call unsafe g_free#} (castPtr cStr)
  return str

-- | Get the 'Pixbuf' under the iterator.
--
textIterGetPixbuf :: TextIter -> IO (Maybe Pixbuf)
textIterGetPixbuf it = do
  pbPtr <- {#call unsafe text_iter_get_pixbuf#} it
  if pbPtr==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkPixbuf (return pbPtr)


-- | Query whether a 'TextIter' is at the
-- start of a 'TextTag'.
--
textIterBeginsTag :: TextIter -> TextTag -> IO Bool
textIterBeginsTag ti tt = liftM toBool $
  {#call unsafe text_iter_begins_tag#} ti tt


-- | Query whether a 'TextIter' is at the end
-- of a 'TextTag'.
--
textIterEndsTag :: TextIter -> TextTag -> IO Bool
textIterEndsTag ti tt = liftM toBool $
  {#call unsafe text_iter_ends_tag#} ti tt

-- | Query if the 'TextIter' is at the
-- beginning or the end of a 'TextTag'.
--
textIterTogglesTag :: TextIter -> TextTag -> IO Bool
textIterTogglesTag ti tt = liftM toBool $
  {#call unsafe text_iter_toggles_tag#} ti tt

-- | Check if 'TextIter' is within a range
-- tagged with tag.
--
textIterHasTag :: TextIter -> TextTag -> IO Bool
textIterHasTag ti tt = liftM toBool $
  {#call unsafe text_iter_has_tag#} ti tt

-- | Check if 'TextIter' is within an
-- editable region.
--
-- * If no tags that affect editability are attached to the current position
--   @def@ will be returned.
--
-- * This function cannot be used to decide whether text can be inserted at
--   'TextIter'. Use the 'textIterCanInsert' function for
--   this purpose.
--
textIterEditable :: TextIter -> Bool -> IO Bool
textIterEditable ti def = liftM toBool $ 
  {#call unsafe text_iter_editable#} ti (fromBool def)

-- | Check if new text can be inserted at
-- 'TextIter'.
--
-- * Use 'textBufferInsertInteractive' if you want to insert text
--   depending on the current editable status.
--
textIterCanInsert :: TextIter -> Bool -> IO Bool
textIterCanInsert ti def = liftM toBool $ 
  {#call unsafe text_iter_can_insert#} ti (fromBool def)

-- | Determine if 'TextIter' begins a new
-- natural-language word.
--
textIterStartsWord :: TextIter -> IO Bool
textIterStartsWord ti = liftM toBool $ {#call unsafe text_iter_starts_word#} ti


-- | Determine if 'TextIter' ends a new
-- natural-language word.
--
textIterEndsWord :: TextIter -> IO Bool
textIterEndsWord ti = liftM toBool $ {#call unsafe text_iter_ends_word#} ti

-- | Determine if 'TextIter' is inside a
-- word.
--
textIterInsideWord :: TextIter -> IO Bool
textIterInsideWord ti = liftM toBool $ {#call unsafe text_iter_inside_word#} ti

-- | Determine if 'TextIter' begins a new
-- line.
--
textIterStartsLine :: TextIter -> IO Bool
textIterStartsLine ti = liftM toBool $ {#call unsafe text_iter_starts_line#} ti

-- | Determine if 'TextIter' point to the
-- beginning of a line delimiter.
--
-- * Returns False if 'TextIter' points to the \n in a \r\n sequence.
--
textIterEndsLine :: TextIter -> IO Bool
textIterEndsLine ti = liftM toBool $ {#call unsafe text_iter_ends_line#} ti

-- | Determine if 'TextIter' starts a
-- sentence.
--
textIterStartsSentence :: TextIter -> IO Bool
textIterStartsSentence ti = liftM toBool $ 
  {#call unsafe text_iter_starts_sentence#} ti

-- | Determine if 'TextIter' ends a
-- sentence.
--
textIterEndsSentence :: TextIter -> IO Bool
textIterEndsSentence ti = liftM toBool $ 
  {#call unsafe text_iter_ends_sentence#} ti

-- | Determine if 'TextIter' is inside
-- a sentence.
--
textIterInsideSentence :: TextIter -> IO Bool
textIterInsideSentence ti = liftM toBool $ 
  {#call unsafe text_iter_inside_sentence#} ti

-- | Determine if 'TextIter' is at a
-- cursor position.
--
textIterIsCursorPosition :: TextIter -> IO Bool
textIterIsCursorPosition ti = liftM toBool $ 
  {#call unsafe text_iter_is_cursor_position#} ti

-- | Return number of characters in this line.
--
-- * The return value includes delimiters.
--
textIterGetCharsInLine :: TextIter -> IO Int
textIterGetCharsInLine ti = liftM fromIntegral $
  {#call unsafe text_iter_get_chars_in_line#} ti

-- | Get the text attributes at the iterator.
--
-- * The @ta@ argument gives the default values if no specific 
--   attributes are set at that specific location.
--
-- * The function returns @Nothing@ if the text at the iterator has 
--   the same attributes.
textIterGetAttributes = undefined

-- | Determine if 'TextIter' is at the end of
-- the buffer.
--
textIterIsEnd :: TextIter -> IO Bool
textIterIsEnd ti = liftM toBool $ 
  {#call unsafe text_iter_is_end#} ti

-- | Determine if 'TextIter' is at the
-- beginning of the buffer.
--
textIterIsStart :: TextIter -> IO Bool
textIterIsStart ti = liftM toBool $ 
  {#call unsafe text_iter_is_start#} ti

-- | Move 'TextIter' forwards.
--
-- * Retuns True if the iterator is pointing to a character.
--
textIterForwardChar :: TextIter -> IO Bool
textIterForwardChar ti = liftM toBool $ 
  {#call unsafe text_iter_forward_char#} ti

-- | Move 'TextIter' backwards.
--
-- * Retuns True if the movement was possible.
--
textIterBackwardChar :: TextIter -> IO Bool
textIterBackwardChar ti = liftM toBool $ 
  {#call unsafe text_iter_backward_char#} ti

-- | Move 'TextIter' forwards by
-- @n@ characters.
--
-- * Retuns True if the iterator is pointing to a new character (and False if
--   the iterator points to a picture or has not moved).
--
textIterForwardChars :: TextIter -> Int -> IO Bool
textIterForwardChars ti n = liftM toBool $ 
  {#call unsafe text_iter_forward_chars#} ti (fromIntegral n)

-- | Move 'TextIter' backwards by
-- @n@ characters.
--
-- * Retuns True if the iterator is pointing to a new character (and False if
--   the iterator points to a picture or has not moved).
--
textIterBackwardChars :: TextIter -> Int -> IO Bool
textIterBackwardChars ti n = liftM toBool $ 
  {#call unsafe text_iter_backward_chars#} ti (fromIntegral n)


-- | Move 'TextIter' forwards.
--
-- * Retuns True if the iterator is pointing to a new line (and False if the
--   iterator points to a picture or has not moved).
--
-- * If 'TextIter' is on the first line, it will be moved to the
--   beginning of the buffer.
--
textIterForwardLine :: TextIter -> IO Bool
textIterForwardLine ti = liftM toBool $ 
  {#call unsafe text_iter_forward_line#} ti

-- | Move 'TextIter' backwards.
--
-- * Retuns True if the iterator is pointing to a new line (and False if the
--   iterator points to a picture or has not moved).
--
-- * If 'TextIter' is on the first line, it will be moved to the end
--   of the buffer.
--
textIterBackwardLine :: TextIter -> IO Bool
textIterBackwardLine ti = liftM toBool $ 
  {#call unsafe text_iter_backward_line#} ti


-- | Move 'TextIter' forwards by
-- @n@ lines.
--
-- * Retuns True if the iterator is pointing to a new line (and False if the
--   iterator points to a picture or has not moved).
--
-- * If 'TextIter' is on the first line, it will be moved to the
--   beginning of the buffer.
--
-- * @n@ can be negative.
--
textIterForwardLines :: TextIter -> Int -> IO Bool
textIterForwardLines ti n = liftM toBool $ 
  {#call unsafe text_iter_forward_lines#} ti (fromIntegral n)

-- | Move 'TextIter' backwards by
-- @n@ lines.
--
-- * Retuns True if the iterator is pointing to a new line (and False if the
--   iterator points to a picture or has not moved).
--
-- * If 'TextIter' is on the first line, it will be moved to the end
--   of the buffer.
--
-- * @n@ can be negative.
--
textIterBackwardLines :: TextIter -> Int -> IO Bool
textIterBackwardLines ti n = liftM toBool $ 
  {#call unsafe text_iter_backward_lines#} ti (fromIntegral n)

-- | Move 'TextIter' forwards by
-- @n@ word ends.
--
-- * Retuns True if the iterator is pointing to a new word end.
--
textIterForwardWordEnds :: TextIter -> Int -> IO Bool
textIterForwardWordEnds ti n = liftM toBool $ 
  {#call unsafe text_iter_forward_word_ends#} ti (fromIntegral n)

-- | Move 'TextIter' backwards by
-- @n@ word beginnings.
--
-- * Retuns True if the iterator is pointing to a new word start.
--
textIterBackwardWordStarts :: TextIter -> Int -> IO Bool
textIterBackwardWordStarts ti n = liftM toBool $ 
  {#call unsafe text_iter_backward_word_starts#} ti (fromIntegral n)

-- | Move 'TextIter' forwards to the
-- next word end.
--
-- * Retuns True if the iterator has moved to a new word end.
--
textIterForwardWordEnd :: TextIter -> IO Bool
textIterForwardWordEnd ti = liftM toBool $ 
  {#call unsafe text_iter_forward_word_end#} ti

-- | Move 'TextIter' backwards to
-- the next word beginning.
--
-- * Retuns True if the iterator has moved to a new word beginning.
--
textIterBackwardWordStart :: TextIter -> IO Bool
textIterBackwardWordStart ti = liftM toBool $ 
  {#call unsafe text_iter_backward_word_start#} ti

-- | Move 'TextIter' forwards to
-- the next cursor position.
--
-- * Some characters are composed of two Unicode codes. This function ensures
--   that 'TextIter' does not point inbetween such double characters.
--
-- * Returns True if 'TextIter' moved and points to a character (not
--   to an object).
--
textIterForwardCursorPosition :: TextIter -> IO Bool
textIterForwardCursorPosition ti = liftM toBool $
  {#call unsafe text_iter_forward_cursor_position#} ti

-- | Move 'TextIter' backwards
-- to the next cursor position.
--
-- * Some characters are composed of two Unicode codes. This function ensures
--   that 'TextIter' does not point inbetween such double characters.
--
-- * Returns True if 'TextIter' moved and points to a character (not
--   to an object).
--
textIterBackwardCursorPosition :: TextIter -> IO Bool
textIterBackwardCursorPosition ti = liftM toBool $
  {#call unsafe text_iter_backward_cursor_position#} ti

-- | Move 'TextIter' forwards
-- by @n@ cursor positions.
--
-- * Returns True if 'TextIter' moved and points to a character (not
--   to an object).
--
textIterForwardCursorPositions :: TextIter -> Int -> IO Bool
textIterForwardCursorPositions ti n = liftM toBool $ 
  {#call unsafe text_iter_forward_cursor_positions#} ti (fromIntegral n)

-- | Move 'TextIter' backwards
-- by @n@ cursor positions.
--
-- * Returns True if 'TextIter' moved and points to a character (not
--   to an object).
--
textIterBackwardCursorPositions :: TextIter -> Int -> IO Bool
textIterBackwardCursorPositions ti n = liftM toBool $ 
  {#call unsafe text_iter_backward_cursor_positions#} ti (fromIntegral n)


-- | Move 'TextIter' forwards by
-- @n@ sentence ends.
--
-- * Retuns True if the iterator is pointing to a new sentence end.
--
textIterForwardSentenceEnds :: TextIter -> Int -> IO Bool
textIterForwardSentenceEnds ti n = liftM toBool $ 
  {#call unsafe text_iter_forward_sentence_ends#} ti (fromIntegral n)

-- | Move 'TextIter' backwards
-- by @n@ sentence beginnings.
--
-- * Retuns True if the iterator is pointing to a new sentence start.
--
textIterBackwardSentenceStarts :: TextIter -> Int -> IO Bool
textIterBackwardSentenceStarts ti n = liftM toBool $ 
  {#call unsafe text_iter_backward_sentence_starts#} ti (fromIntegral n)

-- | Move 'TextIter' forwards to
-- the next sentence end.
--
-- * Retuns True if the iterator has moved to a new sentence end.
--
textIterForwardSentenceEnd :: TextIter -> IO Bool
textIterForwardSentenceEnd ti = liftM toBool $ 
  {#call unsafe text_iter_forward_sentence_end#} ti

-- | Move 'TextIter' backwards
-- to the next sentence beginning.
--
-- * Retuns True if the iterator has moved to a new sentence beginning.
--
textIterBackwardSentenceStart :: TextIter -> IO Bool
textIterBackwardSentenceStart ti = liftM toBool $ 
  {#call unsafe text_iter_backward_sentence_start#} ti

-- | Set 'TextIter' to an offset within the
-- buffer.
--
textIterSetOffset :: TextIter -> Int -> IO ()
textIterSetOffset ti n = 
  {#call unsafe text_iter_set_offset#} ti (fromIntegral n)

-- | Set 'TextIter' to a line within the
-- buffer.
--
textIterSetLine :: TextIter -> Int -> IO ()
textIterSetLine ti n = 
  {#call unsafe text_iter_set_line#} ti (fromIntegral n)

-- | Set 'TextIter' to an offset within
-- the line.
--
textIterSetLineOffset :: TextIter -> Int -> IO ()
textIterSetLineOffset ti n = 
  {#call unsafe text_iter_set_line_offset#} ti (fromIntegral n)

-- | Set 'TextIter' to an visible
-- character within the line.
--
textIterSetVisibleLineOffset :: TextIter -> Int -> IO ()
textIterSetVisibleLineOffset ti n = 
  {#call unsafe text_iter_set_visible_line_offset#} ti (fromIntegral n)

-- | Moves 'TextIter' to the end of the
-- buffer.
--
textIterForwardToEnd :: TextIter -> IO ()
textIterForwardToEnd ti = {#call unsafe text_iter_forward_to_end#} ti

-- | Moves 'TextIter' to the end of
-- the line.
--
-- * Returns True if 'TextIter' moved to a new location which is not
--   the buffer end iterator.
--
textIterForwardToLineEnd :: TextIter -> IO Bool
textIterForwardToLineEnd ti = liftM toBool $
  {#call unsafe text_iter_forward_to_line_end#} ti

-- | Moves 'TextIter' forward to
-- the next change of a 'TextTag'.
--
-- * If Nothing is supplied, any 'TextTag' will be matched.
--
-- * Returns True if there was a tag toggle after 'TextIter'.
--
textIterForwardToTagToggle :: TextIter -> Maybe TextTag -> IO Bool
textIterForwardToTagToggle ti tt = liftM toBool $
  {#call unsafe text_iter_forward_to_tag_toggle#} ti 
    (fromMaybe (mkTextTag nullForeignPtr) tt)

-- | Moves 'TextIter' backward to
-- the next change of a 'TextTag'.
--
-- * If Nothing is supplied, any 'TextTag' will be matched.
--
-- * Returns True if there was a tag toggle before 'TextIter'.
--
textIterBackwardToTagToggle :: TextIter -> Maybe TextTag -> IO Bool
textIterBackwardToTagToggle ti tt = liftM toBool $
  {#call unsafe text_iter_backward_to_tag_toggle#} ti 
    (fromMaybe (mkTextTag nullForeignPtr) tt)

-- Setup a callback for a predicate function.
--
type TextCharPredicateCB = Char -> Bool

{#pointer TextCharPredicate#}

foreign import ccall "wrapper" mkTextCharPredicate ::
  ({#type gunichar#} -> Ptr () -> {#type gboolean#}) -> IO TextCharPredicate

-- | Move 'TextIter' forward until a
-- predicate function returns True.
--
-- * If @pred@ returns True before @limit@ is reached, the
--   search is stopped and the return value is True.
--
-- * If @limit@ is Nothing, the search stops at the end of the buffer.
--
textIterForwardFindChar :: TextIter -> (Char -> Bool) -> Maybe TextIter ->
                           IO Bool
textIterForwardFindChar ti pred limit = do
  fPtr <- mkTextCharPredicate (\c _ -> fromBool $ pred (chr (fromIntegral c)))
  res <- liftM toBool $ {#call text_iter_forward_find_char#} 
    ti fPtr nullPtr (fromMaybe (TextIter nullForeignPtr) limit)
  freeHaskellFunPtr fPtr
  return res

-- | Move 'TextIter' backward until a
-- predicate function returns True.
--
-- * If @pred@ returns True before @limit@ is reached, the
--   search is stopped and the return value is True.
--
-- * If @limit@ is Nothing, the search stops at the end of the buffer.
--
textIterBackwardFindChar :: TextIter -> (Char -> Bool) -> Maybe TextIter ->
                            IO Bool
textIterBackwardFindChar ti pred limit = do
  fPtr <- mkTextCharPredicate (\c _ -> fromBool $ pred (chr (fromIntegral c)))
  res <- liftM toBool $ {#call text_iter_backward_find_char#} 
    ti fPtr nullPtr (fromMaybe (TextIter nullForeignPtr) limit)
  freeHaskellFunPtr fPtr
  return res

-- | Search forward for a specific string.
--
-- * If specified, the last character which is tested against that start of
--   the search pattern will be @limit@.
--
-- * 'TextSearchFlags' may be empty.
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

-- | Search backward for a specific string.
--
-- * If specified, the last character which is tested against that start of
--   the search pattern will be @limit@.
--
-- * 'TextSearchFlags' my be empty.
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

-- | Compare two 'TextIter' for equality.
--
-- * 'TextIter' could be in class Eq and Ord if there is a guarantee
--   that each iterator is copied before it is modified in place. This is done
--   the next abstraction layer.
--
textIterEqual :: TextIter -> TextIter -> IO Bool
textIterEqual ti2 ti1 = liftM toBool $ {#call unsafe text_iter_equal#} ti1 ti2

-- | Compare two 'TextIter'.
--
-- * 'TextIter' could be in class Eq and Ord if there is a guarantee
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

--------------------
-- Attributes

-- | \'visibleLineOffset\' property. See 'textIterGetVisibleLineOffset' and
-- 'textIterSetVisibleLineOffset'
--
textIterVisibleLineOffset :: Attr TextIter Int
textIterVisibleLineOffset = newAttr
  textIterGetVisibleLineOffset
  textIterSetVisibleLineOffset

-- | \'offset\' property. See 'textIterGetOffset' and 'textIterSetOffset'
--
textIterOffset :: Attr TextIter Int
textIterOffset = newAttr
  textIterGetOffset
  textIterSetOffset

-- | \'lineOffset\' property. See 'textIterGetLineOffset' and
-- 'textIterSetLineOffset'
--
textIterLineOffset :: Attr TextIter Int
textIterLineOffset = newAttr
  textIterGetLineOffset
  textIterSetLineOffset

-- | \'line\' property. See 'textIterGetLine' and 'textIterSetLine'
--
textIterLine :: Attr TextIter Int
textIterLine = newAttr
  textIterGetLine
  textIterSetLine
