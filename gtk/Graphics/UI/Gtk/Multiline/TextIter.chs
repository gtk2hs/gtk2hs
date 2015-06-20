{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) TextIter TextBuffer
--
--  Author : Axel Simon, Andy Stewart
--
--  Created: 23 February 2002
--
--  Copyright (C) 2002-2005 Axel Simon
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
-- The following functions do not make sense due to Haskell's wide character
--   representation of Unicode:
--     gtk_text_iter_get_line_index
--     gtk_text_iter_get_visible_line_index
--     gtk_text_iter_get_bytes_in_line
--     gtk_text_iter_set_line_index
--     gtk_text_iter_set_visible_line_index
--
-- All offsets are counted from 0.
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
  TextIter,
  TextSearchFlags(..),

-- * Methods
  textIterCopy,
  textIterGetBuffer,
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
  textIterGetChildAnchor,
  textIterGetMarks,
  textIterGetToggledTags,
  textIterBeginsTag,
  textIterEndsTag,
  textIterTogglesTag,
  textIterHasTag,
  textIterGetTags,
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
  textIterGetAttributes,
  textIterGetLanguage,
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
  textIterInRange,
  textIterOrder,
#if GTK_CHECK_VERSION(2,8,0)
  textIterForwardVisibleLine,
  textIterBackwardVisibleLine,
  textIterForwardVisibleLines,
  textIterBackwardVisibleLines,
#endif
  textIterForwardVisibleWordEnds,
  textIterBackwardVisibleWordStarts,
  textIterForwardVisibleWordEnd,
  textIterBackwardVisibleWordStart,
  textIterForwardVisibleCursorPosition,
  textIterBackwardVisibleCursorPosition,
  textIterForwardVisibleCursorPositions,
  textIterBackwardVisibleCursorPositions,

-- * Attributes
  textIterVisibleLineOffset,
  textIterOffset,
  textIterLineOffset,
  textIterLine,
  ) where

import Control.Monad    (liftM)
import Data.Maybe       (fromMaybe)
import Data.Char        (chr)

import System.Glib.FFI
import System.Glib.Flags                (fromFlags)
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.GList
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.General.Enums    (TextSearchFlags(..))
{#import Graphics.UI.Gtk.Multiline.Types#}
{#import Graphics.UI.Gtk.Multiline.TextTag#}
{#import Graphics.Rendering.Pango.BasicTypes#}

{# context lib="gtk" prefix="gtk" #}

-- methods


-- | Return the 'TextBuffer' this iterator
-- is associated with.
--
textIterGetBuffer :: TextIter -> IO TextBuffer
textIterGetBuffer ti = makeNewGObject mkTextBuffer $
  {#call unsafe text_iter_get_buffer#} ti

-- | Returns the character offset of an iterator. Each character in a
-- 'TextBuffer' has an offset, starting with 0 for the first character in the
-- buffer. Use 'Graphics.UI.Gtk.Multiline.TextBuffer.textBufferGetIterAtOffset'
-- to convert an offset back into an iterator.
--
textIterGetOffset :: TextIter -> IO Int
textIterGetOffset ti = liftM fromIntegral $
  {#call unsafe text_iter_get_offset#} ti

-- | Returns the line number containing the iterator. Lines in a 'TextBuffer'
-- are numbered beginning with 0 for the first line in the buffer.
--
textIterGetLine :: TextIter -> IO Int
textIterGetLine ti = liftM fromIntegral $
  {#call unsafe text_iter_get_line#} ti

-- | Returns the character offset of the iterator, counting from the start of
-- a newline-terminated line. The first character on the line has offset 0.
--
textIterGetLineOffset :: TextIter -> IO Int
textIterGetLineOffset ti = liftM fromIntegral $
  {#call unsafe text_iter_get_line_offset#} ti

-- | Returns the offset in characters from the start of the line to the given
-- @iter@, not counting characters that are invisible due to tags with the
-- \"invisible\" flag toggled on.
--
textIterGetVisibleLineOffset :: TextIter -> IO Int
textIterGetVisibleLineOffset ti = liftM fromIntegral $
  {#call unsafe text_iter_get_visible_line_offset#} ti

-- | Returns the Unicode character at this iterator.
-- If the element at this iterator is a non-character
-- element, such as an image embedded in the buffer, the Unicode \"unknown\"
-- character 0xFFFC is returned. If invoked on the end iterator,
-- @Nothigng@ is returned.
--
textIterGetChar :: TextIter -> IO (Maybe Char)
textIterGetChar ti = do
  res <- liftM fromIntegral $ {#call unsafe text_iter_get_char#} ti
  return $ if res==0 then Nothing else Just (chr res)

-- | Returns the text in the given range. A \"slice\" is a list of
-- characters, including the Unicode \"unknown\"
-- character 0xFFFC for iterable non-character elements in the buffer, such as
-- images. Because images are encoded in the slice, offsets
-- in the returned array will correspond to offsets in the text buffer.
-- Note that 0xFFFC can occur in normal text as well, so it is not a reliable
-- indicator that a pixbuf or widget is in the buffer.
--
textIterGetSlice :: GlibString string => TextIter -> TextIter -> IO string
textIterGetSlice end start = do
  cStr <- {#call text_iter_get_slice#} start end
  str <- peekUTFString cStr
  {#call unsafe g_free#} (castPtr cStr)
  return str

-- | Return the text in a given range.
--
-- * Pictures (and other objects) are stripped form the output. Thus, this
--   function does not preserve offsets.
--
textIterGetText :: GlibString string => TextIter -> TextIter -> IO string
textIterGetText start end = do
  cStr <- {#call text_iter_get_text#} start end
  str <- peekUTFString cStr
  {#call unsafe g_free#} (castPtr cStr)
  return str

-- | Like 'textIterGetSlice', but invisible text is not included. Invisible
-- text is usually invisible because a 'TextTag' with the \"invisible\"
-- attribute turned on has been applied to it.
--
textIterGetVisibleSlice :: GlibString string => TextIter -> TextIter -> IO string
textIterGetVisibleSlice start end = do
  cStr <- {#call text_iter_get_visible_slice#} start end
  str <- peekUTFString cStr
  {#call unsafe g_free#} (castPtr cStr)
  return str

-- | Like 'textIterGetText', but invisible text is not included. Invisible
-- text is usually invisible because a 'TextTag' with the \"invisible\"
-- attribute turned on has been applied to it.
--
textIterGetVisibleText :: GlibString string => TextIter -> TextIter -> IO string
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

-- | If the location at @iter@ contains a child anchor,
-- the anchor is returned (with no new reference count added).
-- Otherwise, @Nothing@ is returned.
--
textIterGetChildAnchor :: TextIter -> IO (Maybe TextChildAnchor)
textIterGetChildAnchor it = do
  tcaPtr <- {#call unsafe text_iter_get_child_anchor#} it
  if tcaPtr == nullPtr then return Nothing else liftM Just $
    makeNewGObject mkTextChildAnchor (return tcaPtr)

-- | Returns a list of all 'TextMark' at this location. Because marks are not
-- iterable (they don't take up any \"space\" in the buffer, they are just
-- marks in between iterable locations), multiple marks can exist in the same
-- place. The returned list is not in any meaningful order.
--
textIterGetMarks :: TextIter
 -> IO [TextMark] -- ^ returns list of 'TextMark'
textIterGetMarks self =
  {# call gtk_text_iter_get_marks #}
    self
  >>= fromGSList
  >>= mapM (\tm -> makeNewGObject mkTextMark (return tm))

-- | Returns a list of 'TextTag' that are toggled on or off at this point. (If
-- @toggledOn@ is @True@, the list contains tags that are toggled on.) If a tag
-- is toggled on at @iter@, then some non-empty range of characters following
-- @iter@ has that tag applied to it. If a tag is toggled off, then some
-- non-empty range following @iter@ does /not/ have the tag applied to it.
--
textIterGetToggledTags :: TextIter
 -> Bool                    -- ^ @toggledOn@ - @True@ to get toggled-on tags
 -> IO [TextTag] -- ^ returns tags toggled at this point
textIterGetToggledTags self toggledOn =
  {# call gtk_text_iter_get_toggled_tags #}
    self
    (fromBool toggledOn)
  >>= fromGSList
  >>= mapM (\tt -> makeNewGObject mkTextTag (return tt))

-- | Returns @True@ if @tag@ is toggled on at exactly this point. If @tag@ is
-- @Nothing@,
-- returns @True@ if any tag is toggled on at this point. Note that the
-- 'textIterBeginsTag' returns @True@ if @iter@ is the /start/ of the tagged
-- range; 'textIterHasTag' tells you whether an iterator is /within/ a tagged
-- range.
--
textIterBeginsTag :: TextIter -> Maybe TextTag -> IO Bool
textIterBeginsTag ti (Just tt) = liftM toBool $
  {#call unsafe text_iter_begins_tag#} ti tt
textIterBeginsTag ti Nothing = liftM toBool $
  {#call unsafe text_iter_begins_tag#} ti (TextTag nullForeignPtr)

-- | Returns @True@ if @tag@ is toggled off at exactly this point. If @tag@ is
-- @Notihng@,
-- returns @True@ if any tag is toggled off at this point. Note that the
-- 'textIterEndsTag' returns @True@ if @iter@ is the /end/ of the tagged range;
-- 'textIterHasTag' tells you whether an iterator is /within/ a tagged range.
--
textIterEndsTag :: TextIter -> Maybe TextTag -> IO Bool
textIterEndsTag ti (Just tt) = liftM toBool $
  {#call unsafe text_iter_ends_tag#} ti tt
textIterEndsTag ti Nothing = liftM toBool $
  {#call unsafe text_iter_ends_tag#} ti (TextTag nullForeignPtr)

-- | Query if the 'TextIter' is at the
-- beginning or the end of a 'TextTag'. This is equivalent to
-- ('textIterBeginsTag' || 'textIterEndsTag'), i.e. it
-- tells you whether a range with @tag@ applied to it begins /or/ ends at
-- @iter@.
--
textIterTogglesTag :: TextIter -> Maybe TextTag -> IO Bool
textIterTogglesTag ti (Just tt) = liftM toBool $
  {#call unsafe text_iter_toggles_tag#} ti tt
textIterTogglesTag ti Nothing = liftM toBool $
  {#call unsafe text_iter_toggles_tag#} ti (TextTag nullForeignPtr)

-- | Check if 'TextIter' is within a range
-- tagged with tag.
--
textIterHasTag :: TextIter -> Maybe TextTag -> IO Bool
textIterHasTag ti (Just tt) = liftM toBool $
  {#call unsafe text_iter_has_tag#} ti tt
textIterHasTag ti Nothing = liftM toBool $
  {#call unsafe text_iter_has_tag#} ti (TextTag nullForeignPtr)

-- | Returns a list of tags that apply to @iter@, in ascending order of
-- priority (highest-priority tags are last).
--
textIterGetTags :: TextIter
 -> IO [TextTag] -- ^ returns list of 'TextTag'
textIterGetTags self =
  {# call gtk_text_iter_get_tags #}
    self
  >>= fromGSList
  >>= mapM (\tt -> makeNewGObject mkTextTag (return tt))


-- | Returns whether the character at @iter@ is within an editable region of
-- text. Non-editable text is \"locked\" and can't be changed by the user via
-- 'TextView'. This function is simply a convenience wrapper around
-- 'textIterGetAttributes'. If no tags applied to this text affect editability,
-- @defaultSetting@ will be returned.
--
-- You don't want to use this function to decide whether text can be
-- inserted at @iter@, because for insertion you don't want to know whether the
-- char at @iter@ is inside an editable range, you want to know whether a new
-- character inserted at @iter@ would be inside an editable range. Use
-- 'textIterCanInsert' to handle this case.
--
textIterEditable :: TextIter -> Bool -> IO Bool
textIterEditable ti def = liftM toBool $
  {#call unsafe text_iter_editable#} ti (fromBool def)

-- | Check if new text can be inserted at 'TextIter'.
--
-- * Considering the default editability of the buffer, and tags that affect
--   editability, determines whether text inserted at @iter@ would be editable.
--   If text inserted at @iter@ would be editable then the user should be allowed
--   to insert text at @iter@.
--   'Graphics.UI.Gtk.Multiline.TextBuffer.textBufferInsertInteractive'
--   uses this function
--   to decide whether insertions are allowed at a given position.
--
-- * Use 'Graphics.UI.Gtk.Multiline.TextBuffer.textBufferInsertInteractive'
-- if you want to insert text depending on the current editable status.
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

-- | Returns @True@ if @iter@ points to the start of the paragraph delimiter
-- characters for a line (delimiters will be either a newline, a carriage
-- return, a carriage return followed by a newline, or a Unicode paragraph
-- separator character). Note that an iterator pointing to the \n of a \r\n
-- pair will not be counted as the end of a line, the line ends before the \r.
-- The end iterator is considered to be at the end of a line, even though there
-- are no paragraph delimiter chars there.
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

-- | Computes the effect of any tags applied to this spot in the text.
-- The values parameter should be initialized to the default settings you wish to use if no tags are in effect.
-- You'd typically obtain the defaults from 'textViewGetDefaultAttributes'.
-- 'textIterGetAttributes' will modify values, applying the effects of any tags present at iter.
-- If any tags affected values, the function returns @True@.
--
textIterGetAttributes :: TextIter -> TextAttributes -> IO Bool
textIterGetAttributes ti ta = liftM toBool $
  {#call unsafe text_iter_get_attributes#} ti ta

-- | A convenience wrapper around 'textIterGetAttributes', which returns the language in effect at iter.
-- If no tags affecting language apply to iter, the return value is identical to that of 'getDefaultLanguage'.
--
textIterGetLanguage :: TextIter -> IO Language
textIterGetLanguage ti = liftM Language $
  {#call unsafe text_iter_get_language#} ti

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
-- * Retuns @True@ if the iterator is pointing to a new character (and @False@ if
--   the iterator points to a picture or has not moved).
--
-- *  Note that images embedded
-- in the buffer occupy 1 character slot, so 'textIterForwardChar' may actually
-- move onto an image instead of a character.
--
textIterForwardChars :: TextIter -> Int -> IO Bool
textIterForwardChars ti n = liftM toBool $
  {#call unsafe text_iter_forward_chars#} ti (fromIntegral n)

-- | Move 'TextIter' backwards by
-- @n@ characters.
--
-- * Retuns @True@ if the iterator is pointing to a new character (and @False@ if
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
-- * If number is negative or larger than the number of lines in the buffer,
--   moves @iter@ to the start of the last line in the buffer.
--
textIterSetLine :: TextIter -> Int -> IO ()
textIterSetLine ti n =
  {#call unsafe text_iter_set_line#} ti (fromIntegral n)

-- | Set 'TextIter' to an offset within the line.
--
-- * The
--   given character offset must be less than or equal to the number of
--   characters in the line; if equal, the iterator moves to the start of the
--   next line.
--
textIterSetLineOffset :: TextIter -> Int -> IO ()
textIterSetLineOffset ti n =
  {#call unsafe text_iter_set_line_offset#} ti (fromIntegral n)

-- | Like 'textIterSetLineOffset', but the offset is in visible characters,
-- i.e. text with a tag making it invisible is not counted in the offset.
--
textIterSetVisibleLineOffset :: TextIter -> Int -> IO ()
textIterSetVisibleLineOffset ti n =
  {#call unsafe text_iter_set_visible_line_offset#} ti (fromIntegral n)

-- | Moves @iter@ forward to the \"end iterator,\" which points one past the
-- last valid character in the buffer.
--
textIterForwardToEnd :: TextIter -> IO ()
textIterForwardToEnd ti = {#call unsafe text_iter_forward_to_end#} ti

-- | Moves the iterator to point to the paragraph delimiter characters, which
-- will be either a newline, a carriage return, a carriage return\/newline in
-- sequence, or the Unicode paragraph separator character. If the iterator is
-- already at the paragraph delimiter characters, moves to the paragraph
-- delimiter characters for the next line. If @iter@ is on the last line in the
-- buffer, which does not end in paragraph delimiters, moves to the end
-- iterator (end of the last line), and returns @False@.
--
textIterForwardToLineEnd :: TextIter -> IO Bool
textIterForwardToLineEnd ti = liftM toBool $
  {#call unsafe text_iter_forward_to_line_end#} ti

-- | Moves 'TextIter' forward to
-- the next change of a 'TextTag'.
--
-- * If Nothing is supplied, any 'TextTag' will be matched.
--
-- * Returns @True@ if there was a tag toggle after 'TextIter'.
--
textIterForwardToTagToggle :: TextIter -> Maybe TextTag -> IO Bool
textIterForwardToTagToggle ti tt = liftM toBool $
  {#call unsafe text_iter_forward_to_tag_toggle#} ti
    (fromMaybe (TextTag nullForeignPtr) tt)

-- | Moves 'TextIter' backward to
-- the next change of a 'TextTag'.
--
-- * If @Nothing@ is supplied, any 'TextTag' will be matched.
--
-- * Returns @True@ if there was a tag toggle before 'TextIter'.
--
textIterBackwardToTagToggle :: TextIter -> Maybe TextTag -> IO Bool
textIterBackwardToTagToggle ti tt = liftM toBool $
  {#call unsafe text_iter_backward_to_tag_toggle#} ti
    (fromMaybe (TextTag nullForeignPtr) tt)

{#pointer TextCharPredicate#}

foreign import ccall "wrapper" mkTextCharPredicate ::
  ({#type gunichar#} -> Ptr () -> IO {#type gboolean#}) -> IO TextCharPredicate

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
  fPtr <- mkTextCharPredicate (\c _ -> return $ fromBool $ pred (chr (fromIntegral c)))
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
  fPtr <- mkTextCharPredicate (\c _ -> return $ fromBool $ pred (chr (fromIntegral c)))
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
textIterForwardSearch :: GlibString string => TextIter -> string -> [TextSearchFlags] ->
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
textIterBackwardSearch :: GlibString string => TextIter -> string -> [TextSearchFlags] ->
                          Maybe TextIter -> IO (Maybe (TextIter, TextIter))
textIterBackwardSearch ti str flags limit = do
  start  <- makeEmptyTextIter
  end <- makeEmptyTextIter
  found <- liftM toBool $ withUTFString str $ \cStr ->
    {#call unsafe text_iter_backward_search#} ti cStr
    ((fromIntegral.fromFlags) flags) start end
    (fromMaybe (TextIter nullForeignPtr) limit)
  return $ if found then Just (start,end) else Nothing

#if GTK_CHECK_VERSION(2,8,0)
-- | Moves @iter@ to the start of the next visible line. Returns @True@ if
-- there was a next line to move to, and @False@ if @iter@ was simply moved to
-- the end of the buffer and is now not dereferenceable, or if @iter@ was
-- already at the end of the buffer.
--
-- * Available since Gtk+ version 2.8
--
textIterForwardVisibleLine :: TextIter
 -> IO Bool  -- ^ returns whether @iter@ can be dereferenced
textIterForwardVisibleLine self =
  liftM toBool $
  {# call gtk_text_iter_forward_visible_line #}
    self

-- | Moves @iter@ to the start of the previous visible line. Returns @True@ if
-- @iter@ could be moved; i.e. if @iter@ was at character offset 0, this
-- function returns @False@. Therefore if @iter@ was already on line 0, but not
-- at the start of the line, @iter@ is snapped to the start of the line and the
-- function returns @True@. (Note that this implies that in a loop calling this
-- function, the line number may not change on every iteration, if your first
-- iteration is on line 0.)
--
-- * Available since Gtk+ version 2.8
--
textIterBackwardVisibleLine :: TextIter
 -> IO Bool  -- ^ returns whether @iter@ moved
textIterBackwardVisibleLine self =
  liftM toBool $
  {# call gtk_text_iter_backward_visible_line #}
    self

-- | Moves @count@ visible lines forward, if possible (if @count@ would move
-- past the start or end of the buffer, moves to the start or end of the
-- buffer). The return value indicates whether the iterator moved onto a
-- dereferenceable position; if the iterator didn't move, or moved onto the end
-- iterator, then @False@ is returned. If @count@ is 0, the function does
-- nothing and returns @False@. If @count@ is negative, moves backward by 0 -
-- @count@ lines.
--
-- * Available since Gtk+ version 2.8
--
textIterForwardVisibleLines :: TextIter
 -> Int      -- ^ @count@ - number of lines to move forward
 -> IO Bool  -- ^ returns whether @iter@ moved and is dereferenceable
textIterForwardVisibleLines self count =
  liftM toBool $
  {# call gtk_text_iter_forward_visible_lines #}
    self
    (fromIntegral count)

-- | Moves @count@ visible lines backward, if possible (if @count@ would move
-- past the start or end of the buffer, moves to the start or end of the
-- buffer). The return value indicates whether the iterator moved onto a
-- dereferenceable position; if the iterator didn't move, or moved onto the end
-- iterator, then @False@ is returned. If @count@ is 0, the function does
-- nothing and returns @False@. If @count@ is negative, moves forward by 0 -
-- @count@ lines.
--
-- * Available since Gtk+ version 2.8
--
textIterBackwardVisibleLines :: TextIter
 -> Int      -- ^ @count@ - number of lines to move backward
 -> IO Bool  -- ^ returns whether @iter@ moved and is dereferenceable
textIterBackwardVisibleLines self count =
  liftM toBool $
  {# call gtk_text_iter_backward_visible_lines #}
    self
    (fromIntegral count)
#endif

-- | Calls 'textIterForwardVisibleWordEnd' up to count times.
--
textIterForwardVisibleWordEnds :: TextIter
 -> Int   -- ^ @couter@ - number of times to move
 -> IO Bool -- ^ return @True@ if iter moved and is not the end iterator
textIterForwardVisibleWordEnds self count =
  liftM toBool $
  {# call text_iter_forward_visible_word_ends #}
    self
    (fromIntegral count)

-- | Calls 'textIterBackwardVisibleWordStart' up to count times.
--
textIterBackwardVisibleWordStarts :: TextIter
 -> Int   -- ^ @couter@ - number of times to move
 -> IO Bool -- ^ return @True@ if iter moved and is not the end iterator
textIterBackwardVisibleWordStarts self count =
  liftM toBool $
  {# call text_iter_backward_visible_word_starts #}
    self
    (fromIntegral count)

-- | Moves forward to the next visible word end.
-- (If iter is currently on a word end, moves forward to the next one after that.)
-- Word breaks are determined by Pango and should be correct for nearly any language
-- (if not, the correct fix would be to the Pango word break algorithms).
--
textIterForwardVisibleWordEnd :: TextIter
 -> IO Bool -- ^ return @True@ if iter moved and is not the end iterator
textIterForwardVisibleWordEnd self =
  liftM toBool $
  {# call text_iter_forward_visible_word_end #}
    self

-- | Moves backward to the previous visible word start.
-- (If iter is currently on a word start, moves backward to the next one after that.)
-- Word breaks are determined by Pango and should be correct for nearly any language
-- (if not, the correct fix would be to the Pango word break algorithms).
--
textIterBackwardVisibleWordStart :: TextIter
 -> IO Bool -- ^ return @True@ if iter moved and is not the end iterator
textIterBackwardVisibleWordStart self =
  liftM toBool $
  {# call text_iter_backward_visible_word_start #}
    self

-- | Moves iter forward to the next visible cursor position.
-- See 'textIterForwardCursorPosition' for details.
--
textIterForwardVisibleCursorPosition :: TextIter
 -> IO Bool -- ^ return @True@ if iter moved and is not the end iterator
textIterForwardVisibleCursorPosition self =
  liftM toBool $
  {# call text_iter_forward_visible_cursor_position #}
    self

-- | Moves iter forward to the previous visible cursor position.
-- See 'textIterBackwardCursorPosition' for details.
--
textIterBackwardVisibleCursorPosition :: TextIter
 -> IO Bool -- ^ return @True@ if iter moved and is not the end iterator
textIterBackwardVisibleCursorPosition self =
  liftM toBool $
  {# call text_iter_backward_visible_cursor_position #}
    self

-- | Moves up to count visible cursor positions.
-- See 'textIterForwardCursorPosition' for details.
textIterForwardVisibleCursorPositions :: TextIter
 -> Int   -- ^ @couter@ - number of times to move
 -> IO Bool -- ^ return @True@ if iter moved and is not the end iterator
textIterForwardVisibleCursorPositions self count =
  liftM toBool $
  {# call text_iter_forward_visible_cursor_positions #}
    self
    (fromIntegral count)

-- | Moves up to count visible cursor positions.
-- See 'textIterBackwardCursorPosition' for details.
--
textIterBackwardVisibleCursorPositions :: TextIter
 -> Int   -- ^ @couter@ - number of times to move
 -> IO Bool -- ^ return @True@ if iter moved and is not the end iterator
textIterBackwardVisibleCursorPositions self count =
  liftM toBool $
  {# call text_iter_backward_visible_cursor_positions #}
    self
    (fromIntegral count)

-- | Compare two 'TextIter' for equality.
--
textIterEqual :: TextIter -> TextIter -> IO Bool
textIterEqual ti2 ti1 = liftM toBool $ {#call unsafe text_iter_equal#} ti1 ti2

-- | Compare two 'TextIter'.
--
textIterCompare :: TextIter -> TextIter -> IO Ordering
textIterCompare ti2 ti1 = do
  res <- {#call unsafe text_iter_compare#} ti1 ti2
  return $ case res of
    (-1)   -> LT
    0      -> EQ
    1      -> GT

-- | Checks whether iter falls in the range [start, end).
-- start and end must be in ascending order.
--
textIterInRange :: TextIter
 -> TextIter -- ^ @start@ start of range
 -> TextIter -- ^ @end@ end of range
 -> IO Bool  -- ^ @True@ if iter is in the range
textIterInRange ti start end = liftM toBool $
  {# call unsafe text_iter_in_range #} ti start end

-- | Swaps the value of first and second if second comes before first in the buffer.
-- That is, ensures that first and second are in sequence.
-- Most text buffer functions that take a range call this automatically on your behalf, so there's no real reason to call it yourself in those cases.
-- There are some exceptions, such as 'textIterInRange', that expect a pre-sorted range.
--
textIterOrder :: TextIter -> TextIter -> IO ()
textIterOrder first second =
  {# call text_iter_order #} first second

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
