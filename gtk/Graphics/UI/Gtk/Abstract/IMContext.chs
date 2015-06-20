{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget IMContext
--
--  Author : Colin McQuillan
--
--  Created: 30 April 2009
--
--  Copyright (C) 2009 Colin McQuillan
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
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Base class for input method contexts
--
module Graphics.UI.Gtk.Abstract.IMContext (

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----IMContext
-- |         +----'IMContextSimple'
-- |         +----'IMMulticontext'
-- @

-- * Types
  IMContext,
  IMContextClass,
  castToIMContext, gTypeIMContext,
  toIMContext,

-- * Methods
  imContextSetClientWindow,
  imContextGetPreeditString,
  imContextFilterKeypress,
  imContextFocusIn,
  imContextFocusOut,
  imContextReset,
  imContextSetCursorLocation,
  imContextSetUsePreedit,
  imContextSetSurrounding,
  imContextGetSurrounding,
  imContextDeleteSurrounding,

-- * Signals
  imContextPreeditStart,
  imContextPreeditEnd,
  imContextPreeditChanged,
  imContextCommit,
  imContextRetrieveSurrounding,
  imContextDeleteSurrounding',
  ) where

import Control.Monad    (liftM)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)

import System.Glib.FFI
import System.Glib.UTFString (readUTFString, withUTFString, genUTFOfs,
                              ofsToUTF, ofsFromUTF, GlibString)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.Gdk.EventM (EventM, EKey)
import Graphics.UI.Gtk.General.Structs (Rectangle)
import Graphics.Rendering.Pango.Enums (PangoAttribute)
import Graphics.Rendering.Pango.Attributes (readAttrList)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Set the client window for the input context; this is the 'DrawWindow' in
-- which the input appears. This window is used in order to correctly position
-- status windows, and may also be used for purposes internal to the input
-- method.
--
imContextSetClientWindow :: IMContextClass self => self
 -> Maybe DrawWindow -- ^ @window@ - the client window. 'Nothing' indicates
                     -- that the previous client window no longer exists.
 -> IO ()
imContextSetClientWindow self window =
  {# call im_context_set_client_window #}
    (toIMContext self)
    (fromMaybe (DrawWindow nullForeignPtr) window)

-- | Retrieve the current preedit string for the input context, and a list of
-- attributes to apply to the string. This string should be displayed inserted
-- at the insertion point.
--
imContextGetPreeditString :: (IMContextClass self, GlibString string) => self
 -> IO (string, [[PangoAttribute]], Int)
                    -- ^ @(str, attrs, cursorPos)@ Retrieved string,
                    -- attributes to apply to the string, position of cursor.
imContextGetPreeditString self =
  alloca $ \strPtr ->
  alloca $ \attrListPtr ->
  alloca $ \cursorPosPtr ->
  {# call im_context_get_preedit_string #}
    (toIMContext self)
    strPtr
    attrListPtr
    cursorPosPtr
  >>
  peek strPtr >>= readUTFString >>= \str ->
  peek attrListPtr >>= readAttrList (genUTFOfs str) >>= \attrs ->
  peek cursorPosPtr >>= \cursorPos ->
  return (str, attrs, fromIntegral cursorPos)

-- | Allow an input method to internally handle key press and release events.
-- If this function returns @True@, then no further processing should be done
-- for this key event.
--
imContextFilterKeypress :: IMContextClass self => self
 -> EventM EKey Bool -- ^ returns @True@ if the input method handled the key
                     -- event.
imContextFilterKeypress self =
  liftM toBool $
  ask >>= \eventPtr ->
  liftIO $
  {# call im_context_filter_keypress #}
    (toIMContext self)
    (castPtr eventPtr)

-- | Notify the input method that the widget to which this input context
-- corresponds has gained focus. The input method may, for example, change the
-- displayed feedback to reflect this change.
--
imContextFocusIn :: IMContextClass self => self -> IO ()
imContextFocusIn self =
  {# call im_context_focus_in #}
    (toIMContext self)

-- | Notify the input method that the widget to which this input context
-- corresponds has lost focus. The input method may, for example, change the
-- displayed feedback or reset the contexts state to reflect this change.
--
imContextFocusOut :: IMContextClass self => self -> IO ()
imContextFocusOut self =
  {# call im_context_focus_out #}
    (toIMContext self)

-- | Notify the input method that a change such as a change in cursor position
-- has been made. This will typically cause the input method to clear the
-- preedit state.
--
imContextReset :: IMContextClass self => self -> IO ()
imContextReset self =
  {# call im_context_reset #}
    (toIMContext self)

-- | Notify the input method that a change in cursor position has been made.
-- The location is relative to the client window.
--
imContextSetCursorLocation :: IMContextClass self => self
 -> Rectangle -- ^ @area@ - new location
 -> IO ()
imContextSetCursorLocation self area =
  with area $ \areaPtr ->
  {# call im_context_set_cursor_location #}
    (toIMContext self)
    (castPtr areaPtr)

-- | Sets whether the IM context should use the preedit string to display
-- feedback. If @usePreedit@ is @False@ (default is @True@), then the IM
-- context may use some other method to display feedback, such as displaying it
-- in a child of the root window.
--
imContextSetUsePreedit :: IMContextClass self => self
 -> Bool -- ^ @usePreedit@ - whether the IM context should use the preedit
         -- string.
 -> IO ()
imContextSetUsePreedit self usePreedit =
  {# call im_context_set_use_preedit #}
    (toIMContext self)
    (fromBool usePreedit)

-- | Sets surrounding context around the insertion point and preedit string.
-- This function is expected to be called in response to the
-- 'imContextRetrieveSurrounding' signal, and will likely have no effect if
-- called at other times.
--
imContextSetSurrounding :: (IMContextClass self, GlibString string) => self
 -> string -- ^ @text@ - text surrounding the insertion point, as UTF-8. the
           -- preedit string should not be included within @text@.
 -> Int    -- ^ @cursorIndex@ - the index of the insertion cursor within
           -- @text@.
 -> IO ()
imContextSetSurrounding self text cursorIndex =
  withUTFString text $ \textPtr ->
  {# call im_context_set_surrounding #}
    (toIMContext self)
    textPtr
    (-1)
    (fromIntegral (ofsToUTF cursorIndex (genUTFOfs text)))

-- | Retrieves context around the insertion point. Input methods typically
-- want context in order to constrain input text based on existing text; this
-- is important for languages such as Thai where only some sequences of
-- characters are allowed.
--
-- This function is implemented by emitting the
-- 'imContextRetrieveSurrounding' signal on the input method; in response to
-- this signal, a widget should provide as much context as is available, up to
-- an entire paragraph, by calling 'imContextSetSurrounding'. Note that there
-- is no obligation for a widget to respond to the 'imContextRetrieveSurrounding'
-- signal, so input methods must be prepared to function without context.
--
imContextGetSurrounding :: (IMContextClass self, GlibString string) => self
 -> IO (Maybe (string, Int)) -- ^ @Maybe (text,cursorIndex)@ Text holding
                             -- context around the insertion point and the
                             -- index of the insertion cursor within @text@.
                             -- 'Nothing' if  no surrounding text was
                             -- provided.
imContextGetSurrounding self =
  alloca $ \textPtr ->
  alloca $ \cursorIndexPtr ->
  {# call im_context_get_surrounding #}
    (toIMContext self)
    textPtr
    cursorIndexPtr >>= \provided ->
  if toBool provided then
      peek textPtr >>= readUTFString >>= \text ->
      peek cursorIndexPtr >>= \cursorIndex ->
      return (Just (text, ofsFromUTF (fromIntegral cursorIndex)
                                     (genUTFOfs text)))
  else
      return Nothing

-- | Asks the widget that the input context is attached to to delete
-- characters around the cursor position by emitting the
-- 'imContextDeleteSurrounding' signal.
--
-- In order to use this function, you should first call
-- 'imContextGetSurrounding' to get the current context, and call this function
-- immediately afterwards to make sure that you know what you are deleting. You
-- should also account for the fact that even if the signal was handled, the
-- input context might not have deleted all the characters that were requested
-- to be deleted.
--
-- This function is used by an input method that wants to make substitutions
-- in the existing text in response to new input. It is not useful for
-- applications.
--
imContextDeleteSurrounding :: IMContextClass self => self
 -> Int     -- ^ @offset@ - offset from cursor position in chars; a negative
            -- value means start before the cursor.
 -> Int     -- ^ @nChars@ - number of characters to delete.
 -> IO Bool -- ^ returns @True@ if the signal was handled.
imContextDeleteSurrounding self offset nChars =
  liftM toBool $
  {# call im_context_delete_surrounding #}
    (toIMContext self)
    (fromIntegral offset)
    (fromIntegral nChars)

--------------------
-- Signals

-- | This signal is emitted when a new preediting sequence starts.
--
imContextPreeditStart :: IMContextClass self => Signal self (IO ())
imContextPreeditStart = Signal (connect_NONE__NONE "preedit-start")

-- | This signal is emitted when a preediting sequence has been completed or
-- canceled.
--
imContextPreeditEnd :: IMContextClass self => Signal self (IO ())
imContextPreeditEnd = Signal (connect_NONE__NONE "preedit-end")

-- | This signal is emitted whenever the preedit sequence currently being
-- entered has changed. It is also emitted at the end of a preedit sequence,
-- in which case 'imContextGetPreeditString' returns the empty string.
--
imContextPreeditChanged :: IMContextClass self => Signal self (IO ())
imContextPreeditChanged = Signal (connect_NONE__NONE "preedit-changed")

-- | This signal is emitted when a complete input sequence has been
-- entered by the user. This can be a single character immediately after a
-- key press or the final result of preediting. Parameters:
--
-- @str@ - the completed character(s) entered by the user
imContextCommit :: (IMContextClass self, GlibString string) => Signal self (string -> IO ())
imContextCommit = Signal (connect_GLIBSTRING__NONE "commit")

-- | This signal is emitted when the input method requires the context
-- surrounding the cursor. The callback should set the input method
-- surrounding context by calling 'imContextSetSurrounding'.
--
-- Returns True if the signal was handled.
imContextRetrieveSurrounding :: IMContextClass self => Signal self (IO Bool)
imContextRetrieveSurrounding = Signal (connect_NONE__BOOL "retrieve-surrounding")

-- | This signal is emitted when the input method needs to delete all or part
-- of the context surrounding the cursor. Parameters:
--
-- @offset@ - the character offset from the cursor position of the text to be
-- deleted. A negative value indicates a position before the cursor.
--
-- @n_chars@ - the number of characters to be deleted.
--
-- Returns True if the signal was handled.
imContextDeleteSurrounding' :: IMContextClass self => Signal self (Int -> Int -> IO Bool)
imContextDeleteSurrounding' = Signal (connect_INT_INT__BOOL "delete-surrounding")
