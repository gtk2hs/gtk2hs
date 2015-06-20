{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget EntryBuffer
--
--  Author : Andy Stewart
--
--  Created: 22 Mar 2010
--
--  Copyright (C) 2010 Andy Stewart
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
-- Text buffer for 'Entry'
--
-- * Module available since Gtk+ version 2.18
--
module Graphics.UI.Gtk.Entry.EntryBuffer (

-- * Detail
--
-- | The 'EntryBuffer' class contains the actual text displayed in a 'Entry'
-- widget.
--
-- A single 'EntryBuffer' object can be shared by multiple 'Entry' widgets
-- which will then share the same text content, but not the cursor position,
-- visibility attributes, icon etc.
--
-- 'EntryBuffer' may be derived from. Such a derived class might allow text
-- to be stored in an alternate location, such as non-pageable memory, useful
-- in the case of important passwords. Or a derived class could integrate with
-- an application's concept of undo\/redo.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----EntryBuffer
-- @

#if GTK_CHECK_VERSION(2,18,0)
-- * Types
  EntryBuffer,
  EntryBufferClass,
  castToEntryBuffer,
  toEntryBuffer,

-- * Constructors
  entryBufferNew,

-- * Methods
  entryBufferGetBytes,
  entryBufferInsertText,
  entryBufferDeleteText,
  entryBufferEmitDeletedText,
  entryBufferEmitInsertedText,

-- * Attributes
  entryBufferText,
  entryBufferLength,
  entryBufferMaxLength,

-- * Signals
  entryBufferInsertedText,
  entryBufferDeletedText,
#endif
  ) where

import Control.Monad    (liftM)
import Data.Maybe       (fromJust)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,18,0)
--------------------
-- Constructors

-- | Create a new 'EntryBuffer' object.
--
-- Optionally, specify initial text to set in the buffer.
--
-- * Available since Gtk+ version 2.18
--
entryBufferNew :: GlibString string
 => Maybe string                -- ^ @initialChars@ - initial buffer text or 'Nothing'
 -> IO EntryBuffer
entryBufferNew initialChars =
  wrapNewGObject mkEntryBuffer $
  maybeWith withUTFString initialChars $ \initialCharsPtr -> do
    let chars = if initialCharsPtr == nullPtr
                   then (-1)
                   else stringLength $ fromJust initialChars
    {# call gtk_entry_buffer_new #}
      initialCharsPtr
      (fromIntegral chars)

--------------------
-- Methods

-- | Retrieves the length in bytes of the buffer. See 'entryBufferGetLength'.
--
-- * Available since Gtk+ version 2.18
--
entryBufferGetBytes :: EntryBufferClass self => self
 -> IO Int -- ^ returns The byte length of the buffer.
entryBufferGetBytes self =
  liftM fromIntegral $
  {# call gtk_entry_buffer_get_bytes #}
    (toEntryBuffer self)

-- | Inserts @chars@ into the contents of the buffer,
-- at position @position@.
--
-- * Available since Gtk+ version 2.18
--
entryBufferInsertText :: (EntryBufferClass self, GlibString string) => self
 -> Int    -- ^ @position@ - the position at which to insert text.
 -> string -- ^ @chars@ - the text to insert into the buffer.
 -> IO Int -- ^ returns The number of characters actually inserted.
entryBufferInsertText self position chars =
  liftM fromIntegral $
  withUTFStringLen chars $ \ (charsPtr, len) ->
  {# call gtk_entry_buffer_insert_text #}
    (toEntryBuffer self)
    (fromIntegral position)
    charsPtr
    (fromIntegral len)

-- | Deletes a sequence of characters from the buffer. @nChars@ characters are
-- deleted starting at @position@. If @nChars@ is negative, then all characters
-- until the end of the text are deleted.
--
-- * Available since Gtk+ version 2.18
--
entryBufferDeleteText :: EntryBufferClass self => self
 -> Int    -- ^ @position@ - position at which to delete text
 -> Int    -- ^ @nChars@ - number of characters to delete
 -> IO Int -- ^ returns The number of characters deleted.
entryBufferDeleteText self position nChars =
  liftM fromIntegral $
  {# call gtk_entry_buffer_delete_text #}
    (toEntryBuffer self)
    (fromIntegral position)
    (fromIntegral nChars)

-- | Used when subclassing 'EntryBuffer'
--
-- * Available since Gtk+ version 2.18
--
entryBufferEmitDeletedText :: EntryBufferClass self => self
 -> Int -- ^ @position@ - position at which text was deleted
 -> Int -- ^ @nChars@ - number of characters deleted
 -> IO ()
entryBufferEmitDeletedText self position nChars =
  {# call gtk_entry_buffer_emit_deleted_text #}
    (toEntryBuffer self)
    (fromIntegral position)
    (fromIntegral nChars)

-- | Used when subclassing 'EntryBuffer'
--
-- * Available since Gtk+ version 2.18
--
entryBufferEmitInsertedText :: (EntryBufferClass self, GlibString string) => self
 -> Int    -- ^ @position@ - position at which text was inserted
 -> string -- ^ @chars@ - text that was inserted
 -> Int    -- ^ @nChars@ - number of characters inserted
 -> IO ()
entryBufferEmitInsertedText self position chars nChars =
  withUTFString chars $ \charsPtr ->
  {# call gtk_entry_buffer_emit_inserted_text #}
    (toEntryBuffer self)
    (fromIntegral position)
    charsPtr
    (fromIntegral nChars)

--------------------
-- Attributes

-- | The contents of the buffer.
--
-- Default value: \"\"
--
-- * Available since Gtk+ version 2.18
--
entryBufferText :: (EntryBufferClass self, GlibString string) => Attr self string
entryBufferText = newAttrFromStringProperty "text"

-- | The length of the text in buffer.
--
-- Allowed values: <= 65535
--
-- Default value: 0
--
-- * Available since Gtk+ version 2.18
--
entryBufferLength :: EntryBufferClass self => ReadAttr self Int
entryBufferLength = readAttrFromIntProperty "length"

-- | The maximum length of the text in the buffer.
--
-- Allowed values: [0,65535]
--
-- Default value: 0
--
-- * Available since Gtk+ version 2.18
--
entryBufferMaxLength :: EntryBufferClass self => Attr self Int
entryBufferMaxLength = newAttrFromIntProperty "max-length"

--------------------
-- Signals

-- |
--
-- * Available since Gtk+ version 2.18
--
entryBufferInsertedText :: (EntryBufferClass self, GlibString string) => Signal self (Int -> string -> Int -> IO ())
entryBufferInsertedText = Signal (connect_INT_GLIBSTRING_INT__NONE "inserted_text")

-- |
--
-- * Available since Gtk+ version 2.18
--
entryBufferDeletedText :: EntryBufferClass self => Signal self (Int -> Int -> IO ())
entryBufferDeletedText = Signal (connect_INT_INT__NONE "deleted_text")

#endif
