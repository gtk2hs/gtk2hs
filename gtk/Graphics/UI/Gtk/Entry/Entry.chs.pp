-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Entry
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.8 $ from $Date: 2005/04/12 23:26:56 $
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
-- A single line text entry field
--
module Graphics.UI.Gtk.Entry.Entry (
-- * Detail
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
  entryGetMaxLength,
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

-- * Properties
  entryMaxLength,
  entryVisibility,
  entryHasFrame,
  entryInvisibleChar,
  entryActivatesDefault,
  entryWidthChars,
  entryText,
  entryAlignment,
  entryCompletion,

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
import Char	(ord, chr)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes		(Attr(..))
import System.Glib.GObject		(makeNewGObject)
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Interfaces

instance EditableClass Entry

--------------------
-- Constructors

-- | Creates a new 'Entry' widget.
--
entryNew :: IO Entry
entryNew =
  makeNewObject mkEntry $
  liftM (castPtr :: Ptr Widget -> Ptr Entry) $
  {# call unsafe entry_new #}

--------------------
-- Methods

-- | Sets the text in the widget to the given value, replacing the current
-- contents.
--
entrySetText :: EntryClass self => self -> String -> IO ()
entrySetText self text =
  withUTFString text $ \textPtr ->
  {# call entry_set_text #}
    (toEntry self)
    textPtr

-- | Retrieves the contents of the entry widget. See also 'editableGetChars'.
--
entryGetText :: EntryClass self => self -> IO String
entryGetText self =
  {# call entry_get_text #}
    (toEntry self)
  >>= peekUTFString

#ifndef DISABLE_DEPRECATED
-- | Appends the given text to the contents of the widget.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
entryAppendText :: EntryClass self => self -> String -> IO ()
entryAppendText self text =
  withUTFString text $ \textPtr ->
  {# call entry_append_text #}
    (toEntry self)
    textPtr

-- | Prepends the given text to the contents of the widget.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
entryPrependText :: EntryClass self => self -> String -> IO ()
entryPrependText self text =
  withUTFString text $ \textPtr ->
  {# call entry_prepend_text #}
    (toEntry self)
    textPtr
#endif

-- | Sets whether the contents of the entry are visible or not. When
-- visibility is set to @False@, characters are displayed as the invisible
-- char, and will also appear that way when the text in the entry widget is
-- copied elsewhere.
--
-- The default invisible char is the asterisk \'*\', but it can be changed
-- with 'entrySetInvisibleChar'.
--
entrySetVisibility :: EntryClass self => self
 -> Bool  -- ^ @visible@ - @True@ if the contents of the entry are displayed
          -- as plaintext.
 -> IO ()
entrySetVisibility self visible =
  {# call entry_set_visibility #}
    (toEntry self)
    (fromBool visible)

-- | Retrieves whether the text in @entry@ is visible. See
-- 'entrySetVisibility'.
--
entryGetVisibility :: EntryClass self => self
 -> IO Bool -- ^ returns @True@ if the text is currently visible
entryGetVisibility self =
  liftM toBool $
  {# call entry_get_visibility #}
    (toEntry self)

-- | Sets the character to use in place of the actual text when
-- 'entrySetVisibility' has been called to set text visibility to @False@. i.e.
-- this is the character used in \"password mode\" to show the user how many
-- characters have been typed. The default invisible char is an asterisk
-- (\'*\'). If you set the invisible char to @\'\\0\'@, then the user will get
-- no feedback at all; there will be no text on the screen as they type.
--
entrySetInvisibleChar :: EntryClass self => self -> Char -> IO ()
entrySetInvisibleChar self ch =
  {# call unsafe entry_set_invisible_char #}
    (toEntry self)
    ((fromIntegral . ord) ch)

-- | Retrieves the character displayed in place of the real characters for
-- entries with visisbility set to false. See 'entrySetInvisibleChar'.
--
entryGetInvisibleChar :: EntryClass self => self
 -> IO Char -- ^ returns the current invisible char, or @\'\\0\'@, if the
            -- entry does not show invisible text at all.
entryGetInvisibleChar self =
  liftM (chr . fromIntegral) $
  {# call unsafe entry_get_invisible_char #}
    (toEntry self)

-- | Sets the maximum allowed length of the contents of the widget. If the
-- current contents are longer than the given length, then they will be
-- truncated to fit.
--
entrySetMaxLength :: EntryClass self => self
 -> Int   -- ^ @max@ - the maximum length of the entry, or 0 for no maximum.
          -- (other than the maximum length of entries.) The value passed in
          -- will be clamped to the range 0-65536.
 -> IO ()
entrySetMaxLength self max =
  {# call entry_set_max_length #}
    (toEntry self)
    (fromIntegral max)

-- | Retrieves the maximum allowed length of the text in @entry@. See
-- 'entrySetMaxLength'.
--
entryGetMaxLength :: EntryClass self => self
 -> IO Int -- ^ returns the maximum allowed number of characters in 'Entry',
           -- or 0 if there is no maximum.
entryGetMaxLength self =
  liftM fromIntegral $
  {# call unsafe entry_get_max_length #}
    (toEntry self)

-- | Query whether pressing return will activate the default widget.
--
entryGetActivatesDefault :: EntryClass self => self
 -> IO Bool -- ^ returns @True@ if the entry will activate the default widget
entryGetActivatesDefault self =
  liftM toBool $
  {# call unsafe entry_get_activates_default #}
    (toEntry self)

-- | If @setting@ is @True@, pressing Enter in the @entry@ will activate the
-- default widget for the window containing the entry. This usually means that
-- the dialog box containing the entry will be closed, since the default widget
-- is usually one of the dialog buttons.
--
-- (For experts: if @setting@ is @True@, the entry calls
-- 'windowActivateDefault' on the window containing the entry, in the default
-- handler for the \"activate\" signal.)
--
-- This setting is useful in 'Dialog' boxes where enter should press the
-- default button.
--
entrySetActivatesDefault :: EntryClass self => self
 -> Bool  -- ^ @setting@ - @True@ to activate window's default widget on Enter
          -- keypress
 -> IO ()
entrySetActivatesDefault self setting =
  {# call entry_set_activates_default #}
    (toEntry self)
    (fromBool setting)

-- | Query if the text 'Entry' is displayed with a frame around it.
--
entryGetHasFrame :: EntryClass self => self
 -> IO Bool -- ^ returns whether the entry has a beveled frame
entryGetHasFrame self =
  liftM toBool $
  {# call unsafe entry_get_has_frame #}
    (toEntry self)

-- | Sets whether the entry has a beveled frame around it.
--
entrySetHasFrame :: EntryClass self => self -> Bool -> IO ()
entrySetHasFrame self setting =
  {# call entry_set_has_frame #}
    (toEntry self)
    (fromBool setting)

-- | Gets the value set by 'entrySetWidthChars'.
--
entryGetWidthChars :: EntryClass self => self
 -> IO Int -- ^ returns number of chars to request space for, or negative if
           -- unset
entryGetWidthChars self =
  liftM fromIntegral $
  {# call unsafe entry_get_width_chars #}
    (toEntry self)

-- | Changes the size request of the entry to be about the right size for
-- @nChars@ characters. Note that it changes the size /request/, the size can
-- still be affected by how you pack the widget into containers. If @nChars@ is
-- -1, the size reverts to the default entry size.
--
-- This setting is only considered when the widget formulates its size
-- request. Make sure that it is not mapped (shown) before you change this
-- value.
--
entrySetWidthChars :: EntryClass self => self
 -> Int   -- ^ @nChars@ - width in chars
 -> IO ()
entrySetWidthChars self nChars =
  {# call entry_set_width_chars #}
    (toEntry self)
    (fromIntegral nChars)

#if GTK_CHECK_VERSION(2,4,0)
-- | Sets the alignment for the contents of the entry. This controls the
-- horizontal positioning of the contents when the displayed text is shorter
-- than the width of the entry.
--
-- * Available since Gtk version 2.4
--
entrySetAlignment :: EntryClass self => self
 -> Float -- ^ @xalign@ - The horizontal alignment, from 0 (left) to 1
          -- (right). Reversed for RTL layouts
 -> IO ()
entrySetAlignment self xalign =
  {# call entry_set_alignment #}
    (toEntry self)
    (realToFrac xalign)

-- | Gets the value set by 'entrySetAlignment'.
--
-- * Available since Gtk version 2.4
--
entryGetAlignment :: EntryClass self => self
 -> IO Float -- ^ returns the alignment
entryGetAlignment self =
  liftM realToFrac $
  {# call unsafe entry_get_alignment #}
    (toEntry self)

-- | Sets the auxiliary completion object to use with the entry. All further
-- configuration of the completion mechanism is done on completion using the
-- 'EntryCompletion' API.
--
-- * Available since Gtk version 2.4
--
entrySetCompletion :: EntryClass self => self -> EntryCompletion -> IO ()
entrySetCompletion self completion =
  {# call gtk_entry_set_completion #}
    (toEntry self)
    completion

-- | Returns the auxiliary completion object currently in use by the entry.
--
-- * Available since Gtk version 2.4
--
entryGetCompletion :: EntryClass self => self
 -> IO EntryCompletion -- ^ returns The auxiliary completion object currently
                       -- in use by @entry@.
entryGetCompletion self =
  makeNewGObject mkEntryCompletion $
  {# call gtk_entry_get_completion #}
    (toEntry self)
#endif

--------------------
-- Properties

-- | Maximum number of characters for this entry. Zero if no maximum.
--
-- Allowed values: [0,65535]
--
-- Default value: 0
--
entryMaxLength :: EntryClass self => Attr self Int
entryMaxLength = Attr 
  entryGetMaxLength
  entrySetMaxLength

-- | @False@ displays the \"invisible char\" instead of the actual text
-- (password mode).
--
-- Default value: @True@
--
entryVisibility :: EntryClass self => Attr self Bool
entryVisibility = Attr 
  entryGetVisibility
  entrySetVisibility

-- | @False@ removes outside bevel from entry.
--
-- Default value: @True@
--
entryHasFrame :: EntryClass self => Attr self Bool
entryHasFrame = Attr 
  entryGetHasFrame
  entrySetHasFrame

-- | The character to use when masking entry contents (in \"password mode\").
--
-- Default value: \'*\'
--
entryInvisibleChar :: EntryClass self => Attr self Char
entryInvisibleChar = Attr 
  entryGetInvisibleChar
  entrySetInvisibleChar

-- | Whether to activate the default widget (such as the default button in a
-- dialog) when Enter is pressed.
--
-- Default value: @False@
--
entryActivatesDefault :: EntryClass self => Attr self Bool
entryActivatesDefault = Attr 
  entryGetActivatesDefault
  entrySetActivatesDefault

-- | Number of characters to leave space for in the entry.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
entryWidthChars :: EntryClass self => Attr self Int
entryWidthChars = Attr 
  entryGetWidthChars
  entrySetWidthChars

-- | The contents of the entry.
--
-- Default value: \"\"
--
entryText :: EntryClass self => Attr self String
entryText = Attr 
  entryGetText
  entrySetText

-- | \'alignment\' property. See 'entryGetAlignment' and 'entrySetAlignment'
--
entryAlignment :: EntryClass self => Attr self Float
entryAlignment = Attr 
  entryGetAlignment
  entrySetAlignment

-- | \'completion\' property. See 'entryGetCompletion' and
-- 'entrySetCompletion'
--
entryCompletion :: EntryClass self => Attr self EntryCompletion
entryCompletion = Attr 
  entryGetCompletion
  entrySetCompletion

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
