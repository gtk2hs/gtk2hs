{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Entry
--
--  Author : Axel Simon, Andy Stewart
--
--  Created: 23 May 2001
--
--  Copyright (C) 1999-2005 Axel Simon
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
  castToEntry, gTypeEntry,
  toEntry,

-- * Constructors
  entryNew,
#if GTK_CHECK_VERSION(2,18,0)
  entryNewWithBuffer,
#endif

-- * Methods
  entrySetText,
  entryGetText,
#if GTK_MAJOR_VERSION < 3
#ifndef DISABLE_DEPRECATED
  entryAppendText,
  entryPrependText,
#endif
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
#if GTK_CHECK_VERSION(3,2,0)
  entrySetPlaceholderText,
  entryGetPlaceholderText,
#endif
#if GTK_CHECK_VERSION(2,4,0)
  entrySetAlignment,
  entryGetAlignment,
  entrySetCompletion,
  entryGetCompletion,
#endif
#if GTK_CHECK_VERSION (2,18,0)
  entryGetBuffer,
  entrySetBuffer,
#endif
#if GTK_MAJOR_VERSION < 3
#if GTK_CHECK_VERSION(2,20,0)
  entryGetIconWindow,
  entryGetTextWindow,
#endif
#endif
#if GTK_CHECK_VERSION(2,22,0)
  entryImContextFilterKeypress,
  entryResetImContext,
#endif

-- * Attributes
  entryCursorPosition,
  entrySelectionBound,
  entryEditable,
  entryMaxLength,
  entryVisibility,
  entryHasFrame,
  entryInvisibleChar,
  entryActivatesDefault,
  entryWidthChars,
  entryScrollOffset,
  entryText,
#if GTK_CHECK_VERSION(3,2,0)
  entryPlaceholderText,
#endif
#if GTK_CHECK_VERSION(2,4,0)
  entryXalign,
  entryAlignment,
  entryCompletion,
#endif
#if GTK_CHECK_VERSION (2,18,0)
  entryBuffer,
#endif

-- * Signals
  entryActivated,
  entryActivate,
  entryBackspace,
  entryCopyClipboard,
  entryCutClipboard,
  entryPasteClipboard,
  entryDeleteFromCursor,
  entryInsertAtCursor,
  entryMoveCursor,
  entryPopulatePopup,
  entryToggleOverwirte,
  entryToggleOverwrite,
#if GTK_CHECK_VERSION(2,20,0)
  entryPreeditChanged,
#endif
#if GTK_CHECK_VERSION(2,16,0)
  entryIconPress,
  entryIconRelease,
#endif

-- * Deprecated
#ifndef DISABLE_DEPRECATED
  onEntryActivate,
  afterEntryActivate,
  onCopyClipboard,
  afterCopyClipboard,
  onCutClipboard,
  afterCutClipboard,
  onPasteClipboard,
  afterPasteClipboard,
  onToggleOverwrite,
  afterToggleOverwrite,
#endif
  ) where

import Control.Monad    (liftM)
import Control.Monad.Reader (runReaderT)
import Data.Char        (ord, chr)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
import Graphics.UI.Gtk.General.Enums (DeleteType (..), MovementStep (..)
#if GTK_CHECK_VERSION(2,16,0)
  , EntryIconPosition (..)
#endif
  )
import Graphics.UI.Gtk.Gdk.EventM       (EventM, EButton, EKey)
import Control.Monad.Reader             ( ask )
import Control.Monad.Trans              ( liftIO )
#if GTK_CHECK_VERSION (2,18,0)
import Graphics.UI.Gtk.Entry.EntryBuffer
#endif
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

#if GTK_CHECK_VERSION(2,18,0)
-- | Creates a new 'Entry' widget backed by a particular 'EntryBuffer'. One
-- buffer can be shared among many widgets.
--
entryNewWithBuffer :: EntryBufferClass buffer => buffer -> IO Entry
entryNewWithBuffer buffer =
  makeNewObject mkEntry $
  liftM (castPtr :: Ptr Widget -> Ptr Entry) $
  {# call unsafe entry_new_with_buffer #}
    (toEntryBuffer buffer)

--------------------
-- Methods

-- Although the documentation doesn't say one way or the other, a look at the
-- source indicates that gtk_entry_get_buffer doesn't increment the reference
-- count of the GtkEntryBuffer it returns, so, like textViewGetBuffer, we must
-- increment it ourselves.

-- | Get the 'EntryBuffer' object which holds the text for this widget.
entryGetBuffer :: EntryClass self => self
  -> IO EntryBuffer
entryGetBuffer self =
  makeNewGObject mkEntryBuffer $
  {# call gtk_entry_get_buffer #}
    (toEntry self)

-- | Set the 'EntryBuffer' object which holds the text for this widget.
entrySetBuffer :: (EntryClass self, EntryBufferClass buffer) => self
  -> buffer -> IO ()
entrySetBuffer self =
  {# call gtk_entry_set_buffer #}
    (toEntry self) . toEntryBuffer
#endif

-- | Sets the text in the widget to the given value, replacing the current
-- contents.
--
entrySetText :: (EntryClass self, GlibString string) => self -> string -> IO ()
entrySetText self text =
  withUTFString text $ \textPtr ->
  {# call entry_set_text #}
    (toEntry self)
    textPtr

-- | Retrieves the contents of the entry widget.
-- See also 'Graphics.UI.Gtk.Display.Entry.Editable.editableGetChars'.
--
entryGetText :: (EntryClass self, GlibString string) => self -> IO string
entryGetText self =
  {# call entry_get_text #}
    (toEntry self)
  >>= peekUTFString
#if GTK_MAJOR_VERSION < 3
#ifndef DISABLE_DEPRECATED
-- | Appends the given text to the contents of the widget.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
entryAppendText :: (EntryClass self, GlibString string) => self -> string -> IO ()
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
-- Removed in Gtk3.
entryPrependText :: (EntryClass self, GlibString string) => self -> string -> IO ()
entryPrependText self text =
  withUTFString text $ \textPtr ->
  {# call entry_prepend_text #}
    (toEntry self)
    textPtr
#endif
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
-- 'Graphics.UI.Gtk.Windows.Window.windowActivateDefault' on the window
-- containing the entry, in the default
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

#if GTK_CHECK_VERSION(3,2,0)
-- | Sets text to be displayed in entry when it is empty and unfocused.
-- This can be used to give a visual hint of the expected contents of the `Entry`.
--
-- Note that since the placeholder text gets removed when the entry received
-- focus, using this feature is a bit problematic if the entry is given the
-- initial focus in a window. Sometimes this can be worked around by delaying
-- the initial focus setting until the first key event arrives.
--
-- * Available since Gtk version 3.2
--
entrySetPlaceholderText :: (EntryClass self, GlibString text) => self
 -> Maybe text -- ^ @text@ a string to be displayed when entry is empty an unfocused, or `Nothing`
 -> IO ()
entrySetPlaceholderText self text =
  maybeWith withUTFString text $ \ textPtr ->
  {# call entry_set_placeholder_text #}
    (toEntry self)
    textPtr

-- | Retrieves the text that will be displayed when entry is empty and unfocused.
--
-- * Available since Gtk version 3.2
--
entryGetPlaceholderText :: (EntryClass self, GlibString text) => self
 -> IO (Maybe text) -- ^ returns placeholder text
entryGetPlaceholderText self =
  {# call unsafe entry_get_placeholder_text #}
    (toEntry self)
  >>= maybePeek peekUTFString
#endif

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

#if GTK_MAJOR_VERSION < 3
#if GTK_CHECK_VERSION(2,20,0)
-- | Returns the 'Window' which contains the entry's icon at @iconPos@. This function is useful when
-- drawing something to the entry in an 'eventExpose' callback because it enables the callback to
-- distinguish between the text window and entry's icon windows.
--
-- See also 'entryGetTextWindow'.
-- Removed in Gtk3.
entryGetIconWindow :: EntryClass self => self
                   -> EntryIconPosition  -- ^ @iconPos@ Icon position
                   -> IO DrawWindow -- ^ returns  the entry's icon window at @iconPos@.
entryGetIconWindow entry iconPos =
    makeNewGObject mkDrawWindow $
    {#call gtk_entry_get_icon_window #}
       (toEntry entry)
       ((fromIntegral . fromEnum) iconPos)

-- | Returns the 'Window' which contains the text. This function is useful when drawing something to the
-- entry in an 'eventExpose' callback because it enables the callback to distinguish between the text
-- window and entry's icon windows.
--
-- See also 'entryGetIconWindow'.
-- Removed in Gtk3.
entryGetTextWindow :: EntryClass self => self
                   -> IO DrawWindow  -- ^ returns the entry's text window.
entryGetTextWindow entry =
    makeNewGObject mkDrawWindow $
    {#call gtk_entry_get_text_window #}
      (toEntry entry)
#endif
#endif

#if GTK_CHECK_VERSION(2,22,0)
-- | Allow the 'Entry' input method to internally handle key press and release events. If this function
-- returns 'True', then no further processing should be done for this key event. See
-- 'imContextFilterKeypress'.
--
-- Note that you are expected to call this function from your handler when overriding key event
-- handling. This is needed in the case when you need to insert your own key handling between the input
-- method and the default key event handling of the 'Entry'. See 'textViewResetImContext' for
-- an example of use.
--
-- * Available since Gtk+ version 2.22
--
entryImContextFilterKeypress :: EntryClass self => self -> EventM EKey Bool
entryImContextFilterKeypress self = do
  ptr <- ask
  liftIO $ liftM toBool $
    {# call gtk_entry_im_context_filter_keypress #}
      (toEntry self)
      (castPtr ptr)

-- | Reset the input method context of the entry if needed.
--
-- This can be necessary in the case where modifying the buffer would confuse on-going input method
-- behavior.
--
-- * Available since Gtk+ version 2.22
--
entryResetImContext :: EntryClass self => self -> IO ()
entryResetImContext self =
  {#call gtk_entry_reset_im_context #} (toEntry self)
#endif

--------------------
-- Attributes

-- | The current position of the insertion cursor in chars.
--
-- Allowed values: [0,65535]
--
-- Default value: 0
--
entryCursorPosition :: EntryClass self => ReadAttr self Int
entryCursorPosition = readAttrFromIntProperty "cursor-position"

-- | The position of the opposite end of the selection from the cursor in
-- chars.
--
-- Allowed values: [0,65535]
--
-- Default value: 0
--
entrySelectionBound :: EntryClass self => ReadAttr self Int
entrySelectionBound = readAttrFromIntProperty "selection-bound"

-- | Whether the entry contents can be edited.
--
-- Default value: @True@
--
entryEditable :: EntryClass self => Attr self Bool
entryEditable = newAttrFromBoolProperty "editable"

-- | Maximum number of characters for this entry. Zero if no maximum.
--
-- Allowed values: [0,65535]
--
-- Default value: 0
--
entryMaxLength :: EntryClass self => Attr self Int
entryMaxLength = newAttr
  entryGetMaxLength
  entrySetMaxLength

-- | @False@ displays the \"invisible char\" instead of the actual text
-- (password mode).
--
-- Default value: @True@
--
entryVisibility :: EntryClass self => Attr self Bool
entryVisibility = newAttr
  entryGetVisibility
  entrySetVisibility

-- | @False@ removes outside bevel from entry.
--
-- Default value: @True@
--
entryHasFrame :: EntryClass self => Attr self Bool
entryHasFrame = newAttr
  entryGetHasFrame
  entrySetHasFrame

-- | The character to use when masking entry contents (in \"password mode\").
--
-- Default value: \'*\'
--
entryInvisibleChar :: EntryClass self => Attr self Char
entryInvisibleChar = newAttr
  entryGetInvisibleChar
  entrySetInvisibleChar

-- | Whether to activate the default widget (such as the default button in a
-- dialog) when Enter is pressed.
--
-- Default value: @False@
--
entryActivatesDefault :: EntryClass self => Attr self Bool
entryActivatesDefault = newAttr
  entryGetActivatesDefault
  entrySetActivatesDefault

-- | Number of characters to leave space for in the entry.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
entryWidthChars :: EntryClass self => Attr self Int
entryWidthChars = newAttr
  entryGetWidthChars
  entrySetWidthChars

-- | Number of pixels of the entry scrolled off the screen to the left.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
entryScrollOffset :: EntryClass self => ReadAttr self Int
entryScrollOffset = readAttrFromIntProperty "scroll-offset"

-- | The contents of the entry.
--
-- Default value: \"\"
--
entryText :: (EntryClass self, GlibString string) => Attr self string
entryText = newAttr
  entryGetText
  entrySetText

#if GTK_CHECK_VERSION(3,2,0)
-- | The text that will be displayed in the `Entry` when it is empty and unfocused.
--
-- Default value: Nothing
--
entryPlaceholderText :: (EntryClass self, GlibString text) => Attr self (Maybe text)
entryPlaceholderText = newAttr
  entryGetPlaceholderText
  entrySetPlaceholderText
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | The horizontal alignment, from 0 (left) to 1 (right). Reversed for RTL
-- layouts.
--
-- Allowed values: [0,1]
--
-- Default value: 0
--
entryXalign :: EntryClass self => Attr self Float
entryXalign = newAttrFromFloatProperty "xalign"

-- | \'alignment\' property. See 'entryGetAlignment' and 'entrySetAlignment'
--
entryAlignment :: EntryClass self => Attr self Float
entryAlignment = newAttr
  entryGetAlignment
  entrySetAlignment

-- | \'completion\' property. See 'entryGetCompletion' and
-- 'entrySetCompletion'
--
entryCompletion :: EntryClass self => Attr self EntryCompletion
entryCompletion = newAttr
  entryGetCompletion
  entrySetCompletion
#endif

#if GTK_CHECK_VERSION(2,18,0)
-- | The buffer being displayed.
--
entryBuffer :: (EntryClass self, EntryBufferClass buffer) =>
  ReadWriteAttr self EntryBuffer buffer
entryBuffer = newAttr
  entryGetBuffer
  entrySetBuffer
#endif


--------------------
-- Signals

-- | A keybinding signal which gets emitted when the user activates the entry.
--
-- Applications should not connect to it, but may emit it with 'signalEmitByName' if they need to
-- control activation programmatically.
entryActivated :: EntryClass ec => Signal ec (IO ())
entryActivated = Signal (connect_NONE__NONE "activate")

-- | Deprecated. See 'entryActivated'.
entryActivate :: EntryClass ec => Signal ec (IO ())
entryActivate = entryActivated

-- | The 'entryBackspace' signal is a keybinding signal which gets emitted when the user asks for it.
--
-- The default bindings for this signal are Backspace and Shift-Backspace.
entryBackspace :: EntryClass ec => Signal ec (IO ())
entryBackspace = Signal (connect_NONE__NONE "backspace")

-- | The 'entryCopyClipboard' signal is a keybinding signal which gets emitted to copy the selection to the
-- clipboard.
--
-- The default bindings for this signal are Ctrl-c and Ctrl-Insert.
entryCopyClipboard :: EntryClass ec => Signal ec (IO ())
entryCopyClipboard = Signal (connect_NONE__NONE "copy-clipboard")

-- | The 'entryCutClipboard' signal is a keybinding signal which gets emitted to cut the selection to the
-- clipboard.
--
-- The default bindings for this signal are Ctrl-x and Shift-Delete.
entryCutClipboard :: EntryClass ec => Signal ec (IO ())
entryCutClipboard = Signal (connect_NONE__NONE "cut-clipboard")

-- | The 'entryDeleteFromCursor' signal is a keybinding signal which gets emitted when the user initiates a
-- text deletion.
--
-- If the type is 'DeleteChars', GTK+ deletes the selection if there is one, otherwise it deletes
-- the requested number of characters.
--
-- The default bindings for this signal are Delete for deleting a character and Ctrl-Delete for
-- deleting a word.
entryDeleteFromCursor :: EntryClass ec => Signal ec (DeleteType -> Int -> IO ())
entryDeleteFromCursor = Signal (connect_ENUM_INT__NONE "delete-from-cursor")

-- | The 'entryInsertAtCursor' signal is a keybinding signal which gets emitted when the user initiates the
-- insertion of a fixed string at the cursor.
entryInsertAtCursor :: (EntryClass ec, GlibString string) => Signal ec (string -> IO ())
entryInsertAtCursor = Signal (connect_GLIBSTRING__NONE "insert-at-cursor")

-- | The 'entryMoveCursor' signal is a keybinding signal which gets emitted when the user initiates a cursor
-- movement. If the cursor is not visible in entry, this signal causes the viewport to be moved
-- instead.
--
-- Applications should not connect to it, but may emit it with 'signalEmitByName' if they need to
-- control the cursor programmatically.
--
-- The default bindings for this signal come in two variants, the variant with the Shift modifier
-- extends the selection, the variant without the Shift modifer does not. There are too many key
-- combinations to list them all here.
--
--   * Arrow keys move by individual characters\/lines
--   * Ctrl-arrow key combinations move by words\/paragraphs
--   * Home\/End keys move to the ends of the buffer
entryMoveCursor :: EntryClass ec => Signal ec (MovementStep -> Int -> Bool -> IO ())
entryMoveCursor = Signal (connect_ENUM_INT_BOOL__NONE "move-cursor")

-- | The 'entryPasteClipboard' signal is a keybinding signal which gets emitted to paste the contents of the
-- clipboard into the text view.
--
-- The default bindings for this signal are Ctrl-v and Shift-Insert.
entryPasteClipboard :: EntryClass ec => Signal ec (IO ())
entryPasteClipboard = Signal (connect_NONE__NONE "paste-clipboard")

-- | The 'entryPopulatePopup' signal gets emitted before showing the context menu of the entry.
--
-- If you need to add items to the context menu, connect to this signal and append your menuitems to
-- the menu.
entryPopulatePopup :: EntryClass ec => Signal ec (Menu -> IO ())
entryPopulatePopup = Signal (connect_OBJECT__NONE "populate-popup")

#if GTK_CHECK_VERSION(2,20,0)
-- | If an input method is used, the typed text will not immediately be committed to the buffer. So if
-- you are interested in the text, connect to this signal.
entryPreeditChanged :: (EntryClass ec, GlibString string) => Signal ec (string -> IO ())
entryPreeditChanged = Signal (connect_GLIBSTRING__NONE "preedit-changed")
#endif

#if GTK_CHECK_VERSION(2,16,0)
-- | The 'iconPress' signal is emitted when an activatable icon is clicked.
--
entryIconPress :: EntryClass ec =>
                    Signal ec (EntryIconPosition -> EventM EButton ())
entryIconPress = Signal $ \after obj f ->
  connect_ENUM_PTR__NONE "icon-press" after obj (runReaderT . f)

-- | The 'iconRelease' signal is emitted on the button release from a mouse click over an activatable
-- icon.
--
entryIconRelease :: EntryClass ec =>
                      Signal ec (EntryIconPosition -> EventM EButton ())
entryIconRelease = Signal $ \after obj f ->
  connect_ENUM_PTR__NONE "icon-press" after obj (runReaderT . f)
#endif

{-# DEPRECATED entryToggleOverwirte "Use entryToggleOverwrite" #-}
entryToggleOverwirte :: EntryClass ec => Signal ec (IO ())
entryToggleOverwirte = entryToggleOverwrite

-- | The 'entryToggleOverwrite' signal is a keybinding signal which gets emitted to toggle the overwrite mode
-- of the entry.
-- The default bindings for this signal is Insert.
--
entryToggleOverwrite :: EntryClass ec => Signal ec (IO ())
entryToggleOverwrite = Signal (connect_NONE__NONE "toggle-overwrite")

#ifndef DISABLE_DEPRECATED
-- | Emitted when the user presses return within
-- the 'Entry' field.
--
onEntryActivate, afterEntryActivate :: EntryClass ec => ec -> IO () ->
                                       IO (ConnectId ec)
onEntryActivate = connect_NONE__NONE "activate" False
afterEntryActivate = connect_NONE__NONE "activate" True

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

-- | Emitted when the user changes from
-- overwriting to inserting.
--
onToggleOverwrite, afterToggleOverwrite :: EntryClass ec => ec -> IO () ->
                                           IO (ConnectId ec)
onToggleOverwrite = connect_NONE__NONE "toggle_overwrite" False
afterToggleOverwrite = connect_NONE__NONE "toggle_overwrite" True
#endif
