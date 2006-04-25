-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget EntryCompletion
--
--  Author : Duncan Coutts
--
--  Created: 24 April 2004
--
--  Version $Revision: 1.18 $ from $Date: 2005/11/26 16:00:21 $
--
--  Copyright (C) 2004-2005 Duncan Coutts
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
-- Completion functionality for 'Entry'
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.Entry.EntryCompletion (
-- * Detail
-- 
-- | 'EntryCompletion' is an auxiliary object to be used in conjunction with
-- 'Entry' to provide the completion functionality. It implements the
-- 'CellLayout' interface, to allow the user to add extra cells to the
-- 'TreeView' with completion matches.
--
-- \"Completion functionality\" means that when the user modifies the text
-- in the entry, 'EntryCompletion' checks which rows in the model match the
-- current content of the entry, and displays a list of matches. By default,
-- the matching is done by comparing the entry text case-insensitively against
-- the text in a model (see 'entryCompletionSetTextModel'), but this
-- can be overridden with a custom match function (see
-- 'entryCompletionSetMatchFunc').
--
-- When the user selects a completion, the content of the entry is updated.
-- By default, the content of the entry is replaced by the text column of the
-- model, but this can be overridden by connecting to the ::match-selected
-- signal and updating the entry in the signal handler. Note that you should
-- return @True@ from the signal handler to suppress the default behaviour.
--
-- To add completion functionality to an entry, use
-- 'Graphics.UI.Gtk.Entry.Entry.entrySetCompletion'.
--
-- In addition to regular completion matches, which will be inserted into
-- the entry when they are selected, 'EntryCompletion' also allows to display
-- \"actions\" in the popup window. Their appearance is similar to menu items,
-- to differentiate them clearly from completion strings. When an action is
-- selected, the ::action-activated signal is emitted.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----EntryCompletion
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  EntryCompletion,
  EntryCompletionClass,
  castToEntryCompletion,
  toEntryCompletion,

-- * Constructors
  entryCompletionNew,

-- * Methods
  entryCompletionGetEntry,
  entryCompletionSetModel,
  entryCompletionSetTextModel,
  entryCompletionSetMatchFunc,
  entryCompletionSetMinimumKeyLength,
  entryCompletionGetMinimumKeyLength,
  entryCompletionComplete,
  entryCompletionInsertActionText,
  entryCompletionInsertActionMarkup,
  entryCompletionDeleteAction,
#if GTK_CHECK_VERSION(2,6,0)
  entryCompletionInsertPrefix,
  entryCompletionSetInlineCompletion,
  entryCompletionGetInlineCompletion,
  entryCompletionSetPopupCompletion,
  entryCompletionGetPopupCompletion,
#endif
#if GTK_CHECK_VERSION(2,8,0)
  entryCompletionSetPopupSetWidth,
  entryCompletionGetPopupSetWidth,
  entryCompletionSetPopupSingleMatch,
  entryCompletionGetPopupSingleMatch,
#endif

-- * Attributes
  entryCompletionModel,
  entryCompletionMinimumKeyLength,
#if GTK_CHECK_VERSION(2,6,0)
  entryCompletionInlineCompletion,
  entryCompletionPopupCompletion,
#endif
#if GTK_CHECK_VERSION(2,8,0)
  entryCompletionPopupSetWidth,
  entryCompletionPopupSingleMatch,
#endif

-- * Signals
#if GTK_CHECK_VERSION(2,6,0)
  onInsertPrefix,
  afterInsertPrefix,
#endif
  onActionActivated,
  afterActionActivated,
#endif
  ) where

import Monad	(liftM)
import Data.IORef (newIORef, readIORef, writeIORef)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.GObject		(constructNewGObject,
					 makeNewGObject, mkFunPtrDestroyNotify)
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.TreeList.TreeIter#} (TreeIter)
{#import Graphics.UI.Gtk.TreeList.Types#}
import Graphics.UI.Gtk.TreeList.CellRendererText
import Graphics.UI.Gtk.TreeList.CellLayout

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,4,0)
--------------------
-- Constructors

-- | Creates a new 'EntryCompletion' object.
--
entryCompletionNew :: IO EntryCompletion
entryCompletionNew =
  constructNewGObject mkEntryCompletion $
  {# call gtk_entry_completion_new #}

--------------------
-- Methods

-- | Gets the entry @completion@ has been attached to.
--
entryCompletionGetEntry :: EntryCompletion
 -> IO (Maybe Entry) -- ^ returns the entry @completion@ has been attached to.
entryCompletionGetEntry self =
  maybeNull (makeNewObject mkEntry) $
  liftM (castPtr :: Ptr Widget -> Ptr Entry) $
  {# call gtk_entry_completion_get_entry #}
    self

-- | Sets the model for a 'EntryCompletion'. If @completion@ already has a
-- model set, it will remove it before setting the new model. If model is
-- @Nothing@, then it will unset the model.
--
entryCompletionSetModel :: TreeModelClass model => EntryCompletion
 -> Maybe model     -- ^ @model@ - The 'TreeModel'.
 -> IO ()
entryCompletionSetModel self model =
  {# call gtk_entry_completion_set_model #}
    self
    (maybe (TreeModel nullForeignPtr) toTreeModel model)

-- | Convenience function for setting up the most used case of this code: a
-- completion list with just strings. This function will set up @completion@ to
-- have a list displaying all (and just) strings in the completion list, and to
-- get those strings from @model@. This functions creates and adds a 
-- 'CellRendererText' which retrieves its content from the given model.
--
entryCompletionSetTextModel :: (TreeModelClass (model String),
				TypedTreeModelClass model)
 => EntryCompletion -- ^ @completion@
 -> model String    -- ^ the model containing 'String's
 -> IO ()
entryCompletionSetTextModel self model = do
  entryCompletionSetModel self (Just model)
  cell <- cellRendererTextNew
  cellLayoutPackStart self cell True
  cellLayoutSetAttributes self cell model (\str -> [cellText := str])

-- | Sets the match function for @completion@ to be @func@. The match function
-- is used to determine if a row should or should not be in the completion
-- list.
--
-- * The passed-in function decides whether the row indicated by the
--   'TreeIter' matches a given key, and should be displayed as a possible
--   completion for key. Note that key is normalized and case-folded.
--   Normalization will standardizing such issues as whether a character
--   with an accent is represented as a base character and combining accent
--   or as a single precomposed character. If this is not appropriate you
--   can extract the original text from the entry.
--
entryCompletionSetMatchFunc :: EntryCompletion -> (String -> TreeIter -> IO Bool) -> IO ()
entryCompletionSetMatchFunc ec handler = do
  hPtr <- mkHandler_GtkEntryCompletionMatchFunc
    (\_ keyPtr iterPtr _ -> do key <- peekUTFString keyPtr
                               iter <- peek iterPtr
                               liftM fromBool $ handler key iter)
  dPtr <- mkFunPtrDestroyNotify hPtr
  {# call gtk_entry_completion_set_match_func #} ec
    (castFunPtr hPtr) nullPtr dPtr

-------------------------------------------------
-- Callback stuff for entryCompletionSetMatchFunc
--

type GtkEntryCompletionMatchFunc =
  Ptr EntryCompletion -> --GtkEntryCompletion *completion
  Ptr CChar ->           --const gchar *key
  Ptr TreeIter ->        --GtkTreeIter *iter
  Ptr () ->              --gpointer user_data
  IO {#type gboolean#}

foreign import ccall "wrapper" mkHandler_GtkEntryCompletionMatchFunc ::
  GtkEntryCompletionMatchFunc -> 
  IO (FunPtr GtkEntryCompletionMatchFunc)

-- | Requires the length of the search key for @completion@ to be at least
-- @length@. This is useful for long lists, where completing using a small key
-- takes a lot of time and will come up with meaningless results anyway (ie, a
-- too large dataset).
--
entryCompletionSetMinimumKeyLength :: EntryCompletion
 -> Int             -- ^ @length@ - The minimum length of the key in order to
                    -- start completing.
 -> IO ()
entryCompletionSetMinimumKeyLength self length =
  {# call gtk_entry_completion_set_minimum_key_length #}
    self
    (fromIntegral length)

-- | Returns the minimum key length as set for @completion@.
--
entryCompletionGetMinimumKeyLength :: EntryCompletion
 -> IO Int          -- ^ returns The currently used minimum key length.
entryCompletionGetMinimumKeyLength self =
  liftM fromIntegral $
  {# call gtk_entry_completion_get_minimum_key_length #}
    self

-- | Requests a completion operation, or in other words a refiltering of the
-- current list with completions, using the current key. The completion list
-- view will be updated accordingly.
--
entryCompletionComplete :: EntryCompletion -> IO ()
entryCompletionComplete self =
  {# call gtk_entry_completion_complete #}
    self

-- | Inserts an action in @completion@'s action item list at position @index@
-- with text @text@. If you want the action item to have markup, use
-- 'entryCompletionInsertActionMarkup'.
--
entryCompletionInsertActionText :: EntryCompletion
 -> Int             -- ^ @index@ - The index of the item to insert.
 -> String          -- ^ @text@ - Text of the item to insert.
 -> IO ()
entryCompletionInsertActionText self index text =
  withUTFString text $ \textPtr ->
  {# call gtk_entry_completion_insert_action_text #}
    self
    (fromIntegral index)
    textPtr

-- | Inserts an action in @completion@'s action item list at position @index@
-- with markup @markup@.
--
entryCompletionInsertActionMarkup :: EntryCompletion
 -> Int             -- ^ @index@ - The index of the item to insert.
 -> String          -- ^ @markup@ - Markup of the item to insert.
 -> IO ()
entryCompletionInsertActionMarkup self index markup =
  withUTFString markup $ \markupPtr ->
  {# call gtk_entry_completion_insert_action_markup #}
    self
    (fromIntegral index)
    markupPtr

-- | Deletes the action at @index@ from @completion@'s action list.
--
entryCompletionDeleteAction :: EntryCompletion
 -> Int             -- ^ @index@ - The index of the item to Delete.
 -> IO ()
entryCompletionDeleteAction self index =
  {# call gtk_entry_completion_delete_action #}
    self
    (fromIntegral index)

#if GTK_CHECK_VERSION(2,6,0)
-- | Requests a prefix insertion.
--
-- * Available since Gtk+ version 2.6
--
entryCompletionInsertPrefix :: EntryCompletion -> IO ()
entryCompletionInsertPrefix self =
  {# call gtk_entry_completion_insert_prefix #}
    self

-- | Sets whether the common prefix of the possible completions should be
-- automatically inserted in the entry.
--
-- * Available since Gtk+ version 2.6
--
entryCompletionSetInlineCompletion :: EntryCompletion
 -> Bool            -- ^ @inlineCompletion@ - @True@ to do inline completion
 -> IO ()
entryCompletionSetInlineCompletion self inlineCompletion =
  {# call gtk_entry_completion_set_inline_completion #}
    self
    (fromBool inlineCompletion)

-- | Returns whether the common prefix of the possible completions should be
-- automatically inserted in the entry.
--
-- * Available since Gtk+ version 2.6
--
entryCompletionGetInlineCompletion :: EntryCompletion
 -> IO Bool         -- ^ returns @True@ if inline completion is turned on
entryCompletionGetInlineCompletion self =
  liftM toBool $
  {# call gtk_entry_completion_get_inline_completion #}
    self

-- | Sets whether the completions should be presented in a popup window.
--
-- * Available since Gtk+ version 2.6
--
entryCompletionSetPopupCompletion :: EntryCompletion
 -> Bool            -- ^ @popupCompletion@ - @True@ to do popup completion
 -> IO ()
entryCompletionSetPopupCompletion self popupCompletion =
  {# call gtk_entry_completion_set_popup_completion #}
    self
    (fromBool popupCompletion)

-- | Returns whether the completions should be presented in a popup window.
--
-- * Available since Gtk+ version 2.6
--
entryCompletionGetPopupCompletion :: EntryCompletion
 -> IO Bool         -- ^ returns @True@ if popup completion is turned on
entryCompletionGetPopupCompletion self =
  liftM toBool $
  {# call gtk_entry_completion_get_popup_completion #}
    self
#endif

#if GTK_CHECK_VERSION(2,8,0)
-- | Sets whether the completion popup window will be resized to be the same
-- width as the entry.
--
-- * Available since Gtk+ version 2.8
--
entryCompletionSetPopupSetWidth :: EntryCompletion
 -> Bool            -- ^ @popupSetWidth@ - @True@ to make the width of the
                    -- popup the same as the entry
 -> IO ()
entryCompletionSetPopupSetWidth self popupSetWidth =
  {# call gtk_entry_completion_set_popup_set_width #}
    self
    (fromBool popupSetWidth)

-- | Returns whether the completion popup window will be resized to the width
-- of the entry.
--
-- * Available since Gtk+ version 2.8
--
entryCompletionGetPopupSetWidth :: EntryCompletion
 -> IO Bool         -- ^ returns @True@ if the popup window will be resized to
                    -- the width of the entry
entryCompletionGetPopupSetWidth self =
  liftM toBool $
  {# call gtk_entry_completion_get_popup_set_width #}
    self

-- | Sets whether the completion popup window will appear even if there is
-- only a single match. You may want to set this to @False@ if you are using
-- inline completion.
--
-- * Available since Gtk+ version 2.8
--
entryCompletionSetPopupSingleMatch :: EntryCompletion
 -> Bool            -- ^ @popupSingleMatch@ - @True@ if the popup should
                    -- appear even for a single match
 -> IO ()
entryCompletionSetPopupSingleMatch self popupSingleMatch =
  {# call gtk_entry_completion_set_popup_single_match #}
    self
    (fromBool popupSingleMatch)

-- | Returns whether the completion popup window will appear even if there is
-- only a single match.
--
-- * Available since Gtk+ version 2.8
--
entryCompletionGetPopupSingleMatch :: EntryCompletion
 -> IO Bool         -- ^ returns @True@ if the popup window will appear
                    -- regardless of the number of matches.
entryCompletionGetPopupSingleMatch self =
  liftM toBool $
  {# call gtk_entry_completion_get_popup_single_match #}
    self
#endif

--------------------
-- Attributes

-- | The model to find matches in.
--
entryCompletionModel :: TreeModelClass model => WriteAttr EntryCompletion (Maybe model)
entryCompletionModel = writeAttr
  entryCompletionSetModel

-- | Minimum length of the search key in order to look up matches.
--
-- Allowed values: >= 0
--
-- Default value: 1
--
entryCompletionMinimumKeyLength :: Attr EntryCompletion Int
entryCompletionMinimumKeyLength = newAttr
  entryCompletionGetMinimumKeyLength
  entryCompletionSetMinimumKeyLength

#if GTK_CHECK_VERSION(2,6,0)

-- | Determines whether the common prefix of the possible completions should
-- be inserted automatically in the entry. Note that this requires text-column
-- to be set, even if you are using a custom match function.
--
-- Default value: @False@
--
entryCompletionInlineCompletion :: Attr EntryCompletion Bool
entryCompletionInlineCompletion = newAttr
  entryCompletionGetInlineCompletion
  entryCompletionSetInlineCompletion

-- | Determines whether the possible completions should be shown in a popup
-- window.
--
-- Default value: @True@
--
entryCompletionPopupCompletion :: Attr EntryCompletion Bool
entryCompletionPopupCompletion = newAttr
  entryCompletionGetPopupCompletion
  entryCompletionSetPopupCompletion
#endif

#if GTK_CHECK_VERSION(2,8,0)
-- | Determines whether the completions popup window will be resized to the
-- width of the entry.
--
-- Default value: @True@
--
entryCompletionPopupSetWidth :: Attr EntryCompletion Bool
entryCompletionPopupSetWidth = newAttr
  entryCompletionGetPopupSetWidth
  entryCompletionSetPopupSetWidth

-- | Determines whether the completions popup window will shown for a single
-- possible completion. You probably want to set this to @False@ if you are
-- using inline completion.
--
-- Default value: @True@
--
entryCompletionPopupSingleMatch :: Attr EntryCompletion Bool
entryCompletionPopupSingleMatch = newAttr
  entryCompletionGetPopupSingleMatch
  entryCompletionSetPopupSingleMatch
#endif

--------------------
-- Signals

#if GTK_CHECK_VERSION(2,6,0)
-- | Gets emitted when the inline autocompletion is triggered. The default
-- behaviour is to make the entry display the whole prefix and select the newly
-- inserted part.
--
-- Applications may connect to this signal in order to insert only a smaller
-- part of the @prefix@ into the entry - e.g. the entry used in the
-- 'FileChooser' inserts only the part of the prefix up to the next \'\/\'.
--
onInsertPrefix, afterInsertPrefix :: EntryCompletionClass self => self
 -> (String -> IO Bool)
 -> IO (ConnectId self)
onInsertPrefix = connect_STRING__BOOL "insert_prefix" False
afterInsertPrefix = connect_STRING__BOOL "insert_prefix" True
#endif

-- | Gets emitted when an action is activated.
--
onActionActivated, afterActionActivated :: EntryCompletionClass self => self
 -> (Int -> IO ())
 -> IO (ConnectId self)
onActionActivated = connect_INT__NONE "action_activated" False
afterActionActivated = connect_INT__NONE "action_activated" True
#endif
