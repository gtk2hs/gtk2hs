-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget EntryCompletion
--
--  Author : Duncan Coutts
--
--  Created: 24 April 2004
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/25 01:11:33 $
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
-- Completion functionality for the "Entry" widget.
--
-- * Added in GTK+ 2.4
--
module Graphics.UI.Gtk.Entry.EntryCompletion (
-- * Description
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
-- the text column of the model (see 'entryCompletionSetTextColumn'), but this
-- can be overridden with a custom match function (see
-- 'entryCompletionSetMatchFunc').
--
-- When the user selects a completion, the content of the entry is updated.
-- By default, the content of the entry is replaced by the text column of the
-- model, but this can be overridden by connecting to the ::match-selected
-- signal and updating the entry in the signal handler. Note that you should
-- return @True@ from the signal handler to suppress the default behaviour.
--
-- To add completion functionality to an entry, use 'entrySetCompletion'.
--
-- In addition to regular completion matches, which will be inserted into
-- the entry when they are selected, 'EntryCompletion' also allows to display
-- \"actions\" in the popup window. Their appearance is similar to menuitems,
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

-- * Constructors
  entryCompletionNew,

-- * Methods
  entryCompletionGetEntry,
  entryCompletionSetModel,
  entryCompletionGetModel,
  entryCompletionSetMatchFunc,
  entryCompletionSetMinimumKeyLength,
  entryCompletionGetMinimumKeyLength,
  entryCompletionComplete,
  entryCompletionInsertActionText,
  entryCompletionInsertActionMarkup,
  entryCompletionDeleteAction,
  entryCompletionSetTextColumn
#endif
) where

#if GTK_CHECK_VERSION(2,4,0)

import Monad	(liftM)
import Data.IORef (newIORef, readIORef, writeIORef)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject		(makeNewGObject)
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.TreeList.TreeModel#} (TreeIter, createTreeIter)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

entryCompletionNew :: IO EntryCompletion
entryCompletionNew =
  makeNewGObject mkEntryCompletion $ liftM castPtr $
  {# call gtk_entry_completion_new #}

--------------------
-- Methods

entryCompletionGetEntry :: EntryCompletion -> IO (Maybe Entry)
entryCompletionGetEntry ec = do
  entryPtr <- {# call gtk_entry_completion_get_entry #} ec
  if entryPtr == nullPtr
    then return Nothing
    else liftM Just $ makeNewObject mkEntry $ return (castPtr entryPtr)

entryCompletionSetModel :: EntryCompletion ->  TreeModel -> IO ()
entryCompletionSetModel ec tm =
  {# call gtk_entry_completion_set_model #} ec tm

entryCompletionGetModel :: EntryCompletion -> IO TreeModel
entryCompletionGetModel ec =
  makeNewGObject mkTreeModel $
  {# call gtk_entry_completion_get_model #} ec

entryCompletionSetMatchFunc :: EntryCompletion -> (String -> TreeIter -> IO ()) -> IO ()
entryCompletionSetMatchFunc ec handler =
  connect_GtkEntryCompletionMatchFunc ec handler

entryCompletionSetMinimumKeyLength :: EntryCompletion -> Int -> IO ()
entryCompletionSetMinimumKeyLength ec minLength =
  {# call gtk_entry_completion_set_minimum_key_length #} ec
    (fromIntegral minLength)

entryCompletionGetMinimumKeyLength :: EntryCompletion -> IO Int
entryCompletionGetMinimumKeyLength ec =
  liftM fromIntegral $
  {# call gtk_entry_completion_get_minimum_key_length #} ec

entryCompletionComplete :: EntryCompletion -> IO ()
entryCompletionComplete ec =
  {# call gtk_entry_completion_complete #} ec

entryCompletionInsertActionText :: EntryCompletion -> Int -> String -> IO ()
entryCompletionInsertActionText ec index text =
  withUTFString text $ \strPtr ->
  {# call gtk_entry_completion_insert_action_text #} ec
    (fromIntegral index) strPtr

entryCompletionInsertActionMarkup :: EntryCompletion -> Int -> String -> IO ()
entryCompletionInsertActionMarkup ec index markup =
  withUTFString markup $ \strPtr ->
  {# call gtk_entry_completion_insert_action_markup #} ec
    (fromIntegral index) strPtr 

entryCompletionDeleteAction :: EntryCompletion -> Int -> IO ()
entryCompletionDeleteAction ec index =
  {# call gtk_entry_completion_delete_action #} ec (fromIntegral index)

entryCompletionSetTextColumn :: EntryCompletion -> Int -> IO ()
entryCompletionSetTextColumn ec column =
  {# call gtk_entry_completion_set_text_column #} ec (fromIntegral column)


-------------------------------------------------
-- Callback stuff for entryCompletionSetMatchFunc
--
{#pointer GDestroyNotify#}

foreign import ccall "wrapper" mkDestructor :: IO () -> IO GDestroyNotify

type GtkEntryCompletionMatchFunc =
  Ptr EntryCompletion -> --GtkEntryCompletion *completion
  Ptr CChar ->           --const gchar *key
  Ptr TreeIter ->        --GtkTreeIter *iter
  Ptr () ->              --gpointer user_data
  IO ()

foreign import ccall "wrapper" mkHandler_GtkEntryCompletionMatchFunc ::
  GtkEntryCompletionMatchFunc -> 
  IO (FunPtr GtkEntryCompletionMatchFunc)

connect_GtkEntryCompletionMatchFunc :: EntryCompletion ->
                                       (String -> TreeIter -> IO ()) ->
                                       IO ()
connect_GtkEntryCompletionMatchFunc ec user = do
  hPtr <- mkHandler_GtkEntryCompletionMatchFunc
    (\_ keyPtr iterPtr _ -> do key <- peekUTFString keyPtr
                               iter <- createTreeIter iterPtr
                               user key iter)
  dRef <- newIORef nullFunPtr
  dPtr <- mkDestructor $ do
    freeHaskellFunPtr hPtr
    dPtr <- readIORef dRef
    freeHaskellFunPtr dPtr
  writeIORef dRef dPtr
  {# call gtk_entry_completion_set_match_func #} ec
    (castFunPtr hPtr) nullPtr dPtr
#endif
