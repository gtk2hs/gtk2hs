{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ComboBoxEntry
--
--  Author : Duncan Coutts
--
--  Created: 25 April 2004
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
-- A text entry field with a dropdown list
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.MenuComboToolbar.ComboBoxEntry (
-- * Detail
--
-- | A 'ComboBoxEntry' is a widget that allows the user to choose from a list
-- of valid choices or enter a different value. It is very similar to a
-- 'ComboBox', but it displays the selected value in an entry to allow
-- modifying it.
--
-- In contrast to a 'ComboBox', the underlying model of a 'ComboBoxEntry' must
-- always have a text column (see 'comboBoxEntrySetTextColumn'), and the entry
-- will show the content of the text column in the selected row. To get the
-- text from the entry, use 'comboBoxEntryGetActiveText'.
--
-- The 'Graphics.UI.Gtk.MenuComboToolbar.ComboBox.changed' signal will be
-- emitted while typing into a 'ComboBoxEntry', as well as when selecting an
-- item from the 'ComboBoxEntry''s list. Use 'comboBoxGetActive' or
-- 'comboBoxGetActiveIter' to discover whether an item was actually selected
-- from the list.
--
-- Connect to the activate signal of the 'Entry' (use 'binGetChild') to detect
-- when the user actually finishes entering text.
--
-- * This module is deprecated and the functionality removed in Gtk3. It is
--   therefore empty in Gtk3.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'ComboBox'
-- |                                 +----ComboBoxEntry
-- @

#if GTK_MAJOR_VERSION < 3
#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  ComboBoxEntry,
  ComboBoxEntryClass,
  castToComboBoxEntry, gTypeComboBoxEntry,
  toComboBoxEntry,

-- * Constructors
  comboBoxEntryNew,
  comboBoxEntryNewText,
  comboBoxEntryNewWithModel,

-- * Methods
  comboBoxEntrySetModelText,

  comboBoxEntrySetTextColumn,
  comboBoxEntryGetTextColumn,
#if GTK_CHECK_VERSION(2,6,0)
  comboBoxEntryGetActiveText,
#endif

-- * Attributes
  comboBoxEntryTextColumn,
#endif
#endif
  ) where

#if GTK_MAJOR_VERSION < 3
import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#} hiding ( ListStore )
import Graphics.UI.Gtk.ModelView.Types
import Graphics.UI.Gtk.MenuComboToolbar.ComboBox
{#import Graphics.UI.Gtk.ModelView.CustomStore#}
{#import Graphics.UI.Gtk.ModelView.TreeModel#}
import Graphics.UI.Gtk.ModelView.ListStore ( ListStore, listStoreNew )

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,4,0)
--------------------
-- Constructors

-- | Creates a new 'ComboBoxEntry' which has a 'Entry' as child. After
-- construction, you should set a model using 'comboBoxSetModel' and a
-- text column using 'comboBoxEntrySetTextColumn'.
--
comboBoxEntryNew :: IO ComboBoxEntry
comboBoxEntryNew =
  makeNewObject mkComboBoxEntry $
  liftM (castPtr :: Ptr Widget -> Ptr ComboBoxEntry) $
  {# call gtk_combo_box_entry_new #}

-- | Creates a new 'ComboBoxEntry' with a store containing strings.
--   See 'comboBoxEntrySetModelText'.
--
comboBoxEntryNewText :: IO ComboBoxEntry
comboBoxEntryNewText = do
  combo <- comboBoxEntryNew
  comboBoxEntrySetModelText combo
  return combo

-- | Creates a new 'ComboBoxEntry' which has a 'Entry' as child and a list of
-- strings as popup. You can get the 'Entry' from a 'ComboBoxEntry' using
-- 'binGetChild'. To add and remove strings from the list, just modify @model@
-- using its data manipulation API.
--
comboBoxEntryNewWithModel :: TreeModelClass model =>
    model        -- ^ @model@ - A 'CustomStore'.
 -> IO ComboBoxEntry
comboBoxEntryNewWithModel model = do
  combo <- comboBoxEntryNew
  comboBoxSetModel combo (Just model)
  return combo

--------------------
-- Methods

-- | Set a model that holds strings.
--
-- This function stores a 'Graphics.UI.Gtk.ModelView.ListStore' with the
-- widget and sets the model to the list store. The widget can contain only
-- strings. The model can be retrieved with 'comboBoxGetModel'. The list
-- store can be retrieved with 'comboBoxGetModelText'.
-- Any exisiting model or renderers are removed before setting the new text
-- model.
-- In order to respond to new texts that the user enters, it is necessary to
-- connect to the 'Graphics.UI.Gtk.Entry.Entry.entryActivated' signal of the
-- contained 'Graphics.UI.Gtk.Entry.Entry.Entry' and insert the text into the
-- text model which can be retrieved with
-- 'Graphics.UI.Gtk.MenuComboToolbar.ComboBox.comboBoxGetModelText'.
-- Note that the functions 'comboBoxAppendText', 'comboBoxInsertText',
-- 'comboBoxPrependText', 'comboBoxRemoveText' and 'comboBoxGetActiveText'
-- can be called on a combo box only once 'comboBoxEntrySetModelText' is
-- called.
--
comboBoxEntrySetModelText :: ComboBoxEntryClass self => self ->
                             IO (ListStore String)
comboBoxEntrySetModelText combo = do
  store <- listStoreNew ([] :: [String])
  comboBoxSetModel combo (Just store)
  let colId = makeColumnIdString 0
  customStoreSetColumn store colId id
  comboBoxEntrySetTextColumn (toComboBoxEntry combo) colId
  objectSetAttribute comboQuark (toComboBoxEntry combo) (Just store)
  return store

-- %hash c:b7d7 d:2818
-- | Sets the model column should be use to get strings from to
-- be @textColumn@.
--
comboBoxEntrySetTextColumn :: (ComboBoxEntryClass self, GlibString string) => self
 -> ColumnId row string -- ^ @textColumn@ - A column in @model@ to get the strings from.
 -> IO ()
comboBoxEntrySetTextColumn self textColumn =
  {# call gtk_combo_box_entry_set_text_column #}
    (toComboBoxEntry self)
    (fromIntegral (columnIdToNumber textColumn))

-- %hash c:a3e3 d:6441
-- | Returns the column which is used to get the strings from.
--
comboBoxEntryGetTextColumn :: (ComboBoxEntryClass self, GlibString string) => self
 -> IO (ColumnId row string) -- ^ returns A column in the data source model of @entryBox@.
comboBoxEntryGetTextColumn self =
  liftM (makeColumnIdString . fromIntegral) $
  {# call gtk_combo_box_entry_get_text_column #}
    (toComboBoxEntry self)

#if GTK_CHECK_VERSION(2,6,0)
-- | Retrieve the text currently in the entry.
--
-- * Returns @Nothing@ if no text is selected or entered.
--
-- * Availabe in Gtk 2.6 or higher.
--
comboBoxEntryGetActiveText :: (ComboBoxEntryClass self, GlibString string) => self
  -> IO (Maybe string)
comboBoxEntryGetActiveText self = do
  strPtr <- {# call gtk_combo_box_get_active_text #} (toComboBox self)
  if strPtr == nullPtr then return Nothing else liftM Just $
    peekUTFString (castPtr strPtr)
#endif

--------------------
-- Attributes

-- %hash c:84ff d:be07
-- | A column in the data source model to get the strings from.
--
-- Allowed values: >= 0
--
-- Default value: 'Graphics.UI.Gtk.ModelView.CustomStore.invalidColumnId'
--
comboBoxEntryTextColumn :: (ComboBoxEntryClass self, GlibString string) => Attr self (ColumnId row string)
comboBoxEntryTextColumn = newAttr
  comboBoxEntryGetTextColumn
  comboBoxEntrySetTextColumn

#endif
#endif
