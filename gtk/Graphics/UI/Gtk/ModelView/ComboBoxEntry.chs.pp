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
module Graphics.UI.Gtk.ModelView.ComboBoxEntry (
-- * Detail
-- 
-- | A 'ComboBoxEntry' is a widget that allows the user to choose from a list
-- of valid choices or enter a different value. It is very similar to a
-- 'ComboBox', but it displays the selected value in an entry to allow
-- modifying it.
--
-- In contrast to a 'ComboBox', the underlying model of a 'ComboBoxEntry'
-- must always have a text column (see 'comboBoxEntrySetTextColumn'), and the
-- entry will show the content of the text column in the selected row. To get
-- the text from the entry, use 'comboBoxGetActiveText'.
--
-- The changed signal will be emitted while typing into a 'ComboBoxEntry',
-- as well as when selecting an item from the 'ComboBoxEntry''s list. Use
-- 'comboBoxGetActive' or 'comboBoxGetActiveIter' to discover whether an item
-- was actually selected from the list.
--
-- Connect to the activate signal of the 'Entry' (use 'binGetChild') to
-- detect when the user actually finishes entering text.
--

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

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  ComboBoxEntry,
  ComboBoxEntryClass,
  castToComboBoxEntry,
  toComboBoxEntry,

-- * Constructors
  comboBoxEntryNew,
  comboBoxEntryNewWithModel,
  comboBoxEntryNewText,

-- * Methods
  comboBoxEntrySetTextColumn,
  comboBoxEntryGetTextColumn,

-- * Attributes
  comboBoxEntryTextColumn,
#endif
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#} hiding ( ListStore )
import Graphics.UI.Gtk.ModelView.Types
import Graphics.UI.Gtk.ModelView.ComboBox
import Graphics.UI.Gtk.ModelView.CellRendererText
import Graphics.UI.Gtk.ModelView.CellLayout
{#import Graphics.UI.Gtk.ModelView.CustomStore#}
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

-- | Convenience function which constructs a new editable text combo box,
-- which is a 'ComboBoxEntry' just displaying strings. Note that this
-- function does not setup any functionality to insert newly typed
-- text into the model. See the module introduction for information
-- about this.
--
comboBoxEntryNewText ::
     (a -> String) -- ^ a function to extract elements from a the store
  -> [a] -- ^ the initial entries in the 'ComboBoxEntry'
  -> IO (ComboBoxEntry, ListStore a)
comboBoxEntryNewText extract initial = do
  store <- listStoreNew initial
  let colId = makeColumnIdString 0
  treeModelSetColumn store colId extract
  combo <- makeNewObject mkComboBoxEntry $
    liftM (castPtr :: Ptr Widget -> Ptr ComboBoxEntry) $
    {# call gtk_combo_box_entry_new_with_model #}
    (toTreeModel store)
    (fromIntegral (columnIdToNumber colId))
  return (combo, store)

--------------------
-- Methods

-- %hash c:b7d7 d:2818
-- | Sets the model column should be use to get strings from to
-- be @textColumn@.
--
comboBoxEntrySetTextColumn :: ComboBoxEntryClass self => self
 -> ColumnId row String -- ^ @textColumn@ - A column in @model@ to get the strings from.
 -> IO ()
comboBoxEntrySetTextColumn self textColumn =
  {# call gtk_combo_box_entry_set_text_column #}
    (toComboBoxEntry self)
    (fromIntegral (columnIdToNumber textColumn))

-- %hash c:a3e3 d:6441
-- | Returns the column which is used to get the strings from.
--
comboBoxEntryGetTextColumn :: ComboBoxEntryClass self => self
 -> IO (ColumnId row String) -- ^ returns A column in the data source model of @entryBox@.
comboBoxEntryGetTextColumn self =
  liftM (makeColumnIdString . fromIntegral) $
  {# call gtk_combo_box_entry_get_text_column #}
    (toComboBoxEntry self)

--------------------
-- Attributes

-- %hash c:84ff d:be07
-- | A column in the data source model to get the strings from.
--
-- Allowed values: >= 0
--
-- Default value: 'Graphics.UI.Gtk.ModelView.CustomStore.invalidColumnId'
--
comboBoxEntryTextColumn :: ComboBoxEntryClass self => Attr self (ColumnId row String)
comboBoxEntryTextColumn = newAttr
  comboBoxEntryGetTextColumn
  comboBoxEntrySetTextColumn

#endif
