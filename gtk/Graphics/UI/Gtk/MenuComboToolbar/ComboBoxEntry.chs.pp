-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ComboBoxEntry
--
--  Author : Duncan Coutts
--
--  Created: 25 April 2004
--
--  Version $Revision: 1.8 $ from $Date: 2005/08/25 01:16:15 $
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
-- In contrast to a 'ComboBox', the underlying model of a 'ComboBoxEntry'
-- must always have a text column (see 'comboBoxEntrySetTextColumn'), and the
-- entry will show the content of the text column in the selected row. To get
-- the text from the entry, use 'comboBoxGetActiveText'.
--
-- The convenience API to construct simple text-only 'ComboBox'es can also
-- be used with 'ComboBoxEntry's which have been constructed with
-- 'comboBoxEntryNewText'.

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

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.GObject		(makeNewGObject)
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

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
    model            -- ^ @model@ - A 'TreeModel'.
 -> Int              -- ^ @textColumn@ - A column in @model@ to get the
                     -- strings from.
 -> IO ComboBoxEntry
comboBoxEntryNewWithModel model textColumn =
  makeNewObject mkComboBoxEntry $
  liftM (castPtr :: Ptr Widget -> Ptr ComboBoxEntry) $
  {# call gtk_combo_box_entry_new_with_model #}
    (toTreeModel model)
    (fromIntegral textColumn)

-- | Convenience function which constructs a new editable text combo box,
-- which is a 'ComboBoxEntry' just displaying strings. If you use this function
-- to create a text combo box, you should only manipulate its data source with
-- the following convenience functions: 'comboBoxAppendText',
-- 'comboBoxInsertText', 'comboBoxPrependText' and 'comboBoxRemoveText'.
--
comboBoxEntryNewText :: IO ComboBoxEntry
comboBoxEntryNewText =
  makeNewObject mkComboBoxEntry $
  liftM (castPtr :: Ptr Widget -> Ptr ComboBoxEntry) $
  {# call gtk_combo_box_entry_new_text #}

--------------------
-- Methods

-- | Sets the model column which the entry box should use to get strings from to
-- be @textColumn@.
--
comboBoxEntrySetTextColumn :: ComboBoxEntryClass self => self
 -> Int   -- ^ @textColumn@ - A column in the model to get the strings from.
 -> IO ()
comboBoxEntrySetTextColumn self textColumn =
  {# call gtk_combo_box_entry_set_text_column #}
    (toComboBoxEntry self)
    (fromIntegral textColumn)

-- | Returns the column which the entry box is using to get the strings from.
--
comboBoxEntryGetTextColumn :: ComboBoxEntryClass self => self
 -> IO Int -- ^ returns a column in the data source model of the entry box.
comboBoxEntryGetTextColumn self =
  liftM fromIntegral $
  {# call gtk_combo_box_entry_get_text_column #}
    (toComboBoxEntry self)

--------------------
-- Attributes

-- | A column in the data source model to get the strings from.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
comboBoxEntryTextColumn :: ComboBoxEntryClass self => Attr self Int
comboBoxEntryTextColumn = newAttr
  comboBoxEntryGetTextColumn
  comboBoxEntrySetTextColumn
#endif
