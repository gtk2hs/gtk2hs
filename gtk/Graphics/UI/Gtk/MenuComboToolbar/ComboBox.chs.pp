-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ComboBox
--
--  Author : Duncan Coutts
--
--  Created: 25 April 2004
--
--  Version $Revision: 1.5 $ from $Date: 2005/04/02 16:52:49 $
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
-- A widget used to choose from a list of items
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.MenuComboToolbar.ComboBox (
-- * Detail
-- 
-- | A 'ComboBox' is a widget that allows the user to choose from a list of
-- valid choices. The 'ComboBox' displays the selected choice. When activated,
-- the 'ComboBox' displays a popup which allows the user to make a new choice.
-- The style in which the selected value is displayed, and the style of the
-- popup is determined by the current theme. It may be similar to a
-- 'OptionMenu', or similar to a Windows-style combo box.
--
-- Unlike its predecessors 'Combo' and 'OptionMenu', the 'ComboBox' uses the
-- model-view pattern; the list of valid choices is specified in the form of a
-- tree model, and the display of the choices can be adapted to the data in the
-- model by using cell renderers, as you would in a tree view. This is possible
-- since 'ComboBox' implements the 'CellLayout' interface. The tree model
-- holding the valid choices is not restricted to a flat list, it can be a real
-- tree, and the popup will reflect the tree structure.
--
-- In addition to the model-view API, 'ComboBox' offers a simple API which
-- is suitable for text-only combo boxes, and hides the complexity of managing
-- the data in a model. It consists of the functions 'comboBoxNewText',
-- 'comboBoxAppendText', 'comboBoxInsertText', 'comboBoxPrependText',
-- 'comboBoxRemoveText' and 'comboBoxGetActiveText'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----ComboBox
-- |                                 +----'ComboBoxEntry'
-- @

#if GTK_CHECK_VERSION(2,4,0)
-- * Types
  ComboBox,
  ComboBoxClass,
  castToComboBox,

-- * Constructors
  comboBoxNew,
  comboBoxNewText,
  comboBoxNewWithModel,

-- * Methods
  comboBoxSetWrapWidth,
  comboBoxSetRowSpanColumn,
  comboBoxSetColumnSpanColumn,
  comboBoxGetActive,
  comboBoxSetActive,
  comboBoxGetActiveIter,
  comboBoxSetActiveIter,
  comboBoxGetModel,
  comboBoxSetModel,
  comboBoxAppendText,
  comboBoxInsertText,
  comboBoxPrependText,
  comboBoxRemoveText,
  comboBoxPopup,
  comboBoxPopdown,

-- * Signals
  onChanged,
  afterChanged,
#endif
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes		(Attr(..))
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import System.Glib.GObject		(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.TreeList.TreeModel#} (TreeIter(..), createTreeIter)

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,4,0)
--------------------
-- Constructors

-- | Creates a new empty 'ComboBox'.
--
comboBoxNew :: IO ComboBox
comboBoxNew =
  makeNewObject mkComboBox $
  liftM (castPtr :: Ptr Widget -> Ptr ComboBox) $
  {# call gtk_combo_box_new #}

-- | Convenience function which constructs a new text combo box, which is a
-- 'ComboBox' just displaying strings. If you use this function to create a
-- text combo box, you should only manipulate its data source with the
-- following convenience functions: 'comboBoxAppendText', 'comboBoxInsertText',
-- 'comboBoxPrependText' and 'comboBoxRemoveText'.
--
comboBoxNewText :: IO ComboBox
comboBoxNewText =
  makeNewObject mkComboBox $
  liftM (castPtr :: Ptr Widget -> Ptr ComboBox) $
  {# call gtk_combo_box_new_text #}

-- | Creates a new 'ComboBox' with the model initialized to @model@.
--
comboBoxNewWithModel :: TreeModelClass model => 
    model       -- ^ @model@ - A 'TreeModel'.
 -> IO ComboBox
comboBoxNewWithModel model =
  makeNewObject mkComboBox $
  liftM (castPtr :: Ptr Widget -> Ptr ComboBox) $
  {# call gtk_combo_box_new_with_model #}
    (toTreeModel model)

--------------------
-- Methods

-- | Sets the wrap width of the combo box to be @width@. The wrap width is
-- basically the preferred number of columns when you want the popup to be
-- layed out in a table.
--
comboBoxSetWrapWidth :: ComboBoxClass self => self -> Int -> IO ()
comboBoxSetWrapWidth self width =
  {# call gtk_combo_box_set_wrap_width #}
    (toComboBox self)
    (fromIntegral width)

-- | Sets the column with row span information for the combo box to be @rowSpan@.
-- The row span column contains integers which indicate how many rows an item
-- should span.
--
comboBoxSetRowSpanColumn :: ComboBoxClass self => self -> Int -> IO ()
comboBoxSetRowSpanColumn self rowSpan =
  {# call gtk_combo_box_set_row_span_column #}
    (toComboBox self)
    (fromIntegral rowSpan)

-- | Sets the column with column span information for the combo box to be
-- @columnSpan@. The column span column contains integers which indicate how
-- many columns an item should span.
--
comboBoxSetColumnSpanColumn :: ComboBoxClass self => self -> Int -> IO ()
comboBoxSetColumnSpanColumn self columnSpan =
  {# call gtk_combo_box_set_column_span_column #}
    (toComboBox self)
    (fromIntegral columnSpan)

-- | Returns the index of the currently active item, or @Nothing@ if there's no
-- active item. If the model is a non-flat treemodel, and the active item is
-- not an immediate child of the root of the tree, this function returns
-- @'treePathGetIndices' path !! 0@, where @path@ is the 'TreePath' of the
-- active item.
--
comboBoxGetActive :: ComboBoxClass self => self
 -> IO (Maybe Int) -- ^ returns An integer which is the index of the currently
                   --  active item, or @Nothing@ if there's no active item.
comboBoxGetActive self =
  {# call gtk_combo_box_get_active #}
    (toComboBox self)
  >>= \index -> if index == -1
                  then return Nothing
                  else return (Just $ fromIntegral index)

-- | Sets the active item of the combo box to be the item at @index@.
--
comboBoxSetActive :: ComboBoxClass self => self
 -> Int   -- ^ @index@ - An index in the model passed during construction, or
          -- -1 to have no active item.
 -> IO ()
comboBoxSetActive self index =
  {# call gtk_combo_box_set_active #}
    (toComboBox self)
    (fromIntegral index)

-- | Returns a 'TreeIter' that points to the current active item, if it exists,
-- or @Nothing@ if there is no current active item.
--
comboBoxGetActiveIter :: ComboBoxClass self => self -> IO (Maybe TreeIter)
comboBoxGetActiveIter self =
  allocaBytes {# sizeof TreeIter #} $ \iterPtr -> do
  iter <- createTreeIter iterPtr
  wasSet <- liftM toBool $ {# call gtk_combo_box_get_active_iter #}
              (toComboBox self) iter
  if wasSet then return (Just iter)
            else return Nothing

-- | Sets the current active item to be the one referenced by @iter@. @iter@
-- must correspond to a path of depth one.
--
comboBoxSetActiveIter :: ComboBoxClass self => self
 -> TreeIter -- ^ @iter@ - The 'TreeIter'.
 -> IO ()
comboBoxSetActiveIter self iter =
  {# call gtk_combo_box_set_active_iter #}
    (toComboBox self)
    iter

-- | Returns the 'TreeModel' which is acting as data source for the combo box.
--
comboBoxGetModel :: ComboBoxClass self => self
 -> IO (Maybe TreeModel) -- ^ returns the 'TreeModel' which was passed during
                         -- construction.
comboBoxGetModel self =
  maybeNull (makeNewGObject mkTreeModel) $
  {# call gtk_combo_box_get_model #}
    (toComboBox self)

-- | Sets the model used by @comboBox@ to be @model@. Will unset a previously
-- set model (if applicable).
--
-- Note that this function does not clear the cell renderers, you have to
-- call 'comboBoxCellLayoutClear' yourself if you need to set up different cell
-- renderers for the new model.
--
comboBoxSetModel :: (ComboBoxClass self, TreeModelClass model) => self -> model -> IO ()
comboBoxSetModel self model =
  {# call gtk_combo_box_set_model #}
    (toComboBox self)
    (toTreeModel model)

-- | Appends the given string to the list of strings stored in the combo box.
-- Note that you can only use this function with combo boxes constructed with
-- 'comboBoxNewText'.
--
comboBoxAppendText :: ComboBoxClass self => self -> String -> IO ()
comboBoxAppendText self text =
  withUTFString text $ \textPtr ->
  {# call gtk_combo_box_append_text #}
    (toComboBox self)
    textPtr

-- | Inserts @text@ at @position@ in the list of strings stored in the
-- combo box. Note that you can only use this function with combo boxes
-- constructed with 'comboBoxNewText'.
--
comboBoxInsertText :: ComboBoxClass self => self
 -> Int    -- ^ @position@ - An index to insert @text@.
 -> String -- ^ @text@
 -> IO ()
comboBoxInsertText self position text =
  withUTFString text $ \textPtr ->
  {# call gtk_combo_box_insert_text #}
    (toComboBox self)
    (fromIntegral position)
    textPtr

-- | Prepends the given string to the list of strings stored in the combo box.
-- Note that you can only use this function with combo boxes constructed with
-- 'comboBoxNewText'.
--
comboBoxPrependText :: ComboBoxClass self => self -> String -> IO ()
comboBoxPrependText self text =
  withUTFString text $ \textPtr ->
  {# call gtk_combo_box_prepend_text #}
    (toComboBox self)
    textPtr

-- | Removes the string at @position@ from the combo box. Note that you can
-- only use this function with combo boxes constructed with 'comboBoxNewText'.
--
comboBoxRemoveText :: ComboBoxClass self => self
 -> Int   -- ^ @position@ - Index of the item to remove.
 -> IO ()
comboBoxRemoveText self position =
  {# call gtk_combo_box_remove_text #}
    (toComboBox self)
    (fromIntegral position)

-- | Pops up the menu or dropdown list of the combo box.
--
-- This function is mostly intended for use by accessibility technologies;
-- applications should have little use for it.
--
comboBoxPopup :: ComboBoxClass self => self -> IO ()
comboBoxPopup self =
  {# call gtk_combo_box_popup #}
    (toComboBox self)

-- | Hides the menu or dropdown list of the combo box.
--
-- This function is mostly intended for use by accessibility technologies;
-- applications should have little use for it.
--
comboBoxPopdown :: ComboBoxClass self => self -> IO ()
comboBoxPopdown self =
  {# call gtk_combo_box_popdown #}
    (toComboBox self)

--------------------
-- Signals

-- | The changed signal gets emitted when the active item is changed. This can
-- be due to the user selecting a different item from the list, or due to a
-- call to 'comboBoxSetActiveIter'.
--
onChanged, afterChanged :: ComboBoxClass self => self
 -> IO ()
 -> IO (ConnectId self)
onChanged = connect_NONE__NONE "changed" False
afterChanged = connect_NONE__NONE "changed" True
#endif
