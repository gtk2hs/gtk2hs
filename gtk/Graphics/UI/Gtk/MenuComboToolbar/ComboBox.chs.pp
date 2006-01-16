-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ComboBox
--
--  Author : Duncan Coutts
--
--  Created: 25 April 2004
--
--  Version $Revision: 1.12 $ from $Date: 2005/10/19 12:57:37 $
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
  toComboBox,

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
#if GTK_CHECK_VERSION(2,6,0)
  comboBoxGetWrapWidth,
  comboBoxGetRowSpanColumn,
  comboBoxGetColumnSpanColumn,
  comboBoxGetActiveText,
  comboBoxSetAddTearoffs,
  comboBoxGetAddTearoffs,
  comboBoxSetFocusOnClick,
  comboBoxGetFocusOnClick,
#endif

-- * Attributes
#if GTK_CHECK_VERSION(2,6,0)
  comboBoxModel,
  comboBoxWrapWidth,
  comboBoxRowSpanColumn,
  comboBoxColumnSpanColumn,
  comboBoxAddTearoffs,
  comboBoxHasFrame,
  comboBoxFocusOnClick,
#endif

-- * Signals
  onChanged,
  afterChanged,
#endif
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import System.Glib.GObject		(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.TreeList.TreeIter#} (TreeIter(..), receiveTreeIter)

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
  receiveTreeIter $ \iterPtr ->
  {# call gtk_combo_box_get_active_iter #}
    (toComboBox self)
    iterPtr

-- | Sets the current active item to be the one referenced by @iter@. @iter@
-- must correspond to a path of depth one.
--
comboBoxSetActiveIter :: ComboBoxClass self => self
 -> TreeIter -- ^ @iter@ - The 'TreeIter'.
 -> IO ()
comboBoxSetActiveIter self iter =
  with iter $ \iterPtr ->
  {# call gtk_combo_box_set_active_iter #}
    (toComboBox self)
    iterPtr

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
-- set model (if applicable). If model is @Nothing@, then it will unset the
-- model.
--
-- Note that this function does not clear the cell renderers, you have to
-- call 'comboBoxCellLayoutClear' yourself if you need to set up different cell
-- renderers for the new model.
--
comboBoxSetModel :: (ComboBoxClass self, TreeModelClass model) => self -> Maybe model -> IO ()
comboBoxSetModel self model =
  {# call gtk_combo_box_set_model #}
    (toComboBox self)
    (maybe (TreeModel nullForeignPtr) toTreeModel model)

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

#if GTK_CHECK_VERSION(2,6,0)
-- | Returns the wrap width which is used to determine the number of columns
-- for the popup menu. If the wrap width is larger than 1, the combo box is in
-- table mode.
--
-- * Available since Gtk+ version 2.6
--
comboBoxGetWrapWidth :: ComboBoxClass self => self -> IO Int
comboBoxGetWrapWidth self =
  liftM fromIntegral $
  {# call gtk_combo_box_get_wrap_width #}
    (toComboBox self)

-- | Returns the column with row span information for @comboBox@.
--
-- * Available since Gtk+ version 2.6
--
comboBoxGetRowSpanColumn :: ComboBoxClass self => self -> IO Int
comboBoxGetRowSpanColumn self =
  liftM fromIntegral $
  {# call gtk_combo_box_get_row_span_column #}
    (toComboBox self)

-- | Returns the column with column span information for @comboBox@.
--
-- * Available since Gtk+ version 2.6
--
comboBoxGetColumnSpanColumn :: ComboBoxClass self => self -> IO Int
comboBoxGetColumnSpanColumn self =
  liftM fromIntegral $
  {# call gtk_combo_box_get_column_span_column #}
    (toComboBox self)

-- | Returns the currently active string in @comboBox@ or @Nothing@ if none is
-- selected. Note that you can only use this function with combo boxes
-- constructed with 'comboBoxNewText' and with 'ComboBoxEntry's.
--
-- * Available since Gtk+ version 2.6
--
comboBoxGetActiveText :: ComboBoxClass self => self -> IO (Maybe String)
comboBoxGetActiveText self =
  {# call gtk_combo_box_get_active_text #}
    (toComboBox self)
  >>= maybePeek readUTFString

-- | Sets whether the popup menu should have a tearoff menu item.
--
-- * Available since Gtk+ version 2.6
--
comboBoxSetAddTearoffs :: ComboBoxClass self => self
 -> Bool  -- ^ @addTearoffs@ - @True@ to add tearoff menu items
 -> IO ()
comboBoxSetAddTearoffs self addTearoffs =
  {# call gtk_combo_box_set_add_tearoffs #}
    (toComboBox self)
    (fromBool addTearoffs)

-- | Gets the current value of the :add-tearoffs property.
--
comboBoxGetAddTearoffs :: ComboBoxClass self => self -> IO Bool
comboBoxGetAddTearoffs self =
  liftM toBool $
  {# call gtk_combo_box_get_add_tearoffs #}
    (toComboBox self)

-- | Sets whether the combo box will grab focus when it is clicked with the
-- mouse. Making mouse clicks not grab focus is useful in places like toolbars
-- where you don't want the keyboard focus removed from the main area of the
-- application.
--
-- * Available since Gtk+ version 2.6
--
comboBoxSetFocusOnClick :: ComboBoxClass self => self
 -> Bool  -- ^ @focusOnClick@ - whether the combo box grabs focus when clicked
          -- with the mouse
 -> IO ()
comboBoxSetFocusOnClick self focusOnClick =
  {# call gtk_combo_box_set_focus_on_click #}
    (toComboBox self)
    (fromBool focusOnClick)

-- | Returns whether the combo box grabs focus when it is clicked with the
-- mouse. See 'comboBoxSetFocusOnClick'.
--
-- * Available since Gtk+ version 2.6
--
comboBoxGetFocusOnClick :: ComboBoxClass self => self
 -> IO Bool -- ^ returns @True@ if the combo box grabs focus when it is
            -- clicked with the mouse.
comboBoxGetFocusOnClick self =
  liftM toBool $
  {# call gtk_combo_box_get_focus_on_click #}
    (toComboBox self)
#endif

--------------------
-- Attributes

#if GTK_CHECK_VERSION(2,6,0)
-- | The model from which the combo box takes the values shown in the list.
--
comboBoxModel :: (ComboBoxClass self, TreeModelClass model) => ReadWriteAttr self (Maybe TreeModel) (Maybe model)
comboBoxModel = newAttr
  comboBoxGetModel
  comboBoxSetModel

-- | If wrap-width is set to a positive value, the list will be displayed in
-- multiple columns, the number of columns is determined by wrap-width.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
comboBoxWrapWidth :: ComboBoxClass self => Attr self Int
comboBoxWrapWidth = newAttr
  comboBoxGetWrapWidth
  comboBoxSetWrapWidth

-- | If this is set to a non-negative value, it must be the index of a column
-- of type @G_TYPE_INT@ in the model.
--
-- The values of that column are used to determine how many rows a value in
-- the list will span. Therefore, the values in the model column pointed to by
-- this property must be greater than zero and not larger than wrap-width.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
comboBoxRowSpanColumn :: ComboBoxClass self => Attr self Int
comboBoxRowSpanColumn = newAttr
  comboBoxGetRowSpanColumn
  comboBoxSetRowSpanColumn

-- | If this is set to a non-negative value, it must be the index of a column
-- of type @G_TYPE_INT@ in the model.
--
-- The values of that column are used to determine how many columns a value
-- in the list will span.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
comboBoxColumnSpanColumn :: ComboBoxClass self => Attr self Int
comboBoxColumnSpanColumn = newAttr
  comboBoxGetColumnSpanColumn
  comboBoxSetColumnSpanColumn

-- | The add-tearoffs property controls whether generated menus have tearoff
-- menu items.
--
-- Note that this only affects menu style combo boxes.
--
-- Default value: @False@
--
comboBoxAddTearoffs :: ComboBoxClass self => Attr self Bool
comboBoxAddTearoffs = newAttr
  comboBoxGetAddTearoffs
  comboBoxSetAddTearoffs

-- | The has-frame property controls whether a frame is drawn around the
-- entry.
--
-- Default value: @True@
--
comboBoxHasFrame :: ComboBoxClass self => Attr self Bool
comboBoxHasFrame = newAttrFromBoolProperty "has-frame"

-- | Whether the combo box grabs focus when it is clicked with the mouse.
--
-- Default value: @True@
--
comboBoxFocusOnClick :: ComboBoxClass self => Attr self Bool
comboBoxFocusOnClick = newAttr
  comboBoxGetFocusOnClick
  comboBoxSetFocusOnClick
#endif

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
