-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ComboBox
--
--  Author : Duncan Coutts
--
--  Created: 25 April 2004
--
--  Copyright (C) 2004-2007 Duncan Coutts, Axel Simon
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
module Graphics.UI.Gtk.ModelView.ComboBox (

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
-- tree model, and the display of the choices can be adapted to the data in
-- the model by using cell renderers, as you would in a tree view. This is
-- possible since 'ComboBox' implements the 'CellLayout' interface. The tree
-- model holding the valid choices is not restricted to a flat list, it can be
-- a real tree, and the popup will reflect the tree structure.
--
-- In addition to the general model-view API, 'ComboBox' offers the function
-- 'comboBoxNewText' which creates a text-only combo box.

-- * Class Hierarchy
--
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
  comboBoxSetRowSpanSource,
#if GTK_CHECK_VERSION(2,6,0)
  comboBoxSetColumnSpanSource,
#endif
  comboBoxGetActive,
  comboBoxSetActive,
  comboBoxGetActiveIter,
  comboBoxSetActiveIter,
  comboBoxGetModel,
  comboBoxSetModel,
  comboBoxPopup,
  comboBoxPopdown,
#if GTK_CHECK_VERSION(2,6,0)
  comboBoxGetWrapWidth,
  comboBoxSetAddTearoffs,
#endif
  comboBoxGetAddTearoffs,
#if GTK_CHECK_VERSION(2,6,0)
  comboBoxSetFocusOnClick,
  comboBoxGetFocusOnClick,
  comboBoxSetRowSeparatorSource,
#if GTK_CHECK_VERSION(2,10,0)
  comboBoxSetTitle,
  comboBoxGetTitle,
#endif
#endif

-- * Attributes
  comboBoxModel,
  comboBoxWrapWidth,
  comboBoxRowSpanColumn,
  comboBoxColumnSpanColumn,
  comboBoxActive,
#if GTK_CHECK_VERSION(2,6,0)
  comboBoxAddTearoffs,
  comboBoxHasFrame,
  comboBoxFocusOnClick,
#if GTK_CHECK_VERSION(2,10,0)
  comboBoxTearoffTitle,
  comboBoxPopupShown,
#endif
  comboBoxTitle,
#endif

-- * Signals
  changed,
  popupShownNotify,
  
-- * Deprecated
#ifndef DISABLE_DEPRECATED
  onChanged,
  afterChanged,
#endif
#endif
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import System.Glib.GObject		(makeNewGObject,
					 mkFunPtrDestroyNotify)
{#import Graphics.UI.Gtk.Types#} hiding (ListStore)
import Graphics.UI.Gtk.ModelView.Types (TypedTreeModelClass)
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.TreeList.TreeIter#} (TreeIter(..), receiveTreeIter)
{#import Graphics.UI.Gtk.ModelView.CustomStore#} (treeModelUpdateColumn,
                                                  treeModelGetRow,
                                                  ColumnAccess(CAInt))
import Graphics.UI.Gtk.ModelView.ListStore ( ListStore, listStoreNew )
import Graphics.UI.Gtk.ModelView.CellLayout ( cellLayoutSetAttributes,
					      cellLayoutPackStart )
import Graphics.UI.Gtk.ModelView.CellRendererText ( cellRendererTextNew, 
						    cellText)
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
-- text combo box, you can supply the @id@ function as first argument. In this
-- case 'comboBoxNewText' will return a @'Graphics.UI.Gtk.ModelView.ListStore'
-- String@ containing the initial list of strings.
--
comboBoxNewText ::
     (a -> String) -- ^ a function to extract elements from a the store
  -> [a] -- ^ the initial contents of the store
  -> IO (ComboBox, ListStore a) -- the resulting combo box and the store
comboBoxNewText extract initial = do
  store <- listStoreNew initial
  combo <- comboBoxNewWithModel store
  ren <- cellRendererTextNew
  cellLayoutPackStart combo ren True
  cellLayoutSetAttributes combo ren store (\a -> [cellText := extract a])
  return (combo, store)

-- %hash c:2570
-- | Creates a new 'ComboBox' with the model initialized to @model@.
--
comboBoxNewWithModel :: TreeModelClass model =>
    model -- ^ @model@ - A 'TreeModel'.
 -> IO ComboBox
comboBoxNewWithModel model =
  makeNewObject mkComboBox $
  liftM (castPtr :: Ptr Widget -> Ptr ComboBox) $
  {# call gtk_combo_box_new_with_model #}
    (toTreeModel model)

--------------------
-- Methods

-- %hash d:566e
-- | Sets the wrap width of @comboBox@ to be @width@. The wrap width is
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

-- | Sets the wrap width of the combo box to be @width@. The wrap width is
-- basically the preferred number of columns when you want the popup to be
-- laid out in a table.
--
comboBoxSetWrapWidth :: ComboBoxClass self => self -> Int -> IO ()
comboBoxSetWrapWidth self width =
  {# call gtk_combo_box_set_wrap_width #}
    (toComboBox self)
    (fromIntegral width)

-- %hash d:f80b
-- | Sets the source of the row span information for the combo box. The
-- row span source contains integers which indicate how many rows an
-- item should span.
--
comboBoxSetRowSpanSource :: (ComboBoxClass self,
			     TreeModelClass (model row),
			     TypedTreeModelClass model)
  => self -- ^ the 'ComboBox' widget
  -> Maybe (model row, row -> Int)
    -- ^ The model and a function to extract the number of rows
    -- from the model. If set to @Nothing@, the mapping is reset.
  -> IO ()
comboBoxSetRowSpanSource self Nothing =
  {# call gtk_combo_box_set_row_span_column #}
    (toComboBox self) (-1)
comboBoxSetRowSpanSource self (Just (model, extract)) = do
  modelPtr <- {#call unsafe gtk_combo_box_get_model#} (toComboBox self)
  let (TreeModel modelFPtr) = toTreeModel model
  if modelPtr/=unsafeForeignPtrToPtr modelFPtr then
    error ("comboBoxSetRowSpanSource: given model is different from what "++
	   "comboBoxGetModel returns") else do
  col <- {# call gtk_combo_box_get_row_span_column #} (toComboBox self)
  col <- treeModelUpdateColumn model (fromIntegral col) (CAInt extract)
  {#call gtk_combo_box_set_row_span_column #} (toComboBox self)
    (fromIntegral col)

#if GTK_CHECK_VERSION(2,6,0)
-- %hash d:4303
-- | Sets the source of the column span information for the combo box. The
-- column span source contains integers which indicate how many columns an
-- item should span.
--
-- * Available since Gtk+ version 2.6
--
comboBoxSetColumnSpanSource ::  (ComboBoxClass self,
			        TreeModelClass (model row),
			        TypedTreeModelClass model)
  => self -- ^ the 'ComboBox' widget
  -> Maybe (model row, row -> Int)
    -- ^ The model and a function to extract the number of rows
    -- from the model. If set to @Nothing@, the mapping is reset.
  -> IO ()
comboBoxSetColumnSpanSource self Nothing =
  {# call gtk_combo_box_set_column_span_column #}
    (toComboBox self) (-1)
comboBoxSetColumnSpanSource self (Just (model, extract)) = do
  modelPtr <- {#call unsafe gtk_combo_box_get_model#} (toComboBox self)
  let (TreeModel modelFPtr) = toTreeModel model
  if modelPtr/=unsafeForeignPtrToPtr modelFPtr then
    error ("comboBoxSetRowSpanSource: given model is different from what "++
	   "comboBoxGetModel returns") else do
  col <- {# call gtk_combo_box_get_column_span_column #} (toComboBox self)
  col <- treeModelUpdateColumn model (fromIntegral col) (CAInt extract)
  {#call gtk_combo_box_set_column_span_column #} (toComboBox self)
    (fromIntegral col)
#endif

-- %hash c:e719 d:e6a
-- | Returns the index of the currently active item, or -1 if there's no
-- active item. If the model is a non-flat treemodel, and the active item is
-- not an immediate child of the root of the tree, this function returns
-- @'treePathGetIndices' path !! 0@, where @path@ is the 'TreePath' of the
-- active item.
--
comboBoxGetActive :: ComboBoxClass self => self
 -> IO Int -- ^ returns An integer which is the index of the currently active
           -- item, or -1 if there's no active item.
comboBoxGetActive self =
  liftM fromIntegral $
  {# call gtk_combo_box_get_active #}
    (toComboBox self)

-- %hash c:3572 d:fbed
-- | Sets the active item of @comboBox@ to be the item at @index@.
--
comboBoxSetActive :: ComboBoxClass self => self
 -> Int -- ^ @index@ - An index in the model passed during construction, or -1
        -- to have no active item.
 -> IO ()
comboBoxSetActive self index =
  {# call gtk_combo_box_set_active #}
    (toComboBox self)
    (fromIntegral index)

-- %hash c:744a d:e897
-- | Returns a 'TreeIter' that points to the current active item, if it
-- exists, or @Nothing@ if there is no current active item.
--
comboBoxGetActiveIter :: ComboBoxClass self => self -> IO (Maybe TreeIter)
comboBoxGetActiveIter self =
  receiveTreeIter $ \iterPtr ->
  {# call gtk_combo_box_get_active_iter #}
    (toComboBox self)
    iterPtr

-- %hash c:9a70
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

-- %hash c:2460
-- | Returns the 'TreeModel' which is acting as data source for @comboBox@.
--
comboBoxGetModel :: (ComboBoxClass self, TreeModelClass model)
 => self
 -> IO (Maybe model) -- ^ returns A 'TreeModel' which was passed during
                         -- construction.
comboBoxGetModel self = liftM (fmap fromTreeModel) $
  maybeNull (makeNewGObject mkTreeModel) $
  {# call unsafe gtk_combo_box_get_model #}
    (toComboBox self)

-- %hash c:f5d0
-- | Sets the model used by @comboBox@ to be @model@. Will unset a previously
-- set model (if applicable). If model is @Nothing@, then it will unset the
-- model.
--
-- Note that this function does not clear the cell renderers, you have to call
-- 'comboBoxCellLayoutClear' yourself if you need to set up different cell
-- renderers for the new model.
--
comboBoxSetModel :: (ComboBoxClass self, TreeModelClass model) => self ->
  Maybe model -> IO ()
comboBoxSetModel self model =
  {# call gtk_combo_box_set_model #}
    (toComboBox self)
    (maybe (TreeModel nullForeignPtr) toTreeModel model)

-- | Pops up the menu or dropdown list of the combo box.
--
-- This function is mostly intended for use by accessibility technologies;
-- applications should have little use for it.
--
comboBoxPopup :: ComboBoxClass self => self -> IO ()
comboBoxPopup self =
  {# call gtk_combo_box_popup #}
    (toComboBox self)

-- %hash c:32a4 d:463e
-- | Hides the menu or dropdown list of @comboBox@.
--
-- This function is mostly intended for use by accessibility technologies;
-- applications should have little use for it.
--
comboBoxPopdown :: ComboBoxClass self => self -> IO ()
comboBoxPopdown self =
  {# call gtk_combo_box_popdown #}
    (toComboBox self)

-- %hash c:5bf8
-- | Sets whether the popup menu should have a tearoff menu item.
--
-- * Available since Gtk+ version 2.6
--
comboBoxSetAddTearoffs :: ComboBoxClass self => self
 -> Bool -- ^ @addTearoffs@ - @True@ to add tearoff menu items
 -> IO ()
comboBoxSetAddTearoffs self addTearoffs =
  {# call gtk_combo_box_set_add_tearoffs #}
    (toComboBox self)
    (fromBool addTearoffs)
#endif

-- | Gets the current value of the :add-tearoffs property.
--
comboBoxGetAddTearoffs :: ComboBoxClass self => self -> IO Bool
comboBoxGetAddTearoffs self =
  liftM toBool $
  {# call gtk_combo_box_get_add_tearoffs #}
    (toComboBox self)

#if GTK_CHECK_VERSION(2,6,0)
-- %hash c:fe18
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

-- %hash c:9168
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

-- %hash c:6fec d:a050
-- | Installs a mapping from the model to a row separator flag, which is used
-- to determine whether a row should be drawn as a separator. If the row
-- separator mapping is @Nothing@, no separators are drawn. This is the
-- default value.
--
-- * Available since Gtk+ version 2.6
--
comboBoxSetRowSeparatorSource :: (ComboBoxClass self,
 				  TreeModelClass (model row),
				  TypedTreeModelClass model)
 => self -- ^ the 'ComboBox' widget
 -> Maybe (model row, row -> Bool)
 -- ^ The model and a function to extract a Boolean from it.
 -> IO ()
comboBoxSetRowSeparatorSource self Nothing  =
  {# call gtk_combo_box_set_row_separator_func #}
    (toComboBox self) nullFunPtr nullPtr nullFunPtr
comboBoxSetRowSeparatorSource self (Just (model, extract)) = do
  funPtr <- mkRowSeparatorFunc $ \_ iterPtr -> do
	iter <- peek iterPtr
	value <- treeModelGetRow model iter
	return (extract value)
  desPtr <- mkFunPtrDestroyNotify funPtr
  {# call gtk_combo_box_set_row_separator_func #}
    (toComboBox self) funPtr nullPtr desPtr

{#pointer TreeViewRowSeparatorFunc#}

foreign import ccall "wrapper" mkRowSeparatorFunc ::
  (Ptr TreeModel -> Ptr TreeIter -> IO Bool) -> IO TreeViewRowSeparatorFunc

#if GTK_CHECK_VERSION(2,10,0)
-- %hash c:64db d:ecde
-- | Sets the menu's title in tearoff mode.
--
-- * Available since Gtk+ version 2.10
--
comboBoxSetTitle :: ComboBoxClass self => self
 -> String -- ^ @title@ - a title for the menu in tearoff mode.
 -> IO ()
comboBoxSetTitle self title =
  withUTFString title $ \titlePtr ->
  {# call gtk_combo_box_set_title #}
    (toComboBox self)
    titlePtr

-- %hash c:9f54 d:e396
-- | Gets the current title of the menu in tearoff mode. See
-- 'comboBoxSetAddTearoffs'.
--
-- * Available since Gtk+ version 2.10
--
comboBoxGetTitle :: ComboBoxClass self => self
 -> IO String -- ^ returns the menu's title in tearoff mode. This is an
              -- internal copy of the string which must not be freed.
comboBoxGetTitle self =
  {# call gtk_combo_box_get_title #}
    (toComboBox self)
  >>= peekUTFString
#endif
#endif

--------------------
-- Attributes

-- %hash c:c23c
-- | The model from which the combo box takes the values shown in the list.
--
comboBoxModel :: (ComboBoxClass self, TreeModelClass treeModel) => ReadWriteAttr self TreeModel treeModel
comboBoxModel = newAttrFromObjectProperty "model"
                  {# call pure unsafe gtk_tree_model_get_type #}

-- %hash c:ea5e
-- | If wrap-width is set to a positive value, the list will be displayed in
-- multiple columns, the number of columns is determined by wrap-width.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
comboBoxWrapWidth :: ComboBoxClass self => Attr self Int
comboBoxWrapWidth = newAttrFromIntProperty "wrap-width"

-- %hash c:a445
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
comboBoxRowSpanColumn = newAttrFromIntProperty "row-span-column"

-- %hash c:7ec7
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
comboBoxColumnSpanColumn = newAttrFromIntProperty "column-span-column"

-- %hash c:f777 d:507b
-- | The item which is currently active. If the model is a non-flat treemodel,
-- and the active item is not an immediate child of the root of the tree, this
-- property has the value @gtk_tree_path_get_indices (path)[0]@, where @path@
-- is the 'TreePath' of the active item.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
comboBoxActive :: ComboBoxClass self => Attr self Int
comboBoxActive = newAttrFromIntProperty "active"

#if GTK_CHECK_VERSION(2,6,0)
-- %hash c:585b d:2096
-- | The add-tearoffs property controls whether generated menus have tearoff
-- menu items.
--
-- Note that this only affects menu style combo boxes.
--
-- Default value: @False@
--
-- * Available since Gtk+ version 2.6
--
comboBoxAddTearoffs :: ComboBoxClass self => Attr self Bool
comboBoxAddTearoffs = newAttrFromBoolProperty "add-tearoffs"

-- %hash d:94cc
-- | The has-frame property controls whether a frame is drawn around the
-- entry.
--
-- Default value: @True@
--
-- * Available since Gtk+ version 2.6
--
comboBoxHasFrame :: ComboBoxClass self => Attr self Bool
comboBoxHasFrame = newAttrFromBoolProperty "has-frame"
#endif

-- %hash c:4808
-- | Whether the combo box grabs focus when it is clicked with the mouse.
--
-- Default value: @True@
--
comboBoxFocusOnClick :: ComboBoxClass self => Attr self Bool
comboBoxFocusOnClick = newAttrFromBoolProperty "focus-on-click"

#if GTK_CHECK_VERSION(2,10,0)
-- %hash c:c1e3 d:ddac
-- | A title that may be displayed by the window manager when the popup is
-- torn-off.
--
-- Default value: \"\"
--
-- * Available since Gtk+ version 2.10
--
comboBoxTearoffTitle :: ComboBoxClass self => Attr self String
comboBoxTearoffTitle = newAttrFromStringProperty "tearoff-title"

-- %hash c:efa9 d:89e5
-- | Whether the combo boxes dropdown is popped up. Note that this property is
-- mainly useful because it allows you to connect to notify::popup-shown.
--
-- Default value: @False@
--
-- * Available since Gtk+ version 2.10
--
comboBoxPopupShown :: ComboBoxClass self => ReadAttr self Bool
comboBoxPopupShown = readAttrFromBoolProperty "popup-shown"
#endif

-- %hash c:52a1 d:79e8
-- | \'title\' property. See 'comboBoxGetTitle' and 'comboBoxSetTitle'
--
comboBoxTitle :: ComboBoxClass self => Attr self String
comboBoxTitle = newAttr
  comboBoxGetTitle
  comboBoxSetTitle

--------------------
-- Signals

-- %hash c:4cee d:36c9
-- | The changed signal is emitted when the active item is changed. The can be
-- due to the user selecting a different item from the list, or due to a call
-- to 'comboBoxSetActiveIter'. It will also be emitted while typing into a
-- 'ComboBoxEntry', as well as when selecting an item from the
-- 'ComboBoxEntry''s list.
--
changed :: ComboBoxClass self => Signal self (IO ())
changed = Signal (connect_NONE__NONE "changed")

-- | The combo box was dropped down or collapsed.
--
popupShownNotify :: ComboBoxClass self => Signal self (IO ())
popupShownNotify = Signal (Connect_NONE__NONE "notify::popup-shown")

--------------------
-- Deprecated Signals

#ifndef DISABLE_DEPRECATED
-- %hash c:c149
onChanged :: ComboBoxClass self => self
 -> IO ()
 -> IO (ConnectId self)
onChanged = connect_NONE__NONE "changed" False
{-# DEPRECATED onChanged "instead of 'onChanged obj' use 'on obj changed'" #-}

-- %hash c:5e28
afterChanged :: ComboBoxClass self => self
 -> IO ()
 -> IO (ConnectId self)
afterChanged = connect_NONE__NONE "changed" True
{-# DEPRECATED afterChanged "instead of 'afterChanged obj' use 'after obj changed'" #-}
#endif
#endif
