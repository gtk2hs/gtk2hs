-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ComboBox
--
--  Author : Duncan Coutts
--
--  Created: 25 April 2004
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/12 17:19:23 $
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
-- A widget used to choose from a list of items.
--
-- * Added in Gtk 2.4
--
module Graphics.UI.Gtk.MenuComboToolbar.ComboBox (
#if GTK_CHECK_VERSION(2,4,0)
  ComboBoxClass,
  ComboBox,
  comboBoxNew,
  comboBoxNewWithModel,
  comboBoxSetWrapWidth,
  comboBoxSetRowSpanColumn,
  comboBoxSetColumnSpanColumn,
  comboBoxGetActive,
  comboBoxSetActive,
  comboBoxGetActiveIter,
  comboBoxSetActiveIter,
  comboBoxGetModel,
  comboBoxSetModel,
  comboBoxNewText,
  comboBoxAppendText,
  comboBoxInsertText,
  comboBoxPrependText,
  comboBoxRemoveText,
  comboBoxPopup,
  comboBoxPopdown
#endif
) where

#if GTK_CHECK_VERSION(2,4,0)

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
import System.Glib.GObject		(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.TreeList.TreeModel#} (TreeIter(..), createTreeIter)

{# context lib="gtk" prefix ="gtk" #}


comboBoxNew :: IO ComboBox
comboBoxNew =
  makeNewObject mkComboBox $ liftM castPtr $
  {# call gtk_combo_box_new #}

comboBoxNewWithModel :: TreeModel -> IO ComboBox
comboBoxNewWithModel model =
  makeNewObject mkComboBox $ liftM castPtr $
  {# call gtk_combo_box_new_with_model #} model

comboBoxSetWrapWidth :: ComboBoxClass combo => combo -> Int -> IO ()
comboBoxSetWrapWidth combo width =
  {# call gtk_combo_box_set_wrap_width #} (toComboBox combo)
    (fromIntegral width)

comboBoxSetRowSpanColumn :: ComboBoxClass combo => combo -> Int -> IO ()
comboBoxSetRowSpanColumn combo rowSpan =
  {# call gtk_combo_box_set_row_span_column #} (toComboBox combo)
    (fromIntegral rowSpan)

comboBoxSetColumnSpanColumn :: ComboBoxClass combo => combo -> Int -> IO ()
comboBoxSetColumnSpanColumn combo columnSpan =
  {# call gtk_combo_box_set_column_span_column #} (toComboBox combo)
    (fromIntegral columnSpan)

comboBoxGetActive :: ComboBoxClass combo => combo -> IO (Maybe Int)
comboBoxGetActive combo = do
  index <- {# call gtk_combo_box_get_active #} (toComboBox combo)
  if index == -1 then return Nothing
                 else return (Just $ fromIntegral index)

comboBoxSetActive :: ComboBoxClass combo => combo -> Int -> IO ()
comboBoxSetActive combo index =
  {# call gtk_combo_box_set_active #} (toComboBox combo)
    (fromIntegral index)

comboBoxGetActiveIter :: ComboBoxClass combo => combo -> IO (Maybe TreeIter)
comboBoxGetActiveIter combo =
  allocaBytes {# sizeof TreeIter #} $ \iterPtr -> do
  iter <- createTreeIter iterPtr
  wasSet <- liftM toBool $ {# call gtk_combo_box_get_active_iter #}
              (toComboBox combo) iter
  if wasSet then return (Just iter)
            else return Nothing

comboBoxSetActiveIter :: ComboBoxClass combo => combo -> TreeIter -> IO ()
comboBoxSetActiveIter combo iter =
  {# call gtk_combo_box_set_active_iter #} (toComboBox combo) iter

comboBoxGetModel :: ComboBoxClass combo => combo -> IO (Maybe TreeModel)
comboBoxGetModel combo = do
  modelPtr <- {# call gtk_combo_box_get_model #} (toComboBox combo)
  if modelPtr == nullPtr
    then return Nothing
    else liftM Just $ makeNewGObject mkTreeModel (return modelPtr)

comboBoxSetModel :: ComboBoxClass combo => combo -> TreeModel -> IO ()
comboBoxSetModel combo model =
  {# call gtk_combo_box_set_model #} (toComboBox combo) model

comboBoxNewText :: IO ComboBox
comboBoxNewText =
  makeNewObject mkComboBox $ liftM castPtr $
  {# call gtk_combo_box_new_text #}

comboBoxAppendText :: ComboBoxClass combo => combo -> String -> IO ()
comboBoxAppendText combo text =
  withUTFString text $ \strPtr ->
  {# call gtk_combo_box_append_text #} (toComboBox combo) strPtr

comboBoxInsertText :: ComboBoxClass combo => combo -> Int -> String -> IO ()
comboBoxInsertText combo index text =
  withUTFString text $ \strPtr ->
  {# call gtk_combo_box_insert_text #} (toComboBox combo)
    (fromIntegral index) strPtr

comboBoxPrependText :: ComboBoxClass combo => combo -> String -> IO ()
comboBoxPrependText combo text =
    withUTFString text $ \strPtr ->
  {# call gtk_combo_box_prepend_text #} (toComboBox combo) strPtr

comboBoxRemoveText :: ComboBoxClass combo => combo -> Int -> IO ()
comboBoxRemoveText combo index =
  {# call gtk_combo_box_remove_text #} (toComboBox combo) (fromIntegral index)

comboBoxPopup :: ComboBoxClass combo => combo -> IO ()
comboBoxPopup combo =
  {# call gtk_combo_box_popup #} (toComboBox combo)

comboBoxPopdown :: ComboBoxClass combo => combo -> IO ()
comboBoxPopdown combo =
  {# call gtk_combo_box_popdown #} (toComboBox combo)

#endif
