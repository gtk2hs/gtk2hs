{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget CellRendererCombo
--
--  Author : Duncan Coutts
--
--  Created: 2 November 2005
--
--  Copyright (C) 2005 Duncan Coutts
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
-- Renders a combo box in a cell
--
-- * Module available since Gtk+ version 2.6
--
module Graphics.UI.Gtk.ModelView.CellRendererCombo (
-- * Detail
--
-- | 'CellRendererCombo' renders text in a cell like
-- 'Graphics.UI.Gtk.ModelView.CellRendererText' from which it is derived. But
-- while 'Graphics.UI.Gtk.ModelView.CellRendererText' offers a simple entry to
-- edit the text, 'CellRendererCombo' offers a
-- 'Graphics.UI.Gtk.ModelView.ComboBox' or
-- 'Graphics.UI.Gtk.ModelView.ComboBoxEntry' widget to edit the text. The
-- values to display in the combo box are taken from the tree model specified
-- in the model property.
--
-- The combo cell renderer takes care of adding a text cell renderer to the
-- combo box and sets it to display the column specified by its
-- 'cellTextModel' property. Further cell renderers can be added in a handler
-- for the 'Graphics.UI.Gtk.ModelView.CellRenderer.editingStarted' signal.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'CellRenderer'
-- |               +----'CellRendererText'
-- |                     +----CellRendererCombo
-- @

#if GTK_CHECK_VERSION(2,6,0)
-- * Types
  CellRendererCombo,
  CellRendererComboClass,
  castToCellRendererCombo, gTypeCellRendererCombo,
  toCellRendererCombo,

-- * Constructors
  cellRendererComboNew,

-- * Attributes
  cellComboHasEntry,
  cellComboTextModel
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes                   (Attr, WriteAttr, writeAttr)
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object          (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.ModelView.Types#}
{#import Graphics.UI.Gtk.ModelView.TreeModel#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,6,0)
--------------------
-- Constructors

-- | Creates a new 'CellRendererCombo'. This 'Renderer' allows for displaying
--   a fixed set of options the user can choose from.
--
cellRendererComboNew :: IO CellRendererCombo
cellRendererComboNew = do
  makeNewObject mkCellRendererCombo $
         liftM (castPtr :: Ptr CellRenderer -> Ptr CellRendererCombo) $
         {# call gtk_cell_renderer_combo_new #}

--------------------
-- Attributes

-- | If @True@, the cell renderer will allow the user to enter
-- values other than the ones in the popup list.
--
-- Default value: @True@
--
cellComboHasEntry :: CellRendererComboClass self => Attr self Bool
cellComboHasEntry = newAttrFromBoolProperty "has-entry"

-- | The tuple containing a model and a column in this model that determine
--   the possible strings that can be shown in the combo box. Note that this
--   tree model can be a datum in the tree model that is used to populate the
--   view in which the 'CellRendererCombo' is part of. In other words, it is
--   possible that every 'CellRendererCombo' can show a different set of
--   options on each row.
--
cellComboTextModel :: ( TreeModelClass (model row),
                        TypedTreeModelClass model,
                        CellRendererComboClass self,
                        GlibString string) =>
                        WriteAttr self (model row, ColumnId row string)
cellComboTextModel = writeAttr setter
  where
  setter cr (model, col) = do
    objectSetPropertyInt "text-column" cr
      ((fromIntegral . columnIdToNumber) col)
    objectSetPropertyGObject {# call fun unsafe gtk_tree_model_get_type #}
      "model" cr (toTreeModel model)

#endif
