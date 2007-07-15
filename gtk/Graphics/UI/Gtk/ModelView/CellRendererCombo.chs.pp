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
-- Renders a combobox in a cell
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
  castToCellRendererCombo,
  toCellRendererCombo,
  TextModel,

-- * Constructors
  cellRendererComboNew,
  textModelNew,

-- * Methods
  textModelGetModel,

-- * Attributes
  cellComboHasEntry,
  cellComboTextModel
#endif
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes			(Attr, WriteAttr, writeAttr)
import System.Glib.Properties
import System.Glib.StoreValue                   (TMType(TMstring))
import System.Glib.GObject			(constructNewGObject)
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.ModelView.Types#}
{#import Graphics.UI.Gtk.ModelView.CustomStore#}

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


-- | An opaque value containing a tree model and a function extracting
-- a string from it. This value is used to set the 'cellComboTextModel'
-- property.
data TreeModelClass model => TextModel model = TextModel model ColumnId

-- Implementation note: it seems from the API that it might be possible to set
-- the model and the attributes of the combo box when the 'editingStarted'
-- signal of the 'CellRenderer'. However, this is not possible since the
-- 'CellRendererCombo' subbornly refuses to populate the cell with a combox
-- box if either the model or the text-column isn't set. Thus, unfortunately,
-- we always need to have a text in the combo box, even though it would be
-- perfectly reasonable to have, say, only icons. As a result of this stupid
-- behaviour, it is necessary to use the clumsy 'TextModel' machinery.
	
-- | Create an opaque value containing a tree model and a function extracting
-- a string from it. This value is used to set the 'cellComboTextModel'
-- property.
--
textModelNew :: (TreeModelClass (model row),
		 TypedTreeModelClass model)
  => model row		-- ^ the model which is to be used to fill the
			-- 'CellRendererCombo'
  -> (row -> String)	-- ^ a function to extract
  -> IO (TextModel (model row))
textModelNew model extract = do
  col <- treeModelUpdateColumn model (-1) (CAString extract)
  return (TextModel model col)

-- | Extact the model from the 'TextModel' value.
textModelGetModel :: TreeModelClass model => TextModel model -> model
textModelGetModel (TextModel m _) = m


--------------------
-- Attributes

-- | If @True@, the cell renderer will allow the user to enter
-- values other than the ones in the popup list.
--
-- Default value: @True@
--
cellComboHasEntry :: CellRendererComboClass self => Attr self Bool
cellComboHasEntry = newAttrFromBoolProperty "has-entry"

-- | The 'TextModel', that is, a tree model with a function that extracts a
-- string from which the options of the combo box are drawn. Note that this
-- tree model can be a datum in the tree model that is used to populate the
-- view in which the 'CellRendererCombo' is part of. In other words, it is
-- possible that every 'CellRendererCombo' can show a different set of options
-- on each row. 
--
cellComboTextModel :: ( TreeModelClass model,
			CellRendererComboClass self) =>
			WriteAttr self (TextModel model)
cellComboTextModel = writeAttr $ \cr (TextModel model row) -> do
  objectSetPropertyInt "text-column" cr row
  objectSetPropertyGObject {# call fun unsafe gtk_tree_model_get_type #}
    "model" cr model

#endif
