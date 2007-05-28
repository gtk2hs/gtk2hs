-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget CellRendererCombo
--
--  Author : Duncan Coutts
--
--  Created: 2 November 2005
--
--  Version $Revision: 1.2 $ from $Date: 2005/11/18 15:41:07 $
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
-- | 'CellRendererCombo' renders text in a cell like 'CellRendererText' from
-- which it is derived. But while 'CellRendererText' offers a simple entry to
-- edit the text, 'CellRendererCombo' offers a 'ComboBox' or 'ComboBoxEntry'
-- widget to edit the text. The values to display in the combo box are taken
-- from the tree model specified in the model property.
--
-- The combo cell renderer takes care of adding a text cell renderer to the
-- combo box and sets it to display the column specified by its text-column
-- property. Further cell renderers can be added in a handler for the
-- editing-started signal.

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

-- * Constructors
  cellRendererComboNew,

-- * Attributes
  cellComboHasEntry,

#endif
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes			(Attr)
import System.Glib.Properties
import System.Glib.StoreValue                   (TMType(TMstring))
import System.Glib.GObject			(constructNewGObject)
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,6,0)
--------------------
-- Constructors

-- | Creates a new 'CellRendererCombo'. This 'Renderer' allows for displaying
--   a fixed set of options the user can choose from, or, using
--  'cellComboHasEntry', allows the user to add new elements. 
--
cellRendererComboNew :: IO CellRendererCombo
cellRendererComboNew = do
  ren <- makeNewObject mkCellRendererCombo $
	 liftM (castPtr :: Ptr CellRenderer -> Ptr CellRendererCombo) $
	 {# call gtk_cell_renderer_combo_new #}
  -- Create a fake model with one string column in it. The model itself is
  -- never used.
  mod <- constructNewGObject mkListStore $
	 withArray [fromIntegral (fromEnum TMstring)] $ \typesArr ->
	 {# call unsafe list_store_newv #} 1 typesArr
  objectSetPropertyGObject {# call pure unsafe gtk_tree_model_get_type #}
    "model" ren mod
  objectSetPropertyInt "text-column" ren 0  
  return ren

--------------------
-- Attributes

-- | If @True@, the cell renderer will allow the user to enter
-- values other than the ones in the popup list.
--
-- Default value: @True@
--
cellComboHasEntry :: CellRendererComboClass self => Attr self Bool
cellComboHasEntry = newAttrFromBoolProperty "has-entry"

#endif
