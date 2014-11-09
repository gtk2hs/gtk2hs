{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) New model-based tree/list widget system
--
--  Author : Duncan Coutts
--
--  Created: 9 December 2006
--
--  Copyright (C) 2006 Duncan Coutts
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
-- New model-based tree\/list widget system.
--
-- This just re-exports the Graphics.UI.Gtk.ModelView.* modules.
--
-- * Note: From this version of Gtk2Hs this system will be the default
-- so it will not be necessary to explicitly import this module.
--
module Graphics.UI.Gtk.ModelView (
  module Graphics.UI.Gtk.ModelView.CellLayout,
  module Graphics.UI.Gtk.ModelView.CellRenderer,
  module Graphics.UI.Gtk.ModelView.CellRendererCombo,
  module Graphics.UI.Gtk.ModelView.CellRendererPixbuf,
  module Graphics.UI.Gtk.ModelView.CellRendererProgress,
  module Graphics.UI.Gtk.ModelView.CellRendererText,
  module Graphics.UI.Gtk.ModelView.CellRendererToggle,
  module Graphics.UI.Gtk.ModelView.CellView,
  module Graphics.UI.Gtk.MenuComboToolbar.ComboBox,
#if GTK_MAJOR_VERSION < 3
  module Graphics.UI.Gtk.MenuComboToolbar.ComboBoxEntry,
#endif
  module Graphics.UI.Gtk.ModelView.CustomStore,
  module Graphics.UI.Gtk.Entry.EntryCompletion,
  module Graphics.UI.Gtk.ModelView.IconView,
  module Graphics.UI.Gtk.ModelView.ListStore,
  module Graphics.UI.Gtk.ModelView.TreeDrag,
  module Graphics.UI.Gtk.ModelView.TreeModel,
  module Graphics.UI.Gtk.ModelView.TreeModelSort,
  module Graphics.UI.Gtk.ModelView.TreeSortable,
  module Graphics.UI.Gtk.ModelView.TreeRowReference,
  module Graphics.UI.Gtk.ModelView.TreeSelection,
  module Graphics.UI.Gtk.ModelView.TreeStore,
  module Graphics.UI.Gtk.ModelView.TreeView,
  module Graphics.UI.Gtk.ModelView.TreeViewColumn
  ) where

import Graphics.UI.Gtk.ModelView.CellLayout
import Graphics.UI.Gtk.ModelView.CellRenderer
import Graphics.UI.Gtk.ModelView.CellRendererCombo
import Graphics.UI.Gtk.ModelView.CellRendererPixbuf
import Graphics.UI.Gtk.ModelView.CellRendererProgress
import Graphics.UI.Gtk.ModelView.CellRendererText
import Graphics.UI.Gtk.ModelView.CellRendererToggle
import Graphics.UI.Gtk.ModelView.CellView
import Graphics.UI.Gtk.MenuComboToolbar.ComboBox -- these moved back to MenuComboToolbar
#if GTK_MAJOR_VERSION < 3
import Graphics.UI.Gtk.MenuComboToolbar.ComboBoxEntry
#endif
import Graphics.UI.Gtk.ModelView.CustomStore
import Graphics.UI.Gtk.Entry.EntryCompletion -- this moved back to Entry
import Graphics.UI.Gtk.ModelView.IconView
import Graphics.UI.Gtk.ModelView.ListStore
import Graphics.UI.Gtk.ModelView.TreeDrag
import Graphics.UI.Gtk.ModelView.TreeModel
import Graphics.UI.Gtk.ModelView.TreeModelSort
import Graphics.UI.Gtk.ModelView.TreeSortable
import Graphics.UI.Gtk.ModelView.TreeRowReference
import Graphics.UI.Gtk.ModelView.TreeSelection
import Graphics.UI.Gtk.ModelView.TreeStore
import Graphics.UI.Gtk.ModelView.TreeView
import Graphics.UI.Gtk.ModelView.TreeViewColumn
