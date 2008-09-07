-- -*-haskell-*-
--  GIMP Toolkit (GTK) Old column-based tree/list widget system
--
--  Author : Axel Simon
--
--  Created: 7 August 2008
--
--  Copyright (C) 2006, 2008 Duncan Coutts, Axel Simon
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
-- Old model-based tree\/list widget system. These modules are all deprecated.
-- 
-- This just re-exports the Graphics.UI.Gtk.TreeList.* modules.
--
-- * Note: The modules will disappear in one of the next versions.
--
module Graphics.UI.Gtk.TreeList (
  module Graphics.UI.Gtk.TreeList.CellRenderer,
  module Graphics.UI.Gtk.TreeList.CellRendererPixbuf,
  module Graphics.UI.Gtk.TreeList.CellRendererText,
  module Graphics.UI.Gtk.TreeList.CellRendererToggle,
  module Graphics.UI.Gtk.TreeList.CellView,
  module Graphics.UI.Gtk.TreeList.IconView,
  module Graphics.UI.Gtk.TreeList.ListStore,
  module Graphics.UI.Gtk.TreeList.TreeModel,
  module Graphics.UI.Gtk.TreeList.TreeModelSort,
  module Graphics.UI.Gtk.TreeList.TreeRowReference,
  module Graphics.UI.Gtk.TreeList.TreeSelection,
  module Graphics.UI.Gtk.TreeList.TreeStore,
  module Graphics.UI.Gtk.TreeList.TreeView,
  module Graphics.UI.Gtk.TreeList.TreeViewColumn
  ) where

import Graphics.UI.Gtk.TreeList.CellRenderer
import Graphics.UI.Gtk.TreeList.CellRendererPixbuf
import Graphics.UI.Gtk.TreeList.CellRendererText
import Graphics.UI.Gtk.TreeList.CellRendererToggle
import Graphics.UI.Gtk.TreeList.CellView
import Graphics.UI.Gtk.TreeList.IconView
import Graphics.UI.Gtk.TreeList.ListStore
import Graphics.UI.Gtk.TreeList.TreeModel
import Graphics.UI.Gtk.TreeList.TreeModelSort
import Graphics.UI.Gtk.TreeList.TreeRowReference
import Graphics.UI.Gtk.TreeList.TreeSelection
import Graphics.UI.Gtk.TreeList.TreeStore hiding (
  GVboolean, TMType, TMboolean, TMdouble, TMenum, TMflags, TMfloat,
  TMint, TMinvalid, TMobject, TMstring, TMuint, GVdouble, GVenum,
  GVflags, GVfloat, GVint, GVobject, GVstring, GVuint, GenericValue )
import Graphics.UI.Gtk.TreeList.TreeView
import Graphics.UI.Gtk.TreeList.TreeViewColumn
