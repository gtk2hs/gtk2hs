-- -*-haskell-*-
--  GIMP Toolkit (GTK) CellRenderer TreeView
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.8 $ from $Date: 2005/11/18 15:41:07 $
--
--  Copyright (C) 1999-2006 Axel Simon
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
-- A 'CellRenderer' is an object that determines how the cell of a
-- 'TreeView' widget is displayed. 
--
-- * Each 'TreeViewColumn' has exactly one accociated 'CellRenderer'.
--   The data supply for a cell is contained in a 'TreeStore' or a
--   'ListStore' (both subclasses of 'TreeModel'). Each 'CellRenderer'
--   may have several attributes. Each 'Attribute' is associated with 
--   one column of the 'TreeModel' database. Thus several columns of a 
--   'TreeModel' may be the supply for one 'TreeViewColumn'.
--

module Graphics.UI.Gtk.TreeList.CellRenderer (
-- * Detail
-- 
-- | The 'CellRenderer' is a base class of a set of objects used for rendering
-- a cell to a 'Drawable'. These objects are used primarily by the 'TreeView'
-- widget, though they aren't tied to them in any specific way. It is worth
-- noting that 'CellRenderer' is not a 'Widget' and cannot be treated as such.
--
-- The primary use of a 'CellRenderer' is for drawing a certain graphical
-- elements on a 'Drawable'. Typically, one cell renderer is used to draw many
-- cells on the screen. To this extent, it isn't expected that a CellRenderer
-- keep any permanent state around. Instead, any state is set just prior to use
-- by changing the 'System.Glib.Attributes'. Then, the cell is measured and rendered
-- in the correct location
--
-- Beyond merely rendering a cell, cell renderers can optionally provide
-- active user interface elements. A cell renderer can be activatable like
-- 'CellRendererToggle', which toggles when it gets activated by a mouse click,
-- or it can be editable like 'CellRendererText', which allows the user to edit
-- the text using a 'Entry'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----CellRenderer
-- |               +----'CellRendererText'
-- |               +----'CellRendererPixbuf'
-- |               +----'CellRendererProgress'
-- |               +----'CellRendererCombo'
-- |               +----'CellRendererToggle'
-- @

-- * Types
  CellRenderer,
  CellRendererClass,
  castToCellRenderer,
  toCellRenderer,

-- * Methods
  cellBackground
  ) where

import Graphics.UI.Gtk.Types
import System.Glib.Attributes ( Attr, ReadAttr, WriteAttr )
import System.Glib.Properties

-- | Cell background color as a string.
--
cellBackground :: CellRendererClass cr => WriteAttr cr String
cellBackground = writeAttrFromStringProperty "cell-background"
