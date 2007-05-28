-- -*-haskell-*-
--  GIMP Toolkit (GTK) CellRenderer TreeView
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
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
-- * Each 'TreeViewColumn' has one or more accociated 'CellRenderer's.
--   The data supply for a cell is contained in a 'TreeStore' or a
--   'ListStore' (both subclasses of 'TreeModel'). Each 'CellRenderer'
--   may have several attributes. Each attribute is associated with 
--   one column of the 'TreeModel' database. Thus, several columns of a 
--   'TreeModel' may be the supply for one 'TreeViewColumn'.
--

module Graphics.UI.Gtk.ModelView.CellRenderer (
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

-- * Attributes
  cellMode,
  cellVisible,
  cellSensitive,
  cellXAlign,
  cellYAlign,
  cellXPad,
  cellYPad,
  cellWidth,
  cellHeight,
  cellIsExpander,
  cellIsExpanded,
  cellBackground,
  cellBackgroundColor,
  cellBackgroundSet,

#if GTK_CHECK_VERSION(2,4,0)
-- * Signals
  onEditingStarted,
  afterEditingStarted
#endif
  ) where

import System.Glib.FFI
import System.Glib.Attributes ( Attr, WriteAttr )
import System.Glib.Properties
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.Gdk.GC		(Color)
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.TreeList.TreePath#}

{#context lib="gtk" prefix ="gtk"#}

{# enum CellRendererMode {underscoreToCase} deriving (Eq) #}

--------------------
-- Attributes

-- | Editable mode of the CellRenderer.
--
-- Default value: 'CellRendererModeInert'
--
cellMode :: CellRendererClass self => Attr self CellRendererMode
cellMode = newAttrFromEnumProperty "mode"
  {# call pure unsafe gtk_cell_renderer_mode_get_type #}

-- | Display the cell.
--
-- Default value: @True@
--
cellVisible :: CellRendererClass self => Attr self Bool
cellVisible = newAttrFromBoolProperty "visible"

-- | Display the cell sensitive.
--
-- Default value: @True@
--
cellSensitive :: CellRendererClass self => Attr self Bool
cellSensitive = newAttrFromBoolProperty "sensitive"

-- | The x-align.
--
-- Allowed values: @[0,1]@
--
-- Default value: @0.5@
--
cellXAlign :: CellRendererClass self => Attr self Float
cellXAlign = newAttrFromFloatProperty "xalign"

-- | The y-align.
--
-- Allowed values: @[0,1]@
--
-- Default value: @0.5@
--
cellYAlign :: CellRendererClass self => Attr self Float
cellYAlign = newAttrFromFloatProperty "yalign"

-- | The xpad.
--
-- Default value: @0@
--
cellXPad :: CellRendererClass self => Attr self Int
cellXPad = newAttrFromUIntProperty "xpad"

-- | The ypad.
--
-- Default value: @0@
--
cellYPad :: CellRendererClass self => Attr self Int
cellYPad = newAttrFromUIntProperty "ypad"

-- | The fixed width.
--
-- Allowed values: @>= -1@
--
-- Default value: @-1@
--
cellWidth :: CellRendererClass self => Attr self Int
cellWidth = newAttrFromIntProperty "width"

-- | The fixed height.
--
-- Allowed values: @>= -1@
--
-- Default value: @-1@
--
cellHeight :: CellRendererClass self => Attr self Int
cellHeight = newAttrFromIntProperty "height"

-- | Row has children.
--
-- Default value: @False@
--
cellIsExpander :: CellRendererClass self => Attr self Bool
cellIsExpander = newAttrFromBoolProperty "is-expander"

-- | Row is an expander row, and is expanded.
--
-- Default value: @False@
--
cellIsExpanded :: CellRendererClass self => Attr self Bool
cellIsExpanded = newAttrFromBoolProperty "is-expanded"

-- | Cell background color as a string.
--
-- Default value: @\"\"@
--
cellBackground :: CellRendererClass self => WriteAttr self String
cellBackground = writeAttrFromStringProperty "cell-background"

-- | Cell background color as a 'Color'.
--
cellBackgroundColor :: CellRendererClass self => Attr self Color
cellBackgroundColor = newAttrFromBoxedStorableProperty "cell-background-gdk"
  {# call pure unsafe gdk_color_get_type #}

-- | Whether the 'cellBackground' \/ 'cellBackgroundColor' attribute is set.
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellBackgroundSet :: CellRendererClass self => Attr self Bool
cellBackgroundSet = newAttrFromBoolProperty "cell-background-set"

#if GTK_CHECK_VERSION(2,4,0)
-- | This signal gets emitted when a cell starts to be edited.
--
-- * The indended
--   use of this signal is to do special setup on the widget that is created
--   to allow the editing process. For example, the 'CellRendererText' uses
--   an 'Entry' widget which has an 'EntryCompletion' interface. On reception
--   of this signal, the program can set the model from which to retrieve the
--   completions.
--
onEditingStarted, afterEditingStarted :: CellRendererClass self => self
 -> (CellEditable -> TreePath -> IO ())
 -> IO (ConnectId self)
onEditingStarted cr act =
  connect_OBJECT_STRING__NONE "editing-started" False cr
  $ \ce path -> act ce (stringToTreePath path)
afterEditingStarted cr act =
  connect_OBJECT_STRING__NONE "editing-started" True cr
  $ \ce path -> act ce (stringToTreePath path)
#endif
