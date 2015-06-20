{-# LANGUAGE CPP #-}
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
-- An object for rendering a cell in a list, icon or combo box widget.
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
-- cells on the screen. To this extent, it isn't expected that a
-- 'CellRenderer' keep any permanent state around. Instead, any state is set
-- just prior to use by changing the attributes of the cell. Then, the cell is
-- measured and rendered in the correct location.
--
-- Beyond merely rendering a cell, cell renderers can optionally provide
-- active user interface elements. A cell renderer can be activatable like
-- 'Graphics.UI.Gtk.ModelView.CellRendererToggle', which toggles when it gets
-- activated by a mouse click, or it can be editable like
-- 'Graphics.UI.Gtk.ModelView.CellRendererText', which allows the user to edit
-- the text using a 'Graphics.UI.Gtk.Entry.Entry'.

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
  castToCellRenderer, gTypeCellRenderer,
  toCellRenderer,
  CellRendererMode(..),

-- * Methods
#if GTK_CHECK_VERSION(2,6,0)
  cellRendererStopEditing,
#endif
  cellRendererGetFixedSize,
  cellRendererSetFixedSize,

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
#if GTK_MAJOR_VERSION < 3
  cellBackgroundColor,
#endif
  cellBackgroundSet,

-- * Signals
#if GTK_CHECK_VERSION(2,6,0)
  editingStarted,
#endif
#if GTK_CHECK_VERSION(2,4,0)
  editingCanceled,
#endif

-- * Deprecated
#ifndef DISABLE_DEPRECATED
#if GTK_CHECK_VERSION(2,6,0)
  onEditingStarted,
  afterEditingStarted,
#endif
#if GTK_CHECK_VERSION(2,4,0)
  onEditingCanceled,
  afterEditingCanceled,
#endif
#endif
  ) where

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes ( Attr, WriteAttr )
import System.Glib.Properties
{#import Graphics.UI.Gtk.Types#}
#if GTK_MAJOR_VERSION < 3
import Graphics.UI.Gtk.Gdk.GC           (Color)
#endif
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.ModelView.Types#}

{#context lib="gtk" prefix ="gtk"#}

{# enum CellRendererMode {underscoreToCase} deriving (Eq) #}


#if GTK_CHECK_VERSION(2,6,0)
-- %hash c:75b3 d:45ca
-- | Informs the cell renderer that the editing is stopped. If @canceled@ is
-- @True@, the cell renderer will emit the 'editingCanceled' signal.
--
-- * Available since Gtk+ version 2.6
--
cellRendererStopEditing :: CellRendererClass self => self
 -> Bool -- ^ @canceled@ - @True@ if the editing has been canceled
 -> IO ()
cellRendererStopEditing self canceled =
  {# call gtk_cell_renderer_stop_editing #}
    (toCellRenderer self)
    (fromBool canceled)
#endif

-- %hash c:6d51 d:dc3e
-- | Returns    @(width, height)@       denoting the size of the fixed size of
-- @cell@. If no fixed size is set, returns @-1@ for that value.
--
cellRendererGetFixedSize :: CellRendererClass self => self
 -> IO (Int, Int) -- ^ @(width, height)@
cellRendererGetFixedSize self =
  alloca $ \widthPtr ->
  alloca $ \heightPtr ->
  {# call gtk_cell_renderer_get_fixed_size #}
    (toCellRenderer self)
    widthPtr
    heightPtr >>
  peek widthPtr >>= \width ->
  peek heightPtr >>= \height ->
  return (fromIntegral width, fromIntegral height)

-- %hash c:85dc d:5fd4
-- | Sets the renderer size to be explicit, independent of the properties set.
--
cellRendererSetFixedSize :: CellRendererClass self => self
 -> Int -- ^ @width@ - the width of the cell renderer, or -1
 -> Int -- ^ @height@ - the height of the cell renderer, or -1
 -> IO ()
cellRendererSetFixedSize self width height =
  {# call gtk_cell_renderer_set_fixed_size #}
    (toCellRenderer self)
    (fromIntegral width)
    (fromIntegral height)

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
cellBackground :: (CellRendererClass self, GlibString string) => WriteAttr self string
cellBackground = writeAttrFromStringProperty "cell-background"

#if GTK_MAJOR_VERSION < 3
-- | Cell background color as a 'Color'.
--
-- Removed in Gtk3.
cellBackgroundColor :: CellRendererClass self => Attr self Color
cellBackgroundColor = newAttrFromBoxedStorableProperty "cell-background-gdk"
  {# call pure unsafe gdk_color_get_type #}
#endif

-- | Whether the 'cellBackground' \/ 'cellBackgroundColor' attribute is set.
--
-- You can use this to reset the attribute to its default.
--
-- Default value: @False@
--
cellBackgroundSet :: CellRendererClass self => Attr self Bool
cellBackgroundSet = newAttrFromBoolProperty "cell-background-set"


--------------------
-- Signals

#if GTK_CHECK_VERSION(2,4,0)
-- %hash c:eff4 d:fc12
-- | This signal gets emitted when the user cancels the process of editing a
-- cell. For example, an editable cell renderer could be written to cancel
-- editing when the user presses Escape.
--
-- * Available since Gtk+ version 2.4
--
editingCanceled :: CellRendererClass self => Signal self (IO ())
editingCanceled = Signal (connect_NONE__NONE "editing-canceled")

#if GTK_CHECK_VERSION(2,6,0)
-- %hash c:41f0 d:49f
-- | This signal gets emitted when a cell starts to be edited. The indended
-- use of this signal is to do special setup on @editable@, e.g. adding a
-- 'EntryCompletion' or setting up additional columns in a 'ComboBox'.
--
-- * The widget that is passed to the handler contains the widget that is used
--   by the 'CellRenderer' to interact with the user. The widget must be
--   casted to the appropriate widget. For instance, a
--   'Graphics.UI.Gtk.ModelView.CellRendererText' uses an
--   'Graphics.UI.Gtk.Entry.Entry' widget, while a
--   'Graphics.UI.Gtk.ModelView.CellRendererCombo' uses a
--   'Graphics.UI.Gtk.ModelView.ComboBox.ComboBox' (if
--   'Graphics.UI.Gtk.ModelView.CellRendererCombo.cellComboHasEntry' is
--   @False@) or a 'Graphics.UI.Gtk.ModelView.ComboBoxEntry.ComboBoxEntry' (if
--   'Graphics.UI.Gtk.ModelView.CellRendererCombo.cellComboHasEntry' is
--   @True@).
--
-- * Available since Gtk+ version 2.6
--
editingStarted :: CellRendererClass self =>
                  Signal self (Widget -> TreePath -> IO ())
editingStarted = Signal editingStartedInternal

editingStartedInternal after cr act =
 connect_OBJECT_GLIBSTRING__NONE "editing-started" after cr
 $ \ce path -> act ce (stringToTreePath path)
#endif
#endif

--------------------
-- Deprecated Signals

#ifndef DISABLE_DEPRECATED

#if GTK_CHECK_VERSION(2,4,0)
-- %hash c:b10f
onEditingCanceled :: CellRendererClass self => self
 -> IO ()
 -> IO (ConnectId self)
onEditingCanceled = connect_NONE__NONE "editing-canceled" False
{-# DEPRECATED onEditingCanceled "instead of 'onEditingCanceled obj' use 'on obj editingCanceled'" #-}

-- %hash c:808e
afterEditingCanceled :: CellRendererClass self => self
 -> IO ()
 -> IO (ConnectId self)
afterEditingCanceled = connect_NONE__NONE "editing-canceled" True
{-# DEPRECATED afterEditingCanceled "instead of 'afterEditingCanceled obj' use 'after obj editingCanceled'" #-}

#if GTK_CHECK_VERSION(2,6,0)
-- %hash c:6d9c
onEditingStarted :: CellRendererClass self => self
 -> (Widget -> TreePath -> IO ())
 -> IO (ConnectId self)
onEditingStarted = editingStartedInternal False
{-# DEPRECATED onEditingStarted "instead of 'onEditingStarted obj' use 'on obj editingStarted'" #-}

-- %hash c:ef1b
afterEditingStarted :: CellRendererClass self => self
 -> (Widget -> TreePath -> IO ())
 -> IO (ConnectId self)
afterEditingStarted = editingStartedInternal True
{-# DEPRECATED afterEditingStarted "instead of 'afterEditingStarted obj' use 'after obj editingStarted'" #-}
#endif
#endif
#endif
