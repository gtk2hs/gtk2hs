{-# OPTIONS -cpp #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK)
--
--  Author : Axel Simon
--
--  Created: 9 April 2001
--
--  Version $Revision: 1.14 $ from $Date: 2005/08/03 01:25:09 $
--
--  Copyright (C) 2001-2005 Axel Simon
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
-- Everything that is marked as deprecated, vanishing or useless for
--   applications is not bound.
--
-- The following modules are not bound:
--   DialogMessage : has only one variadic function which cannot be bound.
--		     The same functionality can be simulated with Dialog.
--   Item :	     The only child of this abstract class is MenuItem. The
--		     three signals Item defines are therefore bound in 
--		     MenuItem.
--
-- TODO
--
-- Every module that is commented out and not mentioned above.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- This module gathers all publicly available functions from the Gtk binding.
--
module Graphics.UI.Gtk (
  -- * General things, initialization
  module Graphics.UI.Gtk.General.General,
  module Graphics.UI.Gtk.General.IconFactory,
  module Graphics.UI.Gtk.General.StockItems,
  module Graphics.UI.Gtk.Gdk.Keys,
  module Graphics.UI.Gtk.General.Style,
  module Graphics.UI.Gtk.Gdk.Drawable,
  module Graphics.UI.Gtk.Gdk.DrawWindow,
  module Graphics.UI.Gtk.Gdk.Region,
  module Graphics.UI.Gtk.Gdk.GC,
  module Graphics.UI.Gtk.Gdk.Pixbuf,
  module Graphics.UI.Gtk.Gdk.Pixmap,
  module Graphics.UI.Gtk.Gdk.Gdk,
  -- * Windows
  module Graphics.UI.Gtk.Windows.Window,
  module Graphics.UI.Gtk.Windows.Dialog,
  module Graphics.UI.Gtk.Windows.AboutDialog,
  module Graphics.UI.Gtk.Windows.WindowGroup,
  -- * Display widgets,
  module Graphics.UI.Gtk.Display.AccelLabel,
  module Graphics.UI.Gtk.Display.Image,
  module Graphics.UI.Gtk.Display.Label,
  module Graphics.UI.Gtk.Display.ProgressBar,
  module Graphics.UI.Gtk.Display.Statusbar,
  -- * Buttons and toggles
  module Graphics.UI.Gtk.Buttons.Button,
  module Graphics.UI.Gtk.Buttons.CheckButton,
  module Graphics.UI.Gtk.Buttons.RadioButton,
  module Graphics.UI.Gtk.Buttons.ToggleButton,
  -- * Numeric\/text data entry
  module Graphics.UI.Gtk.Entry.Editable,
  module Graphics.UI.Gtk.Entry.Entry,
  module Graphics.UI.Gtk.Entry.EntryCompletion,
  module Graphics.UI.Gtk.Entry.HScale,
  module Graphics.UI.Gtk.Entry.VScale,
  module Graphics.UI.Gtk.Entry.SpinButton,
  -- * Multiline text editor
  module Graphics.UI.Gtk.Multiline.TextIter,
  module Graphics.UI.Gtk.Multiline.TextMark,
  module Graphics.UI.Gtk.Multiline.TextBuffer,
  module Graphics.UI.Gtk.Multiline.TextTag,
  module Graphics.UI.Gtk.Multiline.TextTagTable,
  module Graphics.UI.Gtk.Multiline.TextView,
  -- * Tree and list widget
  module Graphics.UI.Gtk.TreeList.TreeModel,
  module Graphics.UI.Gtk.TreeList.TreeSelection,
  module Graphics.UI.Gtk.TreeList.TreeViewColumn,
  module Graphics.UI.Gtk.TreeList.TreeView,
  module Graphics.UI.Gtk.TreeList.IconView,
  module Graphics.UI.Gtk.TreeList.CellView,
--  module TreeSortable,
  module Graphics.UI.Gtk.TreeList.TreeModelSort,
  module Graphics.UI.Gtk.TreeList.CellRenderer,
--  module CellEditable,
  module Graphics.UI.Gtk.TreeList.CellRendererPixbuf,
  module Graphics.UI.Gtk.TreeList.CellRendererText,
  module Graphics.UI.Gtk.TreeList.CellRendererToggle,
  module Graphics.UI.Gtk.TreeList.ListStore,
  module Graphics.UI.Gtk.TreeList.TreeStore,
  -- * Menus, combo box, toolbar
  module Graphics.UI.Gtk.MenuComboToolbar.CheckMenuItem,
  module Graphics.UI.Gtk.MenuComboToolbar.Combo,
  module Graphics.UI.Gtk.MenuComboToolbar.ComboBox,
  module Graphics.UI.Gtk.MenuComboToolbar.ComboBoxEntry,
  module Graphics.UI.Gtk.MenuComboToolbar.Menu,
  module Graphics.UI.Gtk.MenuComboToolbar.MenuBar,
  module Graphics.UI.Gtk.MenuComboToolbar.MenuItem,
  module Graphics.UI.Gtk.MenuComboToolbar.MenuShell,
  module Graphics.UI.Gtk.MenuComboToolbar.OptionMenu,
  module Graphics.UI.Gtk.MenuComboToolbar.ImageMenuItem,
  module Graphics.UI.Gtk.MenuComboToolbar.RadioMenuItem,
  module Graphics.UI.Gtk.MenuComboToolbar.TearoffMenuItem,
  module Graphics.UI.Gtk.MenuComboToolbar.Toolbar,
  module Graphics.UI.Gtk.MenuComboToolbar.ToolItem,
  module Graphics.UI.Gtk.MenuComboToolbar.ToolButton,
  module Graphics.UI.Gtk.MenuComboToolbar.MenuToolButton,
  module Graphics.UI.Gtk.MenuComboToolbar.ToggleToolButton,
  module Graphics.UI.Gtk.MenuComboToolbar.RadioToolButton,
  module Graphics.UI.Gtk.MenuComboToolbar.SeparatorMenuItem,
  module Graphics.UI.Gtk.MenuComboToolbar.SeparatorToolItem,
-- * Action-based menus and toolbars
  module Graphics.UI.Gtk.ActionMenuToolbar.Action,
  module Graphics.UI.Gtk.ActionMenuToolbar.ActionGroup,
  module Graphics.UI.Gtk.ActionMenuToolbar.ToggleAction,
  module Graphics.UI.Gtk.ActionMenuToolbar.RadioAction,
  module Graphics.UI.Gtk.ActionMenuToolbar.UIManager,
  -- * Selectors (file\/font\/color)
  module Graphics.UI.Gtk.Selectors.ColorSelection,
  module Graphics.UI.Gtk.Selectors.ColorSelectionDialog,
  module Graphics.UI.Gtk.Selectors.ColorButton,
  module Graphics.UI.Gtk.Selectors.FileSelection,
  module Graphics.UI.Gtk.Selectors.FontSelection,
  module Graphics.UI.Gtk.Selectors.FontSelectionDialog,
  module Graphics.UI.Gtk.Selectors.FontButton,
--  module InputDialog,
  -- ** File chooser
  module Graphics.UI.Gtk.Selectors.FileChooser,
  module Graphics.UI.Gtk.Selectors.FileChooserDialog,
  module Graphics.UI.Gtk.Selectors.FileChooserWidget,
  module Graphics.UI.Gtk.Selectors.FileChooserButton,
  module Graphics.UI.Gtk.Selectors.FileFilter,
  -- * Layout containers
  module Graphics.UI.Gtk.Layout.Alignment,
  module Graphics.UI.Gtk.Layout.AspectFrame,
  module Graphics.UI.Gtk.Layout.HBox,
  module Graphics.UI.Gtk.Layout.HButtonBox,
  module Graphics.UI.Gtk.Layout.Fixed,
  module Graphics.UI.Gtk.Layout.HPaned,
  module Graphics.UI.Gtk.Layout.Layout,
  module Graphics.UI.Gtk.Layout.Notebook,
  module Graphics.UI.Gtk.Layout.Expander,
  module Graphics.UI.Gtk.Layout.Table,
  module Graphics.UI.Gtk.Layout.VBox,
  module Graphics.UI.Gtk.Layout.VButtonBox,
  module Graphics.UI.Gtk.Layout.VPaned,
  -- * Ornaments
  module Graphics.UI.Gtk.Ornaments.Frame,
  module Graphics.UI.Gtk.Ornaments.HSeparator,
  module Graphics.UI.Gtk.Ornaments.VSeparator,
  -- * Scrolling
  module Graphics.UI.Gtk.Scrolling.HScrollbar,
  module Graphics.UI.Gtk.Scrolling.ScrolledWindow,
  module Graphics.UI.Gtk.Scrolling.VScrollbar,
  -- * Miscellaneous
  module Graphics.UI.Gtk.Misc.Adjustment,
  module Graphics.UI.Gtk.Misc.Arrow,
  module Graphics.UI.Gtk.Misc.Calendar,
  module Graphics.UI.Gtk.Misc.DrawingArea,
  module Graphics.UI.Gtk.Misc.EventBox,
  module Graphics.UI.Gtk.Misc.HandleBox,
--  module IMContext,
--  module IMMulticontext,
  module Graphics.UI.Gtk.Misc.SizeGroup,
  module Graphics.UI.Gtk.Misc.Tooltips,
  module Graphics.UI.Gtk.Misc.Viewport,
  -- * Abstract base classes
  module Graphics.UI.Gtk.Abstract.Box,
  module Graphics.UI.Gtk.Abstract.ButtonBox,
  module Graphics.UI.Gtk.Abstract.Container,
  module Graphics.UI.Gtk.Abstract.Bin,
  module Graphics.UI.Gtk.Abstract.Misc,
  module Graphics.UI.Gtk.Abstract.Object,
  module Graphics.UI.Gtk.Abstract.Paned,
  module Graphics.UI.Gtk.Abstract.Range,
  module Graphics.UI.Gtk.Abstract.Scale,
  module Graphics.UI.Gtk.Abstract.Scrollbar,
  module Graphics.UI.Gtk.Abstract.Separator,
  module Graphics.UI.Gtk.Abstract.Widget,
#ifndef WIN32
  -- * Cross-process embedding
  module Graphics.UI.Gtk.Embedding.Plug,
  module Graphics.UI.Gtk.Embedding.Socket,
#endif
  -- * Non-widgets
  module Graphics.UI.Gtk.Types,
  module Graphics.UI.Gtk.Signals,
  module System.Glib.Attributes,

  -- * Pango text layout modules
  module Graphics.UI.Gtk.Pango.Markup,
  module Graphics.UI.Gtk.Pango.Layout,
  module Graphics.UI.Gtk.Pango.Rendering,
  module Graphics.UI.Gtk.Pango.Description,
  module Graphics.UI.Gtk.Pango.Enums,
  ) where

-- general things, initialization
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.General.IconFactory
import Graphics.UI.Gtk.General.StockItems
import Graphics.UI.Gtk.Gdk.Keys
import Graphics.UI.Gtk.General.Style
import Graphics.UI.Gtk.Gdk.Drawable
import Graphics.UI.Gtk.Gdk.DrawWindow
import Graphics.UI.Gtk.Gdk.Region		hiding (makeNewRegion)
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.UI.Gtk.Gdk.Pixmap
import Graphics.UI.Gtk.Gdk.Gdk
-- windows
import Graphics.UI.Gtk.Windows.Dialog
import Graphics.UI.Gtk.Windows.Window
import Graphics.UI.Gtk.Windows.AboutDialog
import Graphics.UI.Gtk.Windows.WindowGroup
-- display widgets
import Graphics.UI.Gtk.Display.AccelLabel
import Graphics.UI.Gtk.Display.Image
import Graphics.UI.Gtk.Display.Label
import Graphics.UI.Gtk.Display.ProgressBar
import Graphics.UI.Gtk.Display.Statusbar
-- buttons and toggles
import Graphics.UI.Gtk.Buttons.Button
import Graphics.UI.Gtk.Buttons.CheckButton
import Graphics.UI.Gtk.Buttons.RadioButton
import Graphics.UI.Gtk.Buttons.ToggleButton
-- numeric\/text data entry
import Graphics.UI.Gtk.Entry.Editable
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Entry.EntryCompletion
import Graphics.UI.Gtk.Entry.HScale
import Graphics.UI.Gtk.Entry.VScale
import Graphics.UI.Gtk.Entry.SpinButton
-- multiline text editor
import Graphics.UI.Gtk.Multiline.TextIter
import Graphics.UI.Gtk.Multiline.TextMark
import Graphics.UI.Gtk.Multiline.TextBuffer
import Graphics.UI.Gtk.Multiline.TextTag
import Graphics.UI.Gtk.Multiline.TextTagTable
import qualified Graphics.UI.Gtk.Multiline.TextView
import Graphics.UI.Gtk.Multiline.TextView hiding (afterSetScrollAdjustments,
		onSetScrollAdjustments, afterCopyClipboard, onCopyClipboard,
		afterCutClipboard, onCutClipboard, afterInsertAtCursor,
		onInsertAtCursor, afterPasteClipboard, onPasteClipboard,
		afterToggleOverwrite, onToggleOverwrite)
-- tree and list widget
import Graphics.UI.Gtk.TreeList.TreeModel hiding (
		gtk_tree_model_get_iter_from_string)
import Graphics.UI.Gtk.TreeList.TreeSelection
import Graphics.UI.Gtk.TreeList.TreeViewColumn
import Graphics.UI.Gtk.TreeList.TreeView
import Graphics.UI.Gtk.TreeList.IconView
import Graphics.UI.Gtk.TreeList.CellView
--import TreeSortable
import Graphics.UI.Gtk.TreeList.TreeModelSort
import Graphics.UI.Gtk.TreeList.CellRenderer
--import CellEditable
import Graphics.UI.Gtk.TreeList.CellRendererPixbuf
import Graphics.UI.Gtk.TreeList.CellRendererText
import Graphics.UI.Gtk.TreeList.CellRendererToggle
import Graphics.UI.Gtk.TreeList.ListStore
import Graphics.UI.Gtk.TreeList.TreeStore
-- menus, combo box, toolbar
import Graphics.UI.Gtk.MenuComboToolbar.Combo
import Graphics.UI.Gtk.MenuComboToolbar.ComboBox
import Graphics.UI.Gtk.MenuComboToolbar.ComboBoxEntry
-- import ItemFactory
import Graphics.UI.Gtk.MenuComboToolbar.Menu
import Graphics.UI.Gtk.MenuComboToolbar.MenuBar
import Graphics.UI.Gtk.MenuComboToolbar.MenuItem
import Graphics.UI.Gtk.MenuComboToolbar.MenuShell
import Graphics.UI.Gtk.MenuComboToolbar.OptionMenu
import Graphics.UI.Gtk.MenuComboToolbar.ImageMenuItem
import Graphics.UI.Gtk.MenuComboToolbar.RadioMenuItem
import Graphics.UI.Gtk.MenuComboToolbar.CheckMenuItem
import Graphics.UI.Gtk.MenuComboToolbar.TearoffMenuItem
import Graphics.UI.Gtk.MenuComboToolbar.Toolbar
import Graphics.UI.Gtk.MenuComboToolbar.ToolItem
import Graphics.UI.Gtk.MenuComboToolbar.ToolButton
import Graphics.UI.Gtk.MenuComboToolbar.MenuToolButton
import Graphics.UI.Gtk.MenuComboToolbar.ToggleToolButton
import Graphics.UI.Gtk.MenuComboToolbar.RadioToolButton
import Graphics.UI.Gtk.MenuComboToolbar.SeparatorMenuItem
import Graphics.UI.Gtk.MenuComboToolbar.SeparatorToolItem
-- action based menus and toolbars
import Graphics.UI.Gtk.ActionMenuToolbar.Action
import Graphics.UI.Gtk.ActionMenuToolbar.ActionGroup
import Graphics.UI.Gtk.ActionMenuToolbar.ToggleAction
import Graphics.UI.Gtk.ActionMenuToolbar.RadioAction
import Graphics.UI.Gtk.ActionMenuToolbar.UIManager
-- selectors (file\/font\/color\/input device)
import Graphics.UI.Gtk.Selectors.ColorSelection
import Graphics.UI.Gtk.Selectors.ColorSelectionDialog
import Graphics.UI.Gtk.Selectors.ColorButton
import Graphics.UI.Gtk.Selectors.FileSelection
import Graphics.UI.Gtk.Selectors.FileChooser
import Graphics.UI.Gtk.Selectors.FileChooserDialog
import Graphics.UI.Gtk.Selectors.FileChooserWidget
import Graphics.UI.Gtk.Selectors.FileChooserButton
import Graphics.UI.Gtk.Selectors.FileFilter
import Graphics.UI.Gtk.Selectors.FontSelection
import Graphics.UI.Gtk.Selectors.FontSelectionDialog
import Graphics.UI.Gtk.Selectors.FontButton
--import InputDialog
-- layout containers
import Graphics.UI.Gtk.Layout.Alignment
import Graphics.UI.Gtk.Layout.AspectFrame
import Graphics.UI.Gtk.Layout.HBox
import Graphics.UI.Gtk.Layout.VBox
import Graphics.UI.Gtk.Layout.HButtonBox
import Graphics.UI.Gtk.Layout.VButtonBox
import Graphics.UI.Gtk.Layout.Fixed
import Graphics.UI.Gtk.Layout.HPaned
import Graphics.UI.Gtk.Layout.VPaned
import Graphics.UI.Gtk.Layout.Layout
import Graphics.UI.Gtk.Layout.Notebook
import Graphics.UI.Gtk.Layout.Expander
import Graphics.UI.Gtk.Layout.Table
-- ornaments
import Graphics.UI.Gtk.Ornaments.Frame
import Graphics.UI.Gtk.Ornaments.HSeparator
import Graphics.UI.Gtk.Ornaments.VSeparator
-- scrolling
import Graphics.UI.Gtk.Scrolling.HScrollbar
import Graphics.UI.Gtk.Scrolling.VScrollbar
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
-- miscellaneous
import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Misc.Arrow
import Graphics.UI.Gtk.Misc.Calendar
import Graphics.UI.Gtk.Misc.DrawingArea
import Graphics.UI.Gtk.Misc.EventBox
import Graphics.UI.Gtk.Misc.HandleBox
--import IMContext
--import IMContextSimple
--import IMMulitcontext
import Graphics.UI.Gtk.Misc.SizeGroup
import Graphics.UI.Gtk.Misc.Tooltips
import Graphics.UI.Gtk.Misc.Viewport
--import Accessible
-- abstract base classes
import Graphics.UI.Gtk.Abstract.Box
import Graphics.UI.Gtk.Abstract.ButtonBox
import Graphics.UI.Gtk.Abstract.Container
import Graphics.UI.Gtk.Abstract.Bin
import Graphics.UI.Gtk.Abstract.Misc
import Graphics.UI.Gtk.Abstract.Object
import Graphics.UI.Gtk.Abstract.Paned
import Graphics.UI.Gtk.Abstract.Range
import Graphics.UI.Gtk.Abstract.Scale
import Graphics.UI.Gtk.Abstract.Scrollbar
import Graphics.UI.Gtk.Abstract.Separator
import Graphics.UI.Gtk.Abstract.Widget
#ifndef WIN32
-- cross-process embedding
import Graphics.UI.Gtk.Embedding.Plug
import Graphics.UI.Gtk.Embedding.Socket
#endif

-- non widgets
import Graphics.UI.Gtk.Types		(toCellRenderer)
import Graphics.UI.Gtk.Signals	(ConnectId, disconnect)
import System.Glib.Attributes

-- pango modules
import Graphics.UI.Gtk.Pango.Markup
import Graphics.UI.Gtk.Pango.Layout
import Graphics.UI.Gtk.Pango.Rendering
import Graphics.UI.Gtk.Pango.Description
import Graphics.UI.Gtk.Pango.Enums

