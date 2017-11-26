{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK)
--
--  Author : Axel Simon
--
--  Created: 9 April 2001
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
--                   The same functionality can be simulated with Dialog.
--   Item :          The only child of this abstract class is MenuItem. The
--                   three signals Item defines are therefore bound in
--                   MenuItem.
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
  module Graphics.UI.Gtk.General.IconTheme,
  module Graphics.UI.Gtk.General.StockItems,
  module Graphics.UI.Gtk.General.Selection,
  module Graphics.UI.Gtk.General.Settings,
  module Graphics.UI.Gtk.General.Drag,
  module Graphics.UI.Gtk.Gdk.Keys,
  module Graphics.UI.Gtk.General.Style,
  module Graphics.UI.Gtk.General.RcStyle,
  module Graphics.UI.Gtk.General.Clipboard,

  -- * Drawing and other Low-Level Operations
  module Graphics.UI.Gtk.Gdk.AppLaunchContext,
  module Graphics.UI.Gtk.Gdk.Cursor,
#if GTK_MAJOR_VERSION < 3
  module Graphics.UI.Gtk.Gdk.Drawable,
#endif
  module Graphics.UI.Gtk.Gdk.DrawWindow,
#if GTK_MAJOR_VERSION < 3
  module Graphics.UI.Gtk.Gdk.Region,
#endif
--  module Graphics.UI.Gtk.Gdk.GC,
  module Graphics.UI.Gtk.Gdk.EventM,
#if GTK_CHECK_VERSION(3,16,0)
  module Graphics.UI.Gtk.Gdk.GLContext,
#endif
  module Graphics.UI.Gtk.Gdk.Pixbuf,
#if GTK_MAJOR_VERSION < 3
  module Graphics.UI.Gtk.Gdk.Pixmap,
#endif
  module Graphics.UI.Gtk.Gdk.Screen,
  module Graphics.UI.Gtk.Gdk.Keymap,
  module Graphics.UI.Gtk.Gdk.Display,
  module Graphics.UI.Gtk.Gdk.DisplayManager,
  module Graphics.UI.Gtk.Gdk.Gdk,
  -- ** cairo integration
  module Graphics.UI.Gtk.Cairo,
  -- * Windows
  module Graphics.UI.Gtk.Windows.Window,
  module Graphics.UI.Gtk.Windows.OffscreenWindow,
  module Graphics.UI.Gtk.Windows.Invisible,
  module Graphics.UI.Gtk.Windows.Dialog,
  module Graphics.UI.Gtk.Windows.AboutDialog,
  module Graphics.UI.Gtk.Windows.Assistant,
  module Graphics.UI.Gtk.Windows.MessageDialog,
  module Graphics.UI.Gtk.Windows.WindowGroup,
  -- * Display widgets,
  module Graphics.UI.Gtk.Display.AccelLabel,
  module Graphics.UI.Gtk.Display.Image,
  module Graphics.UI.Gtk.Display.Label,
#if GTK_CHECK_VERSION(3,6,0)
  module Graphics.UI.Gtk.Display.LevelBar,
#endif
  module Graphics.UI.Gtk.Display.ProgressBar,
  module Graphics.UI.Gtk.Display.Spinner,
  module Graphics.UI.Gtk.Display.Statusbar,
  module Graphics.UI.Gtk.Display.StatusIcon,
#if GTK_CHECK_VERSION(2,18,0)
  module Graphics.UI.Gtk.Display.InfoBar,
#endif
  -- * Buttons and toggles
  module Graphics.UI.Gtk.Buttons.Button,
  module Graphics.UI.Gtk.Buttons.CheckButton,
  module Graphics.UI.Gtk.Buttons.RadioButton,
  module Graphics.UI.Gtk.Buttons.ToggleButton,
  module Graphics.UI.Gtk.Buttons.LinkButton,
  module Graphics.UI.Gtk.Buttons.ScaleButton,
  module Graphics.UI.Gtk.Buttons.VolumeButton,
  -- * Numeric\/text data entry
  module Graphics.UI.Gtk.Entry.Editable,
  module Graphics.UI.Gtk.Entry.Entry,
  module Graphics.UI.Gtk.Entry.EntryBuffer,
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
  module Graphics.UI.Gtk.ModelView.CellEditable,
  module Graphics.UI.Gtk.ModelView.CellLayout,
  module Graphics.UI.Gtk.ModelView.CellRenderer,
  module Graphics.UI.Gtk.ModelView.CellRendererSpinner,
  module Graphics.UI.Gtk.ModelView.CellRendererCombo,
  module Graphics.UI.Gtk.ModelView.CellRendererPixbuf,
  module Graphics.UI.Gtk.ModelView.CellRendererProgress,
  module Graphics.UI.Gtk.ModelView.CellRendererText,
  module Graphics.UI.Gtk.ModelView.CellRendererAccel,
  module Graphics.UI.Gtk.ModelView.CellRendererSpin,
  module Graphics.UI.Gtk.ModelView.CellRendererToggle,
  module Graphics.UI.Gtk.ModelView.CellView,
  module Graphics.UI.Gtk.ModelView.CustomStore,
  module Graphics.UI.Gtk.ModelView.IconView,
  module Graphics.UI.Gtk.ModelView.ListStore,
  module Graphics.UI.Gtk.ModelView.TreeDrag,
  module Graphics.UI.Gtk.ModelView.TreeModel,
  module Graphics.UI.Gtk.ModelView.TreeModelSort,
  module Graphics.UI.Gtk.ModelView.TreeSortable,
  module Graphics.UI.Gtk.ModelView.TreeModelFilter,
  module Graphics.UI.Gtk.ModelView.TreeRowReference,
  module Graphics.UI.Gtk.ModelView.TreeSelection,
  module Graphics.UI.Gtk.ModelView.TreeStore,
  module Graphics.UI.Gtk.ModelView.TreeView,
  module Graphics.UI.Gtk.ModelView.TreeViewColumn,
  -- * Menus, combo box, toolbar
  module Graphics.UI.Gtk.MenuComboToolbar.CheckMenuItem,
#if GTK_MAJOR_VERSION < 3
  module Graphics.UI.Gtk.MenuComboToolbar.Combo,
#endif
  module Graphics.UI.Gtk.MenuComboToolbar.ComboBox,
#if GTK_MAJOR_VERSION < 3
  module Graphics.UI.Gtk.MenuComboToolbar.ComboBoxEntry,
#endif
  module Graphics.UI.Gtk.MenuComboToolbar.Menu,
  module Graphics.UI.Gtk.MenuComboToolbar.MenuBar,
  module Graphics.UI.Gtk.MenuComboToolbar.MenuItem,
  module Graphics.UI.Gtk.MenuComboToolbar.MenuShell,
#if GTK_MAJOR_VERSION < 3
  module Graphics.UI.Gtk.MenuComboToolbar.OptionMenu,
#endif
  module Graphics.UI.Gtk.MenuComboToolbar.ImageMenuItem,
  module Graphics.UI.Gtk.MenuComboToolbar.RadioMenuItem,
  module Graphics.UI.Gtk.MenuComboToolbar.TearoffMenuItem,
  module Graphics.UI.Gtk.MenuComboToolbar.Toolbar,
  module Graphics.UI.Gtk.MenuComboToolbar.ToolItem,
  module Graphics.UI.Gtk.MenuComboToolbar.ToolItemGroup,
  module Graphics.UI.Gtk.MenuComboToolbar.ToolPalette,
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
  module Graphics.UI.Gtk.ActionMenuToolbar.RecentAction,
  module Graphics.UI.Gtk.ActionMenuToolbar.UIManager,
  -- * Selectors (file\/font\/color)
  module Graphics.UI.Gtk.Selectors.ColorSelection,
  module Graphics.UI.Gtk.Selectors.ColorSelectionDialog,
  module Graphics.UI.Gtk.Selectors.ColorButton,
#if GTK_MAJOR_VERSION < 3
  module Graphics.UI.Gtk.Selectors.FileSelection,
#endif
  module Graphics.UI.Gtk.Selectors.FontSelection,
  module Graphics.UI.Gtk.Selectors.FontSelectionDialog,
  module Graphics.UI.Gtk.Selectors.FontButton,
#if GTK_CHECK_VERSION(2,14,0)
  module Graphics.UI.Gtk.Selectors.HSV,
#endif
#if GTK_MAJOR_VERSION < 3
  -- * Special-purpose features
  module Graphics.UI.Gtk.Special.Ruler,
  module Graphics.UI.Gtk.Special.HRuler,
  module Graphics.UI.Gtk.Special.VRuler,
#endif
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
#if GTK_MAJOR_VERSION >= 3
  module Graphics.UI.Gtk.Layout.Grid,
  module Graphics.UI.Gtk.Layout.Overlay,
#endif
  module Graphics.UI.Gtk.Layout.Expander,
  module Graphics.UI.Gtk.Layout.Table,
  module Graphics.UI.Gtk.Layout.VBox,
  module Graphics.UI.Gtk.Layout.VButtonBox,
  module Graphics.UI.Gtk.Layout.VPaned,
#if GTK_CHECK_VERSION(3,10,0)
  module Graphics.UI.Gtk.Layout.Stack,
  module Graphics.UI.Gtk.Layout.StackSwitcher,
#endif
  -- * Ornaments
  module Graphics.UI.Gtk.Ornaments.Frame,
  module Graphics.UI.Gtk.Ornaments.HSeparator,
  module Graphics.UI.Gtk.Ornaments.VSeparator,
  -- * Printing
  module Graphics.UI.Gtk.Printing.PaperSize,
  module Graphics.UI.Gtk.Printing.PageSetup,
  module Graphics.UI.Gtk.Printing.PrintContext,
  module Graphics.UI.Gtk.Printing.PrintOperation,
  module Graphics.UI.Gtk.Printing.PrintSettings,
  -- * Recent
  module Graphics.UI.Gtk.Recent.RecentChooserMenu,
  module Graphics.UI.Gtk.Recent.RecentChooserWidget,
  module Graphics.UI.Gtk.Recent.RecentFilter,
  module Graphics.UI.Gtk.Recent.RecentManager,
  module Graphics.UI.Gtk.Recent.RecentInfo,
  module Graphics.UI.Gtk.Recent.RecentChooser,
  -- * Scrolling
  module Graphics.UI.Gtk.Scrolling.HScrollbar,
  module Graphics.UI.Gtk.Scrolling.ScrolledWindow,
  module Graphics.UI.Gtk.Scrolling.VScrollbar,
  -- * Miscellaneous
  module Graphics.UI.Gtk.Misc.Accessible,
  module Graphics.UI.Gtk.Misc.Adjustment,
  module Graphics.UI.Gtk.Misc.Arrow,
  module Graphics.UI.Gtk.Misc.Calendar,
  module Graphics.UI.Gtk.Misc.DrawingArea,
#if GTK_CHECK_VERSION(3,16,0)
  module Graphics.UI.Gtk.Misc.GLArea,
#endif
  module Graphics.UI.Gtk.Misc.EventBox,
  module Graphics.UI.Gtk.Misc.HandleBox,
  module Graphics.UI.Gtk.Misc.IMMulticontext,
  module Graphics.UI.Gtk.Misc.IMContextSimple,
  module Graphics.UI.Gtk.Misc.SizeGroup,
  module Graphics.UI.Gtk.Misc.Tooltip,
#if GTK_MAJOR_VERSION < 3
  module Graphics.UI.Gtk.Misc.Tooltips,
#endif
  module Graphics.UI.Gtk.Misc.Viewport,
#if GTK_MAJOR_VERSION >= 3
  module Graphics.UI.Gtk.Misc.Switch,
#endif
  -- * Abstract base classes
  module Graphics.UI.Gtk.Abstract.Box,
  module Graphics.UI.Gtk.Abstract.ButtonBox,
  module Graphics.UI.Gtk.Abstract.Container,
  module Graphics.UI.Gtk.Abstract.Bin,
  module Graphics.UI.Gtk.Abstract.Misc,
  module Graphics.UI.Gtk.Abstract.IMContext,
  module Graphics.UI.Gtk.Abstract.Object,
  module Graphics.UI.Gtk.Abstract.Paned,
  module Graphics.UI.Gtk.Abstract.Range,
  module Graphics.UI.Gtk.Abstract.Scale,
  module Graphics.UI.Gtk.Abstract.Scrollbar,
  module Graphics.UI.Gtk.Abstract.Separator,
  module Graphics.UI.Gtk.Abstract.Widget,
#if (defined(HAVE_PLUG_AND_SOCKET) && (!defined(WIN32) || GTK_CHECK_VERSION(2,8,0))) || defined(GDK_WINDOWING_X11)
  -- * Cross-process embedding
  module Graphics.UI.Gtk.Embedding.Plug,
  module Graphics.UI.Gtk.Embedding.Socket,
#endif
  -- * Non-widgets
  module System.Glib.Signals,
  module System.Glib.Attributes,
  module System.Glib.GObject,
  module Graphics.UI.Gtk.Builder,

  -- * Pango text layout modules
  module Graphics.Rendering.Pango.Context,
  module Graphics.Rendering.Pango.Markup,
  module Graphics.Rendering.Pango.Layout,
  module Graphics.Rendering.Pango.Rendering,
  module Graphics.Rendering.Pango.Font,
  module Graphics.Rendering.Pango.Enums
  ) where

-- general things, initialization
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.General.IconFactory
import Graphics.UI.Gtk.General.IconTheme
import Graphics.UI.Gtk.General.StockItems
import Graphics.UI.Gtk.General.Selection
import Graphics.UI.Gtk.General.Settings
import Graphics.UI.Gtk.General.Drag
import Graphics.UI.Gtk.General.Clipboard
-- drawing
import Graphics.UI.Gtk.Gdk.Keys
import Graphics.UI.Gtk.General.Style
import Graphics.UI.Gtk.General.RcStyle
import Graphics.UI.Gtk.Gdk.AppLaunchContext
import Graphics.UI.Gtk.Gdk.Cursor
#if GTK_MAJOR_VERSION < 3
import Graphics.UI.Gtk.Gdk.Drawable
#endif
import Graphics.UI.Gtk.Gdk.DrawWindow
#if GTK_MAJOR_VERSION < 3
import Graphics.UI.Gtk.Gdk.Region               hiding (makeNewRegion)
#endif
--import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.EventM
#if GTK_CHECK_VERSION(3,16,0)
import Graphics.UI.Gtk.Gdk.GLContext
#endif
import Graphics.UI.Gtk.Gdk.Pixbuf
#if GTK_MAJOR_VERSION < 3
import Graphics.UI.Gtk.Gdk.Pixmap
#endif
import Graphics.UI.Gtk.Gdk.Screen
import Graphics.UI.Gtk.Gdk.Keymap
import Graphics.UI.Gtk.Gdk.Display
import Graphics.UI.Gtk.Gdk.DisplayManager
import Graphics.UI.Gtk.Gdk.Gdk
-- cairo integration
import Graphics.UI.Gtk.Cairo
-- windows
import Graphics.UI.Gtk.Windows.Dialog
import Graphics.UI.Gtk.Windows.Window
import Graphics.UI.Gtk.Windows.OffscreenWindow
import Graphics.UI.Gtk.Windows.Invisible
import Graphics.UI.Gtk.Windows.AboutDialog
import Graphics.UI.Gtk.Windows.Assistant
import Graphics.UI.Gtk.Windows.MessageDialog
import Graphics.UI.Gtk.Windows.WindowGroup
-- display widgets
import Graphics.UI.Gtk.Display.AccelLabel
import Graphics.UI.Gtk.Display.Image
import Graphics.UI.Gtk.Display.Label
#if GTK_CHECK_VERSION(3,6,0)
import Graphics.UI.Gtk.Display.LevelBar
#endif
import Graphics.UI.Gtk.Display.ProgressBar
import Graphics.UI.Gtk.Display.Spinner
import Graphics.UI.Gtk.Display.Statusbar
#if GTK_CHECK_VERSION(2,10,0) && !DISABLE_DEPRECATED
import Graphics.UI.Gtk.Display.StatusIcon hiding (onActivate,afterActivate,onPopupMenu,afterPopupMenu)
#else
import Graphics.UI.Gtk.Display.StatusIcon
#endif
#if GTK_CHECK_VERSION(2,18,0)
import Graphics.UI.Gtk.Display.InfoBar
#endif
-- buttons and toggles
import Graphics.UI.Gtk.Buttons.Button
import Graphics.UI.Gtk.Buttons.CheckButton
import Graphics.UI.Gtk.Buttons.RadioButton
import Graphics.UI.Gtk.Buttons.ToggleButton
import Graphics.UI.Gtk.Buttons.LinkButton
import Graphics.UI.Gtk.Buttons.ScaleButton
import Graphics.UI.Gtk.Buttons.VolumeButton
-- numeric\/text data entry
import Graphics.UI.Gtk.Entry.Editable
import Graphics.UI.Gtk.Entry.Entry
import Graphics.UI.Gtk.Entry.EntryBuffer
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
import Graphics.UI.Gtk.Multiline.TextView
-- tree and list widget
import Graphics.UI.Gtk.ModelView.CellEditable
import Graphics.UI.Gtk.ModelView.CellLayout
import Graphics.UI.Gtk.ModelView.CellRenderer
import Graphics.UI.Gtk.ModelView.CellRendererSpinner
import Graphics.UI.Gtk.ModelView.CellRendererCombo
import Graphics.UI.Gtk.ModelView.CellRendererPixbuf
import Graphics.UI.Gtk.ModelView.CellRendererProgress
import Graphics.UI.Gtk.ModelView.CellRendererText
import Graphics.UI.Gtk.ModelView.CellRendererAccel
import Graphics.UI.Gtk.ModelView.CellRendererSpin
import Graphics.UI.Gtk.ModelView.CellRendererToggle
import Graphics.UI.Gtk.ModelView.CellView
import Graphics.UI.Gtk.ModelView.CustomStore
import Graphics.UI.Gtk.ModelView.IconView
import Graphics.UI.Gtk.ModelView.ListStore
import Graphics.UI.Gtk.ModelView.TreeDrag
import Graphics.UI.Gtk.ModelView.TreeModel
import Graphics.UI.Gtk.ModelView.TreeModelSort
import Graphics.UI.Gtk.ModelView.TreeSortable
import Graphics.UI.Gtk.ModelView.TreeModelFilter
import Graphics.UI.Gtk.ModelView.TreeRowReference
import Graphics.UI.Gtk.ModelView.TreeSelection
import Graphics.UI.Gtk.ModelView.TreeStore
import Graphics.UI.Gtk.ModelView.TreeView
import Graphics.UI.Gtk.ModelView.TreeViewColumn
-- menus, combo box, toolbar
#if GTK_MAJOR_VERSION < 3
import Graphics.UI.Gtk.MenuComboToolbar.Combo
#endif
import Graphics.UI.Gtk.MenuComboToolbar.ComboBox
#if GTK_MAJOR_VERSION < 3
import Graphics.UI.Gtk.MenuComboToolbar.ComboBoxEntry
#endif
-- import ItemFactory
import Graphics.UI.Gtk.MenuComboToolbar.Menu
import Graphics.UI.Gtk.MenuComboToolbar.MenuBar
import Graphics.UI.Gtk.MenuComboToolbar.MenuItem
import Graphics.UI.Gtk.MenuComboToolbar.MenuShell
#if GTK_MAJOR_VERSION < 3
import Graphics.UI.Gtk.MenuComboToolbar.OptionMenu
#endif
import Graphics.UI.Gtk.MenuComboToolbar.ImageMenuItem
import Graphics.UI.Gtk.MenuComboToolbar.RadioMenuItem
import Graphics.UI.Gtk.MenuComboToolbar.CheckMenuItem
import Graphics.UI.Gtk.MenuComboToolbar.TearoffMenuItem
import Graphics.UI.Gtk.MenuComboToolbar.Toolbar
import Graphics.UI.Gtk.MenuComboToolbar.ToolItem
import Graphics.UI.Gtk.MenuComboToolbar.ToolItemGroup
import Graphics.UI.Gtk.MenuComboToolbar.ToolPalette
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
import Graphics.UI.Gtk.ActionMenuToolbar.RecentAction
import Graphics.UI.Gtk.ActionMenuToolbar.UIManager
-- selectors (file\/font\/color\/input device)
import Graphics.UI.Gtk.Selectors.ColorSelection
import Graphics.UI.Gtk.Selectors.ColorSelectionDialog
import Graphics.UI.Gtk.Selectors.ColorButton
#if GTK_MAJOR_VERSION < 3
import Graphics.UI.Gtk.Selectors.FileSelection
#endif
import Graphics.UI.Gtk.Selectors.FileChooser
import Graphics.UI.Gtk.Selectors.FileChooserDialog
import Graphics.UI.Gtk.Selectors.FileChooserWidget
import Graphics.UI.Gtk.Selectors.FileChooserButton
import Graphics.UI.Gtk.Selectors.FileFilter
import Graphics.UI.Gtk.Selectors.FontSelection
import Graphics.UI.Gtk.Selectors.FontSelectionDialog
import Graphics.UI.Gtk.Selectors.FontButton
#if GTK_CHECK_VERSION(2,14,0)
import Graphics.UI.Gtk.Selectors.HSV
#endif
#if GTK_MAJOR_VERSION < 3
-- Special-purpose features
import Graphics.UI.Gtk.Special.Ruler
import Graphics.UI.Gtk.Special.HRuler
import Graphics.UI.Gtk.Special.VRuler
#endif
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
#if GTK_MAJOR_VERSION >= 3
import Graphics.UI.Gtk.Layout.Grid
import Graphics.UI.Gtk.Layout.Overlay
#endif
#if GTK_CHECK_VERSION(3,10,0)
import Graphics.UI.Gtk.Layout.Stack
import Graphics.UI.Gtk.Layout.StackSwitcher
#endif
import Graphics.UI.Gtk.Layout.Expander
import Graphics.UI.Gtk.Layout.Table
-- ornaments
import Graphics.UI.Gtk.Ornaments.Frame
import Graphics.UI.Gtk.Ornaments.HSeparator
import Graphics.UI.Gtk.Ornaments.VSeparator
-- printing
import Graphics.UI.Gtk.Printing.PaperSize
import Graphics.UI.Gtk.Printing.PageSetup
import Graphics.UI.Gtk.Printing.PrintContext
import Graphics.UI.Gtk.Printing.PrintOperation
import Graphics.UI.Gtk.Printing.PrintSettings
-- recent
import Graphics.UI.Gtk.Recent.RecentChooserMenu
import Graphics.UI.Gtk.Recent.RecentChooserWidget
import Graphics.UI.Gtk.Recent.RecentFilter
import Graphics.UI.Gtk.Recent.RecentManager
import Graphics.UI.Gtk.Recent.RecentInfo
import Graphics.UI.Gtk.Recent.RecentChooser
-- scrolling
import Graphics.UI.Gtk.Scrolling.HScrollbar
import Graphics.UI.Gtk.Scrolling.VScrollbar
import Graphics.UI.Gtk.Scrolling.ScrolledWindow
-- miscellaneous
import Graphics.UI.Gtk.Misc.Accessible
import Graphics.UI.Gtk.Misc.Adjustment
import Graphics.UI.Gtk.Misc.Arrow
import Graphics.UI.Gtk.Misc.Calendar
import Graphics.UI.Gtk.Misc.DrawingArea
#if GTK_CHECK_VERSION(3,16,0)
import Graphics.UI.Gtk.Misc.GLArea
#endif
import Graphics.UI.Gtk.Misc.EventBox
import Graphics.UI.Gtk.Misc.HandleBox
import Graphics.UI.Gtk.Misc.IMMulticontext
import Graphics.UI.Gtk.Misc.IMContextSimple
import Graphics.UI.Gtk.Misc.SizeGroup
import Graphics.UI.Gtk.Misc.Tooltip
#if GTK_MAJOR_VERSION < 3
import Graphics.UI.Gtk.Misc.Tooltips
#endif
import Graphics.UI.Gtk.Misc.Viewport
#if GTK_MAJOR_VERSION >= 3
import Graphics.UI.Gtk.Misc.Switch
#endif
--import Accessible
-- abstract base classes
import Graphics.UI.Gtk.Abstract.Box
import Graphics.UI.Gtk.Abstract.ButtonBox
import Graphics.UI.Gtk.Abstract.Container
import Graphics.UI.Gtk.Abstract.Bin
import Graphics.UI.Gtk.Abstract.Misc
import Graphics.UI.Gtk.Abstract.IMContext
import Graphics.UI.Gtk.Abstract.Object (
#if GTK_MAJOR_VERSION < 3
  Object,
  ObjectClass,
  castToObject,
  gTypeObject,
  toObject,
#endif
  GWeakNotify,
  objectWeakref,
  objectWeakunref,
  objectDestroy,
  notifyProperty )
import Graphics.UI.Gtk.Abstract.Paned
import Graphics.UI.Gtk.Abstract.Range
import Graphics.UI.Gtk.Abstract.Scale
import Graphics.UI.Gtk.Abstract.Scrollbar
import Graphics.UI.Gtk.Abstract.Separator
import Graphics.UI.Gtk.Abstract.Widget
-- cross-process embedding
#if defined(HAVE_PLUG_AND_SOCKET) && (!defined(WIN32) || GTK_CHECK_VERSION(2,8,0)) || defined(GDK_WINDOWING_X11)
import Graphics.UI.Gtk.Embedding.Plug
import Graphics.UI.Gtk.Embedding.Socket
#endif

-- non widgets
import System.Glib.Signals
{- do eport 'on' and 'after'
                (ConnectId, disconnect,
                                         signalDisconnect,
                                         signalBlock,
                                         signalUnblock)
-}
import System.Glib.Attributes
import System.Glib.GObject (
  GObject,
  GObjectClass,
  toGObject,
  castToGObject, gTypeGObject,
  quarkFromString,
  objectCreateAttribute,
  objectSetAttribute,
  objectGetAttributeUnsafe,
  isA
  )
import Graphics.UI.Gtk.Builder

-- pango modules
import Graphics.Rendering.Pango.Context
import Graphics.Rendering.Pango.Markup
import Graphics.Rendering.Pango.Layout
import Graphics.Rendering.Pango.Rendering
import Graphics.Rendering.Pango.Font
import Graphics.Rendering.Pango.Enums
