{-# OPTIONS -cpp #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK)
--
--  Author : Axel Simon
--          
--  Created: 9 April 2001
--
--  Version $Revision: 1.16 $ from $Date: 2003/05/16 18:45:23 $
--
--  Copyright (c) 2001 Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- DESCRIPTION --------------------------------------------------------------
--
-- * This module gathers all publicly available functions from the Gtk binding.
--
-- * Everything that is marked as deprecated, vanishing or useless for
--   applications is not bound.
--
--- DOCU ----------------------------------------------------------------------
--
-- * The following modules are not bound:
--   DialogMessage : has only one variadic function which cannot be bound.
--		     The same functionality can be simulated with Dialog.
--   Item :	     The only child of this abstract class is MenuItem. The
--		     three signals Item defines are therefore bound in 
--		     MenuItem.
--   Bin :	     This abstract widget is of no use to the user. We should
--		     not need the Bin and BinClass entities either.
--   Editable :	     This should be the base class of Entry, but it is not.
--		     I moved everything into Entry.
--
--- TODO ----------------------------------------------------------------------
--
-- * Every module that is commented out and not mentioned above.
--
module Gtk(
  -- general things, initialization
  module General,
  module IconFactory,
  module StockItems,
  module Keys,
  module Style,
  module Drawable,
  module DrawWindow,
  module Region,
  module GC,
  module Pixbuf,
  -- windows
  module Dialog,
  module FileSel,
  module Window,
  -- display widgets,
  module AccelLabel,
  module Image,
  module Label,
  module ProgressBar,
  module Statusbar,
  -- buttons and toggles
  module Button,
  module CheckButton,
  module RadioButton,
  module ToggleButton,
  -- numeric/text data entry
  module Entry,
  module HScale,
  module VScale,
  module SpinButton,
  -- multiline text editor
  module TextIter,
  module TextMark,
  module TextBuffer,
  module TextTag,
  module TextTagTable,
  module TextView,
  -- tree and list widget
  module TreeModel,
  module TreeSelection,
  module TreeViewColumn,
  module TreeView,
  --  module TreeSortable,
  module TreeModelSort,
  module CellRenderer,
  --  module CellEditable,
  module CellRendererPixbuf,
  module CellRendererText,
  module CellRendererToggle,
  module ListStore,
  module TreeStore,
  -- menus, combo box, toolbar
  module CheckMenuItem,
  module Combo,
  module Menu,
  module MenuBar,
  module MenuItem,
  module MenuShell,
  module OptionMenu,
  module ImageMenuItem,
  module RadioMenuItem,
  module TearoffMenuItem,
  module Toolbar,
  -- selectors (file/font/color/input device)
--  module ColorSelection,
--  module ColorSelectionDialog,
--  module FileSelection,
--  module FontSelection,
--  module FontSelectionDialog,
--  module InputDialog,
  -- layout containers
  module Alignment,
  module AspectFrame,
  module HBox,
--  module Fixed,
  module HPaned,
  module Layout,
  module Notebook,
  module Table,
  module VBox,
  module VPaned,
  -- ornaments
  module Frame,
  module HSeparator,
  module VSeparator,
  -- scrolling
  module HScrollbar,
  module ScrolledWindow,
  module VScrollbar,
  -- miscellaneous
  module Adjustment,
  module Arrow,
  module Calendar,
  module DrawingArea,
  module EventBox,
  module HandleBox,
--  module IMContext,
--  module IMMulticontext,
  module Tooltips,
  module Viewport,
  -- abstract base classes
  module Box,
  module Container,
  module Misc,
  module Object,
  module Paned,
  module Range,
  module Scale,
  module Scrollbar,
  module Separator,
  module Widget,
#ifndef WIN32
  -- cross-process embedding
  module Plug,
  module Socket,
#endif
  -- non-widgets
  module Hierarchy,
  module Signal,

  -- pango modules
  module Markup,
  module PangoLayout,
  module Rendering
  ) where

-- general things, initialization
import General
import IconFactory
import StockItems
import Keys
import Style
import Drawable
import DrawWindow
import Region		hiding (makeNewRegion)
import GC
import Pixbuf
-- windows
import Dialog
import FileSel
import Window
--import WindowGroup
-- display widgets
import AccelLabel
import Image		hiding (imageGetPixbuf, imageNewFromPixbuf)
import Label
import ProgressBar
import Statusbar
-- buttons and toggles
import Button
import CheckButton
import RadioButton
import ToggleButton
-- numeric/text data entry
import Entry
import HScale
import VScale
import SpinButton
-- multiline text editor
import TextIter
import TextMark
import TextBuffer
import TextTag
import TextTagTable
import qualified TextView
import TextView hiding (afterSetScrollAdjustments,
		onSetScrollAdjustments, afterCopyClipboard, onCopyClipboard,
		afterCutClipboard, onCutClipboard, afterInsertAtCursor,
		onInsertAtCursor, afterPasteClipboard, onPasteClipboard,
		afterToggleOverwrite, onToggleOverwrite)
-- tree and list widget
import TreeModel hiding (createTreeIter,
			 createTreePath,
			 gtk_tree_model_get_iter_from_string)
import TreeSelection
import TreeViewColumn
import TreeView
--import TreeSortable
import TreeModelSort
import CellRenderer
--import CellEditable
import CellRendererPixbuf
import CellRendererText
import CellRendererToggle
import ListStore
import TreeStore
-- menus, combo box, toolbar
import Combo
-- import ItemFactory
import Menu
import MenuBar
import MenuItem
import MenuShell
import OptionMenu
import ImageMenuItem
import RadioMenuItem
import CheckMenuItem
import TearoffMenuItem
import Toolbar
-- selectors (file/font/color/input device)
--import ColorSelection
--import ColorSelectionDialog
--import FileSelection
--import FontSelection
--import FontSelectionDialog
--import InputDialog
-- layout containers
import Alignment
import AspectFrame
import HBox
import VBox
--import HButtonBox
--import VButtonBox
--import Fixed
import HPaned
import VPaned
import Layout
import Notebook
import Table
-- ornaments
import Frame
import HSeparator
import VSeparator
-- scrolling
import HScrollbar
import VScrollbar
import ScrolledWindow
-- miscellaneous
import Adjustment
import Arrow
import Calendar
import DrawingArea
import EventBox
import HandleBox
--import IMContext
--import IMContextSimple
--import IMMulitcontext
--import SizeGroup
import Tooltips
import Viewport
--import Accessible
-- abstract base classes
import Box
--import ButtonBox
import Container
import Misc
import Object
import Paned
import Range
import Scale
import Scrollbar
import Separator
import Widget
#ifndef WIN32
-- cross-process embedding
import Plug
import Socket
#endif

-- non widgets
import Hierarchy	(toCellRenderer)
import Signal		(ConnectId, disconnect)

-- pango modules
import Markup
import PangoLayout
import Rendering
