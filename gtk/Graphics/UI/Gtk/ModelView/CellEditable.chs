{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Interface CellEditable
--
--  Author : Andy Stewart
--
--  Created: 26 Mar 2010
--
--  Copyright (C) 2010 Andy Stewart
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
-- Interface for widgets which can are used for editing cells
--
module Graphics.UI.Gtk.ModelView.CellEditable (

-- * Detail
--
-- | The 'CellEditable' interface must be implemented for widgets to be usable
-- when editing the contents of a 'TreeView' cell.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GInterface'
-- |   +----CellEditable
-- @

-- * Types
  CellEditable,
  CellEditableClass,
  castToCellEditable,
  toCellEditable,

-- * Methods
  cellEditableStartEditing,
  cellEditableEmitEditingDone,
  cellEditableEmitRemoveWidget,

-- * Attributes
#if GTK_CHECK_VERSION(2,20,0)
  cellEditableEditingCanceled,
#endif

-- * Signals
  cellEditableEditingDone,
  cellEditableRemoveWidget,
  ) where

import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans (liftIO)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.Gdk.EventM (EventM, EAny)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Begins editing on a @cellEditable@. @event@ is the 'Event' that began the editing process.
--
cellEditableStartEditing :: CellEditableClass self => self -> EventM EAny ()
cellEditableStartEditing self = do
  eventPtr <- ask
  liftIO $ {# call gtk_cell_editable_start_editing #}
             (toCellEditable self)
             (castPtr eventPtr)

-- | Emits the 'cellEditableEditingDone' signal.
--
cellEditableEmitEditingDone :: CellEditableClass self => self -> IO ()
cellEditableEmitEditingDone self =
  {# call gtk_cell_editable_editing_done #}
    (toCellEditable self)

-- | Emits the 'cellEditableRemoveWidget' signal.
--
cellEditableEmitRemoveWidget :: CellEditableClass self => self -> IO ()
cellEditableEmitRemoveWidget self =
  {# call gtk_cell_editable_remove_widget #}
    (toCellEditable self)

--------------------
-- Attributes
#if GTK_CHECK_VERSION(2,20,0)
-- | Indicates whether editing on the cell has been canceled.
--
-- Default value: 'False'
--
-- * Available since Gtk+ version 2.20
--
cellEditableEditingCanceled :: CellEditableClass self => Attr self Bool
cellEditableEditingCanceled =
  newAttrFromBoolProperty "editing-canceled"
#endif

--------------------
-- Signals

-- | This signal is a sign for the cell renderer to update its value from the
-- @cellEditable@.
--
-- Implementations of 'CellEditable' are responsible for emitting this
-- signal when they are done editing, e.g. 'Entry' is emitting it when the user
-- presses Enter.
--
-- 'cellEditableEmitEditingDone' is a convenience method for emitting
-- ::editing-done.
--
cellEditableEditingDone :: CellEditableClass self => Signal self (IO ())
cellEditableEditingDone = Signal (connect_NONE__NONE "editing_done")

-- | This signal is meant to indicate that the cell is finished editing, and
-- the widget may now be destroyed.
--
-- Implementations of 'CellEditable' are responsible for emitting this
-- signal when they are done editing. It must be emitted after the
-- 'cellEditableEditingDone' signal, to give the cell
-- renderer a chance to update the cell's value before the widget is removed.
--
-- 'cellEditableEmitRemoveWidget' is a convenience method for emitting
-- ::remove-widget.
--
cellEditableRemoveWidget :: CellEditableClass self => Signal self (IO ())
cellEditableRemoveWidget = Signal (connect_NONE__NONE "remove_widget")
