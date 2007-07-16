-- -*-haskell-*-
--  GIMP Toolkit (GTK) CellRendererToggle
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- Renders a toggle button in a cell
--
module Graphics.UI.Gtk.ModelView.CellRendererToggle (
-- * Detail
-- 
-- | 'CellRendererToggle' renders a toggle button in a cell. The button is
-- drawn as a radio or checkbutton, depending on the radio property. When
-- activated, it emits the toggled signal.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'CellRenderer'
-- |               +----CellRendererToggle
-- @

-- * Types
  CellRendererToggle,
  CellRendererToggleClass,
  castToCellRendererToggle,
  toCellRendererToggle,

-- * Constructors
  cellRendererToggleNew,

-- * Methods
  cellRendererToggleGetRadio,
  cellRendererToggleSetRadio,
  cellRendererToggleGetActive,
  cellRendererToggleSetActive,

-- * Attributes
  cellToggleActive,
  cellToggleInconsistent,
  cellToggleActivatable,
  cellToggleRadio,
  cellToggleIndicatorSize,

-- * Signals
  toggled,

-- * Deprecated
#ifndef DISABLE_DEPRECATED
  onToggled,
  afterToggled
#endif
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes                   (Attr)
import System.Glib.Properties			(newAttrFromBoolProperty,
						 newAttrFromIntProperty)
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- %hash c:bafb d:640f
-- | Creates a new 'CellRendererToggle'. Adjust rendering parameters using
-- object properties. Object properties can be set globally (with
-- 'System.Glib.Attributes.set'). Also, within a
-- 'Graphics.UI.Gtk.ModelView.TreeViewColumn', you can bind a property to a
-- value in a 'Graphics.UI.Gtk.ModelView.TreeModel.TreeModel' using
-- 'Graphics.UI.Gtk.ModelView.CellLayout.cellLayoutSetAttributes'. For
-- example, you can bind the 'cellToggleActive' property on the cell renderer
-- to a boolean value in the model, thus causing the check button to reflect
-- the state of the model.
--
cellRendererToggleNew :: IO CellRendererToggle
cellRendererToggleNew =
  makeNewObject mkCellRendererToggle $
  liftM (castPtr :: Ptr CellRenderer -> Ptr CellRendererToggle) $
  {# call unsafe cell_renderer_toggle_new #}

--------------------
-- Methods

-- %hash c:133b d:c428
-- | If @radio@ is @True@, the cell renderer renders a radio toggle (i.e. a
-- toggle in a group of mutually-exclusive toggles). If @False@, it renders a
-- check toggle (a standalone boolean option). This can be set globally for
-- the cell renderer, or changed just before rendering each cell in the model
-- (for 'TreeView', you set up a per-row setting using 'TreeViewColumn' to
-- associate model columns with cell renderer properties).
--
cellRendererToggleSetRadio :: CellRendererToggleClass self => self
 -> Bool -- ^ @radio@ - @True@ to make the toggle look like a radio button
 -> IO ()
cellRendererToggleSetRadio self radio =
  {# call cell_renderer_toggle_set_radio #}
    (toCellRendererToggle self)
    (fromBool radio)

-- %hash c:7f39 d:fe9f
-- | Returns whether we\'re rendering radio toggles rather than checkboxes.
--
cellRendererToggleGetRadio :: CellRendererToggleClass self => self
 -> IO Bool -- ^ returns @True@ if we\'re rendering radio toggles rather than
            -- checkboxes
cellRendererToggleGetRadio self =
  liftM toBool $
  {# call cell_renderer_toggle_get_radio #}
    (toCellRendererToggle self)

-- %hash c:4974 d:3d45
-- | Returns whether the cell renderer is active. See
-- 'cellRendererToggleSetActive'.
--
cellRendererToggleGetActive :: CellRendererToggleClass self => self
 -> IO Bool -- ^ returns @True@ if the cell renderer is active.
cellRendererToggleGetActive self =
  liftM toBool $
  {# call unsafe cell_renderer_toggle_get_active #}
    (toCellRendererToggle self)

-- %hash c:8420 d:5177
-- | Activates or deactivates a cell renderer.
--
cellRendererToggleSetActive :: CellRendererToggleClass self => self
 -> Bool -- ^ @setting@ - the value to set.
 -> IO ()
cellRendererToggleSetActive self setting =
  {# call cell_renderer_toggle_set_active #}
    (toCellRendererToggle self)
    (fromBool setting)

--------------------
-- Attributes

-- %hash c:aed9 d:ab32
-- | The toggle state of the button.
--
-- Default value: @False@
--
cellToggleActive :: CellRendererToggleClass self => Attr self Bool
cellToggleActive = newAttrFromBoolProperty "active"

-- %hash c:85c8 d:8ab1
-- | The inconsistent state of the button.
--
-- Default value: @False@
--
cellToggleInconsistent :: CellRendererToggleClass self => Attr self Bool
cellToggleInconsistent = newAttrFromBoolProperty "inconsistent"

-- %hash c:74e5 d:e41e
-- | The toggle button can be activated.
--
-- Default value: @True@
--
cellToggleActivatable :: CellRendererToggleClass self => Attr self Bool
cellToggleActivatable = newAttrFromBoolProperty "activatable"

-- %hash c:61f2 d:5449
-- | Draw the toggle button as a radio button.
--
-- Default value: @False@
--
cellToggleRadio :: CellRendererToggleClass self => Attr self Bool
cellToggleRadio = newAttrFromBoolProperty "radio"

-- %hash c:698 d:47b4
-- | Size of check or radio indicator.
--
-- Allowed values: >= 0
--
-- Default value: 12
--
cellToggleIndicatorSize :: CellRendererToggleClass self => Attr self Int
cellToggleIndicatorSize = newAttrFromIntProperty "indicator-size"

--------------------
-- Signals

-- %hash c:33ab d:1ba3
-- | The 'toggled' signal is emitted when the cell is toggled.
--
toggled :: CellRendererToggleClass self => Signal self (String -> IO ())
toggled = Signal (connect_STRING__NONE "toggled")

--------------------
-- Deprecated Signals

#ifndef DISABLE_DEPRECATED
-- %hash c:21f7
onToggled :: CellRendererToggleClass self => self
 -> (String -> IO ())
 -> IO (ConnectId self)
onToggled = connect_STRING__NONE "toggled" False
{-# DEPRECATED onToggled "instead of 'onToggled obj' use 'on obj toggled'" #-}

-- %hash c:82f6
afterToggled :: CellRendererToggleClass self => self
 -> (String -> IO ())
 -> IO (ConnectId self)
afterToggled = connect_STRING__NONE "toggled" True
{-# DEPRECATED afterToggled "instead of 'afterToggled obj' use 'after obj toggled'" #-}
#endif
