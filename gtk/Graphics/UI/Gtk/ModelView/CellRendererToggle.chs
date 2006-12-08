-- -*-haskell-*-
--  GIMP Toolkit (GTK) CellRendererToggle
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.5 $ from $Date: 2005/10/19 12:57:37 $
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
  cellActive,
  cellInconsistent,
  cellActivatable,
  cellRadio,
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes                   (Attr)
import System.Glib.Properties			(newAttrFromBoolProperty)
import Graphics.UI.Gtk.Abstract.Object		(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new 'CellRenderer' that displays a 'ToggleButton'.
--
cellRendererToggleNew :: IO CellRendererToggle
cellRendererToggleNew =
  makeNewObject mkCellRendererToggle $
  liftM (castPtr :: Ptr CellRenderer -> Ptr CellRendererToggle) $
  {# call unsafe cell_renderer_toggle_new #}

--------------------
-- Methods

-- | Determine whether the button is drawn as 'RadioButton' or not.
--
cellRendererToggleSetRadio :: CellRendererToggleClass self => self
 -> Bool
 -> IO ()
cellRendererToggleSetRadio self radio =
  {# call cell_renderer_toggle_set_radio #}
    (toCellRendererToggle self)
    (fromBool radio)

-- | Returns whether the button is drawn as 'RadioButton' or not.
--
cellRendererToggleGetRadio :: CellRendererToggleClass self => self -> IO Bool
cellRendererToggleGetRadio self =
  liftM toBool $
  {# call cell_renderer_toggle_get_radio #}
    (toCellRendererToggle self)

-- | Retrieve the current state of the button.
--
cellRendererToggleGetActive :: CellRendererToggleClass self => self -> IO Bool
cellRendererToggleGetActive self =
  liftM toBool $
  {# call unsafe cell_renderer_toggle_get_active #}
    (toCellRendererToggle self)


-- | Modify the state of the button.
--
cellRendererToggleSetActive :: CellRendererToggleClass self => self
 -> Bool
 -> IO ()
cellRendererToggleSetActive self setting =
  {# call cell_renderer_toggle_set_active #}
    (toCellRendererToggle self)
    (fromBool setting)

--------------------
-- Attributes

-- | The toggle state of the button.
--
-- Default value: @False@
--
cellActive :: CellRendererToggleClass self => Attr self Bool
cellActive = newAttrFromBoolProperty "active"

-- | The inconsistent state of the button.
--
-- Default value: @False@
--
cellInconsistent :: CellRendererToggleClass self => Attr self Bool
cellInconsistent = newAttrFromBoolProperty "inconsistent"

-- | The toggle button can be activated.
--
-- Default value: @True@
--
cellActivatable :: CellRendererToggleClass self => Attr self Bool
cellActivatable = newAttrFromBoolProperty "activatable"

-- | Draw the toggle button as a radio button.
--
-- Default value: @False@
--
cellRadio :: CellRendererToggleClass self => Attr self Bool
cellRadio = newAttrFromBoolProperty "radio"
