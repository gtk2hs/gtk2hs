{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget CellRendererSpin
--
--  Author : Andy Stewart
--
--  Created: 25 Mar 2010
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
-- Renders a spin button in a cell
--
-- * Module available since Gtk+ version 2.10
--
module Graphics.UI.Gtk.ModelView.CellRendererSpin (

-- * Detail
--
-- | 'CellRendererSpin' renders text in a cell like 'CellRendererText' from
-- which it is derived. But while 'CellRendererText' offers a simple entry to
-- edit the text, 'CellRendererSpin' offers a 'SpinButton' widget. Of course,
-- that means that the text has to be parseable as a floating point number.
--
-- The range of the spinbutton is taken from the adjustment property of the
-- cell renderer, which can be set explicitly or mapped to a column in the tree
-- model, like all properties of cell renders. 'CellRendererSpin' also has
-- properties for the climb rate and the number of digits to display. Other
-- 'SpinButton' properties can be set in a handler for the start-editing
-- signal.
--
-- The 'CellRendererSpin' cell renderer was added in Gtk+ 2.10.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'CellRenderer'
-- |               +----'CellRendererText'
-- |                     +----CellRendererSpin
-- @

#if GTK_CHECK_VERSION(2,10,0)
-- * Types
  CellRendererSpin,
  CellRendererSpinClass,
  castToCellRendererSpin,
  toCellRendererSpin,

-- * Constructors
  cellRendererSpinNew,

-- * Attributes
  cellRendererSpinAdjustment,
  cellRendererSpinClimbRate,
  cellRendererSpinDigits,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object   (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,10,0)
--------------------
-- Constructors

-- | Creates a new 'CellRendererSpin'.
--
-- * Available since Gtk+ version 2.10
--
cellRendererSpinNew :: IO CellRendererSpin
cellRendererSpinNew =
  makeNewObject mkCellRendererSpin $ liftM castPtr
  {# call gtk_cell_renderer_spin_new #}

--------------------
-- Attributes

-- | The adjustment that holds the value of the spinbutton.
--
-- * Available since Gtk+ version 2.10
--
cellRendererSpinAdjustment :: CellRendererSpinClass self => Attr self Adjustment
cellRendererSpinAdjustment = newAttrFromObjectProperty "adjustment"
                               {# call pure unsafe gtk_adjustment_get_type #}

-- | The acceleration rate when you hold down a button.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
-- * Available since Gtk+ version 2.10
--
cellRendererSpinClimbRate :: CellRendererSpinClass self => Attr self Double
cellRendererSpinClimbRate = newAttrFromDoubleProperty "climb-rate"

-- | The number of decimal places to display.
--
-- Allowed values: <= 20
--
-- Default value: 0
--
-- * Available since Gtk+ version 2.10
--
cellRendererSpinDigits :: CellRendererSpinClass self => Attr self Int
cellRendererSpinDigits = newAttrFromUIntProperty "digits"
#endif
