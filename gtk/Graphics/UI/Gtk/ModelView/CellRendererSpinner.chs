{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget CellRendererSpinner
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
-- Renders a spinning animation in a cell
--
-- * Module available since Gtk+ version 2.20
--
module Graphics.UI.Gtk.ModelView.CellRendererSpinner (
-- * Detail
-- | 'CellRendererSpinner' renders a spinning animation in a cell, very similar to 'Spinner'. It can
-- often be used as an alternative to a 'CellRendererProgress' for displaying indefinite activity,
-- instead of actual progress.
--
-- To start the animation in a cell, set the "active" property to 'True' and increment the "pulse"
-- property at regular intervals.  The usual way to set the cell renderer properties for each cell is
-- to bind them to columns in your tree model using e.g.  'treeViewColumnAddAttribute'.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'CellRenderer'
-- |               +----'CellRendererSpinner'
-- @

#if GTK_CHECK_VERSION(2,20,0)
-- * Types
  CellRendererSpinner,
  CellRendererSpinnerClass,
  castToCellRendererSpinner,
  toCellRendererSpinner,

-- * Constructors
  cellRendererSpinnerNew,

-- * Attributes
  cellRendererSpinnerActive,
  cellRendererSpinnerPulse,
  cellRendererSpinnerSize,
#endif
) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object   (makeNewObject)
import Graphics.UI.Gtk.General.Structs  (IconSize(..))
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,20,0)
-- | Returns a new cell renderer which will show a spinner to indicate activity.
--
-- * Available since Gtk+ version 2.20
--
cellRendererSpinnerNew :: IO CellRendererSpinner
cellRendererSpinnerNew =
  makeNewObject mkCellRendererSpinner $ liftM castPtr $
  {#call gtk_cell_renderer_spinner_new #}

-- | Whether the spinner is active (ie. shown) in the cell.
--
-- Default value: 'False'
--
-- * Available since Gtk+ version 2.20
--
cellRendererSpinnerActive :: CellRendererSpinnerClass self => Attr self Bool
cellRendererSpinnerActive =
    newAttrFromBoolProperty "active"

-- | Pulse of the spinner. Increment this value to draw the next frame of the spinner animation. Usually,
-- you would update this value in a timeout.
--
-- The 'Spinner' widget draws one full cycle of the animation per second by default. You can learn
-- about the number of frames used by the theme by looking at the 'numSteps' style property and the
-- duration of the cycle by looking at 'cycleDuration'.
--
-- Default value: 0
--
-- * Available since Gtk+ version 2.20
--
cellRendererSpinnerPulse :: CellRendererSpinnerClass self => Attr self Int
cellRendererSpinnerPulse =
    newAttrFromIntProperty "pulse"

-- | The 'IconSize' value that specifies the size of the rendered spinner.
--
-- Default value: 'IconSizeMenu'
--
-- * Available since Gtk+ version 2.20
--
cellRendererSpinnerSize :: CellRendererSpinnerClass self => Attr self IconSize
cellRendererSpinnerSize =
    newAttrFromEnumProperty "size"
        {# call pure unsafe gtk_icon_size_get_type #}
#endif
