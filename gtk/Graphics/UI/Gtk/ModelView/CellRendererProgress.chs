{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget CellRendererProgress
--
--  Author : Duncan Coutts
--
--  Created: 2 November 2005
--
--  Copyright (C) 2005 Duncan Coutts
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
-- Renders numbers as progress bars
--
-- * Module available since Gtk+ version 2.6
--
module Graphics.UI.Gtk.ModelView.CellRendererProgress (
-- * Detail
--
-- | 'CellRendererProgress' renders a numeric value as a progress par in a
-- cell. Additionally, it can display a text on top of the progress bar.
--

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'CellRenderer'
-- |               +----CellRendererProgress
-- @

#if GTK_CHECK_VERSION(2,6,0)
-- * Types
  CellRendererProgress,
  CellRendererProgressClass,
  castToCellRendererProgress, gTypeCellRendererProgress,
  toCellRendererProgress,

-- * Constructors
  cellRendererProgressNew,

-- * Attributes
  cellProgressValue,
  cellProgressText,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes                   (Attr)
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object          (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,6,0)
--------------------
-- Constructors

-- | Creates a new 'CellRendererProgress'.
--
cellRendererProgressNew :: IO CellRendererProgress
cellRendererProgressNew =
  makeNewObject mkCellRendererProgress $
  liftM (castPtr :: Ptr CellRenderer -> Ptr CellRendererProgress) $
  {# call gtk_cell_renderer_progress_new #}

--------------------
-- Attributes

-- | The \"value\" property determines the percentage to which the progress
-- bar will be \"filled in\".
--
-- Allowed values: @[0,100]@
--
-- Default value: @0@
--
cellProgressValue :: CellRendererProgressClass self => Attr self Int
cellProgressValue = newAttrFromIntProperty "value"

-- | The 'cellProgressText' attribute determines the label which will be drawn
-- over the progress bar. Setting this property to @Nothing@ causes the
-- default label to be displayed. Setting this property to an empty string
-- causes no label to be displayed.
--
-- Default value: @Nothing@
--
cellProgressText :: (CellRendererProgressClass self, GlibString string) => Attr self (Maybe string)
cellProgressText = newAttrFromMaybeStringProperty "text"
#endif
