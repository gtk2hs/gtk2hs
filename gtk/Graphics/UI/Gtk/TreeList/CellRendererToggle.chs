-- -*-haskell-*-
--  GIMP Toolkit (GTK) CellRendererToggle
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/25 01:11:37 $
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
module Graphics.UI.Gtk.TreeList.CellRendererToggle (

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

-- * Constructors
  cellRendererToggleNew,

-- * Methods
  cellRendererToggleGetRadio,
  cellRendererToggleSetRadio,
  cellRendererToggleGetActive,
  cellRendererToggleSetActive,
  cellActive,
  cellRadio
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.TreeList.CellRenderer (Attribute(..))
import System.Glib.StoreValue   (GenericValue(..), TMType(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new 'CellRenderer' that displays a 'ToggleButton'.
--
cellRendererToggleNew :: IO CellRendererToggle
cellRendererToggleNew  = makeNewObject mkCellRendererToggle $
  liftM castPtr $ {#call unsafe cell_renderer_toggle_new#}

--------------------
-- Methods

-- | Determine whether the button is drawn as 'RadioButton' or not.
--
cellRendererToggleSetRadio :: CellRendererToggleClass crt => crt -> Bool ->
                              IO ()
cellRendererToggleSetRadio crt radio = {#call cell_renderer_toggle_set_radio#}
  (toCellRendererToggle crt) (fromBool radio)

-- | Returns wether the button is drawn as 'RadioButton' or not.
--
cellRendererToggleGetRadio :: CellRendererToggleClass crt => crt -> IO Bool
cellRendererToggleGetRadio crt = liftM toBool $
  {#call cell_renderer_toggle_get_radio#} (toCellRendererToggle crt)

-- | Retrieve the current state of the button.
--
cellRendererToggleGetActive :: CellRendererToggleClass crt => crt -> IO Bool
cellRendererToggleGetActive crt = liftM toBool $
  {#call unsafe cell_renderer_toggle_get_active#} (toCellRendererToggle crt)


-- | Modify the state of the button.
--
cellRendererToggleSetActive :: CellRendererToggleClass crt => crt -> Bool ->
                               IO ()
cellRendererToggleSetActive crt act = {#call cell_renderer_toggle_set_active#}
  (toCellRendererToggle crt) (fromBool act)

-- helper function
--
binAttr :: [String] -> Attribute CellRendererToggle Bool
binAttr str = Attribute str [TMboolean]
		(return.(\x -> [x]).GVboolean)
	        (\[GVboolean b] -> return b)

-- | Define the attribute that reflects the state of the button.
--
cellActive :: Attribute CellRendererToggle Bool
cellActive  = binAttr ["active"]

-- | Define an attribute that determines whether this button
-- is shown as a 'RadioButton' or as a normal 'ToggleButton'.
--
cellRadio :: Attribute CellRendererToggle Bool
cellRadio  = binAttr ["radio"]
