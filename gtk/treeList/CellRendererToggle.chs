-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry CellRendererToggle@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.7 $ from $Date: 2003/07/09 22:42:46 $
--
--  Copyright (c) 1999..2002 Axel Simon
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
-- @description@ --------------------------------------------------------------
--
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

module CellRendererToggle(
  CellRendererToggle,
  CellRendererToggleClass,
  castToCellRendererToggle,
  cellRendererToggleNew,
  cellRendererToggleSetRadio,
  cellRendererToggleGetActive,
  cellRendererToggleSetActive,
  cellActive,
  cellRadio
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import CellRenderer (Attribute(..))
import StoreValue   (GenericValue(..), TMType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor cellRendererToggleNew@ Create a new @ref data CellRenderer@
-- that displays a @ref data ToggleButton@.
--
cellRendererToggleNew :: IO CellRendererToggle
cellRendererToggleNew  = makeNewObject mkCellRendererToggle $
  liftM castPtr $ {#call unsafe cell_renderer_toggle_new#}

-- @method cellRendererToggleSetRadio@ Determine whether the button is drawn
-- as @ref data RadioButton@ or not.
--
cellRendererToggleSetRadio :: CellRendererToggleClass crt => crt -> Bool ->
                              IO ()
cellRendererToggleSetRadio crt radio = {#call cell_renderer_toggle_set_radio#}
  (toCellRendererToggle crt) (fromBool radio)

-- @method cellRendererToggleGetActive@ Retrieve the current state of the
-- button.
--
cellRendererToggleGetActive :: CellRendererToggleClass crt => crt -> IO Bool
cellRendererToggleGetActive crt = liftM toBool $
  {#call unsafe cell_renderer_toggle_get_active#} (toCellRendererToggle crt)


-- @method cellRendererToggleSetActive@ Modify the state of the button.
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

-- @method cellActive@ Define the attribute that reflects the state of the
-- button.
--
cellActive :: Attribute CellRendererToggle Bool
cellActive  = binAttr ["active"]

-- @method cellRadio@ Define an attribute that determines whether this button
-- is shown as a @ref data RadioButton@ or as a normal @ref data ToggleButton@.
--
cellRadio :: Attribute CellRendererToggle Bool
cellRadio  = binAttr ["radio"]
