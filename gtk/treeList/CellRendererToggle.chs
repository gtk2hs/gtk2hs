-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: CellRendererToggle
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
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
--- DESCRIPTION ---------------------------------------------------------------
--
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

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
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import CellRenderer (Attribute(..))
import StoreValue   (GenericValue(..), TMType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a new @CellRenderer that displays a @ToggleButton. (EXPORTED)
--
cellRendererToggleNew :: IO CellRendererToggle
cellRendererToggleNew = makeNewObject mkCellRendererToggle $
  liftM castPtr $ {#call unsafe cell_renderer_toggle_new#}

-- Determine whether the button is drawn as @RadioButton or not. (EXPORTED)
--
cellRendererToggleSetRadio :: CellRendererToggleClass crt => 
  Bool -> crt -> IO ()
cellRendererToggleSetRadio radio crt = {#call cell_renderer_toggle_set_radio#}
  (toCellRendererToggle crt) (fromBool radio)

-- Retrieve the current state of the button. (EXPORTED)
--
cellRendererToggleGetActive :: CellRendererToggleClass crt => crt -> IO Bool
cellRendererToggleGetActive crt = liftM toBool $
  {#call unsafe cell_renderer_toggle_get_active#} (toCellRendererToggle crt)


-- Modify the state of the button. (EXPORTED)
--
cellRendererToggleSetActive :: CellRendererToggleClass crt => 
  Bool -> crt -> IO ()
cellRendererToggleSetActive act crt = {#call cell_renderer_toggle_set_active#}
  (toCellRendererToggle crt) (fromBool act)

-- helper function
--
binAttr str = AttrSingle str TMboolean
		(return.GVboolean)
	        (\(GVboolean b) -> return b)

-- Define the attribute that reflects the state of the button. (EXPORTED)
--
cellActive :: Attribute CellRendererToggle Bool
cellActive = binAttr "active"

-- Define an attribute that determines whether this button is shown as a
-- @RadioButton or as a normal @ToggleButton. (EXPORTED)
--
cellRadio :: Attribute CellRendererToggle Bool
cellRadio = binAttr "radio"
