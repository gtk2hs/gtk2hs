-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget ToggleButton@
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2003/07/09 22:42:43 $
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
-- * A ToggleButton is the base class for all buttons that have an inherit
--   state.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

module ToggleButton(
  ToggleButton,
  ToggleButtonClass,
  castToToggleButton,
  toggleButtonNew,
  toggleButtonNewWithLabel,
  toggleButtonSetMode,
  toggleButtonToggled,
  toggleButtonGetActive,
  toggleButtonSetActive,
  toggleButtonGetInconsistent,
  toggleButtonSetInconsistent,
  onToggled,
  afterToggled
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor toggleButtonNew@ Create a new ToggleButton widget.
--
toggleButtonNew :: IO ToggleButton
toggleButtonNew  = makeNewObject mkToggleButton $ liftM castPtr 
  {#call unsafe toggle_button_new#}


-- @method toggleButtonNewWithLabel@ Create a toggleButton with a label in it.
--
toggleButtonNewWithLabel :: String -> IO ToggleButton
toggleButtonNewWithLabel lbl = withUTFString lbl (\strPtr ->
  makeNewObject mkToggleButton $ liftM castPtr $
  {#call unsafe toggle_button_new_with_label#} strPtr)


-- @method toggleButtonSetMode@ Determines whether or not the toggle button is
-- drawn on screen. Set to True of the button should be invisible.
--
toggleButtonSetMode :: ToggleButtonClass tb => tb -> Bool -> IO ()
toggleButtonSetMode tb invisible =
  {#call toggle_button_set_mode#} (toToggleButton tb) (fromBool invisible)

-- @method toggleButtonToggled@ Emit the @ref method toggled@ signal on the
-- button.
--
toggleButtonToggled :: ToggleButtonClass tb => tb -> IO ()
toggleButtonToggled tb = {#call toggle_button_toggled#} (toToggleButton tb)

-- @method toggleButtonGetActive@ Retrieve the current state of the button.
-- Returns True if the button is depressed.
--
toggleButtonGetActive :: ToggleButtonClass tb => tb -> IO Bool
toggleButtonGetActive tb = liftM toBool $
  {#call unsafe toggle_button_get_active#} (toToggleButton tb)

-- Sets the state of the ToggleButton. True means the button should be
-- depressed. (EXPOTED)
--
toggleButtonSetActive :: ToggleButtonClass tb => Bool -> tb -> IO ()
toggleButtonSetActive active tb = 
  {#call toggle_button_set_active#} (toToggleButton tb) (fromBool active)

-- @method toggleButtonGetInconsistent@ Retrieve the inconsistent flag of the
-- button. An inconsistent state only visually affects the button. It will be
-- displayed in an "in-between" state.
--
toggleButtonGetInconsistent :: ToggleButtonClass tb => tb -> IO Bool
toggleButtonGetInconsistent tb = liftM toBool $
  {#call unsafe toggle_button_get_inconsistent#} (toToggleButton tb)

-- Sets the inconsistent flag of the ToggleButton. (EXPOTED)
--
toggleButtonSetInconsistent :: ToggleButtonClass tb => Bool -> tb -> IO ()
toggleButtonSetInconsistent incon tb = 
  {#call toggle_button_set_inconsistent#} (toToggleButton tb) (fromBool incon)

-- signals


-- @signal connectToToggled@ Whenever the state of the button is changed, the
-- toggled signal is emitted.
--
onToggled, afterToggled :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onToggled = connect_NONE__NONE "toggled" False
afterToggled = connect_NONE__NONE "toggled" True

