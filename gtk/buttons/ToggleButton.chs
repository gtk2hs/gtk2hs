-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget ToggleButton
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:19 $
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
-- * A ToggleButton is the base class for all buttons that have an inherit
--   state.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

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
  connectToToggled
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a new ToggleButton widget. (EXPORTED)
--
toggleButtonNew :: IO ToggleButton
toggleButtonNew = makeNewObject mkToggleButton $ liftM castPtr 
  {#call unsafe toggle_button_new#}


-- Create a toggleButton with a label in it. (EXPORTED)
--
toggleButtonNewWithLabel :: String -> IO ToggleButton
toggleButtonNewWithLabel lbl = withCString lbl (\strPtr ->
  makeNewObject mkToggleButton $ liftM castPtr $
  {#call unsafe toggle_button_new_with_label#} strPtr)


-- Determines whether or not the toggle button is drawn on screen.
-- Set to True of the button should be invisible. (EXPORTED)
--
toggleButtonSetMode :: ToggleButtonClass tb => Bool -> tb -> IO ()
toggleButtonSetMode invisible tb =
  {#call toggle_button_set_mode#} (toToggleButton tb) (fromBool invisible)

-- Emit the @toggled signal on the button. (EXPORTED)
--
toggleButtonToggled :: ToggleButtonClass tb => tb -> IO ()
toggleButtonToggled tb = {#call toggle_button_toggled#} (toToggleButton tb)

-- Retrieve the current state of the button. Returns True if the button
-- is depressed. (EXPORTED)
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

-- Retrieve the inconsistent flag of the button. An inconsistent state
-- only visually affects the button. It will be displayed in an "in-between"
-- state. (EXPORTED)
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


-- Whenever the state of the button is changed, the toggled signal is emitted.
-- (EXPORTED)
--
connectToToggled :: ButtonClass b => IO () -> ConnectAfter -> b -> IO (ConnectId b)
connectToToggled = connect_NONE__NONE "toggled"

