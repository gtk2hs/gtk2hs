-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ToggleButton
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/25 01:11:32 $
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
-- Create buttons which retain their state.
--
module Graphics.UI.Gtk.Buttons.ToggleButton (
-- * Description
-- 
-- | A 'ToggleButton' is a 'Button' which will remain \'pressed-in\' when
-- clicked. Clicking again will cause the toggle button to return to its normal
-- state.
--
-- A toggle button is created by calling either 'toggleButtonNew' or
-- 'toggleButtonNewWithLabel'. If using the former, it is advisable to pack a
-- widget, (such as a 'Label' and\/or a 'Pixmap'), into the toggle button's
-- container. (See 'Button' for more information).
--
-- The state of a 'ToggleButton' can be set specifically using
-- 'toggleButtonSetActive', and retrieved using 'toggleButtonGetActive'.
--
-- To simply switch the state of a toggle button, use 'toggleButtonToggled'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Button'
-- |                                 +----ToggleButton
-- |                                       +----'CheckButton'
-- @

-- * Types
  ToggleButton,
  ToggleButtonClass,
  castToToggleButton,

-- * Constructors
  toggleButtonNew,
  toggleButtonNewWithLabel,
  toggleButtonNewWithMnemonic,

-- * Methods
  toggleButtonSetMode,
  toggleButtonGetMode,
  toggleButtonToggled,
  toggleButtonGetActive,
  toggleButtonSetActive,
  toggleButtonGetInconsistent,
  toggleButtonSetInconsistent,

-- * Signals
  onToggled,
  afterToggled
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new ToggleButton widget.
--
toggleButtonNew :: IO ToggleButton
toggleButtonNew  = makeNewObject mkToggleButton $ liftM castPtr 
  {#call unsafe toggle_button_new#}


-- | Create a ToggleButton with a label in it.
--
toggleButtonNewWithLabel :: String -> IO ToggleButton
toggleButtonNewWithLabel lbl = withUTFString lbl (\strPtr ->
  makeNewObject mkToggleButton $ liftM castPtr $
  {#call unsafe toggle_button_new_with_label#} strPtr)

-- | Create a ToggleButton with a label in it. Underscores in label indicate the
-- mnemonic for the button.
--
toggleButtonNewWithMnemonic :: String -> IO ToggleButton
toggleButtonNewWithMnemonic lbl = withUTFString lbl (\strPtr ->
  makeNewObject mkToggleButton $ liftM castPtr $
  {#call unsafe toggle_button_new_with_mnemonic#} strPtr)

--------------------
-- Methods

-- | Sets whether the button is displayed as a separate indicator and label.
-- You can call this function on a "CheckButton" or a "RadioButton" with @False@
-- to make the button look like a normal button.
--
toggleButtonSetMode :: ToggleButtonClass tb => tb -> Bool -> IO ()
toggleButtonSetMode tb mode =
  {#call toggle_button_set_mode#} (toToggleButton tb) (fromBool mode)

-- | Retrieves whether the button is displayed as a separate indicator and
-- label.
--
toggleButtonGetMode :: ToggleButtonClass tb => tb -> IO Bool
toggleButtonGetMode tb =
  liftM toBool $ {#call unsafe toggle_button_get_mode#} (toToggleButton tb)

-- | Emit the 'toggled' signal on the button.
--
toggleButtonToggled :: ToggleButtonClass tb => tb -> IO ()
toggleButtonToggled tb = {#call toggle_button_toggled#} (toToggleButton tb)

-- | Retrieve the current state of the button. Returns True if the button is
-- depressed.
--
toggleButtonGetActive :: ToggleButtonClass tb => tb -> IO Bool
toggleButtonGetActive tb = liftM toBool $
  {#call unsafe toggle_button_get_active#} (toToggleButton tb)

-- | Sets the state of the ToggleButton. True means the button should be
-- depressed.
--
toggleButtonSetActive :: ToggleButtonClass tb => Bool -> tb -> IO ()
toggleButtonSetActive active tb = 
  {#call toggle_button_set_active#} (toToggleButton tb) (fromBool active)

-- | Retrieve the inconsistent flag of the button. An inconsistent state only
-- visually affects the button. It will be displayed in an \"in-between\" state.
--
toggleButtonGetInconsistent :: ToggleButtonClass tb => tb -> IO Bool
toggleButtonGetInconsistent tb = liftM toBool $
  {#call unsafe toggle_button_get_inconsistent#} (toToggleButton tb)

-- | Sets the inconsistent flag of the ToggleButton.
--
toggleButtonSetInconsistent :: ToggleButtonClass tb => Bool -> tb -> IO ()
toggleButtonSetInconsistent incon tb = 
  {#call toggle_button_set_inconsistent#} (toToggleButton tb) (fromBool incon)

--------------------
-- Signals

-- | Whenever the state of the button is changed, the toggled signal is emitted.
--
onToggled, afterToggled :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onToggled = connect_NONE__NONE "toggled" False
afterToggled = connect_NONE__NONE "toggled" True

