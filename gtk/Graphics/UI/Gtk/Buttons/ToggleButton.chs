{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ToggleButton
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
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
-- Create buttons which retain their state
--
module Graphics.UI.Gtk.Buttons.ToggleButton (
-- * Detail
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
  castToToggleButton, gTypeToggleButton,
  toToggleButton,

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

-- * Attributes
  toggleButtonActive,
  toggleButtonInconsistent,
  toggleButtonDrawIndicator,
  toggleButtonMode,

-- * Signals
  toggled,

-- * Deprecated
#ifndef DISABLE_DEPRECATED
-- * Signals
  onToggled,
  afterToggled,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new toggle button. A widget should be packed into the button,
-- as in 'Graphics.UI.Gtk.Buttons.Button.buttonNew'.
--
toggleButtonNew :: IO ToggleButton
toggleButtonNew =
  makeNewObject mkToggleButton $
  liftM (castPtr :: Ptr Widget -> Ptr ToggleButton) $
  {# call unsafe toggle_button_new #}

-- | Creates a new toggle button with a text label.
--
toggleButtonNewWithLabel :: GlibString string
 => string          -- ^ @label@ - a string containing the message to be
                    -- placed in the toggle button.
 -> IO ToggleButton
toggleButtonNewWithLabel label =
  makeNewObject mkToggleButton $
  liftM (castPtr :: Ptr Widget -> Ptr ToggleButton) $
  withUTFString label $ \labelPtr ->
  {# call unsafe toggle_button_new_with_label #}
    labelPtr

-- | Creates a new 'ToggleButton' containing a label. The label will be
-- created using 'Graphics.UI.Gtk.Display.Label.labelNewWithMnemonic',
-- so underscores in @label@ indicate the
-- mnemonic for the button.
--
toggleButtonNewWithMnemonic :: GlibString string
 => string          -- ^ @label@ - the text of the button, with an underscore
                    -- in front of the mnemonic character
 -> IO ToggleButton
toggleButtonNewWithMnemonic label =
  makeNewObject mkToggleButton $
  liftM (castPtr :: Ptr Widget -> Ptr ToggleButton) $
  withUTFString label $ \labelPtr ->
  {# call unsafe toggle_button_new_with_mnemonic #}
    labelPtr

--------------------
-- Methods

-- | Sets whether the button is displayed as a separate indicator and label.
-- You can call this function on a 'CheckButton' or a 'RadioButton' with @False@
-- to make the button look like a normal button.
--
-- This function only affects instances of classes like 'CheckButton' and
-- 'RadioButton' that derive from 'ToggleButton', not instances of
-- 'ToggleButton' itself.
--
toggleButtonSetMode :: ToggleButtonClass self => self
 -> Bool  -- ^ @drawIndicator@ - if @True@, draw the button as a separate
          -- indicator and label; if @False@, draw the button like a normal
          -- button
 -> IO ()
toggleButtonSetMode self drawIndicator =
  {# call toggle_button_set_mode #}
    (toToggleButton self)
    (fromBool drawIndicator)

-- | Retrieves whether the button is displayed as a separate indicator and
-- label. See 'toggleButtonSetMode'.
--
toggleButtonGetMode :: ToggleButtonClass self => self
 -> IO Bool -- ^ returns @True@ if the togglebutton is drawn as a separate
            -- indicator and label.
toggleButtonGetMode self =
  liftM toBool $
  {# call unsafe toggle_button_get_mode #}
    (toToggleButton self)

-- | Emits the toggled signal on the 'ToggleButton'. There is no good reason
-- for an application ever to call this function.
--
toggleButtonToggled :: ToggleButtonClass self => self -> IO ()
toggleButtonToggled self =
  {# call toggle_button_toggled #}
    (toToggleButton self)

-- | Queries a 'ToggleButton' and returns its current state. Returns @True@ if
-- the toggle button is pressed in and @False@ if it is raised.
--
toggleButtonGetActive :: ToggleButtonClass self => self -> IO Bool
toggleButtonGetActive self =
  liftM toBool $
  {# call unsafe toggle_button_get_active #}
    (toToggleButton self)

-- | Sets the status of the toggle button. Set to @True@ if you want the
-- 'ToggleButton' to be \'pressed in\', and @False@ to raise it. This action
-- causes the toggled signal to be emitted.
--
toggleButtonSetActive :: ToggleButtonClass self => self
 -> Bool  -- ^ @isActive@ - @True@ or @False@.
 -> IO ()
toggleButtonSetActive self isActive =
  {# call toggle_button_set_active #}
    (toToggleButton self)
    (fromBool isActive)

-- | Gets the value set by 'toggleButtonSetInconsistent'.
--
toggleButtonGetInconsistent :: ToggleButtonClass self => self
 -> IO Bool -- ^ returns @True@ if the button is displayed as inconsistent,
            -- @False@ otherwise
toggleButtonGetInconsistent self =
  liftM toBool $
  {# call unsafe toggle_button_get_inconsistent #}
    (toToggleButton self)

-- | If the user has selected a range of elements (such as some text or
-- spreadsheet cells) that are affected by a toggle button, and the current
-- values in that range are inconsistent, you may want to display the toggle in
-- an \"in between\" state. This function turns on \"in between\" display.
-- Normally you would turn off the inconsistent state again if the user toggles
-- the toggle button. This has to be done manually,
-- 'toggleButtonSetInconsistent' only affects visual appearance, it doesn't
-- affect the semantics of the button.
--
toggleButtonSetInconsistent :: ToggleButtonClass self => self
 -> Bool  -- ^ @setting@ - @True@ if state is inconsistent
 -> IO ()
toggleButtonSetInconsistent self setting =
  {# call toggle_button_set_inconsistent #}
    (toToggleButton self)
    (fromBool setting)

--------------------
-- Attributes

-- | If the toggle button should be pressed in or not.
--
-- Default value: @False@
--
toggleButtonActive :: ToggleButtonClass self => Attr self Bool
toggleButtonActive = newAttr
  toggleButtonGetActive
  toggleButtonSetActive

-- | If the toggle button is in an \"in between\" state.
--
-- Default value: @False@
--
toggleButtonInconsistent :: ToggleButtonClass self => Attr self Bool
toggleButtonInconsistent = newAttr
  toggleButtonGetInconsistent
  toggleButtonSetInconsistent

-- | If the toggle part of the button is displayed.
--
-- Default value: @False@
--
toggleButtonDrawIndicator :: ToggleButtonClass self => Attr self Bool
toggleButtonDrawIndicator = newAttrFromBoolProperty "draw-indicator"

-- | \'mode\' property. See 'toggleButtonGetMode' and 'toggleButtonSetMode'
--
toggleButtonMode :: ToggleButtonClass self => Attr self Bool
toggleButtonMode = newAttr
  toggleButtonGetMode
  toggleButtonSetMode

--------------------
-- Signals

-- %hash c:467 d:227e
-- | Should be connected if you wish to perform an action whenever the
-- 'ToggleButton''s state is changed.
--
toggled :: ToggleButtonClass self => Signal self (IO ())
toggled = Signal (connect_NONE__NONE "toggled")

--------------------
-- Deprecated Signals

#ifndef DISABLE_DEPRECATED
-- | Whenever the state of the button is changed, the toggled signal is
-- emitted.
--
onToggled, afterToggled :: ToggleButtonClass self => self
 -> IO ()
 -> IO (ConnectId self)
onToggled = connect_NONE__NONE "toggled" False
afterToggled = connect_NONE__NONE "toggled" True
#endif
