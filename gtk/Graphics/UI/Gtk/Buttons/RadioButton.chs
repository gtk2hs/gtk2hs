-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget RadioButton
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
-- TODO
--
-- No function that directly accesses the group is bound. This is due to the
--   difficulties assuring that these groups are valid as the group is a plain
--   GSList from Glib.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A choice from multiple check buttons.
--
module Graphics.UI.Gtk.Buttons.RadioButton (
-- * Description
-- 
-- | A single radio button performs the same basic function as a
-- 'CheckButton', as its position in the object hierarchy reflects. It is only
-- when multiple radio buttons are grouped together that they become a
-- different user interface component in their own right.
--
-- Every radio button is a member of some group of radio buttons. When one
-- is selected, all other radio buttons in the same group are deselected. A
-- 'RadioButton' is one way of giving the user a choice from many options.
--
-- Each radio button has to be associated with a group. Generating a new
-- radio button makes up a new group. Other group members can be added by
-- generating radio buttons with the function 'radioButtonNewJoinGroup'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Button'
-- |                                 +----'ToggleButton'
-- |                                       +----'CheckButton'
-- |                                             +----RadioButton
-- @

-- * Types
  RadioButton,
  RadioButtonClass,
  castToRadioButton,

-- * Constructors
  radioButtonNew,
  radioButtonNewWithLabel,
  radioButtonNewWithMnemonic,
  radioButtonNewJoinGroup,
  radioButtonNewJoinGroupWithLabel,
  radioButtonNewJoinGroupWithMnemonic,

  -- * Compatibilty aliases
  radioButtonNewFromWidget,
  radioButtonNewWithLabelFromWidget,
  radioButtonNewWithMnemonicFromWidget
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

-- | Create a new RadioButton widget with a new group.
--
radioButtonNew :: IO RadioButton
radioButtonNew  = makeNewObject mkRadioButton $ liftM castPtr $
  {#call unsafe radio_button_new#} nullPtr

-- | Like 'radioButtonNew' but shows a label to the right of the button.
--
radioButtonNewWithLabel :: String -> IO RadioButton
radioButtonNewWithLabel lbl = withUTFString lbl $ \strPtr -> 
  makeNewObject mkRadioButton $ liftM castPtr $
  {#call unsafe radio_button_new_with_label#} nullPtr strPtr

-- | Like 'radioButtonNew' but shows a label to the right of the button.
-- Underscores in the label string indicate the mnemonic for the menu item.
--
radioButtonNewWithMnemonic :: String -> IO RadioButton
radioButtonNewWithMnemonic lbl = withUTFString lbl $ \strPtr -> 
  makeNewObject mkRadioButton $ liftM castPtr $
  {#call unsafe radio_button_new_with_mnemonic#} nullPtr strPtr

-- | Creates a new RadioButton and attaches it to the group of another radio
-- button.
--
-- * This function corresponds to gtk_radio_button_new_from_widget. The new
--   name makes more sense because we do not handle any other grouping
--   mechanism.
--
radioButtonNewJoinGroup :: RadioButton -> IO RadioButton
radioButtonNewJoinGroup rb = makeNewObject mkRadioButton $ liftM castPtr $
  {#call radio_button_new_from_widget#} rb

-- | Create a new RadioButton with a label and group.
--
radioButtonNewJoinGroupWithLabel :: RadioButton -> String -> IO RadioButton
radioButtonNewJoinGroupWithLabel rb lbl = withUTFString lbl $ \strPtr ->
  makeNewObject mkRadioButton $ liftM castPtr $
  {#call radio_button_new_with_label_from_widget#} rb strPtr

-- | Create a new RadioButton with a label and group. Underscores in the label
-- string indicate the mnemonic for the menu item.
--
radioButtonNewJoinGroupWithMnemonic :: RadioButton -> String -> IO RadioButton
radioButtonNewJoinGroupWithMnemonic rb lbl = withUTFString lbl $ \strPtr ->
  makeNewObject mkRadioButton $ liftM castPtr $
  {#call radio_button_new_with_mnemonic_from_widget#} rb strPtr


-- | Alias for 'radioButtonNewJoinGroup'.
radioButtonNewFromWidget = radioButtonNewJoinGroup

-- | Alias for 'radioButtonNewJoinGroupWithLabel'.
radioButtonNewWithLabelFromWidget = radioButtonNewJoinGroupWithLabel

-- | Alias for 'radioButtonNewJoinGroupWithMnemonic'.
radioButtonNewWithMnemonicFromWidget = radioButtonNewJoinGroupWithMnemonic
