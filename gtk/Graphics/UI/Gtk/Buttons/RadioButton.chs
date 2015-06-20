{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget RadioButton
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
-- Note:
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
-- A choice from multiple check buttons
--
module Graphics.UI.Gtk.Buttons.RadioButton (
-- * Detail
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
-- Radio button widgets are created with 'radioButtonNew'.
-- Optionally, 'radioButtonNewWithLabel' can be used if you want a
-- text label on the radio button.
--
-- For the radio button functions that take an existing group, the groups are
-- represented by any of their members. So when adding widgets to an existing
-- group of radio buttons, use 'radioButtonNewFromWidget' with a 'RadioButton'
-- that is already a member of the group. The convenience function
-- 'radioButtonNewWithLabelFromWidget' is also provided.
--
-- To remove a 'RadioButton' from one group and make it part of a new one,
-- use 'radioButtonSetGroup'.
--
-- * How to create a group of two radio buttons.
--
-- >
-- > createRadioButtons :: IO ()
-- > createRadioButtons = do
-- >   window <- windowNew
-- >   box <- vBoxNew True 2
-- >
-- >   -- Create a radio button with a Entry widget
-- >   radio1 <- radioButtonNew
-- >   entry <- entryNew
-- >   containerAdd radio1 entry
-- >
-- >   -- Create a radio button with a label
-- >   radio2 <- radioButtonNewWithLabelFromWidget
-- >               radio1 "I'm the second radio button."
-- >
-- >   -- Pack them into a box, then show all the widgets
-- >   boxPackStart box radio1 PackGrow 2
-- >   boxPackStart box radio2 PackGrow 2
-- >   containerAdd window box
-- >   widgetShowAll window
-- >
--
-- When an unselected button in the group is clicked the clicked button
-- receives the \"toggled\" signal, as does the previously selected button.
-- Inside the \"toggled\" handler,
-- 'Graphics.UI.Gtk.Buttons.ToggleButton.toggleButtonGetActive' can be used to
-- determine if the button has been selected or deselected.

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
  castToRadioButton, gTypeRadioButton,
  toRadioButton,

-- * Constructors
  radioButtonNew,
  radioButtonNewWithLabel,
  radioButtonNewWithMnemonic,
  radioButtonNewFromWidget,
  radioButtonNewWithLabelFromWidget,
  radioButtonNewWithMnemonicFromWidget,

  -- * Compatibilty aliases
  radioButtonNewJoinGroup,
  radioButtonNewJoinGroupWithLabel,
  radioButtonNewJoinGroupWithMnemonic,

-- * Methods
  radioButtonSetGroup,
  radioButtonGetGroup,

-- * Attributes
  radioButtonGroup,

-- * Signals
#if GTK_CHECK_VERSION(2,4,0)
  groupChanged,
#endif

-- * Deprecated
#ifndef DISABLE_DEPRECATED
#if GTK_CHECK_VERSION(2,4,0)
  onGroupChanged,
  afterGroupChanged,
#endif
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'RadioButton' with a new group. To be of any practical
-- value, a widget should then be packed into the radio button.
--
radioButtonNew :: IO RadioButton
radioButtonNew =
  makeNewObject mkRadioButton $
  liftM (castPtr :: Ptr Widget -> Ptr RadioButton) $
  {# call unsafe radio_button_new #}
    nullPtr

-- | Creates a new 'RadioButton' with a text label.
--
radioButtonNewWithLabel :: GlibString string => string -> IO RadioButton
radioButtonNewWithLabel label =
  makeNewObject mkRadioButton $
  liftM (castPtr :: Ptr Widget -> Ptr RadioButton) $
  withUTFString label $ \labelPtr ->
  {# call unsafe radio_button_new_with_label #}
    nullPtr
    labelPtr

-- | Creates a new 'RadioButton' containing a label. The label will be created
-- using 'Graphics.UI.Gtk.Display.Label.labelNewWithMnemonic',
-- so underscores in @label@ indicate the mnemonic
-- for the button.
--
radioButtonNewWithMnemonic :: GlibString string
 => string         -- ^ @label@ - the text of the button, with an underscore
                   -- in front of the mnemonic character
 -> IO RadioButton
radioButtonNewWithMnemonic label =
  makeNewObject mkRadioButton $
  liftM (castPtr :: Ptr Widget -> Ptr RadioButton) $
  withUTFString label $ \labelPtr ->
  {# call unsafe radio_button_new_with_mnemonic #}
    nullPtr
    labelPtr

-- | Creates a new 'RadioButton', adding it to the same group as the group to
-- which @groupMember@ belongs. As with 'radioButtonNew', a widget should be
-- packed into the radio button.
--
radioButtonNewFromWidget ::
    RadioButton    -- ^ @groupMember@ - a member of an existing radio button
                   -- group, to which the new radio button will be added.
 -> IO RadioButton
radioButtonNewFromWidget group =
  makeNewObject mkRadioButton $
  liftM (castPtr :: Ptr Widget -> Ptr RadioButton) $
  {# call radio_button_new_from_widget #}
    group

-- | Creates a new 'RadioButton' with a text label, adding it to the same group
-- as the group to which @groupMember@ belongs.
--
radioButtonNewWithLabelFromWidget :: GlibString string
 => RadioButton    -- ^ @groupMember@ - a member of an existing radio button
                   -- group, to which the new radio button will be added.
 -> string         -- ^ @label@ - a text string to display next to the radio
                   -- button.
 -> IO RadioButton
radioButtonNewWithLabelFromWidget group label =
  makeNewObject mkRadioButton $
  liftM (castPtr :: Ptr Widget -> Ptr RadioButton) $
  withUTFString label $ \labelPtr ->
  {# call radio_button_new_with_label_from_widget #}
    group
    labelPtr

-- | Creates a new 'RadioButton' containing a label, adding it to the same group
-- as the group to which @groupMember@ belongs. The label will be created using
-- 'Graphics.UI.Gtk.Display.Label.labelNewWithMnemonic',
-- so underscores in @label@ indicate the mnemonic for the button.
--
radioButtonNewWithMnemonicFromWidget :: GlibString string
 => RadioButton    -- ^ @groupMember@ - a member of an existing radio button
                   -- group, to which the new radio button will be added.
 -> string         -- ^ @label@ - the text of the button, with an underscore
                   -- in front of the mnemonic character
 -> IO RadioButton
radioButtonNewWithMnemonicFromWidget group label =
  makeNewObject mkRadioButton $
  liftM (castPtr :: Ptr Widget -> Ptr RadioButton) $
  withUTFString label $ \labelPtr ->
  {# call radio_button_new_with_mnemonic_from_widget #}
    group
    labelPtr

-- | Alias for 'radioButtonNewFromWidget'.
radioButtonNewJoinGroup ::
    RadioButton    -- ^ @groupMember@ - a member of an existing radio button
                   -- group, to which the new radio button will be added.
 -> IO RadioButton
radioButtonNewJoinGroup = radioButtonNewFromWidget
{-# DEPRECATED radioButtonNewJoinGroup "use radioButtonNewFromWidget instead" #-}

-- | Alias for 'radioButtonNewWithLabelFromWidget'.
radioButtonNewJoinGroupWithLabel :: GlibString string
 => RadioButton    -- ^ @groupMember@ - a member of an existing radio button
                   -- group, to which the new radio button will be added.
 -> string         -- ^ @label@ - a text string to display next to the radio
                   -- button.
 -> IO RadioButton
radioButtonNewJoinGroupWithLabel = radioButtonNewWithLabelFromWidget
{-# DEPRECATED radioButtonNewJoinGroupWithLabel "use radioButtonNewWithLabelFromWidget instead" #-}

-- | Alias for 'radioButtonNewWithMnemonicFromWidget'.
radioButtonNewJoinGroupWithMnemonic :: GlibString string
 => RadioButton    -- ^ @groupMember@ - a member of an existing radio button
                   -- group, to which the new radio button will be added.
 -> string         -- ^ @label@ - the text of the button, with an underscore
                   -- in front of the mnemonic character
 -> IO RadioButton
radioButtonNewJoinGroupWithMnemonic = radioButtonNewWithMnemonicFromWidget
{-# DEPRECATED radioButtonNewJoinGroupWithMnemonic "use radioButtonNewWithMnemonicFromWidget instead" #-}

--------------------
-- Methods

-- | Sets a 'RadioButton's group. It should be noted that this does not
-- change the layout of your interface in any way, so if you are changing the
-- group, it is likely you will need to re-arrange the user interface to
-- reflect these changes.
--
radioButtonSetGroup :: RadioButton
 -> RadioButton -- ^ @groupMember@ - a member of an existing radio button group,
                -- to which this radio button will be added.
 -> IO ()
radioButtonSetGroup self group =
  {# call unsafe gtk_radio_button_get_group #} group >>= \groupGSList ->
  {# call gtk_radio_button_set_group #}
    self
    groupGSList

-- | Retrieves the group assigned to a radio button.
--
radioButtonGetGroup :: RadioButton
 -> IO [RadioButton] -- ^ returns a list containing all the radio buttons
                     -- in the same group as this radio button.
radioButtonGetGroup self =
  {# call unsafe gtk_radio_button_get_group #}
    self
  >>= readGSList
  >>= mapM (\elemPtr -> makeNewObject mkRadioButton (return elemPtr))

--------------------
-- Attributes

-- | Sets a new group for a radio button.
--
radioButtonGroup :: ReadWriteAttr RadioButton [RadioButton] RadioButton
radioButtonGroup = newAttr
  radioButtonGetGroup
  radioButtonSetGroup

--------------------
-- Signals

#if GTK_CHECK_VERSION(2,4,0)
-- %hash c:be94 d:a584
-- | Emitted when the group of radio buttons that a radio button belongs to
-- changes. This is emitted when a radio button switches from being alone to
-- being part of a group of 2 or more buttons, or vice-versa, and when a
-- buttton is moved from one group of 2 or more buttons to a different one, but
-- not when the composition of the group that a button belongs to changes.
--
-- * Available since Gtk+ version 2.4
--
groupChanged :: RadioButtonClass self => Signal self (IO ())
groupChanged = Signal (connect_NONE__NONE "group-changed")
#endif

--------------------
-- Deprecated Signals

#ifndef DISABLE_DEPRECATED

#if GTK_CHECK_VERSION(2,4,0)
-- | Emitted when the group of radio buttons that a radio button belongs to
-- changes. This is emitted when a radio button switches from being alone to
-- being part of a group of 2 or more buttons, or vice-versa, and when a
-- buttton is moved from one group of 2 or more buttons to a different one, but
-- not when the composition of the group that a button belongs to changes.
--
onGroupChanged, afterGroupChanged :: RadioButtonClass self => self
 -> IO ()
 -> IO (ConnectId self)
onGroupChanged = connect_NONE__NONE "group-changed" False
afterGroupChanged = connect_NONE__NONE "group-changed" True
#endif
#endif
