-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget RadioButton@
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2002/08/05 16:41:34 $
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
-- * A radio group is a set of check buttons where only one button can be 
--   checked.
--
-- @documentation@ ------------------------------------------------------------
--
-- * Each radio button has to be associated with a group. Generating a new
--   radio button makes up a new group. Other group members can be added by
--   generating radio buttons with the function 
--   @ref method radioButtonNewJoinGroup@.
--
-- @todo@ ---------------------------------------------------------------------
--
-- * No function that directly accesses the group is bound. This is due to the
--   difficulties assuring that these groups are valid as the group is a plain
--   GSList from Glib.
--
module RadioButton(
  RadioButton,
  RadioButtonClass,
  castToRadioButton,
  radioButtonNew,
  radioButtonNewWithLabel,
  radioButtonNewJoinGroup,
  radioButtonNewJoinGroupWithLabel
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor radioButtonNew@ Create a new RadioButton widget with a new
-- group.
--
radioButtonNew :: IO RadioButton
radioButtonNew  = makeNewObject mkRadioButton $ liftM castPtr $
  {#call unsafe radio_button_new#} nullPtr

-- @method radioButtonNewWithLabel@ Like @ref type RadioButton@ but shows a
-- label to the right of the button.
--
radioButtonNewWithLabel :: String -> IO RadioButton
radioButtonNewWithLabel lbl = withCString lbl $ \strPtr -> 
  makeNewObject mkRadioButton $ liftM castPtr $
  {#call unsafe radio_button_new_with_label#} nullPtr strPtr

-- @method radioButtonNewJoinGroup@ Creates a new RadioButton and attaches it
-- to the group of another radio button.
--
-- * This function corresponds to gtk_radio_button_new_from_widget. The new
--   name makes more sense because we do not handle any other grouping
--   mechanism.
--
radioButtonNewJoinGroup :: RadioButton -> IO RadioButton
radioButtonNewJoinGroup rb = makeNewObject mkRadioButton $ liftM castPtr $
  {#call radio_button_new_from_widget#} rb

-- @method radioButtonNewJoinGroupWithLabel@ Create a new RadioButton with a
-- label and group.
--
radioButtonNewJoinGroupWithLabel :: RadioButton -> String -> IO RadioButton
radioButtonNewJoinGroupWithLabel rb lbl = withCString lbl $ \strPtr ->
  makeNewObject mkRadioButton $ liftM castPtr $
  {#call radio_button_new_with_label_from_widget#} rb strPtr

