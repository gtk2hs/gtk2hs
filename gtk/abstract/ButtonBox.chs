-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget ButtonBox@
--
--  Author : Matthew Walton
--          
--  Created: 28 April 2004
--
--  Version $Revision: 1.1 $ from $Date: 2004/04/30 13:45:32 $
--
--  Copyright (c) 2004 Matthew Walton
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

module ButtonBox(
  ButtonBox,
  ButtonBoxClass,
  castToButtonBox,
  buttonBoxGetLayout,
  buttonBoxGetChildSecondary,
  buttonBoxSetLayout,
  buttonBoxSetChildSecondary
  ) where

import Monad (liftM)
import FFI

import Object (makeNewObject)
{#import Hierarchy#}
{#import Signal#}

import Enums (ButtonBoxStyle)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @method buttonBoxGetLayout@ Retrieve the method being used to
-- arrange the buttons in the button box
--
buttonBoxGetLayout :: ButtonBoxClass b => b -> IO ButtonBoxStyle
buttonBoxGetLayout b = liftM (toEnum . fromIntegral) $
  {#call gtk_button_box_get_layout#} (toButtonBox b)

-- @method buttonBoxGetChildSecondary@ Returns whether child should appear
-- in a secondary group of children
-- @ref arg child@
--
buttonBoxGetChildSecondary :: (ButtonBoxClass b, WidgetClass w) => b -> w -> IO Bool
buttonBoxGetChildSecondary b w = liftM toBool $
  {#call gtk_button_box_get_child_secondary#} (toButtonBox b) (toWidget w)

-- @method buttonBoxSetLayout@ Changes the way buttons are arranged in their container
-- @ref arg layout@
--
buttonBoxSetLayout :: ButtonBoxClass b => b -> ButtonBoxStyle -> IO ()
buttonBoxSetLayout b l =
  {#call gtk_button_box_set_layout#} (toButtonBox b)
    ((fromIntegral . fromEnum) l)

-- @method buttonBoxSetChildSecondary@ Sets whether child should appear in a secondary
-- group of children. A typical use of a secondary child is the help button in a dialog.
--
-- * This group appears after the other children if the style is GTK_BUTTONBOX_START,
-- ButtonboxSpread or ButtonboxEdge, and before the the other children if the
-- style is ButtonboxEnd. For horizontal button boxes, the definition of before/after
-- depends on direction of the widget (see widgetSetDirection). If the style is
-- buttonBoxStart or buttonBoxEnd, then the secondary children are aligned at
-- the other end of the button box from the main children. For the other styles,
-- they appear immediately next to the main children.
-- @ref arg widget@
-- @ref arg is_secondary@
--
buttonBoxSetChildSecondary :: (ButtonBoxClass b, WidgetClass w) => b -> w -> Bool -> IO ()
buttonBoxSetChildSecondary b w s =
  {#call gtk_button_box_set_child_secondary #} (toButtonBox b) (toWidget w)
    (fromBool s)
