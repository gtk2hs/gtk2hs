-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ButtonBox
--
--  Author : Matthew Walton
--
--  Created: 28 April 2004
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/25 01:11:31 $
--
--  Copyright (C) 2004-2005 Matthew Walton
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
-- Base class for 'HButtonBox' and 'VButtonBox'
--
module Graphics.UI.Gtk.Abstract.ButtonBox (
-- * Description
-- 
-- | The primary purpose of this class is to keep track of the various
-- properties of 'HButtonBox' and 'VButtonBox' widgets.
--
-- 'buttonBoxGetChildSize' retrieves the minimum width and height for
-- widgets in a given button box. 'buttonBoxSetChildSize' allows those
-- properties to be changed.
--
-- The internal padding of buttons can be retrieved and changed per button
-- box using 'buttonBoxGetChildIpadding' and 'buttonBoxSetChildIpadding'
-- respectively.
--
-- 'buttonBoxGetSpacing' and 'buttonBoxSetSpacing' retrieve and change
-- default number of pixels between buttons, respectively.
--
-- 'buttonBoxGetLayout' and 'buttonBoxSetLayout' retrieve and alter the
-- method used to spread the buttons in a button box across the container,
-- respectively.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Box'
-- |                           +----ButtonBox
-- |                                 +----'HButtonBox'
-- |                                 +----'VButtonBox'
-- @

-- * Types
  ButtonBox,
  ButtonBoxClass,
  castToButtonBox,

-- * Methods
  buttonBoxGetLayout,
  buttonBoxSetLayout,
  buttonBoxSetChildSecondary,
#if GTK_CHECK_VERSION(2,4,0)
  buttonBoxGetChildSecondary
#endif
  ) where

import Monad (liftM)
import System.Glib.FFI


import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

import Graphics.UI.Gtk.General.Enums (ButtonBoxStyle)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Retrieve the method being used to
-- arrange the buttons in the button box
--
buttonBoxGetLayout :: ButtonBoxClass b => b -> IO ButtonBoxStyle
buttonBoxGetLayout b = liftM (toEnum . fromIntegral) $
  {#call gtk_button_box_get_layout#} (toButtonBox b)

#if GTK_CHECK_VERSION(2,4,0)
-- | Returns whether child should appear
-- in a secondary group of children
--
-- * Since Gtk 2.4.
buttonBoxGetChildSecondary :: (ButtonBoxClass b, WidgetClass w) => b -> w -> IO Bool
buttonBoxGetChildSecondary b w = liftM toBool $
  {#call gtk_button_box_get_child_secondary#} (toButtonBox b) (toWidget w)
#endif

-- | Changes the way buttons are arranged in their container
--
buttonBoxSetLayout :: ButtonBoxClass b => b -> ButtonBoxStyle -> IO ()
buttonBoxSetLayout b l =
  {#call gtk_button_box_set_layout#} (toButtonBox b)
    ((fromIntegral . fromEnum) l)

-- | Sets whether child should appear in a secondary
-- group of children. A typical use of a secondary child is the help button in a dialog.
--
-- * This group appears after the other children if the style is 'ButtonboxStart',
-- 'ButtonboxSpread' or 'ButtonboxEdge', and before the the other children if the
-- style is 'ButtonboxEnd'. For horizontal button boxes, the definition of before\/after
-- depends on direction of the widget (see 'widgetSetDirection'). If the style is
-- 'buttonBoxStart' or 'buttonBoxEnd', then the secondary children are aligned at
-- the other end of the button box from the main children. For the other styles,
-- they appear immediately next to the main children.
--
buttonBoxSetChildSecondary :: (ButtonBoxClass b, WidgetClass w) => b -> w -> Bool -> IO ()
buttonBoxSetChildSecondary b w s =
  {#call gtk_button_box_set_child_secondary #} (toButtonBox b) (toWidget w)
    (fromBool s)
