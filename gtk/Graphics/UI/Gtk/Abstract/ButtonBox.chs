{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ButtonBox
--
--  Author : Matthew Walton
--
--  Created: 28 April 2004
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
-- * Detail
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
--
-- The main purpose of 'ButtonBox' is to make sure the children have all the
-- same size. Therefore it ignores the homogeneous property which it inherited
-- from 'Box', and always behaves as if homogeneous was @True@.

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
  castToButtonBox, gTypeButtonBox,
  toButtonBox,
  ButtonBoxStyle(..),

-- * Methods
  buttonBoxGetLayout,
  buttonBoxSetLayout,
  buttonBoxSetChildSecondary,
#if GTK_CHECK_VERSION(2,4,0)
  buttonBoxGetChildSecondary,
#endif
#if GTK_CHECK_VERSION(3,2,0)
  buttonBoxSetChildNonHomogeneous,
  buttonBoxGetChildNonHomogeneous,
#endif

-- * Attributes
  buttonBoxLayoutStyle,

-- * Child Attributes
  buttonBoxChildSecondary,
#if GTK_CHECK_VERSION(3,2,0)
  buttonBoxChildNonHomogeneous,
#endif
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Attributes
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.General.Enums (ButtonBoxStyle(..))
import Graphics.UI.Gtk.Abstract.ContainerChildProperties

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Retrieves the method being used to arrange the buttons in the button box.
--
buttonBoxGetLayout :: ButtonBoxClass self => self -> IO ButtonBoxStyle
buttonBoxGetLayout self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_button_box_get_layout #}
    (toButtonBox self)

#if GTK_CHECK_VERSION(2,4,0)
-- | Returns whether @child@ should appear in a secondary group of children.
--
-- * Available since Gtk+ version 2.4
--
buttonBoxGetChildSecondary :: (ButtonBoxClass self, WidgetClass child) => self
 -> child   -- ^ @child@ - a child of the button box widget
 -> IO Bool -- ^ returns whether @child@ should appear in a secondary group of
            -- children.
buttonBoxGetChildSecondary self child =
  liftM toBool $
  {# call gtk_button_box_get_child_secondary #}
    (toButtonBox self)
    (toWidget child)
#endif

#if GTK_CHECK_VERSION(3,2,0)
-- | Sets whether the child is exempted from homogeous sizing.
--
buttonBoxSetChildNonHomogeneous :: (ButtonBoxClass self, WidgetClass child) => self
 -> child -- ^ @child@ - a child of the button box widget
 -> Bool  -- ^ @nonHomogeneous@
 -> IO ()
buttonBoxSetChildNonHomogeneous self child nonHomogeneous =
  {# call gtk_button_box_set_child_non_homogeneous #}
    (toButtonBox self)
    (toWidget child)
    (fromBool nonHomogeneous)

-- | Returns whether the child is exempted from homogenous sizing.
--
buttonBoxGetChildNonHomogeneous :: (ButtonBoxClass self, WidgetClass child) => self
 -> child   -- ^ @child@ - a child of the button box widget
 -> IO Bool
buttonBoxGetChildNonHomogeneous self child =
  liftM toBool $
  {# call gtk_button_box_get_child_non_homogeneous #}
    (toButtonBox self)
    (toWidget child)
#endif

-- | Changes the way buttons are arranged in their container.
--
buttonBoxSetLayout :: ButtonBoxClass self => self
 -> ButtonBoxStyle -- ^ @layoutStyle@ - the new layout style.
 -> IO ()
buttonBoxSetLayout self layoutStyle =
  {# call gtk_button_box_set_layout #}
    (toButtonBox self)
    ((fromIntegral . fromEnum) layoutStyle)

-- | Sets whether @child@ should appear in a secondary group of children. A
-- typical use of a secondary child is the help button in a dialog.
--
-- This group appears after the other children if the style is
-- 'ButtonboxStart', 'ButtonboxSpread' or 'ButtonboxEdge', and before the other
-- children if the style is 'ButtonboxEnd'. For horizontal button boxes, the
-- definition of before\/after depends on direction of the widget (see
-- 'widgetSetDirection'). If the style is 'ButtonboxStart' or 'ButtonboxEnd',
-- then the secondary children are aligned at the other end of the button box
-- from the main children. For the other styles, they appear immediately next
-- to the main children.
--
buttonBoxSetChildSecondary :: (ButtonBoxClass self, WidgetClass child) => self
 -> child -- ^ @child@ - a child of the button box widget
 -> Bool  -- ^ @isSecondary@ - if @True@, the @child@ appears in a secondary
          -- group of the button box.
 -> IO ()
buttonBoxSetChildSecondary self child isSecondary =
  {# call gtk_button_box_set_child_secondary #}
    (toButtonBox self)
    (toWidget child)
    (fromBool isSecondary)

--------------------
-- Attributes

-- | How to layout the buttons in the box. Possible values are default,
-- spread, edge, start and end.
--
-- Default value: 'ButtonboxDefaultStyle'
--
buttonBoxLayoutStyle :: ButtonBoxClass self => Attr self ButtonBoxStyle
buttonBoxLayoutStyle = newAttr
  buttonBoxGetLayout
  buttonBoxSetLayout

--------------------
-- Child Attributes

-- | If @True@, the child appears in a secondary group of children, suitable
-- for, e.g., help buttons.
--
-- Default value: @False@
--
buttonBoxChildSecondary :: (ButtonBoxClass self, WidgetClass child) => child -> Attr self Bool
buttonBoxChildSecondary = newAttrFromContainerChildBoolProperty "secondary"

#if GTK_CHECK_VERSION(3,2,0)
-- | If @True@, the child will not be subject to homogeneous sizing.
--
-- Default value: @False@
--
buttonBoxChildNonHomogeneous :: (ButtonBoxClass self, WidgetClass child) => child -> Attr self Bool
buttonBoxChildNonHomogeneous = newAttrFromContainerChildBoolProperty "non-homogeneous"
#endif
