{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ScaleButton
--
--  Author : Andy Stewart
--
--  Created: 22 Mar 2010
--
--  Copyright (C) 2010 Andy Stewart
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
-- A button which pops up a scale
--
-- * Module available since Gtk+ version 2.12
--
module Graphics.UI.Gtk.Buttons.ScaleButton (

-- * Detail
--
-- | 'ScaleButton' provides a button which pops up a scale widget. This kind
-- of widget is commonly used for volume controls in multimedia applications,
-- and Gtk+ provides a 'VolumeButton' subclass that is tailored for this use
-- case.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Button'
-- |                                 +----ScaleButton
-- |                                       +----'VolumeButton'
-- @

#if GTK_CHECK_VERSION(2,12,0)
-- * Types
  ScaleButton,
  ScaleButtonClass,
  castToScaleButton,
  toScaleButton,

-- * Constructors
  scaleButtonNew,

-- * Methods
  scaleButtonSetIcons,
#if GTK_CHECK_VERSION(2,14,0)
  scaleButtonGetPopup,
  scaleButtonGetPlusButton,
  scaleButtonGetMinusButton,
#endif

-- * Attributes
  scaleButtonValue,
  scaleButtonSize,
  scaleButtonAdjustment,
  scaleButtonIcons,

-- * Signals
  scaleButtonPopdown,
  scaleButtonPopup,
  scaleButtonValueChanged,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
import Graphics.UI.Gtk.General.Structs  (IconSize(..))
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,12,0)
--------------------
-- Interfaces

-- instance OrientableClass ScaleButton

--------------------
-- Constructors

-- | Creates a 'ScaleButton', with a range between @min@ and @max@, with a
-- stepping of @step@.
--
scaleButtonNew :: GlibString string
 => IconSize -- ^ @size@ - a stock icon size
 -> Double   -- ^ @min@ - the minimum value of the scale (usually 0)
 -> Double   -- ^ @max@ - the maximum value of the scale (usually 100)
 -> Double   -- ^ @step@ - the stepping of value when a scroll-wheel event, or
             -- up\/down arrow event occurs (usually 2)
 -> [string] -- ^ @icons@
 -> IO ScaleButton
scaleButtonNew size min max step icons =
  makeNewObject mkScaleButton $
  liftM (castPtr :: Ptr Widget -> Ptr ScaleButton) $
  withUTFStringArray0 icons $ \iconsPtr ->
  {# call gtk_scale_button_new #}
    ((fromIntegral . fromEnum) size)
    (realToFrac min)
    (realToFrac max)
    (realToFrac step)
    iconsPtr

--------------------
-- Methods

-- | Sets the icons to be used by the scale button. For details, see the "icons" property.
scaleButtonSetIcons :: (ScaleButtonClass self, GlibString string) => self
 -> [string] -- ^ @icons@
 -> IO ()
scaleButtonSetIcons self icons =
  withUTFStringArray0 icons $ \iconsPtr ->
  {# call gtk_scale_button_set_icons #}
    (toScaleButton self)
    iconsPtr

#if GTK_CHECK_VERSION(2,14,0)
-- | Retrieves the popup of the 'ScaleButton'.
--
-- * Available since Gtk+ version 2.14
--
scaleButtonGetPopup :: ScaleButtonClass self => self
 -> IO Widget -- ^ returns the popup of the 'ScaleButton'
scaleButtonGetPopup self =
  makeNewObject mkWidget $
  {# call gtk_scale_button_get_popup #}
    (toScaleButton self)

-- | Retrieves the plus button of the 'ScaleButton'.
--
-- * Available since Gtk+ version 2.14
--
scaleButtonGetPlusButton :: ScaleButtonClass self => self
 -> IO Widget -- ^ returns the plus button of the 'ScaleButton'.
scaleButtonGetPlusButton self =
  makeNewObject mkWidget $
  {# call gtk_scale_button_get_plus_button #}
    (toScaleButton self)

-- | Retrieves the minus button of the 'ScaleButton'.
--
-- * Available since Gtk+ version 2.14
--
scaleButtonGetMinusButton :: ScaleButtonClass self => self
 -> IO Widget -- ^ returns the minus button of the 'ScaleButton'.
scaleButtonGetMinusButton self =
  makeNewObject mkWidget $
  {# call gtk_scale_button_get_minus_button #}
    (toScaleButton self)
#endif

--------------------
-- Attributes

-- | The value of the scale.
--
-- Default value: 0
scaleButtonValue :: ScaleButtonClass self => Attr self Double
scaleButtonValue = newAttrFromDoubleProperty "value"

-- | The icon size.
--
-- Default value: ''IconSizeSmallToolbar''
scaleButtonSize :: ScaleButtonClass self => Attr self IconSize
scaleButtonSize = newAttrFromEnumProperty "size"
                    {# call pure unsafe gtk_icon_size_get_type #}

-- | The 'Adjustment' that contains the current value of this scale button object.
scaleButtonAdjustment :: ScaleButtonClass self => Attr self Adjustment
scaleButtonAdjustment = newAttrFromObjectProperty "adjustment"
                          {# call pure unsafe gtk_adjustment_get_type #}

-- | The names of the icons to be used by the scale button. The first item in the array will be used in
-- the button when the current value is the lowest value, the second item for the highest value. All
-- the subsequent icons will be used for all the other values, spread evenly over the range of values.
--
-- If there's only one icon name in the icons array, it will be used for all the values. If only two
-- icon names are in the icons array, the first one will be used for the bottom 50% of the scale, and
-- the second one for the top 50%.
--
-- It is recommended to use at least 3 icons so that the 'ScaleButton' reflects the current value of
-- the scale better for the users.
--
-- Since 2.12
scaleButtonIcons :: (ScaleButtonClass self, GlibString string) => ReadWriteAttr self [string] (Maybe [string])
scaleButtonIcons =
  newAttr (objectGetPropertyBoxedOpaque (peekUTFStringArray0 . castPtr) gtype "search-path")
          (objectSetPropertyBoxedOpaque (\dirs f -> maybeWith withUTFStringArray0 dirs (f . castPtr)) gtype "search-path")
  where gtype = {#call pure g_strv_get_type#}


--------------------
-- Signals

-- | The 'scaleButtonValueChanged' signal is emitted when the value field has changed.
--
scaleButtonValueChanged :: ScaleButtonClass self => Signal self (Double -> IO ())
scaleButtonValueChanged = Signal (connect_DOUBLE__NONE "value_changed")

-- | The 'popup' signal is a keybinding signal which gets emitted to popup the scale widget.
--
-- The default bindings for this signal are Space, Enter and Return.
scaleButtonPopup :: ScaleButtonClass self => Signal self (IO ())
scaleButtonPopup = Signal (connect_NONE__NONE "popup")

-- | The 'popdown' signal is a keybinding signal which gets emitted to popdown the scale widget.
--
-- The default binding for this signal is Escape.
scaleButtonPopdown :: ScaleButtonClass self => Signal self (IO ())
scaleButtonPopdown = Signal (connect_NONE__NONE "popdown")
#endif
