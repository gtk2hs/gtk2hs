{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Range
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
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
-- Base class for widgets which visualize an adjustment
--
module Graphics.UI.Gtk.Abstract.Range (
-- * Description
--
-- | For signals regarding a change in the range or increments, refer to
-- 'Adjustment' which is contained in the 'Range' object.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----Range
-- |                     +----'Scale'
-- |                     +----'Scrollbar'
-- @

-- * Types
  Range,
  RangeClass,
  castToRange, gTypeRange,
  toRange,

-- * Methods
  rangeGetAdjustment,
  rangeSetAdjustment,
#if GTK_MAJOR_VERSION < 3
  rangeGetUpdatePolicy,
  rangeSetUpdatePolicy,
#endif
  rangeGetInverted,
  rangeSetInverted,
  rangeGetValue,
  rangeSetValue,
  rangeSetIncrements,
  rangeSetRange,
  ScrollType(..),
#if GTK_CHECK_VERSION(2,10,0)
  SensitivityType(..),
  rangeSetLowerStepperSensitivity,
  rangeGetLowerStepperSensitivity,
  rangeSetUpperStepperSensitivity,
  rangeGetUpperStepperSensitivity,
#endif
#if GTK_CHECK_VERSION(2,20,0)
  rangeGetMinSliderSize,
  rangeGetRangeRect,
  rangeGetSliderRange,
  rangeGetSliderSizeFixed,
  rangeSetMinSliderSize,
  rangeSetSliderSizeFixed,
#endif

-- * Attributes
#if GTK_MAJOR_VERSION < 3
  rangeUpdatePolicy,
#endif
  rangeAdjustment,
  rangeInverted,
#if GTK_CHECK_VERSION(2,10,0)
  rangeLowerStepperSensitivity,
  rangeUpperStepperSensitivity,
#endif
  rangeValue,
#if GTK_CHECK_VERSION(2,20,0)
  rangeSliderSizeFixed,
  rangeMinSliderSize,
#endif

-- * Signals
  adjustBounds,
  valueChanged,
#if GTK_CHECK_VERSION(2,6,0)
  changeValue,
#endif

-- * Deprecated
#ifndef DISABLE_DEPRECATED
  onMoveSlider,
  afterMoveSlider,
  onAdjustBounds,
  afterAdjustBounds,
#if GTK_CHECK_VERSION(2,6,0)
  onRangeChangeValue,
  afterRangeChangeValue,
#endif
  onRangeValueChanged,
  afterRangeValueChanged
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums    (ScrollType(..))
#if GTK_MAJOR_VERSION < 3
import Graphics.UI.Gtk.General.Enums    (UpdateType(..))
#endif
import Graphics.UI.Gtk.General.Structs  (Rectangle(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Get the 'Adjustment' which is the \"model\" object for 'Range'. See
-- 'rangeSetAdjustment' for details.
--
rangeGetAdjustment :: RangeClass self => self
 -> IO Adjustment -- ^ returns a 'Adjustment'
rangeGetAdjustment self =
  makeNewObject mkAdjustment $
  {# call unsafe range_get_adjustment #}
    (toRange self)

-- | Sets the adjustment to be used as the \"model\" object for this range
-- widget. The adjustment indicates the current range value, the minimum and
-- maximum range values, the step\/page increments used for keybindings and
-- scrolling, and the page size. The page size is normally 0 for 'Scale' and
-- nonzero for 'Scrollbar', and indicates the size of the visible area of the
-- widget being scrolled. The page size affects the size of the scrollbar
-- slider.
--
rangeSetAdjustment :: RangeClass self => self
 -> Adjustment -- ^ @adjustment@ - a 'Adjustment'
 -> IO ()
rangeSetAdjustment self adjustment =
  {# call range_set_adjustment #}
    (toRange self)
    adjustment

#if GTK_MAJOR_VERSION < 3
-- | Gets the update policy of @range@. See 'rangeSetUpdatePolicy'.
--
-- Removed in Gtk3.
rangeGetUpdatePolicy :: RangeClass self => self
 -> IO UpdateType -- ^ returns the current update policy
rangeGetUpdatePolicy self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe range_get_update_policy #}
    (toRange self)


-- | Sets the update policy for the range. 'UpdateContinuous' means that
-- anytime the range slider is moved, the range value will change and the
-- value_changed signal will be emitted. 'UpdateDelayed' means that the value
-- will be updated after a brief timeout where no slider motion occurs, so
-- updates are spaced by a short time rather than continuous.
-- 'UpdateDiscontinuous' means that the value will only be updated when the
-- user releases the button and ends the slider drag operation.
--
-- Removed in Gtk3.
rangeSetUpdatePolicy :: RangeClass self => self
 -> UpdateType -- ^ @policy@ - update policy
 -> IO ()
rangeSetUpdatePolicy self policy =
  {# call range_set_update_policy #}
    (toRange self)
    ((fromIntegral . fromEnum) policy)
#endif

-- | Gets the value set by 'rangeSetInverted'.
--
rangeGetInverted :: RangeClass self => self
 -> IO Bool -- ^ returns @True@ if the range is inverted
rangeGetInverted self =
  liftM toBool $
  {# call unsafe range_get_inverted #}
    (toRange self)

-- | Ranges normally move from lower to higher values as the slider moves from
-- top to bottom or left to right. Inverted ranges have higher values at the
-- top or on the right rather than on the bottom or left.
--
rangeSetInverted :: RangeClass self => self
 -> Bool  -- ^ @setting@ - @True@ to invert the range
 -> IO ()
rangeSetInverted self setting =
  {# call range_set_inverted #}
    (toRange self)
    (fromBool setting)

-- | Gets the current value of the range.
--
rangeGetValue :: RangeClass self => self
 -> IO Double -- ^ returns current value of the range.
rangeGetValue self =
  liftM realToFrac $
  {# call unsafe range_get_value #}
    (toRange self)

-- | Sets the current value of the range; if the value is outside the minimum
-- or maximum range values, it will be clamped to fit inside them. The range
-- emits the 'valueChanged' signal if the value changes.
--
rangeSetValue :: RangeClass self => self
 -> Double -- ^ @value@ - new value of the range
 -> IO ()
rangeSetValue self value =
  {# call range_set_value #}
    (toRange self)
    (realToFrac value)

-- | Sets the step and page sizes for the range. The step size is used when
-- the user clicks the 'Scrollbar' arrows or moves 'Scale' via arrow keys. The
-- page size is used for example when moving via Page Up or Page Down keys.
--
rangeSetIncrements :: RangeClass self => self
 -> Double -- ^ @step@ - step size
 -> Double -- ^ @page@ - page size
 -> IO ()
rangeSetIncrements self step page =
  {# call range_set_increments #}
    (toRange self)
    (realToFrac step)
    (realToFrac page)

-- | Sets the allowable values in the 'Range', and clamps the range value to
-- be between @min@ and @max@. (If the range has a non-zero page size, it is
-- clamped between @min@ and @max@ - page-size.)
--
rangeSetRange :: RangeClass self => self
 -> Double -- ^ @min@ - minimum range value
 -> Double -- ^ @max@ - maximum range value
 -> IO ()
rangeSetRange self min max =
  {# call range_set_range #}
    (toRange self)
    (realToFrac min)
    (realToFrac max)

#if GTK_CHECK_VERSION(2,10,0)

-- | Determines how Gtk+ handles the sensitivity of stepper arrows at the end of range widgets.
--
-- * 'SensitivityAuto': the arrow is made insensitive if the thumb is at the end
--
-- * 'SensitivityOn': the arrow is alwasy sensitive
--
-- * 'SensitivityOff': the arrow is always insensitive
--
{#enum SensitivityType {underscoreToCase} deriving (Bounded,Eq,Show)#}

-- %hash c:3a8d d:d336
-- | Sets the sensitivity policy for the stepper that points to the \'lower\'
-- end of the 'Range''s adjustment.
--
-- * Available since Gtk+ version 2.10
--
rangeSetLowerStepperSensitivity :: RangeClass self => self
 -> SensitivityType -- ^ @sensitivity@ - the lower stepper's sensitivity
                    -- policy.
 -> IO ()
rangeSetLowerStepperSensitivity self sensitivity =
  {# call gtk_range_set_lower_stepper_sensitivity #}
    (toRange self)
    ((fromIntegral . fromEnum) sensitivity)

-- %hash c:12a2 d:2f2a
-- | Gets the sensitivity policy for the stepper that points to the \'lower\'
-- end of the 'Range''s adjustment.
--
-- * Available since Gtk+ version 2.10
--
rangeGetLowerStepperSensitivity :: RangeClass self => self
 -> IO SensitivityType -- ^ returns The lower stepper's sensitivity policy.
rangeGetLowerStepperSensitivity self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_range_get_lower_stepper_sensitivity #}
    (toRange self)

-- %hash c:a939 d:2d79
-- | Sets the sensitivity policy for the stepper that points to the \'upper\'
-- end of the 'Range''s adjustment.
--
-- * Available since Gtk+ version 2.10
--
rangeSetUpperStepperSensitivity :: RangeClass self => self
 -> SensitivityType -- ^ @sensitivity@ - the upper stepper's sensitivity
                    -- policy.
 -> IO ()
rangeSetUpperStepperSensitivity self sensitivity =
  {# call gtk_range_set_upper_stepper_sensitivity #}
    (toRange self)
    ((fromIntegral . fromEnum) sensitivity)

-- %hash c:456e d:896d
-- | Gets the sensitivity policy for the stepper that points to the \'upper\'
-- end of the 'Range''s adjustment.
--
-- * Available since Gtk+ version 2.10
--
rangeGetUpperStepperSensitivity :: RangeClass self => self
 -> IO SensitivityType -- ^ returns The upper stepper's sensitivity policy.
rangeGetUpperStepperSensitivity self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_range_get_upper_stepper_sensitivity #}
    (toRange self)
#endif
#if GTK_CHECK_VERSION(2,20,0)
-- | This function is useful mainly for 'Range' subclasses.
--
-- See 'rangeSetMinSliderSize'.
rangeGetMinSliderSize :: RangeClass self => self
                      -> IO Int  -- ^ returns The minimum size of the range's slider.
rangeGetMinSliderSize range =
  liftM fromIntegral $
  {#call gtk_range_get_min_slider_size #}
    (toRange range)

-- | This function returns the area that contains the range's trough and its steppers, in 'DrawWindow'
-- coordinates.
--
-- This function is useful mainly for 'Range' subclasses.
rangeGetRangeRect :: RangeClass self => self
                  -> IO Rectangle
rangeGetRangeRect self =
  alloca $ \rPtr -> do
  {# call gtk_range_get_range_rect #}
    (toRange self)
    (castPtr rPtr)
  peek rPtr

-- | This function returns sliders range along the long dimension, in 'DrawWindow' coordinates.
--
-- This function is useful mainly for 'Range' subclasses.
rangeGetSliderRange :: RangeClass self => self
                    -> IO (Maybe (Int, Int))
rangeGetSliderRange range =
    alloca $ \ startPtr ->
    alloca $ \ endPtr -> do
      {#call gtk_range_get_slider_range #}
        (toRange range)
        startPtr
        endPtr
      if (startPtr /= nullPtr && endPtr /= nullPtr)
         then do
           start <- peek startPtr
           end <- peek endPtr
           return (Just (fromIntegral start, fromIntegral end))
         else return Nothing

-- | This function is useful mainly for 'Range' subclasses.
--
-- See 'rangeSetSliderSizeFixed'.
rangeGetSliderSizeFixed :: RangeClass self => self
                        -> IO Bool  -- ^ returns whether the range's slider has a fixed size.
rangeGetSliderSizeFixed self =
  liftM toBool $
  {#call gtk_range_get_slider_size_fixed #}
    (toRange self)

-- | Sets the minimum size of the range's slider.
--
-- This function is useful mainly for 'Range' subclasses.
rangeSetMinSliderSize :: RangeClass self => self
                      -> Bool
                      -> IO ()
rangeSetMinSliderSize self minSize =
  {#call gtk_range_set_min_slider_size #}
    (toRange self)
    (fromBool minSize)

-- | Sets whether the range's slider has a fixed size, or a size that depends on it's adjustment's page
-- size.
--
-- This function is useful mainly for 'Range' subclasses.
rangeSetSliderSizeFixed :: RangeClass self => self
                        -> Bool -- ^ @sizeFixed@ 'True' to make the slider size constant
                        -> IO ()
rangeSetSliderSizeFixed self sizeFixed =
  {#call gtk_range_set_slider_size_fixed #}
    (toRange self)
    (fromBool sizeFixed)
#endif

--------------------
-- Attributes

#if GTK_MAJOR_VERSION < 3
-- | How the range should be updated on the screen.
--
-- Default value: 'UpdateContinuous'
--
-- Removed in Gtk3.
rangeUpdatePolicy :: RangeClass self => Attr self UpdateType
rangeUpdatePolicy = newAttr
  rangeGetUpdatePolicy
  rangeSetUpdatePolicy
#endif

-- | The 'Adjustment' that contains the current value of this range object.
--
rangeAdjustment :: RangeClass self => Attr self Adjustment
rangeAdjustment = newAttr
  rangeGetAdjustment
  rangeSetAdjustment

-- | Invert direction slider moves to increase range value.
--
-- Default value: @False@
--
rangeInverted :: RangeClass self => Attr self Bool
rangeInverted = newAttr
  rangeGetInverted
  rangeSetInverted

#if GTK_CHECK_VERSION(2,10,0)
-- %hash c:b6dd d:1607
-- | The sensitivity policy for the stepper that points to the adjustment's
-- lower side.
--
-- Default value: 'SensitivityAuto'
--
rangeLowerStepperSensitivity :: RangeClass self => Attr self SensitivityType
rangeLowerStepperSensitivity = newAttrFromEnumProperty "lower-stepper-sensitivity"
                                 {# call pure unsafe gtk_sensitivity_type_get_type #}

-- %hash c:2fc6 d:132a
-- | The sensitivity policy for the stepper that points to the adjustment's
-- upper side.
--
-- Default value: 'SensitivityAuto'
--
rangeUpperStepperSensitivity :: RangeClass self => Attr self SensitivityType
rangeUpperStepperSensitivity = newAttrFromEnumProperty "upper-stepper-sensitivity"
                                 {# call pure unsafe gtk_sensitivity_type_get_type #}
#endif

-- %hash c:f615 d:2481
-- | \'value\' property. See 'rangeGetValue' and 'rangeSetValue'
--
rangeValue :: RangeClass self => Attr self Double
rangeValue = newAttr
  rangeGetValue
  rangeSetValue

#if GTK_CHECK_VERSION(2,20,0)
-- | Wheter range's slikder has a fixed size, or a size that depends on it's adjustment's page size.
rangeSliderSizeFixed :: RangeClass self => Attr self Bool
rangeSliderSizeFixed = newAttr
  rangeGetSliderSizeFixed
  rangeSetSliderSizeFixed

-- | Get\/Set sliders range along the long dimension, in 'DrawWindow' coordinates.
rangeMinSliderSize :: RangeClass self => ReadWriteAttr self Int Bool
rangeMinSliderSize = newAttr
  rangeGetMinSliderSize
  rangeSetMinSliderSize
#endif

--------------------
-- Signals

-- %hash c:9758 d:680f
-- | Emitted when the range value changes.
--
valueChanged :: RangeClass self => Signal self (IO ())
valueChanged = Signal (connect_NONE__NONE "value-changed")

-- %hash c:9576 d:af3f
-- |
--
adjustBounds :: RangeClass self => Signal self (Double -> IO ())
adjustBounds = Signal (connect_DOUBLE__NONE "adjust-bounds")

#if GTK_CHECK_VERSION(2,6,0)
-- %hash c:a84 d:a60c
-- | The 'changeValue' signal is emitted when a scroll action is performed on
-- a range. It allows an application to determine the type of scroll event that
-- occurred and the resultant new value. The application can handle the event
-- itself and return @True@ to prevent further processing. Or, by returning
-- @False@, it can pass the event to other handlers until the default Gtk+
-- handler is reached.
--
-- The value parameter is unrounded. An application that overrides the
-- 'changeValue' signal is responsible for clamping the value to the desired
-- number of decimal digits.
--
-- It is not possible to use delayed update policies in an overridden
-- 'changeValue' handler.
--
-- * Available since Gtk+ version 2.6
--
changeValue :: RangeClass self => Signal self (ScrollType -> Double -> IO Bool)
changeValue = Signal (connect_ENUM_DOUBLE__BOOL "change-value")
#endif

--------------------
-- Deprecated Signals

#ifndef DISABLE_DEPRECATED

#if GTK_CHECK_VERSION(2,6,0)
-- | Emitted when a scroll action is performed on a range. It allows
--   an application to determine the type of scroll event that
--   occurred and the resultant new value. The application can handle
--   the event itself and return 'True' to prevent further
--   processing. Or, by returning 'False', it can pass the event to
--   other handlers until the default GTK+ handler is reached.
--
--   * Since Gtk 2.6
--
onRangeChangeValue, afterRangeChangeValue :: RangeClass self => self
 -> (ScrollType -> Double -> IO Bool)
 -> IO (ConnectId self)
onRangeChangeValue = connect_ENUM_DOUBLE__BOOL "change_value" False
afterRangeChangeValue = connect_ENUM_DOUBLE__BOOL "change_value" True
#endif

-- | Emitted when the range value is changed either programmatically or by
-- user action.
--
onRangeValueChanged, afterRangeValueChanged :: RangeClass self => self
 -> IO ()
 -> IO (ConnectId self)
onRangeValueChanged = connect_NONE__NONE "value_changed" False
afterRangeValueChanged = connect_NONE__NONE "value_changed" True

-- | Emitted when the range is adjusted by user action. Note the value can be
-- outside the bounds of the range since it depends on the mouse position.
--
-- Usually you should use 'onRangeValueChanged' \/ 'afterRangeValueChanged'
-- instead.
--
onAdjustBounds, afterAdjustBounds :: RangeClass self => self
 -> (Double -> IO ())
 -> IO (ConnectId self)
onAdjustBounds = connect_DOUBLE__NONE "adjust_bounds" False
afterAdjustBounds = connect_DOUBLE__NONE "adjust_bounds" True

-- | Emitted when the user presses a key (e.g. Page Up, Home, Right Arrow) to
-- move the slider. The 'ScrollType' parameter gives the key that was pressed.
--
-- Usually you should use 'onRangeValueChanged' \/
-- 'afterRangeValueChanged' instead.
--
onMoveSlider, afterMoveSlider :: RangeClass self => self
 -> (ScrollType -> IO ())
 -> IO (ConnectId self)
onMoveSlider = connect_ENUM__NONE "move_slider" False
afterMoveSlider = connect_ENUM__NONE "move_slider" True
#endif
