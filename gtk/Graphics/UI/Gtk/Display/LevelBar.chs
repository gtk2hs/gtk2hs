{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget LevelBar
--
--  Author : Vincent Hanquez
--
--  Copyright (C) 2014 Vincent Hanquez
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
-- A widget which indicates progress visually
--
module Graphics.UI.Gtk.Display.LevelBar (
-- * Detail
-- 
-- | The 'LevelBar' is typically used to display level indicator

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----LevelBar
-- @

-- * Types
#if GTK_CHECK_VERSION(3,6,0)
  LevelBar,
  LevelBarClass,
  castToLevelBar, gTypeLevelBar,
  toLevelBar,

-- * Constructors
  levelBarNew,

-- * Methods
  levelBarSetMode,
  levelBarGetMode,
  levelBarAddOffsetValue,
  levelBarRemoveOffsetValue,
  levelBarGetOffsetValue,

-- * Attributes
#if GTK_CHECK_VERSION(3,8,0)
  levelBarInverted,
#endif
  levelBarMaxValue,
  levelBarMinValue,
  levelBarValue,
#endif
  ) where

#if GTK_CHECK_VERSION(3,6,0)
import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.General.Enums (LevelBarMode(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'LevelBar'.
--
levelBarNew :: IO LevelBar
levelBarNew =
  makeNewObject mkLevelBar $
  liftM (castPtr :: Ptr Widget -> Ptr LevelBar) $
  {# call unsafe level_bar_new #}

--------------------
-- Methods

-- | Sets the value of the "mode" property.
--
levelBarSetMode :: LevelBarClass self => self -> LevelBarMode -> IO ()
levelBarSetMode self mode =
  {#call level_bar_set_mode #} (toLevelBar self) ((fromIntegral . fromEnum) mode)

-- | Returns the value of the "mode" property.
levelBarGetMode :: LevelBarClass self => self -> IO LevelBarMode
levelBarGetMode self =
  liftM (toEnum . fromIntegral) $
  {#call level_bar_get_mode #}
    (toLevelBar self)

-- | Add a new offset marker at the position specified by @value
--
levelBarAddOffsetValue :: LevelBarClass self => self
 -> String -- ^ @name@ - Offset name
 -> Double -- ^ @value@ - Offset position
 -> IO ()
levelBarAddOffsetValue self name value =
  withUTFString name $ \namePtr ->
  {# call unsafe level_bar_add_offset_value #}
    (toLevelBar self)
    namePtr
    (realToFrac value)

-- | Add a new offset marker at the position specified by @value
--
levelBarRemoveOffsetValue :: LevelBarClass self => self
 -> String
 -> IO ()
levelBarRemoveOffsetValue self name =
  withUTFString name $ \namePtr ->
  {# call unsafe level_bar_remove_offset_value #}
    (toLevelBar self)
    namePtr

-- | Returns the current fraction of the task that's been completed.
--
levelBarGetOffsetValue :: LevelBarClass self => self
 -> String
 -> IO Double -- ^ returns the value of the offset
levelBarGetOffsetValue self name =
  alloca $ \dPtr ->
  withUTFString name $ \namePtr -> do
    {# call unsafe level_bar_get_offset_value #}
      (toLevelBar self) namePtr dPtr
    liftM realToFrac $ peek dPtr

--------------------
-- Attributes

#if GTK_CHECK_VERSION(3,8,0)
-- | Level bars normally grow from top to bottom or left to right. Inverted level bars grow in the opposite direction.
--
-- Default value: FALSE
--
-- Since 3.8
levelBarInverted :: LevelBarClass self => Attr self Bool
levelBarInverted = newAttrFromBoolProperty "inverted"
#endif

-- | The "value" property determines the currently filled value of the level bar.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
levelBarValue :: LevelBarClass self => Attr self Double
levelBarValue = newAttrFromDoubleProperty "value"

-- | The "min-value" property determines the minimum value of the interval that can be displayed by the bar.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
levelBarMinValue :: LevelBarClass self => Attr self Double
levelBarMinValue = newAttrFromDoubleProperty "min-value"

-- | The "max-value" property determaxes the maximum value of the interval that can be displayed by the bar.
--
-- Allowed values: >= 0
--
-- Default value: 1
levelBarMaxValue :: LevelBarClass self => Attr self Double
levelBarMaxValue = newAttrFromDoubleProperty "max-value"

-- | The "bar-mode" property determines the way LevelBar interprets the
-- value properties to draw the level fill area. Specifically, when the value is
-- LevelBarModeContinous, LevelBar will draw a single block
-- representing the current value in that area; when the value is
-- LevelBarModeDiscrete, the widget will draw a succession of separate
-- blocks filling the draw area, with the number of blocks being equal to the
-- units separating the integral roundings of "min-value" and "max-value".
--
-- Default value: LevelBarModeContinuous
levelBarMode :: LevelBarClass self => Attr self LevelBarMode
levelBarMode = newAttr levelBarGetMode levelBarSetMode

#endif
