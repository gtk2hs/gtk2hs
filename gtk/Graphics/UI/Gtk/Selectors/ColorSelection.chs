-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ColorSelection
--
--  Author : Duncan Coutts
--
--  Created: 2 August 2004
--
--  Version $Revision: 1.4 $ from $Date: 2005/02/25 22:53:42 $
--
--  Copyright (C) 2004-2005 Duncan Coutts
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
-- A widget used to select a color.
--
module Graphics.UI.Gtk.Selectors.ColorSelection (
-- * Description
-- 
-- | The 'ColorSelection' is a widget that is used to select a color. It
-- consists of a color wheel and number of sliders and entry boxes for color
-- parameters such as hue, saturation, value, red, green, blue, and opacity. It
-- is found on the standard color selection dialog box 'ColorSelectionDialog'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Box'
-- |                           +----'VBox'
-- |                                 +----ColorSelection
-- @

-- * Types
  ColorSelection,
  ColorSelectionClass,
  castToColorSelection,

-- * Constructors
  colorSelectionNew,

-- * Methods
  colorSelectionGetCurrentAlpha,
  colorSelectionSetCurrentAlpha,
  colorSelectionGetCurrentColor,
  colorSelectionSetCurrentColor,
  colorSelectionGetHasOpacityControl,
  colorSelectionSetHasOpacityControl,
  colorSelectionGetHasPalette,
  colorSelectionSetHasPalette,
  colorSelectionGetPreviousAlpha,
  colorSelectionSetPreviousAlpha,
  colorSelectionGetPreviousColor,
  colorSelectionSetPreviousColor,
  colorSelectionIsAdjusting
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Structs (Color)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new ColorSelection widget.
--
colorSelectionNew :: IO ColorSelection
colorSelectionNew =
  makeNewObject mkColorSelection $ liftM castPtr $
  {#call unsafe color_selection_new#}

--------------------
-- Methods

-- | Returns the current alpha value.
--
-- * The alpha value is represented by an integer between 0 and 65535.
--
colorSelectionGetCurrentAlpha :: ColorSelectionClass obj => obj -> IO Int
colorSelectionGetCurrentAlpha obj = liftM fromIntegral $
  {#call unsafe color_selection_get_current_alpha#} (toColorSelection obj)

-- | Sets the current opacity. The first time this is called, it will also set
-- the original opacity too.
--
-- * The alpha value is represented by an integer between 0 and 65535.
--
colorSelectionSetCurrentAlpha :: ColorSelectionClass obj => obj -> Int -> IO ()
colorSelectionSetCurrentAlpha obj alpha =
  {#call color_selection_set_current_alpha#} (toColorSelection obj)
    (fromIntegral alpha)

-- | Gets the current color in the ColorSelection widget.
--
colorSelectionGetCurrentColor :: ColorSelectionClass obj => obj -> IO Color
colorSelectionGetCurrentColor obj =
  alloca $ \colorPtr -> do
  {#call unsafe color_selection_get_current_color#} (toColorSelection obj)
    (castPtr colorPtr)
  peek colorPtr

-- | Sets the current color. The first time this is called, it will also set the
-- original color too.
--
colorSelectionSetCurrentColor :: ColorSelectionClass obj => obj
                              -> Color -> IO ()
colorSelectionSetCurrentColor obj color =
  alloca $ \colorPtr -> do
  poke colorPtr color
  {#call color_selection_set_current_color#} (toColorSelection obj)
    (castPtr colorPtr)

-- | Sets the ColorSelection widget to use or not use opacity.
--
colorSelectionGetHasOpacityControl :: ColorSelectionClass obj => obj -> IO Bool
colorSelectionGetHasOpacityControl obj = liftM toBool $
  {#call unsafe color_selection_get_has_opacity_control#} (toColorSelection obj)

-- | Determines whether the ColorSelection widget has an opacity control.
--
colorSelectionSetHasOpacityControl :: ColorSelectionClass obj => obj
                                   -> Bool -> IO ()
colorSelectionSetHasOpacityControl obj hasOpacity =
  {#call color_selection_set_has_opacity_control#} (toColorSelection obj)
    (fromBool hasOpacity)

-- | Determines whether the color selector has a color palette.
--
colorSelectionGetHasPalette :: ColorSelectionClass obj => obj -> IO Bool
colorSelectionGetHasPalette obj = liftM toBool $
  {#call unsafe color_selection_get_has_palette#} (toColorSelection obj)

-- | Sets whether to show or hide the palette.
--
colorSelectionSetHasPalette :: ColorSelectionClass obj => obj -> Bool -> IO ()
colorSelectionSetHasPalette obj hasPalette =
  {#call color_selection_set_has_palette#} (toColorSelection obj)
    (fromBool hasPalette)

-- | Returns the previous alpha value.
--
colorSelectionGetPreviousAlpha :: ColorSelectionClass obj => obj -> IO Int
colorSelectionGetPreviousAlpha obj = liftM fromIntegral $
  {#call unsafe color_selection_get_previous_alpha#} (toColorSelection obj)

-- | Sets the \'previous\' alpha to the given value. 
--
-- * This function should be called with some hesitations, as it might seem
-- confusing to have that alpha change.
--
colorSelectionSetPreviousAlpha :: ColorSelectionClass obj => obj -> Int -> IO ()
colorSelectionSetPreviousAlpha obj alpha =
  {#call color_selection_set_previous_alpha#} (toColorSelection obj)
    (fromIntegral alpha)

-- | Returns the original color value.
--
colorSelectionGetPreviousColor :: ColorSelectionClass obj => obj -> IO Color
colorSelectionGetPreviousColor obj =
  alloca $ \colorPtr -> do
  {#call unsafe color_selection_get_previous_color#} (toColorSelection obj)
    (castPtr colorPtr)
  peek colorPtr

-- | Sets the \'previous\' color.
--
-- * This function should be called with some hesitations, as it might seem
-- confusing to have that color change.
--
-- * Calling 'colorSelectionSetCurrentColor' will also set this color the first
-- time it is called.
--
colorSelectionSetPreviousColor :: ColorSelectionClass obj => obj
                               -> Color -> IO ()
colorSelectionSetPreviousColor obj color =
  alloca $ \colorPtr -> do
  poke colorPtr color
  {#call color_selection_set_previous_color#} (toColorSelection obj)
    (castPtr colorPtr)

-- | Gets the current state of the widget. Returns True if the user is currently
-- dragging a color around, and False if the selection has stopped.
--
colorSelectionIsAdjusting :: ColorSelectionClass obj => obj -> IO Bool
colorSelectionIsAdjusting obj = liftM toBool $
  {#call unsafe color_selection_is_adjusting#} (toColorSelection obj)
