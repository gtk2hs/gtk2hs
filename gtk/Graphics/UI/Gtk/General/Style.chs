{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Styles
--
--  Author : Axel Simon
--
--  Created: 13 February 2003
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
-- It seems sensible to treat Styles as read only. The only way to modify
--   a style should be for the programmer to apply the RcStyle patches directly
--   to the widget.
--
-- Bind the draw... functions, they might be useful.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Customization of widgets.
--
module Graphics.UI.Gtk.General.Style (
-- * Description
--
-- | Styles are attached to widgets and determine how particular parts are
-- drawn and with what color. Thus they are should be seen as mandatory when
-- one implements a new custom widgets via 'DrawingArea'. Although the
-- parameterized drawing function don't have to be used, it is strongly
-- advisable (and more robust) to make use of the predefined graphics contexts
-- for the different states of a widget (retrieved by
-- 'Graphics.UI.Gtk.Abstract.Widget.widgetGetState').
--

-- * Types
  Style,
  StyleClass,
  castToStyle, gTypeStyle,
  toStyle,

-- * Methods
  styleGetForeground,
  styleGetBackground,
  styleGetLight,
  styleGetMiddle,
  styleGetDark,
  styleGetText,
  styleGetBase,
  styleGetAntiAliasing,

#if GTK_MAJOR_VERSION < 3
  stylePaintFlatBox,
  stylePaintLayout,
#endif

  ) where

{# context prefix ="gtk" #}

#if GTK_MAJOR_VERSION < 3
import System.Glib.FFI
{#import Graphics.Rendering.Pango.Types#}
import Graphics.Rendering.Pango.BasicTypes
import Graphics.UI.Gtk.General.Structs          (Rectangle)
import Graphics.UI.Gtk.General.Enums (StateType, ShadowType)
#endif

{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.General.Structs          (styleGetForeground,
                         styleGetBackground,
                         styleGetLight,
                         styleGetMiddle,
                         styleGetDark,
                         styleGetText,
                         styleGetBase,
                         styleGetAntiAliasing)

#if GTK_MAJOR_VERSION < 3
stylePaintFlatBox :: WidgetClass widget
                  => Style
                  -> DrawWindow
                  -> StateType
                  -> ShadowType
                  -> Rectangle
                  -> widget
                  -> String
                  -> Int -> Int -> Int -> Int
                  -> IO ()
stylePaintFlatBox style window stateType shadowType
                  clipRect widget detail x y width height =
  with clipRect $ \rectPtr ->
  withCString detail $ \detailPtr ->
  {# call paint_flat_box #}
    style
    window
    ((fromIntegral.fromEnum) stateType)
    ((fromIntegral.fromEnum) shadowType)
    (castPtr rectPtr)
    (toWidget widget)
    detailPtr
    (fromIntegral x) (fromIntegral y)
    (fromIntegral width) (fromIntegral height)

stylePaintLayout :: WidgetClass widget
                 => Style
                 -> DrawWindow
                 -> StateType
                 -> Bool
                 -> Rectangle
                 -> widget
                 -> String
                 -> Int -> Int
                 -> PangoLayout
                 -> IO ()
stylePaintLayout style window stateType useText
                  clipRect widget detail x y (PangoLayout _ layout) =
  with clipRect $ \rectPtr ->
  withCString detail $ \detailPtr ->
  {# call gtk_paint_layout #}
    style
    window
    ((fromIntegral.fromEnum) stateType)
    (fromBool useText)
    (castPtr rectPtr)
    (toWidget widget)
    detailPtr
    (fromIntegral x) (fromIntegral y)
    layout
#endif
