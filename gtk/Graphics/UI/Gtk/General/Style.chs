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
  castToStyle,
  toStyle,

-- * Methods
  styleGetForeground,
  styleGetBackground,
  styleGetLight,
  styleGetMiddle,
  styleGetDark,
  styleGetText,
  styleGetBase,
  styleGetAntiAliasing
  ) where

{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.General.Structs		(styleGetForeground,
			 styleGetBackground,
			 styleGetLight,
			 styleGetMiddle,
			 styleGetDark,
			 styleGetText,
			 styleGetBase,
			 styleGetAntiAliasing)

