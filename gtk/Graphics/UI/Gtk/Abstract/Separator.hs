-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Separator
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
-- Base class for 'Graphics.UI.Gtk.Ornaments.HSeparator' and
-- 'Graphics.UI.Gtk.Ornaments.VSeparator'.
--
module Graphics.UI.Gtk.Abstract.Separator (
-- * Detail
--
-- | The 'Separator' widget is an abstract class, used only for deriving the
-- subclasses 'Graphics.UI.Gtk.Ornaments.HSeparator' and
-- 'Graphics.UI.Gtk.Ornaments.VSeparator'.

-- * Class Hierarchy
-- |
-- @
-- |  'System.Glib.GObject'
-- |   +----'Graphics.UI.Gtk.Abstract.Object'
-- |         +----'Graphics.UI.Gtk.Abstract.Widget'
-- |               +----Separator
-- |                     +----'Graphics.UI.Gtk.Ornaments.HSeparator'
-- |                     +----'Graphics.UI.Gtk.Ornaments.VSeparator'
-- @

-- * Types
  Separator,
  SeparatorClass,
  castToSeparator, gTypeSeparator,
  toSeparator,
  ) where

import Graphics.UI.Gtk.Types
