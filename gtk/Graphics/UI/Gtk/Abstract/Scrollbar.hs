-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Scrollbar
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/03/14 23:55:07 $
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
-- Base class for 'HScrollbar' and 'VScrollbar'
--
module Graphics.UI.Gtk.Abstract.Scrollbar (
-- * Detail
-- 
-- | The 'Scrollbar' widget is an abstract base class for 'HScrollbar' and
-- 'VScrollbar'. It is not very useful in itself.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Range'
-- |                     +----Scrollbar
-- |                           +----'HScrollbar'
-- |                           +----'VScrollbar'
-- @

-- * Types
  Scrollbar,
  ScrollbarClass,
  castToScrollbar
  ) where

import Graphics.UI.Gtk.Types (Scrollbar, ScrollbarClass, castToScrollbar)

