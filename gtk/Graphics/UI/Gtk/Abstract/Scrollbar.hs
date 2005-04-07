-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Scrollbar
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Version $Revision: 1.4 $ from $Date: 2005/04/07 00:13:59 $
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
--
-- The position of the thumb in a scrollbar is controlled by the scroll
-- adjustments. See 'Adjustment' for the fields in an adjustment - for
-- 'Scrollbar', the \"value\" field represents the position of the scrollbar,
-- which must be between the \"lower\" field and \"upper - page_size.\" The
-- \"page_size\" field represents the size of the visible scrollable area. The
-- \"step_increment\" and \"page_increment\" fields are used when the user asks
-- to step down (using the small stepper arrows) or page down (using for
-- example the PageDown key).

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

