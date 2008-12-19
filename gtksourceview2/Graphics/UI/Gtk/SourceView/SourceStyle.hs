-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SourceView
--
--  Author : Peter Gavin
--  derived from sourceview bindings by Axel Simon and Duncan Coutts
--
--  Created: 18 December 2008
--
--  Copyright (C) 2004-2008 Peter Gavin, Duncan Coutts, Axel Simon
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
module Graphics.UI.Gtk.SourceView.SourceStyle (
  SourceStyle(..),
  ) where

data SourceStyle =
  SourceStyle
  { sourceStyleBackground     :: Maybe String
  , sourceStyleBold           :: Maybe Bool
  , sourceStyleForeground     :: Maybe String
  , sourceStyleItalic         :: Maybe Bool
  , sourceStyleLineBackground :: Maybe String
  , sourceStyleStrikethrough  :: Maybe Bool
  , sourceStyleUnderline      :: Maybe Bool
  }
