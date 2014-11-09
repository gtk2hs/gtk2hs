{-# OPTIONS_HADDOCK hide #-}
-- -*-haskell-*-
--  Internal functions for the GIMP Toolkit (GTK) Binding for Haskell
--
--  Author : Axel Simon
--
--  Copyright (C) 2010 Axel Simon
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
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- This file contains functions that are needed by other library wrappers that build
-- on Gtk2Hs. An application should not need this function nor include this file.
--
module Graphics.UI.GtkInternals (
  module Graphics.UI.Gtk.Types,
  module Graphics.UI.Gtk.General.DNDTypes,
  module Graphics.UI.Gtk.Multiline.Types,
  ) where

{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.General.DNDTypes#} (mkTargetList)
{#import Graphics.UI.Gtk.Multiline.Types#}
