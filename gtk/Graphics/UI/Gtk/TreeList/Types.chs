-- -*-haskell-*-
--  GIMP Toolkit (GTK) CustomStore TreeModel
--
--  Author : Duncan Coutts
--
--  Created: 31 March 2006
--
--  Copyright (C) 2006 Duncan Coutts
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
-- Common types and classes for the TreeList modules.
--
module Graphics.UI.Gtk.TreeList.Types (
  TypedTreeModel(..),
  TypedTreeModelClass,
  toTypedTreeModel,
  ) where

import GHC.Exts (unsafeCoerce#)

import System.Glib.FFI
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

newtype TypedTreeModel row = TypedTreeModel (ForeignPtr (TypedTreeModel row))

class TypedTreeModelClass (model :: * -> *) where
toTypedTreeModel :: TypedTreeModelClass model => model row -> TypedTreeModel row
toTypedTreeModel = unsafeCoerce#

instance TypedTreeModelClass TypedTreeModel
