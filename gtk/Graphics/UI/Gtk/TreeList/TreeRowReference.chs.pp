-- -*-haskell-*-
--  GIMP Toolkit (GTK) Class TreeRowReference
--
--  Author : Duncan Coutts
--
--  Created: 14 April 2005
--
--  Copyright (C) 2005 Axel Simon, Duncan Coutts
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
-- 
--
module Graphics.UI.Gtk.TreeList.TreeRowReference (

-- * Types
  TreeRowReference,

-- * Constructors
  treeRowReferenceNew,

-- * Methods
  treeRowReferenceGetPath,
  treeRowReferenceValid,
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.TreeList.TreePath#}

{# context lib="gtk" prefix="gtk" #}

-- | Tree Row Reference : like a 'TreePath' it points to a subtree or node, but
-- it is persistent. It identifies the same node (so long as it exists) even
-- when items are added, removed, or reordered.
--
{#pointer * TreeRowReference foreign newtype#}

--------------------
-- Constructors

-- | Creates a row reference based on a path. This reference will keep pointing
-- to the node pointed to by the given path, so long as it exists.
--
treeRowReferenceNew :: TreeModelClass self => self
 -> NativeTreePath
 -> IO TreeRowReference
treeRowReferenceNew self path = do
  rowRefPtr <- throwIfNull "treeRowReferenceNew: invalid path given" $
    {#call unsafe gtk_tree_row_reference_new#} (toTreeModel self) path
  liftM TreeRowReference $
    newForeignPtr rowRefPtr tree_row_reference_free

--------------------
-- Methods

-- | Returns a path that the row reference currently points to.
--
-- * The returned path may be the empty list if the reference was invalid.
--
treeRowReferenceGetPath :: TreeRowReference -> IO TreePath
treeRowReferenceGetPath ref =
  {# call unsafe tree_row_reference_get_path #} ref
  >>= fromTreePath

-- | Returns True if the reference refers to a current valid path.
--
treeRowReferenceValid :: TreeRowReference -> IO Bool
treeRowReferenceValid self =
  liftM toBool $
  {# call unsafe tree_row_reference_valid #}
    self

foreign import ccall unsafe "&gtk_tree_row_reference_free"
  tree_row_reference_free :: FinalizerPtr TreeRowReference

