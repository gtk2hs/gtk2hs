{-# LANGUAGE CPP #-}
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
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A persistent index into a tree model.
--
module Graphics.UI.Gtk.ModelView.TreeRowReference (
-- * Detail
--
-- | A 'RowReference' is an index into a
-- 'Graphics.UI.Gtk.ModelView.TreeModel.TreeModel' that is persistent even if
-- rows are inserted, deleted or reordered.
--

-- * Types
  TreeRowReference,

-- * Constructors
  treeRowReferenceNew,

-- * Methods
  treeRowReferenceGetPath,
  treeRowReferenceValid,
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.ModelView.Types#}

{# context lib="gtk" prefix="gtk" #}

-- | Tree Row Reference : like a 'TreePath' it points to a subtree or node, but
-- it is persistent. It identifies the same node (so long as it exists) even
-- when items are added, removed, or reordered.
--
{#pointer * TreeRowReference foreign newtype#}

--------------------
-- Constructors

-- | Creates a row reference based on a path. This reference will keep pointing
-- to the node pointed to by the given path, so long as it exists. Returns @Nothing@ if there is no node at the given path.
--
treeRowReferenceNew :: TreeModelClass self => self
 -> TreePath
 -> IO (Maybe TreeRowReference)
treeRowReferenceNew self path = withTreePath path $ \path -> do
  rowRefPtr <-
    {#call gtk_tree_row_reference_new#} (toTreeModel self) path
  if rowRefPtr==nullPtr then return Nothing else
    liftM (Just . TreeRowReference) $
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
  >>= fromTreePath -- path must be freed

-- | Returns True if the reference refers to a current valid path.
--
treeRowReferenceValid :: TreeRowReference -> IO Bool
treeRowReferenceValid self =
  liftM toBool $
  {# call unsafe tree_row_reference_valid #}
    self

foreign import ccall unsafe "&gtk_tree_row_reference_free"
  tree_row_reference_free :: FinalizerPtr TreeRowReference

