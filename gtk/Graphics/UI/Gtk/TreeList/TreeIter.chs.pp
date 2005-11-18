-- -*-haskell-*-
--  GIMP Toolkit (GTK) Class TreeIter
--
--  Author : Duncan Coutts
--
--  Created: 14 April 2005
--
--  Version $Revision: 1.2 $ from $Date: 2005/11/18 15:54:57 $
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
-- 
--
module Graphics.UI.Gtk.TreeList.TreeIter (

-- * Types
  TreeIter(..),

-- * Methods
  createTreeIter,
  mallocTreeIter,
  receiveTreeIter
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.General.Structs	(treeIterSize)

{# context lib="gtk" prefix="gtk" #}

-- | Tree Iterator: a pointer to an entry in a
-- 'Graphics.UI.Gtk.TreeList.TreeModel'.
--
{#pointer *TreeIter foreign newtype#}

--------------------
-- Methods

createTreeIter :: Ptr TreeIter -> IO TreeIter
createTreeIter tiPtr = do
  tiPtr' <- tree_iter_copy tiPtr
  liftM TreeIter $ newForeignPtr tiPtr' (tree_iter_free tiPtr')

mallocTreeIter :: IO TreeIter
mallocTreeIter = do
  iterPtr <- mallocBytes treeIterSize
  liftM TreeIter $ newForeignPtr iterPtr (tree_iter_free iterPtr)

receiveTreeIter :: (TreeIter -> IO Bool) -> IO (Maybe TreeIter)
receiveTreeIter body = do
  iter <- mallocTreeIter
  result <- body iter
  if result then return (Just iter)
            else return Nothing

foreign import ccall unsafe "gtk_tree_iter_copy"
  tree_iter_copy :: Ptr TreeIter -> IO (Ptr TreeIter)

#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "&gtk_tree_iter_free"
  tree_iter_free' :: FinalizerPtr TreeIter

tree_iter_free :: Ptr TreeIter -> FinalizerPtr TreeIter
tree_iter_free _ = tree_iter_free'

#else

foreign import ccall unsafe "gtk_tree_iter_free"
  tree_iter_free :: Ptr TreeIter -> IO ()

#endif
