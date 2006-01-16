-- -*-haskell-*-
--  GIMP Toolkit (GTK) CustomStore TreeModel
--
--  Author : Duncan Coutts
--
--  Created: 19 Sep 2005
--
--  Version $Revision: 1.1 $ from $Date: 2005/12/08 18:12:43 $
--
--  Copyright (C) 2005 Duncan Coutts
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
-- Maintainer  : custom-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Allows a custom data structure to be used with the 'TreeView'
--
module Graphics.UI.Gtk.TreeList.CustomStore (
  CustomStore(..),
  customStoreNew,
  ) where

import Monad	(liftM, when)

import System.Glib.FFI			hiding	(maybeNull)
import System.Glib.Flags
import System.Glib.GObject			(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.TreeList.TreeModel#}
{#import Graphics.UI.Gtk.TreeList.TreeIter#}
{#import Graphics.UI.Gtk.TreeList.TreePath#}
import System.Glib.StoreValue			(TMType(..), GenericValue(..)
						,valueSetGenericValue)
{#import System.Glib.GValue#}			(GValue(GValue), allocaGValue)
{#import System.Glib.GType#}			(GType)
import System.Glib.GValueTypes                  (valueSetString)

{# context lib="gtk" prefix="gtk" #}

data CustomStore = CustomStore {
    customStoreGetFlags      :: IO [TreeModelFlags],
    customStoreGetNColumns   :: IO Int,
    customStoreGetColumnType :: Int -> IO GType,
    customStoreGetIter       :: TreePath -> IO (Maybe TreeIter),              -- convert a path to an iterator
    customStoreGetPath       :: TreeIter -> IO TreePath,                      -- convert an interator to a path
    customStoreGetValue      :: TreeIter -> Int -> GValue -> IO (),           -- get the value at an iter and column
    customStoreIterNext      :: TreeIter -> IO (Maybe TreeIter),              -- following row (if any)
    customStoreIterChildren  :: Maybe TreeIter -> IO (Maybe TreeIter),        -- first child row (if any)
    customStoreIterHasChild  :: TreeIter -> IO Bool,                          -- row has any children at all
    customStoreIterNChildren :: Maybe TreeIter -> IO Int,                     -- number of children of a row
    customStoreIterNthChild  :: Maybe TreeIter -> Int -> IO (Maybe TreeIter), -- nth child row of a given row
    customStoreIterParent    :: TreeIter -> IO (Maybe TreeIter),              -- parent row of a row
    customStoreRefNode       :: TreeIter -> IO (),                            -- caching hint
    customStoreUnrefNode     :: TreeIter -> IO ()                             -- caching hint
  }

customStoreGetFlags_static :: StablePtr CustomStore -> IO CInt
customStoreGetFlags_static storePtr = do
  store <- deRefStablePtr storePtr
  liftM (fromIntegral . fromFlags) $ customStoreGetFlags store

foreign export ccall "gtk2hs_store_get_flags_impl"
  customStoreGetFlags_static :: StablePtr CustomStore -> IO CInt


customStoreGetNColumns_static :: StablePtr CustomStore -> IO CInt
customStoreGetNColumns_static storePtr = do
  store <- deRefStablePtr storePtr
  liftM fromIntegral $ customStoreGetNColumns store

foreign export ccall "gtk2hs_store_get_n_columns_impl"
  customStoreGetNColumns_static :: StablePtr CustomStore -> IO CInt


customStoreGetColumnType_static :: StablePtr CustomStore -> CInt -> IO GType
customStoreGetColumnType_static storePtr column = do
  store <- deRefStablePtr storePtr
  customStoreGetColumnType store (fromIntegral column)

foreign export ccall "gtk2hs_store_get_column_type_impl"
  customStoreGetColumnType_static :: StablePtr CustomStore -> CInt -> IO GType


customStoreGetIter_static :: StablePtr CustomStore -> Ptr TreeIter -> Ptr NativeTreePath -> IO CInt
customStoreGetIter_static storePtr iterPtr pathPtr = do
  store <- deRefStablePtr storePtr
  path <- peekTreePath pathPtr
  iter <- customStoreGetIter store path
  case iter of
    Nothing   -> return (fromBool False)
    Just iter -> do poke iterPtr iter
                    return (fromBool True)

foreign export ccall "gtk2hs_store_get_iter_impl"
  customStoreGetIter_static :: StablePtr CustomStore -> Ptr TreeIter -> Ptr NativeTreePath -> IO CInt


customStoreGetPath_static :: StablePtr CustomStore -> Ptr TreeIter -> IO (Ptr NativeTreePath)
customStoreGetPath_static storePtr iterPtr = do
  store <- deRefStablePtr storePtr
  iter <- peek iterPtr
  path <- customStoreGetPath store iter
  NativeTreePath pathPtr <- newTreePath path
  return pathPtr

foreign export ccall "gtk2hs_store_get_path_impl"
  customStoreGetPath_static :: StablePtr CustomStore -> Ptr TreeIter -> IO (Ptr NativeTreePath)


customStoreGetValue_static :: StablePtr CustomStore -> Ptr TreeIter -> CInt -> Ptr GValue -> IO ()
customStoreGetValue_static storePtr iterPtr column gvaluePtr = do
  store <- deRefStablePtr storePtr
  iter <- peek iterPtr
  customStoreGetValue store iter (fromIntegral column) (GValue gvaluePtr)

foreign export ccall "gtk2hs_store_get_value_impl"
  customStoreGetValue_static :: StablePtr CustomStore -> Ptr TreeIter -> CInt -> Ptr GValue -> IO ()


customStoreIterNext_static :: StablePtr CustomStore -> Ptr TreeIter -> IO CInt
customStoreIterNext_static storePtr iterPtr = do
  store <- deRefStablePtr storePtr
  iter <- peek iterPtr
  iter' <- customStoreIterNext store iter
  case iter' of
    Nothing    -> return (fromBool False)
    Just iter' -> do poke iterPtr iter'
                     return (fromBool True)

foreign export ccall "gtk2hs_store_iter_next_impl"
  customStoreIterNext_static :: StablePtr CustomStore -> Ptr TreeIter -> IO CInt


customStoreIterChildren_static :: StablePtr CustomStore -> Ptr TreeIter -> Ptr TreeIter -> IO CInt
customStoreIterChildren_static storePtr iterPtr parentIterPtr = do
  store <- deRefStablePtr storePtr
  parentIter <- maybeNull peek parentIterPtr
  iter <- customStoreIterChildren store parentIter
  case iter of
    Nothing   -> return (fromBool False)
    Just iter -> do poke iterPtr iter
                    return (fromBool True)

foreign export ccall "gtk2hs_store_iter_children_impl"
  customStoreIterChildren_static :: StablePtr CustomStore -> Ptr TreeIter -> Ptr TreeIter -> IO CInt


customStoreIterHasChild_static :: StablePtr CustomStore -> Ptr TreeIter -> IO CInt
customStoreIterHasChild_static storePtr iterPtr = do
  store <- deRefStablePtr storePtr
  iter <- peek iterPtr
  liftM fromBool $ customStoreIterHasChild store iter

foreign export ccall "gtk2hs_store_iter_has_child_impl"
  customStoreIterHasChild_static :: StablePtr CustomStore -> Ptr TreeIter -> IO CInt


customStoreIterNChildren_static :: StablePtr CustomStore -> Ptr TreeIter -> IO CInt
customStoreIterNChildren_static storePtr iterPtr = do
  store <- deRefStablePtr storePtr
  iter <- maybeNull peek iterPtr
  liftM fromIntegral $ customStoreIterNChildren store iter

foreign export ccall "gtk2hs_store_iter_n_children_impl"
  customStoreIterNChildren_static :: StablePtr CustomStore -> Ptr TreeIter -> IO CInt


customStoreIterNthChild_static :: StablePtr CustomStore -> Ptr TreeIter -> Ptr TreeIter -> CInt -> IO CInt
customStoreIterNthChild_static storePtr iterPtr parentIterPtr n = do
  store <- deRefStablePtr storePtr
  parentIter <- maybeNull peek parentIterPtr
  iter <- customStoreIterNthChild store parentIter (fromIntegral n)
  case iter of
    Nothing   -> return (fromBool False)
    Just iter -> do poke iterPtr iter
                    return (fromBool True)

foreign export ccall "gtk2hs_store_iter_nth_child_impl"
  customStoreIterNthChild_static :: StablePtr CustomStore -> Ptr TreeIter -> Ptr TreeIter -> CInt -> IO CInt


customStoreIterParent_static :: StablePtr CustomStore -> Ptr TreeIter -> Ptr TreeIter -> IO CInt
customStoreIterParent_static  storePtr iterPtr childIterPtr = do
  store <- deRefStablePtr storePtr
  childIter <- peek childIterPtr
  iter <- customStoreIterParent store childIter
  case iter of
    Nothing   -> return (fromBool False)
    Just iter -> do poke iterPtr iter
                    return (fromBool True)

foreign export ccall "gtk2hs_store_iter_parent_impl"
  customStoreIterParent_static :: StablePtr CustomStore -> Ptr TreeIter -> Ptr TreeIter -> IO CInt


customStoreRefNode_static :: StablePtr CustomStore -> Ptr TreeIter -> IO ()
customStoreRefNode_static storePtr iterPtr = do
  store <- deRefStablePtr storePtr
  iter <- peek iterPtr
  customStoreRefNode store iter

foreign export ccall "gtk2hs_store_ref_node_impl"
  customStoreRefNode_static :: StablePtr CustomStore -> Ptr TreeIter -> IO ()


customStoreUnrefNode_static :: StablePtr CustomStore -> Ptr TreeIter -> IO ()
customStoreUnrefNode_static storePtr iterPtr = do
  store <- deRefStablePtr storePtr
  iter <- peek iterPtr
  customStoreUnrefNode store iter

foreign export ccall "gtk2hs_store_unref_node_impl"
  customStoreUnrefNode_static :: StablePtr CustomStore -> Ptr TreeIter -> IO ()

foreign import ccall unsafe "gtk2hs_store_new"
  gtk2hs_store_new :: StablePtr CustomStore -> IO (Ptr TreeModel)

customStoreNew :: CustomStore -> IO TreeModel
customStoreNew impl = do
  implPtr <- newStablePtr impl
  makeNewGObject mkTreeModel $
    gtk2hs_store_new implPtr

maybeNull :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
maybeNull marshal ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = liftM Just (marshal ptr)
