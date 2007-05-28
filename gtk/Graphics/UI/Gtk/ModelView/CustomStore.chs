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
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Allows a custom data structure to be used with the 'TreeView'
--
module Graphics.UI.Gtk.ModelView.CustomStore (
  treeModelGetRow,

  CustomTreeModel,
  CustomTreeModelImplementation(..),
  customTreeModelNew,
  customTreeModelGetPrivate,
  customTreeModelInvalidateIters,

  -- * View notifcation functions
  treeModelRowChanged,
  treeModelRowInserted,
  treeModelRowHasChildToggled,
  treeModelRowDeleted,
  treeModelRowsReordered,
  ) where

import Control.Monad	(liftM, when)

import System.Glib.FFI			hiding	(maybeNull)
import System.Glib.Flags			(fromFlags)
import System.Glib.GObject			(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.ModelView.Types#}
{#import Graphics.UI.Gtk.ModelView.TreeModel#}
{#import Graphics.UI.Gtk.TreeList.TreeIter#}
{#import Graphics.UI.Gtk.TreeList.TreePath#}
{#import System.Glib.GValue#}			()
{#import System.Glib.GType#}			()

{# context lib="gtk" prefix="gtk" #}

-- A CustomTreeModel is backed by a Gtk2HsStore
-- which is an instance of the GtkTreeModel GInterface
-- it also stores some extra per-model-type private data
newtype CustomTreeModel private row = CustomTreeModel (ForeignPtr (CustomTreeModel private row))
instance GObjectClass (CustomTreeModel private row)
instance TreeModelClass (CustomTreeModel private row)


data CustomTreeModelImplementation row = CustomTreeModelImplementation {
    customTreeModelGetFlags      :: IO [TreeModelFlags],
--    customTreeModelGetNColumns   :: IO Int,
--    customTreeModelGetColumnType :: Int -> IO GType,
    customTreeModelGetIter       :: TreePath -> IO (Maybe TreeIter),              -- convert a path to an iterator
    customTreeModelGetPath       :: TreeIter -> IO TreePath,                      -- convert an interator to a path
--    customTreeModelGetValue      :: TreeIter -> Int -> GValue -> IO (),           -- get the value at an iter and column
    customTreeModelGetRow        :: TreeIter -> IO row,                           -- get the row at an iter
    customTreeModelIterNext      :: TreeIter -> IO (Maybe TreeIter),              -- following row (if any)
    customTreeModelIterChildren  :: Maybe TreeIter -> IO (Maybe TreeIter),        -- first child row (if any)
    customTreeModelIterHasChild  :: TreeIter -> IO Bool,                          -- row has any children at all
    customTreeModelIterNChildren :: Maybe TreeIter -> IO Int,                     -- number of children of a row
    customTreeModelIterNthChild  :: Maybe TreeIter -> Int -> IO (Maybe TreeIter), -- nth child row of a given row
    customTreeModelIterParent    :: TreeIter -> IO (Maybe TreeIter),              -- parent row of a row
    customTreeModelRefNode       :: TreeIter -> IO (),                            -- caching hint
    customTreeModelUnrefNode     :: TreeIter -> IO ()                             -- caching hint
  }

customTreeModelNew :: private -> CustomTreeModelImplementation row -> IO (CustomTreeModel private row)
customTreeModelNew priv impl = do
  implPtr <- newStablePtr impl
  privPtr <- newStablePtr priv
  makeNewGObject CustomTreeModel $
    gtk2hs_store_new implPtr privPtr

foreign import ccall unsafe "Gtk2HsStore.h gtk2hs_store_new"
  gtk2hs_store_new :: StablePtr (CustomTreeModelImplementation row)
                   -> StablePtr private
                   -> IO (Ptr (CustomTreeModel private row))

treeModelGetRow :: TypedTreeModelClass model => model row -> TreeIter -> IO row
treeModelGetRow model iter = 
  case toTypedTreeModel model of
    TypedTreeModel model -> do
      impl <- withForeignPtr model gtk2hs_store_get_impl >>= deRefStablePtr
      customTreeModelGetRow impl iter

foreign import ccall unsafe "Gtk2HsStore.h gtk2hs_store_get_impl"
  gtk2hs_store_get_impl :: Ptr (TypedTreeModel row) -> IO (StablePtr (CustomTreeModelImplementation row))

customTreeModelGetPrivate :: CustomTreeModel private row -> private
customTreeModelGetPrivate (CustomTreeModel model) =
  unsafePerformIO $ -- this is safe because the priv member is set at
                    -- construction time and never modified after that
  withForeignPtr model gtk2hs_store_get_priv >>= deRefStablePtr

foreign import ccall unsafe "Gtk2HsStore.h gtk2hs_store_get_priv"
  gtk2hs_store_get_priv :: Ptr (CustomTreeModel private row) -> IO (StablePtr private)

customTreeModelGetStamp :: CustomTreeModel private row -> IO CInt
customTreeModelGetStamp (CustomTreeModel model) =
  withForeignPtr model gtk2hs_store_get_stamp

foreign import ccall unsafe "Gtk2HsStore.h gtk2hs_store_get_stamp"
  gtk2hs_store_get_stamp :: Ptr (CustomTreeModel private row) -> IO CInt

customTreeModelInvalidateIters :: CustomTreeModel private row -> IO ()
customTreeModelInvalidateIters (CustomTreeModel model) =
  withForeignPtr model gtk2hs_store_increment_stamp

foreign import ccall unsafe "Gtk2HsStore.h gtk2hs_store_increment_stamp"
  gtk2hs_store_increment_stamp :: Ptr (CustomTreeModel private row) -> IO ()


customTreeModelGetFlags_static :: StablePtr (CustomTreeModelImplementation row) -> IO CInt
customTreeModelGetFlags_static storePtr = do
  store <- deRefStablePtr storePtr
  liftM (fromIntegral . fromFlags) $ customTreeModelGetFlags store

foreign export ccall "gtk2hs_store_get_flags_impl"
  customTreeModelGetFlags_static :: StablePtr (CustomTreeModelImplementation row) -> IO CInt

{-
customTreeModelGetNColumns_static :: StablePtr (CustomTreeModelImplementation row) -> IO CInt
customTreeModelGetNColumns_static storePtr = do
  store <- deRefStablePtr storePtr
  liftM fromIntegral $ customTreeModelGetNColumns store

foreign export ccall "gtk2hs_store_get_n_columns_impl"
  customTreeModelGetNColumns_static :: StablePtr (CustomTreeModelImplementation row) -> IO CInt


customTreeModelGetColumnType_static :: StablePtr (CustomTreeModelImplementation row) -> CInt -> IO GType
customTreeModelGetColumnType_static storePtr column = do
  store <- deRefStablePtr storePtr
  customTreeModelGetColumnType store (fromIntegral column)

foreign export ccall "gtk2hs_store_get_column_type_impl"
  customTreeModelGetColumnType_static :: StablePtr (CustomTreeModelImplementation row) -> CInt -> IO GType
-}

customTreeModelGetIter_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> Ptr NativeTreePath -> IO CInt
customTreeModelGetIter_static storePtr iterPtr pathPtr = do
  store <- deRefStablePtr storePtr
  path <- peekTreePath pathPtr
  iter <- customTreeModelGetIter store path
  case iter of
    Nothing   -> return (fromBool False)
    Just iter -> do poke iterPtr iter
                    return (fromBool True)

foreign export ccall "gtk2hs_store_get_iter_impl"
  customTreeModelGetIter_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> Ptr NativeTreePath -> IO CInt

customTreeModelGetPath_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> IO (Ptr NativeTreePath)
customTreeModelGetPath_static storePtr iterPtr = do
  store <- deRefStablePtr storePtr
  iter <- peek iterPtr
  path <- customTreeModelGetPath store iter
  NativeTreePath pathPtr <- newTreePath path
  return pathPtr

foreign export ccall "gtk2hs_store_get_path_impl"
  customTreeModelGetPath_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> IO (Ptr NativeTreePath)

{-
customTreeModelGetValue_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> CInt -> Ptr GValue -> IO ()
customTreeModelGetValue_static storePtr iterPtr column gvaluePtr = do
  store <- deRefStablePtr storePtr
  iter <- peek iterPtr
  customTreeModelGetValue store iter (fromIntegral column) (GValue gvaluePtr)

foreign export ccall "gtk2hs_store_get_value_impl"
  customTreeModelGetValue_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> CInt -> Ptr GValue -> IO ()
-}


customTreeModelIterNext_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> IO CInt
customTreeModelIterNext_static storePtr iterPtr = do
  store <- deRefStablePtr storePtr
  iter <- peek iterPtr
  iter' <- customTreeModelIterNext store iter
  case iter' of
    Nothing    -> return (fromBool False)
    Just iter' -> do poke iterPtr iter'
                     return (fromBool True)

foreign export ccall "gtk2hs_store_iter_next_impl"
  customTreeModelIterNext_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> IO CInt


customTreeModelIterChildren_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> Ptr TreeIter -> IO CInt
customTreeModelIterChildren_static storePtr iterPtr parentIterPtr = do
  store <- deRefStablePtr storePtr
  parentIter <- maybeNull peek parentIterPtr
  iter <- customTreeModelIterChildren store parentIter
  case iter of
    Nothing   -> return (fromBool False)
    Just iter -> do poke iterPtr iter
                    return (fromBool True)

foreign export ccall "gtk2hs_store_iter_children_impl"
  customTreeModelIterChildren_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> Ptr TreeIter -> IO CInt


customTreeModelIterHasChild_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> IO CInt
customTreeModelIterHasChild_static storePtr iterPtr = do
  store <- deRefStablePtr storePtr
  iter <- peek iterPtr
  liftM fromBool $ customTreeModelIterHasChild store iter

foreign export ccall "gtk2hs_store_iter_has_child_impl"
  customTreeModelIterHasChild_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> IO CInt


customTreeModelIterNChildren_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> IO CInt
customTreeModelIterNChildren_static storePtr iterPtr = do
  store <- deRefStablePtr storePtr
  iter <- maybeNull peek iterPtr
  liftM fromIntegral $ customTreeModelIterNChildren store iter

foreign export ccall "gtk2hs_store_iter_n_children_impl"
  customTreeModelIterNChildren_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> IO CInt


customTreeModelIterNthChild_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> Ptr TreeIter -> CInt -> IO CInt
customTreeModelIterNthChild_static storePtr iterPtr parentIterPtr n = do
  store <- deRefStablePtr storePtr
  parentIter <- maybeNull peek parentIterPtr
  iter <- customTreeModelIterNthChild store parentIter (fromIntegral n)
  case iter of
    Nothing   -> return (fromBool False)
    Just iter -> do poke iterPtr iter
                    return (fromBool True)

foreign export ccall "gtk2hs_store_iter_nth_child_impl"
  customTreeModelIterNthChild_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> Ptr TreeIter -> CInt -> IO CInt


customTreeModelIterParent_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> Ptr TreeIter -> IO CInt
customTreeModelIterParent_static  storePtr iterPtr childIterPtr = do
  store <- deRefStablePtr storePtr
  childIter <- peek childIterPtr
  iter <- customTreeModelIterParent store childIter
  case iter of
    Nothing   -> return (fromBool False)
    Just iter -> do poke iterPtr iter
                    return (fromBool True)

foreign export ccall "gtk2hs_store_iter_parent_impl"
  customTreeModelIterParent_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> Ptr TreeIter -> IO CInt


customTreeModelRefNode_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> IO ()
customTreeModelRefNode_static storePtr iterPtr = do
  store <- deRefStablePtr storePtr
  iter <- peek iterPtr
  customTreeModelRefNode store iter

foreign export ccall "gtk2hs_store_ref_node_impl"
  customTreeModelRefNode_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> IO ()


customTreeModelUnrefNode_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> IO ()
customTreeModelUnrefNode_static storePtr iterPtr = do
  store <- deRefStablePtr storePtr
  iter <- peek iterPtr
  customTreeModelUnrefNode store iter

foreign export ccall "gtk2hs_store_unref_node_impl"
  customTreeModelUnrefNode_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> IO ()

maybeNull :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
maybeNull marshal ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = liftM Just (marshal ptr)


iterSetStamp :: CInt -> TreeIter -> TreeIter
iterSetStamp t (TreeIter _ a b c) = (TreeIter t a b c)

-- | Emits the \"row_changed\" signal on the 'CustomTreeModel'.
--
treeModelRowChanged ::
    CustomTreeModel private row
 -> TreePath -- ^ @path@ - A 'TreePath' pointing to the changed row
 -> TreeIter -- ^ @iter@ - A valid 'TreeIter' pointing to the changed row
 -> IO ()
treeModelRowChanged model path iter =
  withTreePath path $ \pathPtr ->
  customTreeModelGetStamp model >>= \stamp ->
  with (iterSetStamp stamp iter) $ \iterPtr ->
  {# call gtk_tree_model_row_changed #}
    (toTreeModel model)
    pathPtr
    iterPtr

-- | Emits the \"row_inserted\" signal on the 'CustomTreeModel'
--
treeModelRowInserted ::
    CustomTreeModel private row
 -> TreePath -- ^ @path@ - A 'TreePath' pointing to the inserted row
 -> TreeIter -- ^ @iter@ - A valid 'TreeIter' pointing to the inserted row
 -> IO ()
treeModelRowInserted model path iter =
  withTreePath path $ \pathPtr ->
  customTreeModelGetStamp model >>= \stamp ->
  with (iterSetStamp stamp iter) $ \iterPtr ->
  {# call gtk_tree_model_row_inserted #}
    (toTreeModel model)
    pathPtr
    iterPtr

-- | Emits the \"row_has_child_toggled\" signal on the 'CustomTreeModel'. This should
-- be called by models after a node went from having no children to having
-- at least one child or vice versa.
--
treeModelRowHasChildToggled ::
    CustomTreeModel private row
 -> TreePath -- ^ @path@ - A 'TreePath' pointing to the changed row
 -> TreeIter -- ^ @iter@ - A valid 'TreeIter' pointing to the changed row
 -> IO ()
treeModelRowHasChildToggled model path iter =
  withTreePath path $ \pathPtr ->
  customTreeModelGetStamp model >>= \stamp ->
  with (iterSetStamp stamp iter) $ \iterPtr ->
  {# call gtk_tree_model_row_has_child_toggled #}
    (toTreeModel model)
    pathPtr
    iterPtr

-- | Emits the \"row_deleted\" signal the 'CustomTreeModel'. This should be called by
-- models after a row has been removed. The location pointed to by @path@
-- should be the location that the row previously was at. It may not be a valid
-- location anymore.
--
treeModelRowDeleted ::
    CustomTreeModel private row
 -> TreePath -- ^ @path@ - A 'TreePath' pointing to the previous location of
             -- the deleted row.
 -> IO ()
treeModelRowDeleted model path =
  withTreePath path $ \pathPtr ->
  {# call gtk_tree_model_row_deleted #}
    (toTreeModel model)
    pathPtr

-- | Emits the \"rows_reordered\" signal on the 'CustomTreeModel'. This should be
-- called by models when their rows have been reordered.
--
treeModelRowsReordered ::
    CustomTreeModel private row
 -> TreePath -- ^ @path@ - A 'TreePath' pointing to the tree node whose
             -- children have been reordered
 -> TreeIter -- ^ @iter@ - A valid 'TreeIter' pointing to the node whose
             -- children have been reordered, or {@NULL@, FIXME: this should
             -- probably be converted to a Maybe data type} if the depth of
             -- @path@ is 0.
 -> [Int]   -- ^ @newOrder@ - an array of integers mapping the current
             -- position of each child to its old position before the
             -- re-ordering, i.e. @newOrder@@[newpos] = oldpos@.
 -> IO ()
treeModelRowsReordered model path iter newOrder =
  withTreePath path $ \pathPtr ->
  customTreeModelGetStamp model >>= \stamp ->
  with (iterSetStamp stamp iter) $ \iterPtr ->
  withArrayLen (map fromIntegral newOrder) $ \newLength newOrderArrPtr -> do
  --check newOrder is the right length or it'll overrun
  curLength <- treeModelIterNChildren model (Just iter)
  when (curLength /= newLength)
       (fail "treeModelRowsReordered: mapping wrong length for store")
  {# call gtk_tree_model_rows_reordered #}
    (toTreeModel model)
    pathPtr
    iterPtr
    newOrderArrPtr
