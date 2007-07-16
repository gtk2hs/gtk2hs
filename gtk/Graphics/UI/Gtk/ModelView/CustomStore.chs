-- -*-haskell-*-
--  GIMP Toolkit (GTK) CustomStore TreeModel
--
--  Author : Duncan Coutts
--
--  Created: 19 Sep 2005
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
  TreeModelFlags(..),
  
  ColumnMap,
  ColumnAccess(..),
  ColumnId,
  columnMapNew,

  treeModelUpdateColumn,
  treeModelGetRow,

  CustomTreeModel,
  CustomTreeModelImplementation(..),
  customTreeModelNew,
  customTreeModelGetPrivate,
  customTreeModelInvalidateIters,
  ) where

import Control.Monad	(liftM, when)
import Data.IORef
import System.Glib.FFI			hiding	(maybeNull)
import System.Glib.Flags			(Flags, fromFlags)
import System.Glib.GObject			(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.ModelView.Types#}
import System.Glib.StoreValue			(TMType(..), GenericValue(..)
						,valueSetGenericValue)
{#import System.Glib.GValue#}			(GValue(GValue), allocaGValue)
{#import System.Glib.GType#}			(GType)
import System.Glib.GValueTypes                  (valueSetString)
import qualified System.Glib.GTypeConstants as GConst
{#import System.Glib.GValueTypes#}
{#import System.Glib.GValue#}			(valueInit)

{# context lib="gtk" prefix="gtk" #}

-- | These flags indicate various properties of a 'TreeModel'.
--
-- * If a model has "TreeModelItersPersist" set, iterators remain valid
--   after a "TreeModel" signal was emitted.
--
-- * The "TreeModelListOnly" flag is set if the rows are arranged in a
--   simple flat list. This is set in the "ListStore" implementation.
--
{#enum TreeModelFlags {underscoreToCase} deriving(Bounded)#}

instance Flags TreeModelFlags

-- A CustomTreeModel is backed by a Gtk2HsStore
-- which is an instance of the GtkTreeModel GInterface
-- it also stores some extra per-model-type private data
newtype CustomTreeModel private row = CustomTreeModel (ForeignPtr (CustomTreeModel private row))
instance GObjectClass (CustomTreeModel private row)
instance TreeModelClass (CustomTreeModel private row)

-- | Accessing a row for a specific value. Used for 'ColumnMap'.
data ColumnAccess row
  = CAInt (row -> Int)
  | CABool (row -> Bool)
  | CAString (row -> String)
  | CAPixbuf (row -> Pixbuf)

-- | Type synonym for viewing the store as a set of columns.
type ColumnMap row = IORef [ColumnAccess row]

-- | Create a new 'ColumnMap' value.
columnMapNew :: IO (ColumnMap row)
columnMapNew = newIORef []

-- | The type of a tree column.
type ColumnId = Int
	
-- | Insert or update a column mapping.
treeModelUpdateColumn :: TypedTreeModelClass model 
	=> model row -- ^ the store in which to allocate a new column
	-> ColumnId -- ^ the column that should be updated, 
	            --   or 'invalidColumnId' to request a new column
	-> ColumnAccess row -- ^ the function that sets the property
	-> IO ColumnId -- ^ returns the newly assigned column
treeModelUpdateColumn model oldC acc =
  case toTypedTreeModel model of
    TypedTreeModel model -> do
      ptr <- withForeignPtr model gtk2hs_store_get_impl
      impl <- deRefStablePtr ptr
      let cMap = customTreeModelColumns impl
      cols <- readIORef cMap
      let l = length cols
      if oldC<0 || oldC>=l then do
         writeIORef cMap (cols++[acc])
         return l
       else do
         let (beg,_:end) = splitAt oldC cols
         writeIORef cMap (beg++acc:end)
         return oldC

data CustomTreeModelImplementation row = CustomTreeModelImplementation {
    customTreeModelGetFlags      :: IO [TreeModelFlags],
    customTreeModelColumns   	 :: ColumnMap row,				  -- provide access via columns
    customTreeModelGetIter       :: TreePath -> IO (Maybe TreeIter),              -- convert a path to an iterator
    customTreeModelGetPath       :: TreeIter -> IO TreePath,                      -- convert an interator to a path
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


customTreeModelGetNColumns_static :: StablePtr (CustomTreeModelImplementation row) -> IO CInt
customTreeModelGetNColumns_static storePtr = do
  store <- deRefStablePtr storePtr
  cmap <- readIORef (customTreeModelColumns store)
  return (fromIntegral (length cmap))

foreign export ccall "gtk2hs_store_get_n_columns_impl"
  customTreeModelGetNColumns_static :: StablePtr (CustomTreeModelImplementation row) -> IO CInt

-- Get the 'GType' for a given 'ColumnAccess'.
caToGType :: ColumnAccess row -> GType
caToGType (CAInt _) = GConst.int
caToGType (CABool _) = GConst.bool
caToGType (CAString _) = GConst.string
caToGType (CAPixbuf _) = {#call fun unsafe gdk_pixbuf_get_type#}

customTreeModelGetColumnType_static :: StablePtr (CustomTreeModelImplementation row) -> CInt -> IO GType
customTreeModelGetColumnType_static storePtr column = do
  store <- deRefStablePtr storePtr
  cols <- readIORef (customTreeModelColumns store)
  case drop (fromIntegral column) cols of
     [] -> return GConst.invalid
     (ca:_) -> return (caToGType ca)

foreign export ccall "gtk2hs_store_get_column_type_impl"
  customTreeModelGetColumnType_static :: StablePtr (CustomTreeModelImplementation row) -> CInt -> IO GType


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


customTreeModelGetValue_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> CInt -> Ptr GValue -> IO ()
customTreeModelGetValue_static storePtr iterPtr column gvaluePtr = do
  store <- deRefStablePtr storePtr
  iter <- peek iterPtr
  row <- customTreeModelGetRow store iter
  cols <- readIORef (customTreeModelColumns store)
  let gVal = (GValue gvaluePtr)
  0 <- {# get GValue->g_type #} gvaluePtr
  case drop (fromIntegral column) cols of
    [] -> valueInit gVal GConst.invalid -- column number out of range
    (acc:_) -> case acc of
      (CAInt ca) -> valueInit gVal GConst.int >> valueSetInt gVal (ca row)
      (CABool ca) -> valueInit gVal GConst.bool >> valueSetBool gVal (ca row)
      (CAString ca) -> valueInit gVal GConst.string >> valueSetString gVal (ca row)
      (CAPixbuf ca) -> valueInit gVal {#call fun unsafe gdk_pixbuf_get_type#} >>
			valueSetGObject gVal (ca row)

foreign export ccall "gtk2hs_store_get_value_impl"
  customTreeModelGetValue_static :: StablePtr (CustomTreeModelImplementation row) -> Ptr TreeIter -> CInt -> Ptr GValue -> IO ()


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

