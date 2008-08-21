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
  -- * The definition of a row-based store.  
  CustomTreeModel,
  TreeModelFlags(..),
  TreeModelIface(..),
  DragSourceIface(..),
  DragDestIface(..),
  customTreeModelNew,
  treeModelGetRow,
  customTreeModelGetPrivate,
  customTreeModelGetStamp,
  customTreeModelInvalidateIters,

  -- * Extracting a specific datum from the row-oriented store for
  --   properties that are not drawn by 'CellRenderer's.
  ColumnId,
  makeColumnIdInt,
  makeColumnIdBool,
  makeColumnIdString,
  makeColumnIdPixbuf,
  columnIdToNumber,
  invalidColumnId,
  treeModelSetColumn,
  ) where

import Control.Monad	                        (liftM, when)
import Control.Monad.Reader                     (runReaderT)
import Data.IORef                               (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe                               (fromMaybe)
import System.Glib.FFI			hiding	(maybeNull)
import System.Glib.Flags			(Flags, fromFlags)
import System.Glib.GObject			(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.ModelView.Types#}
import Graphics.UI.Gtk.General.DNDTypes         (SelectionDataM, SelectionData)

import System.Glib.StoreValue			(TMType(..), GenericValue(..), valueSetGenericValue)
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

-- | A CustomTreeModel is backed by a Gtk2HsStore
-- which is an instance of the GtkTreeModel GInterface
-- it also stores some extra per-model-type private data
newtype CustomTreeModel private row = CustomTreeModel (ForeignPtr (CustomTreeModel private row))

instance TreeModelClass (CustomTreeModel private row)
instance GObjectClass (CustomTreeModel private row) where
  toGObject (CustomTreeModel tm) = mkGObject (castForeignPtr tm)
  unsafeCastGObject = CustomTreeModel . castForeignPtr . unGObject

-- | Accessing a row for a specific value. Used for 'ColumnMap'.
data ColumnAccess row
  = CAInvalid
  | CAInt (row -> Int)
  | CABool (row -> Bool)
  | CAString (row -> String)
  | CAPixbuf (row -> Pixbuf)

-- | Type synonym for viewing the store as a set of columns.
type ColumnMap row = IORef [ColumnAccess row]

-- | Create a new 'ColumnMap' value.
columnMapNew :: IO (ColumnMap row)
columnMapNew = newIORef []

-- | The type of a tree column.
data ColumnId row ty = ColumnId ((row -> ty) -> ColumnAccess row) Int

-- | Create a 'ColumnId' to extract an integer.
makeColumnIdInt :: Int -> ColumnId row Int
makeColumnIdInt = ColumnId CAInt

-- | Create a 'ColumnId' to extract an Boolean.
makeColumnIdBool :: Int -> ColumnId row Bool
makeColumnIdBool = ColumnId CABool

-- | Create a 'ColumnId' to extract an string.
makeColumnIdString :: Int -> ColumnId row String
makeColumnIdString = ColumnId CAString

-- | Create a 'ColumnId' to extract an 'Pixbuf'.
makeColumnIdPixbuf :: Int -> ColumnId row Pixbuf
makeColumnIdPixbuf = ColumnId CAPixbuf

-- | Convert a 'ColumnId' to a bare number.
columnIdToNumber :: ColumnId row ty -> Int
columnIdToNumber (ColumnId _ i) = i

-- | The invalid 'ColumnId'. Widgets use this value if no column id has
--   been set.
invalidColumnId :: ColumnId row ty
invalidColumnId = ColumnId (error "invalidColumnId: no access type") (-1)

instance Eq (ColumnId row ty) where
  (ColumnId _ i1) == (ColumnId _ i2) = i1==i2

instance Show (ColumnId row ty) where
  show (ColumnId _ i) = show i
  
-- | Set or update a column mapping.
treeModelSetColumn :: TypedTreeModelClass model
	=> model row -- ^ the store in which to allocate a new column
	-> (ColumnId row ty) -- ^ the column that should be set
	-> (row -> ty) -- ^ the function that sets the property
	-> IO ()
treeModelSetColumn model (ColumnId setter colId) acc | colId<0 = return ()
                                                     | otherwise =
  case toTypedTreeModel model of
    TypedTreeModel model -> do
      ptr <- withForeignPtr model gtk2hs_store_get_impl
      impl <- deRefStablePtr ptr
      let cMap = customTreeModelColumns impl
      cols <- readIORef cMap
      let l = length cols
      if colId>=l then do
         let fillers = replicate (colId-l) CAInvalid
         writeIORef cMap (cols++fillers++[setter acc])
       else do
         let (beg,_:end) = splitAt colId cols
         writeIORef cMap (beg++setter acc:end)

data CustomTreeModelImplementation model row = CustomTreeModelImplementation {
    customTreeModelColumns   	:: ColumnMap row,	                -- provide access via columns
    customTreeModelIface        :: TreeModelIface row,                  -- functions implementing a tree model
    customTreeDragSourceIface   :: DragSourceIface model row,           -- the drag and drop source interface
    customTreeDragDestIface     :: DragDestIface model row              -- the drag and drop dest interface
  }

data TreeModelIface row = TreeModelIface {
    treeModelIfaceGetFlags      :: IO [TreeModelFlags],
    treeModelIfaceGetIter       :: TreePath -> IO (Maybe TreeIter),              -- convert a path to an iterator
    treeModelIfaceGetPath       :: TreeIter -> IO TreePath,                      -- convert an interator to a path
    treeModelIfaceGetRow        :: TreeIter -> IO row,                           -- get the row at an iter
    treeModelIfaceIterNext      :: TreeIter -> IO (Maybe TreeIter),              -- following row (if any)
    treeModelIfaceIterChildren  :: Maybe TreeIter -> IO (Maybe TreeIter),        -- first child row (if any)
    treeModelIfaceIterHasChild  :: TreeIter -> IO Bool,                          -- row has any children at all
    treeModelIfaceIterNChildren :: Maybe TreeIter -> IO Int,                     -- number of children of a row
    treeModelIfaceIterNthChild  :: Maybe TreeIter -> Int -> IO (Maybe TreeIter), -- nth child row of a given row
    treeModelIfaceIterParent    :: TreeIter -> IO (Maybe TreeIter),              -- parent row of a row
    treeModelIfaceRefNode       :: TreeIter -> IO (),                            -- caching hint
    treeModelIfaceUnrefNode     :: TreeIter -> IO ()                             -- caching hint
  }

data DragSourceIface model row = DragSourceIface {
    treeDragSourceRowDraggable  :: model row -> TreePath -> IO Bool,                 -- query if the row is draggable
    treeDragSourceDragDataGet   :: model row -> TreePath -> SelectionDataM Bool,     -- store row in selection object
    treeDragSourceDragDataDelete:: model row -> TreePath -> IO Bool                  -- instruct store to delete the row
  }

data DragDestIface model row = DragDestIface {
    treeDragDestRowDropPossible :: model row -> TreePath -> SelectionDataM Bool,     -- query if row drop is possible
    treeDragDestDragDataReceived:: model row -> TreePath -> SelectionDataM Bool      -- insert row from selection object
  }
  
-- | Create a new store that implements the 'TreeModelIface' interface and
-- optionally the 'DragSourceIface' and the 'DragDestIface'. If the latter two
-- are set to @Nothing@ a dummy interface is substituted that rejects every
-- drag and drop.
customTreeModelNew :: (TreeModelClass (model row), TypedTreeModelClass model) =>
     private   -- ^ Any private data the store needs to store. Usually an 'IORef'.
  -> ((CustomTreeModel private row) -> model row)
  -> TreeModelIface row         -- ^ Functions necessary to implement the 'TreeModel' interface.
  -> Maybe (DragSourceIface model row)
                                -- ^ Functions to enable this store to generate drag events.
  -> Maybe (DragDestIface model row)
                                -- ^ Functions to enable this store to receive drag events.
  -> IO (model row)
customTreeModelNew priv con tmIface mDragSource mDragDest = do
  cMap <- columnMapNew
  let dummyDragSource = DragSourceIface { treeDragSourceRowDraggable = \_ _ -> return False,
                                          treeDragSourceDragDataGet  = \_ _ -> return False,
                                          treeDragSourceDragDataDelete = \_ _ -> return False }
  let dummyDragDest = DragDestIface { treeDragDestRowDropPossible = \_ _ -> return False,
                                      treeDragDestDragDataReceived = \_ _ -> return False }
  implPtr <- newStablePtr CustomTreeModelImplementation {
        customTreeModelColumns = cMap,
        customTreeModelIface = tmIface,
        customTreeDragSourceIface = fromMaybe dummyDragSource mDragSource,
        customTreeDragDestIface = fromMaybe dummyDragDest mDragDest }
  privPtr <- newStablePtr priv
  liftM con $ makeNewGObject CustomTreeModel $
    gtk2hs_store_new implPtr privPtr

foreign import ccall unsafe "Gtk2HsStore.h gtk2hs_store_new"
  gtk2hs_store_new :: StablePtr (CustomTreeModelImplementation model row)
                   -> StablePtr private
                   -> IO (Ptr (CustomTreeModel private row))

treeModelGetRow :: TypedTreeModelClass model => model row -> TreeIter -> IO row
treeModelGetRow model iter = 
  case toTypedTreeModel model of
    TypedTreeModel model -> do
      impl <- withForeignPtr model gtk2hs_store_get_impl >>= deRefStablePtr
      treeModelIfaceGetRow (customTreeModelIface impl) iter

foreign import ccall unsafe "Gtk2HsStore.h gtk2hs_store_get_impl"
  gtk2hs_store_get_impl :: Ptr (TypedTreeModel row) -> IO (StablePtr (CustomTreeModelImplementation model row))

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

treeModelIfaceGetNColumns_static :: StablePtr (CustomTreeModelImplementation model row) -> IO CInt
treeModelIfaceGetNColumns_static storePtr = do
  store <- deRefStablePtr storePtr
  cmap <- readIORef (customTreeModelColumns store)
  return (fromIntegral (length cmap))

foreign export ccall "gtk2hs_store_get_n_columns_impl"
  treeModelIfaceGetNColumns_static :: StablePtr (CustomTreeModelImplementation model row) -> IO CInt

-- Get the 'GType' for a given 'ColumnAccess'.
caToGType :: ColumnAccess row -> GType
caToGType (CAInt _) = GConst.int
caToGType (CABool _) = GConst.bool
caToGType (CAString _) = GConst.string
caToGType (CAPixbuf _) = {#call fun unsafe gdk_pixbuf_get_type#}
caToGType CAInvalid = GConst.int -- to avoid warnings of functions that iterate through all columns

treeModelIfaceGetColumnType_static :: StablePtr (CustomTreeModelImplementation model row) -> CInt -> IO GType
treeModelIfaceGetColumnType_static storePtr column = do
  store <- deRefStablePtr storePtr
  cols <- readIORef (customTreeModelColumns store)
  case drop (fromIntegral column) cols of
     [] -> return GConst.invalid
     (ca:_) -> return (caToGType ca)

foreign export ccall "gtk2hs_store_get_column_type_impl"
  treeModelIfaceGetColumnType_static :: StablePtr (CustomTreeModelImplementation model row) -> CInt -> IO GType


treeModelIfaceGetFlags_static :: StablePtr (CustomTreeModelImplementation model row) -> IO CInt
treeModelIfaceGetFlags_static storePtr = do
  store <- liftM customTreeModelIface $ deRefStablePtr storePtr
  liftM (fromIntegral . fromFlags) $ treeModelIfaceGetFlags store

foreign export ccall "gtk2hs_store_get_flags_impl"
  treeModelIfaceGetFlags_static :: StablePtr (CustomTreeModelImplementation model row) -> IO CInt


treeModelIfaceGetIter_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> Ptr NativeTreePath -> IO CInt
treeModelIfaceGetIter_static storePtr iterPtr pathPtr = do
  store <- liftM customTreeModelIface $ deRefStablePtr storePtr
  path <- peekTreePath pathPtr
  iter <- treeModelIfaceGetIter store path
  case iter of
    Nothing   -> return (fromBool False)
    Just iter -> do poke iterPtr iter
                    return (fromBool True)

foreign export ccall "gtk2hs_store_get_iter_impl"
  treeModelIfaceGetIter_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> Ptr NativeTreePath -> IO CInt

treeModelIfaceGetPath_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> IO (Ptr NativeTreePath)
treeModelIfaceGetPath_static storePtr iterPtr = do
  store <- liftM customTreeModelIface $ deRefStablePtr storePtr
  iter <- peek iterPtr
  path <- treeModelIfaceGetPath store iter
  NativeTreePath pathPtr <- newTreePath path
  return pathPtr

foreign export ccall "gtk2hs_store_get_path_impl"
  treeModelIfaceGetPath_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> IO (Ptr NativeTreePath)


treeModelIfaceGetValue_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> CInt -> Ptr GValue -> IO ()
treeModelIfaceGetValue_static storePtr iterPtr column gvaluePtr = do
  store <- deRefStablePtr storePtr
  iter <- peek iterPtr
  row <- treeModelIfaceGetRow (customTreeModelIface store) iter
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
      CAInvalid -> valueInit gVal GConst.int >> valueSetInt gVal 0
      
foreign export ccall "gtk2hs_store_get_value_impl"
  treeModelIfaceGetValue_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> CInt -> Ptr GValue -> IO ()


treeModelIfaceIterNext_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> IO CInt
treeModelIfaceIterNext_static storePtr iterPtr = do
  store <- liftM customTreeModelIface $ deRefStablePtr storePtr
  iter <- peek iterPtr
  iter' <- treeModelIfaceIterNext store iter
  case iter' of
    Nothing    -> return (fromBool False)
    Just iter' -> do poke iterPtr iter'
                     return (fromBool True)

foreign export ccall "gtk2hs_store_iter_next_impl"
  treeModelIfaceIterNext_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> IO CInt


treeModelIfaceIterChildren_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> Ptr TreeIter -> IO CInt
treeModelIfaceIterChildren_static storePtr iterPtr parentIterPtr = do
  store <- liftM customTreeModelIface $ deRefStablePtr storePtr
  parentIter <- maybeNull peek parentIterPtr
  iter <- treeModelIfaceIterChildren store parentIter
  case iter of
    Nothing   -> return (fromBool False)
    Just iter -> do poke iterPtr iter
                    return (fromBool True)

foreign export ccall "gtk2hs_store_iter_children_impl"
  treeModelIfaceIterChildren_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> Ptr TreeIter -> IO CInt


treeModelIfaceIterHasChild_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> IO CInt
treeModelIfaceIterHasChild_static storePtr iterPtr = do
  store <- liftM customTreeModelIface $ deRefStablePtr storePtr
  iter <- peek iterPtr
  liftM fromBool $ treeModelIfaceIterHasChild store iter

foreign export ccall "gtk2hs_store_iter_has_child_impl"
  treeModelIfaceIterHasChild_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> IO CInt


treeModelIfaceIterNChildren_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> IO CInt
treeModelIfaceIterNChildren_static storePtr iterPtr = do
  store <- liftM customTreeModelIface $ deRefStablePtr storePtr
  iter <- maybeNull peek iterPtr
  liftM fromIntegral $ treeModelIfaceIterNChildren store iter

foreign export ccall "gtk2hs_store_iter_n_children_impl"
  treeModelIfaceIterNChildren_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> IO CInt


treeModelIfaceIterNthChild_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> Ptr TreeIter -> CInt -> IO CInt
treeModelIfaceIterNthChild_static storePtr iterPtr parentIterPtr n = do
  store <- liftM customTreeModelIface $ deRefStablePtr storePtr
  parentIter <- maybeNull peek parentIterPtr
  iter <- treeModelIfaceIterNthChild store parentIter (fromIntegral n)
  case iter of
    Nothing   -> return (fromBool False)
    Just iter -> do poke iterPtr iter
                    return (fromBool True)

foreign export ccall "gtk2hs_store_iter_nth_child_impl"
  treeModelIfaceIterNthChild_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> Ptr TreeIter -> CInt -> IO CInt


treeModelIfaceIterParent_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> Ptr TreeIter -> IO CInt
treeModelIfaceIterParent_static  storePtr iterPtr childIterPtr = do
  store <- liftM customTreeModelIface $ deRefStablePtr storePtr
  childIter <- peek childIterPtr
  iter <- treeModelIfaceIterParent store childIter
  case iter of
    Nothing   -> return (fromBool False)
    Just iter -> do poke iterPtr iter
                    return (fromBool True)

foreign export ccall "gtk2hs_store_iter_parent_impl"
  treeModelIfaceIterParent_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> Ptr TreeIter -> IO CInt


treeModelIfaceRefNode_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> IO ()
treeModelIfaceRefNode_static storePtr iterPtr = do
  store <- liftM customTreeModelIface $ deRefStablePtr storePtr
  iter <- peek iterPtr
  treeModelIfaceRefNode store iter

foreign export ccall "gtk2hs_store_ref_node_impl"
  treeModelIfaceRefNode_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> IO ()


treeModelIfaceUnrefNode_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> IO ()
treeModelIfaceUnrefNode_static storePtr iterPtr = do
  store <- liftM customTreeModelIface $ deRefStablePtr storePtr
  iter <- peek iterPtr
  treeModelIfaceUnrefNode store iter

foreign export ccall "gtk2hs_store_unref_node_impl"
  treeModelIfaceUnrefNode_static :: StablePtr (CustomTreeModelImplementation model row) -> Ptr TreeIter -> IO ()

treeDragSourceRowDraggable_static :: Ptr TreeModel -> StablePtr (CustomTreeModelImplementation model row) -> Ptr NativeTreePath -> IO CInt
treeDragSourceRowDraggable_static mPtr storePtr pathPtr = do
  model <- makeNewGObject mkTreeModel (return mPtr)
  store <- liftM customTreeDragSourceIface $ deRefStablePtr storePtr
  path <- peekTreePath pathPtr
  liftM fromBool $ treeDragSourceRowDraggable store (unsafeTreeModelToGeneric model) path

foreign export ccall "gtk2hs_store_row_draggable_impl"
  treeDragSourceRowDraggable_static :: Ptr TreeModel -> StablePtr (CustomTreeModelImplementation model row) -> Ptr NativeTreePath -> IO CInt

treeDragSourceDragDataGet_static :: Ptr TreeModel -> StablePtr (CustomTreeModelImplementation model row) -> Ptr NativeTreePath -> SelectionData -> IO CInt
treeDragSourceDragDataGet_static mPtr storePtr pathPtr selectionPtr = do
  model <- makeNewGObject mkTreeModel (return mPtr)
  store <- liftM customTreeDragSourceIface $ deRefStablePtr storePtr
  path <- peekTreePath pathPtr
  liftM fromBool $ runReaderT (treeDragSourceDragDataGet store (unsafeTreeModelToGeneric model) path) selectionPtr

foreign export ccall "gtk2hs_store_drag_data_get_impl"
  treeDragSourceDragDataGet_static :: Ptr TreeModel -> StablePtr (CustomTreeModelImplementation model row) -> Ptr NativeTreePath -> SelectionData -> IO CInt

treeDragSourceDragDataDelete_static :: Ptr TreeModel -> StablePtr (CustomTreeModelImplementation model row) -> Ptr NativeTreePath -> IO CInt
treeDragSourceDragDataDelete_static mPtr storePtr pathPtr = do
  model <- makeNewGObject mkTreeModel (return mPtr)
  store <- liftM customTreeDragSourceIface $ deRefStablePtr storePtr
  path <- peekTreePath pathPtr
  liftM fromBool $ treeDragSourceDragDataDelete store (unsafeTreeModelToGeneric model) path

foreign export ccall "gtk2hs_store_drag_data_delete_impl"
  treeDragSourceDragDataDelete_static :: Ptr TreeModel -> StablePtr (CustomTreeModelImplementation model row) -> Ptr NativeTreePath -> IO CInt

treeDragDestDragDataReceived_static :: Ptr TreeModel -> StablePtr (CustomTreeModelImplementation model row) -> Ptr NativeTreePath -> SelectionData -> IO CInt
treeDragDestDragDataReceived_static mPtr storePtr pathPtr selectionPtr = do
  model <- makeNewGObject mkTreeModel (return mPtr)
  store <- liftM customTreeDragDestIface $ deRefStablePtr storePtr
  path <- peekTreePath pathPtr
  liftM fromBool $ runReaderT (treeDragDestDragDataReceived store (unsafeTreeModelToGeneric model) path) selectionPtr

foreign export ccall "gtk2hs_store_drag_data_received_impl"
  treeDragDestDragDataReceived_static :: Ptr TreeModel -> StablePtr (CustomTreeModelImplementation model row) -> Ptr NativeTreePath -> SelectionData -> IO CInt

treeDragDestRowDropPossible_static :: Ptr TreeModel -> StablePtr (CustomTreeModelImplementation model row) -> Ptr NativeTreePath -> SelectionData -> IO CInt
treeDragDestRowDropPossible_static mPtr storePtr pathPtr selectionPtr = do
  model <- makeNewGObject mkTreeModel (return mPtr)
  store <- liftM customTreeDragDestIface $ deRefStablePtr storePtr
  path <- peekTreePath pathPtr
  liftM fromBool $ runReaderT (treeDragDestRowDropPossible store (unsafeTreeModelToGeneric model) path) selectionPtr

foreign export ccall "gtk2hs_store_row_drop_possible_impl"
  treeDragDestRowDropPossible_static :: Ptr TreeModel -> StablePtr (CustomTreeModelImplementation model row) -> Ptr NativeTreePath -> SelectionData -> IO CInt

maybeNull :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
maybeNull marshal ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = liftM Just (marshal ptr)

iterSetStamp :: CInt -> TreeIter -> TreeIter
iterSetStamp t (TreeIter _ a b c) = (TreeIter t a b c)

