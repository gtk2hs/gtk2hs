-- -*-haskell-*-
--  GIMP Toolkit (GTK) CustomStore TreeModel
--
--  Author : Duncan Coutts, Axel Simon
--
--  Created: 19 Sep 2005
--
--  Copyright (C) 2005 Duncan Coutts, Axel Simon
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
-- #prune

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Allows a custom data structure to be used with the 'TreeView' and other
-- widgets that follow the model-view-controller paradigm. The two models
-- 'Graphics.UI.Gtk.ModelView.ListStore.ListStore' and
-- 'Graphics.UI.Gtk.ModelView.TreeStore.TreeStore' are based on the
-- 'CustomStore'. Even if no application-specific tree model
-- should be implemented, this module is relevant in that it provides the
-- functions 'customStoreSetColumn' and
-- 'customStoreGetRow' functions.
--
module Graphics.UI.Gtk.ModelView.CustomStore (
  -- * The definition of a row-based store.
  CustomStore,
  TreeModelFlags(..),
  TreeModelIface(..),
  DragSourceIface(..),
  DragDestIface(..),
  customStoreNew,
  customStoreGetRow,
  customStoreSetColumn,
  customStoreGetPrivate,
  customStoreGetStamp,
  customStoreInvalidateIters,
  -- for backwards compatability, not documented
  treeModelGetRow,
  treeModelSetColumn,
  ) where

import Control.Monad                            (liftM)
import Control.Monad.Reader                     (runReaderT)
import Data.IORef                               (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe                               (fromMaybe)
import System.Glib.FFI                  hiding  (maybeNull)
import System.Glib.Flags                        (Flags, fromFlags)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.ModelView.Types#}
import Graphics.UI.Gtk.General.DNDTypes         (SelectionDataM, SelectionData)

{#import System.Glib.GValue#}                   (GValue(GValue))
{#import System.Glib.GType#}                    (GType)
import qualified System.Glib.GTypeConstants as GConst
{#import System.Glib.GValueTypes#}
{#import System.Glib.GValue#}                   (valueInit)

{# context lib="gtk" prefix="gtk" #}

-- | These flags indicate various properties of a
-- 'Graphics.UI.Gtk.ModelView.TreeModel.TreeModel'.
--
-- * If a model has 'TreeModelItersPersist' set, iterators remain valid after
--   a 'Graphics.UI.Gtk.ModelView.TreeModel.TreeModel' signal was emitted.
--
-- * The 'TreeModelListOnly' flag is set if the rows are arranged in a simple
--   flat list. This is set in the
--   'Graphics.UI.Gtk.ModelView.ListStore.ListStore' implementation.
--
{#enum TreeModelFlags {underscoreToCase} deriving(Bounded)#}

instance Flags TreeModelFlags

-- A 'CustomStore' is backed by a Gtk2HsStore
-- which is an instance of the GtkTreeModel GInterface
-- it also stores some extra per-model-type private data

-- | A 'CustomStore' is an instance of a Gtk+ 'TreeModel' and can thus be used
--   for any widget that stores data in a 'TreeModel'. The user may either
--   create an instance of a 'CustomStore' or use one of the pre-defined
--   models 'Graphics.UI.Gtk.ModelView.ListStore.ListStore' or
--   'Graphics.UI.Gtk.ModelView.TreeStore.TreeStore'.
newtype CustomStore private row = CustomStore (ForeignPtr (CustomStore private row))

instance TreeModelClass (CustomStore private row)
instance GObjectClass (CustomStore private row) where
  toGObject (CustomStore tm) = GObject (castForeignPtr tm)
  unsafeCastGObject = CustomStore . castForeignPtr . unGObject

-- | Type synonym for viewing the store as a set of columns.
type ColumnMap row = IORef [ColumnAccess row]

-- | Create a new 'ColumnMap' value.
columnMapNew :: IO (ColumnMap row)
columnMapNew = newIORef []

-- | Set or update a column mapping. This function should be used before
--   the model is installed into a widget since the number of defined
--   columns are only checked once by widgets.
customStoreSetColumn :: TypedTreeModelClass model
        => model row -- ^ the store in which to allocate a new column
        -> (ColumnId row ty) -- ^ the column that should be set
        -> (row -> ty) -- ^ the function that sets the property
        -> IO ()
customStoreSetColumn model (ColumnId _ setter colId) acc | colId<0 = return ()
                                                         | otherwise =
  case toTypedTreeModel model of
    TypedTreeModel model -> do
      ptr <- withForeignPtr model gtk2hs_store_get_impl
      impl <- deRefStablePtr ptr
      let cMap = customStoreColumns impl
      cols <- readIORef cMap
      let l = length cols
      if colId>=l then do
         let fillers = replicate (colId-l) CAInvalid
         writeIORef cMap (cols++fillers++[setter acc])
       else do
         let (beg,_:end) = splitAt colId cols
         writeIORef cMap (beg++setter acc:end)

-- this is a backwards compatability definition
treeModelSetColumn :: TypedTreeModelClass model
        => model row -- ^ the store in which to allocate a new column
        -> (ColumnId row ty) -- ^ the column that should be set
        -> (row -> ty) -- ^ the function that sets the property
        -> IO ()
treeModelSetColumn = customStoreSetColumn

data CustomStoreImplementation model row = CustomStoreImplementation {
    customStoreColumns          :: ColumnMap row,                       -- provide access via columns
    customStoreIface            :: TreeModelIface row,            -- functions implementing a tree model
    customTreeDragSourceIface   :: DragSourceIface model row,     -- the drag and drop source interface
    customTreeDragDestIface     :: DragDestIface model row        -- the drag and drop dest interface
  }

-- | The 'TreeModelIface' structure contains all functions that are required
-- to implement an application-specific 'TreeModel'.
data TreeModelIface row = TreeModelIface {
    -- | Return the flags that are valid for this model.
    treeModelIfaceGetFlags      :: IO [TreeModelFlags],
    -- | Convert an path into the tree into a more concise 'TreeIter'.
    --   Return @Nothing@ if the path does not exit.
    treeModelIfaceGetIter       :: TreePath -> IO (Maybe TreeIter),              -- convert a path to an iterator
    -- | Convert an iterator to a path. The iterator will always be valid.
    treeModelIfaceGetPath       :: TreeIter -> IO TreePath,                      -- convert an interator to a path
    -- | Retrieve a row at the given iterator.
    treeModelIfaceGetRow        :: TreeIter -> IO row,                           -- get the row at an iter
    -- | Advance the given iterator to the next node at the same level.
    --   Return @Nothing@ if there is no next node at this level.
    treeModelIfaceIterNext      :: TreeIter -> IO (Maybe TreeIter),              -- following row (if any)
    -- | Advance the given iterator to the first child of this iterator.
    --   Return @Notihing@ if the node at this iterator has no children.
    treeModelIfaceIterChildren  :: Maybe TreeIter -> IO (Maybe TreeIter),        -- first child row (if any)
    -- | Check if the node at the given iterator has children.
    treeModelIfaceIterHasChild  :: TreeIter -> IO Bool,                          -- row has any children at all
    -- | Query the number of children the the node at the given iteratore has.
    treeModelIfaceIterNChildren :: Maybe TreeIter -> IO Int,                     -- number of children of a row
    -- | Ask for an iterator to the @n@th child. Return @Nothing@ if
    --   no such child exists.
    treeModelIfaceIterNthChild  :: Maybe TreeIter -> Int -> IO (Maybe TreeIter), -- nth child row of a given row
    -- | Ask for an iterator to the parent of the node.
    treeModelIfaceIterParent    :: TreeIter -> IO (Maybe TreeIter),              -- parent row of a row
    -- | Increase a reference count for this node. A positive reference count
    --   indicates that the node is used (that is, most likely it is visible)
    --   in at least one widget. Tracking reference counts for nodes is
    --   optional but may be useful to infer when a given row can be discarded
        --   if it was retrieved from an external source.
    treeModelIfaceRefNode       :: TreeIter -> IO (),                            -- caching hint
    -- | Decrement the reference count of the given node.
    treeModelIfaceUnrefNode     :: TreeIter -> IO ()                             -- caching hint
  }

-- | A structure containing functions that enable this widget to be used
--   as a source in drag-and-drop.
data DragSourceIface model row = DragSourceIface {
    -- | Determine if the row at the given path is draggable. Return
    --   @False@ if for some reason this row should not be dragged by
    --   the user.
    treeDragSourceRowDraggable  :: model row -> TreePath -> IO Bool,                 -- query if the row is draggable
    -- | Fill in the 'SelectionDataM' structure with information on
    --   the given node using
    --   'Graphics.UI.Gtk.General.Selection.selectionDataSet'.
    treeDragSourceDragDataGet   :: model row -> TreePath -> SelectionDataM Bool,     -- store row in selection object
    -- | The widget is informed that the row at the given path should
    --   be deleted as the result of this drag.
    treeDragSourceDragDataDelete:: model row -> TreePath -> IO Bool                  -- instruct store to delete the row
  }

-- | A structure containing functions that enable this widget to be used
--   as a target in drag-and-drop.
data DragDestIface model row = DragDestIface {
    -- | Tell the drag-and-drop mechanism if the row can be dropped at the
    --   given path.
    treeDragDestRowDropPossible :: model row -> TreePath -> SelectionDataM Bool,     -- query if row drop is possible
    -- | The data in the 'SelectionDataM' structure should be read using
    --   'Graphics.UI.Gtk.General.Selection.selectionDataGet' and
    --   its information be used to insert a new row at the given path.
    treeDragDestDragDataReceived:: model row -> TreePath -> SelectionDataM Bool      -- insert row from selection object
  }

-- | Create a new store that implements the 'TreeModelIface' interface and
-- optionally the 'DragSourceIface' and the 'DragDestIface'. If the latter two
-- are set to @Nothing@ a dummy interface is substituted that rejects every
-- drag and drop.
customStoreNew :: (TreeModelClass (model row), TypedTreeModelClass model) =>
     private   -- ^ Any private data the store needs to store. Usually an 'IORef'.
  -> ((CustomStore private row) -> model row)
  -> TreeModelIface row         -- ^ Functions necessary to implement the 'TreeModel' interface.
  -> Maybe (DragSourceIface model row)
                                -- ^ Functions to enable this store to generate drag events.
  -> Maybe (DragDestIface model row)
                                -- ^ Functions to enable this store to receive drag events.
  -> IO (model row)
customStoreNew priv con tmIface mDragSource mDragDest = do
  cMap <- columnMapNew
  let dummyDragSource = DragSourceIface { treeDragSourceRowDraggable = \_ _ -> return False,
                                          treeDragSourceDragDataGet  = \_ _ -> return False,
                                          treeDragSourceDragDataDelete = \_ _ -> return False }
  let dummyDragDest = DragDestIface { treeDragDestRowDropPossible = \_ _ -> return False,
                                      treeDragDestDragDataReceived = \_ _ -> return False }
  implPtr <- newStablePtr CustomStoreImplementation {
        customStoreColumns = cMap,
        customStoreIface = tmIface,
        customTreeDragSourceIface = fromMaybe dummyDragSource mDragSource,
        customTreeDragDestIface = fromMaybe dummyDragDest mDragDest }
  privPtr <- newStablePtr priv
  liftM con $ wrapNewGObject (CustomStore, objectUnref) $
    gtk2hs_store_new implPtr privPtr

foreign import ccall unsafe "Gtk2HsStore.h gtk2hs_store_new"
  gtk2hs_store_new :: StablePtr (CustomStoreImplementation model row)
                   -> StablePtr private
                   -> IO (Ptr (CustomStore private row))

-- | Extract a row of the given model at the given 'TreeIter'.
--
customStoreGetRow :: TypedTreeModelClass model => model row -> TreeIter -> IO row
customStoreGetRow model iter =
  case toTypedTreeModel model of
    TypedTreeModel model -> do
      impl <- withForeignPtr model gtk2hs_store_get_impl >>= deRefStablePtr
      treeModelIfaceGetRow (customStoreIface impl) iter

-- this is a backwards compatability definition
treeModelGetRow :: TypedTreeModelClass model => model row -> TreeIter -> IO row
treeModelGetRow = customStoreGetRow

foreign import ccall unsafe "Gtk2HsStore.h gtk2hs_store_get_impl"
  gtk2hs_store_get_impl :: Ptr (TypedTreeModel row) -> IO (StablePtr (CustomStoreImplementation model row))

-- | Return the private data stored in this 'CustomStore'. The private data
--   is meant as a container for the data stored in this model.
customStoreGetPrivate :: CustomStore private row -> private
customStoreGetPrivate (CustomStore model) =
  unsafePerformIO $ -- this is safe because the priv member is set at
                    -- construction time and never modified after that
  withForeignPtr model gtk2hs_store_get_priv >>= deRefStablePtr

foreign import ccall unsafe "Gtk2HsStore.h gtk2hs_store_get_priv"
  gtk2hs_store_get_priv :: Ptr (CustomStore private row) -> IO (StablePtr private)

-- | Query the current value of the stamp that is used to create
--   'TreeIter' iterators. The stamp is compared each time a view
--   accesses this store. If the stamp doesn't match, a warning
--   is emitted. The stamp should be updated each time a the data
--   in the model changes. The rationale is that a view should never
--   use a stale 'TreeIter', i.e., one that refers to an old model.
--
customStoreGetStamp :: CustomStore private row -> IO CInt
customStoreGetStamp (CustomStore model) =
  withForeignPtr model gtk2hs_store_get_stamp

foreign import ccall unsafe "Gtk2HsStore.h gtk2hs_store_get_stamp"
  gtk2hs_store_get_stamp :: Ptr (CustomStore private row) -> IO CInt

-- | Create a new stamp. See 'customStoreGetStamp'.
--
customStoreInvalidateIters :: CustomStore private row -> IO ()
customStoreInvalidateIters (CustomStore model) =
  withForeignPtr model gtk2hs_store_increment_stamp

foreign import ccall unsafe "Gtk2HsStore.h gtk2hs_store_increment_stamp"
  gtk2hs_store_increment_stamp :: Ptr (CustomStore private row) -> IO ()

treeModelIfaceGetNColumns_static :: StablePtr (CustomStoreImplementation model row) -> IO CInt
treeModelIfaceGetNColumns_static storePtr = do
  store <- deRefStablePtr storePtr
  cmap <- readIORef (customStoreColumns store)
  return (fromIntegral (length cmap))

foreign export ccall "gtk2hs_store_get_n_columns_impl"
  treeModelIfaceGetNColumns_static :: StablePtr (CustomStoreImplementation model row) -> IO CInt

-- Get the 'GType' for a given 'ColumnAccess'.
caToGType :: ColumnAccess row -> GType
caToGType (CAInt _) = GConst.int
caToGType (CABool _) = GConst.bool
caToGType (CAString _) = GConst.string
caToGType (CAPixbuf _) = {#call fun unsafe gdk_pixbuf_get_type#}
caToGType CAInvalid = GConst.int -- to avoid warnings of functions that iterate through all columns

treeModelIfaceGetColumnType_static :: StablePtr (CustomStoreImplementation model row) -> CInt -> IO GType
treeModelIfaceGetColumnType_static storePtr column = do
  store <- deRefStablePtr storePtr
  cols <- readIORef (customStoreColumns store)
  case drop (fromIntegral column) cols of
     [] -> return GConst.invalid
     (ca:_) -> return (caToGType ca)

foreign export ccall "gtk2hs_store_get_column_type_impl"
  treeModelIfaceGetColumnType_static :: StablePtr (CustomStoreImplementation model row) -> CInt -> IO GType


treeModelIfaceGetFlags_static :: StablePtr (CustomStoreImplementation model row) -> IO CInt
treeModelIfaceGetFlags_static storePtr = do
  store <- liftM customStoreIface $ deRefStablePtr storePtr
  liftM (fromIntegral . fromFlags) $ treeModelIfaceGetFlags store

foreign export ccall "gtk2hs_store_get_flags_impl"
  treeModelIfaceGetFlags_static :: StablePtr (CustomStoreImplementation model row) -> IO CInt


treeModelIfaceGetIter_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> Ptr NativeTreePath -> IO CInt
treeModelIfaceGetIter_static storePtr iterPtr pathPtr = do
  store <- liftM customStoreIface $ deRefStablePtr storePtr
  path <- peekTreePath pathPtr
  iter <- treeModelIfaceGetIter store path
  case iter of
    Nothing   -> return (fromBool False)
    Just iter -> do poke iterPtr iter
                    return (fromBool True)

foreign export ccall "gtk2hs_store_get_iter_impl"
  treeModelIfaceGetIter_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> Ptr NativeTreePath -> IO CInt

treeModelIfaceGetPath_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> IO (Ptr NativeTreePath)
treeModelIfaceGetPath_static storePtr iterPtr = do
  store <- liftM customStoreIface $ deRefStablePtr storePtr
  iter <- peek iterPtr
  path <- treeModelIfaceGetPath store iter
  NativeTreePath pathPtr <- newTreePath path
  return pathPtr

foreign export ccall "gtk2hs_store_get_path_impl"
  treeModelIfaceGetPath_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> IO (Ptr NativeTreePath)


treeModelIfaceGetValue_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> CInt -> Ptr GValue -> IO ()
treeModelIfaceGetValue_static storePtr iterPtr column gvaluePtr = do
  store <- deRefStablePtr storePtr
  iter <- peek iterPtr
  row <- treeModelIfaceGetRow (customStoreIface store) iter
  cols <- readIORef (customStoreColumns store)
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
  treeModelIfaceGetValue_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> CInt -> Ptr GValue -> IO ()


treeModelIfaceIterNext_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> IO CInt
treeModelIfaceIterNext_static storePtr iterPtr = do
  store <- liftM customStoreIface $ deRefStablePtr storePtr
  iter <- peek iterPtr
  iter' <- treeModelIfaceIterNext store iter
  case iter' of
    Nothing    -> return (fromBool False)
    Just iter' -> do poke iterPtr iter'
                     return (fromBool True)

foreign export ccall "gtk2hs_store_iter_next_impl"
  treeModelIfaceIterNext_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> IO CInt


treeModelIfaceIterChildren_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> Ptr TreeIter -> IO CInt
treeModelIfaceIterChildren_static storePtr iterPtr parentIterPtr = do
  store <- liftM customStoreIface $ deRefStablePtr storePtr
  parentIter <- maybeNull peek parentIterPtr
  iter <- treeModelIfaceIterChildren store parentIter
  case iter of
    Nothing   -> return (fromBool False)
    Just iter -> do poke iterPtr iter
                    return (fromBool True)

foreign export ccall "gtk2hs_store_iter_children_impl"
  treeModelIfaceIterChildren_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> Ptr TreeIter -> IO CInt


treeModelIfaceIterHasChild_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> IO CInt
treeModelIfaceIterHasChild_static storePtr iterPtr = do
  store <- liftM customStoreIface $ deRefStablePtr storePtr
  iter <- peek iterPtr
  liftM fromBool $ treeModelIfaceIterHasChild store iter

foreign export ccall "gtk2hs_store_iter_has_child_impl"
  treeModelIfaceIterHasChild_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> IO CInt


treeModelIfaceIterNChildren_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> IO CInt
treeModelIfaceIterNChildren_static storePtr iterPtr = do
  store <- liftM customStoreIface $ deRefStablePtr storePtr
  iter <- maybeNull peek iterPtr
  liftM fromIntegral $ treeModelIfaceIterNChildren store iter

foreign export ccall "gtk2hs_store_iter_n_children_impl"
  treeModelIfaceIterNChildren_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> IO CInt


treeModelIfaceIterNthChild_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> Ptr TreeIter -> CInt -> IO CInt
treeModelIfaceIterNthChild_static storePtr iterPtr parentIterPtr n = do
  store <- liftM customStoreIface $ deRefStablePtr storePtr
  parentIter <- maybeNull peek parentIterPtr
  iter <- treeModelIfaceIterNthChild store parentIter (fromIntegral n)
  case iter of
    Nothing   -> return (fromBool False)
    Just iter -> do poke iterPtr iter
                    return (fromBool True)

foreign export ccall "gtk2hs_store_iter_nth_child_impl"
  treeModelIfaceIterNthChild_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> Ptr TreeIter -> CInt -> IO CInt


treeModelIfaceIterParent_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> Ptr TreeIter -> IO CInt
treeModelIfaceIterParent_static  storePtr iterPtr childIterPtr = do
  store <- liftM customStoreIface $ deRefStablePtr storePtr
  childIter <- peek childIterPtr
  iter <- treeModelIfaceIterParent store childIter
  case iter of
    Nothing   -> return (fromBool False)
    Just iter -> do poke iterPtr iter
                    return (fromBool True)

foreign export ccall "gtk2hs_store_iter_parent_impl"
  treeModelIfaceIterParent_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> Ptr TreeIter -> IO CInt


treeModelIfaceRefNode_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> IO ()
treeModelIfaceRefNode_static storePtr iterPtr = do
  store <- liftM customStoreIface $ deRefStablePtr storePtr
  iter <- peek iterPtr
  treeModelIfaceRefNode store iter

foreign export ccall "gtk2hs_store_ref_node_impl"
  treeModelIfaceRefNode_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> IO ()


treeModelIfaceUnrefNode_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> IO ()
treeModelIfaceUnrefNode_static storePtr iterPtr = do
  store <- liftM customStoreIface $ deRefStablePtr storePtr
  iter <- peek iterPtr
  treeModelIfaceUnrefNode store iter

foreign export ccall "gtk2hs_store_unref_node_impl"
  treeModelIfaceUnrefNode_static :: StablePtr (CustomStoreImplementation model row) -> Ptr TreeIter -> IO ()

treeDragSourceRowDraggable_static :: Ptr TreeModel -> StablePtr (CustomStoreImplementation model row) -> Ptr NativeTreePath -> IO CInt
treeDragSourceRowDraggable_static mPtr storePtr pathPtr = do
  model <- makeNewGObject mkTreeModel (return mPtr)
  store <- liftM customTreeDragSourceIface $ deRefStablePtr storePtr
  path <- peekTreePath pathPtr
  liftM fromBool $ treeDragSourceRowDraggable store (unsafeTreeModelToGeneric model) path

foreign export ccall "gtk2hs_store_row_draggable_impl"
  treeDragSourceRowDraggable_static :: Ptr TreeModel -> StablePtr (CustomStoreImplementation model row) -> Ptr NativeTreePath -> IO CInt

treeDragSourceDragDataGet_static :: Ptr TreeModel -> StablePtr (CustomStoreImplementation model row) -> Ptr NativeTreePath -> SelectionData -> IO CInt
treeDragSourceDragDataGet_static mPtr storePtr pathPtr selectionPtr = do
  model <- makeNewGObject mkTreeModel (return mPtr)
  store <- liftM customTreeDragSourceIface $ deRefStablePtr storePtr
  path <- peekTreePath pathPtr
  liftM fromBool $ runReaderT (treeDragSourceDragDataGet store (unsafeTreeModelToGeneric model) path) selectionPtr

foreign export ccall "gtk2hs_store_drag_data_get_impl"
  treeDragSourceDragDataGet_static :: Ptr TreeModel -> StablePtr (CustomStoreImplementation model row) -> Ptr NativeTreePath -> SelectionData -> IO CInt

treeDragSourceDragDataDelete_static :: Ptr TreeModel -> StablePtr (CustomStoreImplementation model row) -> Ptr NativeTreePath -> IO CInt
treeDragSourceDragDataDelete_static mPtr storePtr pathPtr = do
  model <- makeNewGObject mkTreeModel (return mPtr)
  store <- liftM customTreeDragSourceIface $ deRefStablePtr storePtr
  path <- peekTreePath pathPtr
  liftM fromBool $ treeDragSourceDragDataDelete store (unsafeTreeModelToGeneric model) path

foreign export ccall "gtk2hs_store_drag_data_delete_impl"
  treeDragSourceDragDataDelete_static :: Ptr TreeModel -> StablePtr (CustomStoreImplementation model row) -> Ptr NativeTreePath -> IO CInt

treeDragDestDragDataReceived_static :: Ptr TreeModel -> StablePtr (CustomStoreImplementation model row) -> Ptr NativeTreePath -> SelectionData -> IO CInt
treeDragDestDragDataReceived_static mPtr storePtr pathPtr selectionPtr = do
  model <- makeNewGObject mkTreeModel (return mPtr)
  store <- liftM customTreeDragDestIface $ deRefStablePtr storePtr
  path <- peekTreePath pathPtr
  liftM fromBool $ runReaderT (treeDragDestDragDataReceived store (unsafeTreeModelToGeneric model) path) selectionPtr

foreign export ccall "gtk2hs_store_drag_data_received_impl"
  treeDragDestDragDataReceived_static :: Ptr TreeModel -> StablePtr (CustomStoreImplementation model row) -> Ptr NativeTreePath -> SelectionData -> IO CInt

treeDragDestRowDropPossible_static :: Ptr TreeModel -> StablePtr (CustomStoreImplementation model row) -> Ptr NativeTreePath -> SelectionData -> IO CInt
treeDragDestRowDropPossible_static mPtr storePtr pathPtr selectionPtr = do
  model <- makeNewGObject mkTreeModel (return mPtr)
  store <- liftM customTreeDragDestIface $ deRefStablePtr storePtr
  path <- peekTreePath pathPtr
  liftM fromBool $ runReaderT (treeDragDestRowDropPossible store (unsafeTreeModelToGeneric model) path) selectionPtr

foreign export ccall "gtk2hs_store_row_drop_possible_impl"
  treeDragDestRowDropPossible_static :: Ptr TreeModel -> StablePtr (CustomStoreImplementation model row) -> Ptr NativeTreePath -> SelectionData -> IO CInt

maybeNull :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
maybeNull marshal ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = liftM Just (marshal ptr)
