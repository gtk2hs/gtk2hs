{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) CustomStore TreeModel
--
--  Author : Duncan Coutts, Axel Simon
--
--  Created: 11 Feburary 2006
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
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Standard model to store list data.
--
module Graphics.UI.Gtk.ModelView.ListStore (

-- * Types
  ListStore,

-- * Constructors
  listStoreNew,
  listStoreNewDND,

-- * Implementation of Interfaces
  listStoreDefaultDragSourceIface,
  listStoreDefaultDragDestIface,

-- * Methods
  listStoreIterToIndex,
  listStoreGetValue,
  listStoreSafeGetValue,
  listStoreSetValue,
  listStoreToList,
  listStoreGetSize,
  listStoreInsert,
  listStorePrepend,
  listStoreAppend,
  listStoreRemove,
  listStoreClear,
  ) where

import Control.Monad (liftM, when)
import Data.IORef
import Data.Ix (inRange)

#if __GLASGOW_HASKELL__>=606
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Foldable as F
#else
import qualified Graphics.UI.Gtk.ModelView.Sequence as Seq
import Graphics.UI.Gtk.ModelView.Sequence (Seq)
#endif

import Graphics.UI.Gtk.Types (GObjectClass(..))
-- import Graphics.UI.Gtk.ModelView.Types ()
import Graphics.UI.Gtk.ModelView.CustomStore
import Graphics.UI.Gtk.ModelView.TreeModel
import Graphics.UI.Gtk.ModelView.TreeDrag
import Control.Monad.Trans ( liftIO )

newtype ListStore a = ListStore (CustomStore (IORef (Seq a)) a)

instance TypedTreeModelClass ListStore
instance TreeModelClass (ListStore a)
instance GObjectClass (ListStore a) where
  toGObject (ListStore tm) = toGObject tm
  unsafeCastGObject = ListStore . unsafeCastGObject

-- | Create a new 'TreeModel' that contains a list of elements.
listStoreNew :: [a] -> IO (ListStore a)
listStoreNew xs = listStoreNewDND xs (Just listStoreDefaultDragSourceIface)
                                     (Just listStoreDefaultDragDestIface)

-- | Create a new 'TreeModel' that contains a list of elements. In addition, specify two
--   interfaces for drag and drop.
--
listStoreNewDND :: [a] -- ^ the initial content of the model
  -> Maybe (DragSourceIface ListStore a) -- ^ an optional interface for drags
  -> Maybe (DragDestIface ListStore a) -- ^ an optional interface to handle drops
  -> IO (ListStore a) -- ^ the new model
listStoreNewDND xs mDSource mDDest = do
  rows <- newIORef (Seq.fromList xs)

  customStoreNew rows ListStore TreeModelIface {
      treeModelIfaceGetFlags      = return [TreeModelListOnly],
      treeModelIfaceGetIter       = \[n] -> readIORef rows >>= \rows ->
                                     return (if inRange (0, Seq.length rows - 1) n
                                                 then Just (TreeIter 0 (fromIntegral n) 0 0)
                                                 else Nothing),
      treeModelIfaceGetPath       = \(TreeIter _ n _ _) -> return [fromIntegral n],
      treeModelIfaceGetRow        = \(TreeIter _ n _ _) ->
                                 readIORef rows >>= \rows ->
                                 if inRange (0, Seq.length rows - 1) (fromIntegral n)
                                   then return (rows `Seq.index` fromIntegral n)
                                   else fail "ListStore.getRow: iter does not refer to a valid entry",

      treeModelIfaceIterNext      = \(TreeIter _ n _ _) ->
                                 readIORef rows >>= \rows ->
                                 if inRange (0, Seq.length rows - 1) (fromIntegral (n+1))
                                   then return (Just (TreeIter 0 (n+1) 0 0))
                                   else return Nothing,
      treeModelIfaceIterChildren  = \index -> readIORef rows >>= \rows ->
                                         case index of
                                             Nothing | not (Seq.null rows) ->
                                                        return (Just (TreeIter 0 0 0 0))
                                             _       -> return Nothing,
      treeModelIfaceIterHasChild  = \_ -> return False,
      treeModelIfaceIterNChildren = \index -> readIORef rows >>= \rows ->
                                           case index of
                                             Nothing -> return $! Seq.length rows
                                             _       -> return 0,
      treeModelIfaceIterNthChild  = \index n -> case index of
                                               Nothing -> return (Just (TreeIter 0 (fromIntegral n) 0 0))
                                               _       -> return Nothing,
      treeModelIfaceIterParent    = \_ -> return Nothing,
      treeModelIfaceRefNode       = \_ -> return (),
      treeModelIfaceUnrefNode     = \_ -> return ()
    } mDSource mDDest


-- | Convert a 'TreeIter' to an an index into the 'ListStore'. Note that this
--   function merely extracts the second element of the 'TreeIter'.
listStoreIterToIndex :: TreeIter -> Int
listStoreIterToIndex (TreeIter _ n _ _) = fromIntegral n

-- | Default drag functions for 'Graphics.UI.Gtk.ModelView.ListStore'. These
-- functions allow the rows of the model to serve as drag source. Any row is
-- allowed to be dragged and the data set in the 'SelectionDataM' object is
-- set with 'treeSetRowDragData', i.e. it contains the model and the
-- 'TreePath' to the row.
listStoreDefaultDragSourceIface :: DragSourceIface ListStore row
listStoreDefaultDragSourceIface = DragSourceIface {
    treeDragSourceRowDraggable = \_ _-> return True,
    treeDragSourceDragDataGet = treeSetRowDragData,
    treeDragSourceDragDataDelete = \model (dest:_) -> do
            liftIO $ listStoreRemove model dest
            return True

  }

-- | Default drop functions for 'Graphics.UI.Gtk.ModelView.ListStore'. These
--   functions accept a row and insert the row into the new location if it is
--   dragged into a tree view
-- that uses the same model.
listStoreDefaultDragDestIface :: DragDestIface ListStore row
listStoreDefaultDragDestIface = DragDestIface {
    treeDragDestRowDropPossible = \model dest -> do
      mModelPath <- treeGetRowDragData
      case mModelPath of
        Nothing -> return False
        Just (model', source) -> return (toTreeModel model==toTreeModel model'),
    treeDragDestDragDataReceived = \model (dest:_) -> do
      mModelPath <- treeGetRowDragData
      case mModelPath of
        Nothing -> return False
        Just (model', (source:_)) ->
          if toTreeModel model/=toTreeModel model' then return False
          else liftIO $ do
            row <- listStoreGetValue model source
            listStoreInsert model dest row
            return True
  }

-- | Extract the value at the given index.
--
listStoreGetValue :: ListStore a -> Int -> IO a
listStoreGetValue (ListStore model) index =
  readIORef (customStoreGetPrivate model) >>= return . (`Seq.index` index)

-- | Extract the value at the given index.
--
listStoreSafeGetValue :: ListStore a -> Int -> IO (Maybe a)
listStoreSafeGetValue (ListStore model) index = do
  seq <- readIORef (customStoreGetPrivate model)
  return $ if index >=0 && index < Seq.length seq
                then Just $ seq `Seq.index` index
                else Nothing

-- | Update the value at the given index. The index must exist.
--
listStoreSetValue :: ListStore a -> Int -> a -> IO ()
listStoreSetValue (ListStore model) index value = do
  modifyIORef (customStoreGetPrivate model) (Seq.update index value)
  stamp <- customStoreGetStamp model
  treeModelRowChanged model [index] (TreeIter stamp (fromIntegral index) 0 0)

-- | Extract all data from the store.
--
listStoreToList :: ListStore a -> IO [a]
listStoreToList (ListStore model) =
  liftM
#if __GLASGOW_HASKELL__>=606
  F.toList
#else
  Seq.toList
#endif
  $ readIORef (customStoreGetPrivate model)

-- | Query the number of elements in the store.
listStoreGetSize :: ListStore a -> IO Int
listStoreGetSize (ListStore model) =
  liftM Seq.length $ readIORef (customStoreGetPrivate model)

-- | Insert an element in front of the given element. The element is appended
-- if the index is greater or equal to the size of the list.
listStoreInsert :: ListStore a -> Int -> a -> IO ()
listStoreInsert (ListStore model) index value = do
  seq <- readIORef (customStoreGetPrivate model)
  when (index >= 0) $ do
    let index' | index > Seq.length seq = Seq.length seq
               | otherwise              = index
    writeIORef (customStoreGetPrivate model) (insert index' value seq)
    stamp <- customStoreGetStamp model
    treeModelRowInserted model [index'] (TreeIter stamp (fromIntegral index') 0 0)

  where insert :: Int -> a -> Seq a -> Seq a
        insert i x xs = front Seq.>< x Seq.<| back
          where (front, back) = Seq.splitAt i xs

-- | Prepend the element to the store.
listStorePrepend :: ListStore a -> a -> IO ()
listStorePrepend (ListStore model) value = do
  modifyIORef (customStoreGetPrivate model)
              (\seq -> value Seq.<| seq)
  stamp <- customStoreGetStamp model
  treeModelRowInserted model [0] (TreeIter stamp 0 0 0)

---- | Prepend a list to the store. Not implemented yet.
--listStorePrependList :: ListStore a -> [a] -> IO ()
--listStorePrependList store list =
--  mapM_ (listStoreInsert store 0) (reverse list)

-- | Append an element to the store. Returns the index of the inserted
-- element.
listStoreAppend :: ListStore a -> a -> IO Int
listStoreAppend (ListStore model) value = do
  index <- atomicModifyIORef (customStoreGetPrivate model)
                             (\seq -> (seq Seq.|> value, Seq.length seq))
  stamp <- customStoreGetStamp model
  treeModelRowInserted model [index] (TreeIter stamp (fromIntegral index) 0 0)
  return index

{-
listStoreAppendList :: ListStore a -> [a] -> IO ()
listStoreAppendList (ListStore model) values = do
  seq <- readIORef (customStoreGetPrivate model)
  let seq' = Seq.fromList values
      startIndex = Seq.length seq
      endIndex = startIndex + Seq.length seq' - 1
  writeIORef (customStoreGetPrivate model) (seq Seq.>< seq')
  stamp <- customStoreGetStamp model
  flip mapM [startIndex..endIndex] $ \index ->
    treeModelRowInserted model [index] (TreeIter stamp (fromIntegral index) 0 0)
-}

-- | Remove the element at the given index.
--
listStoreRemove :: ListStore a -> Int -> IO ()
listStoreRemove (ListStore model) index = do
  seq <- readIORef (customStoreGetPrivate model)
  when (index >=0 && index < Seq.length seq) $ do
    writeIORef (customStoreGetPrivate model) (delete index seq)
    treeModelRowDeleted model [index]
  where delete :: Int -> Seq a -> Seq a
        delete i xs = front Seq.>< Seq.drop 1 back
          where (front, back) = Seq.splitAt i xs

-- | Empty the store.
listStoreClear :: ListStore a -> IO ()
listStoreClear (ListStore model) =

  -- Since deleting rows can cause callbacks (eg due to selection changes)
  -- we have to make sure the model is consitent with the view at each
  -- intermediate step of clearing the store. Otherwise at some intermediate
  -- stage when the view has only been informed about some delections, the
  -- user might query the model expecting to find the remaining rows are there
  -- but find them deleted. That'd be bad.
  --
  let loop (-1) Seq.EmptyR = return ()
      loop n (seq Seq.:> _) = do
        writeIORef (customStoreGetPrivate model) seq
        treeModelRowDeleted model [n]
        loop (n-1) (Seq.viewr seq)

   in do seq <- readIORef (customStoreGetPrivate model)
         loop (Seq.length seq - 1) (Seq.viewr seq)

---- | Permute the rows of the store. Not yet implemented.
--listStoreReorder :: ListStore a -> [Int] -> IO ()
--listStoreReorder store = undefined
--
---- | Swap two rows of the store. Not yet implemented.
--listStoreSwap :: ListStore a -> Int -> Int -> IO ()
--listStoreSwap store = undefined
--
---- | Move the element at the first index in front of the element denoted by
---- the second index. Not yet implemented.
--listStoreMoveBefore :: ListStore a -> Int -> Int -> IO ()
--listStoreMoveBefore store = undefined
--
---- | Move the element at the first index past the element denoted by the
---- second index. Not yet implemented.
--listStoreMoveAfter :: ListStore a -> Int -> Int -> IO ()
--listStoreMoveAfter store = undefined

