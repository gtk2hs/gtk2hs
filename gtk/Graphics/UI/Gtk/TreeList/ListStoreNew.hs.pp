
module Graphics.UI.Gtk.TreeList.ListStoreNew (
  ListStore,
  listStoreNew,
  
  listStoreSetValue,
  listStoreInsert,
  listStorePrepend,
  listStoreAppend,
  listStoreRemove,
  listStoreClear,
  ) where

import Monad (liftM, when)
import Data.IORef
import Data.Ix (inRange)
import Foreign.C.Types (CInt)

#if __GLASGOW_HASKELL__>=606
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
#else
import qualified Graphics.UI.Gtk.TreeList.Sequence as Seq
import Graphics.UI.Gtk.TreeList.Sequence (Seq)
#endif

import Graphics.UI.Gtk.Types (GObjectClass, TreeModelClass)
import Graphics.UI.Gtk.TreeList.TreeModel (TreeModelFlags(TreeModelListOnly))
import Graphics.UI.Gtk.TreeList.CustomStore
import Graphics.UI.Gtk.TreeList.TreeIter

import System.IO ( hPutStr, hPutChar, hFlush, stderr )
import System.Mem ( performGC )

putStrLn str = hPutStr stderr str >> hPutChar stderr '\n'
putStr str = hPutStr stderr str
flush = hFlush stderr

data ListStore a = ListStore {
    model :: TreeModel,
    rows :: IORef (Seq a)
  }

instance StoreClass ListStore where
  storeGetModel = model
  storeGetValue ListStore { rows = rowsRef } (TreeIter _ n _ _) = do
      rows <- readIORef rowsRef
      return (rows `Seq.index` fromIntegral n)

instance GObjectClass (ListStore a)
instance TreeModelClass (ListStore a)
instance TypedTreeModelClass ListStore where
  treeModelGetRow store (TreeIter _ n _ _) =
    readIORef (storeData store) >>= \rows -> 
    if inRange (0, Seq.length rows - 1) (fromIntegral n)
      then return (rows `Seq.index` fromIntegral n)
      else fail "ListStore.getRow: iter does not refer to a valid entry"

listStoreNew :: [a] -> IO (ListStore a)
listStoreNew xs = do
  rows <- newIORef (Seq.fromList xs)

  liftM ListStore $ customStoreNew rows
    CustomStore {
      customStoreGetFlags      = return [TreeModelListOnly],
--      customStoreGetNColumns   = case bounds cols of (_, upper) -> return (upper + 1),
--      customStoreGetColumnType = \n -> return (columnGType (cols ! n)),
      customStoreGetIter       = \[n] -> return (Just (TreeIter 0 (fromIntegral n) 0 0)),
      customStoreGetPath       = \(TreeIter _ n _ _) -> return [fromIntegral n],
--      customStoreGetValue      = \(TreeIter _ n _ _) i gvalue ->
--                                 readIORef rows >>= \rows ->
--                                 -- TODO: add caching of last lookup as a view
--                                   columnSetGValue (cols ! i)
--                                                   (rows `Seq.index` fromIntegral n)
--                                                   gvalue,
      customStoreIterNext      = \(TreeIter _ n _ _) ->
                                 readIORef rows >>= \rows ->
                                    if n >= fromIntegral (Seq.length rows) - 1
                                      then return Nothing
                                      else return (Just (TreeIter 0 (n+1) 0 0)),
      customStoreIterChildren  = \_ -> return Nothing,
      customStoreIterHasChild  = \_ -> return False,
      customStoreIterNChildren = \index -> readIORef rows >>= \rows ->
                                           case index of
                                             Nothing -> return $! Seq.length rows
                                             _       -> return 0,
      customStoreIterNthChild  = \index n -> case index of
                                               Nothing -> return (Just (TreeIter 0 (fromIntegral n) 0 0))
                                               _       -> return Nothing,
      customStoreIterParent    = \_ -> return Nothing,
      customStoreRefNode       = \_ -> return (),
      customStoreUnrefNode     = \_ -> return ()
    }

storeData :: ListStore a -> IORef (Seq a)
storeData (ListStore store) = customStoreGetPrivate store

storeStamp :: ListStore a -> IO CInt
storeStamp (ListStore store) = customStoreGetStamp store

listStoreSetValue :: ListStore a -> Int -> a -> IO ()
listStoreSetValue store index value = do
  modifyIORef (storeData store) (Seq.update index value)
  treeModelRowChanged store [index] (TreeIter 0 (fromIntegral index) 0 0)

listStoreInsert :: ListStore a -> Int -> a -> IO ()
listStoreInsert store index value = do
  seq <- readIORef (storeData store)
  when (index >= 0) $ do
    let index' | index > Seq.length seq = Seq.length seq
               | otherwise              = index
    writeIORef (storeData store) (insert index' value seq)
    stamp <- storeStamp store
    treeModelRowInserted store [index'] (TreeIter stamp (fromIntegral index') 0 0)

  where insert :: Int -> a -> Seq a -> Seq a
        insert i x xs = front Seq.>< x Seq.<| back
          where (front, back) = Seq.splitAt i xs

listStorePrepend :: ListStore a -> a -> IO ()
listStorePrepend store value = do
  modifyIORef (storeData store) (\seq -> value Seq.<| seq)
  stamp <- storeStamp store
  treeModelRowInserted store [0] (TreeIter stamp 0 0 0)

listStorePrependList :: ListStore a -> [a] -> IO ()
listStorePrependList = undefined

listStoreAppend :: ListStore a -> a -> IO ()
listStoreAppend store value = do
  index <- atomicModifyIORef (storeData store) (\seq -> (seq Seq.|> value, Seq.length seq))
  stamp <- storeStamp store
  treeModelRowInserted store [index] (TreeIter stamp (fromIntegral index) 0 0)
{-
listStoreAppendList :: ListStore a -> [a] -> IO ()
listStoreAppendList store values = do
  seq <- readIORef (storeData store)
  let seq' = Seq.fromList values
      startIndex = Seq.length seq
      endIndex = startIndex + Seq.length seq' - 1
  writeIORef (storeData store) (seq Seq.>< seq')
  stamp <- storeStamp store
  flip mapM [startIndex..endIndex] $ \index ->    
    treeModelRowInserted store [index] (TreeIter stamp (fromIntegral index) 0 0)
-}
listStoreRemove :: ListStore a -> Int -> IO ()
listStoreRemove store index = do
  seq <- readIORef (storeData store)
  when (index >=0 && index < Seq.length seq) $ do
    writeIORef (storeData store) (delete index seq)
    treeModelRowDeleted store [index]
  --TODO we should probably fail on a bad index

  where delete :: Int -> Seq a -> Seq a
        delete i xs = front Seq.>< Seq.drop 1 back
          where (front, back) = Seq.splitAt i xs

listStoreClear :: ListStore a -> IO ()
listStoreClear store = do
  seq <- readIORef (storeData store)
  writeIORef (storeData store) Seq.empty
  let loop 0 = treeModelRowDeleted store [0]
      loop n = treeModelRowDeleted store [n] >> loop (n-1)
  loop (Seq.length seq - 1)

-- moving rows about
listStoreReorder :: ListStore a -> [Int] -> IO ()
listStoreReorder store = undefined

listStoreSwap :: ListStore a -> Int -> Int -> IO ()
listStoreSwap store = undefined

listStoreMoveBefore :: ListStore a -> Int -> Int -> IO ()
listStoreMoveBefore store = undefined

listStoreMoveAfter :: ListStore a -> Int -> Int -> IO ()
listStoreMoveAfter store = undefined
