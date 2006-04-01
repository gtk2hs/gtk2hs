
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
import Graphics.UI.Gtk.TreeList.Types (TypedTreeModelClass)
import Graphics.UI.Gtk.TreeList.TreeModel (TreeModelFlags(TreeModelListOnly))
import Graphics.UI.Gtk.TreeList.CustomStore
import Graphics.UI.Gtk.TreeList.TreeIter

newtype ListStore a = ListStore (CustomTreeModel (IORef (Seq a)) a)

instance GObjectClass (ListStore a)
instance TreeModelClass (ListStore a)
instance TypedTreeModelClass ListStore

listStoreNew :: [a] -> IO (ListStore a)
listStoreNew xs = do
  rows <- newIORef (Seq.fromList xs)

  liftM ListStore $ customTreeModelNew rows CustomTreeModelImplementation {
      customTreeModelGetFlags      = return [TreeModelListOnly],
--      customTreeModelGetNColumns   = case bounds cols of (_, upper) -> return (upper + 1),
--      customTreeModelGetColumnType = \n -> return (columnGType (cols ! n)),
      customTreeModelGetIter       = \[n] -> return (Just (TreeIter 0 (fromIntegral n) 0 0)),
      customTreeModelGetPath       = \(TreeIter _ n _ _) -> return [fromIntegral n],
--      customTreeModelGetValue      = \(TreeIter _ n _ _) i gvalue ->
--                                 readIORef rows >>= \rows ->
--                                 -- TODO: add caching of last lookup as a view
--                                   columnSetGValue (cols ! i)
--                                                   (rows `Seq.index` fromIntegral n)
--                                                   gvalue,
      customTreeModelGetRow        = \(TreeIter _ n _ _) ->
                                 readIORef rows >>= \rows -> 
                                 if inRange (0, Seq.length rows - 1) (fromIntegral n)
                                   then return (rows `Seq.index` fromIntegral n)
                                   else fail "ListStore.getRow: iter does not refer to a valid entry",

      customTreeModelIterNext      = \(TreeIter _ n _ _) ->
                                 readIORef rows >>= \rows ->
                                    if n >= fromIntegral (Seq.length rows) - 1
                                      then return Nothing
                                      else return (Just (TreeIter 0 (n+1) 0 0)),
      customTreeModelIterChildren  = \_ -> return Nothing,
      customTreeModelIterHasChild  = \_ -> return False,
      customTreeModelIterNChildren = \index -> readIORef rows >>= \rows ->
                                           case index of
                                             Nothing -> return $! Seq.length rows
                                             _       -> return 0,
      customTreeModelIterNthChild  = \index n -> case index of
                                               Nothing -> return (Just (TreeIter 0 (fromIntegral n) 0 0))
                                               _       -> return Nothing,
      customTreeModelIterParent    = \_ -> return Nothing,
      customTreeModelRefNode       = \_ -> return (),
      customTreeModelUnrefNode     = \_ -> return ()
    }

listStoreSetValue :: ListStore a -> Int -> a -> IO ()
listStoreSetValue (ListStore model) index value = do
  modifyIORef (customTreeModelGetPrivate model) (Seq.update index value)
  treeModelRowChanged model [index] (TreeIter 0 (fromIntegral index) 0 0)

listStoreInsert :: ListStore a -> Int -> a -> IO ()
listStoreInsert (ListStore model) index value = do
  seq <- readIORef (customTreeModelGetPrivate model)
  when (index >= 0) $ do
    let index' | index > Seq.length seq = Seq.length seq
               | otherwise              = index
    writeIORef (customTreeModelGetPrivate model) (insert index' value seq)
    treeModelRowInserted model [index'] (TreeIter 0 (fromIntegral index') 0 0)

  where insert :: Int -> a -> Seq a -> Seq a
        insert i x xs = front Seq.>< x Seq.<| back
          where (front, back) = Seq.splitAt i xs

listStorePrepend :: ListStore a -> a -> IO ()
listStorePrepend (ListStore model) value = do
  modifyIORef (customTreeModelGetPrivate model)
              (\seq -> value Seq.<| seq)
  treeModelRowInserted model [0] (TreeIter 0 0 0 0)

listStorePrependList :: ListStore a -> [a] -> IO ()
listStorePrependList = undefined

listStoreAppend :: ListStore a -> a -> IO ()
listStoreAppend (ListStore model) value = do
  index <- atomicModifyIORef (customTreeModelGetPrivate model)
                             (\seq -> (seq Seq.|> value, Seq.length seq))
  treeModelRowInserted model [index] (TreeIter 0 (fromIntegral index) 0 0)
{-
listStoreAppendList :: ListStore a -> [a] -> IO ()
listStoreAppendList (ListStore model) values = do
  seq <- readIORef (customTreeModelGetPrivate model)
  let seq' = Seq.fromList values
      startIndex = Seq.length seq
      endIndex = startIndex + Seq.length seq' - 1
  writeIORef (customTreeModelGetPrivate model) (seq Seq.>< seq')
  flip mapM [startIndex..endIndex] $ \index ->    
    treeModelRowInserted model [index] (TreeIter 0 (fromIntegral index) 0 0)
-}
listStoreRemove :: ListStore a -> Int -> IO ()
listStoreRemove (ListStore model) index = do
  seq <- readIORef (customTreeModelGetPrivate model)
  when (index >=0 && index < Seq.length seq) $ do
    writeIORef (customTreeModelGetPrivate model) (delete index seq)
    treeModelRowDeleted model [index]
  --TODO we should probably fail on a bad index

  where delete :: Int -> Seq a -> Seq a
        delete i xs = front Seq.>< Seq.drop 1 back
          where (front, back) = Seq.splitAt i xs

listStoreClear :: ListStore a -> IO ()
listStoreClear (ListStore model) = do
  seq <- readIORef (customTreeModelGetPrivate model)
  writeIORef (customTreeModelGetPrivate model) Seq.empty
  let loop 0 = treeModelRowDeleted model [0]
      loop n = treeModelRowDeleted model [n] >> loop (n-1)
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
