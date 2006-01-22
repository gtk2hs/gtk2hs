module Graphics.UI.Gtk.TreeList.TreeStoreNew (
  TreeStore,
  treeStoreNew,
  treeStoreGetTreeModel
  ) where

import Data.Bits
import Data.Word (Word)
import Data.Ix (inRange)
import qualified Data.Array as Array
import qualified Data.Tree as Tree
import Data.Tree (Tree)
import Control.Exception (assert)
import Data.IORef

#if __GLASGOW_HASKELL__>=606
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
#else
import qualified Graphics.UI.Gtk.TreeList.Sequence as Seq
import Graphics.UI.Gtk.TreeList.Sequence (Seq)
#endif

import Graphics.UI.Gtk.TreeList.TreeModel
import Graphics.UI.Gtk.TreeList.CustomStore
import Graphics.UI.Gtk.TreeList.TreeIter
import Graphics.UI.Gtk.TreeList.Column


data TreeStore a = TreeStore {
    model :: TreeModel,
    forest :: IORef (SeqForest a)
  }

treeStoreGetTreeModel = model

--------------------------------------------
-- internal model data types
--

data SeqTree a = Node a !(SeqForest a)
               | Leaf a
type SeqForest a = Seq (SeqTree a)

label :: SeqTree a -> a
label (Node a _) = a
label (Leaf a)   = a

mkSeqForest :: [Tree a] -> SeqForest a
mkSeqForest trees = Seq.fromList (map mkSeqTree trees)

mkSeqTree :: Tree a -> SeqTree a
mkSeqTree (Tree.Node a [])    = Leaf a
mkSeqTree (Tree.Node a trees) = Node a (mkSeqForest trees)

--------------------------------------------
-- the actual TreeStore implementation
--

treeStoreNew :: [Column a] -> [Tree a] -> IO (TreeStore a)
treeStoreNew rs ts = do
  let cols = Array.listArray (0, length rs - 1) rs
  forest <- newIORef (mkSeqForest ts)

  model <- customStoreNew $
    CustomStore {
    customStoreGetFlags      = return [],
    customStoreGetNColumns   = return (length rs),
    customStoreGetColumnType = \n -> return $! columnGType (cols Array.! n),

    customStoreGetIter       = \path -> readIORef forest >>= \forest ->
                               return $! pathToIter forest path,
    customStoreGetPath       = \iter -> readIORef forest >>= \forest ->
                               return $! iterToPath forest iter,

    customStoreGetValue      = \iter i gvalue -> readIORef forest >>= \forest ->
                                 case forest `lookupNode` iter of
                                   (node, _, _, _, _) ->
                                     columnSetGValue (cols Array.! i) (label node) gvalue,

    customStoreIterNext      = \iter -> readIORef forest >>= \forest ->
                               return $! treeStoreIterNext forest iter,
    customStoreIterChildren  = \iter -> readIORef forest >>= \forest ->
                               return $! treeStoreIterChildren forest iter,
    customStoreIterHasChild  = \iter -> readIORef forest >>= \forest ->
                               return $! treeStoreIterHasChild forest iter,
    customStoreIterNChildren = \iter -> readIORef forest >>= \forest ->
                               return $! treeStoreIterNChildren forest iter,
    customStoreIterNthChild  = \iter n -> readIORef forest >>= \forest ->
                               return $! treeStoreIterNthChild forest iter n,
    customStoreIterParent    = \iter -> readIORef forest >>= \forest ->
                               return $! treeStoreIterParent forest iter,

    customStoreRefNode       = \_ -> return (),
    customStoreUnrefNode     = \_ -> return ()
  }

  return TreeStore {
    model = model,
    forest = forest
  }

treeStoreIterNext :: SeqForest a -> TreeIter -> Maybe TreeIter
treeStoreIterNext forest iter =
  case forest `lookupNode` iter of
    (_, forest, index, off, count)
      | inRange (bounds forest) index -> Just $! setBitSlice iter off count (index+1)
      | otherwise                     -> Nothing

treeStoreIterChildren forest Nothing
  | forestElemCount forest == 0 = Nothing
  | otherwise                   = Just $! TreeIter 0 1 0 0
treeStoreIterChildren forest (Just iter) =
  case forest `lookupNode` iter of
    (Leaf _, _, _, _, _) -> Nothing
    (Node _ forest, _, index, off, count) ->
      let count' = forestBitsNeeded forest
       in Just $! setBitSlice iter (off+count) count' 1

treeStoreIterHasChild forest iter =
  case forest `lookupNode` iter of
    (Leaf _,   _, _, _, _) -> False
    (Node _ _, _, _, _, _) -> True

treeStoreIterNChildren forest Nothing = forestElemCount forest
treeStoreIterNChildren forest (Just iter) =
  case forest `lookupNode` iter of
    (Leaf _,        _, _, _, _) -> 0
    (Node _ forest, _, _, _, _) -> forestElemCount forest

treeStoreIterNthChild forest Nothing n = Just $! TreeIter 0 (fromIntegral n + 1) 0 0
treeStoreIterNthChild forest (Just iter) n =
  case forest `lookupNode` iter of
    (Leaf _,        _, _, _, _) -> Nothing
    (Node _ forest, _, _, off, count) ->
      let count' = forestBitsNeeded forest
       in Just $! setBitSlice iter (off+count) count' (fromIntegral n + 1)

treeStoreIterParent forest iter =
  case forest `lookupNode` iter of
    (_, forest, _, 0,   count) -> Nothing
    (_, forest, _, off, count) -> Just $! setBitSlice iter off count 0

--------------------------------------------
-- lookup functions
--

lookupNode :: SeqForest a -> TreeIter -> (SeqTree a, SeqForest a, Word, Int, Int)
lookupNode forest iter =
  let count = forestBitsNeeded forest
      index = getBitSlice iter 0 count
   in lookup forest index 0 count

  where
    lookup ::
        SeqForest a   -- the forest of trees we are currently looking in
     -> Word          -- the index of the node in the forest (non-zero)
     -> Int           -- the iterator bit offset for the node in the forest
     -> Int           -- the number of bits needed to index the current forest

     -> (SeqTree a,   -- the node we found
         SeqForest a, -- the forrest the node is part of
         Word,        -- the index of the node within the forest
         Int,         -- the iterator bit offset for the node we found
         Int)         -- the iterator bit count for the forest
    lookup forest index bit count =
          assert (index > 0) $
          case forest ! (index - 1) of
            node@(Leaf _)         -> (node, forest, index, bit, count)
            node@(Node _ forest') ->
              let count' = forestBitsNeeded forest'
               in case getBitSlice iter (bit+count) count' of
                    0     -> (node, forest, index, bit, count)
                    index -> lookup forest' index (bit+count) count'

iterToPath :: SeqForest a -> TreeIter -> TreePath
iterToPath forest iter = lookup forest 0
  where
    lookup :: SeqForest a -> Int -> TreePath
    lookup forest off =
      let count = forestBitsNeeded forest
       in case getBitSlice iter off count of 
            0 -> []
            index ->
              case forest ! (index - 1) of
                Leaf _        -> fromIntegral index - 1 : []
                Node _ forest -> fromIntegral index - 1
                               : lookup forest (off+count)

pathToIter :: SeqForest a -> TreePath -> Maybe TreeIter
pathToIter forest []   = Nothing
pathToIter forest path = lookup forest path 0 (TreeIter 0 0 0 0)
  where
    lookup :: SeqForest a -> TreePath -> Int -> TreeIter -> Maybe TreeIter
    lookup forest []           off iter = Just iter
    lookup forest (index:path) off iter =
      let count  = forestBitsNeeded forest
          index' = fromIntegral index + 1
          iter'  = setBitSlice iter off count index'
       in if not $ inRange (bounds forest) (index' - 1)
            then Nothing
            else case forest ! (index' - 1) of
                   Leaf _ | null path -> Just $! iter'
                          | otherwise -> Nothing
                   Node _ forest -> lookup forest path (off + count) iter'

forestBitsNeeded :: SeqForest a -> Int
forestBitsNeeded forest = bitsNeeded (fromIntegral $ Seq.length forest + 1)

forestElemCount ::  SeqForest a -> Int
forestElemCount forest = Seq.length forest

bounds ::  SeqForest a -> (Word, Word)
bounds seq = (0, fromIntegral $ Seq.length seq - 1)

(!) :: SeqForest a -> Word -> SeqTree a
seq ! n = Seq.index seq (fromIntegral n)

--------------------------------------------
-- low level bit-twiddling utility functions
--

-- TODO: figure out how these things work when Word is 64 bits

bitsNeeded :: Word -> Int
bitsNeeded n = bitsNeeded' 0 n
  where bitsNeeded' b 0 = b
        bitsNeeded' b n = bitsNeeded' (b+1) (n `shiftR` 1)

getBitSlice :: TreeIter -> Int -> Int -> Word
getBitSlice (TreeIter _ a b c) off count =
      getBitSliceWord a  off     count
  .|. getBitSliceWord b (off-32) count
  .|. getBitSliceWord c (off-64) count

  where getBitSliceWord :: Word -> Int -> Int -> Word
        getBitSliceWord word off count =
          word `shiftR` off .&. (1 `shiftL` count - 1)

setBitSlice :: TreeIter -> Int -> Int -> Word -> TreeIter
setBitSlice (TreeIter stamp a b c) off count value =
  assert (value < 1 `shiftL` count) $
  TreeIter stamp
           (setBitSliceWord a  off     count value)
           (setBitSliceWord b (off-32) count value)
           (setBitSliceWord c (off-64) count value)

  where setBitSliceWord :: Word -> Int -> Int -> Word -> Word
        setBitSliceWord word off count value =
          let mask = (1 `shiftL` count - 1) `shiftL` off
           in (word .&. complement mask) .|. (value `shiftL` off)

-------------------
-- testing
--

aforest :: SeqForest String
aforest =
  let leafNode a = Tree.Node { Tree.rootLabel = a, Tree.subForest = [] }
      trees =
       [Tree.Node {
          Tree.rootLabel = "A",
          Tree.subForest = [leafNode "AA", leafNode "AB"]
        },
        Tree.Node {
          Tree.rootLabel = "B",
          Tree.subForest = [leafNode "BA", leafNode "BB"]
        },
        Tree.Node {
          Tree.rootLabel = "C",
          Tree.subForest = [leafNode "CA", leafNode "CB"]
        },
        leafNode "D"]
  in mkSeqForest trees

instance Show TreeIter where
  show (TreeIter _ a b c) = show (a,b,c)

showIterBits (TreeIter _ a b c) = [showBits a, showBits b, showBits c]

showBits :: Bits a => a -> String
showBits a = [ if testBit a i then '1' else '0' | i <- [0..bitSize a - 1] ]

-- property 1:
--  forall aforest. forall valid path. iterToPath aforest (pathToIter aforest path) == path

-- property 2:
-- forall aforest. forall (node, path). lookup (pathToIter aforest path) == node
treeStoreSetValue :: TreeStore a -> TreePath -> a -> IO ()
treeStoreSetValue store path value = undefined
--  modifyIORef (forest store) (Something.update index value)
--  treeModelRowChanged (model store) path (pathToIter (forest store) path)

treeStoreSetTree :: TreeStore a -> TreePath -> Tree a -> IO ()
treeStoreSetTree store path value = undefined

treeStoreRemove :: TreeStore a -> TreePath -> IO ()
treeStoreRemove store path = undefined

treeStoreInsert :: TreeStore a -> TreePath -> Int -> a -> IO ()
treeStoreInsert store path index value = undefined

treeStoreInsertTree :: TreeStore a -> TreePath -> Int -> Tree a -> IO ()
treeStoreInsertTree store path index value = undefined

treeStorePrepend :: TreeStore a -> TreePath -> a -> IO ()
treeStorePrepend store path value = undefined

treeStorePrependTree :: TreeStore a -> TreePath -> Tree a -> IO ()
treeStorePrependTree store path value = undefined

treeStoreAppend :: TreeStore a -> TreePath -> a -> IO ()
treeStoreAppend store path value = undefined

treeStoreAppendTree :: TreeStore a -> TreePath -> Tree a -> IO ()
treeStoreAppendTree store path value = undefined

treeStoreClear :: TreeStore a -> IO ()
treeStoreClear store = undefined

treeStoreReorder :: TreeStore a -> TreePath -> [Int] -> IO ()
treeStoreReorder store path neworder = undefined

treeStoreSwap :: TreeStore a -> TreePath -> Int -> Int -> IO ()
treeStoreSwap store path = undefined

treeStoreMoveBefore :: TreeStore a -> TreePath -> Int -> Int -> IO ()
treeStoreMoveBefore store path = undefined

treeStoreMoveAfter :: TreeStore a -> TreePath -> Int -> Int -> IO ()
treeStoreMoveAfter store path = undefined
