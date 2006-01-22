module Graphics.UI.Gtk.TreeList.TreeStoreStatic (
  treeStoreNewStatic
  ) where

import Data.Bits
import Data.Word (Word)
import qualified Data.Tree as Tree
import Data.Tree (Tree)
import Data.Array

import Graphics.UI.Gtk.TreeList.TreeModel
import Graphics.UI.Gtk.TreeList.CustomStore
import Graphics.UI.Gtk.TreeList.TreeIter
import Graphics.UI.Gtk.TreeList.Column

import Control.Exception (assert)

--------------------------------------------
-- internal model data types
--

data ArrTree a = Node a !(ArrForest a)
               | Leaf a
type ArrForest a = Array Word (ArrTree a)

label :: ArrTree a -> a
label (Node a _) = a
label (Leaf a)   = a

mkArrForest :: [Tree a] -> ArrForest a
mkArrForest trees =
  let bounds = (0, fromIntegral (length trees) - 1)
   in listArray bounds (map mkArrTree trees)

mkArrTree :: Tree a -> ArrTree a
mkArrTree (Tree.Node a [])    = Leaf a
mkArrTree (Tree.Node a trees) = Node a (mkArrForest trees)

--------------------------------------------
-- the actual TreeStore implementation
--

treeStoreNewStatic :: [Column a] -> [Tree a] -> IO TreeModel
treeStoreNewStatic rs ts =
 customStoreNew $
 let forest = mkArrForest ts
     cols   = listArray (0, length rs - 1) rs
  in CustomStore {
    customStoreGetFlags      = return [],
    customStoreGetNColumns   = return (length rs),
    customStoreGetColumnType = \n -> return $! columnGType (cols ! n),

    customStoreGetIter       = \path -> return $! pathToIter forest path,
    customStoreGetPath       = \iter -> return $! iterToPath forest iter,

    customStoreGetValue      = \iter i gvalue ->
                                 case forest `lookupNode` iter of 
                                   (node, _, _, _, _) ->
                                     columnSetGValue (cols ! i) (label node) gvalue,

    customStoreIterNext      = \iter -> return $! treeStoreIterNext forest iter,
    customStoreIterChildren  = \iter -> return $! treeStoreIterChildren forest iter,
    customStoreIterHasChild  = \iter -> return $! treeStoreIterHasChild forest iter,
    customStoreIterNChildren = \iter -> return $! treeStoreIterNChildren forest iter,
    customStoreIterNthChild  = \iter n -> return $! treeStoreIterNthChild forest iter n,
    customStoreIterParent    = \iter -> return $! treeStoreIterParent forest iter,

    customStoreRefNode       = \_ -> return (),
    customStoreUnrefNode     = \_ -> return ()
}

treeStoreIterNext :: ArrForest a -> TreeIter -> Maybe TreeIter
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

lookupNode :: ArrForest a -> TreeIter -> (ArrTree a, ArrForest a, Word, Int, Int)
lookupNode forest iter =
  let count = forestBitsNeeded forest
      index = getBitSlice iter 0 count
   in lookup forest index 0 count

  where
    lookup :: 
        ArrForest a   -- the forest of trees we are currently looking in
     -> Word        -- the index of the node in the forest (non-zero)
     -> Int           -- the iterator bit offset for the node in the forest
     -> Int           -- the number of bits needed to index the current forest

     -> (ArrTree a,   -- the node we found
         ArrForest a, -- the forrest the node is part of
         Word,      -- the index of the node within the forest
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

iterToPath :: ArrForest a -> TreeIter -> TreePath
iterToPath forest iter = lookup forest 0
  where
    lookup :: ArrForest a -> Int -> TreePath
    lookup forest off =
      let count = forestBitsNeeded forest
       in case getBitSlice iter off count of 
            0 -> []
            index ->
              case forest ! (index - 1) of
                Leaf _        -> fromIntegral index - 1 : []
                Node _ forest -> fromIntegral index - 1
                               : lookup forest (off+count)

pathToIter :: ArrForest a -> TreePath -> Maybe TreeIter
pathToIter forest []   = Nothing
pathToIter forest path = lookup forest path 0 (TreeIter 0 0 0 0)
  where
    lookup :: ArrForest a -> TreePath -> Int -> TreeIter -> Maybe TreeIter
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

forestBitsNeeded :: ArrForest a -> Int
forestBitsNeeded forest =
  case bounds forest of
    (_, upperBound) -> bitsNeeded (upperBound + 2)

forestElemCount ::  ArrForest a -> Int
forestElemCount forest =
  case bounds forest of
    (_, upperBound) -> fromIntegral (upperBound + 1)

--------------------------------------------
-- low leve bit-twiddling utility functions
--

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

aforest :: ArrForest String
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
  in mkArrForest trees

instance Show TreeIter where
  show (TreeIter _ a b c) = show (a,b,c)

showIterBits (TreeIter _ a b c) = [showBits a, showBits b, showBits c]

showBits :: Bits a => a -> String
showBits a = [ if testBit a i then '1' else '0' | i <- [0..bitSize a - 1] ]

-- property 1:
--  forall aforest. forall valid path. iterToPath aforest (pathToIter aforest path) == path

-- property 2:
-- forall aforest. forall (node, path). lookup (pathToIter aforest path) == node
