-- -*-haskell-*-
--  GIMP Toolkit (GTK) CustomStore TreeModel
--
--  Author : Duncan Coutts, Axel Simon
--
--  Created: 11 Feburary 2006
--
--  Version $Revision: 1.1 $ from $Date: 2005/12/08 18:12:43 $
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
-- Standard model to store hierarchical data.
--
module Graphics.UI.Gtk.TreeList.TreeStoreAxel (
  TreeStore,
  treeStoreNew
  ) where

import Data.Bits
import Data.Word (Word)
import Data.Ix (inRange)
import Data.Maybe ( fromMaybe, isJust )
import Control.Exception (assert)
import Control.Concurrent.MVar

import Data.Tree
import System.Glib.FFI ( CInt )
import Graphics.UI.Gtk.TreeList.TreeModel
import Graphics.UI.Gtk.TreeList.CustomStore
import Graphics.UI.Gtk.TreeList.TreeIter

--------------------------------------------
-- internal model data types
--

-- | The abstract store for hierarchical data.
--
data TreeStore a = TreeStore {
    model :: TreeModel,
    store :: MVar (Store a)
  }

-- | Maximum number of nodes on each level.
--
-- * These numbers determine how many bits in a 'TreeIter' are devoted to
--   each level. Hence, these numbers reflect log2 of the maximum number
--   of nodes at a level, rounded up.
--
type Depth = [Int]

-- | A time stamp that is incremented each time iterators become invalid.
type Timestamp = CInt

data Store a = Store {
  depth :: Depth,
  timestamp :: Timestamp,
  content :: Cache a
}

-- | Ask for the time stamp of a 'TreeIter'.
--
iterTime :: TreeIter -> Timestamp
iterTime (TreeIter t _ _ _) = t

instance StoreClass TreeStore where
  storeGetModel = model
  storeGetValue TreeStore { store = mVar } iter = modifyMVar mVar $
    \Store { depth = d, timestamp = t, content = cache } ->
    if iterTime iter/=t then
    error "TreeStore.storeGetValue: iter has wrong time stamp" else
    case checkSuccess d iter cache of
      (True, cache@((_, (Node { rootLabel = val }:_)):_)) ->
	  return (Store { depth = d, timestamp = t, content = cache }, val)
      _ ->
	error "TreeStore.storeGetValue: iter does not refer to a valid entry"

-- | Create a new list store.
--
-- * The given rose tree determines the initial content and may be the empty
--   list. Each 'Tree' in the forest corresponds to one top-level node.
--
treeStoreNew :: Forest a -> IO (TreeStore a)
treeStoreNew forest = do
  mVar <- newMVar Store { depth = calcForestDepth forest,
			  timestamp = 0,
			  content = storeToCache 0 forest }
  model <- customStoreNew CustomStore {
    customStoreGetFlags = return [],
    customStoreGetIter = \path -> readMVar mVar >>=
      \Store { depth = d, timestamp = t } -> return (fromPath t d path),
    customStoreGetPath = \iter -> readMVar mVar >>=
      \store@Store { depth = d, timestamp = t } ->
      return (if iterTime iter/=t then [] else toPath d iter),
    customStoreIterNext = \iter -> modifyMVar mVar $
      \store@Store { depth = d, timestamp = t, content = cache } ->
      let (mIter', cache') = iterNext d iter cache in
      return (if iterTime iter/=t then (store, Nothing) else
	      (Store { depth = d, timestamp = t, content = cache' }, mIter')),
    customStoreIterChildren = \mIter -> modifyMVar mVar $
      \store@Store { depth = d, timestamp = t, content = cache } ->
      let iter = fromMaybe (invalidIter t) mIter
          (mIter', cache') = iterNthChild d 0 iter cache in
      return (if iterTime iter/=t then (store, Nothing) else
	      (Store { depth = d, timestamp = t, content = cache' }, mIter')),
    customStoreIterHasChild = \iter -> modifyMVar mVar $
      \store@Store { depth = d, timestamp = t, content = cache } ->
      let (mIter', cache') = iterNthChild d 0 iter cache in
      return (if iterTime iter/=t then (store, False) else
        (Store { depth = d, timestamp = t, content = cache' }, isJust mIter')),
    customStoreIterNChildren = \mIter -> modifyMVar mVar $
      \store@Store { depth = d, timestamp = t, content = cache } ->
      let iter = fromMaybe (invalidIter t) mIter
          (no, cache') = iterNChildren d iter cache in
      return (if iterTime iter/=t then (store, 0) else
        (Store { depth = d, timestamp = t, content = cache' }, no)),
    customStoreIterNthChild = \mIter idx  -> modifyMVar mVar $
      \store@Store { depth = d, timestamp = t, content = cache } ->
      let iter = fromMaybe (invalidIter t) mIter
          (mIter', cache') = iterNthChild d idx iter cache in
      return (if iterTime iter/=t then (store, Nothing) else
        (Store { depth = d, timestamp = t, content = cache' }, mIter')),
    customStoreIterParent = \iter -> do
      Store { depth = d, timestamp = t } <- readMVar mVar
      return (if iterTime iter/=t then Nothing else iterParent d iter),
    customStoreRefNode = \_ -> return (),
    customStoreUnrefNode = \_ -> return ()
  }
  return TreeStore { model = model,
		     store = mVar }

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


iterPrefixEqual :: TreeIter -> TreeIter -> Int -> Bool
iterPrefixEqual (TreeIter _ a1 b1 c1) (TreeIter _ a2 b2 c2) pos
  | pos>64 = let mask = 1 `shiftL` (pos-64) - 1 in
	     a1==a2 && b1==b2 && (c1 .&. mask) == (c2 .&. mask)
  | pos>32 = let mask = 1 `shiftL` (pos-32) - 1 in
	     a1==a2 && (b1 .&. mask) == (b2 .&. mask)
  | otherwise = let mask = 1 `shiftL` pos - 1 in
		(a1 .&. mask) == (a2 .&. mask)

-- | The invalid tree iterator.
--
invalidIter :: Timestamp -> TreeIter
invalidIter t = TreeIter t 0 0 0

showIterBits (TreeIter _ a b c) = [showBits a, showBits b, showBits c]

showBits :: Bits a => a -> String
showBits a = [ if testBit a i then '1' else '0' | i <- [0..bitSize a - 1] ]

-- | Calculate the maximum number of nodes on a per-level basis.
--
calcForestDepth :: Forest a -> Depth
calcForestDepth f = map bitsNeeded $
		    takeWhile (/=0) $
		    foldr calcTreeDepth (repeat 0) f
  where
  calcTreeDepth Node { subForest = f } (d:ds) =
      (d+1): zipWith max ds (foldr calcTreeDepth (repeat 0) f)


-- | Convert an iterator into a path.
--
toPath :: Depth -> TreeIter -> TreePath
toPath d iter = gP 0 d
  where
  gP pos [] = []
  gP pos (d:ds) = let idx = getBitSlice iter pos d in
	          if idx==0 then [] else fromIntegral (idx-1) : gP (pos+d) ds

-- | Try to convert a path into a 'TreeIter'.
--
fromPath :: Timestamp -> Depth -> TreePath -> Maybe TreeIter
fromPath t = fP 0 (invalidIter t)
  where
  fP pos ti _ [] = Just ti -- the remaining bits are zero anyway
  fP pos ti [] _ = Nothing
  fP pos ti (d:ds) (p:ps) = let idx = fromIntegral (p+1) :: Word in
    if idx >= bit d then Nothing else
    fP (pos+d) (setBitSlice ti pos d idx) ds ps

type Cache a = [(TreeIter, Forest a)]


-- | Create a traversal structure that allows a pre-order traversal in linear
--   time.
--
-- * The returned structure points at the root of the first level which doesn't
--   really exist, but serves to indicate that it is before the very first
--   node.
--
storeToCache :: Timestamp -> Forest a -> Cache a
storeToCache time [] = []
storeToCache time forest = [(invalidIter time, [Node root forest])]
  where
  root = error "TreeStore.storeToCache: accessed non-exitent root of tree"

-- | Advance the traversal structure to the given 'TreeIter'.
--
advanceCache :: Depth -> TreeIter -> Cache a -> Cache a
advanceCache depth goal [] = []
advanceCache depth goal cache@((rootIter,_):_) = 
  moveToSameLevel 0 depth
  where
  moveToSameLevel pos [] = cache
  moveToSameLevel pos (d:ds) =
    let
      goalIdx = getBitSlice goal pos d
      curIdx = getBitSlice rootIter pos d
      isNonZero pos d (ti,_) = getBitSlice ti pos d/=0
    in
    if goalIdx==curIdx then moveToSameLevel (pos+d) ds else
    if goalIdx==0 then dropWhile (isNonZero pos d) cache else
    if curIdx==0 then moveToChild pos (d:ds) cache else
    if goalIdx<curIdx then
      moveToChild pos (d:ds) (dropWhile (isNonZero pos d) cache)
    else let
      -- advance the current iterator to coincide with the goal iterator
      -- at this level
      moveWithinLevel pos d ((ti,forest):parents) = let
	  diff = fromIntegral (goalIdx-curIdx)
	  (dropped, remain) = splitAt diff forest
	  advance = length dropped
	  ti' = setBitSlice ti pos d (curIdx+fromIntegral advance)
	in
	if advance==diff then moveToChild (pos+d) ds ((ti',remain):parents)
	else (ti',remain):parents -- node not found
    in moveWithinLevel pos d $ case ds of
        [] -> cache
	(d':_) -> dropWhile (isNonZero (pos+d) d') cache

  -- Descend into the topmost forest to find the goal iterator. The position
  -- and the remainding depths specify the index in the cache that is zero.
  -- All indices in front of pos coincide with that of the goal iterator.
  moveToChild :: Int -> Depth -> Cache a -> Cache a
  moveToChild pos [] cache = cache -- we can't set more than the leaf
  moveToChild pos (d:ds) cache@((ti,forest):parents)
    | getBitSlice goal pos d == 0 = cache
    | otherwise = case forest of
      [] -> cache -- impossible request
      Node { subForest = children }:_ ->
        let
	  childIdx :: Int
	  childIdx = fromIntegral (getBitSlice goal pos d)-1
	  (dropped, remain) = splitAt childIdx children
	  advanced = length dropped
	  ti' = setBitSlice ti pos d (fromIntegral advanced+1)
	in if advanced<childIdx then ((ti',remain):cache) else 
	   moveToChild (pos+d) ds ((ti',remain):cache)

-- | Advance to the given iterator and return weather this was successful.
--    
checkSuccess :: Depth -> TreeIter -> Cache a -> (Bool, Cache a)
checkSuccess depth iter cache = (cmp cur iter && not (null sibs), cache')
  where
  cmp (TreeIter _ a1 b1 c1) (TreeIter _ a2 b2 c2) =
      a1==a2 && b1==b2 && c2==c2
  cache'@((cur,sibs):_) = advanceCache depth iter cache

-- | Get the leaf index of this iterator.
--
-- * Due to the way we construct the 'TreeIter's, we can check which the last
--   level of an iterator is: The bit sequence of level n is zero if n is
--   greater or equal to the level that the iterator refers to. The returned
--   triple is (pos, leaf, zero) such that pos..pos+leaf denotes the leaf
--   index and pos+leaf..pos+leaf+zero denotes the bit field that is zero.
--
getTreeIterLeaf :: Depth -> TreeIter -> (Int, Int, Int)
getTreeIterLeaf ds ti = topLevel ds
  where
  topLevel [] = (0,0,0)
  topLevel (d:ds) | getBitSlice ti 0 d==0 = (0,0,0)
		  | otherwise = gTIL 0 d ds
  gTIL pos dCur (dNext:ds)
    | getBitSlice ti (pos+dCur) dNext==0 = (pos,dCur,dNext)
    | otherwise = gTIL (pos+dCur) dNext ds
  gTIL pos d [] = (pos, d, 0)

-- | Move an iterator forwards on the same level.
--
iterNext :: Depth -> TreeIter -> Cache a -> (Maybe TreeIter, Cache a)
iterNext depth iter cache = let
    (pos,leaf,child) = getTreeIterLeaf depth iter
    curIdx = getBitSlice iter pos leaf
    nextIdx = curIdx+1
    nextIter = setBitSlice iter pos leaf nextIdx
  in
  if nextIdx==bit leaf then (Nothing, cache) else
  case checkSuccess depth nextIter cache of
    (True, cache) -> (Just nextIter, cache)
    (False, cache) -> (Nothing, cache)

-- | Move down to the child of the given iterator.
--
iterNthChild :: Depth -> Int -> TreeIter -> Cache a  ->
		(Maybe TreeIter, Cache a)
iterNthChild depth childIdx_ iter cache = let
    (pos,leaf,child) = getTreeIterLeaf depth iter
    childIdx = fromIntegral childIdx_+1 :: Word
    nextIter = setBitSlice iter (pos+leaf) child childIdx
  in
  if childIdx>=bit child then (Nothing, cache) else
  case checkSuccess depth nextIter cache of
    (True, cache) -> (Just nextIter, cache)
    (False, cache) -> (Nothing, cache)

-- | Descend to the first child.
--
iterNChildren :: Depth -> TreeIter -> Cache a -> (Int, Cache a)
iterNChildren depth iter cache = case checkSuccess depth iter cache of
  (True, cache@((_,Node { subForest = forest}:_):_)) -> (length forest, cache)
  (_, cache) -> (0, cache)


-- | Ascend to parent.
--
iterParent :: Depth -> TreeIter -> Maybe TreeIter
iterParent depth iter = let
    (pos,leaf,child) = getTreeIterLeaf depth iter
  in if leaf==0 then Nothing else
     if getBitSlice iter pos leaf==0 then Nothing else
     Just (setBitSlice iter pos leaf 0)
