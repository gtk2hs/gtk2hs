{-# OPTIONS -cpp #-}

module Map (
   Map,
   empty, singleton,
   lookup, findWithDefault,
   insert,
   union, unionWith,
   map,
   fromList, toList
) where

import Prelude hiding (lookup, map)

#if __GLASGOW_HASKELL__ >= 603 || !__GLASGOW_HASKELL__
import Data.Map
#else
import Data.FiniteMap

type Map k a = FiniteMap k a

empty :: Map k a
empty = emptyFM

singleton :: k -> a -> Map k a
singleton = unitFM

lookup :: Ord k => k -> Map k a -> Maybe a
lookup = flip lookupFM

findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault a k m = lookupWithDefaultFM m a k

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k a m = addToFM m k a

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith c k a m = addToFM_C (flip c) m k a

union :: Ord k => Map k a -> Map k a -> Map k a
union = flip plusFM

unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith c l r = plusFM_C (flip c) r l

map :: (a -> b) -> Map k a -> Map k b
map f = mapFM (\_ -> f)

fromList :: Ord k => [(k,a)] -> Map k a
fromList = listToFM

toList :: Map k a -> [(k, a)]
toList = fmToList

#endif
