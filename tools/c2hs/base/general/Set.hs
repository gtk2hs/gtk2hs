{-# OPTIONS -cpp #-}

module Set (
   Set,
   empty,
   member,
   insert,
   fromList,
) where

#if __GLASGOW_HASKELL__ >= 603 || !__GLASGOW_HASKELL__
import Data.Set
#else
import Data.Set

empty :: Set a
empty = emptySet

member :: Ord a => a -> Set a -> Bool
member = elementOf

insert :: Ord a => a -> Set a -> Set a
insert a s = addToSet s a

fromList :: Ord a => [a] -> Set a
fromList = mkSet

#endif
