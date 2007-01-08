-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Multi-way trees (/aka/ rose trees) and forests.
--
-----------------------------------------------------------------------------

module Data.Tree(
	Tree(..), Forest,
	-- * Two-dimensional drawing
	drawTree, drawForest,
	-- * Extraction
	flatten, levels,
	-- * Building trees
	unfoldTree, unfoldForest,
	unfoldTreeM, unfoldForestM,
    ) where

import Control.Monad

-- | Multi-way trees, also known as /rose trees/.
data Tree a   = Node {
		rootLabel :: a,		-- ^ label value
		subForest :: Forest a	-- ^ zero or more child trees
	}
  deriving (Eq, Read, Show)

type Forest a = [Tree a]

instance Functor Tree where
  fmap f (Node x ts) = Node (f x) (map (fmap f) ts)


-- | Neat 2-dimensional drawing of a tree.
drawTree :: Tree String -> String
drawTree  = unlines . draw

-- | Neat 2-dimensional drawing of a forest.
drawForest :: Forest String -> String
drawForest  = unlines . map drawTree

draw :: Tree String -> [String]
draw (Node x ts0) = x : drawSubTrees ts0
  where drawSubTrees [] = []
	drawSubTrees [t] =
		"|" : shift "`- " "   " (draw t)
	drawSubTrees (t:ts) =
		"|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

	shift first other = zipWith (++) (first : repeat other)

-- | The elements of a tree in pre-order.
flatten :: Tree a -> [a]
flatten t = squish t []
  where squish (Node x ts) xs = x:Prelude.foldr squish xs ts

-- | Lists of nodes at each level of the tree.
levels :: Tree a -> [[a]]
levels t = map (map rootLabel) $
		takeWhile (not . null) $
		iterate (concatMap subForest) [t]

-- | Build a tree from a seed value
unfoldTree :: (b -> (a, [b])) -> b -> Tree a
unfoldTree f b = let (a, bs) = f b in Node a (unfoldForest f bs)

-- | Build a forest from a list of seed values
unfoldForest :: (b -> (a, [b])) -> [b] -> Forest a
unfoldForest f = map (unfoldTree f)

-- | Monadic tree builder, in depth-first order
unfoldTreeM :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfoldTreeM f b = do
	(a, bs) <- f b
	ts <- unfoldForestM f bs
	return (Node a ts)

-- | Monadic forest builder, in depth-first order
unfoldForestM :: Monad m => (b -> m (a, [b])) -> [b] -> m (Forest a)
unfoldForestM f = Prelude.mapM (unfoldTreeM f)
