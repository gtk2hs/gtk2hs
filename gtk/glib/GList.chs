-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Coping with linked lists@
--
--  Author : Axel Simon
--          
--  Created: 19 March 2002
--
--  Version $Revision: 1.3 $ from $Date: 2002/07/21 16:07:17 $
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- @description@ --------------------------------------------------------------
--
-- * Define functions to extract data from a GList and to produce a GList from
--   a list of pointers.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
--
-- * Figure out if we ever need to generate a GList.
--
module GList(
  GList,
  ptrToInt,
  fromGList
  ) where

import Monad	(liftM)
import Foreign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="g" prefix="g" #}

{#pointer * GList#}

-- methods

-- Convert a pointer to an Int.
--
ptrToInt :: Ptr a -> Int
ptrToInt ptr = minusPtr ptr nullPtr 

-- Turn a GList into a list of pointers.
--
fromGList :: GList -> IO [Ptr a]
fromGList glist = do
    end <- {#call unsafe list_last#} glist
    extractList glist []
  where
    extractList gl xs
      | gl==nullPtr = do
	{#call unsafe list_free#} gl
	return xs
      | otherwise	   = do
	x <- {#get GList.data#} gl
	gl' <- {#call unsafe list_delete_link#} gl gl
	extractList gl' ((castPtr x):xs)


-- Convert an Int into a pointer.
--
intToPtr :: Int -> Ptr a
intToPtr int = plusPtr nullPtr int


-- Turn a list of something into a GList.
--
toGList :: [a] -> (a -> Ptr b) -> IO GList
toGList xs conv = makeList nullPtr xs
  where
    -- makeList :: GList -> [a] -> IO GList
    makeList current (x:xs) = do
      newHead <- {#call unsafe list_prepend#} current ((castPtr.conv) x)
      makeList newHead xs
    makeList current [] = return current