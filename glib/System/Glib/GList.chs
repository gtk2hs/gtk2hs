-- -*-haskell-*-
--  GIMP Toolkit (GTK)
--
--  Author : Axel Simon
--          
--  Created: 19 March 2002
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 17:45:06 $
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
-- |
--
-- Defines functions to extract data from a GList and to produce a GList from
-- a list of pointers.
--
-- * The same for GSList.
--
module System.Glib.GList (
  ptrToInt,
  GList,
  fromGList,
  toGList,
  GSList,
  readGSList,
  fromGSList,
  fromGSListRev,
  toGSList
  ) where

import Monad	(liftM)
import Foreign

{# context lib="glib" prefix="g" #}

{#pointer * GList#}
{#pointer * GSList#}

-- methods

-- Convert a pointer to an Int.
--
ptrToInt :: Ptr a -> Int
ptrToInt ptr = minusPtr ptr nullPtr 

-- Turn a GList into a list of pointers.
--
fromGList :: GList -> IO [Ptr a]
fromGList glist = do
    glist' <- {#call unsafe list_reverse#} glist
    extractList glist' []
  where
    extractList gl xs
      | gl==nullPtr = return xs
      | otherwise   = do
	x <- {#get GList.data#} gl
	gl' <- {#call unsafe list_delete_link#} gl gl
	extractList gl' (castPtr x:xs)

-- Turn a GSList into a list of pointers but don't destroy the list.
--
readGSList :: GSList -> IO [Ptr a]
readGSList gslist
  | gslist==nullPtr = return []
  | otherwise	    = do
    x <- {#get GSList->data#} gslist
    gslist' <- {#get GSList->next#} gslist
    xs <- readGSList gslist'
    return (castPtr x:xs)

-- Turn a GSList into a list of pointers.
--
fromGSList :: GSList -> IO [Ptr a]
fromGSList gslist
  | gslist==nullPtr = return []
  | otherwise	    = do
    x <- {#get GSList->data#} gslist
    gslist' <- {#call unsafe slist_delete_link#} gslist gslist
    xs <- fromGSList gslist'
    return (castPtr x:xs)

-- Turn a GSList into a list of pointers and reverse it.
--
fromGSListRev :: GSList -> IO [Ptr a]
fromGSListRev gslist =
  extractList gslist []
  where
    extractList gslist xs
      | gslist==nullPtr = return xs
      | otherwise	= do
	x <- {#get GSList->data#} gslist
	gslist' <- {#call unsafe slist_delete_link#} gslist gslist
	extractList gslist' (castPtr x:xs)

-- Convert an Int into a pointer.
--
intToPtr :: Int -> Ptr a
intToPtr int = plusPtr nullPtr int


-- Turn a list of something into a GList.
--
toGList :: [Ptr a] -> IO GList
toGList xs = makeList nullPtr xs
  where
    -- makeList :: GList -> [Ptr a] -> IO GList
    makeList current (x:xs) = do
      newHead <- {#call unsafe list_prepend#} current (castPtr x)
      makeList newHead xs
    makeList current [] = return current

-- Turn a list of something into a GSList.
--
toGSList :: [Ptr a] -> IO GSList
toGSList xs = makeList nullPtr xs
  where
    -- makeList :: GSList -> [Ptr a] -> IO GSList
    makeList current (x:xs) = do
      newHead <- {#call unsafe slist_prepend#} current (castPtr x)
      makeList newHead xs
    makeList current [] = return current

