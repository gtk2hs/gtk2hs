-- -*-haskell-*-
--  GIMP Toolkit (GTK)
--
--  Author : Axel Simon
--
--  Created: 19 March 2002
--
--  Copyright (C) 2002 Axel Simon
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
-- Defines functions to extract data from a GList and to produce a GList from
-- a list of pointers.
--
-- * The same for GSList.
--
module System.Glib.GList (
  GList,
  readGList,
  fromGList,
  toGList,
  withGList,

  GSList,
  readGSList,
  fromGSList,
  fromGSListRev,
  toGSList,
  withGSList,
  ) where

import Foreign
import Control.Exception        (bracket)
import Control.Monad            (foldM)

{# context lib="glib" prefix="g" #}

{#pointer * GList#}
{#pointer * GSList#}

-- methods

-- Turn a GList into a list of pointers but don't destroy the list.
--
readGList :: GList -> IO [Ptr a]
readGList glist
  | glist==nullPtr = return []
  | otherwise       = do
    x <- {#get GList->data#} glist
    glist' <- {#get GList->next#} glist
    xs <- readGList glist'
    return (castPtr x:xs)

-- Turn a GList into a list of pointers (freeing the GList in the process).
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
  | otherwise       = do
    x <- {#get GSList->data#} gslist
    gslist' <- {#get GSList->next#} gslist
    xs <- readGSList gslist'
    return (castPtr x:xs)

-- Turn a GSList into a list of pointers (freeing the GSList in the process).
--
fromGSList :: GSList -> IO [Ptr a]
fromGSList gslist
  | gslist==nullPtr = return []
  | otherwise       = do
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
      | otherwise       = do
        x <- {#get GSList->data#} gslist
        gslist' <- {#call unsafe slist_delete_link#} gslist gslist
        extractList gslist' (castPtr x:xs)

-- Turn a list of something into a GList.
--
toGList :: [Ptr a] -> IO GList
toGList = foldM prepend nullPtr . reverse
  where
    -- prepend :: GList -> Ptr a -> IO GList
    prepend l x = {#call unsafe list_prepend#} l (castPtr x)

-- Turn a list of something into a GSList.
--
toGSList :: [Ptr a] -> IO GSList
toGSList = foldM prepend nullPtr . reverse
  where
    -- prepend :: GSList -> Ptr a -> IO GList
    prepend l x = {#call unsafe slist_prepend#} l (castPtr x)

-- Temporarily allocate a list of something
--
withGList :: [Ptr a] -> (GSList -> IO b) -> IO b
withGList xs = bracket (toGList xs) {# call unsafe g_list_free #}

-- Temporarily allocate a list of something
--
withGSList :: [Ptr a] -> (GSList -> IO b) -> IO b
withGSList xs = bracket (toGSList xs) {# call unsafe g_slist_free #}

