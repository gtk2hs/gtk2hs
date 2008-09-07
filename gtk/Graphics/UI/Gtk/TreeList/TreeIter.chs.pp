-- -*-haskell-*-
--  GIMP Toolkit (GTK) Class TreeIter
--
--  Author : Duncan Coutts
--
--  Created: 14 April 2005
--
--  Copyright (C) 2005 Axel Simon, Duncan Coutts
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
-- 
--
-- * This module and all other modules in 'Graphics.UI.Gtk.TreeList' are
--   deprecated. Please use the modules in 'Graphics.UI.Gtk.ModelView'.
--
module Graphics.UI.Gtk.TreeList.TreeIter (

-- * Types
  TreeIter(..),

-- * Methods
  receiveTreeIter
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI

{# context lib="gtk" prefix="gtk" #}

-- | Tree Iterator: a pointer to an entry in a
-- 'Graphics.UI.Gtk.ModelView.TreeModel'.
--
data TreeIter = TreeIter {-# UNPACK #-} !CInt !Word !Word !Word
	      deriving Show

{#pointer *TreeIter as TreeIterPtr -> TreeIter #}

instance Storable TreeIter where
  sizeOf _ = {# sizeof TreeIter #}
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
    stamp      <- {# get TreeIter->stamp      #} ptr
    user_data  <- {# get TreeIter->user_data  #} ptr
    user_data2 <- {# get TreeIter->user_data2 #} ptr
    user_data3 <- {# get TreeIter->user_data3 #} ptr
    return (TreeIter stamp (ptrToWord user_data)
                           (ptrToWord user_data2)
                           (ptrToWord user_data3))

    where ptrToWord :: Ptr a -> Word
          ptrToWord ptr = fromIntegral (ptr `minusPtr` nullPtr)

  poke ptr (TreeIter stamp user_data user_data2 user_data3) = do
    {# set TreeIter->stamp      #} ptr stamp
    {# set TreeIter->user_data  #} ptr (wordToPtr user_data)
    {# set TreeIter->user_data2 #} ptr (wordToPtr user_data2)
    {# set TreeIter->user_data3 #} ptr (wordToPtr user_data3)

    where wordToPtr :: Word -> Ptr a
          wordToPtr word = nullPtr `plusPtr` fromIntegral word

--------------------
-- Methods

receiveTreeIter :: (Ptr TreeIter -> IO CInt) -> IO (Maybe TreeIter)
receiveTreeIter body =
  alloca $ \iterPtr -> do
  result <- body iterPtr
  if toBool result
    then liftM Just (peek iterPtr)
    else return Nothing
