{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK hide #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Pixbuf as Array
--
--  Author : Ciancia, Axel Simon
--
--  Created: 26 March 2002
--
--  Copyright (C) 2002-2005 Axel Simon
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
-- Portability : no (uses MTC, depends on internal GHC module)
--
-- 'PixbufData' exposes 'Pixbuf's as mutable array.
--

-- #hide
module Graphics.UI.Gtk.Gdk.PixbufData (
  PixbufData,
  mkPixbufData
  ) where

import System.Glib.FFI
import Graphics.UI.Gtk.Types
-- internal module of GHC
import Data.Array.Base ( MArray(..), newArray_, unsafeRead, unsafeWrite,
                         getBounds, getNumElements )

-- | An array that stored the raw pixel data of a 'Pixbuf'.
--
-- * See 'Graphics.UI.Gtk.Gdk.Pixbuf.pixbufGetPixels'.
--
data PixbufData i e = PixbufData !Pixbuf
                  {-# UNPACK #-} !(Ptr e)
                                 !(i,i)
                  {-# UNPACK #-} !Int

mkPixbufData :: Storable e => Pixbuf -> Ptr e -> Int -> PixbufData Int e
mkPixbufData pb (ptr :: Ptr e) size =
  PixbufData pb ptr (0, count) count
  where count = fromIntegral (size `div` sizeOf (undefined :: e))

-- | 'PixbufData' is a mutable array.
instance Storable e => MArray PixbufData e IO where
  newArray (l,u) e = error "Gtk.Gdk.Pixbuf.newArray: not implemented"
  newArray_ (l,u)  = error "Gtk.Gdk.Pixbuf.newArray_: not implemented"
  {-# INLINE unsafeRead #-}
  unsafeRead (PixbufData (Pixbuf pb) pixPtr _ _) idx = do
      e <- peekElemOff pixPtr idx
      touchForeignPtr pb
      return e
  {-# INLINE unsafeWrite #-}
  unsafeWrite (PixbufData (Pixbuf pb) pixPtr _ _) idx elem = do
      pokeElemOff pixPtr idx elem
      touchForeignPtr pb
  {-# INLINE getBounds #-}
  getBounds (PixbufData _ _ bd _) = return bd
  {-# INLINE getNumElements #-}
  getNumElements (PixbufData _ _ _ count) = return count
