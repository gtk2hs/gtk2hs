{-# OPTIONS -fglasgow-exts #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Pixbuf as Array
--
--  Author : Ciancia, Axel Simon
--
--  Created: 26 March 2002
--
--  Version $Revision: 1.1 $ from $Date: 2005/05/10 23:30:40 $
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
  PixbufData(PixbufData),
  insertBounds
  ) where

import System.Glib.FFI
import Graphics.UI.Gtk.Types
import Data.Ix
-- internal module of GHC
import Data.Array.Base ( MArray, newArray, newArray_, unsafeRead, unsafeWrite,
			 HasBounds, bounds )

-- | An array that stored the raw pixel data of a 'Pixbuf'.
--
-- * See 'pixbufGetPixels'.
--
data Ix i => PixbufData i e = PixbufData Pixbuf (Ptr e) (i,i)

instance HasBounds PixbufData where
  bounds (PixbufData pb ptr bd) = bd

-- | 'PixbufData' is a mutable array.
instance Storable e => MArray PixbufData e IO where
  newArray (l,u) e = error "Gtk.Gdk.Pixbuf.newArray: not implemented"
  newArray_ (l,u)  = error "Gtk.Gdk.Pixbuf.newArray_: not implemented"
  {-# INLINE unsafeRead #-}
  unsafeRead (PixbufData (Pixbuf pb) pixPtr _) idx = do
      e <- peekElemOff pixPtr idx
      touchForeignPtr pb
      return e
  {-# INLINE unsafeWrite #-}
  unsafeWrite (PixbufData (Pixbuf pb) pixPtr _) idx elem = do
      pokeElemOff pixPtr idx elem
      touchForeignPtr pb

-- Insert bounds into a PixbufData.
insertBounds :: (Num i, Ix i, Storable e) => Int -> 
		PixbufData i e -> PixbufData i e
insertBounds size ((PixbufData pb ptr _) :: PixbufData i e) =
  PixbufData pb ptr (0, fromIntegral (size `div` sizeOf (undefined :: e)))
