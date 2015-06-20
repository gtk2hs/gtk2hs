{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Multiline internal types
--
--  Author : Axel Simon
--
--  Created: 2 March 2006
--
--  Copyright (C) 2002-2006 Axel Simon
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
-- #hide
module Graphics.UI.Gtk.Multiline.Types (

-- * Types
  TextIter(TextIter),

-- * Methods
  textIterCopy,
  mkTextIterCopy,
  makeEmptyTextIter
  ) where

import System.Glib.FFI
import Graphics.UI.Gtk.General.Structs  (textIterSize)

{# context lib="gtk" prefix="gtk" #}

-- methods

{#pointer *TextIter foreign newtype #}

-- | Copy the iterator.
--
textIterCopy :: TextIter -> IO TextIter
textIterCopy (TextIter iter) = do
  iter' <- mallocForeignPtrBytes textIterSize
  withForeignPtr iter' $ \iterPtr' ->
    withForeignPtr iter $ \iterPtr ->
      copyBytes iterPtr' iterPtr textIterSize
  return (TextIter iter')

-- | Interal marshaling util
--
mkTextIterCopy :: Ptr TextIter -> IO TextIter
mkTextIterCopy iterPtr = do
  iter' <- mallocForeignPtrBytes textIterSize
  withForeignPtr iter' $ \iterPtr' ->
    copyBytes iterPtr' iterPtr textIterSize
  return (TextIter iter')

-- | Allocate memory to be filled with a TextIter.
--
makeEmptyTextIter :: IO TextIter
makeEmptyTextIter = do
  iter <- mallocForeignPtrBytes textIterSize
  return (TextIter iter)
