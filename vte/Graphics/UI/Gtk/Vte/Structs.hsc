{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) marshalling of structures for VTE
--
--  Author : Axel Simon
--
--  Created: 26 Sep 2009
--
--  Copyright (C) 2009 Andy Stewart <lazycat.manatee@gmail.com>
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
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Structures for the VTE terminal widget
-- 
-----------------------------------------------------------------------------
-- 
module Graphics.UI.Gtk.Vte.Structs (
-- * Types
  VteAttributes(..),
  
-- * Functions
  gArrayContent
  ) where

import Data.Char
import Data.Word

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Widget (Color)

#include "VteCharAttrFields.h"

data VteAttributes = VteAttributes {
  vaRow :: Int,
  vaCol :: Int,
  vaFore :: Color,
  vaBack :: Color,
  vaUnderline :: Bool,
  vaStrikethrough :: Bool
  }

-- these fields are declard as bit fields which we cannot access portably from
-- Haskell, thus, we define two C helper functions that read these fields
foreign import ccall "getVteCharAttrUnderline"
  getVteCharAttrUnderline :: Ptr VteAttributes -> IO #{type gboolean}

foreign import ccall "getVteCharAttrStrikethrough"
  getVteCharAttrStrikethrough :: Ptr VteAttributes -> IO #{type gboolean}

instance Storable VteAttributes where
  sizeOf _ = #{const sizeof(VteCharAttributes)}
  alignment _ = alignment (undefined :: #{type long})
  peek ptr = do
    row <- #{peek VteCharAttributes, row} ptr :: IO #{type long}
    col <- #{peek VteCharAttributes, column} ptr :: IO #{type long}
    fore <- #{peek VteCharAttributes, fore} ptr
    back <- #{peek VteCharAttributes, back} ptr
    under <- getVteCharAttrUnderline ptr
    strike <- getVteCharAttrStrikethrough ptr
    return VteAttributes {
      vaRow = fromIntegral row,
      vaCol = fromIntegral col,
      vaFore = fore,
      vaBack = back,
      vaUnderline = toBool (fromIntegral under),
      vaStrikethrough = toBool (fromIntegral strike)
      }
  poke ptr VteAttributes {} = error "Storable VteAttributes: not implemented"

-- | Retrieve the two fields of the GArray structure.
--
gArrayContent :: Ptr garray -> IO (Int, Ptr VteAttributes)
gArrayContent gaPtr = do
  len <- #{peek GArray, len} gaPtr :: IO #{type guint}
  ptr <- #{peek GArray, data} gaPtr
  return (fromIntegral len, ptr)
