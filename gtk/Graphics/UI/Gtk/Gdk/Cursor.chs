-- -*-haskell-*-
--  GIMP Toolkit (GTK) Cursor
--
--  Author : Bit Connor <bit@mutantlemon.com>
--
--  Created: 18 November 2007
--
--  Copyright (C) 2007 Bit Connor
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
-- Cursors | Standard and pixmap cursors.
--
module Graphics.UI.Gtk.Gdk.Cursor (
  Cursor(..),
  cursorNewFromPixmap
  ) where

import System.Glib.FFI
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr, unsafeForeignPtrToPtr)
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.General.Structs

{#context lib="gdk" prefix ="gdk"#}

{#pointer *Cursor foreign newtype #}

makeNewCursor :: Ptr Cursor -> IO Cursor
makeNewCursor rPtr = do
  cursor <- newForeignPtr rPtr cursor_unref
  return (Cursor cursor)

foreign import ccall unsafe "&gdk_cursor_unref"
  cursor_unref :: FinalizerPtr Cursor

-- | Creates a new cursor from a given pixmap and mask. Both the pixmap and
-- mask must have a depth of 1 (i.e. each pixel has only 2 values - on or off).
-- The standard cursor size is 16 by 16 pixels.
--
cursorNewFromPixmap ::
     Pixmap -- ^ @source@ - the pixmap specifying the cursor.
  -> Pixmap -- ^ @mask@ - the pixmap specifying the mask, which must be the
            -- same size as source.
  -> Color  -- ^ @fg@ - the foreground color, used for the bits in the source
            -- which are 1. The color does not have to be allocated first.
  -> Color  -- ^ @bg@ - the background color, used for the bits in the source
            -- which are 0. The color does not have to be allocated first.
  -> Int    -- ^ @x@ - the horizontal offset of the \'hotspot\' of the cursor.
  -> Int    -- ^ @y@ - the vertical offset of the \'hotspot\' of the cursor.
  -> IO Cursor
cursorNewFromPixmap source mask fg bg x y =
  with fg $ \fgPtr ->
    with bg $ \bgPtr -> do
      rPtr <- {# call unsafe cursor_new_from_pixmap #} source mask (castPtr fgPtr) (castPtr bgPtr) (fromIntegral x) (fromIntegral y)
      makeNewCursor rPtr

