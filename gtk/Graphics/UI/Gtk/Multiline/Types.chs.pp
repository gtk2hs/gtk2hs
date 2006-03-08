-- -*-haskell-*-
--  GIMP Toolkit (GTK) Multiline internal types
--
--  Author : Axel Simon
--
--  Created: 2 March 2006
--
--  Version $Revision: 1.9 $ from $Date: 2005/11/18 15:54:57 $
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
  TextSearchFlags(..),

-- * Methods
  textIterCopy,
  mkTextIterCopy,
  makeEmptyTextIter
  ) where

import Monad	(liftM)
import Maybe	(fromMaybe)
import Char	(chr)

import System.Glib.FFI
import System.Glib.Flags		(fromFlags)
import System.Glib.UTFString
import System.Glib.GObject		(makeNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Structs	(textIterSize)
import Graphics.UI.Gtk.General.Enums	(TextSearchFlags(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

{#pointer *TextIter foreign newtype #}

-- | Copy the iterator.
--
textIterCopy :: TextIter -> IO TextIter
textIterCopy ti = do
  iterPtr <- {#call unsafe text_iter_copy#} ti
  liftM TextIter $ newForeignPtr iterPtr (text_iter_free iterPtr)

-- Create a copy of a TextIter from a pointer.
--
mkTextIterCopy :: Ptr TextIter -> IO TextIter
mkTextIterCopy iterPtr = do
  iterPtr <- gtk_text_iter_copy iterPtr
  liftM TextIter $ newForeignPtr iterPtr (text_iter_free iterPtr)

#if __GLASGOW_HASKELL__>=600

foreign import ccall unsafe "&gtk_text_iter_free"
  text_iter_free' :: FinalizerPtr TextIter

text_iter_free :: Ptr TextIter -> FinalizerPtr TextIter
text_iter_free _ = text_iter_free'

#else

foreign import ccall unsafe "gtk_text_iter_free"
  text_iter_free :: Ptr TextIter -> IO ()

#endif

-- Allocate memory to be filled with a TextIter.
--
makeEmptyTextIter :: IO TextIter
makeEmptyTextIter = do
  iterPtr <- mallocBytes textIterSize
  liftM TextIter $ newForeignPtr iterPtr (text_iter_free iterPtr)

