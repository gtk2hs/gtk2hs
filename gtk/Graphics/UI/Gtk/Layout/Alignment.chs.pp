-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Alignment
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:22:09 $
--
--  Copyright (c) 1999..2002 Axel Simon
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
module Graphics.UI.Gtk.Layout.Alignment (
  Alignment,
  AlignmentClass,
  castToAlignment,
  alignmentNew,
  alignmentSet
#if GTK_CHECK_VERSION(2,4,0)
 ,alignmentSetPadding,
  alignmentGetPadding
#endif
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create an alignment widget. This widget tells
-- its child widget how to use the given space.
--
alignmentNew :: Float -> Float -> Float -> Float -> IO Alignment
alignmentNew yscale xalign yalign xscale = makeNewObject mkAlignment $
  liftM castPtr $ {#call unsafe alignment_new#} (realToFrac xalign) 
  (realToFrac yalign) (realToFrac xscale) (realToFrac yscale)


-- | Change the space use behaviour of an 'Alignment'.
--
alignmentSet :: AlignmentClass al => al -> Float -> Float -> Float -> Float ->
                IO ()
alignmentSet al xalign yalign xscale yscale = {#call alignment_set#}
  (toAlignment al) (realToFrac xalign) (realToFrac yalign)
  (realToFrac xscale) (realToFrac yscale)

#if GTK_CHECK_VERSION(2,4,0)
-- | Sets the padding on the different sides of the widget.
--
alignmentSetPadding :: AlignmentClass al => al -> Int -> Int -> Int -> Int ->
                       IO ()
alignmentSetPadding al top bottom left right =
  {# call gtk_alignment_set_padding #} (toAlignment al)
    (fromIntegral top) (fromIntegral bottom)
    (fromIntegral left) (fromIntegral right)

-- | Gets the padding on the different sides of the widget.
--
alignmentGetPadding :: AlignmentClass al => al -> IO (Int, Int, Int, Int)
alignmentGetPadding al =
  alloca $ \topPtr -> alloca $ \bottomPtr ->
  alloca $ \leftPtr -> alloca $ \rightPtr -> do
  {# call gtk_alignment_get_padding #} (toAlignment al)
    topPtr bottomPtr leftPtr rightPtr
  top    <- peek topPtr
  bottom <- peek bottomPtr
  left   <- peek leftPtr
  right  <- peek rightPtr
  return (fromIntegral top, fromIntegral bottom
         ,fromIntegral left, fromIntegral right)
  
#endif
