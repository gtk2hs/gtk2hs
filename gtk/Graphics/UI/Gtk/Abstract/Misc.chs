-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Misc
--
--  Author : Manuel M. T. Chakravarty, Axel Simon
--
--  Created: 2 May 2001
--
--  Version $Revision: 1.4 $ from $Date: 2005/02/25 22:53:40 $
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- A base class for widgets with alignments and padding.
--
module Graphics.UI.Gtk.Abstract.Misc (
-- * Description
-- 
-- | The 'Misc' widget is an abstract widget which is not useful itself, but
-- is used to derive subclasses which have alignment and padding attributes.
--
-- The horizontal and vertical padding attributes allows extra space to be
-- added around the widget.
--
-- The horizontal and vertical alignment attributes enable the widget to be
-- positioned within its allocated area. Note that if the widget is added to a
-- container in such a way that it expands automatically to fill its allocated
-- area, the alignment settings will not alter the widgets position.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----Misc
-- |                     +----'Label'
-- |                     +----'Arrow'
-- |                     +----'Image'
-- |                     +----'Pixmap'
-- @

-- * Types
  Misc,
  MiscClass,
  castToMisc,

-- * Methods
  miscSetAlignment,
  miscGetAlignment,
  miscSetPadding,
  miscGetPadding
  ) where

import Monad	(liftM)

import System.Glib.FFI
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Set the alignment of the widget.
--
miscSetAlignment :: MiscClass m => m -> Double -> Double -> IO ()
miscSetAlignment misc xalign yalign =  {#call misc_set_alignment#} 
  (toMisc misc) (realToFrac xalign) (realToFrac yalign) 
    
-- | Get the alignment of the widget.
--
miscGetAlignment :: MiscClass m => m -> IO (Double, Double)
miscGetAlignment misc = 
  alloca $ \xalignPtr -> alloca $ \yalignPtr -> do
  {#call unsafe misc_get_alignment#} (toMisc misc) xalignPtr yalignPtr
  xalign <- peek xalignPtr
  yalign <- peek yalignPtr
  return (realToFrac xalign, realToFrac yalign)

-- | Set the amount of space to add around the widget.
--
miscSetPadding :: MiscClass m => m -> Int -> Int -> IO ()
miscSetPadding misc xpad ypad = {#call misc_set_padding#} 
  (toMisc misc) (fromIntegral xpad) (fromIntegral ypad) 

-- | Get the amount of space added around the widget.
--
miscGetPadding :: MiscClass m => m -> IO (Int, Int)
miscGetPadding misc =
  alloca $ \xpadPtr -> alloca $ \ypadPtr -> do
  {#call unsafe misc_get_padding#} (toMisc misc) xpadPtr ypadPtr
  xpad <- peek xpadPtr
  ypad <- peek ypadPtr
  return (fromIntegral xpad, fromIntegral ypad)

