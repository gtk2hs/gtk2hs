-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Scale
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/12 17:19:21 $
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
-- This is the abstract base class for HScale and VScale. It implements the
-- management of an adjustable value.
--
module Graphics.UI.Gtk.Abstract.Scale (
  Scale,
  ScaleClass,
  castToScale,
  scaleSetDigits,
  scaleGetDigits,
  scaleSetDrawValue,
  scaleGetDrawValue,
  PositionType(..),
  scaleSetValuePos,
  scaleGetValuePos
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(PositionType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Set the number of displayed digits after the comma.
--
scaleSetDigits :: ScaleClass s => s -> Int -> IO ()
scaleSetDigits s prec = 
  {#call scale_set_digits#} (toScale s) (fromIntegral prec)

-- | Get the number of displayed digits after the comma.
--
scaleGetDigits :: ScaleClass s => s -> IO Int
scaleGetDigits s =
  liftM fromIntegral $ {#call unsafe scale_get_digits#} (toScale s)

-- | Specify if the current value is to be drawn next to the slider.
--
scaleSetDrawValue :: ScaleClass s => s -> Bool -> IO ()
scaleSetDrawValue s draw =
  {#call scale_set_draw_value#} (toScale s) (fromBool draw)

-- | Returns whether the current value is drawn next to the slider.
--
scaleGetDrawValue :: ScaleClass s => s -> IO Bool
scaleGetDrawValue s =
  liftM toBool $ {#call unsafe scale_get_draw_value#} (toScale s)

-- | Specify where the value is to be displayed (relative to the slider).
--
scaleSetValuePos :: ScaleClass s => s -> PositionType -> IO ()
scaleSetValuePos s pos =
  {#call scale_set_value_pos#} (toScale s) ((fromIntegral.fromEnum) pos)

-- | Gets the position in which the current value is displayed.
--
scaleGetValuePos :: ScaleClass s => s -> IO PositionType
scaleGetValuePos s =
  liftM (toEnum.fromIntegral) $ {#call unsafe scale_get_value_pos#} (toScale s)

