-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget HScale
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:16:18 $
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
-- * The user may enter a value by moving the handle on the scale.
--
--
--
-- * TODO

module Graphics.UI.Gtk.Entry.HScale (
  HScale,
  HScaleClass,
  castToHScale,
  hScaleNew,
  hScaleNewWithRange
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new HScale widget.
--
hScaleNew :: Adjustment -> IO HScale
hScaleNew adj = makeNewObject mkHScale $ liftM castPtr $
  {#call unsafe hscale_new#} adj

-- | Create a new HScale widget with @min@, @max@ and @step@ values rather than
-- an "Adjustment" object.
--
hScaleNewWithRange :: Double -- ^ Minimum value 
                   -> Double -- ^ Maximum value
                   -> Double -- ^ Step increment (tick size) used with keyboard
		             --   shortcuts. Must be nonzero.
                   -> IO HScale
hScaleNewWithRange min max step =
  makeNewObject mkHScale $ liftM castPtr $
  {#call unsafe hscale_new_with_range#} (realToFrac min) (realToFrac max)
    (realToFrac step)
