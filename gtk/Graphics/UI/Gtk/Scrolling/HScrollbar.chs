-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget HScrollbar
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:32:12 $
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
-- This widget provides a stand-alone scrollbar. All interesting functions
-- can be found in 'Range', from which it is derived.
--

module Graphics.UI.Gtk.Scrolling.HScrollbar (
  HScrollbar,
  HScrollbarClass,
  castToHScrollbar,
  hScrollbarNew,
  hScrollbarNewDefaults
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new HScrollbar.
--
hScrollbarNew :: Adjustment -> IO HScrollbar
hScrollbarNew adj = makeNewObject mkHScrollbar $ liftM castPtr $
  {#call unsafe hscrollbar_new#} adj

-- | Create a new HScrollbar without an 'Adjustment'.
--
hScrollbarNewDefaults :: IO HScrollbar
hScrollbarNewDefaults = makeNewObject mkHScrollbar $ liftM castPtr $
  {#call unsafe hscrollbar_new#} (mkAdjustment nullForeignPtr)


