-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Paned@
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2002/12/19 18:13:39 $
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
-- @description@ --------------------------------------------------------------
--
-- * This abstract widget provides a division line with a handle that can be
--   used by the user to divide the given space between two widgets. The two
--   concrete implementations are HPaned and VPaned.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

module Paned(
  Paned,
  PanedClass,
  castToPaned,
  panedAdd1,
  panedAdd2,
  panedPack1,
  panedPack2,
  panedSetPosition,
  panedGetPosition
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @method panedAdd1@ Add a widget to the first (top or left) area.
--
-- * The widget does not expand if @ref type Paned@ expands. It does not
--   shrink either.
--
panedAdd1 :: (PanedClass p, WidgetClass w) => p -> w -> IO ()
panedAdd1 p w = {#call paned_add1#} (toPaned p) (toWidget w)

-- @method panedAdd2@ Add a widget to the second (bottom or right) area.
--
-- * The widget does not expand if @ref type Paned@ expands. But it does
--   shrink.
--
panedAdd2 :: (PanedClass p, WidgetClass w) => p -> w -> IO ()
panedAdd2 p w = {#call paned_add2#} (toPaned p) (toWidget w)

-- @method panedPack1@ Add a widget to the first area and specify its resizing
-- behaviour.
--
panedPack1 :: (PanedClass p, WidgetClass w) => p -> w -> Bool -> Bool -> IO ()
panedPack1 p w expand shrink = {#call paned_pack1#} 
  (toPaned p) (toWidget w) (fromBool expand) (fromBool shrink)

-- @method panedPack2@ Add a widget to the second area and specify its
-- resizing behaviour.
--
panedPack2 :: (PanedClass p, WidgetClass w) => p -> w -> Bool -> Bool -> IO ()
panedPack2 p w expand shrink = {#call paned_pack2#} 
  (toPaned p) (toWidget w) (fromBool expand) (fromBool shrink)

-- @method panedSetPosition@ Set the gutter to the specified
-- @ref arg position@ (in pixels).
--
panedSetPosition :: PanedClass p => p -> Int -> IO ()
panedSetPosition p position = 
  {#call paned_set_position#} (toPaned p) (fromIntegral position)

-- @method panedGetPosition@ Get the gutter position (in pixels).
--
panedGetPosition :: PanedClass p => p -> IO Int
panedGetPosition p = liftM fromIntegral $
  {#call unsafe paned_get_position#} (toPaned p)
