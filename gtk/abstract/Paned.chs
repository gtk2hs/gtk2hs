-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Blah
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:19 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
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
--- DESCRIPTION ---------------------------------------------------------------
--
-- * This abstract widget provides a division line with a handle that can be
--   used by the user to divide the given space between two widgets. The two
--   concrete implementations are HPaned and VPaned.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

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

-- Add a widget to the first (top or left) area. (EXPORTED)
--
-- * The widget does not expand if @Paned expands. It does not shrink either.
--
panedAdd1 :: (PanedClass p, WidgetClass w) => w -> p -> IO ()
panedAdd1 w p = {#call paned_add1#} (toPaned p) (toWidget w)

-- Add a widget to the second (bottom or right) area. (EXPORTED)
--
-- * The widget does not expand if @Paned expands. But it does shrink.
--
panedAdd2 :: (PanedClass p, WidgetClass w) => w -> p -> IO ()
panedAdd2 w p = {#call paned_add2#} (toPaned p) (toWidget w)

-- Add a widget to the first area and specify its resizing behaviour. 
-- (EXPORTED)
--
panedPack1 :: (PanedClass p, WidgetClass w) => w -> Bool -> Bool -> p -> IO ()
panedPack1 w expand shrink p = {#call paned_pack1#} 
  (toPaned p) (toWidget w) (fromBool expand) (fromBool shrink)

-- Add a widget to the second area and specify its resizing behaviour. 
-- (EXPORTED)
--
panedPack2 :: (PanedClass p, WidgetClass w) => w -> Bool -> Bool -> p -> IO ()
panedPack2 w expand shrink p = {#call paned_pack2#} 
  (toPaned p) (toWidget w) (fromBool expand) (fromBool shrink)

-- Set the gutter to the specified @position (in pixels). (EXPORTED)
--
panedSetPosition :: PanedClass p => Int -> p -> IO ()
panedSetPosition position p = 
  {#call paned_set_position#} (toPaned p) (fromIntegral position)

-- Get the gutter position (in pixels). (EXPORTED)
--
panedGetPosition :: PanedClass p => p -> IO Int
panedGetPosition p = liftM fromIntegral $
  {#call unsafe paned_get_position#} (toPaned p)
