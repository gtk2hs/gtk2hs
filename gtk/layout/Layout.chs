-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Layout
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
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
-- * A layout widget can hold several widgets at arbitrary positions.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module Layout(
  Layout,
  LayoutClass,
  castToLayout,
  layoutNew,
  layoutPut,
  layoutMove,
  layoutSetSize,
  layoutGetHAdjustment,
  layoutGetVAdjustment,
  layoutSetHAdjustment,
  layoutSetVAdjustment,
  connectToSetScrollAdjustments
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a new layout widget. (EXPORTED)
--
layoutNew :: Adjustment -> Adjustment -> IO Layout
layoutNew hadjustment vadjustment = makeNewObject mkLayout $ liftM castPtr $
  {#call unsafe layout_new#} hadjustment vadjustment

-- Insert a widget into the layout container. (EXPORTED)
--
layoutPut :: (LayoutClass l, WidgetClass w) => w -> Int -> Int -> l -> IO ()
layoutPut widget x y l = {#call layout_put#} (toLayout l) (toWidget widget)
  (fromIntegral x) (fromIntegral y)

-- Move an existing widget within the container. (EXPORTED)
--
layoutMove :: (LayoutClass l, WidgetClass w) => w -> Int -> Int -> l -> IO ()
layoutMove widget x y l = {#call layout_move#} (toLayout l) (toWidget widget)
  (fromIntegral x) (fromIntegral y)

-- Set the size of the layout widget. (EXPORTED)
--
layoutSetSize :: LayoutClass l => Int -> Int -> l -> IO ()
layoutSetSize width height l = {#call layout_set_size#} (toLayout l)
  (fromIntegral width) (fromIntegral height)

-- Retrieve the horizontal @Adjustment object from the layout. (EXPORTED)
--
layoutGetHAdjustment :: LayoutClass l => l -> IO Adjustment
layoutGetHAdjustment l = makeNewObject mkAdjustment $
  {#call unsafe layout_get_hadjustment#} (toLayout l)

-- Retrieve the vertical @Adjustment object from the layout. (EXPORTED)
--
layoutGetVAdjustment :: LayoutClass l => l -> IO Adjustment
layoutGetVAdjustment l = makeNewObject mkAdjustment $
  {#call unsafe layout_get_vadjustment#} (toLayout l)

-- Set the horizontal adjustment object. (EXPORTED)
--
layoutSetHAdjustment :: LayoutClass l => Adjustment -> l -> IO ()
layoutSetHAdjustment adj l = {#call layout_set_hadjustment#} (toLayout l) adj

-- Set the vertical adjustment object. (EXPORTED)
--
layoutSetVAdjustment :: LayoutClass l => Adjustment -> l -> IO ()
layoutSetVAdjustment adj l = {#call layout_set_vadjustment#} (toLayout l) adj

-- signals

-- In case the adjustments are replaced, this signal is emitted. (EXPORTED)
--
connectToSetScrollAdjustments :: LayoutClass l => 
  (Adjustment -> Adjustment -> IO ()) -> ConnectAfter -> l -> IO (ConnectId l)
connectToSetScrollAdjustments = 
  connect_OBJECT_OBJECT__NONE "set-scroll-adjustments"

