-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Layout@
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/24 09:43:25 $
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
-- * A layout widget can hold several widgets at arbitrary positions.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

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
  onSetScrollAdjustments,
  afterSetScrollAdjustments
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor layoutNew@ Create a new layout widget.
--
layoutNew :: Adjustment -> Adjustment -> IO Layout
layoutNew vadjustment hadjustment = makeNewObject mkLayout $ liftM castPtr $
  {#call unsafe layout_new#} hadjustment vadjustment

-- @method layoutPut@ Insert a widget into the layout container.
--
layoutPut :: (LayoutClass l, WidgetClass w) => l -> w -> Int -> Int -> IO ()
layoutPut l widget x y = {#call layout_put#} (toLayout l) (toWidget widget)
  (fromIntegral x) (fromIntegral y)

-- @method layoutMove@ Move an existing widget within the container.
--
layoutMove :: (LayoutClass l, WidgetClass w) => l -> w -> Int -> Int -> IO ()
layoutMove l widget x y = {#call layout_move#} (toLayout l) (toWidget widget)
  (fromIntegral x) (fromIntegral y)

-- @method layoutSetSize@ Set the size of the layout widget.
--
layoutSetSize :: LayoutClass l => l -> Int -> Int -> IO ()
layoutSetSize l width height = {#call layout_set_size#} (toLayout l)
  (fromIntegral width) (fromIntegral height)

-- @method layoutGetHAdjustment@ Retrieve the horizontal @ref arg Adjustment@
-- object from the layout.
--
layoutGetHAdjustment :: LayoutClass l => l -> IO Adjustment
layoutGetHAdjustment l = makeNewObject mkAdjustment $
  {#call unsafe layout_get_hadjustment#} (toLayout l)

-- @method layoutGetVAdjustment@ Retrieve the vertical @ref arg Adjustment@
-- object from the layout.
--
layoutGetVAdjustment :: LayoutClass l => l -> IO Adjustment
layoutGetVAdjustment l = makeNewObject mkAdjustment $
  {#call unsafe layout_get_vadjustment#} (toLayout l)

-- @method layoutSetHAdjustment@ Set the horizontal adjustment object.
--
layoutSetHAdjustment :: LayoutClass l => l -> Adjustment -> IO ()
layoutSetHAdjustment l adj = {#call layout_set_hadjustment#} (toLayout l) adj

-- @method layoutSetVAdjustment@ Set the vertical adjustment object.
--
layoutSetVAdjustment :: LayoutClass l => l -> Adjustment -> IO ()
layoutSetVAdjustment l adj = {#call layout_set_vadjustment#} (toLayout l) adj

-- signals

-- @signal connectToSetScrollAdjustments@ In case the adjustments are
-- replaced, this signal is emitted.
--
onSetScrollAdjustments, afterSetScrollAdjustments :: LayoutClass l => l ->(Adjustment -> Adjustment -> IO ()) ->
                                                     IO (ConnectId l)
onSetScrollAdjustments = 
  connect_OBJECT_OBJECT__NONE "set-scroll-adjustments" False
afterSetScrollAdjustments = 
  connect_OBJECT_OBJECT__NONE "set-scroll-adjustments" True

