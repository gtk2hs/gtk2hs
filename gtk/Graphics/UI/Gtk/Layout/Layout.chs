-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Layout
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:23:39 $
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
-- A layout widget can hold several widgets at arbitrary positions.
--

module Graphics.UI.Gtk.Layout.Layout (
  Layout,
  LayoutClass,
  castToLayout,
  layoutNew,
  layoutPut,
  layoutMove,
  layoutSetSize,
  layoutGetSize,
  layoutGetHAdjustment,
  layoutGetVAdjustment,
  layoutSetHAdjustment,
  layoutSetVAdjustment,
  onSetScrollAdjustments,
  afterSetScrollAdjustments
  ) where

import Maybe	(fromMaybe)
import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new layout widget.
--
layoutNew :: Maybe Adjustment -> Maybe Adjustment -> IO Layout
layoutNew vAdj hAdj = makeNewObject mkLayout $ liftM castPtr $
  {#call unsafe layout_new#} (fromMAdj hAdj) (fromMAdj vAdj)
 where
 fromMAdj :: Maybe Adjustment -> Adjustment
 fromMAdj = fromMaybe $ mkAdjustment nullForeignPtr


-- | Insert a widget into the layout container.
--
layoutPut :: (LayoutClass l, WidgetClass w) => l -> w -> Int -> Int -> IO ()
layoutPut l widget x y = {#call layout_put#} (toLayout l) (toWidget widget)
  (fromIntegral x) (fromIntegral y)

-- | Move an existing widget within the container.
--
layoutMove :: (LayoutClass l, WidgetClass w) => l -> w -> Int -> Int -> IO ()
layoutMove l widget x y = {#call layout_move#} (toLayout l) (toWidget widget)
  (fromIntegral x) (fromIntegral y)

-- | Set the size of the layout widget.
--
layoutSetSize :: LayoutClass l => l -> Int -> Int -> IO ()
layoutSetSize l width height = {#call layout_set_size#} (toLayout l)
  (fromIntegral width) (fromIntegral height)

-- | Get the size of the layout widget.
--
layoutGetSize :: LayoutClass l => l -> IO (Int, Int)
layoutGetSize l =
  alloca $ \widthPtr -> alloca $ \heightPtr -> do
  {#call unsafe layout_get_size#} (toLayout l) widthPtr heightPtr
  width <-peek widthPtr
  height <- peek heightPtr
  return (fromIntegral width, fromIntegral height)

-- | Retrieve the horizontal 'Adjustment' object from the layout.
--
layoutGetHAdjustment :: LayoutClass l => l -> IO Adjustment
layoutGetHAdjustment l = makeNewObject mkAdjustment $
  {#call unsafe layout_get_hadjustment#} (toLayout l)

-- | Retrieve the vertical 'Adjustment' object from the layout.
--
layoutGetVAdjustment :: LayoutClass l => l -> IO Adjustment
layoutGetVAdjustment l = makeNewObject mkAdjustment $
  {#call unsafe layout_get_vadjustment#} (toLayout l)

-- | Set the horizontal adjustment object.
--
layoutSetHAdjustment :: LayoutClass l => l -> Adjustment -> IO ()
layoutSetHAdjustment l adj = {#call layout_set_hadjustment#} (toLayout l) adj

-- | Set the vertical adjustment object.
--
layoutSetVAdjustment :: LayoutClass l => l -> Adjustment -> IO ()
layoutSetVAdjustment l adj = {#call layout_set_vadjustment#} (toLayout l) adj

-- signals

-- | In case the adjustments are
-- replaced, this signal is emitted.
--
onSetScrollAdjustments, afterSetScrollAdjustments :: LayoutClass l => l ->(Adjustment -> Adjustment -> IO ()) ->
                                                     IO (ConnectId l)
onSetScrollAdjustments = 
  connect_OBJECT_OBJECT__NONE "set-scroll-adjustments" False
afterSetScrollAdjustments = 
  connect_OBJECT_OBJECT__NONE "set-scroll-adjustments" True

