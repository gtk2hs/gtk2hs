-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Layout
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/25 01:11:34 $
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
-- Infinite scrollable area containing child widgets and\/or custom drawing
--
module Graphics.UI.Gtk.Layout.Layout (
-- * Description
-- 
-- | 'Layout' is similar to 'DrawingArea' in that it's a \"blank slate\" and
-- doesn't do anything but paint a blank background by default. It's different
-- in that it supports scrolling natively (you can add it to a
-- 'ScrolledWindow'), and it can contain child widgets, since it's a
-- 'Container'. However if you\'re just going to draw, a 'DrawingArea' is a
-- better choice since it has lower overhead.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----Layout
-- @

-- * Types
  Layout,
  LayoutClass,
  castToLayout,

-- * Constructors
  layoutNew,

-- * Methods
  layoutPut,
  layoutMove,
  layoutSetSize,
  layoutGetSize,
  layoutGetHAdjustment,
  layoutGetVAdjustment,
  layoutSetHAdjustment,
  layoutSetVAdjustment,

-- * Signals
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

--------------------
-- Constructors

-- | Create a new layout widget.
--
layoutNew :: Maybe Adjustment -> Maybe Adjustment -> IO Layout
layoutNew vAdj hAdj = makeNewObject mkLayout $ liftM castPtr $
  {#call unsafe layout_new#} (fromMAdj hAdj) (fromMAdj vAdj)
 where
 fromMAdj :: Maybe Adjustment -> Adjustment
 fromMAdj = fromMaybe $ mkAdjustment nullForeignPtr

--------------------
-- Methods

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

--------------------
-- Signals

-- | In case the adjustments are
-- replaced, this signal is emitted.
--
onSetScrollAdjustments, afterSetScrollAdjustments :: LayoutClass l => l ->(Adjustment -> Adjustment -> IO ()) ->
                                                     IO (ConnectId l)
onSetScrollAdjustments = 
  connect_OBJECT_OBJECT__NONE "set-scroll-adjustments" False
afterSetScrollAdjustments = 
  connect_OBJECT_OBJECT__NONE "set-scroll-adjustments" True

