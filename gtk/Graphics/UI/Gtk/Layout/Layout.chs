{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Layout
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
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
-- * Detail
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
  castToLayout, gTypeLayout,
  toLayout,

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
  layoutGetDrawWindow,

-- * Attributes
  layoutHAdjustment,
  layoutVAdjustment,
  layoutWidth,
  layoutHeight,

-- * Child Attributes
  layoutChildX,
  layoutChildY,

-- * Signals
  onSetScrollAdjustments,
  afterSetScrollAdjustments,
  ) where

import Data.Maybe       (fromMaybe)
import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.Abstract.ContainerChildProperties

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'Layout'. Unless you have a specific adjustment you'd like
-- the layout to use for scrolling, pass @Nothing@ for @hadjustment@ and
-- @vadjustment@.
--
layoutNew ::
    Maybe Adjustment -- ^ @hadjustment@ - horizontal scroll adjustment, or
                     -- @Nothing@
 -> Maybe Adjustment -- ^ @vadjustment@ - vertical scroll adjustment, or
                     -- @Nothing@
 -> IO Layout
layoutNew hadjustment vadjustment =
  makeNewObject mkLayout $
  liftM (castPtr :: Ptr Widget -> Ptr Layout) $
  {# call unsafe layout_new #}
    (fromMaybe (Adjustment nullForeignPtr) hadjustment)
    (fromMaybe (Adjustment nullForeignPtr) vadjustment)

--------------------
-- Methods

-- | Adds @childWidget@ to @layout@, at position @(x,y)@. @layout@ becomes
-- the new parent container of @childWidget@.
--
layoutPut :: (LayoutClass self, WidgetClass childWidget) => self
 -> childWidget -- ^ @childWidget@ - child widget
 -> Int         -- ^ @x@ - X position of child widget
 -> Int         -- ^ @y@ - Y position of child widget
 -> IO ()
layoutPut self childWidget x y =
  {# call layout_put #}
    (toLayout self)
    (toWidget childWidget)
    (fromIntegral x)
    (fromIntegral y)

-- | Moves a current child of @layout@ to a new position.
--
layoutMove :: (LayoutClass self, WidgetClass childWidget) => self
 -> childWidget -- ^ @childWidget@ - a current child of @layout@
 -> Int         -- ^ @x@ - X position to move to
 -> Int         -- ^ @y@ - Y position to move to
 -> IO ()
layoutMove self childWidget x y =
  {# call layout_move #}
    (toLayout self)
    (toWidget childWidget)
    (fromIntegral x)
    (fromIntegral y)

-- | Sets the size of the scrollable area of the layout.
--
layoutSetSize :: LayoutClass self => self
 -> Int   -- ^ @width@ - width of entire scrollable area
 -> Int   -- ^ @height@ - height of entire scrollable area
 -> IO ()
layoutSetSize self width height =
  {# call layout_set_size #}
    (toLayout self)
    (fromIntegral width)
    (fromIntegral height)

-- | Gets the size that has been set on the layout, and that determines the
-- total extents of the layout's scrollbar area. See 'layoutSetSize'.
--
layoutGetSize :: LayoutClass self => self
 -> IO (Int, Int) -- ^ @(width, height)@
layoutGetSize self =
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
  {# call unsafe layout_get_size #}
    (toLayout self)
    widthPtr
    heightPtr
  width <-peek widthPtr
  height <- peek heightPtr
  return (fromIntegral width, fromIntegral height)

-- | This function should only be called after the layout has been placed in a
-- 'ScrolledWindow' or otherwise configured for scrolling. It returns the
-- 'Adjustment' used for communication between the horizontal scrollbar and
-- @layout@.
--
-- See 'ScrolledWindow', 'Scrollbar', 'Adjustment' for details.
--
layoutGetHAdjustment :: LayoutClass self => self
 -> IO Adjustment -- ^ returns horizontal scroll adjustment
layoutGetHAdjustment self =
  makeNewObject mkAdjustment $
  {# call unsafe layout_get_hadjustment #}
    (toLayout self)

-- | This function should only be called after the layout has been placed in a
-- 'ScrolledWindow' or otherwise configured for scrolling. It returns the
-- 'Adjustment' used for communication between the vertical scrollbar and
-- @layout@.
--
-- See 'ScrolledWindow', 'Scrollbar', 'Adjustment' for details.
--
layoutGetVAdjustment :: LayoutClass self => self
 -> IO Adjustment -- ^ returns vertical scroll adjustment
layoutGetVAdjustment self =
  makeNewObject mkAdjustment $
  {# call unsafe layout_get_vadjustment #}
    (toLayout self)

-- | Sets the horizontal scroll adjustment for the layout.
--
-- See 'ScrolledWindow', 'Scrollbar', 'Adjustment' for details.
--
layoutSetHAdjustment :: LayoutClass self => self
 -> Adjustment -- ^ @adjustment@ - new scroll adjustment
 -> IO ()
layoutSetHAdjustment self adjustment =
  {# call layout_set_hadjustment #}
    (toLayout self)
    adjustment

-- | Sets the vertical scroll adjustment for the layout.
--
-- See 'ScrolledWindow', 'Scrollbar', 'Adjustment' for details.
--
layoutSetVAdjustment :: LayoutClass self => self
 -> Adjustment -- ^ @adjustment@ - new scroll adjustment
 -> IO ()
layoutSetVAdjustment self adjustment =
  {# call layout_set_vadjustment #}
    (toLayout self)
    adjustment

-- | Retrieves the 'Drawable' part of the layout used for drawing operations.
--
layoutGetDrawWindow :: Layout -> IO DrawWindow
layoutGetDrawWindow lay = makeNewGObject mkDrawWindow $
  {# call layout_get_bin_window #}
    (toLayout lay)

--------------------
-- Attributes

-- | The 'Adjustment' for the horizontal position.
--
layoutHAdjustment :: LayoutClass self => Attr self Adjustment
layoutHAdjustment = newAttr
  layoutGetHAdjustment
  layoutSetHAdjustment

-- | The 'Adjustment' for the vertical position.
--
layoutVAdjustment :: LayoutClass self => Attr self Adjustment
layoutVAdjustment = newAttr
  layoutGetVAdjustment
  layoutSetVAdjustment

-- | The width of the layout.
--
-- Allowed values: \<= @('maxBound' :: Int)@
--
-- Default value: 100
--
layoutWidth :: LayoutClass self => Attr self Int
layoutWidth = newAttrFromUIntProperty "width"

-- | The height of the layout.
--
-- Allowed values: \<= @('maxBound' :: Int)@
--
-- Default value: 100
--
layoutHeight :: LayoutClass self => Attr self Int
layoutHeight = newAttrFromUIntProperty "height"

--------------------
-- Child Attributes

-- | X position of child widget.
--
-- Default value: 0
--
layoutChildX :: (LayoutClass self, WidgetClass child) => child -> Attr self Int
layoutChildX = newAttrFromContainerChildIntProperty "x"

-- | Y position of child widget.
--
-- Default value: 0
--
layoutChildY :: (LayoutClass self, WidgetClass child) => child -> Attr self Int
layoutChildY = newAttrFromContainerChildIntProperty "y"

--------------------
-- Signals

-- | In case the adjustments are replaced, this signal is emitted.
--
onSetScrollAdjustments, afterSetScrollAdjustments :: LayoutClass self => self
 -> (Adjustment -> Adjustment -> IO ())
 -> IO (ConnectId self)
onSetScrollAdjustments = connect_OBJECT_OBJECT__NONE "set-scroll-adjustments" False
afterSetScrollAdjustments = connect_OBJECT_OBJECT__NONE "set-scroll-adjustments" True
