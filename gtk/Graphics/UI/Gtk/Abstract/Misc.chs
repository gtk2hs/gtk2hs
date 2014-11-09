{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Misc
--
--  Author : Manuel M. T. Chakravarty, Axel Simon
--
--  Created: 2 May 2001
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
-- Base class for widgets with alignments and padding
--
module Graphics.UI.Gtk.Abstract.Misc (
-- * Detail
--
-- | The 'Misc' widget is an abstract widget which is not useful itself, but
-- is used to derive subclasses which have alignment and padding attributes.
--
-- The horizontal and vertical padding attributes allows extra space to be
-- added around the widget.
--
-- The horizontal and vertical alignment attributes enable the widget to be
-- positioned within its allocated area. Note that if the widget is added to a
-- container in such a way that it expands automatically to fill its allocated
-- area, the alignment settings will not alter the widgets position.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----Misc
-- |                     +----'Label'
-- |                     +----'Arrow'
-- |                     +----'Image'
-- |                     +----'Pixmap'
-- @

-- * Types
  Misc,
  MiscClass,
  castToMisc, gTypeMisc,
  toMisc,

-- * Methods
  miscSetAlignment,
  miscGetAlignment,
  miscSetPadding,
  miscGetPadding,

-- * Attributes
  miscXalign,
  miscYalign,
  miscXpad,
  miscYpad,
  ) where

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Sets the alignment of the widget.
--
miscSetAlignment :: MiscClass self => self
 -> Float -- ^ @xalign@ - the horizontal alignment, from 0 (left) to 1
          -- (right).
 -> Float -- ^ @yalign@ - the vertical alignment, from 0 (top) to 1 (bottom).
 -> IO ()
miscSetAlignment self xalign yalign =
  {# call misc_set_alignment #}
    (toMisc self)
    (realToFrac xalign)
    (realToFrac yalign)

-- | Gets the X and Y alignment of the widget within its allocation. See
-- 'miscSetAlignment'.
--
miscGetAlignment :: MiscClass self => self
 -> IO (Double, Double)
miscGetAlignment self =
  alloca $ \xalignPtr ->
  alloca $ \yalignPtr -> do
  {# call unsafe misc_get_alignment #}
    (toMisc self)
    xalignPtr
    yalignPtr
  xalign <- peek xalignPtr
  yalign <- peek yalignPtr
  return (realToFrac xalign, realToFrac yalign)

-- | Sets the amount of space to add around the widget.
--
miscSetPadding :: MiscClass self => self
 -> Int   -- ^ @xpad@ - the amount of space to add on the left and right of
          -- the widget, in pixels.
 -> Int   -- ^ @ypad@ - the amount of space to add on the top and bottom of
          -- the widget, in pixels.
 -> IO ()
miscSetPadding self xpad ypad =
  {# call misc_set_padding #}
    (toMisc self)
    (fromIntegral xpad)
    (fromIntegral ypad)

-- | Gets the padding in the X and Y directions of the widget. See
-- 'miscSetPadding'.
--
miscGetPadding :: MiscClass self => self
 -> IO (Int, Int)
miscGetPadding self =
  alloca $ \xpadPtr ->
  alloca $ \ypadPtr -> do
  {# call unsafe misc_get_padding #}
    (toMisc self)
    xpadPtr
    ypadPtr
  xpad <- peek xpadPtr
  ypad <- peek ypadPtr
  return (fromIntegral xpad, fromIntegral ypad)

--------------------
-- Attributes

-- | The horizontal alignment, from 0 (left) to 1 (right). Reversed for RTL
-- layouts.
--
-- Allowed values: [0,1]
--
-- Default value: 0.5
--
miscXalign :: MiscClass self => Attr self Float
miscXalign = newAttrFromFloatProperty "xalign"

-- | The vertical alignment, from 0 (top) to 1 (bottom).
--
-- Allowed values: [0,1]
--
-- Default value: 0.5
--
miscYalign :: MiscClass self => Attr self Float
miscYalign = newAttrFromFloatProperty "yalign"

-- | The amount of space to add on the left and right of the widget, in
-- pixels.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
miscXpad :: MiscClass self => Attr self Int
miscXpad = newAttrFromIntProperty "xpad"

-- | The amount of space to add on the top and bottom of the widget, in
-- pixels.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
miscYpad :: MiscClass self => Attr self Int
miscYpad = newAttrFromIntProperty "ypad"
