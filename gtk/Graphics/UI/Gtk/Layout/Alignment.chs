{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Alignment
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
-- A widget which controls the alignment and size of its child
--
module Graphics.UI.Gtk.Layout.Alignment (
-- * Detail
--
-- | The 'Alignment' widget controls the alignment and size of its child
-- widget. It has four settings: xscale, yscale, xalign, and yalign.
--
-- The scale settings are used to specify how much the child widget should
-- expand to fill the space allocated to the 'Alignment'. The values can range
-- from 0 (meaning the child doesn't expand at all) to 1 (meaning the child
-- expands to fill all of the available space).
--
-- The align settings are used to place the child widget within the
-- available area. The values range from 0 (top or left) to 1 (bottom or
-- right). Of course, if the scale settings are both set to 1, the alignment
-- settings have no effect.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----Alignment
-- @

-- * Types
  Alignment,
  AlignmentClass,
  castToAlignment, gTypeAlignment,
  toAlignment,

-- * Constructors
  alignmentNew,

-- * Methods
  alignmentSet,
#if GTK_CHECK_VERSION(2,4,0)
  alignmentSetPadding,
  alignmentGetPadding,
#endif

-- * Attributes
  alignmentXAlign,
  alignmentYAlign,
  alignmentXScale,
  alignmentYScale,
#if GTK_CHECK_VERSION(2,4,0)
  alignmentTopPadding,
  alignmentBottomPadding,
  alignmentLeftPadding,
  alignmentRightPadding,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'Alignment'.
--
alignmentNew ::
    Float        -- ^ @xalign@ - the horizontal alignment of the child widget,
                 -- from 0 (left) to 1 (right).
 -> Float        -- ^ @yalign@ - the vertical alignment of the child widget,
                 -- from 0 (top) to 1 (bottom).
 -> Float        -- ^ @xscale@ - the amount that the child widget expands
                 -- horizontally to fill up unused space, from 0 to 1. A value
                 -- of 0 indicates that the child widget should never expand. A
                 -- value of 1 indicates that the child widget will expand to
                 -- fill all of the space allocated for the 'Alignment'.
 -> Float        -- ^ @yscale@ - the amount that the child widget expands
                 -- vertically to fill up unused space, from 0 to 1. The values
                 -- are similar to @xscale@.
 -> IO Alignment
alignmentNew xalign yalign xscale yscale =
  makeNewObject mkAlignment $
  liftM (castPtr :: Ptr Widget -> Ptr Alignment) $
  {# call unsafe alignment_new #}
    (realToFrac xalign)
    (realToFrac yalign)
    (realToFrac xscale)
    (realToFrac yscale)

--------------------
-- Methods

-- | Sets the 'Alignment' values.
--
alignmentSet :: AlignmentClass self => self
 -> Float -- ^ @xalign@ - the horizontal alignment of the child widget, from 0
          -- (left) to 1 (right).
 -> Float -- ^ @yalign@ - the vertical alignment of the child widget, from 0
          -- (top) to 1 (bottom).
 -> Float -- ^ @xscale@ - the amount that the child widget expands
          -- horizontally to fill up unused space, from 0 to 1. A value of 0
          -- indicates that the child widget should never expand. A value of 1
          -- indicates that the child widget will expand to fill all of the
          -- space allocated for the 'Alignment'.
 -> Float -- ^ @yscale@ - the amount that the child widget expands vertically
          -- to fill up unused space, from 0 to 1. The values are similar to
          -- @xscale@.
 -> IO ()
alignmentSet self xalign yalign xscale yscale =
  {# call alignment_set #}
    (toAlignment self)
    (realToFrac xalign)
    (realToFrac yalign)
    (realToFrac xscale)
    (realToFrac yscale)

#if GTK_CHECK_VERSION(2,4,0)
-- | Sets the padding on the different sides of the widget. The padding adds
-- blank space to the sides of the widget. For instance, this can be used to
-- indent the child widget towards the right by adding padding on the left.
--
-- * Available since Gtk+ version 2.4
--
alignmentSetPadding :: AlignmentClass self => self
 -> Int   -- ^ @paddingTop@ - the padding at the top of the widget
 -> Int   -- ^ @paddingBottom@ - the padding at the bottom of the widget
 -> Int   -- ^ @paddingLeft@ - the padding at the left of the widget
 -> Int   -- ^ @paddingRight@ - the padding at the right of the widget.
 -> IO ()
alignmentSetPadding self paddingTop paddingBottom paddingLeft paddingRight =
  {# call gtk_alignment_set_padding #}
    (toAlignment self)
    (fromIntegral paddingTop)
    (fromIntegral paddingBottom)
    (fromIntegral paddingLeft)
    (fromIntegral paddingRight)

-- | Gets the padding on the different sides of the widget. See
-- 'alignmentSetPadding'.
--
-- * Available since Gtk+ version 2.4
--
alignmentGetPadding :: AlignmentClass self => self
 -> IO (Int, Int, Int, Int) -- ^ @(paddingTop, paddingBottom, paddingLeft,
                            -- paddingRight)@ - the padding at the top,
                            -- bottom, left and right of the widget.
alignmentGetPadding self =
  alloca $ \paddingTopPtr ->
  alloca $ \paddingBottomPtr ->
  alloca $ \paddingLeftPtr ->
  alloca $ \paddingRightPtr -> do
  {# call gtk_alignment_get_padding #}
    (toAlignment self)
    paddingTopPtr
    paddingBottomPtr
    paddingLeftPtr
    paddingRightPtr
  paddingTop <- peek paddingTopPtr
  paddingBottom <- peek paddingBottomPtr
  paddingLeft <- peek paddingLeftPtr
  paddingRight <- peek paddingRightPtr
  return (fromIntegral paddingTop, fromIntegral paddingBottom
         ,fromIntegral paddingLeft, fromIntegral paddingRight)
#endif

--------------------
-- Attributes

-- | Horizontal position of child in available space. 0.0 is left aligned, 1.0
-- is right aligned.
--
-- Allowed values: [0,1]
--
-- Default value: 0.5
--
alignmentXAlign :: AlignmentClass self => Attr self Float
alignmentXAlign = newAttrFromFloatProperty "xalign"

-- | Vertical position of child in available space. 0.0 is top aligned, 1.0 is
-- bottom aligned.
--
-- Allowed values: [0,1]
--
-- Default value: 0.5
--
alignmentYAlign :: AlignmentClass self => Attr self Float
alignmentYAlign = newAttrFromFloatProperty "yalign"

-- | If available horizontal space is bigger than needed for the child, how
-- much of it to use for the child. 0.0 means none, 1.0 means all.
--
-- Allowed values: [0,1]
--
-- Default value: 1
--
alignmentXScale :: AlignmentClass self => Attr self Float
alignmentXScale = newAttrFromFloatProperty "xscale"

-- | If available vertical space is bigger than needed for the child, how much
-- of it to use for the child. 0.0 means none, 1.0 means all.
--
-- Allowed values: [0,1]
--
-- Default value: 1
--
alignmentYScale :: AlignmentClass self => Attr self Float
alignmentYScale = newAttrFromFloatProperty "yscale"

#if GTK_CHECK_VERSION(2,4,0)
-- | The padding to insert at the top of the widget.
--
-- Allowed values: \<= @('maxBound' :: Int)@
--
-- Default value: 0
--
alignmentTopPadding :: AlignmentClass self => Attr self Int
alignmentTopPadding = newAttrFromUIntProperty "top-padding"

-- | The padding to insert at the bottom of the widget.
--
-- Allowed values: \<= @('maxBound' :: Int)@
--
-- Default value: 0
--
alignmentBottomPadding :: AlignmentClass self => Attr self Int
alignmentBottomPadding = newAttrFromUIntProperty "bottom-padding"

-- | The padding to insert at the left of the widget.
--
-- Allowed values: \<= @('maxBound' :: Int)@
--
-- Default value: 0
--
alignmentLeftPadding :: AlignmentClass self => Attr self Int
alignmentLeftPadding = newAttrFromUIntProperty "left-padding"

-- | The padding to insert at the right of the widget.
--
-- Allowed values: \<= @('maxBound' :: Int)@
--
-- Default value: 0
--
alignmentRightPadding :: AlignmentClass self => Attr self Int
alignmentRightPadding = newAttrFromUIntProperty "right-padding"
#endif
