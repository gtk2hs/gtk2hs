-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Box
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Version $Revision: 1.7 $ from $Date: 2005/04/02 19:02:21 $
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
-- Base class for box containers
--
module Graphics.UI.Gtk.Abstract.Box (
-- * Detail
-- 
-- | 'Box' is an abstract widget which encapsulates functionallity for a
-- particular kind of container, one that organizes a variable number of
-- widgets into a rectangular area. 'Box' currently has two derived classes,
-- 'HBox' and 'VBox'.
--
-- The rectangular area of a 'Box' is organized into either a single row or
-- a single column of child widgets depending upon whether the box is of type
-- 'HBox' or 'VBox', respectively. Thus, all children of a 'Box' are allocated
-- one dimension in common, which is the height of a row, or the width of a
-- column.
--
-- 'Box' uses a notion of /packing/. Packing refers to adding widgets with
-- reference to a particular position in a 'Container'. For a 'Box', there are
-- two reference positions: the /start/ and the /end/ of the box. For a 'VBox',
-- the start is defined as the top of the box and the end is defined as the
-- bottom. For a 'HBox' the start is defined as the left side and the end is
-- defined as the right side.
--
-- Use repeated calls to 'boxPackStart' to pack widgets into a 'Box' from
-- start to end. Use 'boxPackEnd' to add widgets from end to start. You may
-- intersperse these calls and add widgets from both ends of the same 'Box'.
--
-- Use 'boxPackStartDefaults' or 'boxPackEndDefaults' to pack widgets into a
-- 'Box' if you do not need to specify the expand, fill, or padding attributes
-- of the child to be added.
--
-- Because 'Box' is a 'Container', you may also use 'containerAdd' to insert
-- widgets into the box, and they will be packed as if with
-- 'boxPackStartDefaults'. Use 'containerRemove' to remove widgets from the
-- 'Box'.
--
-- Use 'boxSetHomogeneous' to specify whether or not all children of the
-- 'Box' are forced to get the same amount of space.
--
-- Use 'boxSetSpacing' to determine how much space will be minimally placed
-- between all children in the 'Box'.
--
-- Use 'boxReorderChild' to move a 'Box' child to a different place in the
-- box.
--
-- Use 'boxSetChildPacking' to reset the expand, fill, and padding
-- attributes of any 'Box' child. Use 'boxQueryChildPacking' to query these
-- fields.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----Box
-- |                           +----'ButtonBox'
-- |                           +----'VBox'
-- |                           +----'HBox'
-- @

-- * Types
  Box,
  BoxClass,
  castToBox,
  Packing(..),

-- * Methods
  boxPackStart,
  boxPackEnd,
  boxPackStartDefaults,
  boxPackEndDefaults,
  boxGetHomogeneous,
  boxSetHomogeneous,
  boxGetSpacing,
  boxSetSpacing,
  boxReorderChild,
  boxQueryChildPacking,
  boxSetChildPacking,

-- * Properties
  boxSpacing,
  boxHomogeneous
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.Attributes		(Attr(..))
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(PackType(..), Packing(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Adds the @child@ widget to the box, packed with reference to the start of
-- the box. The
-- @child@ is packed after any other child packed with reference to the start
-- of the box.
--
-- The 'Packing' parameter determines how the child behaves in the horizontal
-- or vertical way in an HBox or VBox, respectively. 'PackNatural' means the
-- child is as big as it reqests. It will move to the left in an 'HBox' or to
-- the top in an 'VBox' if there is more space availble.
-- All children packed with 'PackRepel' will be padded on both sides with
-- additional space. 'PackGrow' will increase the size of the so that is
-- covers the available space.
--
boxPackStart :: (BoxClass self, WidgetClass child) => self
 -> child -- ^ @child@ - the 'Widget' to be added to the box.
 -> Packing
 -> Int   -- ^ @padding@ - extra space in pixels to put between this child and
          -- its neighbors, over and above the global amount specified by
          -- spacing 'boxSetSpacing'. If @child@
          -- is a widget at one of the reference ends of @box@, then @padding@
          -- pixels are also put between @child@ and the reference edge of
          -- @box@.
 -> IO ()
boxPackStart self child packing padding =
  {# call box_pack_start #}
    (toBox self)
    (toWidget child)
    (fromBool $ packing /= PackNatural)
    (fromBool $ packing == PackGrow)
    (fromIntegral padding)

-- | Adds the @child@ widget to the box, packed with reference to the end of 
-- the box. The
-- @child@ is packed after (away from end of) any other child packed with
-- reference to the end of the box.
--
-- See 'boxPackStart' for a description of the 'Packing' parameter. Of course
-- for 'boxPackEnd' the 'PackNatural' option will move a child to the right in
-- an 'HBox' or to the bottom in an 'VBox' if there is more space availble.
--
boxPackEnd :: (BoxClass self, WidgetClass child) => self
 -> child -- ^ @child@ - the 'Widget' to be added to the box.
 -> Packing
 -> Int   -- ^ @padding@ - extra space in pixels to put between this child and
          -- its neighbors, over and above the global amount specified by
          -- spacing 'boxSetSpacing'. If @child@
          -- is a widget at one of the reference ends of @box@, then @padding@
          -- pixels are also put between @child@ and the reference edge of
          -- @box@.
 -> IO ()
boxPackEnd self child packing padding =
  {# call box_pack_end #}
    (toBox self)
    (toWidget child)
    (fromBool $ packing /= PackNatural)
    (fromBool $ packing == PackGrow)
    (fromIntegral padding)

-- | Like 'boxPackStart' but uses the default parameters 'PackRepel' and 0 for
-- padding.
--
boxPackStartDefaults :: (BoxClass self, WidgetClass widget) => self
 -> widget -- ^ @widget@ - the 'Widget' to be added to the box.
 -> IO ()
boxPackStartDefaults self widget =
  {# call box_pack_start_defaults #}
    (toBox self)
    (toWidget widget)

-- | Like 'boxPackEnd' but uses the default parameters 'PackRepel' and 0 for
-- padding.
--
boxPackEndDefaults :: (BoxClass self, WidgetClass widget) => self
 -> widget -- ^ @widget@ - the 'Widget' to be added to the box.
 -> IO ()
boxPackEndDefaults self widget =
  {# call box_pack_end_defaults #}
    (toBox self)
    (toWidget widget)

-- | Sets the homogeneous property,
-- controlling whether or not all children of the box are given equal space
--
boxSetHomogeneous :: BoxClass self => self
 -> Bool  -- ^ @homogeneous@ - a boolean value, @True@ to create equal
          -- allotments, @False@ for variable allotments.
 -> IO ()
boxSetHomogeneous self homogeneous =
  {# call box_set_homogeneous #}
    (toBox self)
    (fromBool homogeneous)

-- | Returns whether the box is homogeneous (all children are the same size).
-- See 'boxSetHomogeneous'.
--
boxGetHomogeneous :: BoxClass self => self
 -> IO Bool -- ^ returns @True@ if the box is homogeneous.
boxGetHomogeneous self =
  liftM toBool $
  {# call box_get_homogeneous #}
    (toBox self)

-- | Set the standard spacing between two children.
--
-- This space is in addition to the padding parameter that is given for each
-- child.
--
boxSetSpacing :: BoxClass self => self
 -> Int   -- ^ @spacing@ - the number of pixels to put between children.
 -> IO ()
boxSetSpacing self spacing =
  {# call box_set_spacing #}
    (toBox self)
    (fromIntegral spacing)

-- | Moves @child@ to a new @position@ in the list of @box@ children. The list
-- contains both widgets packed 'PackStart' as well as widgets packed
-- 'PackEnd', in the order that these widgets were added to the box.
--
-- A widget's position in the box children list determines where the
-- widget is packed into the box. A child widget at some position in the list
-- will be packed just after all other widgets of the same packing type that
-- appear earlier in the list.
--
boxReorderChild :: (BoxClass self, WidgetClass child) => self
 -> child -- ^ @child@ - the 'Widget' to move.
 -> Int   -- ^ @position@ - the new position for @child@ in the children list
          -- starting from 0. If negative, indicates the end of the list.
 -> IO ()
boxReorderChild self child position =
  {# call box_reorder_child #}
    (toBox self)
    (toWidget child)
    (fromIntegral position)

-- | Returns information about how @child@ is packed into the box.
--
-- Returns information on the behaviour if free space is available 
-- (in 'Packing'), the additional padding for this widget and if the widget
-- was inserted at the start or end of the container ('PackType').
--
boxQueryChildPacking :: (BoxClass self, WidgetClass child) => self
 -> child            -- ^ @child@ - the 'Widget' of the child to query.
 -> IO (Packing,Int,PackType) -- ^ @(packing, padding, packType)@
boxQueryChildPacking self child =
  alloca $ \expandPtr ->
  alloca $ \fillPtr ->
  alloca $ \paddingPtr ->
  alloca $ \packPtr -> do
  {# call unsafe box_query_child_packing #}
    (toBox self)
    (toWidget child)
    expandPtr
    fillPtr
    paddingPtr
    packPtr
  expand  <- liftM toBool $ peek expandPtr
  fill    <- liftM toBool $ peek fillPtr
  padding <- liftM fromIntegral $ peek paddingPtr
  pack    <- liftM (toEnum.fromIntegral) $ peek packPtr
  return (if fill then PackGrow else 
         (if expand then PackRepel else PackNatural),
	  padding,pack)

-- | Sets the way @child@ is packed into the box.
--
boxSetChildPacking :: (BoxClass self, WidgetClass child) => self
 -> child    -- ^ @child@ - the 'Widget' of the child to set.
 -> Packing
 -> Int      -- ^ @padding@
 -> PackType -- ^ @packType@
 -> IO ()
boxSetChildPacking self child packing padding packType =
  {# call box_set_child_packing #}
    (toBox self)
    (toWidget child)
    (fromBool $ packing /= PackNatural)
    (fromBool $ packing == PackGrow)
    (fromIntegral padding)
    ((fromIntegral . fromEnum) packType)

-- | Retrieves the standard spacing between widgets.
--
boxGetSpacing :: BoxClass self => self
 -> IO Int -- ^ returns spacing between children
boxGetSpacing self =
  liftM fromIntegral $
  {# call unsafe box_get_spacing #}
    (toBox self)

--------------------
-- Properties

-- | The amount of space between children.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
boxSpacing :: BoxClass self => Attr self Int
boxSpacing = Attr 
  boxGetSpacing
  boxSetSpacing

-- | Whether the children should all be the same size.
--
-- Default value: @False@
--
boxHomogeneous :: BoxClass self => Attr self Bool
boxHomogeneous = Attr 
  boxGetHomogeneous
  boxSetHomogeneous
