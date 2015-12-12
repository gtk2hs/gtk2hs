{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Box
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
-- Base class for box containers
--
module Graphics.UI.Gtk.Abstract.Box (
-- * Detail
--
-- | 'Box' is an abstract widget which encapsulates functionality for a
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
-- Besides adding widgets at the start or the end of a box, you can also
-- specify the padding around each widget (in pixels) and a 'Packing'
-- parameter that denotes how to fill up unused space.
--
-- While the right amount of padding around each widget is a matter of
-- appearance, the 'Packing' paramter specifies the way the widgets in
-- the container behave when the window is resized and thereby affect
-- the usability. Hence, once you have created a window, you should resize
-- it and see if the widgets behave as expected. The 'Packing' parameter of
-- each child widget determines how excess space is used by that particular
-- widget. See the description of 'Packing' for a detailed explanaition.
--
-- Because 'Box' is a 'Container', you may also use
-- 'Graphics.UI.Gtk.Abstract.Container.containerAdd' to insert widgets into
-- the box, and they will be packed as if with 'boxPackStart' with 'PackRepel'
-- and 0 padding. Use 'Graphics.UI.Gtk.Abstract.Container.containerRemove' to
-- remove widgets from the 'Box'.
--
-- Use 'boxSetHomogeneous' to specify whether or not all children of the
-- 'Box' are forced to get the same amount of space. Note that the
-- 'Packing' options 'PackNatural' and 'PackRepel' coincide if space is
-- allotted homogeneously.
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
  castToBox, gTypeBox,
  toBox,
  Packing(..),

-- * Methods
  boxPackStart,
  boxPackEnd,
#if GTK_MAJOR_VERSION < 3
  boxPackStartDefaults,
  boxPackEndDefaults,
#endif
  boxGetHomogeneous,
  boxSetHomogeneous,
  boxGetSpacing,
  boxSetSpacing,
  boxReorderChild,
  boxQueryChildPacking,
  boxSetChildPacking,
#if GTK_CHECK_VERSION(3,10,0)
  boxGetBaselinePosition,
  boxSetBaselinePosition,
#endif
#if GTK_CHECK_VERSION(3,12,0)
  boxGetCenterWidget,
  boxSetCenterWidget,
#endif

-- * Attributes
  boxSpacing,
  boxHomogeneous,
#if GTK_CHECK_VERSION(3,10,0)
  boxBaselinePosition,
#endif
#if GTK_CHECK_VERSION(3,12,0)
  boxCenterWidget,
#endif

-- * Child Attributes
  boxChildPacking,
  boxChildPadding,
  boxChildPackType,
  boxChildPosition,
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.General.Enums    (PackType(..), Packing(..),
                                        toPacking, fromPacking)
import Graphics.UI.Gtk.Abstract.ContainerChildProperties

#if GTK_CHECK_VERSION(3,10,0)
import Graphics.UI.Gtk.General.Enums    (BaselinePosition)
#endif
#if GTK_CHECK_VERSION(3,12,0)
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
#endif

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Adds the @child@ widget to the box, packed with reference to the start of
-- the box. The
-- @child@ is packed after any other child packed with reference to the start
-- of the box.
--
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
    (fromBool expand)
    (fromBool fill)
    (fromIntegral padding)
  where (expand, fill) = fromPacking packing

-- | Adds the @child@ widget to the box, packed with reference to the end of
-- the box. The
-- @child@ is packed after (away from end of) any other child packed with
-- reference to the end of the box.
--
-- Note that
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
    (fromBool expand)
    (fromBool fill)
    (fromIntegral padding)
  where (expand, fill) = fromPacking packing

#if GTK_MAJOR_VERSION < 3
-- | Like 'boxPackStart' but uses the default parameters 'PackRepel' and 0 for
-- padding.
--
-- Removed in Gtk3
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
-- Removed in Gtk3
boxPackEndDefaults :: (BoxClass self, WidgetClass widget) => self
 -> widget -- ^ @widget@ - the 'Widget' to be added to the box.
 -> IO ()
boxPackEndDefaults self widget =
  {# call box_pack_end_defaults #}
    (toBox self)
    (toWidget widget)
#endif

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
  return (toPacking expand fill, padding, pack)

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
    (fromBool expand)
    (fromBool fill)
    (fromIntegral padding)
    ((fromIntegral . fromEnum) packType)
  where (expand, fill) = fromPacking packing

#if GTK_CHECK_VERSION(3,10,0)
-- | Gets the value set by `boxSetBaselinePostion`
boxGetBaselinePosition :: BoxClass self => self
 -> IO BaselinePosition
boxGetBaselinePosition self =
  liftM (toEnum . fromIntegral) $
  {# call unsafe box_get_baseline_position #}
    (toBox self)

-- | Sets the baseline position of a box. This affects only
-- horizontal boxes with at least one baseline aligned child.
-- If there is more vertical space available than requested,
-- and the baseline is not allocated by the parent then
-- `position` is used to allocate the baseline wrt the extra
-- space available.
boxSetBaselinePosition :: BoxClass self => self
 -> BaselinePosition
 -> IO ()
boxSetBaselinePosition self position =
  {# call unsafe box_set_baseline_position #}
    (toBox self)
    (fromIntegral $ fromEnum position)
#endif

#if GTK_CHECK_VERSION(3,12,0)
-- | Retrieves the center widget of the box.
boxGetCenterWidget :: BoxClass self => self
 -> IO Widget
boxGetCenterWidget self =
  makeNewObject mkWidget $
  {# call unsafe box_get_center_widget #}
    (toBox self)

-- | Sets a center widget; that is a child widget that will be
-- centered with respect to the full width of the box, even if
-- the children at either side take up different amounts of space.
boxSetCenterWidget :: (BoxClass self, WidgetClass widget) => self
 -> widget
 -> IO ()
boxSetCenterWidget self position =
  {# call unsafe box_set_center_widget #}
    (toBox self)
    (toWidget position)
#endif

-- | Retrieves the standard spacing between widgets.
--
boxGetSpacing :: BoxClass self => self
 -> IO Int -- ^ returns spacing between children
boxGetSpacing self =
  liftM fromIntegral $
  {# call unsafe box_get_spacing #}
    (toBox self)

--------------------
-- Attributes

-- | The amount of space between children.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
boxSpacing :: BoxClass self => Attr self Int
boxSpacing = newAttr
  boxGetSpacing
  boxSetSpacing

-- | Whether the children should all be the same size.
--
-- Default value: @False@
--
boxHomogeneous :: BoxClass self => Attr self Bool
boxHomogeneous = newAttr
  boxGetHomogeneous
  boxSetHomogeneous

#if GTK_CHECK_VERSION(3,10,0)
-- | The position of the baseline aligned widgets if extra space is available.
boxBaselinePosition :: BoxClass self => Attr self BaselinePosition
boxBaselinePosition = newAttr
  boxGetBaselinePosition
  boxSetBaselinePosition
#endif

#if GTK_CHECK_VERSION(3,12,0)
-- | A child widget that will be centered with respect to the
-- full width of the box, even if the children at either side
-- take up different amounts of space.
boxCenterWidget :: (BoxClass self, WidgetClass widget) => ReadWriteAttr self Widget widget
boxCenterWidget = newAttr
  boxGetCenterWidget
  boxSetCenterWidget
#endif

--------------------
-- Child Attributes


-- | The packing style of the child.
--
-- Default value: @'PackGrow'@
--
boxChildPacking :: (BoxClass self, WidgetClass child) => child -> Attr self Packing
boxChildPacking child = newAttr
  (\container -> do
     expand <- containerChildGetPropertyBool "expand" child container
     fill   <- containerChildGetPropertyBool "fill"   child container
     return (toPacking expand fill))
  (\container packing ->
     case fromPacking packing of
       (expand, fill) -> do
         containerChildSetPropertyBool "expand" child container expand
         containerChildSetPropertyBool "fill"   child container fill)

-- | Extra space to put between the child and its neighbors, in pixels.
--
-- Allowed values: \<= @('maxBound' :: Int)@
--
-- Default value: 0
--
boxChildPadding :: (BoxClass self, WidgetClass child) => child -> Attr self Int
boxChildPadding = newAttrFromContainerChildUIntProperty "padding"

-- | A 'PackType' indicating whether the child is packed with reference to the
-- start or end of the parent.
--
-- Default value: 'PackStart'
--
boxChildPackType :: (BoxClass self, WidgetClass child) => child -> Attr self PackType
boxChildPackType = newAttrFromContainerChildEnumProperty "pack-type"
                     {# call pure unsafe gtk_pack_type_get_type #}

-- | The index of the child in the parent.
--
-- Allowed values: >= -1
--
-- Default value: 0
--
boxChildPosition :: (BoxClass self, WidgetClass child) => child -> Attr self Int
boxChildPosition = newAttrFromContainerChildIntProperty "position"
