-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Box
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/25 01:11:31 $
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
-- This abstract container class is instatiated by using HBox or VBox. It 
-- supplies all methods to add and remove children.
--
module Graphics.UI.Gtk.Abstract.Box (
-- * Description
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
  boxSetChildPacking
  ) where

import Monad	(liftM)
import System.Glib.FFI


import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(PackType(..), Packing(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Insert a widget at the beginning of the box
-- container.
--
-- * The 'Packing' parameter determines how the child behaves in the
--   horizontal or vertical way in an HBox or VBox, respectively.
--   'PackNatural' means the child is as big as it reqests. It will
--   move to the left in an 'HBox' or to the top in an
--   'VBox' if there is more space availble.
--   All children
--   that have choosen 'PackRepel' for @p@ will be padded 
--   on both sides with
--   additional space. 'PackGrow' will increase the size of the
--   so that is covers the available space.
--
boxPackStart :: (BoxClass b, WidgetClass w) => b -> w -> Packing -> Int ->
                IO ()
boxPackStart b w p pad = {#call box_pack_start#} (toBox b) (toWidget w)
  (fromBool $ p/=PackNatural) (fromBool $ p==PackGrow) (fromIntegral pad)

-- | Insert a widget at the end of the box container.
--
-- * See 'boxPackStart'. The option 'Natural' will
--   move a child to the right in an 'HBox' or to the bottom in an
--   'VBox' if there is more space availble.
--
boxPackEnd :: (BoxClass b, WidgetClass w) => b -> w -> Packing -> Int -> IO ()
boxPackEnd b w p pad = {#call box_pack_end#} (toBox b) (toWidget w)
  (fromBool $ p/=PackNatural) (fromBool $ p==PackGrow) (fromIntegral pad)

-- | Like 'boxPackStart' but uses the
-- default parameters 'PackRepel' and 0 for padding.
--
boxPackStartDefaults :: (BoxClass b, WidgetClass w) => b -> w -> IO ()
boxPackStartDefaults b w = 
  {#call box_pack_start_defaults#} (toBox b) (toWidget w)

-- | Like 'boxPackEnd' but uses the
-- default parameters 'PackRepel' and 0 for padding.
--
boxPackEndDefaults :: (BoxClass b, WidgetClass w) => b -> w -> IO ()
boxPackEndDefaults b w = 
  {#call box_pack_end_defaults#} (toBox b) (toWidget w)

-- | Set if all children should be spread homogeneous
-- within the box.
--
boxSetHomogeneous :: BoxClass b => b -> Bool -> IO ()
boxSetHomogeneous b homo = 
  {#call box_set_homogeneous#} (toBox b) (fromBool homo)

-- | Get whether the box is homogeneous.
--
boxGetHomogeneous :: BoxClass b => b -> IO Bool
boxGetHomogeneous b =
  liftM toBool $ {#call box_get_homogeneous#} (toBox b)

-- | Set the standard spacing between two children.
--
-- * This space is in addition to the padding parameter that is given for each
--   child.
--
boxSetSpacing :: BoxClass b => b -> Int -> IO ()
boxSetSpacing b spacing =
  {#call box_set_spacing#} (toBox b) (fromIntegral spacing)

-- | Move @child@ to a new @position@
-- (counted from 0) in the box.
--
boxReorderChild :: (BoxClass b, WidgetClass w) => b -> w -> Int -> IO ()
boxReorderChild b w position = 
  {#call box_reorder_child#} (toBox b) (toWidget w) (fromIntegral position)

-- | Query the packing parameter of a child.
--
-- * Returns information on the behaviour if free space is available 
-- (in 'Packing'), the additional padding for this widget and
-- if the widget
-- was inserted at the start or end of the container ('PackType').
--
boxQueryChildPacking :: (BoxClass b, WidgetClass w) => b -> w ->
                        IO (Packing,Int,PackType)
boxQueryChildPacking b w = alloca $ \expandPtr -> alloca $ \fillPtr ->
  alloca $ \paddingPtr -> alloca $ \packPtr -> do
    {#call unsafe box_query_child_packing#} (toBox b) (toWidget w)
      expandPtr fillPtr paddingPtr packPtr
    expand  <- liftM toBool $ peek expandPtr
    fill    <- liftM toBool $ peek fillPtr
    padding <- liftM fromIntegral $ peek paddingPtr
    pack    <- liftM (toEnum.fromIntegral) $ peek packPtr
    return (if fill then PackGrow else 
            (if expand then PackRepel else PackNatural),
	    padding,pack)

-- | Set the packing parameter of a child.
--
boxSetChildPacking :: (BoxClass b, WidgetClass w) => b -> w -> Packing ->
                      Int -> PackType -> IO ()
boxSetChildPacking b w pack pad pt = {#call box_set_child_packing#} (toBox b) 
  (toWidget w) (fromBool $ pack/=PackNatural) (fromBool $ pack==PackGrow) 
  (fromIntegral pad) ((fromIntegral.fromEnum) pt)


-- | Retrieves the standard spacing between widgets.
--
boxGetSpacing :: BoxClass b => b -> IO Int
boxGetSpacing b = 
  liftM fromIntegral $ {#call unsafe box_get_spacing#} (toBox b)
