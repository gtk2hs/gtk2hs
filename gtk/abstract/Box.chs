-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Box
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:19 $
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
-- * This abstract container class is instatiated by using HBox or VBox. It 
--   supplies all methods to add and remove children.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module Box(
  Box,
  BoxClass,
  castToBox,
  Packing(..),
  boxPackStart,
  boxPackEnd,
  boxPackStartDefaults,
  boxPackEndDefaults,
  boxSetHomogeneous,
  boxGetSpacing,
  boxSetSpacing,
  boxReorderChild,
  boxQueryChildPacking,
  boxSetChildPacking
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(PackType(..), Packing(..))

{# context lib="gtk" prefix="gtk" #}

-- methods


-- Insert a widget at the beginning of the box container. (EXPORTED)
--
-- * The @Packing parameter determines how the child behaves in the
--   horizontal or vertical way in an HBox or VBox, respectively.
--   @Natural means the child is as big as it reqests. All children
--   that have choosen @Expand for @p will be padded with the 
--   remaining space. @Fill is the same as @Expand except that the
--   child will receive the superfluous space.
--
boxPackStart :: (BoxClass b, WidgetClass w) => 
  w -> Packing -> Int -> b -> IO ()
boxPackStart w p pad b = {#call box_pack_start#} (toBox b) (toWidget w)
  (fromBool $ p/=PackNatural) (fromBool $ p==PackFill) (fromIntegral pad)

-- Insert a widget at the end of the box container. (EXPORTED)
--
-- * See @boxPackStart.
--
boxPackEnd :: (BoxClass b, WidgetClass w) => 
  w -> Packing -> Int -> b -> IO ()
boxPackEnd w p pad b = {#call box_pack_end#} (toBox b) (toWidget w)
  (fromBool $ p/=PackNatural) (fromBool $ p==PackFill) (fromIntegral pad)


-- Like @boxPackStart but uses the default parameters @Fill and 0 for 
-- @Padding. (EXPORTED)
--
boxPackStartDefaults :: (BoxClass b, WidgetClass w) => w -> b -> IO ()
boxPackStartDefaults w b = 
  {#call box_pack_start_defaults#} (toBox b) (toWidget w)

-- Like @boxPackEnd but uses the default parameters @Fill and 0 for @Padding. 
-- (EXPORTED)
--
boxPackEndDefaults :: (BoxClass b, WidgetClass w) => w -> b -> IO ()
boxPackEndDefaults w b = 
  {#call box_pack_end_defaults#} (toBox b) (toWidget w)

-- Set if all children should be spread homogeneous withing the box. (EXPORTED)
--
boxSetHomogeneous :: BoxClass b => Bool -> b -> IO ()
boxSetHomogeneous homo b = 
  {#call box_set_homogeneous#} (toBox b) (fromBool homo)

-- Set the standard spacing between two children. (EXPORTED)
--
-- * This space is in addition to the padding parameter that is given for
--   each child.
--
boxSetSpacing :: BoxClass b => Int -> b -> IO ()
boxSetSpacing spacing b =
  {#call box_set_spacing#} (toBox b) (fromIntegral spacing)

-- Move @child to a new @position (counted from 0) in the box. (EXPORTED)
--
boxReorderChild :: (BoxClass b, WidgetClass w) => w -> Int -> b -> IO ()
boxReorderChild w position b = 
  {#call box_reorder_child#} (toBox b) (toWidget w) (fromIntegral position)

-- Query the packing parameter of a child. Returns the behavious if
-- free space is available (@Packing), the additional padding for this
-- widget (@Int) and if the widget was inserted at the start or end of
-- the container (@PackType). (EXPORTED)
--
boxQueryChildPacking :: (BoxClass b, WidgetClass w) => 
  w -> b -> IO (Packing,Int,PackType)
boxQueryChildPacking w b = alloca $ \expandPtr -> alloca $ \fillPtr ->
  alloca $ \paddingPtr -> alloca $ \packPtr -> do
    {#call unsafe box_query_child_packing#} (toBox b) (toWidget w)
      expandPtr fillPtr paddingPtr packPtr
    expand  <- liftM toBool $ peek expandPtr
    fill    <- liftM toBool $ peek fillPtr
    padding <- liftM fromIntegral $ peek paddingPtr
    pack    <- liftM (toEnum.fromIntegral) $ peek packPtr
    return (if fill then PackFill else 
            (if expand then PackExpand else PackNatural),
	    padding,pack)

-- Set the packing parameter of a child. (EXPORTED)
--
boxSetChildPacking :: (BoxClass b, WidgetClass w) =>
  w -> Packing -> Int -> PackType -> b -> IO ()
boxSetChildPacking w pack pad pt b = {#call box_set_child_packing#} (toBox b) 
  (toWidget w) (fromBool $ pack/=PackNatural) (fromBool $ pack==PackFill) 
  (fromIntegral pad) ((fromIntegral.fromEnum) pt)


-- Retrieves the standard spacing between widgets. (EXPORTED)
--
boxGetSpacing :: BoxClass b => b -> IO Int
boxGetSpacing b = 
  liftM fromIntegral $ {#call unsafe box_get_spacing#} (toBox b)




