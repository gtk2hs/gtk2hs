-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Box@
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/24 09:43:24 $
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
-- * This abstract container class is instatiated by using HBox or VBox. It 
--   supplies all methods to add and remove children.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

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


-- @method boxPackStart@ Insert a widget at the beginning of the box
-- container.
--
-- * The @ref arg Packing@ parameter determines how the child behaves in the
--   horizontal or vertical way in an HBox or VBox, respectively.
--   @ref arg Natural@ means the child is as big as it reqests. All children
--   that have choosen @ref arg Expand@ for @ref arg p@ will be padded with
--   the remaining space. @ref arg Fill@ is the same as @ref arg Expand@
--   except that the child will receive the superfluous space.
--
boxPackStart :: (BoxClass b, WidgetClass w) => b -> w -> Packing -> Int ->
                IO ()
boxPackStart b w p pad = {#call box_pack_start#} (toBox b) (toWidget w)
  (fromBool $ p/=PackNatural) (fromBool $ p==PackFill) (fromIntegral pad)

-- @method boxPackEnd@ Insert a widget at the end of the box container.
--
-- * See @ref method boxPackStart@.
--
boxPackEnd :: (BoxClass b, WidgetClass w) => b -> w -> Packing -> Int -> IO ()
boxPackEnd b w p pad = {#call box_pack_end#} (toBox b) (toWidget w)
  (fromBool $ p/=PackNatural) (fromBool $ p==PackFill) (fromIntegral pad)


-- @method boxPackStartDefaults@ Like @ref method boxPackStart@ but uses the
-- default parameters @ref arg Fill@ and 0 for @ref arg Padding@.
--
boxPackStartDefaults :: (BoxClass b, WidgetClass w) => b -> w -> IO ()
boxPackStartDefaults b w = 
  {#call box_pack_start_defaults#} (toBox b) (toWidget w)

-- @method boxPackEndDefaults@ Like @ref method boxPackEnd@ but uses the
-- default parameters @ref arg Fill@ and 0 for @ref arg Padding@.
--
boxPackEndDefaults :: (BoxClass b, WidgetClass w) => b -> w -> IO ()
boxPackEndDefaults b w = 
  {#call box_pack_end_defaults#} (toBox b) (toWidget w)

-- @method boxSetHomogeneous@ Set if all children should be spread homogeneous
-- withing the box.
--
boxSetHomogeneous :: BoxClass b => b -> Bool -> IO ()
boxSetHomogeneous b homo = 
  {#call box_set_homogeneous#} (toBox b) (fromBool homo)

-- @method boxSetSpacing@ Set the standard spacing between two children.
--
-- * This space is in addition to the padding parameter that is given for each
--   child.
--
boxSetSpacing :: BoxClass b => b -> Int -> IO ()
boxSetSpacing b spacing =
  {#call box_set_spacing#} (toBox b) (fromIntegral spacing)

-- @method boxReorderChild@ Move @ref arg child@ to a new @ref arg position@
-- (counted from 0) in the box.
--
boxReorderChild :: (BoxClass b, WidgetClass w) => b -> w -> Int -> IO ()
boxReorderChild b w position = 
  {#call box_reorder_child#} (toBox b) (toWidget w) (fromIntegral position)

-- @method boxQueryChildPacking@ Query the packing parameter of a child.
-- Returns the behavious if free space is available (@Packing), the additional
-- padding for this widget (@Int) and if the widget was inserted at the start
-- or end of the container (@PackType).
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
    return (if fill then PackFill else 
            (if expand then PackExpand else PackNatural),
	    padding,pack)

-- @method boxSetChildPacking@ Set the packing parameter of a child.
--
boxSetChildPacking :: (BoxClass b, WidgetClass w) => b -> w -> Packing ->
                      Int -> PackType -> IO ()
boxSetChildPacking b w pack pad pt = {#call box_set_child_packing#} (toBox b) 
  (toWidget w) (fromBool $ pack/=PackNatural) (fromBool $ pack==PackFill) 
  (fromIntegral pad) ((fromIntegral.fromEnum) pt)


-- @method boxGetSpacing@ Retrieves the standard spacing between widgets.
--
boxGetSpacing :: BoxClass b => b -> IO Int
boxGetSpacing b = 
  liftM fromIntegral $ {#call unsafe box_get_spacing#} (toBox b)




