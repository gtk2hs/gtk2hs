-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget Container@
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.4 $ from $Date: 2002/10/06 16:14:07 $
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
-- * This abstract widget implements the basis for turning serveral widgets
--   into one compound widget.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
--
-- * Check if the following functions are of interest to the user:
--   containerSetReallocateRedraws, containerQueueResize, 
--   conatinerClearResizeWidgets
--
module Container(
  Container,
  ContainerClass,
  castToContainer,
  containerAdd,
  containerRemove,
  containerForeach,
  DirectionType(..),
--  containerFocus,
  containerSetFocusChild,
  containerSetFocusVAdjustment,
  containerSetFocusHAdjustment,
  containerResizeChildren,
--  containerChildCompositeName,
  containerSetBorderWidth,
  onAdd,
  afterAdd,
  onCheckResize,
  afterCheckResize,
  onFocus,
  afterFocus,
  onRemove,
  afterRemove,
  onSetFocusChild,
  afterSetFocusChild
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import GObject	(objectRef, objectUnref)
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(DirectionType(..))


{# context lib="gtk" prefix="gtk" #}

-- methods

-- @method containerAdd@ Add a widget to the container.
--
-- * Only useful for simple
-- containers like Window. Use boxPackStart or tableAttach in other cases. A
-- widget may not be added to more than one container.
--
containerAdd :: (ContainerClass c, WidgetClass w) => c -> w -> IO ()
containerAdd con widget = 
  {#call container_add#} (toContainer con) (toWidget widget)


-- @method containerRemove@ Removes a present widget from the container.
--
containerRemove :: (ContainerClass c, WidgetClass w) => c -> w -> IO ()
containerRemove con widget =
  {#call container_remove#} (toContainer con) (toWidget widget)


-- @method containerForeach@ Do something for each widget in the container.
--
containerForeach :: ContainerClass c => c -> ContainerForeachCB -> IO ()
containerForeach con fun = do
  fPtr <- mkContainerForeachFunc (\wPtr _ -> do
    objectRef wPtr
    w <- liftM mkWidget $ newForeignPtr wPtr (objectUnref wPtr)
    fun w)
  {#call container_foreach#} (toContainer con) fPtr nullPtr
  freeHaskellFunPtr fPtr

type ContainerForeachCB = Widget -> IO ()
{#pointer Callback#}

foreign export dynamic mkContainerForeachFunc ::
  (Ptr Widget -> Ptr () -> IO ()) -> IO Callback

-- @method containerFocus@ Give the focus to the container.
-- * The @ref arg direction@ argument determines what kind of focus 
-- change is to be
--   simulated.
--
-- * The returned boolean value is the value returned from the 
-- @ref signal focus@ signal emission.
--
--containerFocus :: ContainerClass c => DirectionType -> c -> IO Bool
--containerFocus direction con = liftM toBool $ {#call container_focus#} 
--  (toContainer con) ((fromIntegral.fromEnum) direction)


-- @method containerSetFocusChild@ Give the focus to a specific child of the
-- container.
--
containerSetFocusChild :: (ContainerClass c, WidgetClass w) => c -> w -> IO ()
containerSetFocusChild con widget =
  {#call container_set_focus_child#} (toContainer con) (toWidget widget)


-- @method containerSetFocusVAdjustment@ Install an @ref arg adjustment@
-- widget that is queried when focus is changed.
--
containerSetFocusVAdjustment :: (ContainerClass c, AdjustmentClass a) => c ->
                                a -> IO ()
containerSetFocusVAdjustment con adj =
  {#call container_set_focus_vadjustment#} (toContainer con) (toAdjustment adj)

-- @method containerSetFocusHAdjustment@ Install an @ref arg adjustment@
-- widget that is queried when focus is changed.
--
containerSetFocusHAdjustment :: (ContainerClass c, AdjustmentClass a) => c ->
                                a -> IO ()
containerSetFocusHAdjustment con adj =
  {#call container_set_focus_hadjustment#} (toContainer con) (toAdjustment adj)

-- @method containerResizeChildren@ Make the container resize its children.
--
containerResizeChildren :: ContainerClass c => c -> IO ()
containerResizeChildren con =
  {#call container_resize_children#} (toContainer con)


-- @method containerChildCompositeName@ Query the composite name of a 
-- widget in this container.
-- *  
--containerChildCompositeName :: (ContainerClass c, WidgetClass w) =>
--  w -> c -> IO String
--containerChildCompositeName widget con = do
--  strPtr <- throwIfNull "containerChildCompositeName: illegal name returned" $
--    {#call unsafe container_child_composite_name#} (toContainer con) 
--    (toWidget widget)
--  str <- peekCString strPtr
--  {#call unsafe g_free#} (castPtr strPtr)
--  return str


-- @method containerSetBorderWidth@ Set the amount of empty space around the
-- outside of the container.
--
containerSetBorderWidth :: ContainerClass c => c -> Int -> IO ()
containerSetBorderWidth con width =
  {#call container_set_border_width#} (toContainer con) (fromIntegral width)


-- signals

-- @signal connectToAdd@ This signal is called each time a new widget is added
-- to this container.
--
onAdd, afterAdd :: ContainerClass con => con -> (Widget -> IO ()) ->
                   IO (ConnectId con)
onAdd = connect_OBJECT__NONE "add" False
afterAdd = connect_OBJECT__NONE "add" True

-- @signal connectToCheckResize@ This signal is called when the widget is
-- resized.
--
onCheckResize, afterCheckResize :: ContainerClass con => con -> (IO ()) ->
                                   IO (ConnectId con)
onCheckResize = connect_NONE__NONE "check-resize" False
afterCheckResize = connect_NONE__NONE "check-resize" True

-- @signal connectToFocus@ This signal is called if the container receives the
-- input focus.
--
onFocus, afterFocus :: ContainerClass con => con ->
                       (DirectionType -> IO DirectionType) ->
                       IO (ConnectId con)
onFocus = connect_ENUM__ENUM "focus" False
afterFocus = connect_ENUM__ENUM "focus" True

-- @signal connectToRemove@ This signal is called for each widget that is
-- removed from the container.
--
onRemove, afterRemove :: ContainerClass con => con -> (Widget -> IO ()) ->
                         IO (ConnectId con)
onRemove = connect_OBJECT__NONE "remove" False
afterRemove = connect_OBJECT__NONE "remove" True


-- @signal connectToSetFocusChild@ This signal is called if a child in the
-- container receives the input focus.
--
onSetFocusChild, afterSetFocusChild :: ContainerClass con => con ->
                                       (Widget -> IO ()) -> IO (ConnectId con)
onSetFocusChild = connect_OBJECT__NONE "set-focus-child" False
afterSetFocusChild = connect_OBJECT__NONE "set-focus-child" True





