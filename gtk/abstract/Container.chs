-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Container
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
-- * This abstract widget implements the basis for turning serveral widgets
--   into one compound widget.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------
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
  connectToAdd,
  connectToCheckResize,
  connectToFocus,
  connectToRemove,
  connectToSetFocusChild
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

-- Add a widget to the container. Only useful for simple containers like
-- Window. Use boxPackStart or tableAttach in other cases. A widget may
-- not be added to more than one container. (EXPORTED)
--
containerAdd :: (ContainerClass c, WidgetClass w) => w -> c -> IO ()
containerAdd widget con = 
  {#call container_add#} (toContainer con) (toWidget widget)


-- Removes a present widget from the container. (EXPORTED)
--
containerRemove :: (ContainerClass c, WidgetClass w) => w -> c -> IO ()
containerRemove widget con =
  {#call container_remove#} (toContainer con) (toWidget widget)


-- Do something for each widget in the container. (EXPORTED)
--
containerForeach :: ContainerClass c => ContainerForeachCB -> c -> IO ()
containerForeach fun con = do
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

-- Give the focus to the container. (EXPORTED)
--
-- * The @direction argument determines what kind of focus change is to be
--   simulated.
-- * The returned boolean value is the value returned fromm the @focus signal
--   emission.
--
--containerFocus :: ContainerClass c => DirectionType -> c -> IO Bool
--containerFocus direction con = liftM toBool $ {#call container_focus#} 
--  (toContainer con) ((fromIntegral.fromEnum) direction)

-- Give the focus to a specific child of the container. (EXPORTED)
--
containerSetFocusChild :: (ContainerClass c, WidgetClass w) => w -> c -> IO ()
containerSetFocusChild widget con =
  {#call container_set_focus_child#} (toContainer con) (toWidget widget)


-- Install an @adjustment widget that is queried when focus is changed.
-- (EXPORTED)
--
containerSetFocusVAdjustment :: (ContainerClass c, AdjustmentClass a) =>
  a -> c -> IO ()
containerSetFocusVAdjustment adj con =
  {#call container_set_focus_vadjustment#} (toContainer con) (toAdjustment adj)

-- Install an @adjustment widget that is queried when focus is changed.
-- (EXPORTED)
--
containerSetFocusHAdjustment :: (ContainerClass c, AdjustmentClass a) =>
  a -> c -> IO ()
containerSetFocusHAdjustment adj con =
  {#call container_set_focus_hadjustment#} (toContainer con) (toAdjustment adj)

-- Make the container resize its children. (EXPORTED)
--
containerResizeChildren :: ContainerClass c => c -> IO ()
containerResizeChildren con =
  {#call container_resize_children#} (toContainer con)


-- Query the composite name of a widget in this container. (EXPORTED)
--
--containerChildCompositeName :: (ContainerClass c, WidgetClass w) =>
--  w -> c -> IO String
--containerChildCompositeName widget con = do
--  strPtr <- throwIfNull "containerChildCompositeName: illegal name returned" $
--    {#call unsafe container_child_composite_name#} (toContainer con) 
--    (toWidget widget)
--  str <- peekCString strPtr
--  {#call unsafe g_free#} (castPtr strPtr)
--  return str

-- Set the amount of empty space around the outside of the container.
-- (EXPORTED)
containerSetBorderWidth :: ContainerClass c => Int -> c -> IO ()
containerSetBorderWidth width con =
  {#call container_set_border_width#} (toContainer con) (fromIntegral width)


-- signals

-- This signal is called each time a new widget is added to this container.
-- (EXPORTED)
connectToAdd :: ContainerClass con => 
  (Widget -> IO ()) -> ConnectAfter -> con -> IO (ConnectId con)
connectToAdd = connect_OBJECT__NONE "add"

-- This signal is called when the widget is resized. (EXPORTED)
--
connectToCheckResize :: ContainerClass con =>
  (IO ()) -> ConnectAfter -> con -> IO (ConnectId con)
connectToCheckResize = connect_NONE__NONE "check-resize"

-- This signal is called if the container receives the input focus. (EXPORTED)
--
connectToFocus :: ContainerClass con => (DirectionType -> IO DirectionType) ->
  ConnectAfter -> con -> IO (ConnectId con)
connectToFocus = connect_ENUM__ENUM "focus"

-- This signal is called for each widget that is removed from the container.
-- (EXPORTED)
connectToRemove :: ContainerClass con => 
  (Widget -> IO ()) -> ConnectAfter -> con -> IO (ConnectId con)
connectToRemove = connect_OBJECT__NONE "remove"


-- This signal is called if a child in the container receives the input focus.
-- (EXPORTED)
--
connectToSetFocusChild :: ContainerClass con =>
  (Widget -> IO ()) -> ConnectAfter -> con -> IO (ConnectId con)
connectToSetFocusChild = connect_OBJECT__NONE "set-focus-child"





