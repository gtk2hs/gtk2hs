{-# OPTIONS -cpp #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Container
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:08:47 $
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
-- |
--
-- This abstract widget implements the basis for turning serveral widgets
-- into one compound widget.
--

module Graphics.UI.Gtk.Abstract.Container (
  Container,
  ContainerClass,
  castToContainer,
  containerAdd,
  containerRemove,
  containerForeach,
  containerGetChildren,
  DirectionType(..),
  containerSetFocusChild,
  containerSetFocusChain,
  containerGetFocusChain,
  containerUnsetFocusChain,
  containerSetFocusVAdjustment,
  containerGetFocusVAdjustment,
  containerSetFocusHAdjustment,
  containerGetFocusHAdjustment,
  containerResizeChildren,
  containerSetBorderWidth,
  containerGetBorderWidth,
  containerChildSetProperty,
  containerChildGetProperty,
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

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject		(objectRef, objectUnref)
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import System.Glib.GList		(fromGList, toGList)
{#import System.Glib.GValue#}		(GValue, GenericValue, valueUnset)
import Graphics.UI.Gtk.General.Enums	(DirectionType(..))


{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Add a widget to the container.
--
-- * Only useful for simple
-- containers like Window. Use boxPackStart or tableAttach in other cases. A
-- widget may not be added to more than one container.
--
containerAdd :: (ContainerClass c, WidgetClass w) => c -> w -> IO ()
containerAdd con widget = 
  {#call container_add#} (toContainer con) (toWidget widget)


-- | Removes a present widget from the container.
--
containerRemove :: (ContainerClass c, WidgetClass w) => c -> w -> IO ()
containerRemove con widget =
  {#call container_remove#} (toContainer con) (toWidget widget)


-- | Do something for each widget in the container.
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

foreign import ccall "wrapper" mkContainerForeachFunc ::
  (Ptr Widget -> Ptr () -> IO ()) -> IO Callback

-- | Returns the the container's children.
--
containerGetChildren :: ContainerClass c => c -> IO [Widget]
containerGetChildren con = do
  glist <- {#call container_get_children#} (toContainer con)
  widgetPtrs <- fromGList glist
  mapM (makeNewObject mkWidget . return) widgetPtrs

-- | Give the focus to a specific child of the
-- container.
--
containerSetFocusChild :: (ContainerClass c, WidgetClass w) => c -> w -> IO ()
containerSetFocusChild con widget =
  {#call container_set_focus_child#} (toContainer con) (toWidget widget)

-- | Sets a focus chain, overriding the one computed automatically by GTK+.
--
containerSetFocusChain :: ContainerClass c => c -> [Widget] -> IO ()
containerSetFocusChain con chain =
  let wForeignPtrs = map (\w -> case toWidget w of Widget ptr -> ptr) chain in
  withForeignPtrs wForeignPtrs $ \wPtrs -> do
  glist <- toGList wPtrs
  {#call container_set_focus_chain#} (toContainer con) glist

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs = withForeignPtrs' []
  where withForeignPtrs' accum []     cont = cont (reverse accum)
        withForeignPtrs' accum (p:ps) cont = withForeignPtr p $ \p' ->
                                             withForeignPtrs' (p':accum) ps cont

-- | Retrieves the focus chain of the container, if one has been set explicitly.
--
containerGetFocusChain :: ContainerClass c => c -> IO (Maybe [Widget])
containerGetFocusChain con =
  alloca $ \glistPtr -> do
  {#call container_get_focus_chain#} (toContainer con) glistPtr
  if glistPtr == nullPtr then return Nothing else liftM Just $ do
    glist <- peek glistPtr
    widgetPtrs <- fromGList glist
    mapM (makeNewObject mkWidget . return) widgetPtrs

-- | Removes a focus chain explicitly set with 'containerSetFocusChain'.
--
containerUnsetFocusChain :: ContainerClass c => c -> IO ()
containerUnsetFocusChain con =
  {#call container_unset_focus_chain#} (toContainer con)

-- | Install an adjustment widget that is queried when focus is changed.
--
containerSetFocusVAdjustment :: (ContainerClass c, AdjustmentClass a) => c ->
                                a -> IO ()
containerSetFocusVAdjustment con adj =
  {#call container_set_focus_vadjustment#} (toContainer con) (toAdjustment adj)

-- | Retrieves the vertical focus adjustment for the container, or Nothing if
-- none has been set.
--
containerGetFocusVAdjustment :: ContainerClass c => c -> IO (Maybe Adjustment)
containerGetFocusVAdjustment con = do
  aPtr <- {#call unsafe container_get_focus_vadjustment#} (toContainer con)
  if aPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkAdjustment (return aPtr)

-- | Install an adjustment widget that is queried when focus is changed.
--
containerSetFocusHAdjustment :: (ContainerClass c, AdjustmentClass a) => c ->
                                a -> IO ()
containerSetFocusHAdjustment con adj =
  {#call container_set_focus_hadjustment#} (toContainer con) (toAdjustment adj)

-- | Retrieves the horizontal focus adjustment for the container, or Nothing if
-- none has been set.
--
containerGetFocusHAdjustment :: ContainerClass c => c -> IO (Maybe Adjustment)
containerGetFocusHAdjustment con = do
  aPtr <- {#call unsafe container_get_focus_hadjustment#} (toContainer con)
  if aPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkAdjustment (return aPtr)

-- | Make the container resize its children.
--
containerResizeChildren :: ContainerClass c => c -> IO ()
containerResizeChildren con =
  {#call container_resize_children#} (toContainer con)

-- | Set the amount of empty space around the outside of the container.
--
-- The border width of a container is the amount of space to leave around the
-- outside of the container. The border is added on all sides of the container.
--
containerSetBorderWidth :: ContainerClass c => c -> Int -> IO ()
containerSetBorderWidth con width =
  {#call container_set_border_width#} (toContainer con) (fromIntegral width)

-- | Retrieves the border width of the container. See 'containerSetBorderWidth'.
--
containerGetBorderWidth :: ContainerClass c => c -> IO Int
containerGetBorderWidth con = liftM fromIntegral $
  {#call unsafe container_get_border_width#} (toContainer con)

-- TODO add doc on what child properties are

-- | Sets a child property for child and container.
--
containerChildSetProperty :: (ContainerClass c, WidgetClass widget) => c
                          -> widget       -- ^ Chile widget
                          -> String       -- ^ Property name
                          -> GenericValue -- ^ Property value
                          -> IO ()
containerChildSetProperty con child prop val =
  alloca $ \valPtr ->
  withUTFString prop $ \strPtr -> do
  poke valPtr val
  {#call container_child_set_property#} (toContainer con)
    (toWidget child) strPtr valPtr

-- | Gets the value of a child property for the given child and container.
--
containerChildGetProperty :: (ContainerClass c, WidgetClass widget) => c
                          -> widget  -- ^ Child widget
                          -> String  -- ^ Property name
                          -> IO GenericValue
containerChildGetProperty con child prop =
  alloca $ \valPtr ->
  withUTFString prop $ \strPtr -> do
  {#call unsafe container_child_get_property#} (toContainer con)
    (toWidget child) strPtr valPtr
  res <- peek valPtr
  valueUnset valPtr
  return res

-- signals

-- | This signal is called each time a new widget is added
-- to this container.
--
onAdd, afterAdd :: ContainerClass con => con -> (Widget -> IO ()) ->
                   IO (ConnectId con)
onAdd = connect_OBJECT__NONE "add" False
afterAdd = connect_OBJECT__NONE "add" True

-- | This signal is called when the widget is
-- resized.
--
onCheckResize, afterCheckResize :: ContainerClass con => con -> (IO ()) ->
                                   IO (ConnectId con)
onCheckResize = connect_NONE__NONE "check-resize" False
afterCheckResize = connect_NONE__NONE "check-resize" True

-- | This signal is called if the container receives the
-- input focus.
--
onFocus, afterFocus :: ContainerClass con => con ->
                       (DirectionType -> IO DirectionType) ->
                       IO (ConnectId con)
onFocus = connect_ENUM__ENUM "focus" False
afterFocus = connect_ENUM__ENUM "focus" True

-- | This signal is called for each widget that is
-- removed from the container.
--
onRemove, afterRemove :: ContainerClass con => con -> (Widget -> IO ()) ->
                         IO (ConnectId con)
onRemove = connect_OBJECT__NONE "remove" False
afterRemove = connect_OBJECT__NONE "remove" True


-- | This signal is called if a child in the
-- container receives the input focus.
--
onSetFocusChild, afterSetFocusChild :: ContainerClass con => con ->
                                       (Widget -> IO ()) -> IO (ConnectId con)
onSetFocusChild = connect_OBJECT__NONE "set-focus-child" False
afterSetFocusChild = connect_OBJECT__NONE "set-focus-child" True





