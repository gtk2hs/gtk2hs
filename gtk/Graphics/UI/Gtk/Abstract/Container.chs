-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Container
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
-- This abstract widget implements the basis for turning serveral widgets
-- into one compound widget.
--
module Graphics.UI.Gtk.Abstract.Container (
-- * Description
-- 
-- | A Gtk+ user interface is constructed by nesting widgets inside widgets.
-- Container widgets are the inner nodes in the resulting tree of widgets: they
-- contain other widgets. So, for example, you might have a 'Window' containing
-- a 'Frame' containing a 'Label'. If you wanted an image instead of a textual
-- label inside the frame, you might replace the 'Label' widget with a 'Image'
-- widget.
--
-- There are two major kinds of container widgets in Gtk+. Both are
-- subclasses of the abstract 'Container' base class.
--
-- The first type of container widget has a single child widget and derives
-- from 'Bin'. These containers are decorators, which add some kind of
-- functionality to the child. For example, a 'Button' makes its child into a
-- clickable button; a 'Frame' draws a frame around its child and a 'Window'
-- places its child widget inside a top-level window.
--
-- The second type of container can have more than one child; its purpose is
-- to manage layout. This means that these containers assign sizes and
-- positions to their children. For example, a 'HBox' arranges its children in
-- a horizontal row, and a 'Table' arranges the widgets it contains in a
-- two-dimensional grid.
--
-- To fulfill its task, a layout container must negotiate the size
-- requirements with its parent and its children. This negotiation is carried
-- out in two phases, size requisition and size allocation.

-- ** Size Requisition
-- 
-- | The size requisition of a widget is it's desired width and height. This
-- is represented by a 'Requisition'.
--
-- How a widget determines its desired size depends on the widget. A
-- 'Label', for example, requests enough space to display all its text.
-- Container widgets generally base their size request on the requisitions of
-- their children.
--
-- The size requisition phase of the widget layout process operates
-- top-down. It starts at a top-level widget, typically a 'Window'. The
-- top-level widget asks its child for its size requisition by calling
-- 'widgetSizeRequest'. To determine its requisition, the child asks its own
-- children for their requisitions and so on. Finally, the top-level widget
-- will get a requisition back from its child.

-- ** Size Allocation
-- 
-- | When the top-level widget has determined how much space its child would
-- like to have, the second phase of the size negotiation, size allocation,
-- begins. Depending on its configuration (see 'windowSetResizable'), the
-- top-level widget may be able to expand in order to satisfy the size request
-- or it may have to ignore the size request and keep its fixed size. It then
-- tells its child widget how much space it gets by calling
-- 'widgetSizeAllocate'. The child widget divides the space among its children
-- and tells each child how much space it got, and so on. Under normal
-- circumstances, a 'Window' will always give its child the amount of space the
-- child requested.
--
-- A child's size allocation is represented by an 'Allocation'.
-- This contains not only a width and height, but also a
-- position (i.e. X and Y coordinates), so that containers can tell their
-- children not only how much space they have gotten, but also where they are
-- positioned inside the space available to the container.
--
-- Widgets are required to honor the size allocation they receive; a size
-- request is only a request, and widgets must be able to cope with any size.

-- ** Child properties
-- 
-- | 'Container' introduces child properties - these are object properties
-- that are not specific to either the container or the contained widget, but
-- rather to their relation. Typical examples of child properties are the
-- position or pack-type of a widget which is contained in a 'Box'.
--
-- To set the value of a child property, use 'containerChildSetProperty',
-- 'containerChildSet' or 'containerChildSetValist'. To obtain the value of a
-- child property, use 'containerChildGetProperty', 'containerChildGet' or
-- 'containerChildGetValist'.
-- 

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----Container
-- |                     +----'Bin'
-- |                     +----'Box'
-- |                     +----'CList'
-- |                     +----'Fixed'
-- |                     +----'Paned'
-- |                     +----'Layout'
-- |                     +----'List'
-- |                     +----'MenuShell'
-- |                     +----'Notebook'
-- |                     +----'Socket'
-- |                     +----'Table'
-- |                     +----'TextView'
-- |                     +----'Toolbar'
-- |                     +----'TreeView'
-- @

-- * Types
  Container,
  ContainerClass,
  castToContainer,

-- * Methods
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

-- * Signals
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

--------------------
-- Methods

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

--------------------
-- Signals

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





