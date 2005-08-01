-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Container
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Version $Revision: 1.13 $ from $Date: 2005/08/01 21:33:07 $
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
-- Base class for widgets which contain other widgets
--
module Graphics.UI.Gtk.Abstract.Container (
-- * Detail
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

-- ** Child attributes
-- 
-- | 'Container' introduces child attributes - these are object attributes
-- that are not specific to either the container or the contained widget, but
-- rather to their relation. Typical examples of child attributes are the
-- position or pack-type of a widget which is contained in a 'Box'.
--
-- The 'Container' class does not itself define any child attributes, they are
-- defined (and documented) by the various 'Container' subclasses.
--
-- Child attributes can be set or obtained in a similar way to ordinary
-- attributes. So ordinary attributes are set like so:
--
-- > set object [ attr := value ]
--
-- Whereas child attributes take the child object as a parameter:
--
-- > set container [ attr child := value ]
-- 
-- And similarily for getting a child attribute's value:
--
-- > value <- get container (attr child)
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
-- |                     +----'IconView'
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
  ContainerForeachCB,
  ResizeMode(..),

-- * Methods
  containerAdd,
  containerRemove,
  containerForeach,
  containerForall,
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
  containerGetResizeMode,
  containerSetResizeMode,

-- * Attributes
  containerResizeMode,
  containerBorderWidth,
  containerChild,
  containerFocusHAdjustment,
  containerFocusVAdjustment,

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
  afterSetFocusChild,
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import System.Glib.GList		(fromGList, toGList)
import Graphics.UI.Gtk.General.Enums	(DirectionType(..), ResizeMode(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Methods

-- | Adds @widget@ to the container. Typically used for simple containers such
-- as 'Window', 'Frame', or 'Button'; for more complicated layout containers
-- such as 'Box' or 'Table', this function will pick default packing parameters
-- that may not be correct. So consider functions such as 'boxPackStart' and
-- 'tableAttach' as an alternative to 'containerAdd' in those cases. A widget
-- may be added to only one container at a time; you can't place the same
-- widget inside two different containers.
--
containerAdd :: (ContainerClass self, WidgetClass widget) => self
 -> widget -- ^ @widget@ - a widget to be placed inside @container@
 -> IO ()
containerAdd self widget =
  {# call container_add #}
    (toContainer self)
    (toWidget widget)

-- | Removes @widget@ from @container@. @widget@ must be inside @container@.
--
containerRemove :: (ContainerClass self, WidgetClass widget) => self
 -> widget -- ^ @widget@ - a current child of @container@
 -> IO ()
containerRemove self widget =
  {# call container_remove #}
    (toContainer self)
    (toWidget widget)

-- | Maps @callback@ over each non-internal child of @container@. See
-- 'containerForall' for details on what constitutes an \"internal\" child.
-- Most applications should use 'containerForeach', rather than
-- 'containerForall'.
--
containerForeach :: ContainerClass self => self
 -> ContainerForeachCB
 -> IO ()
containerForeach self fun = do
  fPtr <- mkContainerForeachFunc (\wPtr _ -> do
    w <- makeNewObject mkWidget (return wPtr)
    fun w)
  {# call container_foreach #}
    (toContainer self)
    fPtr
    nullPtr
  freeHaskellFunPtr fPtr

-- | A function that is invoked for all widgets in a container.
type ContainerForeachCB = Widget -> IO ()
{#pointer Callback#}

foreign import ccall "wrapper" mkContainerForeachFunc ::
  (Ptr Widget -> Ptr () -> IO ()) -> IO Callback

-- | Maps @callback@ over each child of @container@, including children that
-- are considered \"internal\" (implementation details of the container).
-- \"Internal\" children generally weren't added by the user of the container,
-- but were added by the container implementation itself. Most applications
-- should use 'containerForeach', rather than 'containerForall'.
--
containerForall :: ContainerClass self => self
 -> ContainerForeachCB -- ^ @callback@ - a callback
 -> IO ()
containerForall self fun = do
  fPtr <- mkContainerForeachFunc (\wPtr _ -> do
    w <- makeNewObject mkWidget (return wPtr)
    fun w)
  {# call container_forall #}
    (toContainer self)
    fPtr
    nullPtr
  freeHaskellFunPtr fPtr

-- | Returns the container's non-internal children. See 'containerForall' for
-- details on what constitutes an \"internal\" child.
--
containerGetChildren :: ContainerClass self => self
 -> IO [Widget]
containerGetChildren self = do
  glist <- {# call container_get_children #} (toContainer self)
  widgetPtrs <- fromGList glist
  mapM (makeNewObject mkWidget . return) widgetPtrs

-- | Give the focus to a specific child of the container.
--
containerSetFocusChild :: (ContainerClass self, WidgetClass child) => self
 -> child -- ^ @child@
 -> IO ()
containerSetFocusChild self child =
  {# call container_set_focus_child #}
    (toContainer self)
    (toWidget child)

-- | Sets a focus chain, overriding the one computed automatically by Gtk+.
--
-- In principle each widget in the chain should be a descendant of the
-- container, but this is not enforced by this method, since it's allowed to
-- set the focus chain before you pack the widgets, or have a widget in the
-- chain that isn't always packed. The necessary checks are done when the focus
-- chain is actually traversed.
--
containerSetFocusChain :: ContainerClass self => self
 -> [Widget] -- ^ @focusableWidgets@ - the new focus chain.
 -> IO ()
containerSetFocusChain self chain =
  withForeignPtrs (map unWidget chain) $ \wPtrs -> do
  glist <- toGList wPtrs
  {# call container_set_focus_chain #}
    (toContainer self)
    glist

-- | Retrieves the focus chain of the container, if one has been set
-- explicitly. If no focus chain has been explicitly set, Gtk+ computes the
-- focus chain based on the positions of the children. In that case the
-- function returns @Nothing@.
--
containerGetFocusChain :: ContainerClass self => self
 -> IO (Maybe [Widget])
containerGetFocusChain self =
  alloca $ \glistPtr -> do
  {# call container_get_focus_chain #}
    (toContainer self)
    glistPtr
  if glistPtr == nullPtr then return Nothing else liftM Just $ do
    glist <- peek glistPtr
    widgetPtrs <- fromGList glist
    mapM (makeNewObject mkWidget . return) widgetPtrs

-- | Removes a focus chain explicitly set with 'containerSetFocusChain'.
--
containerUnsetFocusChain :: ContainerClass self => self -> IO ()
containerUnsetFocusChain self =
  {# call container_unset_focus_chain #}
    (toContainer self)

-- | Hooks up an adjustment to focus handling in a container, so when a child
-- of the container is focused, the adjustment is scrolled to show that widget.
-- This function sets the vertical alignment. See
-- 'scrolledWindowGetVAdjustment' for a typical way of obtaining the adjustment
-- and 'containerSetFocusHAdjustment' for setting the horizontal adjustment.
--
-- The adjustments have to be in pixel units and in the same coordinate
-- system as the allocation for immediate children of the container.
--
containerSetFocusVAdjustment :: ContainerClass self => self
 -> Adjustment -- ^ @adjustment@ - an adjustment which should be adjusted when
               -- the focus is moved among the descendents of @container@
 -> IO ()
containerSetFocusVAdjustment self adjustment =
  {# call container_set_focus_vadjustment #}
    (toContainer self)
    adjustment

-- | Retrieves the vertical focus adjustment for the container. See
-- 'containerSetFocusVAdjustment'.
--
containerGetFocusVAdjustment :: ContainerClass self => self
 -> IO (Maybe Adjustment) -- ^ returns the vertical focus adjustment, or
                          -- @Nothing@ if none has been set.
containerGetFocusVAdjustment self =
  maybeNull (makeNewObject mkAdjustment) $
  {# call unsafe container_get_focus_vadjustment #}
    (toContainer self)

-- | Hooks up an adjustment to focus handling in a container, so when a child
-- of the container is focused, the adjustment is scrolled to show that widget.
-- This function sets the horizontal alignment. See
-- 'scrolledWindowGetHAdjustment' for a typical way of obtaining the adjustment
-- and 'containerSetFocusVAdjustment' for setting the vertical adjustment.
--
-- The adjustments have to be in pixel units and in the same coordinate
-- system as the allocation for immediate children of the container.
--
containerSetFocusHAdjustment :: ContainerClass self => self
 -> Adjustment -- ^ @adjustment@ - an adjustment which should be adjusted when
               -- the focus is moved among the descendents of @container@
 -> IO ()
containerSetFocusHAdjustment self adjustment =
  {# call container_set_focus_hadjustment #}
    (toContainer self)
    adjustment

-- | Retrieves the horizontal focus adjustment for the container. See
-- 'containerSetFocusHAdjustment'.
--
containerGetFocusHAdjustment :: ContainerClass self => self
 -> IO (Maybe Adjustment) -- ^ returns the horizontal focus adjustment, or
                          -- @Nothing@ if none has been set.
containerGetFocusHAdjustment self =
  maybeNull (makeNewObject mkAdjustment) $
  {# call unsafe container_get_focus_hadjustment #}
    (toContainer self)

-- | Make the container resize its children.
--
containerResizeChildren :: ContainerClass self => self -> IO ()
containerResizeChildren self =
  {# call container_resize_children #}
    (toContainer self)

-- | Sets the border width of the container.
--
-- The border width of a container is the amount of space to leave around
-- the outside of the container. The only exception to this is 'Window';
-- because toplevel windows can't leave space outside, they leave the space
-- inside. The border is added on all sides of the container. To add space to
-- only one side, one approach is to create a 'Alignment' widget, call
-- 'widgetSetUsize' to give it a size, and place it on the side of the
-- container as a spacer.
--
containerSetBorderWidth :: ContainerClass self => self
 -> Int   -- ^ @borderWidth@ - amount of blank space to leave /outside/ the
          -- container. Valid values are in the range 0-65535 pixels.
 -> IO ()
containerSetBorderWidth self borderWidth =
  {# call container_set_border_width #}
    (toContainer self)
    (fromIntegral borderWidth)

-- | Retrieves the border width of the container. See
-- 'containerSetBorderWidth'.
--
containerGetBorderWidth :: ContainerClass self => self
 -> IO Int -- ^ returns the current border width
containerGetBorderWidth self =
  liftM fromIntegral $
  {# call unsafe container_get_border_width #}
    (toContainer self)

-- | Returns the resize mode for the container. See 'containerSetResizeMode'.
--
containerGetResizeMode :: ContainerClass self => self
 -> IO ResizeMode -- ^ returns the current resize mode
containerGetResizeMode self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_container_get_resize_mode #}
    (toContainer self)

-- | Sets the resize mode for the container.
--
-- The resize mode of a container determines whether a resize request will
-- be passed to the container's parent, queued for later execution or executed
-- immediately.
--
containerSetResizeMode :: ContainerClass self => self
 -> ResizeMode -- ^ @resizeMode@ - the new resize mode.
 -> IO ()
containerSetResizeMode self resizeMode =
  {# call gtk_container_set_resize_mode #}
    (toContainer self)
    ((fromIntegral . fromEnum) resizeMode)

--------------------
-- Attributes

-- | Specify how resize events are handled.
--
-- Default value: 'ResizeParent'
--
containerResizeMode :: ContainerClass self => Attr self ResizeMode
containerResizeMode = newAttr
  containerGetResizeMode
  containerSetResizeMode

-- | The width of the empty border outside the containers children.
--
-- Allowed values: \<= @('maxBound' :: Int)@
--
-- Default value: 0
--
containerBorderWidth :: ContainerClass self => Attr self Int
containerBorderWidth = newAttr
  containerGetBorderWidth
  containerSetBorderWidth

-- | Can be used to add a new child to the container.
--
containerChild :: (ContainerClass self, WidgetClass widget) => WriteAttr self widget
containerChild = writeAttrFromObjectProperty "child"

-- | \'focusHadjustment\' property. See 'containerGetFocusHAdjustment' and
-- 'containerSetFocusHAdjustment'
--
containerFocusHAdjustment :: ContainerClass self => ReadWriteAttr self (Maybe Adjustment) Adjustment
containerFocusHAdjustment = newAttr
  containerGetFocusHAdjustment
  containerSetFocusHAdjustment

-- | \'focusVadjustment\' property. See 'containerGetFocusVAdjustment' and
-- 'containerSetFocusVAdjustment'
--
containerFocusVAdjustment :: ContainerClass self => ReadWriteAttr self (Maybe Adjustment) Adjustment
containerFocusVAdjustment = newAttr
  containerGetFocusVAdjustment
  containerSetFocusVAdjustment

--------------------
-- Signals

-- | This signal is called each time a new widget is added to this container.
--
onAdd, afterAdd :: ContainerClass self => self
 -> (Widget -> IO ())
 -> IO (ConnectId self)
onAdd = connect_OBJECT__NONE "add" False
afterAdd = connect_OBJECT__NONE "add" True

-- | This signal is called when the widget is resized.
--
onCheckResize, afterCheckResize :: ContainerClass self => self
 -> IO ()
 -> IO (ConnectId self)
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

-- | This signal is called for each widget that is removed from the container.
--
onRemove, afterRemove :: ContainerClass self => self
 -> (Widget -> IO ())
 -> IO (ConnectId self)
onRemove = connect_OBJECT__NONE "remove" False
afterRemove = connect_OBJECT__NONE "remove" True

-- | This signal is called if a child in the container receives the input
-- focus.
--
onSetFocusChild, afterSetFocusChild :: ContainerClass self => self
 -> (Widget -> IO ())
 -> IO (ConnectId self)
onSetFocusChild = connect_OBJECT__NONE "set-focus-child" False
afterSetFocusChild = connect_OBJECT__NONE "set-focus-child" True
