-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget class
--
--  Author : Axel Simon
--          
--  Created: 27 April 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:19 $
--
--  Copyright (c) 2001 Axel Simon
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
--  * Widget is the base class of all widgets. It provides the methods to
--    attach and detach signals.
--
--- DOCU ----------------------------------------------------------------------
--
--  * This modules reexports everything a normal widget needs from GObject
--    and Object.
--
--- TODO ----------------------------------------------------------------------
--
--  * unimplemented methods that seem to be useful in user programs:
--      widgetSizeRequest, widgetAddAccelerator, widgetRemoveAccrelerator,
--	widgetAcceleratorSignal, widgetIntersect, widgetGrabDefault,
--	widgetGetPointer, widgetPath, widgetClassPath, getCompositeName,
--	widgetSetCompositeName,
--	widgetModifyStyle, widgetGetModifierStyle, widgetModifyFg,
--	widgetModifyBG, widgetModifyText, widgetModifyBase, widgetModifyFont,
--	widgetPango*, widgetSetAdjustments
--	
--
--  * implement the following methods in GtkWindow object:
--      widget_set_uposition, widget_set_usize
--
--  * implement the following methods in GtkDrawingArea object:
--      widgetQueueDrawArea, widgetSetDoubleBufferd, widgetRegionIntersect
--
module Widget(
  Widget,
  WidgetClass,
  castToWidget,
  Allocation,
  Requisition(..),
  Rectangle(..),
  widgetShow,			-- Showing and hiding a widget.
  widgetShowNow,
  widgetHide,
  widgetShowAll,
  widgetHideAll,
  widgetDestroy,
  widgetQueueDraw,		-- Functions to be used with DrawingArea.
  widgetHasIntersection,
  widgetActivate,		-- Manipulate widget state.
  widgetSetSensitivity,
  widgetIsFocus,
  widgetGrabFocus,
  widgetSetAppPaintable,
  widgetSetName,		-- Naming, Themes
  widgetGetName,
  widgetGetToplevel,		-- Widget browsing.
  widgetIsAncestor,
  widgetReparent,
  TextDirection(..),
  widgetSetDirection,		-- General Setup.
  widgetGetDirection,
--  widgetLockAccelerators,
--  widgetUnlockAccelerators,
  Event(..),
  connectToButtonPress,
  connectToButtonRelease,
  connectToClient,
  connectToConfigure,
  connectToDelete,
  connectToDestroy,
  connectToDirectionChanged,
  connectToEnterNotify,
  connectToLeaveNotify,
  connectToExpose,
  connectToFocusIn,
  connectToFocusOut,
  connectToGrabFocus,
  connectToHide,
  connectToHierarchyChanged,
  connectToKeyPress,
  connectToKeyRelease,
  connectToMnemonicActivate,
  connectToMotionNotify,
  connectToParentSet,
  connectToPopupMenu,
  connectToProximityIn,
  connectToProximityOut,
  connectToScroll,
  connectToShow,
  connectToSizeAllocate,
  connectToSizeRequest,
  StateType(..),
  connectToStateChanged,
  connectToUnmap,
  connectToUnrealize,
  connectToVisibilityNotify,
  connectToWindowState
  ) where

import Monad	(liftM, unless)
import UTFCForeign
import Foreign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import GdkEnums
import Structs	(Allocation, Rectangle(..), Requisition(..))
import Events	(Event(..), marshalEvent)
import Enums	(StateType(..), TextDirection(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Showing and hiding a widget. (EXPORTED)
--
widgetShow, widgetShowNow, widgetHide, widgetShowAll, widgetHideAll :: 
  WidgetClass w => w -> IO ()
widgetShow    = {#call widget_show#}.toWidget
widgetShowNow = {#call widget_show_now#}.toWidget
widgetHide    = {#call widget_hide#}.toWidget
widgetShowAll = {#call widget_show_all#}.toWidget
widgetHideAll = {#call widget_hide_all#}.toWidget

-- Destroy a widget. (EXPORTED)
--
-- * The @widgetDestroy function is used to shutdown an object, i.e. a widget
--   will be removed from the screen and unrealized. Resources will be freed
--   when all references are released. 
-- 
widgetDestroy :: WidgetClass obj => obj -> IO ()
widgetDestroy = {#call widget_destroy#}.toWidget

-- Functions to be used with DrawingArea.

-- Send a redraw request to a widget. (EXPORTED)
widgetQueueDraw :: WidgetClass w => w -> IO ()
widgetQueueDraw = {#call widget_queue_draw#}.toWidget

-- Check if the widget intersects with a given area. (EXPORTED)
widgetHasIntersection :: WidgetClass w => Rectangle -> w -> IO Bool
widgetHasIntersection r w = 
  liftM toBool $
  withObject r $ \r' ->
  {#call unsafe widget_intersect#} (toWidget w) (castPtr r') (castPtr nullPtr)

-- Manipulate widget state.

-- Activate the widget (e.g. clicking a button). (EXPORTED)
widgetActivate :: WidgetClass w => w -> IO Bool
widgetActivate w = liftM toBool $ {#call widget_activate#} (toWidget w)

-- Set the widgets sensitivity (Grayed or Usable). (EXPORTED)
widgetSetSensitivity :: WidgetClass w => Bool -> w -> IO ()
widgetSetSensitivity b w = 
  {#call widget_set_sensitive#} (toWidget w) (fromBool b)

-- Set and query the input focus of a widget. (EXPORTED)
widgetIsFocus :: WidgetClass w => w -> IO Bool
widgetIsFocus w = liftM toBool $ 
  {#call unsafe widget_is_focus#} (toWidget w)

widgetGrabFocus :: WidgetClass w => w -> IO ()
widgetGrabFocus = {#call widget_grab_focus#}.toWidget

-- Sets some weired flag in the widget. (EXPORTED)
widgetSetAppPaintable :: WidgetClass w => Bool -> w -> IO ()
widgetSetAppPaintable p w = 
  {#call widget_set_app_paintable#} (toWidget w) (fromBool p)

-- Set the name of a widget. (EXPORTED)
widgetSetName :: WidgetClass w => String -> w -> IO ()
widgetSetName name w = 
  withCString name ({#call widget_set_name#} (toWidget w))

-- Get the name of a widget. (EXPORTED)
widgetGetName :: WidgetClass w => w -> IO String
widgetGetName w = {#call unsafe widget_get_name#} (toWidget w) >>= 
		  peekCString

-- Enable event signals. (EXPORTED)
widgetAddEvents :: WidgetClass w => [EventMask] -> w -> IO ()
widgetAddEvents em w = 
  {#call widget_add_events#} (toWidget w) (fromIntegral $ fromFlags em)

-- Get enabled event signals. (EXPORTED)
widgetGetEvents :: WidgetClass w => w -> IO [EventMask]
widgetGetEvents w = liftM (toFlags.fromIntegral) $ 
  {#call unsafe widget_get_events#} (toWidget w)

-- Set extension events. (EXPORTED)
widgetSetExtensionEvents :: WidgetClass w => [ExtensionMode] -> w -> IO ()
widgetSetExtensionEvents em w = 
  {#call widget_set_extension_events#} (toWidget w) 
    (fromIntegral $ fromFlags em)

-- Get extension events. (EXPORTED)
widgetGetExtensionEvents :: WidgetClass w => w -> IO [ExtensionMode]
widgetGetExtensionEvents w = liftM (toFlags.fromIntegral) $ 
  {#call widget_get_extension_events#} (toWidget w)

-- Widget browsing.

-- Retrieves the topmost widget in this tree. (EXPORTED)
widgetGetToplevel :: WidgetClass w => w -> IO Widget
widgetGetToplevel w = makeNewObject mkWidget $
  {#call unsafe widget_get_toplevel#} (toWidget w)

-- Return True if the second widget is (possibly indirectly) held by the
-- first. (EXPORTED)
widgetIsAncestor :: (WidgetClass w, WidgetClass anc) => w -> anc -> IO Bool
widgetIsAncestor w anc = liftM toBool $  
 {#call unsafe widget_is_ancestor#} (toWidget w) (toWidget anc)

-- Move a widget to a new parent. (EXPORTED)
--
widgetReparent :: (WidgetClass w, WidgetClass par) => par -> w -> IO ()
widgetReparent par w = 
  {#call widget_reparent#} (toWidget w) (toWidget par)

-- Setting packaging and writing direction. (EXPORTED)
--
widgetSetDirection :: WidgetClass w => TextDirection -> w -> IO ()
widgetSetDirection td w = 
  {#call widget_set_direction#} (toWidget w) ((fromIntegral.fromEnum) td)

-- Retrieve the default direction of text writing. (EXPORTED)
--
widgetGetDirection :: WidgetClass w => w -> IO TextDirection
widgetGetDirection w = liftM (toEnum.fromIntegral) $ 
  {#call widget_get_direction#} (toWidget w)

-- Accelerator handling.

-- Lock accelerators. (EXPORTED)
--
--widgetLockAccelerators :: WidgetClass w => w -> IO ()
--widgetLockAccelerators = {#call unsafe widget_lock_accelerators#}.toWidget

-- Unlock accelerators. (EXPORTED)
--
--widgetUnlockAccelerators :: WidgetClass w => w -> IO ()
--widgetUnlockAccelerators = {#call widget_unlock_accelerators#}.toWidget



-- signals

-- Because there are so many similar signals (those that take an Event and
-- return a Bool) we will abstract out the skeleton. As some of these events
-- are emitted at a high rate often a bit has to be set to enable emission.
event :: WidgetClass w => SignalName -> [EventMask] ->
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
event name eMask fun after obj = do
  id <- connect_BOXED__BOOL name marshalEvent fun after obj
  widgetAddEvents eMask obj
  return id

connectToButtonPress :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToButtonPress = event "button_press_event" [ButtonPressMask]

connectToButtonRelease :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToButtonRelease = event "button_release_event" [ButtonReleaseMask]

connectToClient :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToClient = event "client_event" []

connectToConfigure :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToConfigure = event "configure_event" [] 

-- This signal is emitted when the close icon on the surrounding window is
-- pressed. The default action is to emit the @destroy signal. (EXPORTED)
--
connectToDelete :: WidgetClass w =>
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToDelete = event "delete_event" []

connectToDestroy :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToDestroy = event "destroy_event" []

connectToDirectionChanged :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToDirectionChanged = event "direction_changed" []

connectToEnterNotify :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToEnterNotify = event "enter_notify_event" [EnterNotifyMask]

connectToLeaveNotify :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToLeaveNotify = event "leave_notify_event" [LeaveNotifyMask]

connectToExpose :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToExpose = event "expose_event" []

connectToFocusIn  :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToFocusIn = event "focus_in_event" [FocusChangeMask]

connectToFocusOut :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToFocusOut = event "focus_out_event" [FocusChangeMask]

connectToGrabFocus :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToGrabFocus = event "grab_focus" []

connectToHide :: WidgetClass w => 
  IO () -> ConnectAfter -> w -> IO (ConnectId w)
connectToHide = connect_NONE__NONE "hide"

connectToHierarchyChanged :: WidgetClass w => 
  IO () -> ConnectAfter -> w -> IO (ConnectId w)
connectToHierarchyChanged = connect_NONE__NONE "hierarchy_changed"

connectToKeyPress  :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToKeyPress = event "key_press_event" [KeyPressMask]

connectToKeyRelease :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToKeyRelease = event "key_release_event" [KeyReleaseMask]

connectToMnemonicActivate :: WidgetClass w =>
  (Bool -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToMnemonicActivate = connect_BOOL__BOOL "mnemonic_activate"

connectToMotionNotify  :: WidgetClass w => 
  Bool -> (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToMotionNotify hint = event "motion_notify_event" 
  (if hint then [PointerMotionHintMask] else [PointerMotionMask])

connectToParentSet :: (WidgetClass w, WidgetClass old) =>
  (old -> IO ()) -> ConnectAfter -> w -> IO (ConnectId w)
connectToParentSet = connect_OBJECT__NONE "parent_set" 

connectToPopupMenu :: WidgetClass w => 
  IO () -> ConnectAfter -> w -> IO (ConnectId w)
connectToPopupMenu = connect_NONE__NONE "popup_menu"

connectToProximityIn :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToProximityIn = event "proximity_in_event" [ProximityInMask]

connectToProximityOut  :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToProximityOut = event "proximity_out_event" [ProximityOutMask]

connectToScroll  :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToScroll = event "scroll_event" [ScrollMask]

connectToShow :: WidgetClass w =>
  IO () -> ConnectAfter -> w -> IO (ConnectId w)
connectToShow = connect_NONE__NONE "show"

-- After querying a widget for the size it wants to have (through emitting 
-- the @sizeRequest@ signal) a container will emit this signal to inform 
-- the widget about the real size it should occupy. (EXPORTED)
--
connectToSizeAllocate :: WidgetClass w =>
  (Allocation -> IO ()) -> ConnectAfter -> w -> IO (ConnectId w)
connectToSizeAllocate fun = connect_PTR__NONE "size_allocate" (\allPtr -> do
  all <- peek allPtr
  fun all)

-- A parent container emits this signal to its child to query the needed
-- height and width of the child. (EXPORTED)
--
connectToSizeRequest :: WidgetClass w =>
  (IO Requisition) -> ConnectAfter -> w -> IO (ConnectId w)
connectToSizeRequest fun = connect_PTR__NONE "size_request" (\rqPtr -> do
  req <- fun
  unless (rqPtr==nullPtr) $ poke rqPtr req)

connectToStateChanged :: WidgetClass w =>
  (StateType -> IO ()) -> ConnectAfter -> w -> IO (ConnectId w)
connectToStateChanged = connect_ENUM__NONE "state_changed"

connectToUnmap :: WidgetClass w =>
  IO () -> ConnectAfter -> w -> IO (ConnectId w)
connectToUnmap = connect_NONE__NONE "unmap"

connectToUnrealize :: WidgetClass w =>
  IO () -> ConnectAfter -> w -> IO (ConnectId w)
connectToUnrealize = connect_NONE__NONE "unrealize"

connectToVisibilityNotify  :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToVisibilityNotify = 
  event "visibility_notify_event" [VisibilityNotifyMask]

connectToWindowState  :: WidgetClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToWindowState = event "window_state_event" []

