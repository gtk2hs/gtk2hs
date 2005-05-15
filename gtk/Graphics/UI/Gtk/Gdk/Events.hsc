-- -*-haskell-*-
--  GIMP Toolkit (GTK) GDK Events
--
--  Author : Axel Simon
--
--  Created: 27 April 2001
--
--  Version $Revision: 1.4 $ from $Date: 2005/05/15 19:34:46 $
--
--  Copyright (C) 2001-2005 Axel Simon
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
-- Maintainer  : gtk2hs-users\@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
module Graphics.UI.Gtk.Gdk.Events (
  Modifier,		-- a mask of control keys
  Event(..),		-- information in event callbacks from Gdk
  marshExposeRect,

  -- selector functions
#if __GLASGOW_HASKELL__<600
  sent,
  area,
  region,
  count,
  time,
  x,y,
  modif,
  isHint,
  xRoot, yRoot,
  modif,
  click,
  button,
  release,
  withCapsLock,
  withNumLock,
  withScrollLock,
  keyName,
  keyChar,
  cMode,
  nType,
  inFocus,
  xPar, yPar,
  width, height,
  visible,
  wMask, wState,
  touches,
#endif
  marshalEvent,		-- convert a pointer to an event data structure
  -- used data structures
  VisibilityState(..),
  CrossingMode(..),
  NotifyType(..),
  WindowState(..),
  ScrollDirection(..),
  Button(..),
  Click(..),
  Rectangle(..)
  ) where

import Data.Bits ((.&.), (.|.))
import Data.Char ( chr )
import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Flags
import Graphics.UI.Gtk.Gdk.Region       (Region, makeNewRegion)
import Graphics.UI.Gtk.Gdk.Enums	(VisibilityState(..),
					 CrossingMode(..),
					 NotifyType(..),
					 WindowState(..),
					 ScrollDirection(..))
import Graphics.UI.Gtk.General.Enums	(Button(..), Click(..))
import Graphics.UI.Gtk.General.Structs	(Rectangle(..))

#include <gdk/gdk.h>

-- | Modifer keys.
--
-- * This data type is used to build lists of modifers that were active
--   during an event.
--
-- * While 'Apple' stands for the Apple key on Macintoshs, it also
--   stands for the Windows key on PC keyboards or the Super key on
--   Unix machines. It\'s called Apple since it is probably mostly used
--   in the Macintosh environment.
--
data Modifier
  = Shift
  | Control
  | Alt
  | Apple
  -- | Compose is often labelled Alt Gr.
  | Compose
  deriving (Bounded, Show)

instance Enum Modifier where
  toEnum #{const GDK_SHIFT_MASK} = Shift
  toEnum #{const GDK_CONTROL_MASK} = Control
  toEnum #{const GDK_MOD1_MASK} = Alt
  toEnum #{const GDK_MOD4_MASK} = Apple
  toEnum #{const GDK_MOD5_MASK} = Compose
  fromEnum Shift = #{const GDK_SHIFT_MASK}
  fromEnum Control = #{const GDK_CONTROL_MASK}
  fromEnum Alt = #{const GDK_MOD1_MASK}
  fromEnum Apple = #{const GDK_MOD4_MASK}
  fromEnum Compose = #{const GDK_MOD5_MASK}

instance Flags Modifier

-- Turn an int into a modifier.
--
-- * Use instead of (toFlags . fromIntegral) since the latter will fail
--   if flags are set for which no constructor exists
--
toModifier :: #{type guint} -> [Modifier]
toModifier i = toFlags ((fromIntegral i) .&. mask)
  where
  mask = foldl (.|.) 0 (map fromEnum [Shift, Control, Alt, Apple, Compose])


-- Note on Event:
-- * 'Event' can communicate a small array of data to another widget. This
--   functionality is not bound as it can be done easier in Haskell.
--
-- * EventDND is not implemented as registering a DND source or sink
--   should be easier and sufficient for everything.
--
-- * EventProperty is not bound since it involves Atoms and its hard to see
--   how a Haskell application should extract the data. It should be possible
--   to connect directly to property-changed signals. If there is a need
--   to monitor a property for which there is no signal we could add
--   a trigger for just that property.
--
-- * EventSelection - I don\'t quite see how this works, so not bound.
--
-- * NoExpose - seems pointless: you copy from a drawable and this signal
--   tells you that it was up-to-date without redrawing. Maybe I'm missing
--   something.
--
-- * EventSetting informs about a change in setting that are shared among
--   several applications. They are probably not relevant to user defined
--   widgets. Anyway they don\'t make sense before GtkSettings isn\'t bound.
--
-- * Property is a TODO. These come from RC files which are useful for
--   custom widgets.

-- | Events that are delivered to a wdiget.
--
-- * Any given signal only emits one of these variants as described
--   in 'Widget'. Many events share common attributes which are
--   described below.
--
-- ** The 'sent' attribute is @True@ if the event was not created by the
--    user but by another application.
--
-- ** The 'time' attribute contains a time in milliseconds when the event
--    happened.
--
-- ** The 'x' and 'y' attributes contain the coordinates relative to the
--    'DrawWindow' associated with this widget. The values can contain
--    sub-pixel information if the input device is a graphics tablet or
--    the like.
--
-- ** The 'modif' attribute denotes what modifier key was pressed during
--    the event.
--
data Event
  = Event {
    sent	:: Bool }    
  -- | The expose event.
  --
  -- * A region of widget that receives this event needs to be redrawn.
  --   This event is the result of revealing part or all of a window
  --   or by the application calling functions like 'widgetQueueDrawArea'.
  --
  | Expose {
    sent	:: Bool,
    -- | A bounding box denoting what needs to be updated. For a more
    -- detailed information on the area that needs redrawing, use the
    -- next field.
    area	:: Rectangle,
    -- | A set of horizontal stripes that denote the invalid area.
    region      :: Region,

    -- | The number of contiguous 'Expose' events following this
    --   one. The only use for this is "exposure compression", i.e. 
    --   handling all contiguous 'Expose' events in one go, though Gdk
    --   performs some exposure compression so this is not normally needed.
    count	:: Int }
  -- | Mouse motion.
  --
  -- * Captures the movement of the mouse cursor while it is within the area
  --   of the widget.
  --
  | Motion {
    sent	:: Bool,
    time	:: Integer,
    x,y		:: Double,
    modif	:: [Modifier],
    -- | Indicate if this event is only a hint of the motion.
    --
    -- * If the 'PointerMotionHintMask' is set with 'widgetAddEvents' then
    --   mouse positions are only generated each time a 'drawWindowGetPointer'
    --   is called. In this case 'isHint' is set to @True@.
    --
    isHint	:: Bool,
    xRoot,
    yRoot	:: Double } 
  -- | A mouse button was pressed or released.
  --
  -- * This event is triggered if the mouse button was pressed or released
  --   while the mouse cursor was within the region of the widget.
  --
  | Button {
    sent	:: Bool,
    -- | The kind of button press, see 'Click'. Note that double clicks will
    --   trigger this event with 'click' set to 'SingleClick', 'ReleaseClick',
    --   'SingleClick', 'DoubleClick', 'ReleaseClick'. Triple clicks will
    --   produce this sequence followed by 'SingleClick', 'DoubleClick',
    --   'TripleClick', 'ReleaseClick'.
    click	:: Click,
    -- | The time of the event in milliseconds.
    time	:: Integer,
    x,y		:: Double,
    modif	:: [Modifier],
    -- | The button that was pressed.
    button	:: Button,
    -- | The coordinates of the click relative the the screen origin.
    xRoot,
    yRoot	:: Double }
  -- | A key was pressed while the widget had the input focus.
  --
  -- * If the widget has the current input focus (see 'widgetSetCanFocus')
  --   it will receive key pressed events. Certain key combinations are of
  --   no interest to a normal widget like Alt-F to access the file menu.
  --   For all these keys, the handler must return @False@ to indicate that
  --   the key stroke should be propagated to the parent widget. At the
  --   top-level widget, keyboard shortcuts like Alt-F are turned into the
  --   corresponding signals.
  --
  | Key {
    -- | This flag is set if the key was released. This flag makes it possible
    -- to connect the same handler to 'onKeyPress' and 'onKeyRelease'.
    release	:: Bool,
    sent	:: Bool,
    time	:: Integer,
    modif	:: [Modifier],
    -- | This flag is @True@ if Caps Lock is on while this key was pressed.
    withCapsLock,
    -- | This flag is @True@ if Number Lock is on while this key was pressed.
    withNumLock,
    -- | This flag is @True@ if Scroll Lock is on while this key was pressed.
    withScrollLock :: Bool,
    -- | A string representing the key that was pressed or released.
    --
    -- * This string contains a description of the key rather than what
    --   should appear on screen. For example, pressing "1" on the keypad
    --   results in "KP_1". Of particular interest are "F1" till "F12",
    --   for a complete list refer to "<gdk/gdkkeysyms.h>" where all
    --   possible values are defined. The corresponding strings are the
    --   constants without the GDK_ prefix.
    keyName	:: String,
    -- | A character matching the key that was pressed.
    --
    -- * This entry can be used to build up a whole input string.
    --   The character is @Nothing@ if the key does not correspond to a simple
    --   unicode character.
    --
    keyChar     :: Maybe Char }
  -- | Mouse cursor crossing event.
  --
  -- * This event indicates that the mouse cursor is hovering over this
  --   widget. It is used to set a widget into the pre-focus state where
  --   some GUI elements like buttons on a toolbar change their appearance.
  --
  | Crossing {
    sent	:: Bool,
    time	:: Integer,
    x,y		:: Double,
    xRoot,
    yRoot	:: Double,
    -- | Kind of enter/leave event.
    --
    -- * The mouse cursor might enter this widget because it grabs the mouse
    --   cursor for e.g. a modal dialog box.
    --
    cMode	:: CrossingMode,
    -- | Information on from what level of the widget hierarchy the mouse
    --   cursor came.
    --
    -- * See 'NotifyType'.
    --
    nType	:: NotifyType,
    modif	:: [Modifier]}
  -- | Gaining or loosing input focus.
  --
  | Focus {
    sent	:: Bool,
    -- | This flag is @True@ if the widget receives the focus and @False@ if
    -- it just lost the input focus.
    inFocus	:: Bool}
  -- | The widget\'s size has changed.
  --
  -- * In response to this event the application can allocate resources that
  --   are specific to the size of the widget. It is emitted when the widget
  --   is shown the first time and on every resize.
  --
  | Configure {
    sent	:: Bool,
    -- | Position within the parent window.
    xPar	:: Int,
    -- | Position within the parent window.
    yPar	:: Int,
    width	:: Int,
    height	:: Int}
  -- | Change of visibility of a widget.
  | Visibility {
    sent	:: Bool,
    -- | Denote what portions of the widget is visible.
    visible	:: VisibilityState }
  -- | Wheel movement of the mouse.
  --
  -- * This action denotes that the content of the widget should be scrolled.
  --   The event is triggered by the movement of the mouse wheel. Surrounding
  --   scroll bars are independant of this signal. Most mice do not have
  --   buttons for horizontal scrolling, hence 'direc' will usually not
  --   contain 'ScrollLeft' and 'ScrollRight'. Mice with additional
  --   buttons may not work on X since only five buttons are supported
  --   (the three main buttons and two for the wheel).
  --
  -- * The handler of this signal should update the scroll bars that
  --   surround this widget which in turn tell this widget to update.
  --
  | Scroll {
    sent	:: Bool,
    time	:: Integer,
    x,y		:: Double,
    direc	:: ScrollDirection,
    xRoot,
    yRoot	:: Double}
  -- | Indicate how the appearance of this window has changed.
  | WindowState {
    sent	:: Bool,
    -- | The mask indicates which flags have changed.
    wMask	:: WindowState,
    -- | The state indicates the current state of the window.
    wState	:: WindowState} 
  -- | The state of the pen of a graphics tablet pen.
  | Proximity {
    sent	:: Bool,
    time	:: Integer,
    -- | Whether the pen was removed or set onto the tablet.
    touches     :: Bool
  }

marshalEvent :: Ptr Event -> IO Event
marshalEvent ptr = do
  (eType::#type GdkEventType) <- #{peek GdkEventAny,type} ptr
  (case eType of
    #{const GDK_EXPOSE}		-> marshExpose
    #{const GDK_MOTION_NOTIFY}	-> marshMotion
    #{const GDK_BUTTON_PRESS}	-> marshButton SingleClick
    #{const GDK_2BUTTON_PRESS}  -> marshButton DoubleClick
    #{const GDK_3BUTTON_PRESS}	-> marshButton TripleClick
    #{const GDK_BUTTON_RELEASE}	-> marshButton ReleaseClick
    #{const GDK_KEY_PRESS}	-> marshKey False
    #{const GDK_KEY_RELEASE}	-> marshKey True
    #{const GDK_ENTER_NOTIFY}	-> marshCrossing
    #{const GDK_FOCUS_CHANGE}	-> marshFocus
    #{const GDK_CONFIGURE}	-> marshConfigure
--    #{const GDK_PROPERTY_NOTIFY}-> marshProperty
    #{const GDK_PROXIMITY_IN}   -> marshProximity True
    #{const GDK_PROXIMITY_OUT}	-> marshProximity False
    #{const GDK_VISIBILITY_NOTIFY}-> marshVisibility
    #{const GDK_SCROLL}		-> marshScroll
    #{const GDK_WINDOW_STATE}	-> marshWindowState
    _				-> \_ -> return
      (error "marshalEvent: unhandled event type")
    ) ptr

marshExpose ptr = do
  (#{const GDK_EXPOSE}::#type GdkEventType) <- #{peek GdkEventAny,type} ptr
  (sent_   ::#type gint8)	<- #{peek GdkEventExpose, send_event} ptr
  (area_   ::Rectangle)		<- #{peek GdkEventExpose, area} ptr
  (reg_   :: Ptr Region)	<- #{peek GdkEventExpose, region} ptr
  reg_ <- gdk_region_copy reg_
  region_ <- makeNewRegion reg_
  (count_  ::#type gint)	<- #{peek GdkEventExpose, count} ptr
  return $ Expose {
    sent   = toBool sent_,
    area   = area_,
    region = region_,
    count  = fromIntegral count_}

foreign import ccall "gdk_region_copy"
  gdk_region_copy :: Ptr Region -> IO (Ptr Region)

marshExposeRect ptr = do
  (#{const GDK_EXPOSE}::#type GdkEventType) <- #{peek GdkEventAny,type} ptr
  (area_   ::Rectangle)		<- #{peek GdkEventExpose, area} ptr
  return area_

marshMotion ptr = do
  (sent_   ::#type gint8)	<- #{peek GdkEventMotion, send_event} ptr
  (time_   ::#type guint32)	<- #{peek GdkEventMotion, time} ptr
  (x_	   ::#type gdouble)	<- #{peek GdkEventMotion, x} ptr
  (y_	   ::#type gdouble)	<- #{peek GdkEventMotion, y} ptr
  (modif_  ::#type guint)	<- #{peek GdkEventMotion, state} ptr
  (isHint_ ::#type gint16)	<- #{peek GdkEventMotion, is_hint} ptr
  (xRoot_  ::#type gdouble)	<- #{peek GdkEventMotion, x_root} ptr
  (yRoot_  ::#type gdouble)	<- #{peek GdkEventMotion, y_root} ptr
  return $ Motion {
    sent   = toBool sent_,
    time   = fromIntegral time_,
    x	   = (fromRational.toRational) x_,
    y	   = (fromRational.toRational) y_,
    modif  = toModifier modif_,
    isHint = toBool isHint_,
    xRoot  = (fromRational.toRational) xRoot_,
    yRoot  = (fromRational.toRational) yRoot_}

marshButton but ptr = do
  (sent_   ::#type gint8)	<- #{peek GdkEventButton, send_event} ptr
  (time_   ::#type guint32)	<- #{peek GdkEventButton, time} ptr
  (x_	   ::#type gdouble)	<- #{peek GdkEventButton, x} ptr
  (y_	   ::#type gdouble)	<- #{peek GdkEventButton, y} ptr
  (modif_  ::#type guint)	<- #{peek GdkEventButton, state} ptr
  (button_ ::#type guint)	<- #{peek GdkEventButton, button} ptr
  (xRoot_  ::#type gdouble)	<- #{peek GdkEventButton, x_root} ptr
  (yRoot_  ::#type gdouble)	<- #{peek GdkEventButton, y_root} ptr
  return $ Button {
    click  = but,
    sent   = toBool sent_,
    time   = fromIntegral time_,
    x	   = (fromRational.toRational) x_,
    y	   = (fromRational.toRational) y_,
    modif  = toModifier modif_,
    button = (toEnum.fromIntegral) button_,
    xRoot  = (fromRational.toRational) xRoot_,
    yRoot  = (fromRational.toRational) yRoot_}


marshKey up ptr = do
  (sent_   ::#type gint8)	<- #{peek GdkEventKey, send_event} ptr
  (time_   ::#type guint32)	<- #{peek GdkEventKey, time} ptr
  (modif_  ::#type guint)	<- #{peek GdkEventKey, state} ptr
  (keyval_ ::#type guint)	<- #{peek GdkEventKey, keyval} ptr
  
  (length_ ::#type gint)	<- #{peek GdkEventKey, length} ptr
  return $ Key {
    release = up,
    sent = toBool sent_,
    time   = fromIntegral time_,
    modif  = toModifier modif_,
    withCapsLock = (modif_ .&. #{const GDK_LOCK_MASK})/=0,
    withNumLock = (modif_ .&. #{const GDK_MOD2_MASK})/=0,
    withScrollLock = (modif_ .&. #{const GDK_MOD3_MASK})/=0,
    keyName = unsafePerformIO $ do
      valPtr <- gdk_keyval_name keyval_
      peekUTFString valPtr,
    keyChar = unsafePerformIO $ do
      uchar <- gdk_keyval_to_unicode keyval_
      return (if uchar==0 then Nothing else Just (chr (fromIntegral uchar))) }

foreign import ccall "gdk_keyval_name"
  gdk_keyval_name :: #{type guint} -> IO CString

foreign import ccall "gdk_keyval_to_unicode"
  gdk_keyval_to_unicode :: #{type guint} -> IO #{type guint32}

marshCrossing ptr = do
  (sent_   ::#type gint8)	<- #{peek GdkEventCrossing, send_event} ptr
  (time_   ::#type guint32)	<- #{peek GdkEventCrossing, time} ptr
  (x_	   ::#type gdouble)	<- #{peek GdkEventCrossing, x} ptr
  (y_	   ::#type gdouble)	<- #{peek GdkEventCrossing, y} ptr
  (modif_  ::#type guint)	<- #{peek GdkEventCrossing, state} ptr
  (xRoot_  ::#type gdouble)	<- #{peek GdkEventCrossing, x_root} ptr
  (yRoot_  ::#type gdouble)	<- #{peek GdkEventCrossing, y_root} ptr
  (cMode_  ::#type GdkCrossingMode) 
				<- #{peek GdkEventCrossing, mode} ptr
  (nType_  ::#type GdkNotifyType)
				<- #{peek GdkEventCrossing, detail} ptr  
  (modif_  ::#type guint)	<- #{peek GdkEventCrossing, state} ptr
  return $ Crossing {
    sent   = toBool sent_,
    time   = fromIntegral time_,
    x	   = (fromRational.toRational) x_,
    y	   = (fromRational.toRational) y_,
    xRoot  = (fromRational.toRational) xRoot_,
    yRoot  = (fromRational.toRational) yRoot_,
    cMode  = (toEnum.fromIntegral) cMode_,
    nType  = (toEnum.fromIntegral) nType_,
    modif  = toModifier modif_}


marshFocus ptr = do
  (sent_   ::#type gint8)	<- #{peek GdkEventFocus, send_event} ptr
  (inFocus_::#type gint16)	<- #{peek GdkEventFocus, in} ptr
  return $ Focus {
    sent   = toBool sent_,
    inFocus= toBool inFocus_}

marshConfigure ptr = do
  (sent_   ::#type gint8)	<- #{peek GdkEventConfigure, send_event} ptr
  (xPar_   ::#type gint)	<- #{peek GdkEventConfigure, x} ptr
  (yPar_   ::#type gint)	<- #{peek GdkEventConfigure, y} ptr
  (width_  ::#type gint)	<- #{peek GdkEventConfigure, width} ptr
  (height_ ::#type gint)	<- #{peek GdkEventConfigure, height} ptr
  return $ Configure {
    sent   = toBool sent_,
    xPar   = fromIntegral xPar_,
    yPar   = fromIntegral yPar_,
    width  = fromIntegral width_,
    height = fromIntegral height_}

{-
marshProperty ptr = do
  (sent_   ::#type gint8)	<- #{peek GdkEventProperty, send_event} ptr
  (time_   ::#type guint32)	<- #{peek GdkEventProperty, time} ptr
  return $ Property {
    sent   = toBool sent_,
    time   = fromIntegral time_}
-}

marshProximity touches ptr = do
  (sent_   ::#type gint8)	<- #{peek GdkEventProximity, send_event} ptr
  (time_   ::#type guint32)	<- #{peek GdkEventProximity, time} ptr
  return $ Proximity {
    sent   = toBool sent_,
    time   = fromIntegral time_,
    touches = touches}

marshVisibility ptr = do
  (sent_   ::#type gint8)	<- #{peek GdkEventVisibility, send_event} ptr
  (state_  ::#type GdkVisibilityState)
				<- #{peek GdkEventVisibility, state} ptr
  return $ Visibility {
    sent   = toBool sent_,
    visible= (toEnum.fromIntegral) state_}

marshScroll ptr = do
  (sent_   ::#type gint8)	<- #{peek GdkEventScroll, send_event} ptr
  (time_   ::#type guint32)	<- #{peek GdkEventScroll, time} ptr
  (x_	  ::#type gdouble)	<- #{peek GdkEventScroll, x} ptr
  (y_	  ::#type gdouble)	<- #{peek GdkEventScroll, y} ptr
  (direc_  ::#type GdkScrollDirection)
				<- #{peek GdkEventScroll, direction} ptr
  (xRoot_  ::#type gdouble)	<- #{peek GdkEventScroll, x_root} ptr
  (yRoot_  ::#type gdouble)	<- #{peek GdkEventScroll, y_root} ptr
  return $ Scroll {
    sent   = toBool sent_,
    time   = fromIntegral time_,
    x	   = (fromRational.toRational) x_,
    y	   = (fromRational.toRational) y_,
    direc  = (toEnum.fromIntegral) direc_,
    xRoot  = (fromRational.toRational) xRoot_,
    yRoot  = (fromRational.toRational) yRoot_}


marshWindowState ptr = do
  (sent_   ::#type gint8)	<- #{peek GdkEventWindowState, send_event} ptr
  (wMask_  ::#type GdkWindowState)
			<- #{peek GdkEventWindowState, changed_mask} ptr
  (wState_ ::#type GdkWindowState)
			<- #{peek GdkEventWindowState, new_window_state} ptr
  return $ WindowState {
    sent   = toBool sent_,
    wMask  = (toEnum.fromIntegral) wMask_,
    wState = (toEnum.fromIntegral) wState_}




