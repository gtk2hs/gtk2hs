-- -*-haskell-*-
--  GIMP Toolkit (GTK) Datastructure
--
--  Author : Axel Simon
--          
--  Created: 27 April 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:17:26 $
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

module Graphics.UI.Gtk.Gdk.Events (
  Modifier,		-- a mask of control keys
  -- tests for a specific control key
  hasShift,
  hasLock, 
  hasControl, 
  hasMod1, 
  hasMod2, 
  hasMod3, 
  hasMod4, 
  hasMod5,
  hasButLeft, 
  hasButRight, 
  hasButMiddle,
  Event(..),		-- information in event callbacks from Gdk
  -- selector functions
#if __GLASGOW_HASKELL__<600
  sent,			-- True if this is event does not come from user input
  area,			-- Rectangle which is to be exposed, etc.
  count,		-- number of upcoming events
  time,			-- running number of event
  x,y,			-- floating point coordinates within widget
  xRoot, yRoot,		-- dto., relative to parent widget
  modif,		-- the modifier keys that were active
  isHint,		-- True if this is a hint in the X Windows meaning
  button,		-- Button number which triggered event
  keyval,		-- key code (see gdk/gdkkeysyms.h)
  len,			-- length of string that a key generated
  str,			-- the string a key generated
  cMode,		-- crossing mode
  nType,		-- notify type
  inFocus,		-- True if event is generated for entering widget
  xPar, yPar,		-- new integral values for position relative to parent
  width, height,	-- new size of a widget
  visible,		-- state of visibility
  wMask, wState,	-- new (?possible? and) real state of a window
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

import Data.Bits ((.&.))

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Gdk.Enums	(VisibilityState(..),
					 CrossingMode(..),
					 NotifyType(..),
					 WindowState(..),
					 ScrollDirection(..))
import Graphics.UI.Gtk.General.Enums	(Button(..), Click(..))
import Graphics.UI.Gtk.General.Structs	(Rectangle(..))

#include <gdk/gdk.h>


-- | modifier key flags
--
type Modifier = #{type guint}

hasShift, hasLock, hasControl, hasMod1, hasMod2, hasMod3, hasMod4, hasMod5,
  hasButLeft, hasButRight, hasButMiddle :: Modifier -> Bool
hasShift     x = (x .&. #{const GDK_SHIFT_MASK}) /= 0
hasLock	     x = (x .&. #{const GDK_LOCK_MASK}) /= 0
hasControl   x = (x .&. #{const GDK_CONTROL_MASK}) /= 0
hasMod1	     x = (x .&. #{const GDK_MOD1_MASK}) /= 0
hasMod2	     x = (x .&. #{const GDK_MOD2_MASK}) /= 0
hasMod3	     x = (x .&. #{const GDK_MOD3_MASK}) /= 0
hasMod4	     x = (x .&. #{const GDK_MOD4_MASK}) /= 0
hasMod5	     x = (x .&. #{const GDK_MOD5_MASK}) /= 0
hasButLeft   x = (x .&. #{const GDK_BUTTON1_MASK}) /= 0
hasButRight  x = (x .&. #{const GDK_BUTTON3_MASK}) /= 0
hasButMiddle x = (x .&. #{const GDK_BUTTON2_MASK}) /= 0


data Event
  = Event {
    sent	:: Bool }    
  | Expose {
    sent	:: Bool,
    area	:: Rectangle,
    count	:: Int }
  | Motion {
    sent	:: Bool,
    time	:: Integer,
    x,y		:: Double,
    modif	:: Modifier,
    isHint	:: Bool,
    xRoot,
    yRoot	:: Double } 
  | Button {
    sent	:: Bool,
    click	:: Click,
    time	:: Integer,
    x,y		:: Double,
    modif	:: Modifier,
    button	:: Button,
    xRoot,
    yRoot	:: Double }
  | Key {
    release	:: Bool,
    sent	:: Bool,
    time	:: Integer,
    modif	:: Modifier,
    keyval	:: Integer,
    len		:: Int,
    str		:: String }
  | Crossing {
    sent	:: Bool,
    time	:: Integer,
    x,y		:: Double,
    xRoot,
    yRoot	:: Double,
    cMode	:: CrossingMode,
    nType	:: NotifyType,
    modif	:: Modifier}
  | Focus {
    sent	:: Bool,
    inFocus	:: Bool}
  | Configure {
    sent	:: Bool,
    xPar	:: Int,
    yPar	:: Int,
    width	:: Int,
    height	:: Int}
  | Property {
    sent	:: Bool,
    time	:: Integer}
  | Visibility {
    sent	:: Bool,
    visible	:: VisibilityState }
  | Scroll {
    sent	:: Bool,
    time	:: Integer,
    x,y		:: Double,
    direc	:: ScrollDirection,
    xRoot,
    yRoot	:: Double}
  | WindowState {
    sent	:: Bool,
    wMask	:: WindowState,
    wState	:: WindowState} 

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
    #{const GDK_PROPERTY_NOTIFY}-> marshProperty
    #{const GDK_SELECTION_CLEAR}-> marshSelection
    #{const GDK_SELECTION_REQUEST}-> marshSelection
    #{const GDK_SELECTION_NOTIFY}-> marshSelection
    #{const GDK_PROXIMITY_IN}   -> marshProximity True
    #{const GDK_PROXIMITY_OUT}	-> marshProximity False
    #{const GDK_DRAG_ENTER}	-> marshDND
    #{const GDK_DRAG_LEAVE}	-> marshDND
    #{const GDK_DRAG_MOTION}	-> marshDND
    #{const GDK_DRAG_STATUS}	-> marshDND
    #{const GDK_DROP_START}	-> marshDND
    #{const GDK_DROP_FINISHED}	-> marshDND
    #{const GDK_CLIENT_EVENT}	-> marshClient
    #{const GDK_VISIBILITY_NOTIFY}-> marshVisibility
    #{const GDK_NO_EXPOSE}	-> marshNoExpose
    #{const GDK_SCROLL}		-> marshScroll
    #{const GDK_WINDOW_STATE}	-> marshWindowState
    #{const GDK_SETTING}	-> marshSetting
    _				-> marshAny
    ) ptr

marshAny ptr = do
  (sent_   ::#type gint8)	<- #{peek GdkEventAny, send_event} ptr
  return $ Event {
    sent   = toBool sent_ }

marshExpose ptr = do
  (sent_   ::#type gint8)	<- #{peek GdkEventExpose, send_event} ptr
  (area_   ::Rectangle)		<- #{peek GdkEventExpose, area} ptr
  (count_  ::#type gint)	<- #{peek GdkEventExpose, count} ptr
  return $ Expose {
    sent   = toBool sent_,
    area   = area_,
    count  = fromIntegral count_}

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
    modif  = fromIntegral modif_,
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
    modif  = fromIntegral modif_,
    button = (toEnum.fromIntegral) button_,
    xRoot  = (fromRational.toRational) xRoot_,
    yRoot  = (fromRational.toRational) yRoot_}


marshKey up ptr = do
  (sent_   ::#type gint8)	<- #{peek GdkEventKey, send_event} ptr
  (time_   ::#type guint32)	<- #{peek GdkEventKey, time} ptr
  (modif_  ::#type guint)	<- #{peek GdkEventKey, state} ptr
  (keyval_ ::#type guint)	<- #{peek GdkEventKey, keyval} ptr
  (string_ ::CString)		<- #{peek GdkEventKey, string} ptr
  str_				<- peekUTFString string_
  (length_ ::#type gint)	<- #{peek GdkEventKey, length} ptr
  return $ Key {
    release = up,
    sent = toBool sent_,
    time   = fromIntegral time_,
    modif  = fromIntegral modif_,
    keyval = fromIntegral keyval_,
    len	   = fromIntegral length_,
    str	   = str_}

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
    modif  = fromIntegral modif_}


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

marshProperty ptr = do
  (sent_   ::#type gint8)	<- #{peek GdkEventProperty, send_event} ptr
  (time_   ::#type guint32)	<- #{peek GdkEventProperty, time} ptr
  return $ Property {
    sent   = toBool sent_,
    time   = fromIntegral time_}

marshSelection = marshAny

marshProximity _ = marshAny

marshDND = marshAny

-- this should be changed (i.e. implemented)
marshClient = marshAny

marshVisibility ptr = do
  (sent_   ::#type gint8)	<- #{peek GdkEventVisibility, send_event} ptr
  (state_  ::#type GdkVisibilityState)
				<- #{peek GdkEventVisibility, state} ptr
  return $ Visibility {
    sent   = toBool sent_,
    visible= (toEnum.fromIntegral) state_}


marshNoExpose = marshAny

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

-- what event might this type be?
marshSetting = marshAny



