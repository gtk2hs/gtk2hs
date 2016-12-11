{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE FlexibleContexts #-}
#endif
-- -*-haskell-*-

#include <gtk/gtk.h>
#include "template-hsc-gtk2hs.h"

--  GIMP Toolkit (GTK) GDK Event information in a Monad
--
--  Author : Axel Simon
--
--  Created 12 October 2008
--
--  Copyright (C) 2008 Axel Simon
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
-- #prune

-- |
-- Maintainer  : gtk2hs-users\@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Types and accessors to examine information in events.
--
module Graphics.UI.Gtk.Gdk.EventM (
-- * Detail
--
-- | This modules provides a monad that encapsulates the information in an
--   event.
--
--   The events a widget can receive are defined in
--   "Graphics.UI.Gtk.Abstract.Widget#7". Every event carries additional
--   information which is accessible through functions in the 'EventM' monad.
--   For instance, every event is associated with a
--   'Graphics.UI.Gtk.Gdk.DrawWindow.DrawWindow' which is accessed using the
--   'eventWindow' accessor function. Other information is only available in
--   one specific event. For example, the
--   area that has to be redrawn, accessed by 'eventArea' is only available in
--   the 'Graphics.UI.Gtk.Abstract.Widget.exposeEvent'. Indeed, you can only
--   call 'eventArea' if the first type parameter of 'EventM' is the phantom
--   type 'EExpose'. (A phantom type is a type for which no values exist and
--   which is only used to enforce certain constraints on the usage of
--   functions such as 'eventArea'.) Some information is available in several
--   but not all events. In order to express these constraints the module
--   defines type classes whose names start with @Has...@ but which are not
--   exported, implying that no new instance can be created. (They could be
--   called phantom type classes.) For instance, the mouse pointer coordinates
--   can be retrieved using the function 'eventCoordinates' which requires
--   that the first type parameter of 'EventM' is in the class
--   'HasCoordinates'. The module supplies instance of class 'HasCoordinates'
--   for the types 'EButton', 'ECrossing', 'EMotion' and 'EScroll'. Thus for
--   all events that require an 'EventM' action with one of the types above,
--   the accessor function 'eventCoordinates' may be used.
--
--   Note that an event handler must always returns @True@ if the event
--   was handled or @False@ if the event should be dealt with by another
--   event handler. For instance, a handler for a key press should return
--   @False@ if the pressed key is not one of those that the widget reacts
--   to. In this case the event is passed to the parent widgets. This
--   ensures that pressing, say, @Alt-F@ opens the file menu even if the
--   current input focus is in a text entry widget. In order to facilitate
--   writing handlers that may abort handling an event, this module provides
--   the function 'tryEvent'. This function catches pattern match exceptions
--   and returns @False@. If the signal successfully runs to its end, it
--   returns @True@. A typical use is as follows:
--
--   > widget `on` keyPressEvent $ tryEvent $ do
--   >   [Control] <- eventModifier
--   >   "Return" <- eventKeyName
--   >   liftIO $ putStrLn "Ctrl-Return pressed"
--
--   The rationale is that the action will throw an exception if the
--   two event functions 'eventModifier' and 'eventKeyName' return something
--   else than what is stated in
--   the pattern. When no exception is thrown, execution continues to
--   the last statement where the event is processed, here we merely
--   print a message. Note that the return
--   value of this statement must be @()@ since 'tryEvent' always
--   assumes that the
--   function handeled the event if no exception is thrown. A handler
--   wrapped by 'tryEvent' can also indicate that it cannot handle the
--   given event by calling 'stopEvent'.
--
--   Finally, not that the 'EventM' monad wraps the @IO@ monad. As such
--   you can (and usually have to) use @liftIO@ to execute @IO@ functions.
--

-- * Classes

  HasCoordinates,
  HasRootCoordinates,
  HasModifier,
  HasTime,

-- * Event monad and type tags
  EventM,
  EAny,
  EKey,
  EButton,
  EScroll,
  EMotion,
  EExpose,
  EVisibility,
  ECrossing,
  EFocus,
  EConfigure,
  EProperty,
  EProximity,
  EWindowState,
#if GTK_CHECK_VERSION(2,6,0)
  EOwnerChange,
#endif
#if GTK_CHECK_VERSION(2,8,0)
  EGrabBroken,
#endif

-- * Accessor functions for event information
  eventWindow,
  eventSent,
  eventCoordinates,
  eventRootCoordinates,
  eventModifier,
  eventModifierAll,
  eventModifierMouse,
  eventTime,
  eventKeyVal,
  eventKeyName,
  eventHardwareKeycode,
  eventKeyboardGroup,
  MouseButton(..),
  eventButton,
  Click(..),
  eventClick,
  ScrollDirection(..),
  eventScrollDirection,
  eventIsHint,
#if GTK_CHECK_VERSION(2,12,0)
  eventRequestMotions,
#endif
  eventArea,
#if GTK_MAJOR_VERSION < 3
  eventRegion,
#endif
  VisibilityState(..),
  eventVisibilityState,
  CrossingMode(..),
  eventCrossingMode,
  NotifyType(..),
  eventNotifyType,
  eventCrossingFocus,
  eventFocusIn,
  eventPosition,
  eventSize,
  eventProperty,
  WindowState(..),
  eventWindowStateChanged,
  eventWindowState,
#if GTK_CHECK_VERSION(2,6,0)
  OwnerChange(..),
  eventChangeReason,
  eventSelection,
  eventSelectionTime,
#endif
#if GTK_CHECK_VERSION(2,8,0)
  eventKeyboardGrab,
  eventImplicit,
  eventGrabWindow,
#endif

-- * Auxilliary Definitions
  Modifier(..),         -- a mask of control keys
  TimeStamp,
  currentTime,
  tryEvent,
  stopEvent,
  ) where

import Prelude hiding (catch)
import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Flags
import System.Glib.GObject ( makeNewGObject )
import Graphics.UI.Gtk.Gdk.Keys         (KeyVal, KeyCode, keyName)
#if GTK_MAJOR_VERSION < 3
import Graphics.UI.Gtk.Gdk.Region       (Region, makeNewRegion)
#endif
import Graphics.UI.Gtk.Gdk.Enums        (Modifier(..), VisibilityState(..),
  CrossingMode(..), NotifyType(..), WindowState(..), ScrollDirection(..),
#if GTK_CHECK_VERSION(2,6,0)
  OwnerChange(..)
#endif
  )
import Graphics.UI.Gtk.General.Enums    (MouseButton(..), Click(..))
import Graphics.UI.Gtk.General.Structs  (Rectangle(..))
import Graphics.UI.Gtk.General.DNDTypes (Atom(..), SelectionTag)
import Graphics.UI.Gtk.Types ( DrawWindow, mkDrawWindow )

import Data.List (isPrefixOf)
import Control.Monad.Reader ( ReaderT, ask, runReaderT )
import Control.Monad.Trans ( liftIO )
import Control.Monad ( liftM )
#if __GLASGOW_HASKELL__ >= 610
import Control.Exception ( Handler(..)
                         , PatternMatchFail(..)
                         , catches, throw )
import System.IO.Error (isUserError, ioeGetErrorString)
#else
import Control.Exception (catch, throw,
                          Exception(PatternMatchFail,IOException) )
#endif


-- | A monad providing access to data in an event.
--
type EventM t = ReaderT (Ptr t) IO

-- | A tag for events that do not carry any event-specific information.
data EAny

-- | A tag for /key/ events.
data EKey

-- | A tag for /Button/ events.
data EButton

-- | A tag for /Scroll/ events.
data EScroll

-- | A tag for /Motion/ events.
data EMotion

-- | A tag for /Expose/ events.
data EExpose

-- | A tag for /Visibility/ events.
data EVisibility

-- | A tag for /Crossing/ events.
data ECrossing

-- | A tag for /Focus/ events.
data EFocus

-- | A tag for /Configure/ events.
data EConfigure

-- | A tag for /Property/ events.
data EProperty

-- | A tag for /Proximity/ events.
data EProximity

-- | A tag for /WindowState/ event.
data EWindowState

#if GTK_CHECK_VERSION(2,6,0)
-- | A tag for /OwnerChange/ events.
data EOwnerChange
#endif


#if GTK_CHECK_VERSION(2,8,0)
-- | A tag for /GrabBroken/ events.
data EGrabBroken
#endif

-- | Retrieve the 'Graphics.UI.Gtk.Gdk.DrawWindow.DrawWindow' that this
--   event relates to.
eventWindow :: EventM any DrawWindow
eventWindow = do
  ptr <- ask
  liftIO $ makeNewGObject mkDrawWindow (#{peek GdkEventAny, window} ptr)

-- | Query if this event was sent sent explicitly by the application
--   (rather than being generated by human interaction).
eventSent :: EventM any Bool
eventSent = do
  ptr <- ask
  liftIO $ #{peek GdkEventAny, send_event} ptr

class HasCoordinates a
instance HasCoordinates EButton
instance HasCoordinates EScroll
instance HasCoordinates EMotion
instance HasCoordinates ECrossing

-- | Retrieve the @(x,y)@ coordinates of the mouse.
eventCoordinates :: HasCoordinates t => EventM t (Double, Double)
eventCoordinates = do
  ptr <- ask
  liftIO $ do
    (ty :: #{gtk2hs_type GdkEventType}) <- peek (castPtr ptr)
    if ty `elem` [ #{const GDK_BUTTON_PRESS},
                   #{const GDK_2BUTTON_PRESS},
                   #{const GDK_3BUTTON_PRESS},
                   #{const GDK_BUTTON_RELEASE}] then do
        (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventButton, x} ptr
        (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventButton, y} ptr
        return (realToFrac x, realToFrac y)
      else if ty `elem` [ #{const GDK_SCROLL} ] then do
        (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventScroll, x} ptr
        (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventScroll, y} ptr
        return (realToFrac x, realToFrac y)
      else if ty `elem` [ #{const GDK_MOTION_NOTIFY} ] then do
        (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventMotion, x} ptr
        (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventMotion, y} ptr
        return (realToFrac x, realToFrac y)
      else if ty `elem` [ #{const GDK_ENTER_NOTIFY},
                          #{const GDK_LEAVE_NOTIFY}] then do
        (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventCrossing, x} ptr
        (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventCrossing, y} ptr
        return (realToFrac x, realToFrac y)
      else error ("eventCoordinates: none for event type "++show ty)

class HasRootCoordinates a
instance HasRootCoordinates EButton
instance HasRootCoordinates EScroll
instance HasRootCoordinates EMotion
instance HasRootCoordinates ECrossing

-- | Retrieve the @(x,y)@ coordinates of the mouse relative to the
--   root (origin) of the screen.
eventRootCoordinates :: HasRootCoordinates t => EventM t (Double, Double)
eventRootCoordinates = do
  ptr <- ask
  liftIO $ do
    (ty :: #{gtk2hs_type GdkEventType}) <- peek (castPtr ptr)
    if ty `elem` [ #{const GDK_BUTTON_PRESS},
                   #{const GDK_2BUTTON_PRESS},
                   #{const GDK_3BUTTON_PRESS},
                   #{const GDK_BUTTON_RELEASE}] then do
        (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventButton, x_root} ptr
        (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventButton, y_root} ptr
        return (realToFrac x, realToFrac y)
      else if ty `elem` [ #{const GDK_SCROLL} ] then do
        (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventScroll, x_root} ptr
        (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventScroll, y_root} ptr
        return (realToFrac x, realToFrac y)
      else if ty `elem` [ #{const GDK_MOTION_NOTIFY} ] then do
        (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventMotion, x_root} ptr
        (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventMotion, y_root} ptr
        return (realToFrac x, realToFrac y)
      else if ty `elem` [ #{const GDK_ENTER_NOTIFY},
                          #{const GDK_LEAVE_NOTIFY}] then do
        (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventCrossing, x_root} ptr
        (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventCrossing, y_root} ptr
        return (realToFrac x, realToFrac y)
      else error ("eventRootCoordinates: none for event type "++show ty)

class HasModifier a
instance HasModifier EKey
instance HasModifier EButton
instance HasModifier EScroll
instance HasModifier EMotion
instance HasModifier ECrossing

-- | Query the modifier keys that were depressed when the event happened.
--   Sticky modifiers such as CapsLock are omitted in the return value.
--   Use 'eventModifierAll' your application requires all modifiers.
--   Use 'eventModifierMouse' if you just need the mouse buttons.
--
eventModifier :: HasModifier t => EventM t [Modifier]
eventModifier = eM defModMask

-- | Query the modifier keys that were depressed when the event happened.
--   The result includes sticky modifiers such as CapsLock. Normally,
--   'eventModifier' is more appropriate in applications.
--
eventModifierAll :: HasModifier t => EventM t [Modifier]
eventModifierAll = eM allModMask

-- | Query the mouse buttons that were depressed when the event happened.
--
eventModifierMouse :: HasModifier t => EventM t [Modifier]
eventModifierMouse = eM mouseModMask

allModMask = -1

foreign import ccall safe "gtk_accelerator_get_default_mod_mask"
  defModMask :: #gtk2hs_type guint

mouseModMask = #{const GDK_BUTTON1_MASK}
           .|. #{const GDK_BUTTON2_MASK}
           .|. #{const GDK_BUTTON3_MASK}
           .|. #{const GDK_BUTTON4_MASK}
           .|. #{const GDK_BUTTON5_MASK}

eM mask = do
  ptr <- ask
  liftIO $ do
    (ty :: #{gtk2hs_type GdkEventType}) <- peek (castPtr ptr)
    if ty `elem` [ #{const GDK_KEY_PRESS},
                   #{const GDK_KEY_RELEASE}] then do
        (modif ::#gtk2hs_type guint)    <- #{peek GdkEventKey, state} ptr
        return (toFlags (fromIntegral (modif .&. mask)))
      else if ty `elem` [ #{const GDK_BUTTON_PRESS},
                   #{const GDK_2BUTTON_PRESS},
                   #{const GDK_3BUTTON_PRESS},
                   #{const GDK_BUTTON_RELEASE}] then do
        (modif ::#gtk2hs_type guint)    <- #{peek GdkEventButton, state} ptr
        return (toFlags (fromIntegral (modif .&. mask)))
      else if ty `elem` [ #{const GDK_SCROLL} ] then do
        (modif ::#gtk2hs_type guint)    <- #{peek GdkEventScroll, state} ptr
        return (toFlags (fromIntegral (modif .&. mask)))
      else if ty `elem` [ #{const GDK_MOTION_NOTIFY} ] then do
        (modif ::#gtk2hs_type guint)    <- #{peek GdkEventMotion, state} ptr
        return (toFlags (fromIntegral (modif .&. mask)))
      else if ty `elem` [ #{const GDK_ENTER_NOTIFY},
                          #{const GDK_LEAVE_NOTIFY}] then do
        (modif ::#gtk2hs_type guint)    <- #{peek GdkEventCrossing, state} ptr
        return (toFlags (fromIntegral (modif .&. mask)))
      else error ("eventModifiers: none for event type "++show ty)

class HasTime a
instance HasTime EKey
instance HasTime EButton
instance HasTime EScroll
instance HasTime EMotion
instance HasTime ECrossing
instance HasTime EProperty
instance HasTime EProximity
#if GTK_CHECK_VERSION(2,6,0)
instance HasTime EOwnerChange
#endif

-- | The time (in milliseconds) when an event happened. This is used mostly
-- for ordering events and responses to events.
--
type TimeStamp = Word32
-- TODO: make this a newtype

-- | Represents the current time, and can be used anywhere a time is expected.
currentTime :: TimeStamp
currentTime = #{const GDK_CURRENT_TIME}

-- | Query the time when the event occurred.
eventTime :: HasTime t => EventM t TimeStamp
eventTime = do
  ptr <- ask
  liftIO $ do
    (ty :: #{gtk2hs_type GdkEventType}) <- peek (castPtr ptr)
    if ty `elem` [ #{const GDK_KEY_PRESS},
                   #{const GDK_KEY_RELEASE}] then do
        (time :: #gtk2hs_type guint32) <- #{peek GdkEventKey, time} ptr
        return (fromIntegral time)
      else if ty `elem` [ #{const GDK_BUTTON_PRESS},
                   #{const GDK_2BUTTON_PRESS},
                   #{const GDK_3BUTTON_PRESS},
                   #{const GDK_BUTTON_RELEASE}] then do
        (time :: #gtk2hs_type guint32) <- #{peek GdkEventButton, time} ptr
        return (fromIntegral time)
      else if ty `elem` [ #{const GDK_SCROLL} ] then do
        (time :: #gtk2hs_type guint32) <- #{peek GdkEventScroll, time} ptr
        return (fromIntegral time)
      else if ty `elem` [ #{const GDK_MOTION_NOTIFY} ] then do
        (time :: #gtk2hs_type guint32) <- #{peek GdkEventMotion, time} ptr
        return (fromIntegral time)
      else if ty `elem` [ #{const GDK_ENTER_NOTIFY},
                          #{const GDK_LEAVE_NOTIFY}] then do
        (time :: #gtk2hs_type guint32) <- #{peek GdkEventCrossing, time} ptr
        return (fromIntegral time)
      else if ty `elem` [ #{const GDK_PROPERTY_NOTIFY} ] then do
        (time :: #gtk2hs_type guint32) <- #{peek GdkEventProperty, time} ptr
        return (fromIntegral time)
      else if ty `elem` [ #{const GDK_PROXIMITY_IN},
                          #{const GDK_PROXIMITY_OUT}] then do
        (time :: #gtk2hs_type guint32) <- #{peek GdkEventProximity, time} ptr
        return (fromIntegral time)
#if GTK_CHECK_VERSION(2,6,0)
      else if ty `elem` [ #{const GDK_OWNER_CHANGE} ] then do
        (time :: #gtk2hs_type guint32) <- #{peek GdkEventOwnerChange, time} ptr
        return (fromIntegral time)
#endif
      else error ("eventModifiers: none for event type "++show ty)

-- | The key value. See 'Graphics.UI.Gtk.Gdk.Keys.KeyVal'.
eventKeyVal :: EventM EKey KeyVal
eventKeyVal = ask >>= \ptr -> liftIO $ liftM fromIntegral
  (#{peek GdkEventKey, keyval} ptr :: IO #{gtk2hs_type guint})

-- | The key value as a string. See 'Graphics.UI.Gtk.Gdk.Keys.KeyVal'.
eventKeyName :: EventM EKey DefaultGlibString
eventKeyName = liftM keyName $ eventKeyVal

-- | The hardware key code.
eventHardwareKeycode :: EventM EKey KeyCode
eventHardwareKeycode = ask >>= \ptr -> liftIO $ liftM fromIntegral
  (#{peek GdkEventKey, hardware_keycode} ptr :: IO #{gtk2hs_type guint16})

-- | The keyboard group.
eventKeyboardGroup :: EventM EKey Word8
eventKeyboardGroup = ask >>= \ptr -> liftIO $ liftM fromIntegral
  (#{peek GdkEventKey, group} ptr :: IO #{gtk2hs_type guint8})

-- | Query the mouse buttons.
eventButton :: EventM EButton MouseButton
eventButton = ask >>= \ptr -> liftIO $ liftM (toEnum . fromIntegral)
  (#{peek GdkEventButton, button} ptr :: IO #{gtk2hs_type guint})

--- | Query the mouse click.
eventClick :: EventM EButton Click
eventClick = do
  ptr <- ask
  liftIO $ do
    (ty :: #{gtk2hs_type GdkEventType}) <- peek (castPtr ptr)
    case ty of
      #{const GDK_BUTTON_PRESS} -> return SingleClick
      #{const GDK_2BUTTON_PRESS} -> return DoubleClick
      #{const GDK_3BUTTON_PRESS} -> return TripleClick
      #{const GDK_BUTTON_RELEASE} -> return ReleaseClick
      _ -> error ("eventClick: non for event type "++show ty)

-- | Query the direction of scrolling.
eventScrollDirection :: EventM EScroll ScrollDirection
eventScrollDirection = ask >>= \ptr -> liftIO $ liftM (toEnum . fromIntegral)
  (#{peek GdkEventScroll, direction} ptr :: IO #{gtk2hs_type GdkScrollDirection})

-- | Check if the motion event is only a hint rather than the full mouse
--   movement information.
eventIsHint :: EventM EMotion Bool
eventIsHint = ask >>= \ptr -> liftIO $ liftM toBool
  (#{peek GdkEventMotion, is_hint} ptr :: IO #{gtk2hs_type gint16})

#if GTK_CHECK_VERSION(2,12,0)
-- | Request more motion notifies if this event is a motion notify hint event.
--
-- This action should be used instead of 'drawWindowGetPointer' to request
-- further motion notifies, because it also works for extension events where
-- motion notifies are provided for devices other than the core pointer.
--
-- Coordinate extraction, processing and requesting more motion events from a
-- 'motionNotifyEvent' usually works like this:
--
-- > on widget motionNotifyEvent $ do
-- >   (x, y) <- eventCoordinates
-- >   -- handle the x,y motion:
-- >   ...
-- >   -- finally, notify that we are ready to get more motion events:
-- >   eventRequestMotions
--
eventRequestMotions :: EventM EMotion ()
eventRequestMotions = ask >>= \ptr -> liftIO $
  gdk_event_request_motions ptr

foreign import ccall "gdk_event_request_motions"
  gdk_event_request_motions :: Ptr EMotion -> IO ()
#endif

-- | Query a bounding box of the region that needs to be updated.
eventArea :: EventM EExpose Rectangle
eventArea = ask >>= \ptr -> liftIO $
  (#{peek GdkEventExpose, area} ptr :: IO Rectangle)

#if GTK_MAJOR_VERSION < 3
-- | Query the region that needs to be updated.
-- Removed in Gtk3.
eventRegion :: EventM EExpose Region
eventRegion = ask >>= \ptr -> liftIO $ do
  (reg_   :: Ptr Region)        <- #{peek GdkEventExpose, region} ptr
  reg_ <- gdk_region_copy reg_
  makeNewRegion reg_

foreign import ccall "gdk_region_copy"
  gdk_region_copy :: Ptr Region -> IO (Ptr Region)
#endif

-- | Get the visibility status of a window.
eventVisibilityState :: EventM EVisibility VisibilityState
eventVisibilityState = ask >>= \ptr -> liftIO $ liftM (toEnum . fromIntegral)
  (#{peek GdkEventVisibility, state} ptr :: IO #{gtk2hs_type GdkVisibilityState})

-- | Get the mode of the mouse cursor crossing a window.
eventCrossingMode :: EventM ECrossing CrossingMode
eventCrossingMode = ask >>= \ptr -> liftIO $ liftM (toEnum . fromIntegral)
  (#{peek GdkEventCrossing, mode} ptr :: IO #{gtk2hs_type GdkCrossingMode})

-- | Get the notify type of the mouse cursor crossing a window.
eventNotifyType :: EventM ECrossing NotifyType
eventNotifyType = ask >>= \ptr -> liftIO $ liftM (toEnum . fromIntegral)
  (#{peek GdkEventCrossing, detail} ptr :: IO #{gtk2hs_type GdkNotifyType})

-- | Query if the window has the focus or is an inferior window.
eventCrossingFocus :: EventM ECrossing Bool
eventCrossingFocus = ask >>= \ptr -> liftIO $ liftM toBool
  (#{peek GdkEventCrossing, focus} ptr :: IO #{gtk2hs_type gboolean})

-- | Query if a window gained focus (@True@) or lost the focus (@False@).
eventFocusIn :: EventM EFocus Bool
eventFocusIn = ask >>= \ptr -> liftIO $ liftM toBool
  (#{peek GdkEventFocus, in} ptr :: IO #{gtk2hs_type gint16})

-- | Get the @(x,y)@ position of the window within the parent window.
eventPosition :: EventM EConfigure (Int,Int)
eventPosition = ask >>= \ptr -> liftIO $ do
  (x :: #{gtk2hs_type gint})    <- #{peek GdkEventConfigure, x} ptr
  (y :: #{gtk2hs_type gint})    <- #{peek GdkEventConfigure, y} ptr
  return (fromIntegral x, fromIntegral y)

-- | Get the new size of the window as @(width,height)@.
eventSize :: EventM EConfigure (Int,Int)
eventSize = ask >>= \ptr -> liftIO $ do
  (x :: #{gtk2hs_type gint})    <- #{peek GdkEventConfigure, width} ptr
  (y :: #{gtk2hs_type gint})    <- #{peek GdkEventConfigure, height} ptr
  return (fromIntegral x, fromIntegral y)

eventProperty :: EventM EProperty Atom
eventProperty = ask >>= \ptr -> liftIO $ liftM Atom
  (#{peek GdkEventProperty, atom} ptr :: IO (Ptr ()))

-- | Query which window state bits have changed.
eventWindowStateChanged :: EventM EWindowState [WindowState]
eventWindowStateChanged = ask >>= \ptr -> liftIO $ liftM (toFlags . fromIntegral)
  (#{peek GdkEventWindowState, changed_mask} ptr :: IO #{gtk2hs_type GdkWindowState})

-- | Query the new window state.
eventWindowState :: EventM EWindowState [WindowState]
eventWindowState = ask >>= \ptr -> liftIO $ liftM (toFlags . fromIntegral)
  (#{peek GdkEventWindowState, new_window_state} ptr :: IO #{gtk2hs_type GdkWindowState})

#if GTK_CHECK_VERSION(2,6,0)
-- | Query why a seleciton changed its owner.
eventChangeReason :: EventM EOwnerChange OwnerChange
eventChangeReason = ask >>= \ptr -> liftIO $ liftM (toEnum . fromIntegral)
  (#{peek GdkEventOwnerChange, reason} ptr :: IO #{gtk2hs_type GdkOwnerChange})

-- | Query what selection changed its owner.
eventSelection :: EventM EOwnerChange SelectionTag
eventSelection = ask >>= \ptr -> liftIO $ liftM Atom
  (#{peek GdkEventOwnerChange, selection} ptr :: IO (Ptr ()))

-- | Query the time when the selection was taken over.
eventSelectionTime :: EventM EOwnerChange TimeStamp
eventSelectionTime = ask >>= \ptr -> liftIO $ liftM fromIntegral
  (#{peek GdkEventOwnerChange, selection_time} ptr :: IO (#{gtk2hs_type guint32}))
#endif

#if GTK_CHECK_VERSION(2,8,0)
-- | Check if a keyboard (@True@) or a mouse pointer grap (@False@) was
--   broken.
eventKeyboardGrab :: EventM EGrabBroken Bool
eventKeyboardGrab = ask >>= \ptr -> liftIO $ liftM toBool
  (#{peek GdkEventGrabBroken, keyboard} ptr :: IO #{gtk2hs_type gboolean})

-- | Check if a grab was broken implicitly.
eventImplicit :: EventM EGrabBroken Bool
eventImplicit = ask >>= \ptr -> liftIO $ liftM toBool
  (#{peek GdkEventGrabBroken, implicit} ptr :: IO #{gtk2hs_type gboolean})

-- | Get the new window that owns the grab or @Nothing@ if the window
--   is not part of this application.
eventGrabWindow :: EventM EGrabBroken (Maybe DrawWindow)
eventGrabWindow = do
  ptr <- ask
  liftIO $ maybeNull (makeNewGObject mkDrawWindow) (#{peek GdkEventAny, window} ptr)
#endif


-- | Execute an event handler and assume it handled the event unless it
--   threw a pattern match exception or calls mzero (e.g. via guard).
tryEvent :: EventM any () -> EventM any Bool
tryEvent act = do
  ptr <- ask
  liftIO $ (runReaderT (act >> return True) ptr)
#if __GLASGOW_HASKELL__ >= 610
    `catches` [ Handler (\ (PatternMatchFail _) -> return False)
              , Handler (\ e -> if isUserError e &&
                                   ("Pattern" `isPrefixOf` ioeGetErrorString e ||
                                    "mzero" == ioeGetErrorString e)
                                then return False
                                else throw e) ]
#else
    `catch` (\e -> case e of
               IOException e
                 | "user error (Pattern" `isPrefixOf` show e ->
                   return False
                 | "user error (mzero" `isPrefixOf` show e ->
                   return False
               PatternMatchFail _ -> return False
               _ -> throw e)
#endif


-- | Explicitly stop the handling of an event. This function should only be
--   called inside a handler that is wrapped with 'tryEvent'. (It merely
--   throws a bogus pattern matching error which 'tryEvent' interprets as if
--   the handler does not handle the event.)
stopEvent :: EventM any ()
stopEvent =
  liftIO $ throw (PatternMatchFail "EventM.stopEvent called explicitly")
