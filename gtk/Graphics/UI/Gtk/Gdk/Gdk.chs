{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Gdk
--
--  Author : Jens Petersen <petersen@haskell.org>
--
--  Created: 6 June 2003
--
--  Copyright (C) 2003-2005 Jens-Ulrik Holger Petersen
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
-- Gdk general functions.
--
module Graphics.UI.Gtk.Gdk.Gdk (
  flush,
  screenWidth,
  screenHeight,
  screenWidthMM,
  screenHeightMM,
  GrabStatus(..),
  pointerGrab,
  pointerUngrab,
  pointerIsGrabbed,
  keyboardGrab,
  keyboardUngrab,
  beep
#if GTK_MAJOR_VERSION >= 3
 ,RGBA(..)
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.Flags                (fromFlags)
import System.Glib.FFI
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Gdk.Cursor#} (Cursor(..))
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Gdk.Enums        (EventMask, GrabStatus(..))
#if GTK_MAJOR_VERSION >= 3
import Graphics.UI.Gtk.General.Structs (RGBA(..))
#endif

{#context lib="gdk" prefix ="gdk"#}

-- | Emits a short beep.
--
beep :: IO ()
beep = {#call beep#}

-- | Flushes the X output buffer and waits until all requests have been
-- processed by the server. This is rarely needed by applications.
--
flush :: IO ()
flush = {#call flush#}

-- | Returns the width of the default screen in pixels.
--
screenWidth :: IO Int
screenWidth = liftM fromIntegral $ {#call screen_width#}

-- | Returns the height of the default screen in pixels.
--
screenHeight :: IO Int
screenHeight = liftM fromIntegral $ {#call screen_height#}

-- | Returns the width of the default screen in millimeters. Note that on many
-- X servers this value will not be correct.
--
screenWidthMM :: IO Int
screenWidthMM = liftM fromIntegral $ {#call screen_width_mm#}

-- | Returns the height of the default screen in millimeters. Note that on many
-- X servers this value will not be correct.
--
screenHeightMM :: IO Int
screenHeightMM = liftM fromIntegral $ {#call screen_height_mm#}

-- | Grabs the pointer (usually a mouse) so that all events are passed to this
-- application until the pointer is ungrabbed with 'pointerUngrab', or the grab
-- window becomes unviewable. This overrides any previous pointer grab by this
-- client.
--
-- Pointer grabs are used for operations which need complete control over mouse
-- events, even if the mouse leaves the application. For example in GTK+ it is
-- used for Drag and Drop, for dragging the handle in the GtkHPaned and
-- GtkVPaned widgets, and for resizing columns in GtkCList widgets.
--
-- Note that if the event mask of an X window has selected both button press
-- and button release events, then a button press event will cause an automatic
-- pointer grab until the button is released. X does this automatically since
-- most applications expect to receive button press and release events in
-- pairs. It is equivalent to a pointer grab on the window with @owner_events@
-- set to @True@.
--
-- If you set up anything at the time you take the grab that needs to be
-- cleaned up when the grab ends, you should handle the GdkEventGrabBroken
-- events that are emitted when the grab ends unvoluntarily.
--
pointerGrab :: (DrawWindowClass window, DrawWindowClass confine_to) =>
    window -- ^ @window@ - the 'DrawWindow' which will own the grab (the grab
           -- window).
  -> Bool  -- ^ @owner_events@ - if @False@ then all pointer events are
           -- reported with respect to @window@ and are only reported if
           -- selected by @event_mask@. If @True@ then pointer events for this
           -- application are reported as normal, but pointer events outside
           -- this application are reported with respect to @window@ and only
           -- if selected by @event_mask@. In either mode, unreported events
           -- are discarded.
  -> [EventMask] -- ^ @event_mask@ - specifies the event mask, which is used in
                 -- accordance with @owner_events@. Note that only pointer
                 -- events (i.e. button and motion events) may be selected.
  -> Maybe confine_to -- ^ @confine_to@ If supplied, the pointer will be
                      -- confined to this window during the grab. If the
                      -- pointer is outside @confine_to@, it will automatically
                      -- be moved to the closest edge of @confine_to@ and enter
                      -- and leave events will be generated as necessary.
  -> Maybe Cursor -- ^ @cursor@ - the cursor to display while the grab is
                  -- active. If this is @Nothing@ then the normal cursors are
                  -- used for @window@ and its descendants, and the cursor for
                  -- @window@ is used for all other windows.
  -> TimeStamp -- ^ @time@ - the timestamp of the event which led to this
               -- pointer grab. This usually comes from an 'Event', though
               -- 'currentTime' can be used if the time isn't known.
  -> IO GrabStatus -- ^ @Returns@ - 'GrabSuccess' if the grab was successful.
pointerGrab window owner_events event_mask mbConfine_to mbCursor time =
  liftM (toEnum . fromIntegral) $
  {#call pointer_grab#}
    (toDrawWindow window)
    (fromBool owner_events)
    (fromIntegral $ fromFlags event_mask)
    (maybe (DrawWindow nullForeignPtr) toDrawWindow mbConfine_to)
    (maybe (Cursor nullForeignPtr) id mbCursor)
    (fromIntegral time)

-- | Ungrabs the pointer on the default display, if it is grabbed by this
-- application.
--
pointerUngrab ::
    TimeStamp -- ^ @time@ - a timestamp from an 'Event', or 'currentTime' if no
              -- timestamp is available.
  -> IO ()
pointerUngrab time = {#call pointer_ungrab#} (fromIntegral time)

-- | Returns @True@ if the pointer on the default display is currently grabbed
-- by this application.
--
-- Note that this does not take the inmplicit pointer grab on button presses
-- into account.
--
pointerIsGrabbed :: IO Bool
pointerIsGrabbed = liftM toBool $ {#call pointer_is_grabbed#}

-- | Grabs the keyboard so that all events are passed to this application until
-- the keyboard is ungrabbed with 'keyboardUngrab'. This overrides any previous
-- keyboard grab by this client.
--
-- If you set up anything at the time you take the grab that needs to be
-- cleaned up when the grab ends, you should handle the GdkEventGrabBroken
-- events that are emitted when the grab ends unvoluntarily.
keyboardGrab :: (DrawWindowClass window) =>
    window -- ^ @window@ - the 'DrawWindow' which will own the grab (the grab
           -- window).
  -> Bool  -- ^ @owner_events@ - if @False@ then all keyboard events are
           -- reported with respect to @window@. If @True@ then keyboard events
           -- for this application are reported as normal, but keyboard events
           -- outside this application are reported with respect to @window@.
           -- Both key press and key release events are always reported,
           -- independant of the event mask set by the application.
  -> TimeStamp -- ^ @time@ - a timestamp from an 'Event', or 'currentTime' if
               -- no timestamp is available.
  -> IO GrabStatus -- ^ @Returns@ - 'GrabSuccess' if the grab was successful.
keyboardGrab window owner_events time =
  liftM (toEnum . fromIntegral) $
  {#call keyboard_grab#}
    (toDrawWindow window)
    (fromBool owner_events)
    (fromIntegral time)

-- | Ungrabs the keyboard on the default display, if it is grabbed by this
-- application.
keyboardUngrab ::
    TimeStamp -- ^ @time@ - a timestamp from an 'Event', or 'currentTime' if no
              -- timestamp is available.
  -> IO ()
keyboardUngrab time = {#call keyboard_ungrab#} (fromIntegral time)

