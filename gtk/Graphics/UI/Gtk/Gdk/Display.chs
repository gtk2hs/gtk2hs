{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Display - a description of a keyboard/mouse/monitors combination
--
--  Author : Axel Simon
--
--  Created: 22 October 2009
--
--  Copyright (C) 2009 Axel Simon
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
-- Controls the keyboard\/mouse\/monitors combination.
--
-- * Module available since Gdk version 2.2
--
module Graphics.UI.Gtk.Gdk.Display (

-- * Detail
--
-- | 'Display' objects purpose are two fold:
--
-- * To grab\/ungrab keyboard focus and mouse pointer
--
-- * To manage and provide information about the 'Screen'(s) available for
-- this 'Display'
--
-- 'Display' objects are the GDK representation of the X Display which can
-- be described as /a workstation consisting of a keyboard a pointing device
-- (such as a mouse) and one or more screens/. It is used to open and keep
-- track of various 'Screen' objects currently instanciated by the application.
-- It is also used to grab and release the keyboard and the mouse pointer.

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----Display
-- @

#if GTK_CHECK_VERSION(2,2,0)
-- * Types
  Display,
  DisplayClass,
  castToDisplay, gTypeDisplay,
  toDisplay,

-- * Methods
  displayOpen,
  displayGetDefault,
  displayGetName,
  displayGetNScreens,
  displayGetScreen,
  displayGetDefaultScreen,
  displayPointerUngrab,
  displayKeyboardUngrab,
  displayPointerIsGrabbed,
  displayBeep,
  displaySync,
#if GTK_CHECK_VERSION(2,4,0)
  displayFlush,
#endif
  displayClose,
  displayListDevices,
  displaySetDoubleClickTime,
#if GTK_CHECK_VERSION(2,4,0)
  displaySetDoubleClickDistance,
#endif
  displayGetPointer,
  displayGetWindowAtPointer,
#if GTK_CHECK_VERSION(2,8,0)
  displayWarpPointer,
#endif
#if GTK_CHECK_VERSION(2,4,0)
  displaySupportsCursorColor,
  displaySupportsCursorAlpha,
  displayGetDefaultCursorSize,
  displayGetMaximalCursorSize,
  displayGetDefaultGroup,
#if GTK_CHECK_VERSION(2,6,0)
  displaySupportsSelectionNotification,
  displayRequestSelectionNotification,
  displaySupportsClipboardPersistence,
  displayStoreClipboard,
#if GTK_CHECK_VERSION(2,10,0)
  displaySupportsShapes,
  displaySupportsInputShapes,
#if GTK_CHECK_VERSION(2,12,0)
  displaySupportsComposite,
#endif
#endif
#endif
#endif

-- * Signals
  displayClosed,

#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Flags
import System.Glib.GList
{#import Graphics.UI.Gtk.Types#}
import Graphics.UI.Gtk.Signals
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.General.DNDTypes (SelectionTag, TargetTag, Atom(..))

{# context lib="gdk" prefix="gdk" #}

#if GTK_CHECK_VERSION(2,2,0)
--------------------
-- Methods

-- | Opens a display.
--
displayOpen :: GlibString string
 => string     -- ^ @displayName@ - the name of the display to open
 -> IO (Maybe Display)
               -- ^ returns a 'Display', or @Nothing@ if the display
               -- could not be opened.
displayOpen displayName =
  maybeNull (wrapNewGObject mkDisplay) $
  withUTFString displayName $ \displayNamePtr ->
  {# call gdk_display_open #}
    displayNamePtr

-- | Gets the default 'Display'. This is a convenience function for
-- @displayManagerGetDefaultDisplay displayManagerGet@.
--
displayGetDefault ::
    IO (Maybe Display)
               -- ^ returns a 'Display', or @Nothing@ if there is no
               -- default display.
displayGetDefault =
  maybeNull (makeNewGObject mkDisplay) $
  {# call gdk_display_get_default #}

-- | Gets the name of the display.
--
displayGetName :: GlibString string => Display
 -> IO string -- ^ returns a string representing the display name
displayGetName self =
  {# call gdk_display_get_name #}
    self
  >>= peekUTFString

-- | Gets the number of screen managed by the @display@.
--
displayGetNScreens :: Display
 -> IO Int -- ^ returns number of screens.
displayGetNScreens self =
  liftM fromIntegral $
  {# call gdk_display_get_n_screens #}
    self

-- | Returns a screen object for one of the screens of the display.
--
displayGetScreen :: Display
 -> Int       -- ^ @screenNum@ - the screen number
 -> IO Screen -- ^ returns the 'Screen' object
displayGetScreen self screenNum =
  makeNewGObject mkScreen $
  {# call gdk_display_get_screen #}
    self
    (fromIntegral screenNum)

-- | Get the default 'Screen' for @display@.
--
displayGetDefaultScreen :: Display
 -> IO Screen -- ^ returns the default 'Screen' object for @display@
displayGetDefaultScreen self =
  makeNewGObject mkScreen $
  {# call gdk_display_get_default_screen #}
    self

-- | Release any pointer grab.
--
displayPointerUngrab :: Display
 -> TimeStamp -- ^ @time@ - a timestap (e.g. 'currentTime').
 -> IO ()
displayPointerUngrab self time =
  {# call gdk_display_pointer_ungrab #}
    self
    (fromIntegral time)

-- | Release any keyboard grab
--
displayKeyboardUngrab :: Display
 -> TimeStamp -- ^ @time@ - a timestap (e.g 'currentTime').
 -> IO ()
displayKeyboardUngrab self time =
  {# call gdk_display_keyboard_ungrab #}
    self
    (fromIntegral time)

-- | Test if the pointer is grabbed.
--
displayPointerIsGrabbed :: Display
 -> IO Bool -- ^ returns @True@ if an active X pointer grab is in effect
displayPointerIsGrabbed self =
  liftM toBool $
  {# call gdk_display_pointer_is_grabbed #}
    self

-- | Emits a short beep on @display@
--
displayBeep :: Display -> IO ()
displayBeep self =
  {# call gdk_display_beep #}
    self

-- | Flushes any requests queued for the windowing system and waits until all
-- requests have been handled. This is often used for making sure that the
-- display is synchronized with the current state of the program. Calling
-- 'displaySync' before 'errorTrapPop' makes sure that any errors generated
-- from earlier requests are handled before the error trap is removed.
--
-- This is most useful for X11. On windowing systems where requests are
-- handled synchronously, this function will do nothing.
--
displaySync :: Display -> IO ()
displaySync self =
  {# call gdk_display_sync #}
    self

#if GTK_CHECK_VERSION(2,4,0)
-- | Flushes any requests queued for the windowing system; this happens
-- automatically when the main loop blocks waiting for new events, but if your
-- application is drawing without returning control to the main loop, you may
-- need to call this function explicitely. A common case where this function
-- needs to be called is when an application is executing drawing commands from
-- a thread other than the thread where the main loop is running.
--
-- This is most useful for X11. On windowing systems where requests are
-- handled synchronously, this function will do nothing.
--
-- * Available since Gdk version 2.4
--
displayFlush :: Display -> IO ()
displayFlush self =
  {# call gdk_display_flush #}
    self
#endif

-- | Closes the connection to the windowing system for the given display, and
-- cleans up associated resources.
--
displayClose :: Display -> IO ()
displayClose self =
  {# call gdk_display_close #}
    self

-- | Returns the list of available input devices attached to @display@.
--
displayListDevices :: Display
 -> IO [Device] -- ^ returns a list of 'Device'
displayListDevices self =
  {# call gdk_display_list_devices #}
    self
  >>= readGList
  >>= mapM (makeNewGObject mkDevice . return)


-- | Sets the double click time (two clicks within this time interval count as
-- a double click and result in an 'eventButton' where 'eventClick' is
-- 'DoubleClick'). Applications should /not/ set this, it is a global
-- user-configured setting.
--
displaySetDoubleClickTime :: Display
 -> Int -- ^ @msec@ - double click time in milliseconds (thousandths of a
        -- second)
 -> IO ()
displaySetDoubleClickTime self msec =
  {# call gdk_display_set_double_click_time #}
    self
    (fromIntegral msec)

#if GTK_CHECK_VERSION(2,4,0)
-- | Sets the double click distance (two clicks within this distance count as
-- a double click and result in an 'eventButton' where 'eventClick' is
-- 'DoubleClick'). See also 'displaySetDoubleClickTime'. Applications should
-- /not/ set this, it is a global user-configured setting.
--
-- * Available since Gdk version 2.4
--
displaySetDoubleClickDistance :: Display
 -> Int -- ^ @distance@ - distance in pixels
 -> IO ()
displaySetDoubleClickDistance self distance =
  {# call gdk_display_set_double_click_distance #}
    self
    (fromIntegral distance)
#endif

-- | Gets the current location of the pointer and the current modifier mask
-- for a given display.
--
displayGetPointer :: Display
 -> IO (Screen, [Modifier], Int, Int)
  -- ^ @(s, m, x, y)@ - the screen @s@, the modifier mask @m@ and the @x@ and
  -- @y@ coordinates of the pointer
displayGetPointer self =
  alloca $ \sPtr ->
  alloca $ \xPtr ->
  alloca $ \yPtr ->
  alloca $ \mPtr ->
  {# call gdk_display_get_pointer #}
    self
    (castPtr sPtr)
    xPtr
    yPtr
    mPtr >>
  makeNewGObject mkScreen (peek sPtr) >>= \s ->
  peek xPtr >>= \x ->
  peek yPtr >>= \y ->
  peek mPtr >>= \m ->
  return (s, toFlags (fromIntegral m), fromIntegral x, fromIntegral y)

-- | Obtains the window underneath the mouse pointer, returning the location
-- of the pointer in that window in @winX@, @winY@ for @screen@. Returns
-- @Nothing@ if
-- the window under the mouse pointer is not known to GDK (for example, belongs
-- to another application).
--
displayGetWindowAtPointer :: Display
 -> IO (Maybe (DrawWindow, Int, Int))
  -- ^ @(screen, winX, winY)@  returns the window under the mouse
  -- pointer, or @Nothing@. The @winX@  and @winY@ denote the pointer location
  -- relative to the window origin
displayGetWindowAtPointer self =
  alloca $ \winXPtr ->
  alloca $ \winYPtr -> do
  wPtr <- {# call gdk_display_get_window_at_pointer #}
    self
    winXPtr
    winYPtr
  if wPtr==nullPtr then return Nothing else
    peek winXPtr >>= \winX ->
    peek winYPtr >>= \winY ->
    makeNewGObject mkDrawWindow (return wPtr) >>= \win ->
    return (Just (win, fromIntegral winX, fromIntegral winY))

{-  not worth the trouble

-- | This function allows for hooking into the operation of getting the
-- current location of the pointer on a particular display. This is only useful
-- for such low-level tools as an event recorder. Applications should never
-- have any reason to use this facility.
--
displaySetPointerHooks :: Display
 -> {-const-GdkDisplayPointerHooks*-} -- ^ @newHooks@ - a table of pointers to
                                      -- functions for getting quantities
                                      -- related to the current pointer
                                      -- position, or {@NULL@, FIXME: this
                                      -- should probably be converted to a
                                      -- Maybe data type} to restore the
                                      -- default table.
 -> IO {-GdkDisplayPointerHooks*-}    -- ^ returns the previous pointer hook
                                      -- table
displaySetPointerHooks self newHooks =
  {# call gdk_display_set_pointer_hooks #}
    self
    {-newHooks-}
-}

#if GTK_CHECK_VERSION(2,8,0)
-- | Moves the pointer of @display@ to the point @x@,@y@ on the screen
-- @screen@, unless the pointer is confined to a window by a grab, in which
-- case it will be moved as far as allowed by the grab. Warping the pointer
-- creates events as if the user had moved the mouse instantaneously to the
-- destination.
--
-- Note that the pointer should normally be under the control of the user.
-- This function was added to cover some rare use cases like keyboard
-- navigation support for the color picker in the 'ColorSelectionDialog'.
--
-- * Available since Gdk version 2.8
--
displayWarpPointer :: Display
 -> Screen -- ^ @screen@ - the screen of @display@ to warp the pointer to
 -> Int    -- ^ @x@ - the x coordinate of the destination
 -> Int    -- ^ @y@ - the y coordinate of the destination
 -> IO ()
displayWarpPointer self screen x y =
  {# call gdk_display_warp_pointer #}
    self
    screen
    (fromIntegral x)
    (fromIntegral y)
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Returns @True@ if multicolored cursors are supported on @display@.
-- Otherwise, cursors have only a forground and a background color.
--
-- * Available since Gdk version 2.4
--
displaySupportsCursorColor :: Display
 -> IO Bool -- ^ returns whether cursors can have multiple colors.
displaySupportsCursorColor self =
  liftM toBool $
  {# call gdk_display_supports_cursor_color #}
    self

-- | Returns @True@ if cursors can use an 8bit alpha channel on @display@.
-- Otherwise, cursors are restricted to bilevel alpha (i.e. a mask).
--
-- * Available since Gdk version 2.4
--
displaySupportsCursorAlpha :: Display
 -> IO Bool -- ^ returns whether cursors can have alpha channels.
displaySupportsCursorAlpha self =
  liftM toBool $
  {# call gdk_display_supports_cursor_alpha #}
    self

-- | Returns the default size to use for cursors on @display@.
--
-- * Available since Gdk version 2.4
--
displayGetDefaultCursorSize :: Display
 -> IO Int -- ^ returns the default cursor size.
displayGetDefaultCursorSize self =
  liftM fromIntegral $
  {# call gdk_display_get_default_cursor_size #}
    self

-- | Gets the maximal size to use for cursors on @display@.
--
-- * Available since Gdk version 2.4
--
displayGetMaximalCursorSize :: Display
 -> IO (Int, Int) -- ^ @(width, height)@
                  -- maximal @width@ and @height@  of the cursor
displayGetMaximalCursorSize self =
  alloca $ \widthPtr ->
  alloca $ \heightPtr ->
  {# call gdk_display_get_maximal_cursor_size #}
    self
    widthPtr
    heightPtr >>
  peek widthPtr >>= \width ->
  peek heightPtr >>= \height ->
  return (fromIntegral width, fromIntegral height)

-- | Returns the default group leader window for all toplevel windows on
-- @display@. This window is implicitly created by GDK. See 'windowSetGroup'.
--
-- * Available since Gdk version 2.4
--
displayGetDefaultGroup :: Display
 -> IO DrawWindow -- ^ returns The default group leader window for @display@
displayGetDefaultGroup self =
  makeNewGObject mkDrawWindow $
  {# call gdk_display_get_default_group #}
    self

#if GTK_CHECK_VERSION(2,6,0)
-- | Returns whether 'EOwnerChange' events will be
-- sent when the owner of a selection changes.
--
-- * Available since Gdk version 2.6
--
displaySupportsSelectionNotification :: Display
 -> IO Bool -- ^ returns whether 'EOwnerChange'
            -- events will be sent.
displaySupportsSelectionNotification self =
  liftM toBool $
  {# call gdk_display_supports_selection_notification #}
    self

-- | Request 'EOwnerChange' events for ownership
-- changes of the selection named by the given atom.
--
-- * Available since Gdk version 2.6
--
displayRequestSelectionNotification :: Display
 -> SelectionTag -- ^ @selection@ - the 'Atom' naming
                -- the selection for which ownership change notification is
                -- requested
 -> IO Bool     -- ^ returns whether 'EOwnerChange'
                -- events will be sent.
displayRequestSelectionNotification self (Atom selection) =
  liftM toBool $
  {# call gdk_display_request_selection_notification #}
    self
    selection

-- | Returns whether the speicifed display supports clipboard persistance;
-- i.e. if it's possible to store the clipboard data after an application has
-- quit. On X11 this checks if a clipboard daemon is running.
--
-- * Available since Gdk version 2.6
--
displaySupportsClipboardPersistence :: Display
 -> IO Bool -- ^ returns @True@ if the display supports clipboard persistance.
displaySupportsClipboardPersistence self =
  liftM toBool $
  {# call gdk_display_supports_clipboard_persistence #}
    self

-- | Issues a request to the clipboard manager to store the clipboard data. On
-- X11, this is a special program that works according to the freedesktop
-- clipboard specification, available at
-- http:\/\/www.freedesktop.org\/Standards\/clipboard-manager-spec.
--
-- * Available since Gdk version 2.6
--
displayStoreClipboard :: Display
 -> DrawWindow         -- ^ @clipboardWindow@ - a 'DrawWindow' belonging to
                       -- the clipboard owner
 -> Word32             -- ^ @time@ - a timestamp
 -> (Maybe [TargetTag])  -- ^ @targets@ - an array of targets that should be
                         -- saved, or @Nothing@ if all available
                         -- targets should be saved.
 -> IO ()
displayStoreClipboard self clipboardWindow time (Just targets) =
  withArrayLen (map (\(Atom a) -> a) targets) $ \nTargets tPtr ->
  {# call gdk_display_store_clipboard #}
    self
    clipboardWindow
    (fromIntegral time)
    tPtr
    (fromIntegral nTargets)
displayStoreClipboard self clipboardWindow time Nothing =
  {# call gdk_display_store_clipboard #}
    self
    clipboardWindow
    (fromIntegral time)
    nullPtr
    0

#if GTK_CHECK_VERSION(2,10,0)
-- | Returns @True@ if 'windowShapeCombineMask' can be used to create shaped
-- windows on @display@.
--
-- * Available since Gdk version 2.10
--
displaySupportsShapes :: Display
 -> IO Bool -- ^ returns @True@ if shaped windows are supported
displaySupportsShapes self =
  liftM toBool $
  {# call gdk_display_supports_shapes #}
    self

-- | Returns @True@ if 'windowInputShapeCombineMask' can be used to modify the
-- input shape of windows on @display@.
--
-- * Available since Gdk version 2.10
--
displaySupportsInputShapes :: Display
 -> IO Bool -- ^ returns @True@ if windows with modified input shape are
            -- supported
displaySupportsInputShapes self =
  liftM toBool $
  {# call gdk_display_supports_input_shapes #}
    self

#if GTK_CHECK_VERSION(2,12,0)
-- | Returns @True@ if 'windowSetComposited' can be used to redirect drawing
-- on the window using compositing.
--
-- Currently this only works on X11 with XComposite and XDamage extensions
-- available.
--
-- * Available since Gdk version 2.12
--
displaySupportsComposite :: Display
 -> IO Bool -- ^ returns @True@ if windows may be composited.
displaySupportsComposite self =
  liftM toBool $
  {# call gdk_display_supports_composite #}
    self
#endif
#endif
#endif
#endif

--------------------
-- Signals

-- | The 'displayClosed' signal is emitted when the connection to the windowing
-- system for @display@ is closed.
--
displayClosed :: DisplayClass self => Signal self (Bool -> IO ())
displayClosed = Signal (connect_BOOL__NONE "closed")

#endif
