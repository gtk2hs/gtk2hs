-- -*-haskell-*-
--  GIMP Toolkit (GTK) Window
--
--  Author : Manuel M. T. Chakravarty, Axel Simon
--
--  Created: 27 April 2001
--
--  Version $Revision: 1.6 $ from $Date: 2005/03/27 11:54:52 $
--
--  Copyright (C) 2001-2005 Manuel M. T. Chakravarty, Axel Simon
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
-- Toplevel which can contain other widgets
--
module Graphics.UI.Gtk.Windows.Window (

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----Window
-- |                                 +----'Dialog'
-- |                                 +----'Plug'
-- @

-- * Types
  Window,
  WindowClass,
  castToWindow,

-- * Constructors
  windowNew,

-- * Methods
  windowSetTitle,
  windowSetResizable,
  windowGetResizable,
--  windowAddAccelGroup, 
--  windowRemoveAccelGroup,
  windowActivateFocus,
  windowActivateDefault,
  windowSetModal,
  windowSetDefaultSize,
--  windowSetGeometryHints,
#ifndef DISABLE_DEPRECATED
  windowSetPolicy,
#endif
  windowSetPosition,
  WindowPosition(..),
  windowSetTransientFor,
  windowSetDestroyWithParent,
  windowGetDestroyWithParent,
  windowIsActive,
  windowHasToplevelFocus,
-- windowListToplevels,
-- windowAddMnemonic,
-- windowRemoveMnemonic,
-- windowSetMnemonicModifier,
  windowPresent,
  windowDeiconify,
  windowIconify,
  windowMaximize,
  windowUnmaximize,
#if GTK_CHECK_VERSION(2,2,0)
  windowFullscreen,
  windowUnfullscreen,
#endif
#if GTK_CHECK_VERSION(2,4,0)
  windowSetKeepAbove,
  windowSetKeepBelow,
#endif
#if GTK_CHECK_VERSION(2,2,0)
  windowSetSkipTaskbarHint,
  windowGetSkipTaskbarHint,
  windowSetSkipPagerHint,
  windowGetSkipPagerHint,
#endif
#if GTK_CHECK_VERSION(2,4,0)
  windowSetAcceptFocus,
  windowGetAcceptFocus,
#endif
#if GTK_CHECK_VERSION(2,6,0)
  windowSetFocusOnMap,
  windowGetFocusOnMap,
#endif
  windowSetDecorated,
-- windowSetDecorationsHint,
  windowSetFrameDimensions,
-- windowSetFunctionHint,
  windowSetRole,
  windowStick,
  windowUnstick,
  windowAddAccelGroup,
  windowRemoveAccelGroup,
  windowSetIcon,
#if GTK_CHECK_VERSION(2,6,0)
  windowSetIconName,
  windowGetIconName,
  windowSetDefaultIconName,
#endif

-- * Properties
  windowResizable,

-- * Signals
  onFrameEvent,
  afterFrameEvent,
  onSetFocus,
  afterSetFocus
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes		(Attr(..))
import Graphics.UI.Gtk.General.Enums	(WindowType(WindowToplevel), WindowPosition(..))
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.Gdk.Events	(Event, marshalEvent)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new top level window.
--
windowNew :: IO Window
windowNew =
  makeNewObject mkWindow $
  liftM (castPtr :: Ptr Widget -> Ptr Window) $
  {# call window_new #}
    ((fromIntegral . fromEnum) WindowToplevel)

--------------------
-- Methods

-- | Sets the title of the 'Window'. The title of a window will be displayed
-- in its title bar; on the X Window System, the title bar is rendered by the
-- window manager, so exactly how the title appears to users may vary according
-- to a user's exact configuration. The title should help a user distinguish
-- this window from other windows they may have open. A good title might
-- include the application name and current document filename, for example.
--
windowSetTitle :: WindowClass self => self -> String -> IO ()
windowSetTitle self title =
  withUTFString title $ \titlePtr ->
  {# call gtk_window_set_title #}
    (toWindow self)
    titlePtr

-- | Sets whether the user can resize a window. Windows are user resizable by
-- default.
--
windowSetResizable :: WindowClass self => self -> Bool -> IO ()
windowSetResizable self resizable =
  {# call window_set_resizable #}
    (toWindow self)
    (fromBool resizable)

-- | Gets the value set by 'windowSetResizable'.
--
windowGetResizable :: WindowClass self => self
 -> IO Bool -- ^ returns @True@ if the user can resize the window
windowGetResizable self =
  liftM toBool $
  {# call unsafe window_get_resizable #}
    (toWindow self)

-- | Activates the current focused widget within the window.
--
windowActivateFocus :: WindowClass self => self
 -> IO Bool -- ^ returns @True@ if a widget got activated.
windowActivateFocus self =
  liftM toBool $
  {# call window_activate_focus #}
    (toWindow self)

-- | Activates the default widget for the window, unless the current focused
-- widget has been configured to receive the default action (see
-- 'ReceivesDefault' in 'WidgetFlags'), in which case the focused widget is
-- activated.
--
windowActivateDefault :: WindowClass self => self
 -> IO Bool -- ^ returns @True@ if a widget got activated.
windowActivateDefault self =
  liftM toBool $
  {# call window_activate_default #}
    (toWindow self)

#ifndef DISABLE_DEPRECATED
{-# DEPRECATED windowSetPolicy "Use windowSetResizable instead." #-}
-- | Sets the window resizing policy.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code. Use 'windowSetResizable' instead.
--
windowSetPolicy :: WindowClass self => self -> Bool -> Bool -> Bool -> IO ()
windowSetPolicy self allowShrink allowGrow autoShrink =
  {# call window_set_policy #}
    (toWindow self)
    (fromBool allowShrink)
    (fromBool allowGrow)
    (fromBool autoShrink)
#endif

-- | Sets a window modal or non-modal. Modal windows prevent interaction with
-- other windows in the same application. To keep modal dialogs on top of main
-- application windows, use 'windowSetTransientFor' to make the dialog
-- transient for the parent; most window managers will then disallow lowering
-- the dialog below the parent.
--
windowSetModal :: WindowClass self => self
 -> Bool  -- ^ @modal@ - whether the window is modal
 -> IO ()
windowSetModal self modal =
  {# call window_set_modal #}
    (toWindow self)
    (fromBool modal)

-- | Sets the default size of a window. If the window's \"natural\" size (its
-- size request) is larger than the default, the default will be ignored. More
-- generally, if the default size does not obey the geometry hints for the
-- window ('windowSetGeometryHints' can be used to set these explicitly), the
-- default size will be clamped to the nearest permitted size.
--
-- Unlike 'widgetSetSizeRequest', which sets a size request for a widget and
-- thus would keep users from shrinking the window, this function only sets the
-- initial size, just as if the user had resized the window themselves. Users
-- can still shrink the window again as they normally would. Setting a default
-- size of -1 means to use the \"natural\" default size (the size request of
-- the window).
--
-- For more control over a window's initial size and how resizing works,
-- investigate 'windowSetGeometryHints'.
--
-- For some uses, 'windowResize' is a more appropriate function.
-- 'windowResize' changes the current size of the window, rather than the size
-- to be used on initial display. 'windowResize' always affects the window
-- itself, not the geometry widget.
--
-- The default size of a window only affects the first time a window is
-- shown; if a window is hidden and re-shown, it will remember the size it had
-- prior to hiding, rather than using the default size.
--
-- Windows can't actually be 0x0 in size, they must be at least 1x1, but
-- passing 0 for @width@ and @height@ is OK, resulting in a 1x1 default size.
--
windowSetDefaultSize :: WindowClass self => self
 -> Int   -- ^ @height@ - height in pixels, or -1 to unset the default height
 -> Int   -- ^ @width@ - width in pixels, or -1 to unset the default width
 -> IO ()
windowSetDefaultSize self height width =
  {# call window_set_default_size #}
    (toWindow self)
    (fromIntegral height)
    (fromIntegral width)

-- | Sets a position constraint for this window. If the old or new constraint
-- is 'WinPosCenterAlways', this will also cause the window to be repositioned
-- to satisfy the new constraint.
--
windowSetPosition :: WindowClass self => self -> WindowPosition -> IO ()
windowSetPosition self position =
  {# call window_set_position #}
    (toWindow self)
    ((fromIntegral . fromEnum) position)

-- | Dialog windows should be set transient for the main application window
-- they were spawned from. This allows window managers to e.g. keep the dialog
-- on top of the main window, or center the dialog over the main window.
-- 'dialogNewWithButtons' and other convenience functions in Gtk+ will
-- sometimes call 'windowSetTransientFor' on your behalf.
--
-- On Windows, this function will and put the child window on top of the
-- parent, much as the window manager would have done on X.
--
windowSetTransientFor :: (WindowClass self, WindowClass parent) => self
 -> parent -- ^ @parent@ - parent window
 -> IO ()
windowSetTransientFor self parent =
  {# call window_set_transient_for #}
    (toWindow self)
    (toWindow parent)

-- | If this setting is @True@, then destroying the transient parent of the
-- window will also destroy the window itself. This is useful for dialogs that
-- shouldn't persist beyond the lifetime of the main window they\'re associated
-- with, for example.
--
windowSetDestroyWithParent :: WindowClass self => self -> Bool -> IO ()
windowSetDestroyWithParent self setting =
  {# call window_set_destroy_with_parent #}
    (toWindow self)
    (fromBool setting)

-- | Returns whether the window will be destroyed with its transient parent.
-- See 'windowSetDestroyWithParent'.
--
windowGetDestroyWithParent :: WindowClass self => self
 -> IO Bool -- ^ returns @True@ if the window will be destroyed with its
            -- transient parent.
windowGetDestroyWithParent self =
  liftM toBool $
  {# call gtk_window_get_destroy_with_parent #}
    (toWindow self)

#if GTK_CHECK_VERSION(2,4,0)
-- | Returns whether the window is part of the current active toplevel. (That
-- is, the toplevel window receiving keystrokes.) The return value is @True@ if
-- the window is active toplevel itself, but also if it is, say, a 'Plug'
-- embedded in the active toplevel. You might use this function if you wanted
-- to draw a widget differently in an active window from a widget in an
-- inactive window. See 'windowHasToplevelFocus'
--
-- * Available since Gtk version 2.4
--
windowIsActive :: WindowClass self => self
 -> IO Bool -- ^ returns @True@ if the window part of the current active
            -- window.
windowIsActive self =
  liftM toBool $
  {# call gtk_window_is_active #}
    (toWindow self)

-- | Returns whether the input focus is within this 'Window'. For real
-- toplevel windows, this is identical to 'windowIsActive', but for embedded
-- windows, like 'Plug', the results will differ.
--
-- * Available since Gtk version 2.4
--
windowHasToplevelFocus :: WindowClass self => self
 -> IO Bool -- ^ returns @True@ if the the input focus is within this 'Window'
windowHasToplevelFocus self =
  liftM toBool $
  {# call gtk_window_has_toplevel_focus #}
    (toWindow self)
#endif

-- | Presents a window to the user. This may mean raising the window in the
-- stacking order, deiconifying it, moving it to the current desktop, and\/or
-- giving it the keyboard focus, possibly dependent on the user's platform,
-- window manager, and preferences.
--
-- If @window@ is hidden, this function calls 'widgetShow' as well.
--
-- This function should be used when the user tries to open a window that's
-- already open. Say for example the preferences dialog is currently open, and
-- the user chooses Preferences from the menu a second time; use
-- 'windowPresent' to move the already-open dialog where the user can see it.
--
windowPresent :: WindowClass self => self -> IO ()
windowPresent self =
  {# call gtk_window_present #}
    (toWindow self)

-- | Asks to deiconify (i.e. unminimize) the specified @window@. Note that you
-- shouldn't assume the window is definitely deiconified afterward, because
-- other entities (e.g. the user or window manager) could iconify it again
-- before your code which assumes deiconification gets to run.
--
-- You can track iconification via the \"window_state_event\" signal on
-- 'Widget'.
--
windowDeiconify :: WindowClass self => self -> IO ()
windowDeiconify self =
  {# call window_deiconify #}
    (toWindow self)

-- | Asks to iconify (i.e. minimize) the specified @window@. Note that you
-- shouldn't assume the window is definitely iconified afterward, because other
-- entities (e.g. the user or window manager) could deiconify it again, or
-- there may not be a window manager in which case iconification isn't
-- possible, etc. But normally the window will end up iconified. Just don't
-- write code that crashes if not.
--
-- It's permitted to call this function before showing a window, in which
-- case the window will be iconified before it ever appears onscreen.
--
-- You can track iconification via the \"window_state_event\" signal on
-- 'Widget'.
--
windowIconify :: WindowClass self => self -> IO ()
windowIconify self =
  {# call window_iconify #}
    (toWindow self)

-- | Asks to maximize the window, so that it becomes full-screen. Note that you
-- shouldn't assume the window is definitely maximized afterward, because other
-- entities (e.g. the user or window manager) could unmaximize it again, and
-- not all window managers support maximization. But normally the window will
-- end up maximized. Just don't write code that crashes if not.
--
-- It's permitted to call this function before showing a window, in which
-- case the window will be maximized when it appears onscreen initially.
--
-- You can track maximization via the \"window_state_event\" signal on
-- 'Widget'.
--
windowMaximize :: WindowClass self => self -> IO ()
windowMaximize self =
  {# call window_maximize #}
    (toWindow self)

-- | Asks to unmaximize the window. Note that you shouldn't assume the window is
-- definitely unmaximized afterward, because other entities (e.g. the user or
-- window manager) could maximize it again, and not all window managers honor
-- requests to unmaximize. But normally the window will end up unmaximized.
-- Just don't write code that crashes if not.
--
-- You can track maximization via the \"window_state_event\" signal on
-- 'Widget'.
--
windowUnmaximize :: WindowClass self => self -> IO ()
windowUnmaximize self =
  {# call window_unmaximize #}
    (toWindow self)

#if GTK_CHECK_VERSION(2,2,0)
-- | Asks to place @window@ in the fullscreen state. Note that you shouldn't
-- assume the window is definitely full screen afterward, because other
-- entities (e.g. the user or window manager) could unfullscreen it again, and
-- not all window managers honor requests to fullscreen windows. But normally
-- the window will end up fullscreen. Just don't write code that crashes if
-- not.
--
-- You can track the fullscreen state via the \"window_state_event\" signal
-- on 'Widget'.
--
-- * Available since Gtk version 2.2
--
windowFullscreen :: WindowClass self => self -> IO ()
windowFullscreen self =
  {# call gtk_window_fullscreen #}
    (toWindow self)

-- | Asks to toggle off the fullscreen state for @window@. Note that you
-- shouldn't assume the window is definitely not full screen afterward, because
-- other entities (e.g. the user or window manager) could fullscreen it again,
-- and not all window managers honor requests to unfullscreen windows. But
-- normally the window will end up restored to its normal state. Just don't
-- write code that crashes if not.
--
-- You can track the fullscreen state via the \"window_state_event\" signal
-- on 'Widget'.
--
-- * Available since Gtk version 2.2
--
windowUnfullscreen :: WindowClass self => self -> IO ()
windowUnfullscreen self =
  {# call gtk_window_unfullscreen #}
    (toWindow self)
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Asks to keep @window@ above, so that it stays on top. Note that you
-- shouldn't assume the window is definitely above afterward, because other
-- entities (e.g. the user or window manager) could not keep it above, and not
-- all window managers support keeping windows above. But normally the window
-- will end kept above. Just don't write code that crashes if not.
--
-- It's permitted to call this function before showing a window, in which
-- case the window will be kept above when it appears onscreen initially.
--
-- You can track the above state via the \"window_state_event\" signal on
-- 'Widget'.
--
-- Note that, according to the Extended Window Manager Hints specification,
-- the above state is mainly meant for user preferences and should not be used
-- by applications e.g. for drawing attention to their dialogs.
--
-- * Available since Gtk version 2.4
--
windowSetKeepAbove :: WindowClass self => self
 -> Bool  -- ^ @setting@ - whether to keep @window@ above other windows
 -> IO ()
windowSetKeepAbove self setting =
  {# call gtk_window_set_keep_above #}
    (toWindow self)
    (fromBool setting)

-- | Asks to keep @window@ below, so that it stays in bottom. Note that you
-- shouldn't assume the window is definitely below afterward, because other
-- entities (e.g. the user or window manager) could not keep it below, and not
-- all window managers support putting windows below. But normally the window
-- will be kept below. Just don't write code that crashes if not.
--
-- It's permitted to call this function before showing a window, in which
-- case the window will be kept below when it appears onscreen initially.
--
-- You can track the below state via the \"window_state_event\" signal on
-- 'Widget'.
--
-- Note that, according to the Extended Window Manager Hints specification,
-- the above state is mainly meant for user preferences and should not be used
-- by applications e.g. for drawing attention to their dialogs.
--
-- * Available since Gtk version 2.4
--
windowSetKeepBelow :: WindowClass self => self
 -> Bool  -- ^ @setting@ - whether to keep @window@ below other windows
 -> IO ()
windowSetKeepBelow self setting =
  {# call gtk_window_set_keep_below #}
    (toWindow self)
    (fromBool setting)
#endif

#if GTK_CHECK_VERSION(2,2,0)
-- | Windows may set a hint asking the desktop environment not to display the
-- window in the task bar. This function sets this hint.
--
-- * Available since Gtk version 2.2
--
windowSetSkipTaskbarHint :: WindowClass self => self
 -> Bool  -- ^ @setting@ - @True@ to keep this window from appearing in the
          -- task bar
 -> IO ()
windowSetSkipTaskbarHint self setting =
  {# call gtk_window_set_skip_taskbar_hint #}
    (toWindow self)
    (fromBool setting)

-- | Gets the value set by 'windowSetSkipTaskbarHint'
--
-- * Available since Gtk version 2.2
--
windowGetSkipTaskbarHint :: WindowClass self => self
 -> IO Bool -- ^ returns @True@ if window shouldn't be in taskbar
windowGetSkipTaskbarHint self =
  liftM toBool $
  {# call gtk_window_get_skip_taskbar_hint #}
    (toWindow self)

-- | Windows may set a hint asking the desktop environment not to display the
-- window in the pager. This function sets this hint. (A \"pager\" is any
-- desktop navigation tool such as a workspace switcher that displays a
-- thumbnail representation of the windows on the screen.)
--
-- * Available since Gtk version 2.2
--
windowSetSkipPagerHint :: WindowClass self => self
 -> Bool  -- ^ @setting@ - @True@ to keep this window from appearing in the
          -- pager
 -> IO ()
windowSetSkipPagerHint self setting =
  {# call gtk_window_set_skip_pager_hint #}
    (toWindow self)
    (fromBool setting)

-- | Gets the value set by 'windowSetSkipPagerHint'.
--
-- * Available since Gtk version 2.2
--
windowGetSkipPagerHint :: WindowClass self => self
 -> IO Bool -- ^ returns @True@ if window shouldn't be in pager
windowGetSkipPagerHint self =
  liftM toBool $
  {# call gtk_window_get_skip_pager_hint #}
    (toWindow self)
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Windows may set a hint asking the desktop environment not to receive the
-- input focus. This function sets this hint.
--
-- * Available since Gtk version 2.4
--
windowSetAcceptFocus :: WindowClass self => self
 -> Bool  -- ^ @setting@ - @True@ to let this window receive input focus
 -> IO ()
windowSetAcceptFocus self setting =
  {# call gtk_window_set_accept_focus #}
    (toWindow self)
    (fromBool setting)

-- | Gets the value set by 'windowSetAcceptFocus'.
--
-- * Available since Gtk version 2.4
--
windowGetAcceptFocus :: WindowClass self => self
 -> IO Bool -- ^ returns @True@ if window should receive the input focus
windowGetAcceptFocus self =
  liftM toBool $
  {# call gtk_window_get_accept_focus #}
    (toWindow self)
#endif

#if GTK_CHECK_VERSION(2,6,0)
-- | Windows may set a hint asking the desktop environment not to receive the
-- input focus when the window is mapped. This function sets this hint.
--
-- * Available since Gtk version 2.6
--
windowSetFocusOnMap :: WindowClass self => self
 -> Bool  -- ^ @setting@ - @True@ to let this window receive input focus on
          -- map
 -> IO ()
windowSetFocusOnMap self setting =
  {# call gtk_window_set_focus_on_map #}
    (toWindow self)
    (fromBool setting)

-- | Gets the value set by 'windowSetFocusOnMap'.
--
-- * Available since Gtk version 2.6
--
windowGetFocusOnMap :: WindowClass self => self
 -> IO Bool -- ^ returns @True@ if window should receive the input focus when
            -- mapped.
windowGetFocusOnMap self =
  liftM toBool $
  {# call gtk_window_get_focus_on_map #}
    (toWindow self)
#endif


-- | By default, windows are decorated with a title bar, resize controls, etc.
-- Some window managers allow Gtk+ to disable these decorations, creating a
-- borderless window. If you set the decorated property to @False@ using this
-- function, Gtk+ will do its best to convince the window manager not to
-- decorate the window. Depending on the system, this function may not have any
-- effect when called on a window that is already visible, so you should call
-- it before calling 'windowShow'.
--
-- On Windows, this function always works, since there's no window manager
-- policy involved.
--
windowSetDecorated :: WindowClass self => self -> Bool -> IO ()
windowSetDecorated self setting =
  {# call window_set_decorated #}
    (toWindow self)
    (fromBool setting)

-- | (Note: this is a special-purpose function intended for the framebuffer
-- port; see 'windowSetHasFrame'. It will have no effect on the window border
-- drawn by the window manager, which is the normal case when using the X
-- Window system.)
--
-- For windows with frames (see 'windowSetHasFrame') this function can be
-- used to change the size of the frame border.
--
windowSetFrameDimensions :: WindowClass self => self
 -> Int   -- ^ @left@ - The width of the left border
 -> Int   -- ^ @top@ - The height of the top border
 -> Int   -- ^ @right@ - The width of the right border
 -> Int   -- ^ @bottom@ - The height of the bottom border
 -> IO ()
windowSetFrameDimensions self left top right bottom =
  {# call window_set_frame_dimensions #}
    (toWindow self)
    (fromIntegral left)
    (fromIntegral top)
    (fromIntegral right)
    (fromIntegral bottom)

-- | This function is only useful on X11, not with other Gtk+ targets.
--
-- In combination with the window title, the window role allows a window
-- manager to identify \"the same\" window when an application is restarted. So
-- for example you might set the \"toolbox\" role on your app's toolbox window,
-- so that when the user restarts their session, the window manager can put the
-- toolbox back in the same place.
--
-- If a window already has a unique title, you don't need to set the role,
-- since the WM can use the title to identify the window when restoring the
-- session.
--
windowSetRole :: WindowClass self => self
 -> String -- ^ @role@ - unique identifier for the window to be used when
           -- restoring a session
 -> IO ()
windowSetRole self role =
  withUTFString role $ \rolePtr ->
  {# call window_set_role #}
    (toWindow self)
    rolePtr

-- | Asks to stick @window@, which means that it will appear on all user
-- desktops. Note that you shouldn't assume the window is definitely stuck
-- afterward, because other entities (e.g. the user or window manager) could
-- unstick it again, and some window managers do not support sticking windows.
-- But normally the window will end up stuck. Just don't write code that
-- crashes if not.
--
-- It's permitted to call this function before showing a window.
--
-- You can track stickiness via the \"window_state_event\" signal on
-- 'Widget'.
--
windowStick :: WindowClass self => self -> IO ()
windowStick self =
  {# call window_stick #}
    (toWindow self)

-- | Asks to unstick @window@, which means that it will appear on only one of
-- the user's desktops. Note that you shouldn't assume the window is definitely
-- unstuck afterward, because other entities (e.g. the user or window manager)
-- could stick it again. But normally the window will end up stuck. Just don't
-- write code that crashes if not.
--
-- You can track stickiness via the \"window_state_event\" signal on
-- 'Widget'.
--
windowUnstick :: WindowClass self => self -> IO ()
windowUnstick self =
  {# call window_unstick #}
    (toWindow self)

-- | Associate @accelGroup@ with @window@, such that calling
-- 'accelGroupsActivate' on @window@ will activate accelerators in
-- @accelGroup@.
--
windowAddAccelGroup :: (WindowClass self, AccelGroupClass accelGroup) => self
 -> accelGroup -- ^ @accelGroup@ - a 'AccelGroup'
 -> IO ()
windowAddAccelGroup self accelGroup =
  {# call gtk_window_add_accel_group #}
    (toWindow self)
    (toAccelGroup accelGroup)

-- | Reverses the effects of 'windowAddAccelGroup'.
--
windowRemoveAccelGroup :: (WindowClass self, AccelGroupClass accelGroup) => self
 -> accelGroup -- ^ @accelGroup@ - a 'AccelGroup'
 -> IO ()
windowRemoveAccelGroup self accelGroup =
  {# call gtk_window_remove_accel_group #}
    (toWindow self)
    (toAccelGroup accelGroup)

-- | Sets up the icon representing a 'Window'. This icon is used when the
-- window is minimized (also known as iconified). Some window managers or
-- desktop environments may also place it in the window frame, or display it in
-- other contexts.
--
-- The icon should be provided in whatever size it was naturally drawn; that
-- is, don't scale the image before passing it to Gtk+. Scaling is postponed
-- until the last minute, when the desired final size is known, to allow best
-- quality.
--
-- If you have your icon hand-drawn in multiple sizes, use
-- 'windowSetIconList'. Then the best size will be used.
--
-- This function is equivalent to calling 'windowSetIconList' with a
-- 1-element list.
--
-- See also 'windowSetDefaultIconList' to set the icon for all windows in
-- your application in one go.
--
windowSetIcon :: WindowClass self => self
 -> Pixbuf -- ^ @icon@ - icon image
 -> IO ()
windowSetIcon self icon =
  {# call gtk_window_set_icon #}
    (toWindow self)
    icon

#if GTK_CHECK_VERSION(2,6,0)
-- | Sets the icon for the window from a named themed icon. See the docs for
-- 'IconTheme' for more details.
--
-- Note that this has nothing to do with the WM_ICON_NAME property which is
-- mentioned in the ICCCM.
--
-- * Available since Gtk version 2.6
--
windowSetIconName :: WindowClass self => self
 -> String -- ^ @name@ - the name of the themed icon
 -> IO ()
windowSetIconName self name =
  withUTFString name $ \namePtr ->
  {# call gtk_window_set_icon_name #}
    (toWindow self)
    namePtr

-- | Returns the name of the themed icon for the window, see
-- 'windowSetIconName'.
--
-- * Available since Gtk version 2.6
--
windowGetIconName :: WindowClass self => self
 -> IO String -- ^ returns the icon name or {@NULL@, FIXME: this should
              -- probably be converted to a Maybe data type} if the window has
              -- no themed icon
windowGetIconName self =
  {# call gtk_window_get_icon_name #}
    (toWindow self)
  >>= peekUTFString

-- | Sets an icon to be used as fallback for windows that haven't had
-- 'windowSetIconList' called on them from a named themed icon, see
-- 'windowSetIconName'.
--
-- * Available since Gtk version 2.6
--
windowSetDefaultIconName ::
    String -- ^ @name@ - the name of the themed icon
 -> IO ()
windowSetDefaultIconName name =
  withUTFString name $ \namePtr ->
  {# call gtk_window_set_default_icon_name #}
    namePtr
#endif

--------------------
-- Properties

-- | If @True@, users can resize the window.
--
-- Default value: @True@
--
windowResizable :: WindowClass self => Attr self Bool
windowResizable = Attr 
  windowGetResizable
  windowSetResizable

--------------------
-- Signals

-- | 
--
onFrameEvent, afterFrameEvent :: WindowClass self => self
 -> (Event -> IO Bool)
 -> IO (ConnectId self)
onFrameEvent = connect_BOXED__BOOL "frame_event" marshalEvent False
afterFrameEvent = connect_BOXED__BOOL "frame_event" marshalEvent True

-- | 
--
onSetFocus, afterSetFocus :: (WindowClass self, WidgetClass foc) => self
 -> (foc -> IO ())
 -> IO (ConnectId self)
onSetFocus = connect_OBJECT__NONE "set_focus" False
afterSetFocus = connect_OBJECT__NONE "set_focus" True
