-- -*-haskell-*-
--  GIMP Toolkit (GTK) Window
--
--  Author : Manuel M. T. Chakravarty, Axel Simon
--
--  Created: 27 April 2001
--
--  Version $Revision: 1.5 $ from $Date: 2005/03/26 00:11:42 $
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
-- windowListToplevels,
-- windowAddMnemonic,
-- windowRemoveMnemonic,
-- windowSetMnemonicModifier,
  windowDeiconify,
  windowIconify,
  windowMaximize,
  windowUnmaximize,
  windowSetDecorated,
-- windowSetDecorationsHint,
  windowSetFrameDimensions,
-- windowSetFunctionHint,
  windowSetRole,
  windowStick,
  windowUnstick,

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
