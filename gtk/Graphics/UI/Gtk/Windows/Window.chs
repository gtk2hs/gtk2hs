{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Window
--
--  Author : Manuel M. T. Chakravarty, Axel Simon, Andy Stewart
--
--  Created: 27 April 2001
--
--  Copyright (C) 2001-2005 Manuel M. T. Chakravarty, Axel Simon
--  Copyright (C) 2009 Andy Stewart
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
  castToWindow, gTypeWindow,
  toWindow,
  WindowType(..),
  WindowEdge(..),
  WindowTypeHint(..),
  Gravity(..),

-- * Constructors
  windowNew,
  windowNewPopup,

-- * Methods
  windowActivateFocus,
  windowActivateDefault,
  windowSetDefaultSize,
  windowGetDefaultSize,
  windowSetPosition,
  WindowPosition(..),
#if GTK_CHECK_VERSION(2,4,0)
  windowIsActive,
  windowHasToplevelFocus,
#endif
  windowListToplevels,
  windowSetDefault,
#if GTK_CHECK_VERSION(2,14,0)
  windowGetDefaultWidget,
#endif
  windowAddMnemonic,
  windowRemoveMnemonic,
  windowMnemonicActivate,
  windowActivateKey,
  windowPropagateKeyEvent,
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
#if GTK_CHECK_VERSION(2,12,0)
  windowSetStartupId,
#endif
#if GTK_MAJOR_VERSION < 3
  windowGetFrame,
  windowSetFrameDimensions,
  windowGetFrameDimensions,
#endif
  windowStick,
  windowUnstick,
  windowAddAccelGroup,
  windowRemoveAccelGroup,
  windowSetDefaultIconList,
  windowGetDefaultIconList,
#if GTK_CHECK_VERSION(2,4,0)
  windowSetDefaultIcon,
#endif
#if GTK_CHECK_VERSION(2,2,0)
  windowSetDefaultIconFromFile,
  windowSetDefaultIconName,
#if GTK_CHECK_VERSION(2,16,0)
  windowGetDefaultIconName,
#endif
#endif
  windowSetGravity,
  windowGetGravity,
#if GTK_CHECK_VERSION(2,2,0)
  windowSetScreen,
  windowGetScreen,
#endif
  windowBeginResizeDrag,
  windowBeginMoveDrag,
  windowSetTypeHint,
  windowGetTypeHint,
  windowGetIcon,
  windowGetPosition,
  windowGetSize,
  windowMove,
  windowParseGeometry,
  windowReshowWithInitialSize,
  windowResize,
#if GTK_CHECK_VERSION(2,2,0)
  windowSetIconFromFile,
  windowSetAutoStartupNotification,
#endif
#if GTK_CHECK_VERSION(2,8,0)
  windowPresentWithTime,
#endif
  windowSetGeometryHints,
#if GTK_CHECK_VERSION(2,10,0)
  windowGetGroup,
#endif
#if GTK_CHECK_VERSION(2,20,0)
  windowGetWindowType,
#endif

-- * Attributes
  windowTitle,
  windowType,
  windowAllowShrink,
  windowAllowGrow,
  windowResizable,
#if GTK_MAJOR_VERSION >= 3
  windowHasResizeGrip,
#endif
  windowModal,
#if GTK_CHECK_VERSION(2,12,0)
  windowOpacity,
#endif
  windowRole,
#if GTK_CHECK_VERSION(2,12,0)
  windowStartupId,
#endif
  windowWindowPosition,
  windowDefaultWidth,
  windowDefaultHeight,
  windowDeletable,
  windowDestroyWithParent,
  windowIcon,
  windowIconName,
#if GTK_CHECK_VERSION(2,2,0)
  windowScreen,
#endif
  windowTypeHint,
#if GTK_CHECK_VERSION(2,2,0)
  windowSkipTaskbarHint,
  windowSkipPagerHint,
#endif
#if GTK_CHECK_VERSION(2,8,0)
  windowUrgencyHint,
#endif
#if GTK_CHECK_VERSION(2,4,0)
  windowAcceptFocus,
#endif
#if GTK_CHECK_VERSION(2,6,0)
  windowFocusOnMap,
#endif
#if GTK_CHECK_VERSION(2,4,0)
  windowDecorated,
  windowGravity,
#endif
  windowToplevelFocus,
  windowTransientFor,
  windowFocus,
#if GTK_MAJOR_VERSION < 3
  windowHasFrame,
#endif
  windowIconList,
  windowMnemonicModifier,
#if GTK_CHECK_VERSION(2,20,0)
  windowMnemonicVisible,
#endif

-- * Signals
  frameEvent,
  keysChanged,
  setFocus,

-- * Deprecated
#ifndef DISABLE_DEPRECATED
  windowSetTitle,
  windowGetTitle,
  windowSetResizable,
  windowGetResizable,
#if GTK_MAJOR_VERSION >= 3
  windowSetHasResizeGrip,
  windowGetHasResizeGrip,
#endif
  windowSetModal,
  windowGetModal,
#if GTK_MAJOR_VERSION < 3
  windowSetPolicy,
#endif
  windowSetTransientFor,
  windowGetTransientFor,
  windowSetDestroyWithParent,
  windowGetDestroyWithParent,
  windowGetFocus,
  windowSetFocus,
  windowSetMnemonicModifier,
  windowGetMnemonicModifier,
#if GTK_CHECK_VERSION(2,2,0)
  windowSetSkipTaskbarHint,
  windowGetSkipTaskbarHint,
  windowSetSkipPagerHint,
  windowGetSkipPagerHint,
#if GTK_CHECK_VERSION(2,4,0)
  windowSetAcceptFocus,
  windowGetAcceptFocus,
#if GTK_CHECK_VERSION(2,6,0)
  windowSetFocusOnMap,
  windowGetFocusOnMap,
#endif
#endif
#endif
  windowSetDecorated,
  windowGetDecorated,
#if GTK_CHECK_VERSION(2,10,0)
  windowSetDeletable,
  windowGetDeletable,
#endif
#if GTK_MAJOR_VERSION < 3
  windowSetHasFrame,
  windowGetHasFrame,
#endif
  windowSetRole,
  windowGetRole,
  windowSetIcon,
  windowSetIconList,
  windowGetIconList,
#if GTK_CHECK_VERSION(2,6,0)
  windowSetIconName,
  windowGetIconName,
#endif
#if GTK_CHECK_VERSION(2,8,0)
  windowSetUrgencyHint,
  windowGetUrgencyHint,
#if GTK_CHECK_VERSION(2,12,0)
  windowSetOpacity,
  windowGetOpacity,
#endif
#endif
  onSetFocus,
  afterSetFocus
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Flags
import System.Glib.GError
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GList                (fromGList, withGList)
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
import Graphics.UI.Gtk.General.Enums    (WindowType(..), WindowPosition(..))
#if GTK_MAJOR_VERSION < 3
import Graphics.UI.Gtk.General.Structs  (windowGetFrame)
#endif
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.Gdk.Enums#}    (Modifier(..))
{#import Graphics.UI.Gtk.Gdk.Keys#}     (KeyVal)
import Graphics.UI.Gtk.Gdk.EventM       (EventM, EAny, EKey, MouseButton, TimeStamp)
import Control.Monad.Reader             ( runReaderT, ask )
import Control.Monad.Trans              ( liftIO )
import Graphics.UI.Gtk.Gdk.Enums        (WindowEdge(..), WindowTypeHint(..),
                                        Gravity(..))

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

-- | Create a popup window.
--
windowNewPopup :: IO Window
windowNewPopup =
  makeNewObject mkWindow $
  liftM (castPtr :: Ptr Widget -> Ptr Window) $
  {# call window_new #}
    ((fromIntegral . fromEnum) WindowPopup)

--------------------
-- Methods

-- | Sets the title of the 'Window'. The title of a window will be displayed
-- in its title bar; on the X Window System, the title bar is rendered by the
-- window manager, so exactly how the title appears to users may vary according
-- to a user's exact configuration. The title should help a user distinguish
-- this window from other windows they may have open. A good title might
-- include the application name and current document filename, for example.
--
windowSetTitle :: (WindowClass self, GlibString string) => self -> string -> IO ()
windowSetTitle self title =
  withUTFString title $ \titlePtr ->
  {# call gtk_window_set_title #}
    (toWindow self)
    titlePtr

-- | Retrieves the title of the window. See 'windowSetTitle'.
--
windowGetTitle :: (WindowClass self, GlibString string) => self -> IO string
windowGetTitle self =
  {# call gtk_window_get_title #}
    (toWindow self)
  >>= \strPtr -> if strPtr == nullPtr
                   then return ""
                   else peekUTFString strPtr

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

#if GTK_MAJOR_VERSION >= 3
-- | Sets whether the window has a resize grip. @True@ by default.
--
windowSetHasResizeGrip :: WindowClass self => self -> Bool -> IO ()
windowSetHasResizeGrip self setting =
  {# call window_set_has_resize_grip #}
    (toWindow self)
    (fromBool setting)

-- | Returns whether the window has a resize grip.
--
windowGetHasResizeGrip :: WindowClass self => self -> IO Bool
windowGetHasResizeGrip self =
  liftM toBool $
  {# call unsafe window_get_has_resize_grip #}
    (toWindow self)
#endif

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

#if GTK_MAJOR_VERSION < 3
#ifndef DISABLE_DEPRECATED
{-# DEPRECATED windowSetPolicy "Use windowSetResizable instead." #-}
-- | Sets the window resizing policy.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code. Use 'windowSetResizable' instead.
--
-- Removed in Gtk3.
windowSetPolicy :: WindowClass self => self -> Bool -> Bool -> Bool -> IO ()
windowSetPolicy self allowShrink allowGrow autoShrink =
  {# call window_set_policy #}
    (toWindow self)
    (fromBool allowShrink)
    (fromBool allowGrow)
    (fromBool autoShrink)
#endif
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

-- | Returns whether the window is modal. See 'windowSetModal'.
--
windowGetModal :: WindowClass self => self
 -> IO Bool -- ^ returns @True@ if the window is set to be modal and
            -- establishes a grab when shown
windowGetModal self =
  liftM toBool $
  {# call gtk_window_get_modal #}
    (toWindow self)

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

-- | Adds a mnemonic to this window.
--
windowAddMnemonic :: (WindowClass self, WidgetClass widget) => self
 -> KeyVal  -- ^ @keyval@ - the mnemonic
 -> widget  -- ^ @target@ - the widget that gets activated by the mnemonic
 -> IO ()
windowAddMnemonic self keyval target =
  {# call window_add_mnemonic #}
    (toWindow self)
    (fromIntegral keyval)
    (toWidget target)

-- | Removes a mnemonic from this window.
--
windowRemoveMnemonic :: (WindowClass self, WidgetClass widget) => self
 -> KeyVal -- ^ @keyval@ - the mnemonic
 -> widget  -- ^ @target@ - the widget that gets activated by the mnemonic
 -> IO ()
windowRemoveMnemonic self keyval target =
  {# call window_remove_mnemonic #}
    (toWindow self)
    (fromIntegral keyval)
    (toWidget target)

-- | Activates the targets associated with the mnemonic.
windowMnemonicActivate :: WindowClass self => self
 -> KeyVal  -- ^ @keyval@ - the mnemonic
 -> [Modifier]  -- ^ @modifier@ - the modifiers
 -> IO Bool  -- ^ return @True@ if the activation is done.
windowMnemonicActivate self keyval modifier = liftM toBool $
  {# call window_mnemonic_activate #}
    (toWindow self)
    (fromIntegral keyval)
    (fromIntegral (fromFlags modifier))

-- | Sets the mnemonic modifier for this window.
windowSetMnemonicModifier :: WindowClass self => self
 -> [Modifier]  -- ^ @modifier@ - the modifier mask used to activate mnemonics on this window.
 -> IO ()
windowSetMnemonicModifier self modifier =
  {# call window_set_mnemonic_modifier #}
    (toWindow self)
    (fromIntegral (fromFlags modifier))

-- | Returns the mnemonic modifier for this window. See 'windowSetMnemonicModifier'.
windowGetMnemonicModifier :: WindowClass self => self
 -> IO [Modifier]  -- ^ return the modifier mask used to activate mnemonics on this window.
windowGetMnemonicModifier self = liftM (toFlags . fromIntegral) $
  {# call window_get_mnemonic_modifier #}
    (toWindow self)

-- | Activates mnemonics and accelerators for this 'Window'.
-- This is normally called by the default 'keyPressEvent' handler for toplevel windows,
-- however in some cases it may be useful to call this directly when overriding the standard key handling for a toplevel window.
--
windowActivateKey :: WindowClass self => self -> EventM EKey Bool
  -- ^ return @True@ if a mnemonic or accelerator was found and activated.
windowActivateKey self = do
  ptr <- ask
  liftIO $ liftM toBool $
    {# call window_activate_key #}
      (toWindow self)
      (castPtr ptr)

-- | Propagate a key press or release event to the focus widget and up the focus container chain until a widget handles event.
-- This is normally called by the default 'keyPressEvent' and 'keyReleaseEvent' handlers for toplevel windows,
-- however in some cases it may be useful to call this directly when overriding the standard key handling for a toplevel window.
--
windowPropagateKeyEvent :: WindowClass self => self
  -> EventM EKey Bool
  -- ^ return @True@ if a widget in the focus chain handled the event.
windowPropagateKeyEvent self = do
  ptr <- ask
  liftIO $ liftM toBool $
    {# call window_propagate_key_event #}
      (toWindow self)
      (castPtr ptr)

-- | Gets the default size of the window. A value of -1 for the width or
-- height indicates that a default size has not been explicitly set for that
-- dimension, so the \"natural\" size of the window will be used.
--
windowGetDefaultSize :: WindowClass self => self
 -> IO (Int, Int) -- ^ @(width, height)@ - the default width and height
windowGetDefaultSize self =
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
  {# call gtk_window_get_default_size #}
    (toWindow self)
    widthPtr
    heightPtr
  width <- peek widthPtr
  height <- peek heightPtr
  return (fromIntegral width, fromIntegral height)

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
-- Note that if you want to show a window @self@ on top of a full-screen window @parent@, you need to
-- turn the @self@ window into a dialog (using 'windowSetTypeHint' with 'WindowTypeHintDialog').
-- Otherwise the @parent@ window will always cover the @self@ window.
--
windowSetTransientFor :: (WindowClass self, WindowClass parent) => self
 -> parent -- ^ @parent@ - parent window
 -> IO ()
windowSetTransientFor self parent =
  {# call window_set_transient_for #}
    (toWindow self)
    (toWindow parent)

-- | Fetches the transient parent for this window. See
-- 'windowSetTransientFor'.
--
windowGetTransientFor :: WindowClass self => self
 -> IO (Maybe Window) -- ^ returns the transient parent for this window, or
                      -- @Nothing@ if no transient parent has been set.
windowGetTransientFor self =
  maybeNull (makeNewObject mkWindow) $
  {# call gtk_window_get_transient_for #}
    (toWindow self)

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
-- * Available since Gtk+ version 2.4
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
-- * Available since Gtk+ version 2.4
--
windowHasToplevelFocus :: WindowClass self => self
 -> IO Bool -- ^ returns @True@ if the the input focus is within this 'Window'
windowHasToplevelFocus self =
  liftM toBool $
  {# call gtk_window_has_toplevel_focus #}
    (toWindow self)
#endif

-- | Returns a list of all existing toplevel windows.
--
windowListToplevels :: IO [Window]
windowListToplevels = do
  glistPtr <- {#call unsafe gtk_window_list_toplevels#}
  winPtrs <- fromGList glistPtr
  mapM (\ptr -> makeNewGObject mkWindow (return ptr)) winPtrs

-- | Retrieves the current focused widget within the window.
-- | Note that this is the widget that would have the focus if the toplevel
-- | window focused; if the toplevel window is not focused then
-- | 'widgetHasFocus' will not be True for the widget.
--
windowGetFocus :: WindowClass self => self -> IO (Maybe Widget)
windowGetFocus self =
  maybeNull (makeNewObject mkWidget) $
  {# call unsafe gtk_window_get_focus #}
    (toWindow self)

-- | If focus is not the current focus widget, and is focusable, sets it as
-- | the focus widget for the window. If focus is Nothing, unsets the focus
-- | widget for this window. To set the focus to a particular widget in the
-- | toplevel, it is usually more convenient to use 'widgetGrabFocus' instead
-- | of this function.
--
windowSetFocus :: (WindowClass self, WidgetClass widget) => self
  -> Maybe widget
  -> IO ()
windowSetFocus self focus =
  {# call unsafe gtk_window_set_focus #}
    (toWindow self)
    (maybe (Widget nullForeignPtr) toWidget focus)

#if GTK_CHECK_VERSION(2,14,0)
-- | Returns the default widget for window. See 'windowSetDefault' for more details.
--
-- * Available since Gtk+ version 2.14
--
windowGetDefaultWidget :: WindowClass self => self
 -> IO (Maybe Widget)
windowGetDefaultWidget self =
  maybeNull (makeNewObject mkWidget) $
  {# call window_get_default_widget #}
    (toWindow self)
#endif

-- | The default widget is the widget that's activated when the user presses
--   Enter in a dialog (for example). This function sets or unsets the default
--   widget for a Window about. When setting (rather than unsetting) the
--   default widget it's generally easier to call widgetGrabDefault on the
--   widget. Before making a widget the default widget, you must set the
--   'widgetCanDefault' flag on the widget.
--
windowSetDefault :: (WindowClass self, WidgetClass widget) => self
  -> Maybe widget
  -> IO ()
windowSetDefault self defaultWidget =
  {# call unsafe gtk_window_set_focus #}
    (toWindow self)
    (maybe (Widget nullForeignPtr) toWidget defaultWidget)

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
-- If you are calling this function in response to a user interaction, it is
-- preferable to use 'windowPresentWithTime'.
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
-- You can track iconification via the 'windowStateEvent' signal on
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
-- You can track iconification via the 'windowStateEvent' signal on
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
-- You can track maximization via the 'windowStateEvent' signal on
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
-- You can track maximization via the 'windowStateEvent' signal on
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
-- You can track the fullscreen state via the 'windowStateEvent' signal
-- on 'Widget'.
--
-- * Available since Gtk+ version 2.2
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
-- You can track the fullscreen state via the 'windowStateEvent' signal
-- on 'Widget'.
--
-- * Available since Gtk+ version 2.2
--
windowUnfullscreen :: WindowClass self => self -> IO ()
windowUnfullscreen self =
  {# call gtk_window_unfullscreen #}
    (toWindow self)

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
-- You can track the above state via the 'windowStateEvent' signal on
-- 'Widget'.
--
-- Note that, according to the Extended Window Manager Hints specification,
-- the above state is mainly meant for user preferences and should not be used
-- by applications e.g. for drawing attention to their dialogs.
--
-- * Available since Gtk+ version 2.4
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
-- You can track the below state via the 'windowStateEvent' signal on
-- 'Widget'.
--
-- Note that, according to the Extended Window Manager Hints specification,
-- the above state is mainly meant for user preferences and should not be used
-- by applications e.g. for drawing attention to their dialogs.
--
-- * Available since Gtk+ version 2.4
--
windowSetKeepBelow :: WindowClass self => self
 -> Bool  -- ^ @setting@ - whether to keep @window@ below other windows
 -> IO ()
windowSetKeepBelow self setting =
  {# call gtk_window_set_keep_below #}
    (toWindow self)
    (fromBool setting)
#endif

-- | Windows may set a hint asking the desktop environment not to display the
-- window in the task bar. This function sets this hint.
--
-- * Available since Gtk+ version 2.2
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
-- * Available since Gtk+ version 2.2
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
-- * Available since Gtk+ version 2.2
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
-- * Available since Gtk+ version 2.2
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
-- * Available since Gtk+ version 2.4
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
-- * Available since Gtk+ version 2.4
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
-- * Available since Gtk+ version 2.6
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
-- * Available since Gtk+ version 2.6
--
windowGetFocusOnMap :: WindowClass self => self
 -> IO Bool -- ^ returns @True@ if window should receive the input focus when
            -- mapped.
windowGetFocusOnMap self =
  liftM toBool $
  {# call gtk_window_get_focus_on_map #}
    (toWindow self)
#endif

#if GTK_CHECK_VERSION(2,12,0)
-- | Startup notification identifiers are used by desktop environment to track application startup,
-- to provide user feedback and other features. This function changes the corresponding property on the underlying GdkWindow.
-- Normally, startup identifier is managed automatically and you should only use this function in special cases like transferring focus from other processes. You should use this function before calling 'windowPresent' or any equivalent function generating a window map event.
--
-- This function is only useful on X11, not with other GTK+ targets.
--
-- * Available since Gtk+ version 2.12
--
windowSetStartupId :: (WindowClass self, GlibString string) => self
 -> string
 -> IO ()
windowSetStartupId self startupId =
  withUTFString startupId $ \idPtr ->
  {# call window_set_startup_id #}
    (toWindow self)
    idPtr
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

-- | Returns whether the window has been set to have decorations such as a
-- title bar via 'windowSetDecorated'.
--
windowGetDecorated :: WindowClass self => self
 -> IO Bool -- ^ returns @True@ if the window has been set to have decorations
windowGetDecorated self =
  liftM toBool $
  {# call gtk_window_get_decorated #}
    (toWindow self)

#if GTK_CHECK_VERSION(2,10,0)
#ifndef DISABLE_DEPRECATED
-- | By default, windows have a close button in the window frame.
-- Some window managers allow GTK+ to disable this button.
-- If you set the deletable property to  @False@ using this function, GTK+ will do its best to convince the window manager not to show a close button.
-- Depending on the system, this function may not have any effect when called on a window that is already visible,
-- so you should call it before calling 'windowShow'.
--
-- On Windows, this function always works, since there's no window manager policy involved.
--
-- * Available since Gtk+ version 2.10
--
windowSetDeletable :: WindowClass self => self
 -> Bool  -- ^ @setting@ - @True@ to decorate the window as deletable
 -> IO ()
windowSetDeletable self setting =
  {# call window_set_deletable #}
    (toWindow self)
    (fromBool setting)

-- | Returns whether the window has been set to have a close button via 'windowSetDeletable'.
--
-- * Available since Gtk+ version 2.10
--
windowGetDeletable :: WindowClass self => self
 -> IO Bool  -- ^ return @True@ if the window has been set to have a close button
windowGetDeletable self = liftM toBool $
  {# call window_get_deletable #}
    (toWindow self)
#endif
#endif

#if GTK_MAJOR_VERSION < 3
-- | (Note: this is a special-purpose function intended for the framebuffer
-- port; see 'windowSetHasFrame'. It will have no effect on the window border
-- drawn by the window manager, which is the normal case when using the X
-- Window system.)
--
-- For windows with frames (see 'windowSetHasFrame') this function can be
-- used to change the size of the frame border.
--
-- Removed in Gtk3.
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

-- |  Retrieves the dimensions of the frame window for this toplevel. See
--    'windowSetHasFrame', 'windowSetFrameDimensions'.
--
-- (Note: this is a special-purpose function intended for the framebuffer port;
-- see 'windowSetHasFrame'.
-- It will not return the size of the window border drawn by the window manager,
-- which is the normal case when using a windowing system.
-- See 'drawWindowGetFrameExtents' to get the standard window border extents.)
--
-- Removed in Gtk3.
windowGetFrameDimensions :: WindowClass self => self
 -> IO (Int, Int, Int, Int)
 -- ^ returns @(left, top, right, bottom)@. @left@ is the
 -- width of the frame at the left, @top@ is the height of the frame at the top, @right@
 -- is the width of the frame at the right, @bottom@ is the height of the frame at the bottom.
windowGetFrameDimensions self =
  alloca $ \lPtr -> alloca $ \tPtr -> alloca $ \rPtr -> alloca $ \bPtr -> do
    {# call window_get_frame_dimensions #} (toWindow self) lPtr tPtr rPtr bPtr
    lv <- peek lPtr
    tv <- peek tPtr
    rv <- peek rPtr
    bv <- peek bPtr
    return (fromIntegral lv, fromIntegral tv, fromIntegral rv, fromIntegral bv)

-- | If this function is called on a window with setting of @True@, before it is realized
-- or showed, it will have a "frame" window around its 'DrawWindow',
-- accessible using 'windowGetFrame'. Using the signal 'windowFrameEvent' you can
-- receive all events targeted at the frame.
--
-- (Note: this is a special-purpose function for the framebuffer port, that causes GTK+ to draw its own window border.
-- For most applications, you want  'windowSetDecorated' instead, which tells the window manager whether to draw the window border.)
--
-- This function is used by the linux-fb port to implement managed windows,
-- but it could conceivably be used by X-programs that want to do their own window
-- decorations.
--
-- Removed in Gtk3.
windowSetHasFrame :: WindowClass self => self
 -> Bool  -- ^ @setting@ - a boolean
 -> IO ()
windowSetHasFrame self setting =
  {# call window_set_has_frame #}
    (toWindow self)
    (fromBool setting)

-- | Accessor for whether the window has a frame window exterior to window->window. Gets the value set by 'windowSetHasFrame'.
--
-- Removed in Gtk3.
windowGetHasFrame :: WindowClass self => self
 -> IO Bool  -- ^ return @True@ if a frame has been added to the window via 'windowSetHasFrame'.
windowGetHasFrame self = liftM toBool $
  {# call window_get_has_frame #}
    (toWindow self)
#endif

#ifndef DISABLE_DEPRECATED
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
windowSetRole :: (WindowClass self, GlibString string) => self
 -> string -- ^ @role@ - unique identifier for the window to be used when
           -- restoring a session
 -> IO ()
windowSetRole self role =
  withUTFString role $ \rolePtr ->
  {# call window_set_role #}
    (toWindow self)
    rolePtr

-- | Returns the role of the window. See 'windowSetRole' for further
-- explanation.
--
windowGetRole :: (WindowClass self, GlibString string) => self
 -> IO (Maybe string) -- ^ returns the role of the window if set, or
                      -- @Nothing@.
windowGetRole self =
  {# call gtk_window_get_role #}
    (toWindow self)
  >>= maybePeek peekUTFString
#endif

-- | Asks to stick @window@, which means that it will appear on all user
-- desktops. Note that you shouldn't assume the window is definitely stuck
-- afterward, because other entities (e.g. the user or window manager) could
-- unstick it again, and some window managers do not support sticking windows.
-- But normally the window will end up stuck. Just don't write code that
-- crashes if not.
--
-- It's permitted to call this function before showing a window.
--
-- You can track stickiness via the 'windowStateEvent' signal on
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
-- You can track stickiness via the 'windowStateEvent' signal on
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
windowAddAccelGroup :: WindowClass self => self
 -> AccelGroup -- ^ @accelGroup@ - a 'AccelGroup'
 -> IO ()
windowAddAccelGroup self accelGroup =
  {# call gtk_window_add_accel_group #}
    (toWindow self)
    accelGroup

-- | Reverses the effects of 'windowAddAccelGroup'.
--
windowRemoveAccelGroup :: WindowClass self => self
 -> AccelGroup -- ^ @accelGroup@ - a 'AccelGroup'
 -> IO ()
windowRemoveAccelGroup self accelGroup =
  {# call gtk_window_remove_accel_group #}
    (toWindow self)
    accelGroup

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
 -> Maybe Pixbuf -- ^ @icon@ - icon image
 -> IO ()
windowSetIcon self Nothing =
  {# call gtk_window_set_icon #}
    (toWindow self)
    (Pixbuf nullForeignPtr)
windowSetIcon self (Just icon) =
  {# call gtk_window_set_icon #}
    (toWindow self)
    icon

-- | Gets the value set by 'windowSetIcon' (or if you\'ve called
-- 'windowSetIconList', gets the first icon in the icon list).
--
windowGetIcon :: WindowClass self => self
 -> IO (Maybe Pixbuf) -- ^ returns icon for window, or @Nothing@ if none was set
windowGetIcon self =
  maybeNull (makeNewGObject mkPixbuf) $
  {# call gtk_window_get_icon #}
    (toWindow self)

-- | Sets up the icon representing a 'Window'. The icon is used when the window is minimized (also known as iconified).
-- Some window managers or desktop environments may also place it in the window frame, or display it in other contexts.
--
-- 'windowSetIconList' allows you to pass in the same icon in several hand-drawn sizes.
-- The list should contain the natural sizes your icon is available in; that is, don't scale the image before passing it to GTK+.
-- Scaling is postponed until the last minute, when the desired final size is known, to allow best quality.
--
-- By passing several sizes, you may improve the final image quality of the icon, by reducing or eliminating automatic image scaling.
--
-- Recommended sizes to provide: 16x16, 32x32, 48x48 at minimum, and larger images (64x64, 128x128) if you have them.
--
-- See also 'windowSetDefaultIconList' to set the icon for all windows in your application in one go.
--
-- Note that transient windows (those who have been set transient for another window using 'windowSetTransientFor' will inherit their icon from their
-- transient parent.
-- So there's no need to explicitly set the icon on transient windows.
--
windowSetIconList :: WindowClass self => self
 -> [Pixbuf]
 -> IO ()
windowSetIconList self list =
  withForeignPtrs (map unPixbuf list) $ \ptrList ->
  withGList ptrList $ \glist ->
  {# call window_set_icon_list #}
     (toWindow self)
     glist

-- | Retrieves the list of icons set by 'windowSetIconList'.
--
windowGetIconList :: WindowClass self => self
 -> IO [Pixbuf]
windowGetIconList self = do
  glist <- {# call window_get_icon_list #} (toWindow self)
  ptrList <- fromGList glist
  mapM (makeNewGObject mkPixbuf . return) ptrList

-- | Sets an icon list to be used as fallback for windows that haven't had 'windowSetIconList' called on them to set up a window-specific icon list.
-- This function allows you to set up the icon for all windows in your app at once.
--
-- See 'windowSetIconList' for more details.
--
windowSetDefaultIconList :: [Pixbuf] -> IO ()
windowSetDefaultIconList list =
  withForeignPtrs (map unPixbuf list) $ \ptrList ->
  withGList ptrList $ \glist ->
  {# call window_set_default_icon_list #} glist

-- | Gets the value set by 'windowSetDefaultIconList'.
--
windowGetDefaultIconList :: IO [Pixbuf]
windowGetDefaultIconList = do
  glist <- {# call window_get_default_icon_list #}
  ptrList <- fromGList glist
  mapM (makeNewGObject mkPixbuf . return) ptrList

#if GTK_CHECK_VERSION(2,6,0)
#ifndef DISABLE_DEPRECATED
-- | Sets the icon for the window from a named themed icon. See the docs for
-- 'IconTheme' for more details.
--
-- Note that this has nothing to do with the WM_ICON_NAME property which is
-- mentioned in the ICCCM.
--
-- * Available since Gtk+ version 2.6
--
windowSetIconName :: (WindowClass self, GlibString string) => self
 -> string -- ^ @name@ - the name of the themed icon
 -> IO ()
windowSetIconName self name =
  withUTFString name $ \namePtr ->
  {# call gtk_window_set_icon_name #}
    (toWindow self)
    namePtr

-- | Returns the name of the themed icon for the window, see
-- 'windowSetIconName'.
--
-- * Available since Gtk+ version 2.6
--
windowGetIconName :: (WindowClass self, GlibString string) => self
 -> IO string -- ^ returns the icon name or @\"\"@ if the window has no themed
              -- icon.
windowGetIconName self =
  {# call gtk_window_get_icon_name #}
    (toWindow self)
  >>= \strPtr -> if strPtr == nullPtr
                then return ""
                else peekUTFString strPtr
#endif

-- | Sets an icon to be used as fallback for windows that haven't had
-- 'windowSetIconList' called on them from a named themed icon, see
-- 'windowSetIconName'.
--
-- * Available since Gtk+ version 2.6
--
windowSetDefaultIconName :: GlibString string
 => string -- ^ @name@ - the name of the themed icon
 -> IO ()
windowSetDefaultIconName name =
  withUTFString name $ \namePtr ->
  {# call gtk_window_set_default_icon_name #}
    namePtr
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Sets an icon to be used as fallback for windows that haven't had 'windowSetIcon' called on them from a pixbuf.
--
-- * Available since Gtk+ version 2.4
--
windowSetDefaultIcon :: Maybe Pixbuf -> IO ()
windowSetDefaultIcon (Just icon) =
  {# call window_set_default_icon #} icon
windowSetDefaultIcon Nothing =
  {# call window_set_default_icon #} (Pixbuf nullForeignPtr)
#endif

#if GTK_CHECK_VERSION(2,2,0)
-- | Sets an icon to be used as fallback for windows that haven't had
-- 'windowSetIconList' called on them from a file on disk. May throw a 'GError' if
--  the file cannot be loaded.
--
-- * Available since Gtk+ version 2.2
--
windowSetDefaultIconFromFile :: GlibString string
 => string  -- ^ @filename@ - location of icon file
 -> IO Bool -- ^ returns @True@ if setting the icon succeeded.
windowSetDefaultIconFromFile filename =
  liftM toBool $
  propagateGError $ \errPtr ->
  withUTFString filename $ \filenamePtr ->
  {# call gtk_window_set_default_icon_from_file #}
    filenamePtr
    errPtr
#endif

#if GTK_CHECK_VERSION(2,16,0)
-- | Returns the fallback icon name for windows that has been set with
-- 'windowSetDefaultIconName'.
--
-- * Available since Gtk+ version 2.16
--
windowGetDefaultIconName :: GlibString string
 => IO string -- ^ returns the fallback icon name for windows
windowGetDefaultIconName =
  {# call window_get_default_icon_name #}
  >>= peekUTFString
#endif

#if GTK_CHECK_VERSION(2,2,0)
-- | Sets the 'Screen' where the @window@ is displayed; if the window is
-- already mapped, it will be unmapped, and then remapped on the new screen.
--
-- * Available since Gtk+ version 2.2
--
windowSetScreen :: WindowClass self => self
 -> Screen -- ^ @screen@ - a 'Screen'.
 -> IO ()
windowSetScreen self screen =
  {# call gtk_window_set_screen #}
    (toWindow self)
    screen

-- | Returns the 'Screen' associated with the window.
--
-- * Available since Gtk+ version 2.2
--
windowGetScreen :: WindowClass self => self
 -> IO Screen -- ^ returns a 'Screen'.
windowGetScreen self =
  makeNewGObject mkScreen $
  {# call gtk_window_get_screen #}
    (toWindow self)

-- | Sets the icon for @window@.
--
-- This function is equivalent to calling 'windowSetIcon' with a pixbuf
-- created by loading the image from @filename@.
--
-- This may throw an exception if the file cannot be loaded.
--
-- * Available since Gtk+ version 2.2
--
windowSetIconFromFile :: (WindowClass self, GlibFilePath fp) => self
 -> fp  -- ^ @filename@ - location of icon file
 -> IO ()
windowSetIconFromFile self filename =
  propagateGError $ \errPtr ->
  withUTFFilePath filename $ \filenamePtr -> do
#if defined (WIN32) && GTK_CHECK_VERSION(2,6,0) && GTK_MAJOR_VERSION < 3
  {# call gtk_window_set_icon_from_file_utf8 #}
#else
  {# call gtk_window_set_icon_from_file #}
#endif
    (toWindow self)
    filenamePtr
    errPtr
  return ()

-- | By default, after showing the first 'Window' for each 'Screen', Gtk+
-- calls 'screenNotifyStartupComplete'. Call this function to disable the
-- automatic startup notification. You might do this if your first window is a
-- splash screen, and you want to delay notification until after your real main
-- window has been shown, for example.
--
-- In that example, you would disable startup notification temporarily, show
-- your splash screen, then re-enable it so that showing the main window would
-- automatically result in notification.
--
-- * Available since Gtk+ version 2.2
--
windowSetAutoStartupNotification ::
    Bool  -- ^ @setting@ - @True@ to automatically do startup notification
 -> IO ()
windowSetAutoStartupNotification setting =
  {# call gtk_window_set_auto_startup_notification #}
    (fromBool setting)
#endif

-- | Window gravity defines the meaning of coordinates passed to 'windowMove'.
-- See 'windowMove' and 'Gravity' for more details.
--
-- The default window gravity is 'GravityNorthWest' which will typically
-- \"do what you mean.\"
--
windowSetGravity :: WindowClass self => self
 -> Gravity -- ^ @gravity@ - window gravity
 -> IO ()
windowSetGravity self gravity =
  {# call gtk_window_set_gravity #}
    (toWindow self)
    ((fromIntegral . fromEnum) gravity)

-- | Gets the value set by 'windowSetGravity'.
--
windowGetGravity :: WindowClass self => self
 -> IO Gravity -- ^ returns window gravity
windowGetGravity self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_window_get_gravity #}
    (toWindow self)

-- | Asks the window manager to move @window@ to the given position. Window
-- managers are free to ignore this; most window managers ignore requests for
-- initial window positions (instead using a user-defined placement algorithm)
-- and honor requests after the window has already been shown.
--
-- Note: the position is the position of the gravity-determined reference
-- point for the window. The gravity determines two things: first, the location
-- of the reference point in root window coordinates; and second, which point
-- on the window is positioned at the reference point.
--
-- By default the gravity is 'GravityNorthWest', so the reference point is
-- simply the @x@, @y@ supplied to 'windowMove'. The top-left corner of the
-- window decorations (aka window frame or border) will be placed at @x@, @y@.
-- Therefore, to position a window at the top left of the screen, you want to
-- use the default gravity (which is 'GravityNorthWest') and move the window to
-- 0,0.
--
-- To position a window at the bottom right corner of the screen, you would
-- set 'GravitySouthEast', which means that the reference point is at @x@ + the
-- window width and @y@ + the window height, and the bottom-right corner of the
-- window border will be placed at that reference point. So, to place a window
-- in the bottom right corner you would first set gravity to south east, then
-- write: @gtk_window_move (window, gdk_screen_width() - window_width,
-- gdk_screen_height() - window_height)@.
--
-- The Extended Window Manager Hints specification at
-- http:\/\/www.freedesktop.org\/Standards\/wm-spec has a nice table of
-- gravities in the \"implementation notes\" section.
--
-- The 'windowGetPosition' documentation may also be relevant.
--
windowMove :: WindowClass self => self
 -> Int   -- ^ @x@ - X coordinate to move window to
 -> Int   -- ^ @y@ - Y coordinate to move window to
 -> IO ()
windowMove self x y =
  {# call gtk_window_move #}
    (toWindow self)
    (fromIntegral x)
    (fromIntegral y)

-- | Parses a standard X Window System geometry string - see the manual page for X (type 'man X') for details on this.
-- 'windowParseGeometry' does work on all GTK+ ports including Win32 but is primarily intended for an X environment.
--
-- If either a size or a position can be extracted from the geometry string,
-- 'windowParseGeometry' returns @True@ and calls gtk_window_set_default_size() and/or gtk_window_move() to resize/move the window.
--
-- If 'windowParseGeometry' returns @True@,
-- it will also set the 'HintUserPos' and/or 'HintUserSize' hints indicating to the window manager that the size/position of the window was user-specified
-- This causes most window managers to honor the geometry.
--
-- Note that for 'windowParseGeometry' to work as expected, it has to be called when the window has its "final" size, i.e.
-- after calling 'widgetShowAll' on the contents and 'windowSetGeometryHints' on the window.
--
windowParseGeometry :: (WindowClass self, GlibString string) => self
 -> string
 -> IO Bool
windowParseGeometry self geometry = liftM toBool $
  withUTFString geometry $ \geometryPtr ->
  {# call window_parse_geometry #}
     (toWindow self)
     geometryPtr

-- | Hides window, then reshows it, resetting the default size and position of the window. Used by GUI builders only.
--
windowReshowWithInitialSize :: WindowClass self => self -> IO ()
windowReshowWithInitialSize self =
  {# call window_reshow_with_initial_size #} (toWindow self)

-- | Resizes the window as if the user had done so, obeying geometry
-- constraints. The default geometry constraint is that windows may not be
-- smaller than their size request; to override this constraint, call
-- 'widgetSetSizeRequest' to set the window's request to a smaller value.
--
-- If 'windowResize' is called before showing a window for the first time,
-- it overrides any default size set with 'windowSetDefaultSize'.
--
-- Windows may not be resized smaller than 1 by 1 pixels.
--
windowResize :: WindowClass self => self
 -> Int   -- ^ @width@ - width in pixels to resize the window to
 -> Int   -- ^ @height@ - height in pixels to resize the window to
 -> IO ()
windowResize self width height =
  {# call gtk_window_resize #}
    (toWindow self)
    (fromIntegral width)
    (fromIntegral height)

-- | Starts resizing a window. This function is used if an application has
-- window resizing controls. When GDK can support it, the resize will be done
-- using the standard mechanism for the window manager or windowing system.
-- Otherwise, GDK will try to emulate window resizing, potentially not all that
-- well, depending on the windowing system.
--
windowBeginResizeDrag :: WindowClass self => self
 -> WindowEdge -- ^ @edge@ - position of the resize control
 -> MouseButton -- ^ @button@ - mouse button that initiated the drag
 -> Int        -- ^ @rootX@ - X position where the user clicked to initiate
               -- the drag, in root window coordinates
 -> Int        -- ^ @rootY@ - Y position where the user clicked to initiate
               -- the drag
 -> TimeStamp  -- ^ @timestamp@ - timestamp from the click event that
               -- initiated the drag
 -> IO ()
windowBeginResizeDrag self edge button rootX rootY timestamp =
  {# call gtk_window_begin_resize_drag #}
    (toWindow self)
    ((fromIntegral . fromEnum) edge)
    ((fromIntegral . fromEnum) button)
    (fromIntegral rootX)
    (fromIntegral rootY)
    (fromIntegral timestamp)

-- | Starts moving a window. This function is used if an application has
-- window movement grips. When GDK can support it, the window movement will be
-- done using the standard mechanism for the window manager or windowing
-- system. Otherwise, GDK will try to emulate window movement, potentially not
-- all that well, depending on the windowing system.
--
windowBeginMoveDrag :: WindowClass self => self
 -> MouseButton -- ^ @button@ - mouse button that initiated the drag
 -> Int    -- ^ @rootX@ - X position where the user clicked to initiate the
           -- drag, in root window coordinates
 -> Int    -- ^ @rootY@ - Y position where the user clicked to initiate the
           -- drag
 -> TimeStamp -- ^ @timestamp@ - timestamp from the click event that initiated
           -- the drag
 -> IO ()
windowBeginMoveDrag self button rootX rootY timestamp =
  {# call gtk_window_begin_move_drag #}
    (toWindow self)
    ((fromIntegral . fromEnum) button)
    (fromIntegral rootX)
    (fromIntegral rootY)
    (fromIntegral timestamp)

-- | This function returns the position you need to pass to 'windowMove' to
-- keep @window@ in its current position. This means that the meaning of the
-- returned value varies with window gravity. See 'windowMove' for more
-- details.
--
-- If you haven't changed the window gravity, its gravity will be
-- 'GravityNorthWest'. This means that 'windowGetPosition' gets the position of
-- the top-left corner of the window manager frame for the window. 'windowMove'
-- sets the position of this same top-left corner.
--
-- Moreover, nearly all window managers are historically broken with respect
-- to their handling of window gravity. So moving a window to its current
-- position as returned by 'windowGetPosition' tends to result in moving the
-- window slightly. Window managers are slowly getting better over time.
--
-- If a window has gravity 'GravityStatic' the window manager frame is not
-- relevant, and thus 'windowGetPosition' will always produce accurate results.
-- However you can't use static gravity to do things like place a window in a
-- corner of the screen, because static gravity ignores the window manager
-- decorations.
--
-- If you are saving and restoring your application's window positions, you
-- should know that it's impossible for applications to do this without getting
-- it somewhat wrong because applications do not have sufficient knowledge of
-- window manager state. The Correct Mechanism is to support the session
-- management protocol (see the \"GnomeClient\" object in the GNOME libraries
-- for example) and allow the window manager to save your window sizes and
-- positions.
--
windowGetPosition :: WindowClass self => self
 -> IO (Int, Int) -- ^ @(rootX, rootY)@ - X and Y coordinate of
                  -- gravity-determined reference point
windowGetPosition self =
  alloca $ \rootXPtr ->
  alloca $ \rootYPtr -> do
  {# call gtk_window_get_position #}
    (toWindow self)
    rootXPtr
    rootYPtr
  rootX <- peek rootXPtr
  rootY <- peek rootYPtr
  return (fromIntegral rootX, fromIntegral rootY)

-- | Obtains the current size of the window. If the window is not onscreen, it
-- returns the size Gtk+ will suggest to the window manager for the initial
-- window size (but this is not reliably the same as the size the window
-- manager will actually select). The size obtained by 'windowGetSize' is the
-- last size received in a 'EventConfigure', that is,
-- Gtk+ uses its locally-stored size, rather than querying the X server for the
-- size. As a result, if you call 'windowResize' then immediately call
-- 'windowGetSize', the size won't have taken effect yet. After the window
-- manager processes the resize request, Gtk+ receives notification that the
-- size has changed via a configure event, and the size of the window gets
-- updated.
--
-- Note 1: Nearly any use of this function creates a race condition, because
-- the size of the window may change between the time that you get the size and
-- the time that you perform some action assuming that size is the current
-- size. To avoid race conditions, connect to \"configure_event\" on the window
-- and adjust your size-dependent state to match the size delivered in the
-- 'EventConfigure'.
--
-- Note 2: The returned size does /not/ include the size of the window
-- manager decorations (aka the window frame or border). Those are not drawn by
-- Gtk+ and Gtk+ has no reliable method of determining their size.
--
-- Note 3: If you are getting a window size in order to position the window
-- onscreen, there may be a better way. The preferred way is to simply set the
-- window's semantic type with 'windowSetTypeHint', which allows the window
-- manager to e.g. center dialogs. Also, if you set the transient parent of
-- dialogs with 'windowSetTransientFor' window managers will often center the
-- dialog over its parent window. It's much preferred to let the window manager
-- handle these things rather than doing it yourself, because all apps will
-- behave consistently and according to user prefs if the window manager
-- handles it. Also, the window manager can take the size of the window
-- decorations\/border into account, while your application cannot.
--
-- In any case, if you insist on application-specified window positioning,
-- there's /still/ a better way than doing it yourself - 'windowSetPosition'
-- will frequently handle the details for you.
--
windowGetSize :: WindowClass self => self
 -> IO (Int, Int) -- ^ @(width, height)@
windowGetSize self =
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
  {# call gtk_window_get_size #}
    (toWindow self)
    widthPtr
    heightPtr
  width <- peek widthPtr
  height <- peek heightPtr
  return (fromIntegral width, fromIntegral height)

-- | By setting the type hint for the window, you allow the window manager to
-- decorate and handle the window in a way which is suitable to the function of
-- the window in your application.
--
-- This function should be called before the window becomes visible.
--
windowSetTypeHint :: WindowClass self => self
 -> WindowTypeHint -- ^ @hint@ - the window type
 -> IO ()
windowSetTypeHint self hint =
  {# call gtk_window_set_type_hint #}
    (toWindow self)
    ((fromIntegral . fromEnum) hint)

-- | Gets the type hint for this window. See 'windowSetTypeHint'.
--
windowGetTypeHint :: WindowClass self => self
 -> IO WindowTypeHint -- ^ returns the type hint for @window@.
windowGetTypeHint self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_window_get_type_hint #}
    (toWindow self)

#if GTK_CHECK_VERSION(2,8,0)
-- | Presents a window to the user in response to a user interaction. If you
-- need to present a window without a timestamp, use 'windowPresent'. See
-- 'windowPresent' for details.
--
-- * Available since Gtk+ version 2.8
--
windowPresentWithTime :: WindowClass self => self
 -> TimeStamp -- ^ @timestamp@ - the timestamp of the user interaction
              -- (typically a button or key press event) which triggered this
              -- call
 -> IO ()
windowPresentWithTime self timestamp =
  {# call gtk_window_present_with_time #}
    (toWindow self)
    (fromIntegral timestamp)

-- | Windows may set a hint asking the desktop environment to draw the users
-- attention to the window. This function sets this hint.
--
-- * Available since Gtk+ version 2.8
--
windowSetUrgencyHint :: WindowClass self => self
 -> Bool  -- ^ @setting@ - @True@ to mark this window as urgent
 -> IO ()
windowSetUrgencyHint self setting =
  {# call gtk_window_set_urgency_hint #}
    (toWindow self)
    (fromBool setting)

-- | Gets the value set by 'windowSetUrgencyHint'
--
-- * Available since Gtk+ version 2.8
--
windowGetUrgencyHint :: WindowClass self => self
 -> IO Bool -- ^ returns @True@ if window is urgent
windowGetUrgencyHint self =
  liftM toBool $
  {# call gtk_window_get_urgency_hint #}
    (toWindow self)
#endif

-- | This function sets up hints about how a window can be resized by the
-- user. You can set a minimum and maximum size, the allowed resize increments
-- (e.g. for xterm, you can only resize by the size of a character) and aspect
-- ratios.
--
-- If you set a geometry widget, the hints will apply to the geometry widget
-- instead of directly to the toplevel window. Of course since the geometry
-- widget is a child widget of the top level window, constraining the sizing
-- behaviour of the widget will have a knock-on effect on the sizing of the
-- toplevel window.
--
-- The @minWidth@\/@minHeight@\/@maxWidth@\/@maxHeight@ fields may be set to
-- @-1@, to use the size request of the window or geometry widget. If the
-- minimum size hint is not provided, Gtk+ will use the size requisition of the
-- window (or the geometry widget if it set) as the minimum size. The base size
-- is treated similarly.
--
-- The canonical use-case for 'windowSetGeometryHints' is to get a terminal
-- widget to resize properly. Here, the terminal text area should be the
-- geometry widget. Gtk+ will then automatically set the base size of the
-- terminal window to the size of other widgets in the terminal window, such as
-- the menubar and scrollbar. Then, the @widthInc@ and @heightInc@ values
-- should be set to the size of one character in the terminal. Finally, the
-- base size should be set to the size of one character. The net effect is that
-- the minimum size of the terminal will have a 1x1 character terminal area,
-- and only terminal sizes on the \"character grid\" will be allowed.
--
-- The other useful settings are @minAspect@ and @maxAspect@. These specify a
-- width\/height ratio as a floating point number. If a geometry widget is set,
-- the aspect applies to the geometry widget rather than the entire window. The
-- most common use of these hints is probably to set @minAspect@ and
-- @maxAspect@ to the same value, thus forcing the window to keep a constant
-- aspect ratio.
--
windowSetGeometryHints :: (WindowClass self, WidgetClass widget) =>
    self             -- ^ @window@ - the top level window
 -> Maybe widget     -- ^ @geometryWidget@ - optionall a widget the geometry
                     -- hints will be applied to rather than directly to the
                     -- top level window
 -> Maybe (Int, Int) -- ^ @(minWidth, minHeight)@ - minimum width and height
                     -- of window (or -1 to use requisition)
 -> Maybe (Int, Int) -- ^ @(maxWidth, maxHeight)@ - maximum width and height
                     -- of window (or -1 to use requisition)
 -> Maybe (Int, Int) -- ^ @(baseWidth, baseHeight)@ - the allowed window widths
                     -- are @base_width + width_inc * N@ for any int @N@.
                     -- Similarly, the allowed window widths are @base_height +
                     -- height_inc * N@ for any int @N@. For either the base
                     -- width or height -1 is allowed as described above.
 -> Maybe (Int, Int) -- ^ @(widthInc, heightInc)@ - width and height resize
                     -- increment
 -> Maybe (Double, Double) -- ^ @(minAspect, maxAspect)@ - minimum and maximum
                           -- width\/height ratio
 -> IO ()
windowSetGeometryHints self geometryWidget
  minSize maxSize baseSize incSize aspect =
  allocaBytes {# sizeof GdkGeometry #} $ \geometryPtr -> do
  minSizeFlag <- case minSize of
    Nothing -> return 0
    Just (width, height) -> do
      {# set GdkGeometry->min_width  #} geometryPtr (fromIntegral width)
      {# set GdkGeometry->min_height #} geometryPtr (fromIntegral height)
      return (fromEnum GdkHintMinSize)
  maxSizeFlag <- case maxSize of
    Nothing -> return 0
    Just (width, height) -> do
      {# set GdkGeometry->max_width  #} geometryPtr (fromIntegral width)
      {# set GdkGeometry->max_height #} geometryPtr (fromIntegral height)
      return (fromEnum GdkHintMaxSize)
  baseSizeFlag <- case baseSize of
    Nothing -> return 0
    Just (width, height) -> do
      {# set GdkGeometry->base_width  #} geometryPtr (fromIntegral width)
      {# set GdkGeometry->base_height #} geometryPtr (fromIntegral height)
      return (fromEnum GdkHintBaseSize)
  incSizeFlag <- case incSize of
    Nothing -> return 0
    Just (width, height) -> do
      {# set GdkGeometry->width_inc  #} geometryPtr (fromIntegral width)
      {# set GdkGeometry->height_inc #} geometryPtr (fromIntegral height)
      return (fromEnum GdkHintResizeInc)
  aspectFlag <- case aspect of
    Nothing -> return 0
    Just (min, max) -> do
      {# set GdkGeometry->min_aspect #} geometryPtr (realToFrac min)
      {# set GdkGeometry->max_aspect #} geometryPtr (realToFrac max)
      return (fromEnum GdkHintAspect)

  {# call gtk_window_set_geometry_hints #}
    (toWindow self)
    (maybe (Widget nullForeignPtr) toWidget geometryWidget)
    geometryPtr
    (fromIntegral $ minSizeFlag .|. maxSizeFlag .|. baseSizeFlag
                 .|. incSizeFlag .|. aspectFlag)

{# enum GdkWindowHints {underscoreToCase} #}

#if GTK_CHECK_VERSION(2,12,0)
#ifndef DISABLE_DEPRECATED
-- | Request the windowing system to make window partially transparent, with opacity 0 being fully transparent and 1 fully opaque.
-- (Values of the opacity parameter are clamped to the [0,1] range.)
-- On X11 this has any effect only on X screens with a compositing manager running.
-- See 'widgetIsComposited'. On Windows it should work always.
--
-- Note that setting a window's opacity after the window has been shown causes it to
-- flicker once on Windows.
--
-- * Available since Gtk+ version 2.12
--
windowSetOpacity :: WindowClass self => self
 -> Double  -- ^ @opacity@ - desired opacity, between 0 and 1
 -> IO ()
windowSetOpacity self opacity =
  {#call window_set_opacity #} (toWindow self) (realToFrac opacity)

-- | Fetches the requested opacity for this window. See 'windowSetOpacity'.
--
-- * Available since Gtk+ version 2.12
--
windowGetOpacity :: WindowClass self => self
 -> IO Double  -- ^ return the requested opacity for this window.
windowGetOpacity self = liftM realToFrac $
 {#call window_get_opacity#} (toWindow self)
#endif
#endif

#if GTK_CHECK_VERSION(2,10,0)
-- | Returns the group for window or the default group, if window is @Nothing@ or if window does not have an explicit window group.
--
-- * Available since Gtk+ version 2.10
--
windowGetGroup :: WindowClass self => Maybe self
 -> IO WindowGroup  -- ^ return the 'WindowGroup' for a window or the default group
windowGetGroup self =
  makeNewGObject mkWindowGroup $
  {# call window_get_group #} (maybe (Window nullForeignPtr) toWindow self)
#endif

#if GTK_CHECK_VERSION(2,20,0)
-- | Gets the type of the window. See 'WindowType'.
--
-- * Available since Gtk version 2.20
--
windowGetWindowType :: WindowClass self => self
                    -> IO WindowType  -- ^ returns the type of the window
windowGetWindowType self =
  liftM (toEnum . fromIntegral) $
  {#call gtk_window_get_window_type #}
    (toWindow self)
#endif

--------------------
-- Attributes

-- | The title of the window.
--
windowTitle :: (WindowClass self, GlibString string) => Attr self string
windowTitle = newAttr
  windowGetTitle
  windowSetTitle

-- | The type of the window.
--
-- Default value: 'WindowToplevel'
--
windowType :: WindowClass self => ReadAttr self WindowType
windowType = readAttrFromEnumProperty "type"
  {# call pure unsafe gtk_window_type_get_type #}

-- | If @True@, the window has no mimimum size. Setting this to @True@ is 99%
-- of the time a bad idea.
--
-- Default value: @False@
--
windowAllowShrink :: WindowClass self => Attr self Bool
windowAllowShrink = newAttrFromBoolProperty "allow-shrink"

-- | If @True@, users can expand the window beyond its minimum size.
--
-- Default value: @True@
--
windowAllowGrow :: WindowClass self => Attr self Bool
windowAllowGrow = newAttrFromBoolProperty "allow-grow"

-- | If @True@, users can resize the window.
--
-- Default value: @True@
--
windowResizable :: WindowClass self => Attr self Bool
windowResizable = newAttr
  windowGetResizable
  windowSetResizable

#if GTK_MAJOR_VERSION >= 3
-- | If @True@, window has a resize grip.
--
-- Default value: @True@
--
windowHasResizeGrip :: WindowClass self => Attr self Bool
windowHasResizeGrip = newAttr
  windowGetHasResizeGrip
  windowSetHasResizeGrip
#endif

-- | If @True@, the window is modal (other windows are not usable while this
-- one is up).
--
-- Default value: @False@
--
windowModal :: WindowClass self => Attr self Bool
windowModal = newAttr
  windowGetModal
  windowSetModal

#if GTK_CHECK_VERSION(2,12,0)
-- | The requested opacity of the window. See 'windowSetOpacity' for more details about window opacity.
--
-- Allowed values: [0,1]
--
-- Default values: 1
--
-- * Available since Gtk+ version 2.12
--
windowOpacity :: WindowClass self => Attr self Double
windowOpacity = newAttrFromDoubleProperty "opacity"
#endif

-- | If @focus@ is not the current focus widget, and is focusable, sets it as
-- the focus widget for the window. If @focus@ is @Nothing@, unsets the focus widget for
-- this window. To set the focus to a particular widget in the toplevel, it is
-- usually more convenient to use 'widgetGrabFocus' instead of this function.
--
windowFocus :: WindowClass self => Attr self (Maybe Widget)
windowFocus = newAttr
  windowGetFocus
  windowSetFocus

#if GTK_MAJOR_VERSION < 3
-- | (Note: this is a special-purpose function for the framebuffer port, that
-- causes Gtk+ to draw its own window border. For most applications, you want
-- 'windowSetDecorated' instead, which tells the window manager whether to draw
-- the window border.)
--
-- If this function is called on a window with setting of @True@, before it
-- is realized or showed, it will have a \"frame\" window around
-- its 'DrawWindow', accessible using 'windowGetFrame'. Using the signal
-- 'windowFrameEvent' you can receive all events targeted at the frame.
--
-- This function is used by the linux-fb port to implement managed windows,
-- but it could conceivably be used by X-programs that want to do their own
-- window decorations.
--
-- Removed in Gtk3.
windowHasFrame :: WindowClass self => Attr self Bool
windowHasFrame = newAttr
  windowGetHasFrame
  windowSetHasFrame
#endif

-- | Sets up the icon representing a 'Window'. The icon is used when the
-- window is minimized (also known as iconified). Some window managers or
-- desktop environments may also place it in the window frame, or display it in
-- other contexts.
--
-- By passing several sizes, you may improve the final image quality of the
-- icon, by reducing or eliminating automatic image scaling.
--
-- Recommended sizes to provide: 16x16, 32x32, 48x48 at minimum, and larger
-- images (64x64, 128x128) if you have them.
--
-- See also 'windowSetDefaultIconList' to set the icon for all windows in
-- your application in one go.
--
-- Note that transient windows (those who have been set transient for
-- another window using 'windowSetTransientFor') will inherit their icon from
-- their transient parent. So there's no need to explicitly set the icon on
-- transient windows.
--
windowIconList :: WindowClass self => Attr self [Pixbuf]
windowIconList = newAttr
  windowGetIconList
  windowSetIconList

-- | The mnemonic modifier for this window.
--
windowMnemonicModifier :: WindowClass self => Attr self [Modifier]
windowMnemonicModifier = newAttr
  windowGetMnemonicModifier
  windowSetMnemonicModifier

#if GTK_CHECK_VERSION(2,20,0)
windowMnemonicVisible :: WindowClass self => Attr self Bool
windowMnemonicVisible = newAttrFromBoolProperty "mnemonics-visible"
#endif

-- | Unique identifier for the window to be used when restoring a session.
--
-- Default value: "\\"
--
windowRole :: (WindowClass self, GlibString string) => Attr self string
windowRole = newAttrFromStringProperty "role"

#if GTK_CHECK_VERSION(2,12,0)
-- | The 'windowStartupId' is a write-only property for setting window's startup notification identifier.
--
-- Default value: "\\"
--
-- * Available since Gtk+ version 2.12
--
windowStartupId :: (WindowClass self, GlibString string) => Attr self string
windowStartupId = newAttrFromStringProperty "startup-id"
#endif

-- | The initial position of the window.
--
-- Default value: 'WinPosNone'
--
windowWindowPosition :: WindowClass self => Attr self WindowPosition
windowWindowPosition = newAttrFromEnumProperty "window-position"
  {# call pure unsafe gtk_window_position_get_type #}

-- | The default width of the window, used when initially showing the window.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
windowDefaultWidth :: WindowClass self => Attr self Int
windowDefaultWidth = newAttrFromIntProperty "default-width"

-- | The default height of the window, used when initially showing the window.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
windowDefaultHeight :: WindowClass self => Attr self Int
windowDefaultHeight = newAttrFromIntProperty "default-height"

-- | Whether the window frame should have a close button.
--
-- Default values: @True@
--
-- * Available since Gtk+ version 2.10
--
windowDeletable :: WindowClass self => Attr self Bool
windowDeletable = newAttrFromBoolProperty "deletable"

-- | If this window should be destroyed when the parent is destroyed.
--
-- Default value: @False@
--
windowDestroyWithParent :: WindowClass self => Attr self Bool
windowDestroyWithParent = newAttr
  windowGetDestroyWithParent
  windowSetDestroyWithParent

-- | Icon for this window.
--
windowIcon :: WindowClass self => Attr self (Maybe Pixbuf)
windowIcon = newAttr
  windowGetIcon
  windowSetIcon

-- | The 'windowIconName' property specifies the name of the themed icon to use as the window icon. See 'IconTheme' for more details.
--
-- Default values: "\\"
--
-- * Available since Gtk+ version 2.6
--
--
windowIconName :: (WindowClass self, GlibString string) => Attr self string
windowIconName = newAttrFromStringProperty "icon-name"

#if GTK_CHECK_VERSION(2,2,0)
-- | The screen where this window will be displayed.
--
windowScreen :: WindowClass self => Attr self Screen
windowScreen = newAttr
  windowGetScreen
  windowSetScreen
#endif

-- | Hint to help the desktop environment understand what kind of window this
-- is and how to treat it.
--
-- Default value: 'WindowTypeHintNormal'
--
windowTypeHint :: WindowClass self => Attr self WindowTypeHint
windowTypeHint = newAttr
  windowGetTypeHint
  windowSetTypeHint

#if GTK_CHECK_VERSION(2,2,0)
-- | @True@ if the window should not be in the task bar.
--
-- Default value: @False@
--
windowSkipTaskbarHint :: WindowClass self => Attr self Bool
windowSkipTaskbarHint = newAttr
  windowGetSkipTaskbarHint
  windowSetSkipTaskbarHint

-- | @True@ if the window should not be in the pager.
--
-- Default value: @False@
--
windowSkipPagerHint :: WindowClass self => Attr self Bool
windowSkipPagerHint = newAttr
  windowGetSkipPagerHint
  windowSetSkipPagerHint
#endif

#if GTK_CHECK_VERSION(2,8,0)
-- | @True@ if the window should be brought to the user's attention.
--
-- Default value: @False@
--
windowUrgencyHint :: WindowClass self => Attr self Bool
windowUrgencyHint = newAttr
  windowGetUrgencyHint
  windowSetUrgencyHint
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | @True@ if the window should receive the input focus.
--
-- Default value: @True@
--
windowAcceptFocus :: WindowClass self => Attr self Bool
windowAcceptFocus = newAttr
  windowGetAcceptFocus
  windowSetAcceptFocus
#endif

#if GTK_CHECK_VERSION(2,6,0)
-- | @True@ if the window should receive the input focus when mapped.
--
-- Default value: @True@
--
windowFocusOnMap :: WindowClass self => Attr self Bool
windowFocusOnMap = newAttr
  windowGetFocusOnMap
  windowSetFocusOnMap
#endif

#if GTK_CHECK_VERSION(2,4,0)
-- | Whether the window should be decorated by the window manager.
--
-- Default value: @True@
--
windowDecorated :: WindowClass self => Attr self Bool
windowDecorated = newAttr
  windowGetDecorated
  windowSetDecorated

-- | The window gravity of the window. See 'windowMove' and 'Gravity' for more
-- details about window gravity.
--
-- Default value: 'GravityNorthWest'
--
windowGravity :: WindowClass self => Attr self Gravity
windowGravity = newAttr
  windowGetGravity
  windowSetGravity
#endif

-- | Whether the input focus is within this GtkWindow.
--
-- Note: If add `window` before `HasToplevelFocus` (has-toplevel-focus attribute)
-- will conflicts with fucntion `windowHasToplevelFocus`, so we named this attribute
-- to `windowToplevelFocus`.
--
-- Default values: @False@
--
windowToplevelFocus :: WindowClass self => Attr self Bool
windowToplevelFocus = newAttrFromBoolProperty "has-toplevel-focus"

-- | \'transientFor\' property. See 'windowGetTransientFor' and
-- 'windowSetTransientFor'
--
windowTransientFor :: (WindowClass self, WindowClass parent) => ReadWriteAttr self (Maybe Window) parent
windowTransientFor = newAttr
  windowGetTransientFor
  windowSetTransientFor

--------------------
-- Signals

-- | Observe events that are emitted on the frame of this window.
--
frameEvent :: WindowClass self => Signal self (EventM EAny Bool)
frameEvent = Signal (\after obj fun ->
                     connect_PTR__BOOL "frame-event" after obj (runReaderT fun))

-- | The 'keysChanged' signal gets emitted when the set of accelerators or mnemonics that are associated with window changes.
--
keysChanged :: WindowClass self => Signal self (IO ())
keysChanged = Signal (connect_NONE__NONE "keys-changed")

-- | Observe a change in input focus.
--
setFocus :: WindowClass self => Signal self (Maybe Widget -> IO ())
setFocus = Signal (connect_MOBJECT__NONE "set-focus")

-- * Deprecated
#ifndef DISABLE_DEPRECATED
-- | Observe a change in input focus.
--
onSetFocus, afterSetFocus :: (WindowClass self, WidgetClass foc) => self
 -> (Maybe foc -> IO ())
 -> IO (ConnectId self)
onSetFocus = connect_MOBJECT__NONE "set-focus" False
afterSetFocus = connect_MOBJECT__NONE "set-focus" True

#endif
