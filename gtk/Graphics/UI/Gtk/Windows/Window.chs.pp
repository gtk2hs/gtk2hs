-- -*-haskell-*-
--  GIMP Toolkit (GTK) Window
--
--  Author : Manuel M. T. Chakravarty, Axel Simon
--
--  Created: 27 April 2001
--
--  Version $Revision: 1.3 $ from $Date: 2005/02/25 01:11:37 $
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

-- * Signals
  onFrameEvent,
  afterFrameEvent,
  onSetFocus,
  afterSetFocus
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.General.Enums	(WindowType(WindowToplevel), WindowPosition(..))
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.Gdk.Events	(Event, marshalEvent)

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new window of the given type.
--
windowNew :: IO Window
windowNew = makeNewObject mkWindow $ liftM castPtr $
  {#call window_new#} ((fromIntegral.fromEnum) WindowToplevel)

--------------------
-- Methods

-- | set the title string of the given window
--
windowSetTitle :: WindowClass w => w -> String -> IO ()
windowSetTitle w str = 
  withUTFString str ({#call window_set_title#} (toWindow w))

-- | Sets whether the user can resize a window.
--
-- * Windows are user resizable by default.
--
windowSetResizable :: WindowClass w => w -> Bool -> IO ()
windowSetResizable w res = 
  {#call window_set_resizable#} (toWindow w) (fromBool res)

-- | Retrieve the value set by
-- 'windowSetResizable'.
--
windowGetResizable :: WindowClass w => w -> IO Bool
windowGetResizable w = liftM toBool $ 
  {#call unsafe window_get_resizable#} (toWindow w)

-- | dunno
--
windowActivateFocus :: WindowClass w => w -> IO Bool
windowActivateFocus w = 
  liftM toBool $ {#call window_activate_focus#} (toWindow w)

-- | dunno
--
windowActivateDefault :: WindowClass w => w -> IO Bool
windowActivateDefault w = 
  liftM toBool $ {#call window_activate_default#} (toWindow w)

#ifndef DISABLE_DEPRECATED
{-# DEPRECATED windowSetPolicy "Use windowSetResizable instead." #-}
-- windowSetPolicy: set the window policy
--
windowSetPolicy :: WindowClass w => w -> Bool -> Bool -> Bool -> IO ()
windowSetPolicy w shrink grow auto = {#call window_set_policy#} 
  (toWindow w) (fromBool shrink) (fromBool grow) (fromBool auto)
#endif

-- | make a window application modal
--
windowSetModal :: WindowClass w => w -> Bool -> IO ()
windowSetModal w m = {#call window_set_modal#} (toWindow w) (fromBool m)

-- | set window default size
--
-- * Sets the default size of a window. If the window's \"natural\" size (its
--   size request) is larger than the default, the default will be ignored.
--   More generally, if the default size does not obey the geometry hints for
--   the window ('windowSetGeometryHints' can be used to set these
--   explicitly), the default size will be clamped to the nearest permitted
--   size.
--
-- * Unlike @widgetSetSizeRequest@, which sets a size request for a
--   widget and thus would keep users from shrinking the window, this function
--   only sets the initial size, just as if the user had resized the window
--   themselves. Users can still shrink the window again as they normally
--   would. Setting a default size of -1 means to use the \"natural\" default
--   size (the size request of the window).
--
-- * For more control over a window's initial size and how resizing works,
--   investigate 'windowSetGeometryHints'.
--
-- * For some uses, 'windowResize' is a more appropriate function.
--   'windowResize' changes the current size of the window, rather
--   than the size to be used on initial display. 'windowResize'
--   always affects the window itself, not the geometry widget.The default
--   size of a window only affects the first time a window is shown; if a
--   window is hidden and re-shown, it will remember the size it had prior to
--   hiding, rather than using the default size. Windows can't actually be 0x0
--   in size, they must be at least 1x1, but passing 0 for width and height is
--   OK, resulting in a 1x1 default size.
--
windowSetDefaultSize :: WindowClass w => w -> Int -> Int -> IO ()
windowSetDefaultSize w height width =
  {#call window_set_default_size#} (toWindow w) (fromIntegral height)
  (fromIntegral width)

-- | set the window position policy
--
windowSetPosition :: WindowClass w => w -> WindowPosition -> IO ()
windowSetPosition w pos = 
  {#call window_set_position#} (toWindow w) ((fromIntegral.fromEnum) pos)

-- | set transient window
--
windowSetTransientFor :: (WindowClass win, WindowClass parent) => win ->
                         parent -> IO ()
windowSetTransientFor w p = 
  {#call window_set_transient_for#} (toWindow w) (toWindow p)

-- | destory transient window with parent
--
windowSetDestroyWithParent :: WindowClass w => w -> Bool -> IO ()
windowSetDestroyWithParent w b = 
  {#call window_set_destroy_with_parent#} (toWindow w) (fromBool b)

-- | restore the window
--
windowDeiconify :: WindowClass w => w -> IO ()
windowDeiconify w = {#call window_deiconify#} (toWindow w)

-- | minimize the window
--
windowIconify :: WindowClass w => w -> IO ()
windowIconify w = {#call window_iconify#} (toWindow w)

-- | maximize the window
--
windowMaximize :: WindowClass w => w -> IO ()
windowMaximize w = {#call window_maximize#} (toWindow w)

-- | unmaximize the window
--
windowUnmaximize :: WindowClass w => w -> IO ()
windowUnmaximize w = {#call window_unmaximize#} (toWindow w)

-- | remove the border
--
windowSetDecorated :: WindowClass w => w -> Bool -> IO ()
windowSetDecorated w b =
  {#call window_set_decorated#} (toWindow w) (fromBool b)

-- | set border widths
--
windowSetFrameDimensions :: WindowClass w => w -> Int -> Int -> Int -> Int ->
                            IO ()
windowSetFrameDimensions w left top right bottom =
 {#call window_set_frame_dimensions#} (toWindow w) (fromIntegral left)
 (fromIntegral top) (fromIntegral right) (fromIntegral bottom)

-- | set role (additional window name for the WM)
--
windowSetRole :: WindowClass w => w -> String -> IO ()
windowSetRole w str = 
  withUTFString str ({#call window_set_role#} (toWindow w))

-- | show the window on every workspace
--
windowStick :: WindowClass w => w -> IO ()
windowStick w = {#call window_stick#} (toWindow w)

-- | do not show the window on every workspace
--
windowUnstick :: WindowClass w => w -> IO ()
windowUnstick w = {#call window_unstick#} (toWindow w)

--------------------
-- Signals

-- | 
--
onFrameEvent, afterFrameEvent :: WindowClass w => w -> (Event -> IO Bool) ->
                                 IO (ConnectId w)
onFrameEvent = connect_BOXED__BOOL "frame_event" marshalEvent False
afterFrameEvent = connect_BOXED__BOOL "frame_event" marshalEvent True

-- | 
--
onSetFocus, afterSetFocus :: (WindowClass w, WidgetClass foc) => w ->
                             (foc -> IO ()) -> IO (ConnectId w)
onSetFocus = connect_OBJECT__NONE "set_focus" False
afterSetFocus = connect_OBJECT__NONE "set_focus" True



