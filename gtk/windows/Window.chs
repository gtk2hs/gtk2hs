-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: abstract (usually toplevel) Window
--
--  Author : Manuel M. T. Chakravarty, Axel Simon
--          
--  Created: 27 April 2001
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/04 14:02:30 $
--
--  Copyright (c) 2001 Manuel M. T. Chakravarty, Axel Simon
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
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------
--
-- * missing but possibly useful methods are commented out
--      
module Window(
  Window,
  WindowClass,
  castToWindow,
  windowNew,
  windowSetTitle,
--  windowAddAccelGroup, 
--  windowRemoveAccelGroup,
  windowActivateFocus,
  windowActivateDefault,
  windowSetModal,
  windowSetDefaultSize,
--  windowSetGeometryHints,
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
  connectToFrameEvent,
  connectToSetFocus
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Enums	(WindowType(WindowToplevel), WindowPosition(..))
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Events	(Event, marshalEvent)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- create a new window of the given type (EXPORTED)
--

windowNew :: IO Window
windowNew = makeNewObject mkWindow $ liftM castPtr $
  {#call window_new#} ((fromIntegral.fromEnum) WindowToplevel)

-- set the title string of the given window (EXPORTED)
--
windowSetTitle :: WindowClass w => String -> w -> IO ()
windowSetTitle str w = 
  withCString str ({#call window_set_title#} (toWindow w))

-- dunno (EXPORTED)
--
windowActivateFocus :: WindowClass w => w -> IO Bool
windowActivateFocus w = 
  liftM toBool $ {#call window_activate_focus#} (toWindow w)

-- dunno (EXPORTED)
--
windowActivateDefault :: WindowClass w => w -> IO Bool
windowActivateDefault w = 
  liftM toBool $ {#call window_activate_default#} (toWindow w)

-- make a window application modal (EXPORTED)
--
windowSetModal :: WindowClass w => Bool -> w -> IO ()
windowSetModal m w = {#call window_set_modal#} (toWindow w) (fromBool m)

-- set window default size (EXPORTED)
--
-- * Sets the default size of a window. If the window's "natural" size (its size request) is larger than the default, the default will be ignored. More generally, if the default size does not obey the geometry hints for the window (@windowSetGeometryHints can be used to set these explicitly), the default size will be clamped to the nearest permitted size.
--
-- * Unlike @widgetSetSizeRequest, which sets a size request for a widget and thus would keep users from shrinking the window, this function only sets the initial size, just as if the user had resized the window themselves. Users can still shrink the window again as they normally would. Setting a default size of -1 means to use the "natural" default size (the size request of the window).
--
-- * For more control over a window's initial size and how resizing works, investigate @windowSetGeometryHints.
--
-- * For some uses, @windowResize is a more appropriate function. @windowResize changes the current size of the window, rather than the size to be used on initial display. @windowResize always affects the window itself, not the geometry widget.The default size of a window only affects the first time a window is shown; if a window is hidden and re-shown, it will remember the size it had prior to hiding, rather than using the default size. Windows can't actually be 0x0 in size, they must be at least 1x1, but passing 0 for width and height is OK, resulting in a 1x1 default size.
--
windowSetDefaultSize :: WindowClass w => Int -> Int -> w -> IO ()
windowSetDefaultSize height width w =
  {#call window_set_default_size#} (toWindow w) (fromIntegral height)
  (fromIntegral width)

-- set the window position policy (EXPORTED)
--
windowSetPosition :: WindowClass w => WindowPosition -> w -> IO ()
windowSetPosition pos w = 
  {#call window_set_position#} (toWindow w) ((fromIntegral.fromEnum) pos)

-- set transient window (EXPORTED)
--
windowSetTransientFor :: (WindowClass win, WindowClass parent) => 
			 parent -> win -> IO ()
windowSetTransientFor p w = 
  {#call window_set_transient_for#} (toWindow w) (toWindow p)

-- destory transient window with parent (EXPORTED)
--
windowSetDestroyWithParent :: WindowClass w => Bool -> w -> IO ()
windowSetDestroyWithParent b w = 
  {#call window_set_destroy_with_parent#} (toWindow w) (fromBool b)

-- restore the window (EXPORTED)
--
windowDeiconify :: WindowClass w => w -> IO ()
windowDeiconify w = {#call window_deiconify#} (toWindow w)

-- minimize the window (EXPORTED)
--
windowIconify :: WindowClass w => w -> IO ()
windowIconify w = {#call window_iconify#} (toWindow w)

-- maximize the window (EXPORTED)
--
windowMaximize :: WindowClass w => w -> IO ()
windowMaximize w = {#call window_maximize#} (toWindow w)

-- unmaximize the window (EXPORTED)
--
windowUnmaximize :: WindowClass w => w -> IO ()
windowUnmaximize w = {#call window_unmaximize#} (toWindow w)

-- remove the border (EXPORTED)
--
windowSetDecorated :: WindowClass w => Bool -> w -> IO ()
windowSetDecorated b w =
  {#call window_set_decorated#} (toWindow w) (fromBool b)

-- set border widths (EXPORTED)
--
windowSetFrameDimensions :: WindowClass w =>
			    Int -> Int -> Int -> Int -> w -> IO ()
windowSetFrameDimensions left top right bottom w =
 {#call window_set_frame_dimensions#} (toWindow w) (fromIntegral left)
 (fromIntegral top) (fromIntegral right) (fromIntegral bottom)

-- set role (additional window name for the WM) (EXPORTED)
--
windowSetRole :: WindowClass w => String -> w -> IO ()
windowSetRole str w = 
  withCString str ({#call window_set_role#} (toWindow w))

-- show the window on every workspace (EXPORTED)
--
windowStick :: WindowClass w => w -> IO ()
windowStick w = {#call window_stick#} (toWindow w)

-- do not show the window on every workspace (EXPORTED)
--
windowUnstick :: WindowClass w => w -> IO ()
windowUnstick w = {#call window_unstick#} (toWindow w)

-- signals

connectToFrameEvent :: WindowClass w => 
  (Event -> IO Bool) -> ConnectAfter -> w -> IO (ConnectId w)
connectToFrameEvent = connect_BOXED__BOOL "frame_event" marshalEvent

connectToSetFocus :: (WindowClass w, WidgetClass foc) =>
  (foc -> IO ()) -> ConnectAfter -> w -> IO (ConnectId w)
connectToSetFocus = connect_OBJECT__NONE "set_focus"
  



