-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry abstract (usually toplevel) Window@
--
--  Author : Manuel M. T. Chakravarty, Axel Simon
--          
--  Created: 27 April 2001
--
--  Version $Revision: 1.4 $ from $Date: 2002/07/19 09:09:31 $
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
-- @description@ --------------------------------------------------------------
--
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
--
-- * missing but possibly useful methods are commented out
--      
module Window(
  Window,
  WindowClass,
  castToWindow,
  windowNew,
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
  windowSetPolicy,
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
  onFrameEvent,
  afterFrameEvent,
  onSetFocus,
  afterSetFocus
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

-- @dunno@create a new window of the given type

windowNew :: IO Window
windowNew = makeNewObject mkWindow $ liftM castPtr $
  {#call window_new#} ((fromIntegral.fromEnum) WindowToplevel)

-- @method windowSetTitle@ set the title string of the given window
--
windowSetTitle :: WindowClass w => w -> String -> IO ()
windowSetTitle w str = 
  withCString str ({#call window_set_title#} (toWindow w))

-- @method windowSetResizable@ Sets whether the user can resize a window.
--
-- * Windows are user resizable by default.
--
windowSetResizable :: WindowClass w => w -> Bool -> IO ()
windowSetResizable w res = 
  {#call window_set_resizable#} (toWindow w) (fromBool res)

-- @method windowGetResizable@ Retrieve the value set by
-- @ref method windowSetResizable@.
--
windowGetResizable :: WindowClass w => w -> IO Bool
windowGetResizable w = liftM toBool $ 
  {#call unsafe window_get_resizable#} (toWindow w)

-- @method windowActivateFocus@ dunno
--
windowActivateFocus :: WindowClass w => w -> IO Bool
windowActivateFocus w = 
  liftM toBool $ {#call window_activate_focus#} (toWindow w)

-- @method windowActivateDefault@ dunno
--
windowActivateDefault :: WindowClass w => w -> IO Bool
windowActivateDefault w = 
  liftM toBool $ {#call window_activate_default#} (toWindow w)

{-# DEPRECATED windowSetPolicy "Use windowSetResizable instead." #-}
-- windowSetPolicy: set the window policy
--
windowSetPolicy :: WindowClass w => w -> Bool -> Bool -> Bool -> IO ()
windowSetPolicy w shrink grow auto = {#call window_set_policy#} 
  (toWindow w) (fromBool shrink) (fromBool grow) (fromBool auto)

-- @method windowSetModal@ make a window application modal
--
windowSetModal :: WindowClass w => w -> Bool -> IO ()
windowSetModal w m = {#call window_set_modal#} (toWindow w) (fromBool m)

-- @method windowSetDefaultSize@ set window default size
--
-- * Sets the default size of a window. If the window's "natural" size (its
--   size request) is larger than the default, the default will be ignored.
--   More generally, if the default size does not obey the geometry hints for
--   the window (@method windowSetGeometryHints@ can be used to set these
--   explicitly), the default size will be clamped to the nearest permitted
--   size.
--
-- * Unlike @ref arg widgetSetSizeRequest@, which sets a size request for a
--   widget and thus would keep users from shrinking the window, this function
--   only sets the initial size, just as if the user had resized the window
--   themselves. Users can still shrink the window again as they normally
--   would. Setting a default size of -1 means to use the "natural" default
--   size (the size request of the window).
--
-- * For more control over a window's initial size and how resizing works,
--   investigate @ref method windowSetGeometryHints@.
--
-- * For some uses, @ref method windowResize@ is a more appropriate function.
--   @ref method windowResize@ changes the current size of the window, rather
--   than the size to be used on initial display. @ref method windowResize@
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

-- @method windowSetPosition@ set the window position policy
--
windowSetPosition :: WindowClass w => w -> WindowPosition -> IO ()
windowSetPosition w pos = 
  {#call window_set_position#} (toWindow w) ((fromIntegral.fromEnum) pos)

-- @method windowSetTransientFor@ set transient window
--
windowSetTransientFor :: (WindowClass win, WindowClass parent) => win ->
                         parent -> IO ()
windowSetTransientFor w p = 
  {#call window_set_transient_for#} (toWindow w) (toWindow p)

-- @method windowSetDestroyWithParent@ destory transient window with parent
--
windowSetDestroyWithParent :: WindowClass w => w -> Bool -> IO ()
windowSetDestroyWithParent w b = 
  {#call window_set_destroy_with_parent#} (toWindow w) (fromBool b)

-- @method windowDeiconify@ restore the window
--
windowDeiconify :: WindowClass w => w -> IO ()
windowDeiconify w = {#call window_deiconify#} (toWindow w)

-- @method windowIconify@ minimize the window
--
windowIconify :: WindowClass w => w -> IO ()
windowIconify w = {#call window_iconify#} (toWindow w)

-- @method windowMaximize@ maximize the window
--
windowMaximize :: WindowClass w => w -> IO ()
windowMaximize w = {#call window_maximize#} (toWindow w)

-- @method windowUnmaximize@ unmaximize the window
--
windowUnmaximize :: WindowClass w => w -> IO ()
windowUnmaximize w = {#call window_unmaximize#} (toWindow w)

-- @method windowSetDecorated@ remove the border
--
windowSetDecorated :: WindowClass w => w -> Bool -> IO ()
windowSetDecorated w b =
  {#call window_set_decorated#} (toWindow w) (fromBool b)

-- @method windowSetFrameDimensions@ set border widths
--
windowSetFrameDimensions :: WindowClass w => w -> Int -> Int -> Int -> Int ->
                            IO ()
windowSetFrameDimensions w left top right bottom =
 {#call window_set_frame_dimensions#} (toWindow w) (fromIntegral left)
 (fromIntegral top) (fromIntegral right) (fromIntegral bottom)

-- @method windowSetRole@ set role (additional window name for the WM)
--
windowSetRole :: WindowClass w => w -> String -> IO ()
windowSetRole w str = 
  withCString str ({#call window_set_role#} (toWindow w))

-- @method windowStick@ show the window on every workspace
--
windowStick :: WindowClass w => w -> IO ()
windowStick w = {#call window_stick#} (toWindow w)

-- @method windowUnstick@ do not show the window on every workspace
--
windowUnstick :: WindowClass w => w -> IO ()
windowUnstick w = {#call window_unstick#} (toWindow w)

-- signals

-- @signal connectToFrameEvent@ 
--
onFrameEvent, afterFrameEvent :: WindowClass w => w -> (Event -> IO Bool) ->
                                 IO (ConnectId w)
onFrameEvent = connect_BOXED__BOOL "frame_event" marshalEvent False
afterFrameEvent = connect_BOXED__BOOL "frame_event" marshalEvent True

-- @signal connectToSetFocus@ 
--
onSetFocus, afterSetFocus :: (WindowClass w, WidgetClass foc) => w ->
                             (foc -> IO ()) -> IO (ConnectId w)
onSetFocus = connect_OBJECT__NONE "set_focus" False
afterSetFocus = connect_OBJECT__NONE "set_focus" True



