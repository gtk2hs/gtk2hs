-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: abstract (usually toplevel) Window
--
--  Author : Manuel M. T. Chakravarty, Axel Simon
--          
--  Created: 27 April 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
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

-- set window resizing geometry (EXPORTED)
--

-- set the window position policy (EXPORTED)
--
windowSetPosition :: WindowClass w => WindowPosition -> w -> IO ()
windowSetPosition pos w = 
  {#call window_set_position#} (toWindow w) ((fromIntegral.fromEnum) pos)

-- set transient window (EXPORTED)
--
windowSetTransientFor :: (WindowClass win, WindowClass parent) 
			 => parent -> win -> IO ()
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
  



