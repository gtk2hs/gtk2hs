-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget ScrolledWindow
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
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
-- * @ScrolledWindow is a container that adds scroll bars to its child. Some
--   widgets have native scrolling support, in which case the scrolling action
--   is performed by the child itself (e.g. a TreeView widget does this by only
--   moving the table part and not the titles of a table). If a widget does
--   not support native scrolling it has to be put into a @TreeView widget.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module ScrolledWindow(
  ScrolledWindow,
  ScrolledWindowClass,
  castToScrolledWindow,
  scrolledWindowNew,
  scrolledWindowGetHAdjustment,
  scrolledWindowGetVAdjustment,
  PolicyType(..),
  scrolledWindowSetPolicy,
  scrolledWindowAddWithViewport,
  CornerType(..),
  scrolledWindowSetPlacement,
  ShadowType(..),
  scrolledWindowSetShadowType,
  scrolledWindowSetHAdjustment,
  scrolledWindowSetVAdjustment,
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(PolicyType(..), CornerType(..), ShadowType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a new @ScrolledWindow. (EXPORTED)
--
scrolledWindowNew :: Adjustment -> Adjustment -> IO ScrolledWindow
scrolledWindowNew hAdj vAdj = makeNewObject mkScrolledWindow $ liftM castPtr $
  {#call unsafe scrolled_window_new#} hAdj vAdj

-- Retrieve the horizontal @Adjustment of the @ScrolledWindow. (EXPORTED)
--
scrolledWindowGetHAdjustment :: ScrolledWindowClass w => w -> IO Adjustment
scrolledWindowGetHAdjustment w = makeNewObject mkAdjustment $
  {#call unsafe scrolled_window_get_hadjustment#} (toScrolledWindow w)

-- Retrieve the vertical @Adjustment of the @ScrolledWindow. (EXPORTED)
--
scrolledWindowGetVAdjustment :: ScrolledWindowClass w => w -> IO Adjustment
scrolledWindowGetVAdjustment w = makeNewObject mkAdjustment $
  {#call unsafe scrolled_window_get_vadjustment#} (toScrolledWindow w)

-- Specify if the scrollbars should vanish if the child size is sufficiently
-- small. (EXPORTED)
--
scrolledWindowSetPolicy :: ScrolledWindowClass w => 
  PolicyType -> PolicyType -> w -> IO ()
scrolledWindowSetPolicy hPol vPol w = {#call scrolled_window_set_policy#}
  (toScrolledWindow w) ((fromIntegral.fromEnum) hPol) 
  ((fromIntegral.fromEnum) vPol)

-- Add a child widget without native scrolling support to this @ScrolledWindow.
-- (EXPORTED)
--
scrolledWindowAddWithViewport :: (ScrolledWindowClass w, WidgetClass wid) =>
  wid -> w -> IO ()
scrolledWindowAddWithViewport wid w = 
  {#call scrolled_window_add_with_viewport#} (toScrolledWindow w) 
  (toWidget wid)

-- Specify where the scrollbars should be placed. (EXPORTED)
--
scrolledWindowSetPlacement :: ScrolledWindowClass w =>
  CornerType -> w -> IO ()
scrolledWindowSetPlacement ct w =
  {#call scrolled_window_set_placement#} (toScrolledWindow w)
  ((fromIntegral.fromEnum) ct)

-- Specify if and how an outer frame should be drawn around the child. 
-- (EXPORTED)
--
scrolledWindowSetShadowType :: ScrolledWindowClass w => 
  ShadowType -> w -> IO ()
scrolledWindowSetShadowType st w = {#call scrolled_window_set_shadow_type#}
  (toScrolledWindow w) ((fromIntegral.fromEnum) st)

-- Set the horizontal @Adjustment of the @ScrolledWindow. (EXPORTED)
--
scrolledWindowSetHAdjustment :: ScrolledWindowClass w => 
  Adjustment -> w -> IO ()
scrolledWindowSetHAdjustment adj w = {#call scrolled_window_set_hadjustment#}
  (toScrolledWindow w) adj

-- Set the vertical @Adjustment of the @ScrolledWindow. (EXPORTED)
--
scrolledWindowSetVAdjustment :: ScrolledWindowClass w => 
  Adjustment -> w -> IO ()
scrolledWindowSetVAdjustment adj w = {#call scrolled_window_set_hadjustment#}
  (toScrolledWindow w) adj


