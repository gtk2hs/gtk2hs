-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget ScrolledWindow@
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.6 $ from $Date: 2003/07/09 22:42:45 $
--
--  Copyright (c) 1999..2002 Axel Simon
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
-- * @ref data ScrolledWindow@ is a container that adds scroll bars to its 
--   child. Some
--   widgets have native scrolling support, in which case the scrolling action
--   is performed by the child itself (e.g. a TreeView widget does this by only
--   moving the table part and not the titles of a table). If a widget does
--   not support native scrolling it can be put into a 
--   @ref data ScrolledWindow@ widget.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

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
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(PolicyType(..), CornerType(..), ShadowType(..))
import Maybe    (fromMaybe)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor scrolledWindowNew@ Create a new @ref data ScrolledWindow@.
--
scrolledWindowNew :: Maybe Adjustment -> Maybe Adjustment -> IO ScrolledWindow
scrolledWindowNew hAdj vAdj = makeNewObject mkScrolledWindow $ liftM castPtr $
  {#call unsafe scrolled_window_new#} (fromMAdj hAdj) (fromMAdj vAdj)
 where
 fromMAdj :: Maybe Adjustment -> Adjustment
 fromMAdj = fromMaybe $ mkAdjustment nullForeignPtr

-- @method scrolledWindowGetHAdjustment@ Retrieve the horizontal
-- @ref data Adjustment@ of the @ref data ScrolledWindow@.
--
scrolledWindowGetHAdjustment :: ScrolledWindowClass w => w -> IO Adjustment
scrolledWindowGetHAdjustment w = makeNewObject mkAdjustment $
  {#call unsafe scrolled_window_get_hadjustment#} (toScrolledWindow w)

-- @method scrolledWindowGetVAdjustment@ Retrieve the vertical
-- @ref data Adjustment@ of the @ref data ScrolledWindow@.
--
scrolledWindowGetVAdjustment :: ScrolledWindowClass w => w -> IO Adjustment
scrolledWindowGetVAdjustment w = makeNewObject mkAdjustment $
  {#call unsafe scrolled_window_get_vadjustment#} (toScrolledWindow w)

-- @method scrolledWindowSetPolicy@ Specify if the scrollbars should vanish if
-- the child size is sufficiently small.
--
scrolledWindowSetPolicy :: ScrolledWindowClass w => w -> PolicyType ->
                           PolicyType -> IO ()
scrolledWindowSetPolicy w hPol vPol = {#call scrolled_window_set_policy#}
  (toScrolledWindow w) ((fromIntegral.fromEnum) hPol) 
  ((fromIntegral.fromEnum) vPol)

-- @method scrolledWindowAddWithViewport@ Add a child widget without native
-- scrolling support to this @ref data ScrolledWindow@.
--
scrolledWindowAddWithViewport :: (ScrolledWindowClass w, WidgetClass wid) => 
                                 w -> wid -> IO ()
scrolledWindowAddWithViewport w wid = 
  {#call scrolled_window_add_with_viewport#} (toScrolledWindow w) 
  (toWidget wid)

-- @method scrolledWindowSetPlacement@ Specify where the scrollbars should be
-- placed.
--
scrolledWindowSetPlacement :: ScrolledWindowClass w => w -> CornerType -> IO ()
scrolledWindowSetPlacement w ct =
  {#call scrolled_window_set_placement#} (toScrolledWindow w)
  ((fromIntegral.fromEnum) ct)

-- @method scrolledWindowSetShadowType@ Specify if and how an outer frame
-- should be drawn around the child.
--
scrolledWindowSetShadowType :: ScrolledWindowClass w => w -> ShadowType ->
                               IO ()
scrolledWindowSetShadowType w st = {#call scrolled_window_set_shadow_type#}
  (toScrolledWindow w) ((fromIntegral.fromEnum) st)

-- @method scrolledWindowSetHAdjustment@ Set the horizontal
-- @ref data Adjustment@ of the @ref data ScrolledWindow@.
--
scrolledWindowSetHAdjustment :: ScrolledWindowClass w => w -> Adjustment ->
                                IO ()
scrolledWindowSetHAdjustment w adj = {#call scrolled_window_set_hadjustment#}
  (toScrolledWindow w) adj

-- @method scrolledWindowSetVAdjustment@ Set the vertical @ref data Adjustment@
-- of the @ref data ScrolledWindow@.
--
scrolledWindowSetVAdjustment :: ScrolledWindowClass w => w -> Adjustment ->
                                IO ()
scrolledWindowSetVAdjustment w adj = {#call scrolled_window_set_hadjustment#}
  (toScrolledWindow w) adj


