-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ScrolledWindow
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.5 $ from $Date: 2005/03/13 19:34:37 $
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- 'ScrolledWindow' is a container that adds scroll bars to its child
--
module Graphics.UI.Gtk.Scrolling.ScrolledWindow (
-- * Description
-- 
-- | 'ScrolledWindow' is a 'Bin' subclass: it's a container the accepts a
-- single child widget. 'ScrolledWindow' adds scrollbars to the child widget
-- and optionally draws a beveled frame around the child widget.
--
-- The scrolled window can work in two ways. Some widgets have native
-- scrolling support; these widgets have \"slots\" for 'Adjustment' objects.
-- Widgets with native scroll support include 'TreeView', 'TextView', and
-- 'Layout'.
--
-- For widgets that lack native scrolling support, the 'Viewport' widget
-- acts as an adaptor class, implementing scrollability for child widgets that
-- lack their own scrolling capabilities. Use 'Viewport' to scroll child
-- widgets such as 'Table', 'Box', and so on.
--
-- If a widget has native scrolling abilities, it can be added to the
-- 'ScrolledWindow' with 'containerAdd'. If a widget does not, you must first
-- add the widget to a 'Viewport', then add the 'Viewport' to the scrolled
-- window. The convenience function 'scrolledWindowAddWithViewport' does
-- exactly this, so you can ignore the presence of the viewport.
--
-- The position of the scrollbars is controlled by the scroll adjustments.
-- See 'Adjustment' for the fields in an adjustment - for 'Scrollbar', used by
-- 'ScrolledWindow', the \"value\" field represents the position of the
-- scrollbar, which must be between the \"lower\" field and \"upper -
-- page_size.\" The \"page_size\" field represents the size of the visible
-- scrollable area. The \"step_increment\" and \"page_increment\" fields are
-- used when the user asks to step down (using the small stepper arrows) or
-- page down (using for example the PageDown key).
--
-- If a 'ScrolledWindow' doesn't behave quite as you would like, or doesn't
-- have exactly the right layout, it's very possible to set up your own
-- scrolling with 'Scrollbar' and for example a 'Table'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----ScrolledWindow
-- @

-- * Types
  ScrolledWindow,
  ScrolledWindowClass,
  castToScrolledWindow,

-- * Constructors
  scrolledWindowNew,

-- * Methods
  scrolledWindowGetHAdjustment,
  scrolledWindowGetVAdjustment,
  PolicyType(..),
  scrolledWindowSetPolicy,
  scrolledWindowGetPolicy,
  scrolledWindowAddWithViewport,
  CornerType(..),
  scrolledWindowSetPlacement,
  scrolledWindowGetPlacement,
  ShadowType(..),
  scrolledWindowSetShadowType,
  scrolledWindowGetShadowType,
  scrolledWindowSetHAdjustment,
  scrolledWindowSetVAdjustment,

-- * Properties
  scrolledWindowHAdjustment,
  scrolledWindowVAdjustment,
  scrolledWindowShadowType,
  scrolledWindowPlacement
  ) where

import Monad	(liftM)
import Maybe    (fromMaybe)

import System.Glib.FFI
import System.Glib.Attributes		(Attr(..))
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(PolicyType(..), CornerType(..), ShadowType(..))

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Create a new 'ScrolledWindow'.
--
scrolledWindowNew :: Maybe Adjustment -> Maybe Adjustment -> IO ScrolledWindow
scrolledWindowNew hAdj vAdj = makeNewObject mkScrolledWindow $ liftM castPtr $
  {#call unsafe scrolled_window_new#} (fromMAdj hAdj) (fromMAdj vAdj)
 where
 fromMAdj :: Maybe Adjustment -> Adjustment
 fromMAdj = fromMaybe $ mkAdjustment nullForeignPtr

--------------------
-- Methods

-- | Retrieve the horizontal 'Adjustment' of the 'ScrolledWindow'.
--
scrolledWindowGetHAdjustment :: ScrolledWindowClass w => w -> IO Adjustment
scrolledWindowGetHAdjustment w = makeNewObject mkAdjustment $
  {#call unsafe scrolled_window_get_hadjustment#} (toScrolledWindow w)

-- | Retrieve the vertical 'Adjustment' of the 'ScrolledWindow'.
--
scrolledWindowGetVAdjustment :: ScrolledWindowClass w => w -> IO Adjustment
scrolledWindowGetVAdjustment w = makeNewObject mkAdjustment $
  {#call unsafe scrolled_window_get_vadjustment#} (toScrolledWindow w)

-- | Specify if the scrollbars should vanish if the child size is sufficiently
-- small.
--
scrolledWindowSetPolicy :: ScrolledWindowClass w => w -> PolicyType ->
                           PolicyType -> IO ()
scrolledWindowSetPolicy w hPol vPol = {#call scrolled_window_set_policy#}
  (toScrolledWindow w) ((fromIntegral.fromEnum) hPol) 
  ((fromIntegral.fromEnum) vPol)

-- | Retrieves the current policy values for the horizontal and vertical
-- scrollbars.
--
scrolledWindowGetPolicy :: ScrolledWindowClass w => w
                        -> IO (PolicyType, PolicyType)
scrolledWindowGetPolicy w =
  alloca $ \hPolPtr -> alloca $ \vPolPtr -> do
  {#call unsafe scrolled_window_get_policy#} (toScrolledWindow w)
    hPolPtr vPolPtr
  hPol <- liftM (toEnum.fromIntegral) $ peek hPolPtr
  vPol <- liftM (toEnum.fromIntegral) $ peek vPolPtr
  return (hPol, vPol)

-- | Add a child widget without native scrolling support to this
-- 'ScrolledWindow'.
--
scrolledWindowAddWithViewport :: (ScrolledWindowClass w, WidgetClass wid) => 
                                 w -> wid -> IO ()
scrolledWindowAddWithViewport w wid = 
  {#call scrolled_window_add_with_viewport#} (toScrolledWindow w) 
  (toWidget wid)

-- | Specify where the scrollbars should be placed.
--
scrolledWindowSetPlacement :: ScrolledWindowClass w => w -> CornerType -> IO ()
scrolledWindowSetPlacement w ct =
  {#call scrolled_window_set_placement#} (toScrolledWindow w)
  ((fromIntegral.fromEnum) ct)

-- | Gets the placement of the scrollbars for the scrolled window.
--
scrolledWindowGetPlacement :: ScrolledWindowClass w => w -> IO CornerType
scrolledWindowGetPlacement w = liftM (toEnum.fromIntegral) $
  {#call unsafe scrolled_window_get_placement#} (toScrolledWindow w)

-- | Specify if and how an outer frame should be drawn around the child.
--
scrolledWindowSetShadowType :: ScrolledWindowClass w => w -> ShadowType ->
                               IO ()
scrolledWindowSetShadowType w st = {#call scrolled_window_set_shadow_type#}
  (toScrolledWindow w) ((fromIntegral.fromEnum) st)

-- | Gets the shadow type of the scrolled window.
--
scrolledWindowGetShadowType :: ScrolledWindowClass w => w -> IO ShadowType
scrolledWindowGetShadowType w = liftM (toEnum.fromIntegral) $
  {#call unsafe scrolled_window_get_shadow_type#} (toScrolledWindow w)

-- | Set the horizontal 'Adjustment' of the 'ScrolledWindow'.
--
scrolledWindowSetHAdjustment :: ScrolledWindowClass w => w -> Adjustment ->
                                IO ()
scrolledWindowSetHAdjustment w adj = {#call scrolled_window_set_hadjustment#}
  (toScrolledWindow w) adj

-- | Set the vertical 'Adjustment' of the 'ScrolledWindow'.
--
scrolledWindowSetVAdjustment :: ScrolledWindowClass w => w -> Adjustment ->
                                IO ()
scrolledWindowSetVAdjustment w adj = {#call scrolled_window_set_vadjustment#}
  (toScrolledWindow w) adj

--------------------
-- Properties

-- | The 'Adjustment' for the horizontal position.
--
scrolledWindowHAdjustment :: Attr ScrolledWindow Adjustment
scrolledWindowHAdjustment = Attr 
  scrolledWindowGetHAdjustment
  scrolledWindowSetHAdjustment

-- | The 'Adjustment' for the vertical position.
--
scrolledWindowVAdjustment :: Attr ScrolledWindow Adjustment
scrolledWindowVAdjustment = Attr 
  scrolledWindowGetVAdjustment
  scrolledWindowSetVAdjustment

-- | Style of bevel around the contents.
--
-- Default value: 'ShadowNone'
--
scrolledWindowShadowType :: Attr ScrolledWindow ShadowType
scrolledWindowShadowType = Attr 
  scrolledWindowGetShadowType
  scrolledWindowSetShadowType

-- | \'placement\' property. See 'scrolledWindowGetPlacement' and
-- 'scrolledWindowSetPlacement'
--
scrolledWindowPlacement :: Attr ScrolledWindow CornerType
scrolledWindowPlacement = Attr 
  scrolledWindowGetPlacement
  scrolledWindowSetPlacement
