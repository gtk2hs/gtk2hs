-- -*-haskell-*-
--  GIMP Toolkit (GTK) Enumerations
--
--  Author : Manuel M. T. Chakravarty, Axel Simon
--
--  Created: 13 Januar 1999
--
--  Copyright (C) 1999-2005 Manuel M. T. Chakravarty, Axel Simon
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
-- General enumeration types.
--
module Graphics.UI.Gtk.Gdk.Enums (
  CapStyle(..),
  CrossingMode(..),
  Dither(..),
  DragProtocol(..),
  DragAction(..),
  EventMask(..),
  ExtensionMode(..),
  Fill(..),
  Function(..),
  InputCondition(..),
  JoinStyle(..),
  LineStyle(..),
  NotifyType(..),
  ScrollDirection(..),
  SubwindowMode(..),
  VisibilityState(..),
  WindowState(..),
  WindowEdge(..),
  WindowTypeHint(..),
  Gravity(..),
  GrabStatus(..),
  ) where

import System.Glib.Flags	(Flags)

{#context lib="gdk" prefix ="gdk"#}

-- | Specify the how the ends of a line is drawn.
--
{#enum CapStyle {underscoreToCase}#}

-- | How focus is crossing the widget.
--
{#enum CrossingMode {underscoreToCase} deriving(Show) #}

-- | Used in 'Graphics.UI.Gtk.Gdk.Drag.DragContext' to indicate the protocol according to which DND is done.
--
{#enum DragProtocol {underscoreToCase} deriving (Bounded,Show)#}

-- | Specify the kind of action performed on a drag event.
{#enum DragAction {underscoreToCase} deriving (Bounded,Show)#}

instance Flags DragAction

-- | Specify how to dither colors onto the screen.
--
{#enum RgbDither as Dither {underscoreToCase} deriving(Show) #}

-- | specify which events a widget will emit signals on
--
{#enum EventMask {underscoreToCase} deriving (Bounded)#}

instance Flags EventMask

-- | specify which input extension a widget desires
--
{#enum ExtensionMode {underscoreToCase} deriving(Bounded,Show)#}

instance Flags ExtensionMode

-- | How objects are filled.
--
{#enum Fill {underscoreToCase} deriving(Show) #}

-- | Determine how bitmap operations are carried out.
--
{#enum Function {underscoreToCase} deriving(Show) #}

-- | Specify on what file condition a callback should be
-- done.
--
{#enum InputCondition {underscoreToCase} deriving(Bounded) #}

instance Flags InputCondition

-- | Determines how adjacent line ends are drawn.
--
{#enum JoinStyle {underscoreToCase}#}

-- | Determines if a line is solid or dashed.
--
{#enum LineStyle {underscoreToCase}#}

-- | Information on from what level of the widget hierarchy the mouse
--   cursor came.
--
-- ['NotifyAncestor'] The window is entered from an ancestor or left towards
-- an ancestor.
--
-- ['NotifyVirtual'] The pointer moves between an ancestor and an inferior
-- of the window.
--
-- ['NotifyInferior'] The window is entered from an inferior or left
-- towards an inferior.
--
-- ['NotifyNonlinear'] The window is entered from or left towards a
-- window which is neither an ancestor nor an inferior.
--
-- ['NotifyNonlinearVirtual'] The pointer moves between two windows which
-- are not ancestors of each other and the window is part of the ancestor
-- chain between one of these windows and their least common ancestor.
--
-- ['NotifyUnknown'] The level change does not fit into any of the other
-- categories or could not be determined.
--
{#enum NotifyType {underscoreToCase} deriving(Show) #}

-- | in which direction was scrolled?
--
{#enum ScrollDirection {underscoreToCase} deriving(Show) #}

-- | Determine if child widget may be overdrawn.
--
{#enum SubwindowMode {underscoreToCase} deriving(Show) #}

-- | visibility of a window
--
{#enum VisibilityState {underscoreToCase,
			VISIBILITY_PARTIAL as VisibilityPartialObscured}
			 deriving(Show) #}

-- | The state a @DrawWindow@ is in.
--
{#enum WindowState {underscoreToCase} deriving (Bounded,Show)#}

instance Flags WindowState

-- | Determines a window edge or corner.
--
{#enum WindowEdge {underscoreToCase} deriving(Show) #}

-- | These are hints for the window manager that indicate what type of function
-- the window has. The window manager can use this when determining decoration
-- and behaviour of the window. The hint must be set before mapping the window.
--
-- See the extended window manager hints specification for more details about
-- window types.
--
{#enum WindowTypeHint {underscoreToCase} #}

-- | Defines the reference point of a window and the meaning of coordinates
-- passed to 'Graphics.UI.Gtk.Windows.Window.windowMove'. See
-- 'Graphics.UI.Gtk.Windows.Window.windowMove' and the "implementation notes"
-- section of the extended window manager hints specification for more details.
--
{#enum Gravity {underscoreToCase} deriving(Show) #}

-- | Returned by 'pointerGrab' and 'keyboardGrab' to indicate success or the
-- reason for the failure of the grab attempt.
--
-- [@GrabSuccess@] the resource was successfully grabbed.
--
-- [@GrabAlreadyGrabbed@] the resource is actively grabbed by another client.
--
-- [@GrabInvalidTime@] the resource was grabbed more recently than the
--   specified time.
--
-- [@GrabNotViewable@] the grab window or the confine_to window are not
--   viewable.
--
-- [@GrabFrozen@] the resource is frozen by an active grab of another client.
--
{#enum GrabStatus {underscoreToCase} deriving(Show) #}

