-- -*-haskell-*-
--  GIMP Toolkit (GTK) Enumerations
--
--  Author : Manuel M. T. Chakravarty, Axel Simon
--
--  Created: 13 Januar 1999
--
--  Version $Revision: 1.7 $ from $Date: 2005/11/17 17:12:55 $
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
  Gravity(..)
  ) where

import System.Glib.Flags	(Flags, fromFlags, toFlags)

{#context lib="gdk" prefix ="gdk"#}

-- | Specify the how the ends of a line is drawn.
--
{#enum CapStyle {underscoreToCase}#}

-- | How focus is crossing the widget.
--
{#enum CrossingMode {underscoreToCase}#}

-- | Used in 'Graphics.UI.Gtk.Gdk.Drag.DragContext' to indicate the protocol according to which DND is done.
--
{#enum DragProtocol {underscoreToCase} deriving (Bounded)#}

-- | Specify how to dither colors onto the screen.
--
{#enum RgbDither as Dither {underscoreToCase}#}

-- | specify which events a widget will emit signals on
--
{#enum EventMask {underscoreToCase} deriving (Bounded)#}

instance Flags EventMask

-- | specify which input extension a widget desires
--
{#enum ExtensionMode {underscoreToCase} deriving(Bounded)#}

instance Flags ExtensionMode

-- | How objects are filled.
--
{#enum Fill {underscoreToCase}#}

-- | Determine how bitmap operations are carried out.
--
{#enum Function {underscoreToCase}#}

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
{#enum NotifyType {underscoreToCase}#}

-- | in which direction was scrolled?
--
{#enum ScrollDirection {underscoreToCase}#}

-- | Determine if child widget may be overdrawn.
--
{#enum SubwindowMode {underscoreToCase}#}

-- | visibility of a window
--
{#enum VisibilityState {underscoreToCase,
			VISIBILITY_PARTIAL as VisibilityPartialObscured}#}

-- | The state a @DrawWindow@ is in.
--
{#enum WindowState {underscoreToCase} deriving (Bounded)#}

instance Flags WindowState

-- | Determines a window edge or corner.
--
{#enum WindowEdge {underscoreToCase} #}

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
{#enum Gravity {underscoreToCase} #}
    
