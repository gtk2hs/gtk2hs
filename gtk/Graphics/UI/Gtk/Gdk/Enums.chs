{-# LANGUAGE CPP #-}
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
  CrossingMode(..),
  DragProtocol(..),
  DragAction(..),
  EventMask(..),
  Modifier(..),
#if GTK_CHECK_VERSION(3,4,0)
  ModifierIntent(..),
#endif
  NotifyType(..),
  ScrollDirection(..),
  VisibilityState(..),
  WindowState(..),
  WindowEdge(..),
  WindowTypeHint(..),
  Gravity(..),
  GrabStatus(..),
#if GTK_CHECK_VERSION(2,6,0)
  OwnerChange(..),
#endif
#if GTK_MAJOR_VERSION < 3
  ExtensionMode(..),
  CapStyle(..),
  Dither(..),
  Fill(..),
  Function(..),
  InputCondition(..),
  JoinStyle(..),
  LineStyle(..),
  SubwindowMode(..),
#endif
  ) where

import System.Glib.Flags        (Flags)

{#context lib="gdk" prefix ="gdk"#}

#if GTK_MAJOR_VERSION < 3
-- | Specify the how the ends of a line is drawn.
--
-- Removed in Gtk3.
{#enum CapStyle {underscoreToCase} deriving(Eq,Show)#}
#endif

-- | How focus is crossing the widget.
--
{#enum CrossingMode {underscoreToCase} deriving(Eq,Show) #}

-- | Used in 'Graphics.UI.Gtk.Gdk.Drag.DragContext' to indicate the protocol according to which DND is done.
--
{#enum DragProtocol {underscoreToCase} deriving(Eq,Bounded,Show)#}


-- | Used in 'Graphics.UI.Gtk.Genearl.Drag.DragContext' to indicate what the
-- destination should do with the dropped data.
--
--   * 'ActionDefault': Initialisation value, should not be used.
--
--   * 'ActionCopy': Copy the data.
--
--   * 'ActionMove': Move the data, i.e. first copy it, then delete it from the source.
--
--   * 'ActionLink':  Add a link to the data. Note that this is only useful if source and
--     destination agree on what it means.
--
--   * 'ActionPrivate': Special action which tells the source that the destination will do
--     something that the source doesn't understand.
--
--   * 'ActionAsk': Ask the user what to do with the data.
--
{#enum DragAction {underscoreToCase} deriving(Eq,Bounded,Show)#}

instance Flags DragAction

#if GTK_MAJOR_VERSION < 3
-- | Specify how to dither colors onto the screen.
--
-- Removed in Gtk3.
{#enum RgbDither as Dither {underscoreToCase} deriving(Eq,Show) #}
#endif

-- | Specify which events a widget will emit signals on.
--
{#enum EventMask {underscoreToCase} deriving(Eq,Bounded,Show)#}

instance Flags EventMask

-- | Keyboard modifiers that are depressed when the user presses
--   a key or a mouse button.
--
-- * This data type is used to build lists of modifers that were active
--   during an event.
--
-- * The "Apple" key on Macintoshs is mapped to 'Alt2' and the 'Meta'
--   key (if available).
--
-- * Since Gtk 2.10, there are also 'Super', 'Hyper' and 'Meta' modifiers
--   which are simply generated from 'Alt' .. 'Compose' modifier keys,
--   depending on the mapping used by the windowing system. Due to one
--   key being mapped to e.g. 'Alt2' and 'Meta', you shouldn't pattern
--   match directly against a certain key but check whether a key is
--   in the list using the 'elem' function, say.
--
#if GTK_CHECK_VERSION(2,10,0)
{#enum ModifierType as Modifier {SHIFT_MASK as Shift,
                                 LOCK_MASK as Lock,
                                 CONTROL_MASK as Control,
                                 MOD1_MASK as Alt,
                                 MOD2_MASK as Alt2,
                                 MOD3_MASK as Alt3,
                                 MOD4_MASK as Alt4,
                                 MOD5_MASK as Alt5,
                                 BUTTON1_MASK as Button1,
                                 BUTTON2_MASK as Button2,
                                 BUTTON3_MASK as Button3,
                                 BUTTON4_MASK as Button4,
                                 BUTTON5_MASK as Button5,
                                 SUPER_MASK as Super,
                                 HYPER_MASK as Hyper,
                                 META_MASK as Meta,
                                 RELEASE_MASK as Release,
                                 MODIFIER_MASK as ModifierMask
                                 } deriving(Bounded,Show,Eq) #}
#else
{#enum ModifierType as Modifier {SHIFT_MASK as Shift,
                                 LOCK_MASK as Lock,
                                 CONTROL_MASK as Control,
                                 MOD1_MASK as Alt,
                                 MOD2_MASK as Alt2,
                                 MOD3_MASK as Alt3,
                                 MOD4_MASK as Alt4,
                                 MOD5_MASK as Alt5,
                                 BUTTON1_MASK as Button1,
                                 BUTTON2_MASK as Button2,
                                 BUTTON3_MASK as Button3,
                                 BUTTON4_MASK as Button4,
                                 BUTTON5_MASK as Button5,
                                 RELEASE_MASK as Release,
                                 MODIFIER_MASK as ModifierMask
                                 } deriving(Bounded,Show,Eq) #}
#endif

instance Flags Modifier

#if GTK_CHECK_VERSION(3,4,0)
{#enum ModifierIntent {underscoreToCase} deriving(Eq,Show) #}
#endif

#if GTK_MAJOR_VERSION < 3
-- | specify which input extension a widget desires
--
{#enum ExtensionMode {underscoreToCase} deriving(Eq,Bounded,Show)#}

instance Flags ExtensionMode

-- | How objects are filled.
--
-- Removed in Gtk3.
{#enum Fill {underscoreToCase} deriving(Eq,Show) #}

-- | Determine how bitmap operations are carried out.
--
-- Removed in Gtk3.
{#enum Function {underscoreToCase} deriving(Eq,Show) #}

-- | Specify on what file condition a callback should be
-- done.
--
-- Removed in Gtk3.
{#enum InputCondition {underscoreToCase} deriving(Eq,Bounded) #}

instance Flags InputCondition

-- | Determines how adjacent line ends are drawn.
--
-- Removed in Gtk3.
{#enum JoinStyle {underscoreToCase} deriving(Eq,Show)#}

-- | Determines if a line is solid or dashed.
--
-- Removed in Gtk3.
{#enum LineStyle {underscoreToCase} deriving(Eq,Show)#}
#endif
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
{#enum NotifyType {underscoreToCase} deriving(Eq,Show) #}

-- | in which direction was scrolled?
--
{#enum ScrollDirection {underscoreToCase} deriving(Eq,Show) #}


#if GTK_MAJOR_VERSION < 3
-- | Determine if child widget may be overdrawn.
--
-- Removed in Gtk3.
{#enum SubwindowMode {underscoreToCase} deriving(Eq,Show) #}
#endif

-- | visibility of a window
--
{#enum VisibilityState {underscoreToCase,
                        VISIBILITY_PARTIAL as VisibilityPartialObscured}
                         deriving(Eq,Show) #}

-- | The state a @DrawWindow@ is in.
--
{#enum WindowState {underscoreToCase} deriving(Eq,Bounded,Show)#}

instance Flags WindowState

-- | Determines a window edge or corner.
--
{#enum WindowEdge {underscoreToCase} deriving(Eq,Show) #}

-- | These are hints for the window manager that indicate what type of function
-- the window has. The window manager can use this when determining decoration
-- and behaviour of the window. The hint must be set before mapping the window.
--
-- See the extended window manager hints specification for more details about
-- window types.
--
{#enum WindowTypeHint {underscoreToCase} deriving(Eq,Show)#}

-- | Defines the reference point of a window and the meaning of coordinates
-- passed to 'Graphics.UI.Gtk.Windows.Window.windowMove'. See
-- 'Graphics.UI.Gtk.Windows.Window.windowMove' and the "implementation notes"
-- section of the extended window manager hints specification for more details.
--
{#enum Gravity {underscoreToCase} deriving(Eq,Show) #}

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
{#enum GrabStatus {underscoreToCase} deriving(Eq,Show) #}

#if GTK_CHECK_VERSION(2,6,0)
-- | Specifies why a selection ownership was changed.
--
-- [@OwnerChangeNewOwner@] some other application claimed the ownership
--
-- [@OwnerChangeDestroy@] the window was destroyed
--
-- [@OwnerChangeClose@] the client was closed
--
{#enum OwnerChange {underscoreToCase} deriving(Eq,Show) #}
#endif
