{-# LANGUAGE CPP #-}
--  -*-haskell-*-
--  GIMP Toolkit (GTK) Enumerations
--
--  Author : Axel Simon, Manuel Chakravarty
--
--  Created: 13 January 1999
--
--  Copyright (C) 1999..2005 Axel Simon, Manuel Chakravarty
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
module Graphics.UI.Gtk.General.Enums (
  AccelFlags(..),
#if GTK_CHECK_VERSION(3,0,0)
  Align(..),
#endif
  ArrowType(..),
  AttachOptions(..),
#if GTK_CHECK_VERSION(3,10,0)
  BaselinePosition(..),
#endif
  MouseButton(..),
  ButtonBoxStyle(..),
  CalendarDisplayOptions(..),
  Click(..),
  CornerType(..),
  DeleteType(..),
  DestDefaults(..),
#if GTK_CHECK_VERSION(2,12,0)
  DragResult(..),
#endif
  DirectionType(..),
  Justification(..),
#if GTK_CHECK_VERSION(3,6,0)
  LevelBarMode(..),
#endif
#if GTK_MAJOR_VERSION < 3
#ifndef DISABLE_DEPRECATED
  MatchType(..),
#endif
#endif
  MenuDirectionType(..),
#if GTK_MAJOR_VERSION < 3
#if GTK_CHECK_VERSION(2,8,0)
  MetricType(..),
#endif
#endif
  MovementStep(..),
  Orientation(..),
  Packing(..), toPacking, fromPacking,
  PackType(..),
  PathPriorityType(..),
  PathType(..),
  PolicyType(..),
  PositionType(..),
#if GTK_MAJOR_VERSION < 3
  ProgressBarOrientation(..),
#endif
  ReliefStyle(..),
  ResizeMode(..),
  ScrollType(..),
  ScrollStep (..),
  SelectionMode(..),
  ShadowType(..),
#if GTK_CHECK_VERSION(3,0,0)
  StateFlags(..),
#endif
  SortType(..),
  StateType(..),
#if GTK_MAJOR_VERSION < 3
#ifndef DISABLE_DEPRECATED
  SubmenuDirection(..),
  SubmenuPlacement(..),
#endif
#endif
  SpinButtonUpdatePolicy(..),
  SpinType(..),
  TargetFlags(..),
  TextDirection(..),
  TextSearchFlags(..),
  TextWindowType(..),
  ToolbarStyle(..),
  TreeViewColumnSizing(..),
  --TroughType(..),
#if GTK_MAJOR_VERSION < 3
  UpdateType(..),
  Visibility(..),
#endif
  WindowPosition(..),
  WindowType(..),
  WrapMode(..),
#if GTK_CHECK_VERSION(2,16,0)
  EntryIconPosition(..),
#endif
#if GTK_MAJOR_VERSION < 3
  AnchorType (..),
#endif
#if GTK_CHECK_VERSION(3,10,0)
  StackTransitionType (..),
#endif

module Graphics.UI.Gtk.Gdk.Enums
  ) where

import System.Glib.Flags        (Flags)
import Graphics.UI.Gtk.Gdk.Enums

{#context lib="gtk" prefix ="gtk"#}


-- | State of an accelerator
--
{#enum AccelFlags {underscoreToCase} deriving(Bounded,Eq,Show)#}

instance Flags AccelFlags

#if GTK_CHECK_VERSION(3,0,0)
-- | State of an accelerator
--
{#enum Align {underscoreToCase} deriving(Bounded,Eq,Show)#}
#endif

-- | Arrow directions for the arrow widget
--
{#enum ArrowType {underscoreToCase} deriving (Eq,Show)#}

-- | Child widget attach options for table containers
--
{#enum AttachOptions {underscoreToCase} deriving(Bounded,Eq,Show)#}

instance Flags AttachOptions

#if GTK_CHECK_VERSION(3,10,0)
-- | Whenever a container has some form of natural row it may align children in 
-- that row along a common typographical baseline. If the amount of verical space
-- in the row is taller than the total requested height of the baseline-aligned
-- children then it can use a BaselinePosition to select where to put the
-- baseline inside the extra availible space.
--
{#enum BaselinePosition {underscoreToCase} deriving (Eq,Show)#}
#endif

-- | Mouse buttons.
--
data MouseButton = LeftButton
                 | MiddleButton
                 | RightButton
                 | OtherButton Int
                   deriving (Eq,Show)

instance Enum MouseButton where
  toEnum 1 = LeftButton
  toEnum 2 = MiddleButton
  toEnum 3 = RightButton
  toEnum n = OtherButton (fromIntegral n)
  fromEnum LeftButton   = 1
  fromEnum MiddleButton = 2
  fromEnum RightButton  = 3
  fromEnum (OtherButton n) = fromIntegral n

-- | Dictate the style that a ButtonBox uses to align it contents
--
{#enum ButtonBoxStyle {underscoreToCase} deriving (Eq,Show)#}

-- | Specify which items of a calendar should be displayed.
--
{#enum CalendarDisplayOptions {underscoreToCase} deriving(Bounded,Eq,Show)#}

instance Flags CalendarDisplayOptions

-- | Type of mouse click
--
data Click = SingleClick
           | DoubleClick
           | TripleClick
           | ReleaseClick
  deriving (Eq,Show,Enum)

-- | Specifies in which corner a child widget should be placed
--
{#enum CornerType {underscoreToCase} deriving (Eq,Show)#}

-- | Editing option
--
{#enum DeleteType {underscoreToCase} deriving (Eq,Show)#}

-- | The 'DestDefaults' enumeration specifies the various types of action that
-- will be taken on behalf of the user for a drag destination site.
--
-- * 'DestDefaultMotion': If set for a widget, GTK+, during a drag over this
--   widget will check if the drag matches this widget's list of possible
--   targets and actions. GTK+ will then call
--   'Graphics.UI.Gtk.Gdk.Drag.dragStatus' as appropriate.
--
-- * 'DestDefaultHighlight': If set for a widget, GTK+ will draw a
--   highlight on this widget as long as a drag is over this widget and the
--   widget drag format and action are acceptable.
--
-- * 'DestDefaultDrop': If set for a widget, when a drop occurs, GTK+ will
--   will check if the drag matches this widget's list of possible targets and
--   actions. If so, GTK+ will call 'Graphics.UI.Gtk.Gdk.Drag.dragGetData' on
--   behalf of the widget. Whether or not the drop is successful, GTK+ will
--   call 'Graphics.UI.Gtk.Gdk.Drag.dragFinish'. If the action was a move,
--   then if the drag was successful, then @True@ will be passed for the
--   delete parameter to 'Graphics.UI.Gtk.Gdk.Drag.dragFinish'
--
-- * 'DestDefaultAll':   If set, specifies that all default actions should be
--   taken.
--
{#enum DestDefaults {underscoreToCase} deriving (Bounded,Eq,Show)#}

instance Flags DestDefaults

#if GTK_CHECK_VERSION(2,12,0)
-- | Gives an indication why a drag operation failed. The value can by
-- obtained by connecting to the 'dragFailed' signal.
--
-- * 'DragResultSuccess': The drag operation was successful
--
-- * 'DragResultNoTarget': No suitable drag target
--
-- * 'DragResultUserCancelled': The user cancelled the drag operation
--
-- * 'DragResultTimeoutExpired': The drag operation timed out
--
-- * 'DragResultGrabBroken': The pointer or keyboard grab used for the drag
--   operation was broken
--
-- * 'DragResultError': The drag operation failed due to some unspecified error
--
{#enum DragResult {underscoreToCase} deriving (Bounded,Eq,Show)#}
#endif

-- | Editing direction
--
{#enum DirectionType {underscoreToCase} deriving (Eq,Show)#}

-- | Justification for label and maybe other widgets (text?)
--
{#enum Justification {underscoreToCase} deriving (Eq,Show)#}

#if GTK_CHECK_VERSION(3,6,0)
{#enum LevelBarMode {underscoreToCase} deriving (Eq,Show)#}
#endif

#if GTK_MAJOR_VERSION < 3
#ifndef DISABLE_DEPRECATED
-- | Some kind of string search options
--
-- Removed in Gtk3.
{#enum MatchType {underscoreToCase} deriving (Eq,Show)#}
#endif
#endif

-- | From where was a menu item entered?
--
{#enum MenuDirectionType {underscoreToCase} deriving (Eq,Show)#}

#if GTK_MAJOR_VERSION < 3
-- | Units of measure
--
-- Removed in Gtk3.
{#enum MetricType {underscoreToCase} deriving (Eq,Show)#}
#endif

-- | Movement in text widget
--
{#enum MovementStep {underscoreToCase} deriving (Eq,Show)#}

-- | Orientation is good
--
{#enum Orientation {underscoreToCase} deriving (Eq,Show)#}

-- | Packing parameters of a widget
--
-- * The 'Packing' parameter determines how the child behaves in the horizontal
--   or vertical way in an 'Graphics.UI.Gtk.Layout.HBox' or
--   'Graphics.UI.Gtk.Layout.VBox', respectively. 'PackNatural'
--   means the child is as big as it requests. It will stay at the start or
--   end of a 'Graphics.UI.Gtk.Layout.Box' if there is more space available.
--   All children packed with 'PackRepel' will be padded on both sides with
--   additional space. 'PackGrow' will increase the size of a widget so that it
--   covers the available space. A menu bar, for instance, should always
--   stay at the top of a window and should only occupy as little space
--   as possible. Hence it should be packed at the start of a
--  'Graphics.UI.Gtk.Layout.VBox' with
--   the packing option 'PackNatural'. The working area of a window
--   (e.g. the text area in an editor) should expand when the window is
--   resized. Here the packing option 'PackGrow' is the right choice and
--   it is irrelevant whether the main area is inserted at the start or
--   the end of a box. Finally 'PackRepel' is most useful in a window
--   where no widget can make use of excess space. Examples include a
--   dialog box without list boxes or text fields.
--
data Packing = PackRepel
             | PackGrow
             | PackNatural
             deriving (Enum,Eq,Show)

-- The conversions between our Packing type and Gtk's expand and fill
-- properties.
--
toPacking :: Bool -> Bool -> Packing
toPacking expand True = PackGrow
toPacking True   fill = PackRepel
toPacking False  fill = PackNatural

fromPacking :: Packing -> (Bool, Bool)
fromPacking PackGrow    = (True,True)
fromPacking PackRepel   = (True,False)
fromPacking PackNatural = (False,False)

-- | Packing of widgets at start or end in a box
--
{#enum PackType {underscoreToCase} deriving (Eq,Show)#}

-- | Priorities
--
{#enum PathPriorityType {underscoreToCase} deriving (Eq,Show)#}

-- | Widget identification path
--
{#enum PathType {underscoreToCase} deriving (Eq,Show)#}

-- | Scrollbar policy types (for scrolled windows)
--
{#enum PolicyType {underscoreToCase} deriving (Eq,Show)#}

-- | Position a scale's value is drawn relative to the
-- trough
--
{#enum PositionType {underscoreToCase} deriving (Eq,Show)#}

#if GTK_MAJOR_VERSION < 3
-- | Is the ProgressBar horizontally or vertically
-- directed?
--
-- Removed in Gtk3.
{#enum ProgressBarOrientation {underscoreToCase} deriving (Eq,Show)#}
#endif

-- | I don't have a clue.
--
{#enum ReliefStyle {underscoreToCase} deriving (Eq,Show)#}

-- | Resize mode, for containers
--
-- * 'ResizeParent' Pass resize request to the parent
--
-- * 'ResizeQueue' Queue resizes on this widget
--
-- * 'ResizeImmediate' Perform the resizes now
--
{#enum ResizeMode {underscoreToCase} deriving (Eq,Show)#}

-- | Scrolling type
--
{#enum ScrollType {underscoreToCase} deriving (Eq,Show)#}

-- | Scrolling step
--
{#enum ScrollStep {underscoreToCase} deriving (Eq,Show)#}

-- | Mode in which selections can be performed
--
-- * There is a deprecated entry SelectionExtended which should have the same
--   value as SelectionMultiple. C2HS chokes on that construct.
--
data SelectionMode = SelectionNone
                   | SelectionSingle
                   | SelectionBrowse
                   | SelectionMultiple
                   deriving (Enum,Eq,Show)
-- {#enum SelectionMode {underscoreToCase} deriving (Eq,Show)#}

-- | Shadow types
--
{#enum ShadowType {underscoreToCase} deriving (Eq,Show)#}

#if GTK_CHECK_VERSION(3,0,0)
-- | Describes a widget state. Widget states are used to match the widget against
-- CSS pseudo-classes. Note that GTK extends the regular CSS classes and
-- sometimes uses different names.
--
{#enum StateFlags {underscoreToCase} deriving (Bounded,Eq,Show)#}

instance Flags StateFlags
#endif

-- Sort a 'Graphics.UI.Gtk.ModelView.TreeViewColumn' in ascending or descending
-- order.
--
{#enum SortType {underscoreToCase} deriving (Eq,Show)#}

-- | Widget states
--
{#enum StateType {underscoreToCase} deriving (Eq,Show)#}

#if GTK_MAJOR_VERSION < 3
#ifndef DISABLE_DEPRECATED
-- | Submenu direction policies
--
-- Removed in Gtk3.
{#enum SubmenuDirection {underscoreToCase} deriving (Eq,Show)#}

-- | Submenu placement policies
--
-- Removed in Gtk3.
{#enum SubmenuPlacement {underscoreToCase} deriving (Eq,Show)#}
#endif
#endif

-- | Whether to clamp or ignore illegal values.
--
{#enum SpinButtonUpdatePolicy {underscoreToCase} deriving (Eq,Show)#}

-- | Spin a SpinButton with the following method.
--
{#enum SpinType {underscoreToCase} deriving (Eq,Show)#}

-- | The 'TargetFlags' enumeration is used to specify constraints on an entry
--       in a 'Graphics.UI.Gtk.Gdk.Selection.TargetList'. These flags are only
--       used for drag and drop.
--
-- * If the 'TargetSameApp' flag is set, the target will only be selected for
--       drags within a single application.
--
-- * If the 'TargetSameWidget' flag is set, the target will only be selected
--       for drags within a single widget.
--
{#enum TargetFlags {underscoreToCase} deriving(Bounded,Eq,Show) #}

instance Flags TargetFlags

-- | Is the text written from left to right or the exotic way?
--
{#enum TextDirection {underscoreToCase} deriving (Eq,Show)#}

-- | Specify the way the search function for
--   'Graphics.UI.Gtk.Multiline.TextBuffer' works.
--
{#enum TextSearchFlags {underscoreToCase} deriving(Bounded,Eq,Show)#}

instance Flags TextSearchFlags

-- | The window type for coordinate translation.
--
{#enum TextWindowType {underscoreToCase} deriving (Eq,Show)#}

-- | Where to place the toolbar?
--
{#enum ToolbarStyle {underscoreToCase} deriving (Eq,Show)#}

-- | Wether columns of a tree or list widget can be resized.
--
{#enum TreeViewColumnSizing {underscoreToCase} deriving (Eq,Show)#}

-- hm... text editing?
--{#enum TroughType {underscoreToCase} deriving (Eq,Show)#}

#if GTK_MAJOR_VERSION < 3
-- | Updating types for range widgets (determines when the
-- @\"connectToValueChanged\"@ signal is emitted by the widget)
--
-- Removed in Gtk3.
{#enum UpdateType {underscoreToCase} deriving (Eq,Show)#}

-- | Visibility
--
-- Removed in Gtk3.
{#enum Visibility {underscoreToCase} deriving (Eq,Show)#}
#endif

-- | Window position types
--
{#enum WindowPosition {underscoreToCase} deriving (Eq,Show)#}

-- | Interaction of a window with window manager
--
{#enum WindowType {underscoreToCase} deriving (Eq,Show)#}

-- | Determine how lines are wrapped in a 'Graphics.UI.Gtk.Multiline.TextView'.
--
{#enum WrapMode {underscoreToCase} deriving (Eq,Show)#}

#if GTK_CHECK_VERSION(2,16,0)
-- | Specifies the side of the entry at which an icon is placed.
--
{#enum EntryIconPosition {underscoreToCase} deriving (Eq,Show)#}
#endif

#if GTK_MAJOR_VERSION < 3
-- |
--
-- Removed in Gtk3.
{#enum AnchorType {underscoreToCase} deriving (Eq,Show)#}
#endif

#if GTK_CHECK_VERSION(3,10,0)
{#enum StackTransitionType {underscoreToCase} deriving (Eq,Show)#}
#endif
