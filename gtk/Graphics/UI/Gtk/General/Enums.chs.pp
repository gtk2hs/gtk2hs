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
  ArrowType(..),
  AttachOptions(..),
  MouseButton(..),
  ButtonBoxStyle(..),
  CalendarDisplayOptions(..),
  Click(..),
  CornerType(..),
  DeleteType(..),
  DestDefaults(..),
  DirectionType(..),
  Justification(..),
#ifndef DISABLE_DEPRECATED
  MatchType(..),
#endif
  MenuDirectionType(..),
  MetricType(..),
  MovementStep(..),
  Orientation(..),
  Packing(..), toPacking, fromPacking,
  PackType(..),
  PathPriorityType(..),
  PathType(..),
  PolicyType(..),
  PositionType(..),
  ProgressBarOrientation(..),
  ReliefStyle(..),
  ResizeMode(..),
  ScrollType(..),
  SelectionMode(..),
  ShadowType(..),
  SortType(..),
  StateType(..),
#ifndef DISABLE_DEPRECATED
  SubmenuDirection(..),
  SubmenuPlacement(..),
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
  UpdateType(..),
  Visibility(..),
  WindowPosition(..),
  WindowType(..),
  WrapMode(..), 
  module Graphics.UI.Gtk.Gdk.Enums
  ) where

import System.Glib.Flags	(Flags)
import Graphics.UI.Gtk.Gdk.Enums

{#context lib="gtk" prefix ="gtk"#}


-- | State of an accelerator
--
{#enum AccelFlags {underscoreToCase} deriving(Eq, Bounded)#}

instance Flags AccelFlags

-- | Arrow directions for the arrow widget
--
{#enum ArrowType {underscoreToCase} deriving (Eq)#}

-- | Child widget attach options for table containers
--
{#enum AttachOptions {underscoreToCase} deriving(Eq, Bounded)#}

instance Flags AttachOptions

-- | Mouse buttons.
--
data MouseButton = LeftButton
		 | MiddleButton
		 | RightButton
		   deriving (Eq,Show)

instance Enum MouseButton where
  toEnum 1 = LeftButton
  toEnum 2 = MiddleButton
  toEnum 3 = RightButton
  fromEnum LeftButton   = 1
  fromEnum MiddleButton = 2
  fromEnum RightButton  = 3

-- | Dictate the style that a ButtonBox uses to align it contents
--
{#enum ButtonBoxStyle {underscoreToCase} deriving (Eq)#}

-- | Specify which items of a calendar should be displayed.
--
{#enum CalendarDisplayOptions {underscoreToCase} deriving(Eq, Bounded)#}

instance Flags CalendarDisplayOptions

-- | Type of mouse click
--
data Click = SingleClick
	   | DoubleClick
	   | TripleClick
	   | ReleaseClick
  deriving Eq

-- | Specifies in which corner a child widget should be placed
--
{#enum CornerType {underscoreToCase} deriving (Eq)#}

-- | Editing option
--
{#enum DeleteType {underscoreToCase} deriving (Eq)#}

-- | The 'DestDefaults' enumeration specifies the various types of action that
-- will be taken on behalf of the user for a drag destination site.
--
-- * 'DestDefaultMotion':   If set for a widget, GTK+, during a drag over this
--   widget will check if the drag matches this widget's list of possible
--   targets and actions. GTK+ will then call
--   'Graphics.UI.Gtk.Gdk.Drag.dragStatus' as appropriate.
-- * 'DestDefaultHightlight':   If set for a widget, GTK+ will draw a
--   highlight on this widget as long as a drag is over this widget and the
--   widget drag format and action are acceptable.
-- * 'DestDefaultDrop':   If set for a widget, when a drop occurs, GTK+ will
--   will check if the drag matches this widget's list of possible targets and
--   actions. If so, GTK+ will call 'Graphics.UI.Gtk.Gdk.Drag.dragGetData' on
--   behalf of the widget. Whether or not the drop is successful, GTK+ will
--   call 'Graphics.UI.Gtk.Gdk.Drag.dragFinish'. If the action was a move,
--   then if the drag was successful, then @True@ will be passed for the
--   delete parameter to 'Graphics.UI.Gtk.Gdk.Drag.dragFinish'
-- * 'DestDefaultAll':   If set, specifies that all default actions should be
--   taken.
--
{#enum DestDefaults {underscoreToCase} deriving (Bounded,Eq)#}

instance Flags DestDefaults

-- | Editing direction
--
{#enum DirectionType {underscoreToCase} deriving (Eq)#}

-- | Justification for label and maybe other widgets (text?)
--
{#enum Justification {underscoreToCase} deriving (Eq)#}

#ifndef DISABLE_DEPRECATED
-- | Some kind of string search options
--
{#enum MatchType {underscoreToCase} deriving (Eq)#}
#endif

-- | From where was a menu item entered?
--
{#enum MenuDirectionType {underscoreToCase} deriving (Eq)#}

-- | Units of measure
--
{#enum MetricType {underscoreToCase} deriving (Eq)#}

-- | Movement in text widget
--
{#enum MovementStep {underscoreToCase} deriving (Eq)#}

-- | Orientation is good
--
{#enum Orientation {underscoreToCase} deriving (Eq)#}

-- | Packing parameters of a widget
--
-- * The 'Packing' parameter determines how the child behaves in the horizontal
--   or vertical way in an 'Graphics.UI.Gtk.Layout.HBox' or
--   'Graphics.UI.Gtk.Layout.VBox', respectively. 'PackNatural'
--   means the child is as big as it reqests. It will stay at the start of the
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
	     deriving (Enum,Eq)

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
{#enum PackType {underscoreToCase} deriving (Eq)#}

-- | Priorities
--
{#enum PathPriorityType {underscoreToCase} deriving (Eq)#}

-- | Widget identification path
--
{#enum PathType {underscoreToCase} deriving (Eq)#}

-- | Scrollbar policy types (for scrolled windows)
--
{#enum PolicyType {underscoreToCase} deriving (Eq)#}

-- | Position a scale's value is drawn relative to the
-- trough
--
{#enum PositionType {underscoreToCase} deriving (Eq)#}

-- | Is the ProgressBar horizontally or vertically
-- directed?
--
{#enum ProgressBarOrientation {underscoreToCase} deriving (Eq)#}

-- | I don't have a clue.
--
{#enum ReliefStyle {underscoreToCase} deriving (Eq)#}

-- | Resize mode, for containers
--
-- * 'ResizeParent' Pass resize request to the parent
--
-- * 'ResizeQueue' Queue resizes on this widget
--
-- * 'ResizeImmediate' Perform the resizes now
--
{#enum ResizeMode {underscoreToCase} deriving (Eq)#}

-- | Scrolling type
--
{#enum ScrollType {underscoreToCase} deriving (Eq)#}

-- | Mode in which selections can be performed
--
-- * There is a deprecated entry SelectionExtended which should have the same
--   value as SelectionMultiple. C2HS chokes on that construct.
--
data SelectionMode = SelectionNone
                   | SelectionSingle
                   | SelectionBrowse
                   | SelectionMultiple
                   deriving (Enum, Eq)
-- {#enum SelectionMode {underscoreToCase} deriving (Eq)#}

-- | Shadow types
--
{#enum ShadowType {underscoreToCase} deriving (Eq)#}

-- Sort a 'Graphics.UI.Gtk.TreeList.TreeViewColumn' in ascending or descending
-- order.
--
{#enum SortType {underscoreToCase} deriving (Eq)#}

-- | Widget states
--
{#enum StateType {underscoreToCase} deriving (Eq)#}

#ifndef DISABLE_DEPRECATED
-- | Submenu direction policies
--
{#enum SubmenuDirection {underscoreToCase} deriving (Eq)#}

-- | Submenu placement policies
--
{#enum SubmenuPlacement {underscoreToCase} deriving (Eq)#}
#endif

-- | Whether to clamp or ignore illegal values.
--
{#enum SpinButtonUpdatePolicy {underscoreToCase} deriving (Eq)#}

-- | Spin a SpinButton with the following method.
--
{#enum SpinType {underscoreToCase} deriving (Eq)#}

-- | The 'TargetFlags' enumeration is used to specify constraints on an entry
--	 in a 'Graphics.UI.Gtk.Gdk.Selection.TargetList'. These flags are only
--	 used for drag and drop.
--
-- * If the 'TargetSameApp' flag is set, the target will only be selected for
--	 drags within a single application.
--
-- * If the 'TargetSameWidget' flag is set, the target will only be selected
--	 for drags within a single widget.
--
{#enum TargetFlags {underscoreToCase} deriving(Bounded) #}

instance Flags TargetFlags

-- | Is the text written from left to right or the exotic way?
--
{#enum TextDirection {underscoreToCase} deriving (Eq)#}

-- | Specify the way the search function for
--   'Graphics.UI.Gtk.Multiline.TextBuffer' works.
--
{#enum TextSearchFlags {underscoreToCase} deriving(Eq, Bounded)#}

instance Flags TextSearchFlags

-- | The window type for coordinate translation.
--
{#enum TextWindowType {underscoreToCase} deriving (Eq)#}

-- | Where to place the toolbar?
--
{#enum ToolbarStyle {underscoreToCase} deriving (Eq)#}

-- | Wether columns of a tree or list widget can be resized.
--
{#enum TreeViewColumnSizing {underscoreToCase} deriving (Eq)#}

-- hm... text editing?
--{#enum TroughType {underscoreToCase} deriving (Eq)#}


-- | Updating types for range widgets (determines when the
-- @\"connectToValueChanged\"@ signal is emitted by the widget)
--
{#enum UpdateType {underscoreToCase} deriving (Eq)#}

-- | Visibility
--
{#enum Visibility {underscoreToCase} deriving (Eq)#}

-- | Window position types
--
{#enum WindowPosition {underscoreToCase} deriving (Eq)#}

-- | Interaction of a window with window manager
--
{#enum WindowType {underscoreToCase} deriving (Eq)#}

-- | Determine how lines are wrapped in a 'Graphics.UI.Gtk.Multiline.TextView'.
--
{#enum WrapMode {underscoreToCase} deriving (Eq)#}


