--  -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Enumerations@
--
--  Author : Axel Simon, Manuel Chakravarty
--  Created: 13 Januar 1999
--
--  Version $Revision: 1.6 $ from $Date: 2002/11/03 20:35:42 $
--
--  Copyright (c) [1999..2001] Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- @description@ --------------------------------------------------------------
--
--  General enumeration types.
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
--
--  * Documentation
--
module Enums(
  AccelFlags(..),
  ArrowType(..),
  AttachOptions(..),
  Button(..),
  ButtonBoxStyle(..),
  CalendarDisplayOptions(..),
  Click(..),
  CornerType(..),
  CurveType(..),
  DeleteType(..),
  DirectionType(..),
  Justification(..),
  MatchType(..),
  MenuDirectionType(..),
  MetricType(..),
  MovementStep(..),
  Orientation(..),
  Packing(..),
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
  StateType(..),
  SubmenuDirection(..),
  SubmenuPlacement(..),
  SpinButtonUpdatePolicy(..),
  SpinType(..),
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
  SortType(..),
  module GdkEnums
  ) where

import GdkEnums

{#context lib="gtk" prefix ="gtk"#}


-- @data AccelFlags@ state of an accelerator
--
{#enum AccelFlags {underscoreToCase} deriving(Bounded)#}

instance Flags AccelFlags

-- @data ArrowType@ arrow directions for the arrow widget
--
{#enum ArrowType {underscoreToCase}#}

-- @data AttachOptions@ child widget attach options for table containers
--
{#enum AttachOptions {underscoreToCase} deriving(Bounded)#}

instance Flags AttachOptions

-- @data Button@ button number
--
data Button = LeftButton
	    | MiddleButton
	    | RightButton
	    | WheelUp
	    | WheelDown
	    | OtherButton

instance Enum Button where
  toEnum 1 = LeftButton
  toEnum 2 = MiddleButton
  toEnum 3 = RightButton
  toEnum 4 = WheelUp
  toEnum 5 = WheelDown
  toEnum _ = OtherButton
  fromEnum LeftButton   = 1
  fromEnum MiddleButton = 2
  fromEnum RightButton  = 3
  fromEnum WheelUp	= 4
  fromEnum WheelDown	= 5
  fromEnum OtherButton	= 6

-- @data ButtonBoxStyle@ dictate the style that a ButtonBox uses to align it
-- contents
--
{#enum ButtonBoxStyle {underscoreToCase}#}

-- @data CalendarDisplayOptions@ Specify which items of a calendar should be
-- displayed.
--
{#enum CalendarDisplayOptions {underscoreToCase} deriving(Bounded)#}

instance Flags CalendarDisplayOptions

-- @data Click@ type of mouse click
--
data Click = SingleClick
	   | DoubleClick
	   | TripleClick
	   | ReleaseClick

-- @data CornerType@ specifies in which corner a child widget should be placed
--
{#enum CornerType {underscoreToCase}#}

-- @data CurveType@ specifies how curves in the gamma widget (?) are drawn
--
{#enum CurveType {underscoreToCase}#}

-- @data DeleteType@ editing option
--
{#enum DeleteType {underscoreToCase}#}

-- @data DirectionType@ editing direction
--
{#enum DirectionType {underscoreToCase}#}

-- @data Justification@ justification for label and maybe other widgets
-- (text?)
--
{#enum Justification {underscoreToCase}#}

-- @data MatchType@ some kind of string search options
--
{#enum MatchType {underscoreToCase}#}

-- @data MenuDirectionType@ From where was a menu item entered?
--
{#enum MenuDirectionType {underscoreToCase}#}

-- @data MetricType@ units of measure
--
{#enum MetricType {underscoreToCase}#}

-- @data MovementStep@ movement in text widget
--
{#enum MovementStep {underscoreToCase}#}

-- @data Orientation@ orientation is good
--
{#enum Orientation {underscoreToCase}#}

-- @data Packing@ packing parameters of a widget
--
data Packing = PackRepel
	     | PackGrow
	     | PackNatural
	     deriving (Enum,Eq)

-- packing of widgets at start or end in a box
--
{#enum PackType {underscoreToCase}#}

-- @data PathPriorityType@ priorities
--
{#enum PathPriorityType {underscoreToCase}#}

-- @data PathType@ widget identification path
--
{#enum PathType {underscoreToCase}#}

-- @data PolicyType@ Scrollbar policy types (for scrolled windows)
--
{#enum PolicyType {underscoreToCase}#}

-- @data PositionType@ position a scale's value is drawn relative to the
-- trough
--
{#enum PositionType {underscoreToCase}#}

-- @data ProgressBarOrientation@ Is the ProgressBar horizontally or vertically
-- directed?
--
{#enum ProgressBarOrientation {underscoreToCase}#}

-- @data ReliefStyle@ I don't have a clue.
--
{#enum ReliefStyle {underscoreToCase}#}

-- @data ResizeMode@ resize mode, for containers
--
-- * @ref type ResizeParent@ Pass resize request to the parent
--
-- * @ref type ResizeQueue@ Queue resizes on this widget
--
-- * @ref type ResizeImmediate@ Perform the resizes now
--
{#enum ResizeMode {underscoreToCase}#}

-- @data ScrollType@ scrolling type
--
{#enum ScrollType {underscoreToCase}#}

-- @data SelectionMode@ mode in which selections can be performed
--
-- * There is a deprecated entry SelectionExtended which should have the same
--   value as SelectionMultiple. C2HS chokes on that construct.
--
data SelectionMode = SelectionNone
                   | SelectionSingle
                   | SelectionBrowse
                   | SelectionMultiple
                   deriving (Enum)
-- {#enum SelectionMode {underscoreToCase}#}

-- @data ShadowType@ shadow types
--
{#enum ShadowType {underscoreToCase}#}

-- @data StateType@ widget states
--
{#enum StateType {underscoreToCase}#}

-- @data SubmenuDirection@ Submenu direction policies
--
{#enum SubmenuDirection {underscoreToCase}#}

-- @data SubmenuPlacement@ Submenu placement policies
--
{#enum SubmenuPlacement {underscoreToCase}#}

-- @data SpinButtonUpdatePolicy@ Whether to clamp or ignore illegal values.
--
{#enum SpinButtonUpdatePolicy {underscoreToCase}#}

-- @data SpinType@ Spin a SpinButton with the following method.
--
{#enum SpinType {underscoreToCase}#}

-- @data TextDirection@ Is the text written from left to right or the awkward
-- way?
--
{#enum TextDirection {underscoreToCase}#}

-- @data TextSearchFlags@ Specify the way the search function for
-- @ref type TextBuffer@ works.
--
{#enum TextSearchFlags {underscoreToCase} deriving(Bounded)#}

instance Flags TextSearchFlags

-- @data TextWindowType@ The window type for coordinate translation.
--
{#enum TextWindowType {underscoreToCase}#}

-- @data ToolbarStyle@ Where to place the toolbar?
--
{#enum ToolbarStyle {underscoreToCase}#}

-- @data TreeViewColumnSizing@ Wether columns of a tree or list widget can be
-- resized.
--
{#enum TreeViewColumnSizing {underscoreToCase}#}

-- hm... text editing?
--{#enum TroughType {underscoreToCase}#}


-- @data UpdateType@ updating types for range widgets (determines when the
-- @ref signal connectToValueChanged@ signal is emitted by the widget)
--
{#enum UpdateType {underscoreToCase}#}

-- @data Visibility@ visibility
--
{#enum Visibility {underscoreToCase}#}

-- @data WindowPosition@ window position types
--
{#enum WindowPosition {underscoreToCase}#}

-- @data WindowType@ interaction of a window with window manager
--
{#enum WindowType {underscoreToCase}#}

-- @data WrapMode@ Determine how lines are warpped in a @ref data TextView@.
--
{#enum WrapMode {underscoreToCase}#}

-- sort in ascending or descending order (used in CList widget)
--
{#enum SortType {underscoreToCase}#}

