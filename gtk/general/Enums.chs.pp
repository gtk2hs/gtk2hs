--  -*-haskell-*-
--  GIMP Toolkit (GTK) Enumerations
--
--  Author : Axel Simon, Manuel Chakravarty
--  Created: 13 Januar 1999
--
--  Version $Revision: 1.1 $ from $Date: 2004/11/21 15:06:14 $
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
-- |
--
-- General enumeration types.
--
-- TODO
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
#ifndef DISABLE_DEPRECATED
  MatchType(..),
#endif
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
#ifndef DISABLE_DEPRECATED
  SubmenuDirection(..),
  SubmenuPlacement(..),
#endif
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


-- | state of an accelerator
--
{#enum AccelFlags {underscoreToCase} deriving(Bounded)#}

instance Flags AccelFlags

-- | arrow directions for the arrow widget
--
{#enum ArrowType {underscoreToCase}#}

-- | child widget attach options for table containers
--
{#enum AttachOptions {underscoreToCase} deriving(Bounded)#}

instance Flags AttachOptions

-- | button number
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

-- | dictate the style that a ButtonBox uses to align it
-- contents
--
{#enum ButtonBoxStyle {underscoreToCase}#}

-- | Specify which items of a calendar should be
-- displayed.
--
{#enum CalendarDisplayOptions {underscoreToCase} deriving(Bounded)#}

instance Flags CalendarDisplayOptions

-- | type of mouse click
--
data Click = SingleClick
	   | DoubleClick
	   | TripleClick
	   | ReleaseClick

-- | specifies in which corner a child widget should be placed
--
{#enum CornerType {underscoreToCase}#}

-- | specifies how curves in the gamma widget (?) are drawn
--
{#enum CurveType {underscoreToCase}#}

-- | editing option
--
{#enum DeleteType {underscoreToCase}#}

-- | editing direction
--
{#enum DirectionType {underscoreToCase}#}

-- | justification for label and maybe other widgets
-- (text?)
--
{#enum Justification {underscoreToCase}#}

#ifndef DISABLE_DEPRECATED
-- | some kind of string search options
--
{#enum MatchType {underscoreToCase}#}
#endif

-- | From where was a menu item entered?
--
{#enum MenuDirectionType {underscoreToCase}#}

-- | units of measure
--
{#enum MetricType {underscoreToCase}#}

-- | movement in text widget
--
{#enum MovementStep {underscoreToCase}#}

-- | orientation is good
--
{#enum Orientation {underscoreToCase}#}

-- | packing parameters of a widget
--
data Packing = PackRepel
	     | PackGrow
	     | PackNatural
	     deriving (Enum,Eq)

-- packing of widgets at start or end in a box
--
{#enum PackType {underscoreToCase}#}

-- | priorities
--
{#enum PathPriorityType {underscoreToCase}#}

-- | widget identification path
--
{#enum PathType {underscoreToCase}#}

-- | Scrollbar policy types (for scrolled windows)
--
{#enum PolicyType {underscoreToCase}#}

-- | position a scale's value is drawn relative to the
-- trough
--
{#enum PositionType {underscoreToCase}#}

-- | Is the ProgressBar horizontally or vertically
-- directed?
--
{#enum ProgressBarOrientation {underscoreToCase}#}

-- | I don't have a clue.
--
{#enum ReliefStyle {underscoreToCase}#}

-- | resize mode, for containers
--
-- * 'ResizeParent' Pass resize request to the parent
--
-- * 'ResizeQueue' Queue resizes on this widget
--
-- * 'ResizeImmediate' Perform the resizes now
--
{#enum ResizeMode {underscoreToCase}#}

-- | scrolling type
--
{#enum ScrollType {underscoreToCase}#}

-- | mode in which selections can be performed
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

-- | shadow types
--
{#enum ShadowType {underscoreToCase}#}

-- | widget states
--
{#enum StateType {underscoreToCase}#}

#ifndef DISABLE_DEPRECATED
-- | Submenu direction policies
--
{#enum SubmenuDirection {underscoreToCase}#}

-- | Submenu placement policies
--
{#enum SubmenuPlacement {underscoreToCase}#}
#endif

-- | Whether to clamp or ignore illegal values.
--
{#enum SpinButtonUpdatePolicy {underscoreToCase}#}

-- | Spin a SpinButton with the following method.
--
{#enum SpinType {underscoreToCase}#}

-- | Is the text written from left to right or the awkward
-- way?
--
{#enum TextDirection {underscoreToCase}#}

-- | Specify the way the search function for
-- 'TextBuffer' works.
--
{#enum TextSearchFlags {underscoreToCase} deriving(Bounded)#}

instance Flags TextSearchFlags

-- | The window type for coordinate translation.
--
{#enum TextWindowType {underscoreToCase}#}

-- | Where to place the toolbar?
--
{#enum ToolbarStyle {underscoreToCase}#}

-- | Wether columns of a tree or list widget can be
-- resized.
--
{#enum TreeViewColumnSizing {underscoreToCase}#}

-- hm... text editing?
--{#enum TroughType {underscoreToCase}#}


-- | updating types for range widgets (determines when the
-- @\"connectToValueChanged\"@ signal is emitted by the widget)
--
{#enum UpdateType {underscoreToCase}#}

-- | visibility
--
{#enum Visibility {underscoreToCase}#}

-- | window position types
--
{#enum WindowPosition {underscoreToCase}#}

-- | interaction of a window with window manager
--
{#enum WindowType {underscoreToCase}#}

-- | Determine how lines are wrapped in a 'TextView'.
--
{#enum WrapMode {underscoreToCase}#}

-- sort in ascending or descending order (used in CList widget)
--
{#enum SortType {underscoreToCase}#}

