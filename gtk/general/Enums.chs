--  -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Enumeration types
--
--  Author : Axel Simon, Manuel Chakravarty
--  Created: 13 Januar 1999
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/04 14:02:30 $
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
--- DESCRIPTION ---------------------------------------------------------------
--
--  General enumeration types.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------
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
--  TroughType(..),
  UpdateType(..),
  Visibility(..),
  WindowPosition(..),
  WindowType(..), 
  SortType(..),
  module GdkEnums
  ) where

import GdkEnums

{#context lib="gtk" prefix ="gtk"#}


-- state of an accelerator (EXPORTED)
--
{#enum AccelFlags {underscoreToCase} deriving(Bounded)#}

instance Flags AccelFlags

-- arrow directions for the arrow widget (EXPORTED)
--
{#enum ArrowType {underscoreToCase}#}

-- child widget attach options for table containers (EXPORTED)
--
{#enum AttachOptions {underscoreToCase} deriving(Bounded)#}

instance Flags AttachOptions

-- button number (EXPORTED)
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

-- dictate the style that a ButtonBox uses to align it contents (EXPORTED)
--
{#enum ButtonBoxStyle {underscoreToCase}#}

-- Specify which items of a calendar should be displayed. (EXPORTED)
--
{#enum CalendarDisplayOptions {underscoreToCase} deriving(Bounded)#}

instance Flags CalendarDisplayOptions

-- type of mouse click (EXPORTED)
--
data Click = SingleClick
	   | DoubleClick
	   | TripleClick
	   | ReleaseClick

-- specifies in which corner a child widget should be placed (EXPORTED)
--
{#enum CornerType {underscoreToCase}#}

-- specifies how curves in the gamma widget (?) are drawn (EXPORTED)
--
{#enum CurveType {underscoreToCase}#}

-- editing option (EXPORTED)
--
{#enum DeleteType {underscoreToCase}#}

-- editing direction (EXPORTED)
--
{#enum DirectionType {underscoreToCase}#}

-- justification for label and maybe other widgets (text?) (EXPORTED)
--
{#enum Justification {underscoreToCase}#}

-- some kind of string search options (EXPORTED)
--
{#enum MatchType {underscoreToCase}#}

-- From where was a menu item entered? (EXPORTED)
--
{#enum MenuDirectionType {underscoreToCase}#}

-- units of measure (EXPORTED)
--
{#enum MetricType {underscoreToCase}#}

-- movement in text widget (EXPORTED)
--
{#enum MovementStep {underscoreToCase}#}

-- orientation is good (EXPORTED)
--
{#enum Orientation {underscoreToCase}#}

-- packing parameters of a widget (EXPORTED)
--
data Packing = PackExpand
	     | PackFill
	     | PackNatural
	     deriving (Enum,Eq)

-- packing of widgets at start or end in a box
--
{#enum PackType {underscoreToCase}#}

-- priorities (EXPORTED)
--
{#enum PathPriorityType {underscoreToCase}#}

-- widget identification path (EXPORTED)
--
{#enum PathType {underscoreToCase}#}

-- Scrollbar policy types (for scrolled windows) (EXPORTED)
--
{#enum PolicyType {underscoreToCase}#}

-- position a scale's value is drawn relative to the trough (EXPORTED)
--
{#enum PositionType {underscoreToCase}#}

-- Is the ProgressBar horizontally or vertically directed? (EXPORTED)
--
{#enum ProgressBarOrientation {underscoreToCase}#}

-- I don't have a clue. (EXPORTED)
--
{#enum ReliefStyle {underscoreToCase}#}

-- resize mode, for containers (EXPORTED)
--
-- * @ResizeParent Pass resize request to the parent
--
-- * @ResizeQueue  Queue resizes on this widget
--
-- * @ResizeImmediate Perform the resizes now
--
{#enum ResizeMode {underscoreToCase}#}

-- scrolling type (EXPORTED)
--
{#enum ScrollType {underscoreToCase}#}

-- mode in which selections can be performed (EXPORTED)
--
-- * There is a deprecated entry SelectionExtended which should have the
--   same value as SelectionMultiple. C2HS chokes on that construct.
--
data SelectionMode = SelectionNone
                   | SelectionSingle
                   | SelectionBrowse
                   | SelectionMultiple
                   deriving (Enum)
-- {#enum SelectionMode {underscoreToCase}#}

-- shadow types (EXPORTED)
--
{#enum ShadowType {underscoreToCase}#}

-- widget states (EXPORTED)
--
{#enum StateType {underscoreToCase}#}

-- Submenu direction policies (EXPORTED)
--
{#enum SubmenuDirection {underscoreToCase}#}

-- Submenu placement policies (EXPORTED)
--
{#enum SubmenuPlacement {underscoreToCase}#}

-- Whether to clamp or ignore illegal values. (EXPORTED)
--
{#enum SpinButtonUpdatePolicy {underscoreToCase}#}

-- Spin a SpinButton with the following method. (EXPORTED)
--
{#enum SpinType {underscoreToCase}#}

-- Is the text written from left to right or the awkward way? (EXPORTED)
--
{#enum TextDirection {underscoreToCase}#}

-- Specify the way the search function for @TextBuffer works. (EXPORTED)
--
{#enum TextSearchFlags {underscoreToCase} deriving(Bounded)#}

instance Flags TextSearchFlags

-- The window type for coordinate translation. (EXPORTED)
--
{#enum TextWindowType {underscoreToCase}#}

-- Where to place the toolbar? (EXPORTED)
--
{#enum ToolbarStyle {underscoreToCase}#}

-- Wether columns of a tree or list widget can be resized. (EXPORTED)
--
{#enum TreeViewColumnSizing {underscoreToCase}#}

-- hm... text editing? (EXPORTED)
--
--{#enum TroughType {underscoreToCase}#}

-- updating types for range widgets (determines when the @connectToValueChanged
-- signal is emitted by the widget)  (EXPORTED)
--
{#enum UpdateType {underscoreToCase}#}

-- visibility (EXPORTED)
--
{#enum Visibility {underscoreToCase}#}

-- window position types (EXPORTED)
--
{#enum WindowPosition {underscoreToCase}#}

-- interaction of a window with window manager (EXPORTED)
--
{#enum WindowType {underscoreToCase}#}

-- sort in ascending or descending order (used in CList widget)
--
{#enum SortType {underscoreToCase}#}

