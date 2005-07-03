--  -*-haskell-*-
--  GIMP Toolkit (GTK) Enumerations
--
--  Author : Axel Simon, Manuel Chakravarty
--
--  Created: 13 January 1999
--
--  Version $Revision: 1.5 $ from $Date: 2005/07/03 12:27:10 $
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
  module Graphics.UI.Gtk.Gdk.Enums
  ) where

import System.Glib.Flags	(Flags, fromFlags, toFlags)
import Graphics.UI.Gtk.Gdk.Enums

{#context lib="gtk" prefix ="gtk"#}


-- | State of an accelerator
--
{#enum AccelFlags {underscoreToCase} deriving(Bounded)#}

instance Flags AccelFlags

-- | Arrow directions for the arrow widget
--
{#enum ArrowType {underscoreToCase}#}

-- | Child widget attach options for table containers
--
{#enum AttachOptions {underscoreToCase} deriving(Bounded)#}

instance Flags AttachOptions

-- | Button number
--
data Button = LeftButton
	    | MiddleButton
	    | RightButton

instance Enum Button where
  toEnum 1 = LeftButton
  toEnum 2 = MiddleButton
  toEnum 3 = RightButton
  fromEnum LeftButton   = 1
  fromEnum MiddleButton = 2
  fromEnum RightButton  = 3

-- | Dictate the style that a ButtonBox uses to align it contents
--
{#enum ButtonBoxStyle {underscoreToCase}#}

-- | Specify which items of a calendar should be displayed.
--
{#enum CalendarDisplayOptions {underscoreToCase} deriving(Bounded)#}

instance Flags CalendarDisplayOptions

-- | Type of mouse click
--
data Click = SingleClick
	   | DoubleClick
	   | TripleClick
	   | ReleaseClick

-- | Specifies in which corner a child widget should be placed
--
{#enum CornerType {underscoreToCase}#}

-- | Specifies how curves in the gamma widget (?) are drawn
--
{#enum CurveType {underscoreToCase}#}

-- | Editing option
--
{#enum DeleteType {underscoreToCase}#}

-- | Editing direction
--
{#enum DirectionType {underscoreToCase}#}

-- | Justification for label and maybe other widgets (text?)
--
{#enum Justification {underscoreToCase}#}

#ifndef DISABLE_DEPRECATED
-- | Some kind of string search options
--
{#enum MatchType {underscoreToCase}#}
#endif

-- | From where was a menu item entered?
--
{#enum MenuDirectionType {underscoreToCase}#}

-- | Units of measure
--
{#enum MetricType {underscoreToCase}#}

-- | Movement in text widget
--
{#enum MovementStep {underscoreToCase}#}

-- | Orientation is good
--
{#enum Orientation {underscoreToCase}#}

-- | Packing parameters of a widget
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
{#enum PackType {underscoreToCase}#}

-- | Priorities
--
{#enum PathPriorityType {underscoreToCase}#}

-- | Widget identification path
--
{#enum PathType {underscoreToCase}#}

-- | Scrollbar policy types (for scrolled windows)
--
{#enum PolicyType {underscoreToCase}#}

-- | Position a scale's value is drawn relative to the
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

-- | Resize mode, for containers
--
-- * 'ResizeParent' Pass resize request to the parent
--
-- * 'ResizeQueue' Queue resizes on this widget
--
-- * 'ResizeImmediate' Perform the resizes now
--
{#enum ResizeMode {underscoreToCase}#}

-- | Scrolling type
--
{#enum ScrollType {underscoreToCase}#}

-- | Mode in which selections can be performed
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

-- | Shadow types
--
{#enum ShadowType {underscoreToCase}#}

-- | Widget states
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

-- | Is the text written from left to right or the awkward way?
--
{#enum TextDirection {underscoreToCase}#}

-- | Specify the way the search function for 'TextBuffer' works.
--
{#enum TextSearchFlags {underscoreToCase} deriving(Bounded)#}

instance Flags TextSearchFlags

-- | The window type for coordinate translation.
--
{#enum TextWindowType {underscoreToCase}#}

-- | Where to place the toolbar?
--
{#enum ToolbarStyle {underscoreToCase}#}

-- | Wether columns of a tree or list widget can be resized.
--
{#enum TreeViewColumnSizing {underscoreToCase}#}

-- hm... text editing?
--{#enum TroughType {underscoreToCase}#}


-- | Updating types for range widgets (determines when the
-- @\"connectToValueChanged\"@ signal is emitted by the widget)
--
{#enum UpdateType {underscoreToCase}#}

-- | Visibility
--
{#enum Visibility {underscoreToCase}#}

-- | Window position types
--
{#enum WindowPosition {underscoreToCase}#}

-- | Interaction of a window with window manager
--
{#enum WindowType {underscoreToCase}#}

-- | Determine how lines are wrapped in a 'TextView'.
--
{#enum WrapMode {underscoreToCase}#}

-- Sort in ascending or descending order (used in CList widget)
--
{#enum SortType {underscoreToCase}#}

