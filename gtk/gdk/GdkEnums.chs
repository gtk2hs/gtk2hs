--  GIMP Toolkit (GTK) @entry Enumeration types     -*-haskell-*-@
--
--  Author : Manuel M. T. Chakravarty, Axel Simon
--  Created: 13 Januar 1999
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/24 09:43:24 $
--
--  Copyright (c) [1999..2001] Manuel M. T. Chakravarty
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
module GdkEnums(
  EventMask(..),
  ExtensionMode(..),
  VisibilityState(..),
  CrossingMode(..),
  NotifyType(..),
  WindowState(..),
  ScrollDirection(..),
  InputCondition(..),
  Flags(fromFlags,toFlags)
  ) where

import Bits((.|.))

class  (Enum a, Bounded a) => Flags a where
  fromFlags ::  [a] -> Int
  toFlags   ::  Int -> [a]

  fromFlags is = orNum 0 is
    where
      orNum n []     = n
      orNum n (i:is) = orNum (n .|. fromEnum i) is
  toFlags n = andNum n minBound
    where
      andNum n (m::a) = (if (n .|. fromEnum m) == n then (m:) else id)
        (if fromEnum m==fromEnum (maxBound::a) then [] else andNum n (succ m))

{#context lib="libgdk" prefix ="gdk"#}

-- @data EventMask@ specify which events a widget will emit signals on
--
{#enum EventMask {underscoreToCase} deriving (Bounded)#}

instance Flags EventMask

-- @data ExtensionMode@ specify which input extension a widget desires
--
{#enum ExtensionMode {underscoreToCase} deriving(Bounded)#}

instance Flags ExtensionMode

-- @data VisibilityState@ visibility of a window
--
{#enum VisibilityState {underscoreToCase,
			VISIBILITY_PARTIAL as VisibilityPartialObscured}#}

-- @data CrossingMode@ provide additionl information if cursor crosses a
-- window
--
{#enum CrossingMode {underscoreToCase}#}

-- dunno
--
{#enum NotifyType {underscoreToCase}#}


-- @data WindowState@ the state a GDK window is in
--
{#enum WindowState {underscoreToCase} deriving (Bounded)#}

instance Flags WindowState

-- @data ScrollDirection@ in which direction was scrolled?
--
{#enum ScrollDirection {underscoreToCase}#}

-- @data InputCondition@ Specify on what file condition a callback should be
-- done.
--
{#enum InputCondition {underscoreToCase} deriving(Bounded) #}

instance Flags InputCondition
