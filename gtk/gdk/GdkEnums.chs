--  GIMP Toolkit (GTK) Binding for Haskell: Enumeration types     -*-haskell-*-
--
--  Author : Manuel M. T. Chakravarty, Axel Simon
--  Created: 13 Januar 1999
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:19 $
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

-- specify which events a widget will emit signals on (EXPORTED)
--
{#enum EventMask {underscoreToCase} deriving (Bounded)#}

instance Flags EventMask

-- specify which input extension a widget desires (EXPORTED)
--
{#enum ExtensionMode {underscoreToCase} deriving(Bounded)#}

instance Flags ExtensionMode

-- visibility of a window (EXPORTED)
--
{#enum VisibilityState {underscoreToCase,
			VISIBILITY_PARTIAL as VisibilityPartialObscured}#}

-- provide additionl information if cursor crosses a window (EXPORTED)
--
{#enum CrossingMode {underscoreToCase}#}

-- dunno
--
{#enum NotifyType {underscoreToCase}#}


-- the state a GDK window is in (EXPORTED)
--
{#enum WindowState {underscoreToCase} deriving (Bounded)#}

instance Flags WindowState

-- in which direction was scrolled? (EXPORTED)
--
{#enum ScrollDirection {underscoreToCase}#}

-- Specify on what file condition a callback should be done. (EXPORTED)
--
{#enum InputCondition {underscoreToCase} deriving(Bounded) #}

instance Flags InputCondition
