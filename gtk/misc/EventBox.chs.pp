-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget EventBox
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2004/11/21 15:06:15 $
--
--  Copyright (c) 1999..2002 Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- |
--
-- This container can be used to receive 'Event's for a widget
-- that has no window on its own.
--
-- TODO
--
-- * check: Is this widget useful?
--
module EventBox(
  EventBox,
  EventBoxClass,
  castToEventBox,
  eventBoxNew
#if GTK_CHECK_VERSION(2,4,0)
 ,eventBoxSetVisibleWindow,
  eventBoxGetVisibleWindow,
  eventBoxSetAboveChild,
  eventBoxGetAboveChild
#endif
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new 'EventBox'.
--
eventBoxNew :: IO EventBox
eventBoxNew  = makeNewObject mkEventBox $ 
  liftM castPtr {#call unsafe event_box_new#}

#if GTK_CHECK_VERSION(2,4,0)
-- | Set whether the event box uses a visible or invisible child window. The
-- default is to use visible windows. The C documentation for details of what
-- difference this makes.
--
eventBoxSetVisibleWindow :: EventBox -> Bool -> IO ()
eventBoxSetVisibleWindow ebox visible =
  {#call event_box_set_visible_window#} ebox (fromBool visible)

-- | Returns whether the event box has a visible window.
--
eventBoxGetVisibleWindow :: EventBox -> IO Bool
eventBoxGetVisibleWindow ebox =
  liftM toBool $ {#call unsafe event_box_get_visible_window#} ebox

-- | Set whether the event box window is positioned above the windows of its
-- child, as opposed to below it. 
--
-- * If the window is above, all events inside the event box will go to the
-- event box. If the window is below, events in windows of child widgets will
-- first got to that widget, and then to its parents.
--
eventBoxSetAboveChild :: EventBox -> Bool -> IO ()
eventBoxSetAboveChild ebox above =
  {#call event_box_set_above_child#} ebox (fromBool above)

-- | Returns whether the event box window is above or below the windows of its
-- child. See 'eventBoxSetAboveChild' for details.
--
eventBoxGetAboveChild :: EventBox -> IO Bool
eventBoxGetAboveChild ebox =
  liftM toBool $ {#call unsafe event_box_get_above_child#} ebox
#endif
