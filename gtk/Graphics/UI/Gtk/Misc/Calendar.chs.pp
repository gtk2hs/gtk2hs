-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Calendar
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/12 17:19:23 $
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- Display a calendar and\/or allow the user to select a date.
--
module Graphics.UI.Gtk.Misc.Calendar (
  Calendar,
  CalendarClass,
  castToCalendar,
  calendarNew,
  calendarSelectMonth,
  calendarSelectDay,
  calendarMarkDay,
  calendarUnmarkDay,
  calendarClearMarks,
  calendarDisplayOptions,
#if GTK_CHECK_VERSION(2,4,0)
  calendarSetDisplayOptions,
  calendarGetDisplayOptions,
#endif
  calendarGetDate,
  onDaySelected,
  afterDaySelected,
  onDaySelectedDoubleClick,
  afterDaySelectedDoubleClick,
  onMonthChanged,
  afterMonthChanged,
  onNextMonth,
  afterNextMonth,
  onNextYear,
  afterNextYear,
  onPrevMonth,
  afterPrevMonth,
  onPrevYear,
  afterPrevYear
  ) where

import Monad	(liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(CalendarDisplayOptions(..), fromFlags, toFlags)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new calendar widget.
--
-- * No sensible date will be set.
--
calendarNew :: IO Calendar
calendarNew  = makeNewObject mkCalendar $ 
  liftM castPtr {#call unsafe calendar_new#}

-- | Flip the page to a month , 0 is January,.., 11
-- is December.
--
-- * Returns True if the operation succeeded.
--
calendarSelectMonth :: CalendarClass c => c -> Int -> Int -> IO Bool
calendarSelectMonth cal month year = liftM toBool $
  {#call calendar_select_month#} (toCalendar cal) (fromIntegral month)
  (fromIntegral year)

-- | Shift to a day, counted form 1 to 31 (depending
-- on the month of course).
--
calendarSelectDay :: CalendarClass c => c -> Int -> IO ()
calendarSelectDay cal day = 
  {#call calendar_select_day#} (toCalendar cal) (fromIntegral day)

-- | Mark (select) a day in the current month.
--
-- * Returns True if the argument was within bounds and the day was previously
--   deselected.
--
calendarMarkDay :: CalendarClass c => c -> Int -> IO Bool
calendarMarkDay cal day = liftM toBool $
  {#call calendar_mark_day#} (toCalendar cal) (fromIntegral day)

-- | Unmark (deselect) a day in the current month.
--
-- * Returns True if the argument was within bounds and the day was previously
--   selected.
--
calendarUnmarkDay :: CalendarClass c => c -> Int -> IO Bool
calendarUnmarkDay cal day = liftM toBool $
  {#call calendar_unmark_day#} (toCalendar cal) (fromIntegral day)

-- | Unmark every day in the current page.
--
calendarClearMarks :: CalendarClass c => c -> IO ()
calendarClearMarks cal = {#call calendar_clear_marks#} (toCalendar cal)

#if GTK_CHECK_VERSION(2,4,0)
-- | Specifies how the calendar should be displayed.
--
calendarSetDisplayOptions :: CalendarClass c => c
                          -> [CalendarDisplayOptions] -> IO ()
calendarSetDisplayOptions cal opts =
  {#call calendar_set_display_options#} (toCalendar cal)
    ((fromIntegral.fromFlags) opts)

-- | Returns the current display options for the calendar.
--
calendarGetDisplayOptions :: CalendarClass c => c
                          -> IO [CalendarDisplayOptions]
calendarGetDisplayOptions cal = liftM (toFlags.fromIntegral) $
  {#call calendar_get_display_options#} (toCalendar cal)

-- | Depreciaded, use 'calendarSetDisplayOptions'.
--
calendarDisplayOptions :: CalendarClass c => c
                       -> [CalendarDisplayOptions] -> IO ()
calendarDisplayOptions = calendarSetDisplayOptions
#else

-- | Specifies how the calendar should be displayed.
--
calendarDisplayOptions :: CalendarClass c => c -> [CalendarDisplayOptions] ->
                          IO ()
calendarDisplayOptions cal opts = {#call calendar_display_options#}
  (toCalendar cal) ((fromIntegral.fromFlags) opts)
#endif

-- | Retrieve the currently selected date.
--
-- * Returns (year, month, day) of the selection.
--
calendarGetDate :: CalendarClass c => c -> IO (Int,Int,Int)
calendarGetDate cal = alloca $ \yearPtr -> alloca $ \monthPtr -> 
  alloca $ \dayPtr -> do
    {#call unsafe calendar_get_date#} (toCalendar cal) yearPtr monthPtr dayPtr
    year  <- liftM fromIntegral $ peek yearPtr
    month <- liftM fromIntegral $ peek monthPtr
    day	  <- liftM fromIntegral	$ peek dayPtr
    return (year,month,day)

-- | Freeze the calender for several update operations.
--
calendarFreeze :: CalendarClass c => c -> IO a -> IO a
calendarFreeze cal update = do
  {#call unsafe calendar_freeze#} (toCalendar cal)
  res <- update
  {#call calendar_thaw#} (toCalendar cal)
  return res

-- signals

-- | Emitted when a day was selected.
--
onDaySelected, afterDaySelected :: CalendarClass c => c -> IO () ->
                                   IO (ConnectId c)
onDaySelected = connect_NONE__NONE "day-selected" False
afterDaySelected = connect_NONE__NONE "day-selected" True

-- | Emitted when a day received a
-- double click.
--
onDaySelectedDoubleClick, afterDaySelectedDoubleClick :: CalendarClass c => 
                                                         c -> IO () ->
                                                         IO (ConnectId c)
onDaySelectedDoubleClick = 
  connect_NONE__NONE "day-selected-double-click" False
afterDaySelectedDoubleClick = 
  connect_NONE__NONE "day-selected-double-click" True

-- | The month changed.
--
onMonthChanged, afterMonthChanged :: CalendarClass c => c -> IO () ->
                                     IO (ConnectId c)
onMonthChanged = connect_NONE__NONE "month-changed" False
afterMonthChanged = connect_NONE__NONE "month-changed" True

-- | The next month was selected.
--
onNextMonth, afterNextMonth :: CalendarClass c => c -> IO () ->
                               IO (ConnectId c)
onNextMonth = connect_NONE__NONE "next-month" False
afterNextMonth = connect_NONE__NONE "next-month" True

-- | The next year was selected.
--
onNextYear, afterNextYear :: CalendarClass c => c -> IO () -> IO (ConnectId c)
onNextYear = connect_NONE__NONE "next-year" False
afterNextYear = connect_NONE__NONE "next-year" True

-- | The previous month was selected.
--
onPrevMonth, afterPrevMonth :: CalendarClass c => c -> IO () ->
                               IO (ConnectId c)
onPrevMonth = connect_NONE__NONE "prev-month" False
afterPrevMonth = connect_NONE__NONE "prev-month" True

-- | The previous year was selected.
--
onPrevYear, afterPrevYear :: CalendarClass c => c -> IO () -> IO (ConnectId c)
onPrevYear = connect_NONE__NONE "prev-year" False
afterPrevYear = connect_NONE__NONE "prev-year" True

  
