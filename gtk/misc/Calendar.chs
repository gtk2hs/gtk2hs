-- -*-haskell-*-
--  GIMP Toolkit (GTK) Binding for Haskell: Widget Calendar
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.1.1.1 $ from $Date: 2002/03/24 21:56:20 $
--
--  Copyright (c) [1999.2001] Manuel Chakravarty, Axel Simon
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
--- DESCRIPTION ---------------------------------------------------------------
--
-- * This widget shows a calendar.
--
--- DOCU ----------------------------------------------------------------------
--
--
--- TODO ----------------------------------------------------------------------

module Calendar(
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
  calendarGetDate,
  connectToDaySelected,
  connectToDaySelectedDoubleClick,
  connectToMonthChanged,
  connectToNextMonth,
  connectToNextYear,
  connectToPrevMonth,
  connectToPrevYear
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(CalendarDisplayOptions(..), fromFlags)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- Create a new calendar widget. (EXPORTED)
--
-- * No sensible date will be set.
--
calendarNew :: IO Calendar
calendarNew = makeNewObject mkCalendar $ 
  liftM castPtr {#call unsafe calendar_new#}

-- Flip the page to a month , 0 is January,.., 11 is December. (EXPORTED)
--
-- * Returns True if the operation succeeded.
--
calendarSelectMonth :: CalendarClass c => Int -> Int -> c -> IO Bool
calendarSelectMonth month year cal = liftM toBool $
  {#call calendar_select_month#} (toCalendar cal) (fromIntegral month)
  (fromIntegral year)

-- Shift to a day, counted form 1 to 31 (depending on the month of course). 
-- (EXPORTED)
--
calendarSelectDay :: CalendarClass c => Int -> c -> IO ()
calendarSelectDay day cal = 
  {#call calendar_select_day#} (toCalendar cal) (fromIntegral day)

-- Mark (select) a day in the current month. (EXPORTED)
--
-- * Returns True if the argument was within bounds and the day was
--   previously deselected.
--
calendarMarkDay :: CalendarClass c => Int -> c -> IO Bool
calendarMarkDay day cal = liftM toBool $
  {#call calendar_mark_day#} (toCalendar cal) (fromIntegral day)

-- Unmark (deselect) a day in the current month. (EXPORTED)
--
-- * Returns True if the argument was within bounds and the day was
--   previously selected.
--
calendarUnmarkDay :: CalendarClass c => Int -> c -> IO Bool
calendarUnmarkDay day cal = liftM toBool $
  {#call calendar_unmark_day#} (toCalendar cal) (fromIntegral day)

-- Unmark every day in the current page. (EXPORTED)
--
calendarClearMarks :: CalendarClass c => c -> IO ()
calendarClearMarks cal = {#call calendar_clear_marks#} (toCalendar cal)

-- Specifies how the calendar should be displayed. (EXPORTED)
--
calendarDisplayOptions :: CalendarClass c => 
  [CalendarDisplayOptions] -> c -> IO ()
calendarDisplayOptions opts cal = {#call calendar_display_options#}
  (toCalendar cal) ((fromIntegral.fromFlags) opts)

-- Retrieve the currently selected date. (EXPORTED)
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

-- Freeze the calender for several update operations. (EXPORTED)
--
calendarFreeze :: CalendarClass c => IO a -> c -> IO a
calendarFreeze update cal = do
  {#call unsafe calendar_freeze#} (toCalendar cal)
  res <- update
  {#call calendar_thaw#} (toCalendar cal)
  return res

-- signals

-- Emitted when a day was selected. (EXPORTED)
--
connectToDaySelected :: CalendarClass c => 
  IO () -> ConnectAfter -> c -> IO (ConnectId c)
connectToDaySelected = connect_NONE__NONE "day-selected"

-- Emitted when a day received a double click. (EXPORTED)
--
connectToDaySelectedDoubleClick :: CalendarClass c =>
  IO () -> ConnectAfter -> c -> IO (ConnectId c)
connectToDaySelectedDoubleClick = 
  connect_NONE__NONE "day-selected-double-click"

-- The month changed. (EXPORTED)
--
connectToMonthChanged :: CalendarClass c =>
  IO () -> ConnectAfter -> c -> IO (ConnectId c)
connectToMonthChanged = connect_NONE__NONE "month-changed"

-- The next month was selected. (EXPORTED)
--
connectToNextMonth :: CalendarClass c =>
  IO () -> ConnectAfter -> c -> IO (ConnectId c)
connectToNextMonth = connect_NONE__NONE "next-month"

-- The next year was selected. (EXPORTED)
--
connectToNextYear :: CalendarClass c =>
  IO () -> ConnectAfter -> c -> IO (ConnectId c)
connectToNextYear = connect_NONE__NONE "next-year"

-- The previous month was selected. (EXPORTED)
--
connectToPrevMonth :: CalendarClass c =>
  IO () -> ConnectAfter -> c -> IO (ConnectId c)
connectToPrevMonth = connect_NONE__NONE "prev-month"

-- The previous year was selected. (EXPORTED)
--
connectToPrevYear :: CalendarClass c =>
  IO () -> ConnectAfter -> c -> IO (ConnectId c)
connectToPrevYear = connect_NONE__NONE "prev-year"

  