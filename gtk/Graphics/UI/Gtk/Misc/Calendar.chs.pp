-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Calendar
--
--  Author : Axel Simon
--
--  Created: 23 May 2001
--
--  Version $Revision: 1.6 $ from $Date: 2005/04/08 14:42:22 $
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
-- Displays a calendar and allows the user to select a date
--
module Graphics.UI.Gtk.Misc.Calendar (
-- * Detail
-- 
-- | 'Calendar' is a widget that displays a calendar, one month at a time. It
-- can be created with 'calendarNew'.
--
-- The month and year currently displayed can be altered with
-- 'calendarSelectMonth'. The exact day can be selected from the displayed
-- month using 'calendarSelectDay'.
--
-- To place a visual marker on a particular day, use 'calendarMarkDay' and
-- to remove the marker, 'calendarUnmarkDay'. Alternative, all marks can be
-- cleared with 'calendarClearMarks'.
--
-- The way in which the calendar itself is displayed can be altered using
-- 'calendarSetDisplayOptions'.
--
-- The selected date can be retrieved from a 'Calendar' using
-- 'calendarGetDate'.
--
-- If performing many \'mark\' operations, the calendar can be frozen to
-- prevent flicker, using 'calendarFreeze', and \'thawed\' again using
-- 'calendarThaw'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----Calendar
-- @

-- * Types
  Calendar,
  CalendarClass,
  castToCalendar,

-- * Constructors
  calendarNew,

-- * Methods
  calendarSelectMonth,
  calendarSelectDay,
  calendarMarkDay,
  calendarUnmarkDay,
  calendarClearMarks,
#ifndef DISABLE_DEPRECATED
  calendarDisplayOptions,
#endif
#if GTK_CHECK_VERSION(2,4,0)
  calendarSetDisplayOptions,
  calendarGetDisplayOptions,
#endif
  calendarGetDate,
  calendarFreeze,

-- * Properties
--  calendarDisplayOptions,

-- * Signals
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

--------------------
-- Constructors

-- | Creates a new calendar, with the current date being selected.
--
calendarNew :: IO Calendar
calendarNew =
  makeNewObject mkCalendar $
  liftM (castPtr :: Ptr Widget -> Ptr Calendar) $
  {# call unsafe calendar_new #}

--------------------
-- Methods

-- | Shifts the calendar to a different month.
--
calendarSelectMonth :: CalendarClass self => self
 -> Int     -- ^ @month@ - a month number between 0 and 11.
 -> Int     -- ^ @year@ - the year the month is in.
 -> IO Bool -- ^ returns @True@ if the operation succeeded.
calendarSelectMonth self month year =
  liftM toBool $
  {# call calendar_select_month #}
    (toCalendar self)
    (fromIntegral month)
    (fromIntegral year)

-- | Selects a day from the current month.
--
calendarSelectDay :: CalendarClass self => self
 -> Int   -- ^ @day@ - the day number between 1 and 31, or 0 to unselect the
          -- currently selected day.
 -> IO ()
calendarSelectDay self day =
  {# call calendar_select_day #}
    (toCalendar self)
    (fromIntegral day)

-- | Places a visual marker on a particular day.
--
calendarMarkDay :: CalendarClass self => self
 -> Int     -- ^ @day@ - the day number to mark between 1 and 31.
 -> IO Bool -- ^ returns @True@ if the argument was within bounds and the day
            -- was previously deselected.
calendarMarkDay self day =
  liftM toBool $
  {# call calendar_mark_day #}
    (toCalendar self)
    (fromIntegral day)

-- | Removes the visual marker from a particular day.
--
calendarUnmarkDay :: CalendarClass self => self
 -> Int     -- ^ @day@ - the day number to unmark between 1 and 31.
 -> IO Bool -- ^ returns @True@ if the argument was within bounds and the day
            -- was previously selected.
calendarUnmarkDay self day =
  liftM toBool $
  {# call calendar_unmark_day #}
    (toCalendar self)
    (fromIntegral day)

-- | Remove all visual markers.
--
calendarClearMarks :: CalendarClass self => self -> IO ()
calendarClearMarks self =
  {# call calendar_clear_marks #}
    (toCalendar self)

#if GTK_CHECK_VERSION(2,4,0)
-- | Sets display options (whether to display the heading and the month
-- headings).
--
-- * Available since Gtk version 2.4
--
calendarSetDisplayOptions :: CalendarClass self => self
 -> [CalendarDisplayOptions]
 -> IO ()
calendarSetDisplayOptions self flags =
  {# call calendar_set_display_options #}
    (toCalendar self)
    ((fromIntegral . fromFlags) flags)

-- | Returns the current display options for the calendar.
--
-- * Available since Gtk version 2.4
--
calendarGetDisplayOptions :: CalendarClass self => self
 -> IO [CalendarDisplayOptions]
calendarGetDisplayOptions self =
  liftM (toFlags . fromIntegral) $
  {# call calendar_get_display_options #}
    (toCalendar self)
#endif

#ifndef DISABLE_DEPRECATED
-- | Sets display options (whether to display the heading and the month
-- headings).
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code. Use 'calendarSetDisplayOptions' instead.
--
calendarDisplayOptions :: CalendarClass self => self
 -> [CalendarDisplayOptions] -> IO ()
calendarDisplayOptions self flags =
  {# call calendar_display_options #}
    (toCalendar self)
    ((fromIntegral . fromFlags) flags)
#endif

-- | Retrieve the currently selected date.
--
calendarGetDate :: CalendarClass self => self
 -> IO (Int,Int,Int) -- ^ @(year, month, day)@
calendarGetDate self =
  alloca $ \yearPtr ->
  alloca $ \monthPtr ->
  alloca $ \dayPtr -> do
  {# call unsafe calendar_get_date #}
    (toCalendar self)
    yearPtr
    monthPtr
    dayPtr
  year  <- liftM fromIntegral $ peek yearPtr
  month <- liftM fromIntegral $ peek monthPtr
  day   <- liftM fromIntegral $ peek dayPtr
  return (year,month,day)

-- | Freeze the calender for several update operations. This prevents flicker
-- that would otherwise result from a series of updates to the calendar.
--
calendarFreeze :: CalendarClass self => self
 -> IO a -- ^ An action that performs several update operations on the
         -- calendar. After the action finnishes all the changes made by it
         -- are displayed.
 -> IO a
calendarFreeze self update = do
  {# call unsafe calendar_freeze #} (toCalendar self)
  res <- update
  {# call calendar_thaw #} (toCalendar self)
  return res

--------------------
-- Properties

-- | \'displayOptions\' property. See 'calendarGetDisplayOptions' and
-- 'calendarSetDisplayOptions'
--
--calendarDisplayOptions :: CalendarClass self => Attr self [CalendarDisplayOptions]
--calendarDisplayOptions = Attr 
--  calendarGetDisplayOptions
--  calendarSetDisplayOptions

--------------------
-- Signals

-- | Emitted when a day was selected.
--
onDaySelected, afterDaySelected :: CalendarClass self => self
 -> IO ()
 -> IO (ConnectId self)
onDaySelected = connect_NONE__NONE "day-selected" False
afterDaySelected = connect_NONE__NONE "day-selected" True

-- | Emitted when a day received a double click.
--
onDaySelectedDoubleClick, afterDaySelectedDoubleClick :: CalendarClass self => self
 -> IO ()
 -> IO (ConnectId self)
onDaySelectedDoubleClick = 
  connect_NONE__NONE "day-selected-double-click" False
afterDaySelectedDoubleClick = 
  connect_NONE__NONE "day-selected-double-click" True

-- | The month changed.
--
onMonthChanged, afterMonthChanged :: CalendarClass self => self
 -> IO ()
 -> IO (ConnectId self)
onMonthChanged = connect_NONE__NONE "month-changed" False
afterMonthChanged = connect_NONE__NONE "month-changed" True

-- | The next month was selected.
--
onNextMonth, afterNextMonth :: CalendarClass self => self
 -> IO ()
 -> IO (ConnectId self)
onNextMonth = connect_NONE__NONE "next-month" False
afterNextMonth = connect_NONE__NONE "next-month" True

-- | The next year was selected.
--
onNextYear, afterNextYear :: CalendarClass self => self
 -> IO ()
 -> IO (ConnectId self)
onNextYear = connect_NONE__NONE "next-year" False
afterNextYear = connect_NONE__NONE "next-year" True

-- | The previous month was selected.
--
onPrevMonth, afterPrevMonth :: CalendarClass self => self
 -> IO ()
 -> IO (ConnectId self)
onPrevMonth = connect_NONE__NONE "prev-month" False
afterPrevMonth = connect_NONE__NONE "prev-month" True

-- | The previous year was selected.
--
onPrevYear, afterPrevYear :: CalendarClass self => self
 -> IO ()
 -> IO (ConnectId self)
onPrevYear = connect_NONE__NONE "prev-year" False
afterPrevYear = connect_NONE__NONE "prev-year" True
