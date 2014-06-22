{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK)
--
--  Author : Peter Gavin
--
--  Created: July 2007
--
--  Copyright (C) 2007 Peter Gavin
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
module System.Glib.GDateTime (
  GTimeValPart,
  GTimeVal(..),
  gGetCurrentTime,
  gUSleep,
  gTimeValAdd,
#if GLIB_CHECK_VERSION(2,12,0)
  gTimeValFromISO8601,
  gTimeValToISO8601,
#endif
  GDate(..),
  GDateDay,
  GDateMonth,
  GDateYear,
  GDateJulianDay,
  GDateWeekday,
  gDateValidJulian,
  gDateValidDMY,
  gDateNewJulian,
  gDateNewDMY,
  gDateSetDay,
  gDateSetMonth,
  gDateSetYear,
#if GLIB_CHECK_VERSION(2,10,0)
  gDateNewTimeVal,
#endif
  gDateParse,
  gDateAddDays,
  gDateSubtractDays,
  gDateAddMonths,
  gDateSubtractMonths,
  gDateAddYears,
  gDateSubtractYears,
  gDateDaysBetween,
  gDateCompare,
  gDateClamp,
  gDateDay,
  gDateMonth,
  gDateYear,
  gDateWeekday
  ) where

import Control.Monad (liftM)
import System.Glib.FFI
import System.Glib.UTFString

type GTimeValPart = {# type glong #}
data GTimeVal = GTimeVal { gTimeValSec  :: GTimeValPart
                         , gTimeValUSec :: GTimeValPart }
                deriving (Eq, Ord)
instance Storable GTimeVal where
    sizeOf _ = {# sizeof GTimeVal #}
    alignment _ = alignment (undefined :: CString)
    peek ptr =
        do sec <- {# get GTimeVal->tv_sec #} ptr
           uSec <- {# get GTimeVal->tv_usec #} ptr
           return $ GTimeVal sec uSec
    poke ptr (GTimeVal sec uSec) =
        do {# set GTimeVal->tv_sec #} ptr sec
           {# set GTimeVal->tv_usec #} ptr uSec

gGetCurrentTime :: IO GTimeVal
gGetCurrentTime =
    alloca $ \ptr ->
        do {# call g_get_current_time #} $ castPtr ptr
           peek ptr

gUSleep :: GTimeValPart
        -> IO ()
gUSleep microseconds =
    {# call g_usleep #} $ fromIntegral microseconds

gTimeValAdd :: GTimeVal
            -> GTimeValPart
            -> GTimeVal
gTimeValAdd time microseconds =
    unsafePerformIO $ with time $ \ptr ->
        do {# call g_time_val_add #} (castPtr ptr) microseconds
           peek ptr

#if GLIB_CHECK_VERSION(2,12,0)
gTimeValFromISO8601 :: GlibString string
                    => string
                    -> Maybe GTimeVal
gTimeValFromISO8601 isoDate =
    unsafePerformIO $ withUTFString isoDate $ \cISODate ->
        alloca $ \ptr ->
            do success <- liftM toBool $ {# call g_time_val_from_iso8601 #} cISODate $ castPtr ptr
               if success
                   then liftM Just $ peek ptr
                   else return Nothing

gTimeValToISO8601 :: GlibString string
                  => GTimeVal
                  -> string
gTimeValToISO8601 time =
    unsafePerformIO $ with time $ \ptr ->
        {# call g_time_val_to_iso8601 #} (castPtr ptr) >>= readUTFString
#endif

newtype GDateDay = GDateDay {# type GDateDay #}
    deriving (Eq, Ord)
instance Bounded GDateDay where
    minBound = GDateDay 1
    maxBound = GDateDay 31

{# enum GDateMonth {underscoreToCase} deriving (Eq, Ord) #}
instance Bounded GDateMonth where
    minBound = GDateJanuary
    maxBound = GDateDecember

newtype GDateYear = GDateYear {# type GDateYear #}
    deriving (Eq, Ord)
instance Bounded GDateYear where
    minBound = GDateYear 1
    maxBound = GDateYear (maxBound :: {# type guint16 #})

type GDateJulianDay = {# type guint32 #}
newtype GDate = GDate { gDateJulianDay :: GDateJulianDay }
    deriving (Eq)
instance Storable GDate where
    sizeOf _ = {# sizeof GDate #}
    alignment _ = alignment (undefined :: CString)
    peek =
        (liftM (GDate . fromIntegral)) . {# call g_date_get_julian #} . castPtr
    poke ptr val =
        {# call g_date_set_julian #} (castPtr ptr) $ gDateJulianDay val

{# enum GDateWeekday {underscoreToCase} deriving (Eq, Ord) #}
instance Bounded GDateWeekday where
    minBound = GDateMonday
    maxBound = GDateSunday

gDateValidJulian :: GDateJulianDay
                 -> Bool
gDateValidJulian =
    toBool . {# call fun g_date_valid_julian #}

gDateValidDMY :: GDateDay
              -> GDateMonth
              -> GDateYear
              -> Bool
gDateValidDMY (GDateDay day) month (GDateYear year) =
    toBool $ {# call fun g_date_valid_dmy #} day
                                             (fromIntegral $ fromEnum month)
                                             year

gDateNewJulian :: GDateJulianDay
               -> Maybe GDate
gDateNewJulian julian =
    if gDateValidJulian julian
        then Just $ GDate julian
        else Nothing

gDateNewDMY :: GDateDay
            -> GDateMonth
            -> GDateYear
            -> Maybe GDate
gDateNewDMY day month year =
    if gDateValidDMY day month year
        then Just $ unsafePerformIO $ alloca $ \ptr ->
            do let GDateDay day' = day
                   GDateYear year' = year
               {# call g_date_set_dmy #} (castPtr ptr)
                                         day'
                                         (fromIntegral $ fromEnum month)
                                         year'
               peek ptr
        else Nothing

gDateSetDay :: GDate
            -> GDateDay
            -> Maybe GDate
gDateSetDay date (GDateDay day) =
    unsafePerformIO $ with date $ \ptr ->
        do {# call g_date_set_day #} (castPtr ptr) day
           valid <- liftM toBool $ {# call g_date_valid #} $ castPtr ptr
           if valid
               then liftM Just $ peek ptr
               else return Nothing

gDateSetMonth :: GDate
              -> GDateMonth
              -> Maybe GDate
gDateSetMonth date month =
    unsafePerformIO $ with date $ \ptr ->
        do {# call g_date_set_month #} (castPtr ptr) $ fromIntegral $ fromEnum month
           valid <- liftM toBool $ {# call g_date_valid #} $ castPtr ptr
           if valid
               then liftM Just $ peek ptr
               else return Nothing

gDateSetYear :: GDate
             -> GDateYear
             -> Maybe GDate
gDateSetYear date (GDateYear year) =
    unsafePerformIO $ with date $ \ptr ->
        do {# call g_date_set_year #} (castPtr ptr) year
           valid <- liftM toBool $ {# call g_date_valid #} $ castPtr ptr
           if valid
               then liftM Just $ peek ptr
               else return Nothing

#if GLIB_CHECK_VERSION(2,10,0)
gDateNewTimeVal :: GTimeVal
                -> GDate
gDateNewTimeVal timeVal =
    unsafePerformIO $ alloca $ \ptr ->
        with timeVal $ \timeValPtr ->
        do {# call g_date_set_time_val #} (castPtr ptr) $ castPtr timeValPtr
           peek ptr
#endif

gDateParse :: GlibString string
           => string
           -> IO (Maybe GDate)
gDateParse str =
    alloca $ \ptr ->
        do withUTFString str $ {# call g_date_set_parse #} $ castPtr ptr
           valid <- liftM toBool $ {# call g_date_valid #} $ castPtr ptr
           if valid
               then liftM Just $ peek ptr
               else return Nothing

gDateAddDays :: GDate
             -> Word
             -> GDate
gDateAddDays date nDays =
    unsafePerformIO $ with date $ \ptr ->
        do {# call g_date_add_days #} (castPtr ptr) $ fromIntegral nDays
           peek ptr

gDateSubtractDays :: GDate
                  -> Word
                  -> GDate
gDateSubtractDays date nDays =
    unsafePerformIO $ with date $ \ptr ->
        do {# call g_date_subtract_days #} (castPtr ptr) $ fromIntegral nDays
           peek ptr

gDateAddMonths :: GDate
               -> Word
               -> GDate
gDateAddMonths date nMonths =
    unsafePerformIO $ with date $ \ptr ->
        do {# call g_date_add_months #} (castPtr ptr) $ fromIntegral nMonths
           peek ptr

gDateSubtractMonths :: GDate
                    -> Word
                    -> GDate
gDateSubtractMonths date nMonths =
    unsafePerformIO $ with date $ \ptr ->
        do {# call g_date_subtract_months #} (castPtr ptr) $ fromIntegral nMonths
           peek ptr

gDateAddYears :: GDate
              -> Word
              -> GDate
gDateAddYears date nYears =
    unsafePerformIO $ with date $ \ptr ->
        do {# call g_date_add_years #} (castPtr ptr) $ fromIntegral nYears
           peek ptr

gDateSubtractYears :: GDate
                   -> Word
                   -> GDate
gDateSubtractYears date nYears =
    unsafePerformIO $ with date $ \ptr ->
        do {# call g_date_subtract_years #} (castPtr ptr) $ fromIntegral nYears
           peek ptr

gDateDaysBetween :: GDate
                 -> GDate
                 -> Int
gDateDaysBetween date1 date2 =
    fromIntegral $ unsafePerformIO $ with date1 $ \ptr1 ->
        with date2 $ \ptr2 ->
            {# call g_date_days_between #} (castPtr ptr1) $ castPtr ptr2

gDateCompare :: GDate
             -> GDate
             -> Ordering
gDateCompare date1 date2 =
    let result = fromIntegral $ unsafePerformIO $ with date1 $ \ptr1 ->
                     with date2 $ \ptr2 ->
                         {# call g_date_compare #} (castPtr ptr1) $ castPtr ptr2
        ordering | result < 0 = LT
                 | result > 0 = GT
                 | otherwise  = EQ
    in ordering

instance Ord GDate where
    compare = gDateCompare

gDateClamp :: GDate
           -> GDate
           -> GDate
           -> GDate
gDateClamp date minDate maxDate =
    unsafePerformIO $ with date $ \ptr ->
        with minDate $ \minPtr ->
            with maxDate $ \maxPtr ->
                do {# call g_date_clamp #} (castPtr ptr) (castPtr minPtr) $ castPtr maxPtr
                   peek ptr

gDateDay :: GDate
         -> GDateDay
gDateDay date =
    GDateDay $ unsafePerformIO $ with date $ {# call g_date_get_day #} . castPtr

gDateMonth :: GDate
           -> GDateMonth
gDateMonth date =
    toEnum $ fromIntegral $ unsafePerformIO $ with date $ {# call g_date_get_month #} . castPtr

gDateYear :: GDate
          -> GDateYear
gDateYear date =
    GDateYear $ unsafePerformIO $ with date $ {# call g_date_get_year #} . castPtr

gDateWeekday :: GDate
             -> GDateWeekday
gDateWeekday date =
    toEnum $ fromIntegral $ unsafePerformIO $ with date $ {# call g_date_get_weekday #} . castPtr
