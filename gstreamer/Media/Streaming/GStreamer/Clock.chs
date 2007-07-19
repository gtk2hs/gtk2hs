-- GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer   -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Copyright (c) 2007 Peter Gavin
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
module Media.Streaming.GStreamer.Clock (
  ) where

import Control.Monad (liftM, liftM4)
{#import Media.Streaming.GStreamer.Types#}
import System.Glib.FFI

{# context lib = "gstreamer" prefix = "gst" #}

clockAddObservation :: ClockClass clock
                    => clock
                    -> ClockTime
                    -> ClockTime
                    -> IO (Maybe Double)
clockAddObservation clock slave master =
    alloca $ \rSquaredPtr ->
        do success <- {# call clock_add_observation #} (toClock clock) slave master rSquaredPtr
           if toBool success
               then liftM (Just . realToFrac) $ peek rSquaredPtr
               else return Nothing

clockSetMaster :: (ClockClass clock, ClockClass master)
               => clock
               -> master
               -> IO Bool
clockSetMaster clock master =
    liftM toBool $ {# call clock_set_master #} (toClock clock) (toClock master)

clockGetMaster :: ClockClass clock
               => clock
               -> IO (Maybe Clock)
clockGetMaster clock =
    {# call clock_get_master #} (toClock clock) >>= maybePeek newClock

clockSetResolution :: ClockClass clock
                   => clock
                   -> ClockTime
                   -> IO ClockTime
clockSetResolution =
    {# call clock_set_resolution #} . toClock

clockGetResolution :: ClockClass clock
                   => clock
                   -> IO ClockTime
clockGetResolution =
    {# call clock_get_resolution #} . toClock

clockGetTime :: ClockClass clock
             => clock
             -> IO ClockTime
clockGetTime =
    {# call clock_get_time #} . toClock

clockNewSingleShotID :: ClockClass clock
                     => clock
                     -> ClockTime
                     -> IO ClockID
clockNewSingleShotID clock time =
    {# call clock_new_single_shot_id #} (toClock clock) time >>=
        newClockID . castPtr

clockNewPeriodicID :: ClockClass clock
                   => clock
                   -> ClockTime
                   -> ClockTime
                   -> IO ClockID
clockNewPeriodicID clock startTime interval =
    {# call clock_new_periodic_id #} (toClock clock) startTime interval >>=
        newClockID . castPtr

clockGetInternalTime :: ClockClass clock
                     => clock
                     -> IO ClockTime
clockGetInternalTime =
    {# call clock_get_internal_time #} . toClock

clockGetCalibration :: ClockClass clock
                    => clock
                    -> IO (ClockTime, ClockTime, ClockTime, ClockTime)
clockGetCalibration clock =
    alloca $ \internalPtr ->
        alloca $ \externalPtr ->
            alloca $ \rateNumPtr ->
                alloca $ \rateDenomPtr ->
                    do {# call clock_get_calibration #} (toClock clock)
                                                        internalPtr
                                                        externalPtr
                                                        rateNumPtr
                                                        rateDenomPtr
                       liftM4 (\a b c d -> (a, b, c, d))
                              (peek internalPtr)
                              (peek externalPtr)
                              (peek rateNumPtr)
                              (peek rateDenomPtr)

clockSetCalibration :: ClockClass clock
                    => clock
                    -> ClockTime
                    -> ClockTime
                    -> ClockTime
                    -> ClockTime
                    -> IO ()
clockSetCalibration =
    {# call clock_set_calibration #} . toClock

clockIDGetTime :: ClockID
               -> IO ClockTime
clockIDGetTime clockID =
    withClockID clockID $ {# call clock_id_get_time #} . castPtr

clockIDWait :: ClockID
            -> IO (ClockReturn, ClockTimeDiff)
clockIDWait clockID =
    alloca $ \jitterPtr ->
        do result <- withClockID clockID $ \clockIDPtr ->
                         {# call clock_id_wait #} (castPtr clockIDPtr) jitterPtr
           jitter <- peek jitterPtr
           return $ (toClockReturn result, jitter)

clockIDUnschedule :: ClockID
                  -> IO ()
clockIDUnschedule clockID =
    withClockID clockID $ {# call clock_id_unschedule #} . castPtr

