--  GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Copyright (c) 2007 Peter Gavin
--
--  This library is free software: you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public License
--  as published by the Free Software Foundation, either version 3 of
--  the License, or (at your option) any later version.
--  
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--  
--  You should have received a copy of the GNU Lesser General Public
--  License along with this program.  If not, see
--  <http://www.gnu.org/licenses/>.
--  
--  GStreamer, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GStreamer documentation.
--  
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module Media.Streaming.GStreamer.Core.Clock (
  
  Clock,
  ClockClass,
  castToClock,
  toClock,
  
  ClockTime,
  clockTimeNone,
  clockTimeIsValid,
  second,
  msecond,
  usecond,
  nsecond,
  
  ClockTimeDiff,
  ClockReturn(..),
  ClockID,
  
  ClockFlags(..),
  clockGetFlags,
  clockSetFlags,
  clockUnsetFlags,
  
  clockAddObservation,
  clockSetMaster,
  clockGetMaster,
  clockSetResolution,
  clockGetResolution,
  clockGetTime,
  clockNewSingleShotID,
  clockNewPeriodicID,
  clockGetInternalTime,
  clockGetCalibration,
  clockSetCalibration,
  clockIDGetTime,
  clockIDWait,
  clockIDUnschedule,
  
  clockTimeout,
  clockWindowSize,
  clockWindowThreshold
  
  ) where

import Data.Ratio ( Ratio
                  , (%)
                  , numerator
                  , denominator )
import Control.Monad (liftM, liftM4)
{#import Media.Streaming.GStreamer.Core.Types#}
import System.Glib.FFI
import System.Glib.Attributes ( Attr
                              , newAttr )
import System.Glib.Properties

{# context lib = "gstreamer" prefix = "gst" #}

clockGetFlags :: ClockClass clockT
              => clockT
              -> IO [ClockFlags]
clockGetFlags = mkObjectGetFlags

clockSetFlags :: ClockClass clockT
              => clockT
              -> [ClockFlags]
              -> IO ()
clockSetFlags = mkObjectSetFlags

clockUnsetFlags :: ClockClass clockT
                => clockT
                -> [ClockFlags]
                -> IO ()
clockUnsetFlags = mkObjectUnsetFlags

clockTimeIsValid :: ClockTime
                 -> Bool
clockTimeIsValid = (/= clockTimeNone)

clockAddObservation :: ClockClass clock
                    => clock
                    -> ClockTime
                    -> ClockTime
                    -> IO (Maybe Double)
clockAddObservation clock slave master =
    alloca $ \rSquaredPtr ->
        do success <- {# call clock_add_observation #} (toClock clock)
                                                       (fromIntegral slave)
                                                       (fromIntegral master)
                                                       rSquaredPtr
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
    {# call clock_get_master #} (toClock clock) >>= maybePeek takeObject

clockSetResolution :: ClockClass clock
                   => clock
                   -> ClockTime
                   -> IO ClockTime
clockSetResolution clock resolution =
    liftM fromIntegral $
        {# call clock_set_resolution #} (toClock clock)
                                        (fromIntegral resolution)

clockGetResolution :: ClockClass clock
                   => clock
                   -> IO ClockTime
clockGetResolution clock =
    liftM fromIntegral $
        {# call clock_get_resolution #} (toClock clock)

clockGetTime :: ClockClass clock
             => clock
             -> IO ClockTime
clockGetTime clock =
    liftM fromIntegral $
        {# call clock_get_time #} (toClock clock)

clockNewSingleShotID :: ClockClass clock
                     => clock
                     -> ClockTime
                     -> IO ClockID
clockNewSingleShotID clock time =
    {# call clock_new_single_shot_id #} (toClock clock)
                                        (fromIntegral time) >>=
        takeClockID . castPtr

clockNewPeriodicID :: ClockClass clock
                   => clock
                   -> ClockTime
                   -> ClockTime
                   -> IO ClockID
clockNewPeriodicID clock startTime interval =
    {# call clock_new_periodic_id #} (toClock clock)
                                     (fromIntegral startTime)
                                     (fromIntegral interval) >>=
        takeClockID . castPtr

clockGetInternalTime :: ClockClass clock
                     => clock
                     -> IO ClockTime
clockGetInternalTime clock =
    liftM fromIntegral $
        {# call clock_get_internal_time #} (toClock clock)

clockGetCalibration :: ClockClass clock
                    => clock
                    -> IO (ClockTime, ClockTime, Ratio ClockTime)
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
                       liftM4 (\a b c d ->
                               (fromIntegral a,
                                fromIntegral b,
                                (fromIntegral c) % (fromIntegral d)))
                              (peek internalPtr)
                              (peek externalPtr)
                              (peek rateNumPtr)
                              (peek rateDenomPtr)

clockSetCalibration :: ClockClass clock
                    => clock
                    -> ClockTime
                    -> ClockTime
                    -> Ratio ClockTime
                    -> IO ()
clockSetCalibration clock internal external rate =
    {# call clock_set_calibration #} (toClock clock)
                                     (fromIntegral internal)
                                     (fromIntegral external)
                                     (fromIntegral $ numerator rate)
                                     (fromIntegral $ denominator rate)

clockIDGetTime :: ClockID
               -> IO ClockTime
clockIDGetTime clockID =
    liftM fromIntegral $ withClockID clockID $
        {# call clock_id_get_time #} . castPtr

clockIDWait :: ClockID
            -> IO (ClockReturn, ClockTimeDiff)
clockIDWait clockID =
    alloca $ \jitterPtr ->
        do result <- withClockID clockID $ \clockIDPtr ->
                         {# call clock_id_wait #} (castPtr clockIDPtr) jitterPtr
           jitter <- peek jitterPtr
           return $ (cToEnum result, fromIntegral jitter)

clockIDUnschedule :: ClockID
                  -> IO ()
clockIDUnschedule clockID =
    withClockID clockID $ {# call clock_id_unschedule #} . castPtr

clockTimeout :: ClockClass clockT
             => Attr clockT ClockTime
clockTimeout = newAttr
    (objectGetPropertyUInt64 "timeout")
    (objectSetPropertyUInt64 "timeout")

clockWindowSize :: ClockClass clockT
                => Attr clockT Int
clockWindowSize =
    newAttrFromIntProperty "window-size"

clockWindowThreshold :: ClockClass clockT
                     => Attr clockT Int
clockWindowThreshold =
    newAttrFromIntProperty "window-threshold"
