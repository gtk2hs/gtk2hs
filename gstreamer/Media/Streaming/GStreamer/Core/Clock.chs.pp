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
--  |
--  Maintainer  : gtk2hs-devel@lists.sourceforge.net
--  Stability   : alpha
--  Portability : portable (depends on GHC)
--  
--  Abstract class of global clocks.
module Media.Streaming.GStreamer.Core.Clock (
  
-- * Detail
  -- | GStreamer uses a global clock to synchronize the plugins in a
  --   pipeline. Different clock implementations are possible by
  --   implementing this abstract base class.
  --   
  --   The 'Clock' returns a monotonically increasing time with the
  --   method 'clockGetTime'. Its accuracy and base time depend
  --   on the specific clock implementation but time is always
  --   expressed in nanoseconds. Since the baseline of the clock is
  --   undefined, the clock time returned is not meaningful in itself,
  --   what matters are the deltas between two clock times. The time
  --   returned by a clock is called the absolute time.
  --   
  --   The pipeline uses the clock to calculate the stream
  --   time. Usually all renderers synchronize to the global clock
  --   using the buffer timestamps, the newsegment events and the
  --   element's base time, see GstPipeline.
  --   
  --   A clock implementation can support periodic and single shot
  --   clock notifications both synchronous and asynchronous.
  --   
  --   One first needs to create a 'ClockID' for the periodic or
  --   single shot notification using 'clockNewSingleShotID' or
  --   'clockNewPeriodicID'.
  --   
  --   To perform a blocking wait for the specific time of the
  --   'ClockID' use 'clockIDWait'. This calls can be interrupted with
  --   the 'clockIDUnschedule' call. If the blocking wait is
  --   unscheduled a return value of 'ClockUnscheduled' is returned.
  --   
  --   Periodic callbacks scheduled async will be repeadedly called
  --   automatically until it is unscheduled. To schedule a sync
  --   periodic callback, 'clockIDWait' should be called repeatedly.
  --   
  --   The async callbacks can happen from any thread, either provided
  --   by the core or from a streaming thread. The application should
  --   be prepared for this.
  --   
  --   A 'ClockID' that has been unscheduled cannot be used again for
  --   any wait operation; a new 'ClockID' should be created.
  --   
  --   It is possible to perform a blocking wait on the same 'ClockID'
  --   from multiple threads. However, registering the same 'ClockID'
  --   for multiple async notifications is not possible, the callback
  --   will only be called for the thread registering the entry last.
  --   
  --   These clock operations do not operate on the stream time, so
  --   the callbacks will also occur when not in the playing state as
  --   if the clock just keeps on running. Some clocks however do not
  --   progress when the element that provided the clock is not
  --   playing.
  --   
  --   When a clock has the 'ClockFlagCanSetMaster' flag set, it can
  --   be slaved to another 'Clock' with 'clockSetMaster'. The clock
  --   will then automatically be synchronized to this master clock by
  --   repeatedly sampling the master clock and the slave clock and
  --   recalibrating the slave clock with 'clockSetCalibration'. This
  --   feature is mostly useful for plugins that have an internal
  --   clock but must operate with another clock selected by the
  --   GstPipeline. They can track the offset and rate difference of
  --   their internal clock relative to the master clock by using the
  --   'clockGetCalibration' function.
  --   
  --   The master\/slave synchronisation can be tuned with the
  --   the 'clockTimeout', 'clockWindowSize' and 'clockWindowThreshold' properties.
  --   The 'clockTimeout' property defines the interval to
  --   sample the master clock and run the calibration
  --   functions. 'clockWindowSize' defines the number of samples to
  --   use when calibrating and 'clockWindowThreshold' defines the
  --   minimum number of samples before the calibration is performed.

-- * Types
  Clock,
  ClockClass,
  castToClock,
  gTypeClock,
  
  -- | A time value measured in nanoseconds.
  ClockTime,
  
  -- | The 'ClockTime' value representing an invalid time.
  clockTimeNone,
  clockTimeIsValid,
  
  -- | The 'ClockTime' value representing 1 second, i.e. 1e9.
  second,
  -- | The 'ClockTime' value representing 1 millisecond, i.e. 1e6.
  msecond,
  -- | The 'ClockTime' value representing 1 microsecond, i.e. 1e3.
  usecond,
  -- | The 'ClockTime' value representing 1 nanosecond, i.e. 1.
  nsecond,
  -- | A value holding the difference between two 'ClockTime's.
  ClockTimeDiff,
  -- | An opaque identifier for a timer event.
  ClockID,
  -- | An enumeration type returned by 'clockIDWait'.
  ClockReturn(..),
  -- | The flags a 'Clock' may have.
  ClockFlags(..),
  clockGetFlags,
  clockSetFlags,
  clockUnsetFlags,

-- * Clock Operations  
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
  
-- * Clock Properties
  clockTimeout,
  clockWindowSize,
  clockWindowThreshold
  
  ) where

import Data.Ratio ( Ratio
                  , (%)
                  , numerator
                  , denominator )
import Control.Monad ( liftM
                     , liftM4)
{#import Media.Streaming.GStreamer.Core.Types#}
import System.Glib.FFI
#if __GLASGOW_HASKELL__ < 606
    hiding ( withObject )
#endif
import System.Glib.Attributes ( Attr
                              , newAttr )
import System.Glib.Properties

{# context lib = "gstreamer" prefix = "gst" #}

-- | Get the flags set on the clock.
clockGetFlags :: ClockClass clockT
              => clockT          -- ^ @clock@
              -> IO [ClockFlags] -- ^ the flags currently set on the clock
clockGetFlags = mkObjectGetFlags

-- | Set the given flags on the clock.
clockSetFlags :: ClockClass clockT
              => clockT       -- ^ @clock@
              -> [ClockFlags] -- ^ @flags@ - the flags to be set
              -> IO ()
clockSetFlags = mkObjectSetFlags

-- | Unset the given flags on the clock.
clockUnsetFlags :: ClockClass clockT
                => clockT       -- ^ @clock@
                -> [ClockFlags] -- ^ @flags@ - the flags to be unset
                -> IO ()
clockUnsetFlags = mkObjectUnsetFlags

-- | Returns 'True' if the given 'ClockTime' is valid, and 'False'
--   otherwise.
clockTimeIsValid :: ClockTime -- ^ @clockTime@
                 -> Bool      -- ^ 'True' if @clockTime@ is valid, 'False' otherwise
clockTimeIsValid = (/= clockTimeNone)

-- | The time master of the master clock and the time slave of the
--   slave clock are added to the list of observations. If enough
--   observations are available, a linear regression algorithm is run
--   on the observations and clock is recalibrated.
--   
--   If a calibration is performed, the correlation coefficient of the
--   interpolation will be returned. A value of 1.0 means the clocks
--   are in perfect sync. This value can be used to control the
--   sampling frequency of the master and slave clocks.
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

-- | Set @master@ as the master clock for @clock@. The @clock@ will
--   automatically be calibrated so that 'clockGetTime' reports the
--   same time as the @master@ clock.
--   
--   A clock provider that slaves its clock to a master can get the
--   current calibration values with 'clockGetCalibration'.
--   
--   The @master@ clock can be 'Nothing' in which case @clock@ will
--   not be slaved any longer. It will, however, continue to report
--   its time adjusted using the last configured rate and time
--   offsets.
--   
--   Note that if @clock@ does not have the 'ClockFlagCanSetMaster'
--   flag set, this function will not succeed and return 'False'.
clockSetMaster :: (ClockClass clock, ClockClass master)
               => clock        -- ^ @clock@
               -> Maybe master -- ^ @master@
               -> IO Bool      -- ^ 'True' if @clock@ is capable of
                               --   being slaved to the @master@ clock, otherwise 'False'
clockSetMaster clock master =
    withObject (toClock clock) $ \clockPtr ->
        maybeWith withObject (liftM toClock $ master) $ \masterPtr ->
            liftM toBool $ gst_clock_set_master clockPtr masterPtr
    where
      _ = {# call clock_set_master #}

-- | Return the master that @clock@ is slaved to, or 'Nothing' if
--   @clock@ is not slaved.
clockGetMaster :: ClockClass clock
               => clock            -- ^ @clock@
               -> IO (Maybe Clock) -- ^ the master that @clock@ is slaved to, or 'Nothing'
clockGetMaster clock =
    {# call clock_get_master #} (toClock clock) >>= maybePeek takeObject

-- | Set the resolution of @clock@. Some clocks have the possibility
--   to operate with different resolution at the expense of more
--   resource usage. There is normally no need to change the default
--   resolution of a clock. The resolution of a clock can only be
--   changed if the clock has the 'ClockFlagCanSetResolution' flag
--   set.
clockSetResolution :: ClockClass clock
                   => clock
                   -> ClockTime
                   -> IO ClockTime
clockSetResolution clock resolution =
    liftM fromIntegral $
        {# call clock_set_resolution #} (toClock clock)
                                        (fromIntegral resolution)

-- | Get the resolution of the @clock@. The resolution of the clock is
--   the granularity of the values returned by 'clockGetTime'.
clockGetResolution :: ClockClass clock
                   => clock        -- ^ @clock@ - 
                   -> IO ClockTime -- ^ the resolution currently set in @clock@
clockGetResolution clock =
    liftM fromIntegral $
        {# call clock_get_resolution #} (toClock clock)

-- | Get the current time stored in @clock@. The time is always
--   monotonically increasing and adjusted according to the current
--   offset and rate.
clockGetTime :: ClockClass clock
             => clock        -- ^ @clock@
             -> IO ClockTime -- ^ the current time in @clock@
clockGetTime clock =
    liftM fromIntegral $
        {# call clock_get_time #} (toClock clock)

-- | Get a 'ClockID' from @clock@ to trigger a single shot
--   notification at the requested time.
clockNewSingleShotID :: ClockClass clock
                     => clock      -- ^ @clock@
                     -> ClockTime  -- ^ @clockTime@
                     -> IO ClockID -- ^ a single shot notification id triggered at @clockTime@
clockNewSingleShotID clock time =
    {# call clock_new_single_shot_id #} (toClock clock)
                                        (fromIntegral time) >>=
        takeClockID . castPtr

-- | Get a 'ClockID' from @clock@ to trigger periodic
--   notifications. The notifications will start at time @startTime@
--   and then be fired at each @interval@ after.
clockNewPeriodicID :: ClockClass clock
                   => clock      -- ^ @clock@
                   -> ClockTime  -- ^ @startTime@
                   -> ClockTime  -- ^ @interval@
                   -> IO ClockID -- ^ a periodic notification id
clockNewPeriodicID clock startTime interval =
    {# call clock_new_periodic_id #} (toClock clock)
                                     (fromIntegral startTime)
                                     (fromIntegral interval) >>=
        takeClockID . castPtr

-- | Gets the current internal time of @clock@. The time is
--   returned unadjusted in the offset and rate.
clockGetInternalTime :: ClockClass clock
                     => clock        -- ^ @clock@
                     -> IO ClockTime -- ^ the clock's internal time value
clockGetInternalTime clock =
    liftM fromIntegral $
        {# call clock_get_internal_time #} (toClock clock)

-- | Gets the internal rate and reference time of @clock@. See
--   'clockSetCalibration' for more information.
clockGetCalibration :: ClockClass clock
                    => clock -- ^ @clock@
                    -> IO (ClockTime, ClockTime, Ratio ClockTime)
                       -- ^ the clock's internal time, external (adjusted) time, and skew rate
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
                                fromIntegral c % fromIntegral d))
                              (peek internalPtr)
                              (peek externalPtr)
                              (peek rateNumPtr)
                              (peek rateDenomPtr)

-- | Adjusts the rate and time of clock. A rate of @1 % 1@ is the
--   normal speed of the clock. Larger values make the clock go
--   faster.
--   
--   The parameters @internal@ and @external@ specifying that
--   'clockGetTime' should have returned @external@ when the clock had
--   internal time @internal@. The parameter @internal@ should not be
--   in the future; that is, it should be less than the value returned
--   by 'clockGetInternalTime' when this function is called.
--   
--   Subsequent calls to 'clockGetTime' will return clock times
--   computed as follows:
--   
--   > (clock_internal - internal) * rate + external
--   
--   Note that 'clockGetTime' always returns increasing values, so if
--   the clock is moved backwards, 'clockGetTime' will report the
--   previous value until the clock catches up.
clockSetCalibration :: ClockClass clock
                    => clock           -- ^ @clock@
                    -> ClockTime       -- ^ @internal@
                    -> ClockTime       -- ^ @external@
                    -> Ratio ClockTime -- ^ @rate@
                    -> IO ()
clockSetCalibration clock internal external rate =
    {# call clock_set_calibration #} (toClock clock)
                                     (fromIntegral internal)
                                     (fromIntegral external)
                                     (fromIntegral $ numerator rate)
                                     (fromIntegral $ denominator rate)

-- | Get the time of @clockID@.
clockIDGetTime :: ClockID      -- ^ @clockID@
               -> IO ClockTime
clockIDGetTime clockID =
    liftM fromIntegral $ withClockID clockID $
        {# call clock_id_get_time #} . castPtr

-- | Perform a blocking wait on @clockID@. The parameter @clockID@
--   should have been created with 'clockNewSingleShotID' or
--   'clockNewPeriodicID', and should not been unscheduled with a call
--   to 'clockIDUnschedule'.
--   
--   If second value in the returned pair is not 'Nothing', it will
--   contain the difference against the clock and the time of
--   @clockID@ when this method was called. Positive values indicate
--   how late @clockID@ was relative to the clock. Negative values
--   indicate how much time was spend waiting on the clock before the
--   function returned.
clockIDWait :: ClockID -- ^ @clockID@
            -> IO (ClockReturn, Maybe ClockTimeDiff)
clockIDWait clockID =
    alloca $ \jitterPtr ->
        do result <- liftM cToEnum $ withClockID clockID $ \clockIDPtr ->
                         {# call clock_id_wait #} (castPtr clockIDPtr) jitterPtr
           jitter <- let peekJitter = liftM (Just . fromIntegral) $ peek jitterPtr
                     in case result of
                          ClockOk -> peekJitter
                          ClockEarly -> peekJitter
                          _ -> return Nothing
           return (result, jitter)

-- | Cancel an outstanding request with @clockID@. After this call,
--   @clockID@ cannot be used anymore to recieve notifications; you
--   must create a new 'ClockID'.
clockIDUnschedule :: ClockID -- ^ @clockID@
                  -> IO ()
clockIDUnschedule clockID =
    withClockID clockID $ {# call clock_id_unschedule #} . castPtr

-- | The amount of time, in nanoseconds, between samples.
clockTimeout :: ClockClass clockT
             => Attr clockT ClockTime
clockTimeout = newAttr
    (objectGetPropertyUInt64 "timeout")
    (objectSetPropertyUInt64 "timeout")

-- | The size of the window used to calculate rate and offset.
clockWindowSize :: ClockClass clockT
                => Attr clockT Int
clockWindowSize =
    newAttrFromIntProperty "window-size"

-- | The threshold to start calculating rate and offset.
clockWindowThreshold :: ClockClass clockT
                     => Attr clockT Int
clockWindowThreshold =
    newAttrFromIntProperty "window-threshold"
