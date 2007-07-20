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
module Media.Streaming.GStreamer.Segment (
  
  Segment(..),
  
  segmentClip,
  segmentSetDuration,
  segmentSetLastStop,
  segmentSetNewsegment,
  segmentSetSeek,
  segmentToRunningTime,
  segmentToStreamTime
  
  ) where

{# context lib = "gstreamer" prefix = "gst" #}

import Control.Monad (liftM)
import System.Glib.FFI
import System.Glib.Flags
{#import Media.Streaming.GStreamer.Types#}

segmentClip :: Segment
            -> Format
            -> Int64
            -> Int64
            -> (Segment, Bool, Int64, Int64)
segmentClip segment format start stop =
    unsafePerformIO $ alloca $ \clipStartPtr ->
        alloca $ \clipStopPtr ->
            with segment $ \segmentPtr ->
                do result <- liftM toBool $
                                 {# call segment_clip #} (castPtr segmentPtr)
                                                         (fromFormat format)
                                                         (fromIntegral start)
                                                         (fromIntegral stop)
                                                         clipStartPtr
                                                         clipStopPtr
                   segment' <- peek segmentPtr
                   clipStart <- liftM fromIntegral $ peek clipStartPtr
                   clipStop <- liftM fromIntegral $ peek clipStopPtr
                   return (segment', result, clipStart, clipStop)

segmentSetDuration :: Segment
                   -> Format
                   -> Int64
                   -> Segment
segmentSetDuration segment format duration =
    unsafePerformIO $ with segment $ \segmentPtr ->
        do {# call segment_set_duration #} (castPtr segmentPtr)
                                           (fromFormat format)
                                           (fromIntegral duration)
           peek segmentPtr

segmentSetLastStop :: Segment
                   -> Format
                   -> Int64
                   -> Segment
segmentSetLastStop segment format position =
    unsafePerformIO $ with segment $ \segmentPtr ->
        do {# call segment_set_last_stop #} (castPtr segmentPtr)
                                            (fromFormat format)
                                            (fromIntegral position)
           peek segmentPtr

segmentSetNewsegment :: Segment
                     -> Bool
                     -> Double
                     -> Format
                     -> Int64
                     -> Int64
                     -> Int64
                     -> Segment
segmentSetNewsegment segment update rate format start stop time =
    unsafePerformIO $ with segment $ \segmentPtr ->
        do {# call segment_set_newsegment #} (castPtr segmentPtr)
                                             (fromBool update)
                                             (realToFrac rate)
                                             (fromFormat format)
                                             (fromIntegral start)
                                             (fromIntegral stop)
                                             (fromIntegral time)
           peek segmentPtr

segmentSetSeek :: Segment
               -> Double
               -> Format
               -> [SeekFlags]
               -> SeekType
               -> Int64
               -> SeekType
               -> Int64
               -> (Segment, Bool)
segmentSetSeek segment rate format flags startType start stopType stop =
    unsafePerformIO $ with segment $ \segmentPtr ->
        alloca $ \updatePtr ->
            do {# call segment_set_seek #} (castPtr segmentPtr)
                                           (realToFrac rate)
                                           (fromFormat format)
                                           (fromIntegral $ fromFlags flags)
                                           (fromSeekType startType)
                                           (fromIntegral start)
                                           (fromSeekType stopType)
                                           (fromIntegral stop)
                                           updatePtr
               update <- liftM toBool $ peek updatePtr
               segment' <- peek segmentPtr
               return (segment', update)

segmentToRunningTime :: Segment
                     -> Format
                     -> Int64
                     -> Int64
segmentToRunningTime segment format position =
    fromIntegral $ unsafePerformIO $ with segment $ \segmentPtr ->
        {# call segment_to_running_time #} (castPtr segmentPtr)
                                           (fromFormat format)
                                           (fromIntegral position)

segmentToStreamTime :: Segment
                    -> Format
                    -> Int64
                    -> Int64
segmentToStreamTime segment format position =
    fromIntegral $ unsafePerformIO $ with segment $ \segmentPtr ->
        {# call segment_to_stream_time #} (castPtr segmentPtr)
                                          (fromFormat format)
                                          (fromIntegral position)
