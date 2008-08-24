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
--  Maintainer  : gtk2hs-devel\@lists.sourceforge.net
--  Stability   : alpha
--  Portability : portable (depends on GHC)
--  
--  An object describing events that are passed up and down a pipeline.
module Media.Streaming.GStreamer.Core.Event (

-- * Detail

  -- | An 'Event' is a message that is passed up and down a pipeline.
  --   
  --   There are a number of predefined events and functions returning
  --   events. To send an event an application will usually use
  --   'Media.Streaming.GStreamer.Core.Element.elementSendEvent', and
  --   elements will use
  --   'Media.Streaming.GStreamer.Core.Pad.padSendEvent' or
  --   'Media.Streaming.GStreamer.Core.padPushEvent'.
  --   
  --   

-- * Types
  Event,
  EventClass,
  EventType(..),

-- * Event Operations
  eventType,
  eventNewCustom,
  eventNewEOS,
  eventNewFlushStart,
  eventNewFlushStop,
#if GSTREAMER_CHECK_VERSION(0,10,12)
  eventNewLatency,
#endif
  eventNewNavigation,
  eventNewNewSegment,
  eventNewNewSegmentFull,
  eventNewQOS,
  eventNewSeek,
  eventNewTag,
  eventParseBufferSize,
#if GSTREAMER_CHECK_VERSION(0,10,12)
  eventParseLatency,
#endif
  eventParseNewSegment,
  eventParseNewSegmentFull,
  eventParseQOS,
  eventParseSeek,
  eventParseTag,
  eventTypeGetName,
  eventTypeGetFlags,
  ) where


import Control.Monad (liftM)
{#import Media.Streaming.GStreamer.Core.Types#}
import System.Glib.FFI
import System.Glib.GObject
import System.Glib.UTFString
import System.Glib.GError

{# context lib = "gstreamer" prefix = "gst" #}

eventType :: EventClass event
          => event
          -> EventType
eventType event =
    cToEnum $ unsafePerformIO $ withMiniObject (toEvent event) cEventType
foreign import ccall unsafe "_hs_gst_event_type"
    cEventType :: Ptr Event
               -> IO {# type GstEventType #}

eventNewCustom :: EventType
               -> Structure
               -> IO Event
eventNewCustom eventType structure =
    {# call event_new_custom #} (cFromEnum eventType)
                                structure >>=
        takeMiniObject

eventNewEOS, eventNewFlushStart, eventNewFlushStop :: IO Event
eventNewEOS = {# call event_new_eos #} >>= takeMiniObject
eventNewFlushStart = {# call event_new_flush_start #} >>= takeMiniObject
eventNewFlushStop = {# call event_new_flush_stop #} >>= takeMiniObject

#if GSTREAMER_CHECK_VERSION(0,10,12)
eventNewLatency :: ClockTime
                -> IO Event
eventNewLatency latency =
    {# call event_new_latency #} (fromIntegral latency) >>=
        takeMiniObject
#endif

eventNewNavigation :: Structure
                   -> IO Event
eventNewNavigation structure =
    {# call event_new_navigation #} structure >>=
        takeMiniObject

eventNewNewSegment :: Bool
                   -> Double
                   -> Format
                   -> Int64
                   -> Int64
                   -> Int64
                   -> IO Event
eventNewNewSegment update rate format start stop position =
    {# call event_new_new_segment #} (fromBool update)
                                     (realToFrac rate)
                                     (fromIntegral $ fromFormat format)
                                     (fromIntegral start)
                                     (fromIntegral stop)
                                     (fromIntegral position) >>=
        takeMiniObject

eventNewNewSegmentFull :: Bool
                       -> Double
                       -> Double
                       -> Format
                       -> Int64
                       -> Int64
                       -> Int64
                       -> IO Event
eventNewNewSegmentFull update appliedRate rate format start stop position =
    {# call event_new_new_segment_full #} (fromBool update)
                                          (realToFrac rate)
                                          (realToFrac appliedRate)
                                          (fromIntegral $ fromFormat format)
                                          (fromIntegral start)
                                          (fromIntegral stop)
                                          (fromIntegral position) >>=
        takeMiniObject

eventNewQOS :: Double
            -> ClockTimeDiff
            -> ClockTime
            -> IO Event
eventNewQOS proportion diff timestamp =
    {# call event_new_qos #} (realToFrac proportion)
                             (fromIntegral diff)
                             (fromIntegral timestamp) >>=
        takeMiniObject

eventNewSeek :: Double
             -> Format
             -> [SeekFlags]
             -> SeekType
             -> Int64
             -> SeekType
             -> Int64
             -> IO Event
eventNewSeek rate format flags startType start stopType stop =
    {# call event_new_seek #} (realToFrac rate)
                              (fromIntegral $ fromFormat format)
                              (cFromFlags flags)
                              (cFromEnum startType)
                              (fromIntegral start)
                              (cFromEnum stopType)
                              (fromIntegral stop) >>=
        takeMiniObject

eventNewTag :: TagList
            -> IO Event
eventNewTag tagList =
    withTagList tagList ({# call event_new_tag #} . castPtr) >>=
        takeMiniObject

eventParseBufferSize :: EventClass event
                     => event
                     -> Maybe (Format, Int64, Int64, Bool)
eventParseBufferSize event | eventType event == EventBufferSize =
    Just $ unsafePerformIO $ alloca $ \formatPtr -> alloca $ \minSizePtr ->
        alloca $ \maxSizePtr -> alloca $ \asyncPtr ->
            do {# call event_parse_buffer_size #} (toEvent event)
                                                  formatPtr
                                                  minSizePtr
                                                  maxSizePtr
                                                  asyncPtr
               format <- liftM (toFormat . fromIntegral) $ peek formatPtr
               minSize <- liftM fromIntegral $ peek minSizePtr
               maxSize <- liftM fromIntegral $ peek maxSizePtr
               async <- liftM toBool $ peek asyncPtr
               return (format, minSize, maxSize, async)
                           | otherwise = Nothing

#if GSTREAMER_CHECK_VERSION(0,10,12)
eventParseLatency :: EventClass event
                  => event
                  -> Maybe ClockTime
eventParseLatency event | eventType event == EventLatency =
    Just $ unsafePerformIO $ alloca $ \latencyPtr ->
        do {# call event_parse_latency #} (toEvent event)
                                          latencyPtr
           liftM fromIntegral $ peek latencyPtr
                        | otherwise = Nothing
#endif

eventParseNewSegment :: EventClass event
                     => event
                     -> Maybe (Bool, Double, Format, Int64, Int64, Int64)
eventParseNewSegment event | eventType event == EventNewSegment =
    Just $ unsafePerformIO $ alloca $ \updatePtr ->
        alloca $ \ratePtr -> alloca $ \formatPtr ->
            alloca $ \startPtr -> alloca $ \stopPtr ->
                alloca $ \positionPtr ->
                    do {# call event_parse_new_segment #} (toEvent event)
                                                          ratePtr
                                                          updatePtr
                                                          formatPtr
                                                          startPtr
                                                          stopPtr
                                                          positionPtr
                       update <- liftM toBool $ peek updatePtr
                       rate <- liftM realToFrac $ peek ratePtr
                       format <- liftM (toFormat . fromIntegral) $ peek formatPtr
                       start <- liftM fromIntegral $ peek startPtr
                       stop <- liftM fromIntegral $ peek stopPtr
                       position <- liftM fromIntegral $ peek positionPtr
                       return (update, rate, format, start, stop, position)
                           | otherwise = Nothing

eventParseNewSegmentFull :: EventClass event
                         => event
                         -> Maybe (Bool, Double, Double, Format, Int64, Int64, Int64)
eventParseNewSegmentFull event | eventType event == EventNewSegment =
    Just $ unsafePerformIO $ alloca $ \updatePtr ->
        alloca $ \ratePtr -> alloca $ \appliedRatePtr ->
            alloca $ \formatPtr -> alloca $ \startPtr ->
                alloca $ \stopPtr -> alloca $ \positionPtr ->
                    do {# call event_parse_new_segment_full #} (toEvent event)
                                                               ratePtr
                                                               appliedRatePtr
                                                               updatePtr
                                                               formatPtr
                                                               startPtr
                                                               stopPtr
                                                               positionPtr
                       update <- liftM toBool $ peek updatePtr
                       rate <- liftM realToFrac $ peek ratePtr
                       appliedRate <- liftM realToFrac $ peek appliedRatePtr
                       format <- liftM (toFormat . fromIntegral) $ peek formatPtr
                       start <- liftM fromIntegral $ peek startPtr
                       stop <- liftM fromIntegral $ peek stopPtr
                       position <- liftM fromIntegral $ peek positionPtr
                       return (update, rate, appliedRate, format, start, stop, position)
                           | otherwise = Nothing

eventParseQOS :: EventClass event
              => event
              -> Maybe (Double, ClockTimeDiff, ClockTime)
eventParseQOS event | eventType event == EventQOS =
    Just $ unsafePerformIO $ alloca $ \proportionPtr ->
        alloca $ \diffPtr -> alloca $ \timestampPtr ->
            do {# call event_parse_qos #} (toEvent event)
                                          proportionPtr
                                          diffPtr
                                          timestampPtr
               proportion <- liftM realToFrac $ peek proportionPtr
               diff <- liftM fromIntegral $ peek diffPtr
               timestamp <- liftM fromIntegral $ peek timestampPtr
               return (proportion, diff, timestamp)
                    | otherwise = Nothing

eventParseSeek :: EventClass event
               => event
               -> Maybe (Double, Format, [SeekFlags], SeekType, Int64, SeekType, Int64)
eventParseSeek event | eventType event == EventSeek =
    Just $ unsafePerformIO $ alloca $ \ratePtr ->
        alloca $ \formatPtr -> alloca $ \flagsPtr ->
            alloca $ \startTypePtr -> alloca $ \startPtr ->
                alloca $ \stopTypePtr -> alloca $ \stopPtr ->
                    do {# call event_parse_seek #} (toEvent event)
                                                   ratePtr
                                                   formatPtr
                                                   flagsPtr
                                                   startTypePtr
                                                   startPtr
                                                   stopTypePtr
                                                   stopPtr
                       rate <- liftM realToFrac $ peek ratePtr
                       format <- liftM (toFormat . fromIntegral) $ peek formatPtr
                       flags <- liftM cToFlags $ peek flagsPtr
                       startType <- liftM cToEnum $ peek startTypePtr
                       start <- liftM fromIntegral $ peek startPtr
                       stopType <- liftM cToEnum $ peek stopTypePtr
                       stop <- liftM fromIntegral $ peek stopPtr
                       return (rate, format, flags, startType, start, stopType, stop)
                     | otherwise = Nothing

eventParseTag :: EventClass event
              => event
              -> Maybe TagList
eventParseTag event | eventType event == EventTag =
    Just $ unsafePerformIO $ alloca $ \tagListPtr ->
        do {# call event_parse_tag #} (toEvent event) (castPtr tagListPtr)
           peek tagListPtr >>= peekTagList
                    | otherwise = Nothing

eventTypeGetName :: EventType
                 -> String
eventTypeGetName eventType =
    unsafePerformIO $
        {# call event_type_get_name #} (cFromEnum eventType) >>=
            peekUTFString

eventTypeGetFlags :: EventType
                  -> [EventTypeFlags]
eventTypeGetFlags =
    cToFlags . {# call fun event_type_get_flags #} . cFromEnum
