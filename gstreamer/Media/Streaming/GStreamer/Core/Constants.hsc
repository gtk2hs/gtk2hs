{-# OPTIONS_HADDOCK hide #-}
--  GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer -*-haskell-*-

#include "hsgstreamer.h"
#include "template-hsc-gtk2hs.h"

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

-- #hide
  
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module Media.Streaming.GStreamer.Core.Constants where

import Data.Int
import Data.Word
import System.Glib.Flags

-- | A time value in nanoseconds.
type ClockTime = Word64

-- | The undefined 'ClockTime' value.
clockTimeNone :: ClockTime
clockTimeNone = #{const GST_CLOCK_TIME_NONE}

second, msecond, usecond, nsecond :: ClockTime
-- | One second as a 'ClockTime' value.
second  = #{const GST_SECOND}
-- | One millisecond as a 'ClockTime' value.
msecond = #{const GST_MSECOND}
-- | One microsecond as a 'ClockTime' value.
usecond = #{const GST_USECOND}
-- | One nanosecond as a 'ClockTime' value.
nsecond = #{const GST_NSECOND}

-- | The type for buffer offsets.
type BufferOffset = Word64
-- | The undefined 'BufferOffset' value.
bufferOffsetNone :: BufferOffset
bufferOffsetNone = #{const GST_BUFFER_OFFSET_NONE}

-- | A format identifier.
newtype FormatId = FormatId #{gtk2hs_type GstFormat}
    deriving (Eq, Ord, Show)

-- | An enumeration of standard predefined formats.
data Format = FormatUndefined     -- ^ no format
            | FormatDefault       -- ^ the default format of the pad or element; this can be, e.g., samples for raw audio
            | FormatBytes         -- ^ bytes
            | FormatTime          -- ^ time in nanoseconds
            | FormatBuffers       -- ^ buffers
            | FormatPercent       -- ^ percentage of stream
            | FormatUser FormatId -- ^ a user defined format
              deriving (Eq, Ord, Show)
toFormat :: #{gtk2hs_type GstFormat} -> Format
toFormat n | n == #{const GST_FORMAT_UNDEFINED} = FormatUndefined
           | n == #{const GST_FORMAT_DEFAULT}   = FormatDefault
           | n == #{const GST_FORMAT_BYTES}     = FormatBytes
           | n == #{const GST_FORMAT_TIME}      = FormatTime
           | n == #{const GST_FORMAT_BUFFERS}   = FormatBuffers
           | n == #{const GST_FORMAT_PERCENT}   = FormatPercent
           | otherwise                          = FormatUser (FormatId n)
fromFormat :: Format -> #{gtk2hs_type GstFormat}
fromFormat FormatUndefined = #{const GST_FORMAT_UNDEFINED}
fromFormat FormatDefault   = #{const GST_FORMAT_DEFAULT}
fromFormat FormatBytes     = #{const GST_FORMAT_BYTES}
fromFormat FormatTime      = #{const GST_FORMAT_TIME}
fromFormat FormatBuffers   = #{const GST_FORMAT_BUFFERS}
fromFormat FormatPercent   = #{const GST_FORMAT_PERCENT}
fromFormat (FormatUser (FormatId id)) = id

-- | The format value for 'FormatPercent' is between 0 and this value.
formatPercentMax :: Int64
formatPercentMax = #{const GST_FORMAT_PERCENT_MAX}

-- | The value used to scale down the reported 'FormatPercent' format
--   value to its real value.
formatPercentScale :: Int64
formatPercentScale = #{const GST_FORMAT_PERCENT_SCALE}

-- | The flags that an 'Object' may have.
data ObjectFlags = ObjectDisposing  -- ^ The object has been
                                    --   destroyed, don't use it any
                                    --   more.
                   deriving (Bounded, Show)
instance Enum ObjectFlags where
    toEnum n | n == #{const GST_OBJECT_DISPOSING} = ObjectDisposing
    fromEnum ObjectDisposing = #{const GST_OBJECT_DISPOSING}
instance Flags ObjectFlags

-- | The flags that a 'Pad' may have.
data PadFlags = PadBlocked   -- ^ dataflow on the pad is blocked
              | PadFlushing  -- ^ the pad is refusing buffers
              | PadInGetCaps -- ^ 'padGetCaps' is executing
              | PadInSetCaps -- ^ 'padSetCaps' is executing
#if GST_CHECK_VERSION(0,10,11)
              | PadBlocking  -- ^ the pad is blocking on a buffer or event
#endif
                deriving (Eq, Bounded, Show)
instance Enum PadFlags where
    toEnum n | n == #{const GST_PAD_BLOCKED}    = PadBlocked
             | n == #{const GST_PAD_FLUSHING}   = PadFlushing
             | n == #{const GST_PAD_IN_GETCAPS} = PadInGetCaps
             | n == #{const GST_PAD_IN_SETCAPS} = PadInSetCaps
#if GST_CHECK_VERSION(0,10,11)
             | n == #{const GST_PAD_BLOCKING}   = PadBlocking
#endif
    fromEnum PadBlocked   = #{const GST_PAD_BLOCKED}
    fromEnum PadFlushing  = #{const GST_PAD_FLUSHING}
    fromEnum PadInGetCaps = #{const GST_PAD_IN_GETCAPS}
    fromEnum PadInSetCaps = #{const GST_PAD_IN_SETCAPS}
#if GST_CHECK_VERSION(0,10,11)
    fromEnum PadBlocking  = #{const GST_PAD_BLOCKING}
#endif
instance Flags PadFlags

-- | The flags that an 'Element' may have.
data ElementFlags = ElementLockedState -- ^ parent state changes are ignored
                  | ElementIsSink      -- ^ the element is a sink
                  | ElementUnparenting -- ^ child is being removed
                                       --   from the parent bin
                    deriving (Eq, Bounded, Show)
instance Enum ElementFlags where
    toEnum n | n == #{const GST_ELEMENT_LOCKED_STATE} = ElementLockedState
             | n == #{const GST_ELEMENT_IS_SINK}      = ElementIsSink
             | n == #{const GST_ELEMENT_UNPARENTING}  = ElementUnparenting
    fromEnum ElementLockedState = #{const GST_ELEMENT_LOCKED_STATE}
    fromEnum ElementIsSink      = #{const GST_ELEMENT_IS_SINK}
    fromEnum ElementUnparenting = #{const GST_ELEMENT_UNPARENTING}
instance Flags ElementFlags

-- | The different state changes that are passed to the state change
--   functions of 'Element's.
data StateChange = StateChangeNullToReady     -- ^ state change from 'StateNull' to 'StateReady'
                 | StateChangeReadyToPaused   -- ^ state change from 'StateReady' to 'StatePaused'
                 | StateChangePausedToPlaying -- ^ state change from 'StatePaused' to 'StatePlaying'
                 | StateChangePlayingToPaused -- ^ state change from 'StatePlaying' to 'StatePaused'
                 | StateChangePausedToReady   -- ^ state change from 'StatePaused' to 'StateReady'
                 | StateChangeReadyToNull     -- ^ state change from 'StateReady' to 'StateNull'
                   deriving (Eq, Show)
instance Enum StateChange where
    toEnum n | n == #{const GST_STATE_CHANGE_NULL_TO_READY}     = StateChangeNullToReady
             | n == #{const GST_STATE_CHANGE_READY_TO_PAUSED}   = StateChangeReadyToPaused
             | n == #{const GST_STATE_CHANGE_PAUSED_TO_PLAYING} = StateChangePausedToPlaying
             | n == #{const GST_STATE_CHANGE_PLAYING_TO_PAUSED} = StateChangePlayingToPaused
             | n == #{const GST_STATE_CHANGE_PAUSED_TO_READY}   = StateChangePausedToReady
             | n == #{const GST_STATE_CHANGE_READY_TO_NULL}     = StateChangeReadyToNull
    
    fromEnum StateChangeNullToReady     = #{const GST_STATE_CHANGE_NULL_TO_READY}
    fromEnum StateChangeReadyToPaused   = #{const GST_STATE_CHANGE_READY_TO_PAUSED}
    fromEnum StateChangePausedToPlaying = #{const GST_STATE_CHANGE_PAUSED_TO_PLAYING}
    fromEnum StateChangePlayingToPaused = #{const GST_STATE_CHANGE_PLAYING_TO_PAUSED}
    fromEnum StateChangePausedToReady   = #{const GST_STATE_CHANGE_PAUSED_TO_READY}
    fromEnum StateChangeReadyToNull     = #{const GST_STATE_CHANGE_READY_TO_NULL}

-- | The flags that a 'Bus' may have.
data BusFlags = BusFlushing -- ^ the bus is currently dropping all messages
                deriving (Eq, Bounded, Show)
instance Enum BusFlags where
    toEnum n | n == #{const GST_BUS_FLUSHING} = BusFlushing
    fromEnum BusFlushing = #{const GST_BUS_FLUSHING}
instance Flags BusFlags

-- | The flags that a 'Clock' may have.
data ClockFlags = ClockCanDoSingleSync    -- ^ the clock can do a single sync timeout request
                | ClockCanDoSingleAsync   -- ^ the clock can do a single async timeout request
                | ClockCanDoPeriodicSync  -- ^ the clock can do periodic sync timeout requests
                | ClockCanDoPeriodicAsync -- ^ the clock can do periodic async timeout requests
                | ClockCanSetResolution   -- ^ the clock's resolution can be changed
                | ClockCanSetMaster       -- ^ the clock can be slaved to a master clock
                  deriving (Eq, Bounded, Show)
instance Enum ClockFlags where
    toEnum n | n == #{const GST_CLOCK_FLAG_CAN_DO_SINGLE_SYNC}    = ClockCanDoSingleSync
             | n == #{const GST_CLOCK_FLAG_CAN_DO_SINGLE_ASYNC}   = ClockCanDoSingleAsync
             | n == #{const GST_CLOCK_FLAG_CAN_DO_PERIODIC_SYNC}  = ClockCanDoPeriodicSync
             | n == #{const GST_CLOCK_FLAG_CAN_DO_PERIODIC_ASYNC} = ClockCanDoPeriodicAsync
             | n == #{const GST_CLOCK_FLAG_CAN_SET_RESOLUTION}    = ClockCanSetResolution
             | n == #{const GST_CLOCK_FLAG_CAN_SET_MASTER}        = ClockCanSetMaster
    
    fromEnum ClockCanDoSingleSync    = #{const GST_CLOCK_FLAG_CAN_DO_SINGLE_SYNC}
    fromEnum ClockCanDoSingleAsync   = #{const GST_CLOCK_FLAG_CAN_DO_SINGLE_ASYNC}
    fromEnum ClockCanDoPeriodicSync  = #{const GST_CLOCK_FLAG_CAN_DO_PERIODIC_SYNC}
    fromEnum ClockCanDoPeriodicAsync = #{const GST_CLOCK_FLAG_CAN_DO_PERIODIC_ASYNC}
    fromEnum ClockCanSetResolution   = #{const GST_CLOCK_FLAG_CAN_SET_RESOLUTION}
    fromEnum ClockCanSetMaster       = #{const GST_CLOCK_FLAG_CAN_SET_MASTER}
instance Flags ClockFlags

-- | The flags an 'Index' may have.
data IndexFlags = IndexWritable -- ^ the index is writable
                | IndexReadable -- ^ the index is readable
                  deriving (Eq, Bounded, Show)
instance Enum IndexFlags where
    toEnum n | n == #{const GST_INDEX_WRITABLE} = IndexWritable
             | n == #{const GST_INDEX_READABLE} = IndexReadable
    fromEnum IndexWritable = #{const GST_INDEX_WRITABLE}
    fromEnum IndexReadable = #{const GST_INDEX_READABLE}
instance Flags IndexFlags

-- | The flags a 'MiniObject' may have.
data MiniObjectFlags = MiniObjectReadOnly -- ^ the object is not writable
                       deriving (Eq, Bounded, Show)
instance Enum MiniObjectFlags where
    toEnum n | n == #{const GST_MINI_OBJECT_FLAG_READONLY} = MiniObjectReadOnly
    fromEnum MiniObjectReadOnly = #{const GST_MINI_OBJECT_FLAG_READONLY}
instance Flags MiniObjectFlags

-- | The flags a 'Buffer' may have.
data BufferFlags = BufferPreroll   -- ^ the buffer is part of a preroll and should not be displayed
                 | BufferDiscont   -- ^ the buffer marks a discontinuity in the stream
                 | BufferInCaps    -- ^ the buffer has been added as a field in a 'Caps'
                 | BufferGap       -- ^ the buffer has been created to fill a gap in the stream
                 | BufferDeltaUnit -- ^ the buffer cannot be decoded independently
                   deriving (Eq, Bounded, Show)
instance Enum BufferFlags where
    toEnum n | n == #{const GST_BUFFER_FLAG_PREROLL}    = BufferPreroll
             | n == #{const GST_BUFFER_FLAG_DISCONT}    = BufferDiscont
             | n == #{const GST_BUFFER_FLAG_IN_CAPS}    = BufferInCaps
             | n == #{const GST_BUFFER_FLAG_GAP}        = BufferGap
             | n == #{const GST_BUFFER_FLAG_DELTA_UNIT} = BufferDeltaUnit
    fromEnum BufferPreroll   = #{const GST_BUFFER_FLAG_PREROLL}
    fromEnum BufferDiscont   = #{const GST_BUFFER_FLAG_DISCONT}
    fromEnum BufferInCaps    = #{const GST_BUFFER_FLAG_IN_CAPS}
    fromEnum BufferGap       = #{const GST_BUFFER_FLAG_GAP}
    fromEnum BufferDeltaUnit = #{const GST_BUFFER_FLAG_DELTA_UNIT}
instance Flags BufferFlags

-- | The event types that may occur in a pipeline.
data EventType = EventUnknown             -- ^ an unknown event
               | EventFlushStart          -- ^ start a flush operation
               | EventFlushStop           -- ^ stop a flush operation
               | EventEOS                 -- ^ end of stream
               | EventNewSegment          -- ^ a new segment follows in the dataflow
               | EventTag                 -- ^ a new set of metadata tags has been found
               | EventBufferSize          -- ^ notification of buffering requirements
               | EventQOS                 -- ^ quality of service notification
               | EventSeek                -- ^ a request for a new playback position and rate
               | EventNavigation          -- ^ notification of user request
#if GST_CHECK_VERSION(0, 10, 12)
               | EventLatency             -- ^ notification of latency adjustment
#endif
               | EventCustomUpstream      -- ^ custom upstream event
               | EventCustomDownstream    -- ^ custom downstream event
               | EventCustomDownstreamOOB -- ^ custom downstream out-of-band event
               | EventCustomBoth          -- ^ custom bidirectional event
               | EventCustomBothOOB       -- ^ custom bidirectional out-of-band event
                 deriving (Eq, Bounded, Show)
instance Enum EventType where
    toEnum n | n == #{const GST_EVENT_UNKNOWN} = EventUnknown
             | n == #{const GST_EVENT_FLUSH_START} = EventFlushStart
             | n == #{const GST_EVENT_FLUSH_STOP} = EventFlushStop
             | n == #{const GST_EVENT_EOS} = EventEOS
             | n == #{const GST_EVENT_NEWSEGMENT} = EventNewSegment
             | n == #{const GST_EVENT_TAG} = EventTag
             | n == #{const GST_EVENT_BUFFERSIZE} = EventBufferSize
             | n == #{const GST_EVENT_QOS} = EventQOS
             | n == #{const GST_EVENT_SEEK} = EventSeek
             | n == #{const GST_EVENT_NAVIGATION} = EventNavigation
#if GST_CHECK_VERSION(0, 10, 12)
             | n == #{const GST_EVENT_LATENCY} = EventLatency
#endif
             | n == #{const GST_EVENT_CUSTOM_UPSTREAM} = EventCustomUpstream
             | n == #{const GST_EVENT_CUSTOM_DOWNSTREAM} = EventCustomDownstream
             | n == #{const GST_EVENT_CUSTOM_DOWNSTREAM_OOB} = EventCustomDownstreamOOB
             | n == #{const GST_EVENT_CUSTOM_BOTH} = EventCustomBoth
             | n == #{const GST_EVENT_CUSTOM_BOTH_OOB} = EventCustomBothOOB

    fromEnum EventUnknown             = #{const GST_EVENT_UNKNOWN}
    fromEnum EventFlushStart          = #{const GST_EVENT_FLUSH_START}
    fromEnum EventFlushStop           = #{const GST_EVENT_FLUSH_STOP}
    fromEnum EventEOS                 = #{const GST_EVENT_EOS}
    fromEnum EventNewSegment          = #{const GST_EVENT_NEWSEGMENT}
    fromEnum EventTag                 = #{const GST_EVENT_TAG}
    fromEnum EventBufferSize          = #{const GST_EVENT_BUFFERSIZE}
    fromEnum EventQOS                 = #{const GST_EVENT_QOS}
    fromEnum EventSeek                = #{const GST_EVENT_SEEK}
    fromEnum EventNavigation          = #{const GST_EVENT_NAVIGATION}
#if GST_CHECK_VERSION(0, 10, 12)
    fromEnum EventLatency             = #{const GST_EVENT_LATENCY}
#endif
    fromEnum EventCustomUpstream      = #{const GST_EVENT_CUSTOM_UPSTREAM}
    fromEnum EventCustomDownstream    = #{const GST_EVENT_CUSTOM_DOWNSTREAM}
    fromEnum EventCustomDownstreamOOB = #{const GST_EVENT_CUSTOM_DOWNSTREAM_OOB}
    fromEnum EventCustomBoth          = #{const GST_EVENT_CUSTOM_BOTH}
    fromEnum EventCustomBothOOB       = #{const GST_EVENT_CUSTOM_BOTH_OOB}

-- | The messages types that may be sent by a pipeline.
data MessageType = MessageEOS             -- ^ end-of-stream
                 | MessageError           -- ^ an error message
                 | MessageWarning         -- ^ a warning message
                 | MessageInfo            -- ^ an informational message
                 | MessageTag             -- ^ a metadata tag
                 | MessageBuffering       -- ^ the pipeline is buffering
                 | MessageStateChanged    -- ^ the pipeline changed state
                 | MessageStepDone        -- ^ a framestep finished
                 | MessageClockProvide    -- ^ an element is able to provide a clock
                 | MessageClockLost       -- ^ the current clock has become unusable
                 | MessageNewClock        -- ^ a new clock was selected by the pipeline
                 | MessageStructureChange -- ^ the structure of the pipeline has changed
                 | MessageStreamStatus    -- ^ a stream status message
                 | MessageApplication     -- ^ a message posted by the application
                 | MessageElement         -- ^ an element specific message
                 | MessageSegmentStart    -- ^ the pipeline started playback of a segment
                 | MessageSegmentDone     -- ^ the pipeline finished playback of a segment
                 | MessageDuration        -- ^ the duration of the pipeline changed
#if GST_CHECK_VERSION(0, 10, 12)
                 | MessageLatency         -- ^ an element's latency has changed
#endif
#if GST_CHECK_VERSION(0, 10, 13)
                 | MessageAsyncStart      -- ^ an element has started an async state change; used internally
                 | MessageAsyncDone       -- ^ an element has completed an async state change; used internally
#endif
                   deriving (Eq, Bounded, Show)
instance Enum MessageType where
    toEnum n | n == #{const GST_MESSAGE_EOS}               = MessageEOS
             | n == #{const GST_MESSAGE_ERROR}             = MessageError
             | n == #{const GST_MESSAGE_WARNING}           = MessageWarning
             | n == #{const GST_MESSAGE_INFO}              = MessageInfo
             | n == #{const GST_MESSAGE_TAG}               = MessageTag
             | n == #{const GST_MESSAGE_BUFFERING}         = MessageBuffering
             | n == #{const GST_MESSAGE_STATE_CHANGED}     = MessageStateChanged
             | n == #{const GST_MESSAGE_STEP_DONE}         = MessageStepDone
             | n == #{const GST_MESSAGE_CLOCK_PROVIDE}     = MessageClockProvide
             | n == #{const GST_MESSAGE_CLOCK_LOST}        = MessageClockLost
             | n == #{const GST_MESSAGE_NEW_CLOCK}         = MessageNewClock
             | n == #{const GST_MESSAGE_STRUCTURE_CHANGE}  = MessageStructureChange
             | n == #{const GST_MESSAGE_STREAM_STATUS}     = MessageStreamStatus
             | n == #{const GST_MESSAGE_APPLICATION}       = MessageApplication
             | n == #{const GST_MESSAGE_ELEMENT}           = MessageElement
             | n == #{const GST_MESSAGE_SEGMENT_START}     = MessageSegmentStart
             | n == #{const GST_MESSAGE_SEGMENT_DONE}      = MessageSegmentDone
             | n == #{const GST_MESSAGE_DURATION}          = MessageDuration
#if GST_CHECK_VERSION(0, 10, 12)
             | n == #{const GST_MESSAGE_LATENCY}           = MessageLatency
#endif
#if GST_CHECK_VERSION(0, 10, 13)
             | n == #{const GST_MESSAGE_ASYNC_START}       = MessageAsyncStart
             | n == #{const GST_MESSAGE_ASYNC_DONE}        = MessageAsyncDone
#endif
    fromEnum MessageEOS             = #{const GST_MESSAGE_EOS}
    fromEnum MessageError           = #{const GST_MESSAGE_ERROR}
    fromEnum MessageWarning         = #{const GST_MESSAGE_WARNING}
    fromEnum MessageInfo            = #{const GST_MESSAGE_INFO}
    fromEnum MessageTag             = #{const GST_MESSAGE_TAG}
    fromEnum MessageBuffering       = #{const GST_MESSAGE_BUFFERING}
    fromEnum MessageStateChanged    = #{const GST_MESSAGE_STATE_CHANGED}
    fromEnum MessageStepDone        = #{const GST_MESSAGE_STEP_DONE}
    fromEnum MessageClockProvide    = #{const GST_MESSAGE_CLOCK_PROVIDE}
    fromEnum MessageClockLost       = #{const GST_MESSAGE_CLOCK_LOST}
    fromEnum MessageNewClock        = #{const GST_MESSAGE_NEW_CLOCK}
    fromEnum MessageStructureChange = #{const GST_MESSAGE_STRUCTURE_CHANGE}
    fromEnum MessageStreamStatus    = #{const GST_MESSAGE_STREAM_STATUS}
    fromEnum MessageApplication     = #{const GST_MESSAGE_APPLICATION}
    fromEnum MessageElement         = #{const GST_MESSAGE_ELEMENT}
    fromEnum MessageSegmentStart    = #{const GST_MESSAGE_SEGMENT_START}
    fromEnum MessageSegmentDone     = #{const GST_MESSAGE_SEGMENT_DONE}
    fromEnum MessageDuration        = #{const GST_MESSAGE_DURATION}
#if GST_CHECK_VERSION(0, 10, 12)
    fromEnum MessageLatency         = #{const GST_MESSAGE_LATENCY}
#endif
#if GST_CHECK_VERSION(0, 10, 13)
    fromEnum MessageAsyncStart      = #{const GST_MESSAGE_ASYNC_START}
    fromEnum MessageAsyncDone       = #{const GST_MESSAGE_ASYNC_DONE}
#endif
instance Flags MessageType

-- | The flags that a 'Caps' may have.
data CapsFlags = CapsAny
                 deriving (Eq, Bounded, Show)
instance Enum CapsFlags where
    toEnum n | n == #{const GST_CAPS_FLAGS_ANY} = CapsAny
    fromEnum CapsAny = #{const GST_CAPS_FLAGS_ANY}
instance Flags CapsFlags
