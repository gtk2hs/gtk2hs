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
module Media.Streaming.GStreamer.Core.Constants (
  
  ClockTime,
  
  clockTimeNone,
  second,
  msecond,
  usecond,
  nsecond,
  
  ObjectFlags(..),
  PadFlags(..),
  ElementFlags(..),
  StateChange(..),
  BusFlags(..),
  ClockFlags(..),
  IndexFlags(..),
  MiniObjectFlags(..),
  BufferFlags(..),
  bufferOffsetNone,
  EventType(..),
  MessageType(..),
  CapsFlags(..),
  
  ) where

import Data.Word
import System.Glib.Flags

type ClockTime = Word64

clockTimeNone :: ClockTime
clockTimeNone = #{const GST_CLOCK_TIME_NONE}

second, msecond, usecond, nsecond :: ClockTime
second  = #{const GST_SECOND}
msecond = #{const GST_MSECOND}
usecond = #{const GST_USECOND}
nsecond = #{const GST_NSECOND}

data ObjectFlags = ObjectDisposing
                   deriving (Bounded, Show)
instance Enum ObjectFlags where
    toEnum n | n == #{const GST_OBJECT_DISPOSING} = ObjectDisposing
    fromEnum ObjectDisposing = #{const GST_OBJECT_DISPOSING}
instance Flags ObjectFlags

data PadFlags = PadBlocked
              | PadFlushing
              | PadInGetCaps
              | PadInSetCaps
              | PadBlocking
                deriving (Eq, Bounded, Show)
instance Enum PadFlags where
    toEnum n | n == #{const GST_PAD_BLOCKED}    = PadBlocked
             | n == #{const GST_PAD_FLUSHING}   = PadFlushing
             | n == #{const GST_PAD_IN_GETCAPS} = PadInGetCaps
             | n == #{const GST_PAD_IN_SETCAPS} = PadInSetCaps
             | n == #{const GST_PAD_BLOCKING}   = PadBlocking
    fromEnum PadBlocked   = #{const GST_PAD_BLOCKED}
    fromEnum PadFlushing  = #{const GST_PAD_FLUSHING}
    fromEnum PadInGetCaps = #{const GST_PAD_IN_GETCAPS}
    fromEnum PadInSetCaps = #{const GST_PAD_IN_SETCAPS}
    fromEnum PadBlocking  = #{const GST_PAD_BLOCKING}
instance Flags PadFlags

data ElementFlags = ElementLockedState
                  | ElementIsSink
                  | ElementUnparenting
                    deriving (Eq, Bounded, Show)
instance Enum ElementFlags where
    toEnum n | n == #{const GST_ELEMENT_LOCKED_STATE} = ElementLockedState
             | n == #{const GST_ELEMENT_IS_SINK}      = ElementIsSink
             | n == #{const GST_ELEMENT_UNPARENTING}  = ElementUnparenting
    fromEnum ElementLockedState = #{const GST_ELEMENT_LOCKED_STATE}
    fromEnum ElementIsSink      = #{const GST_ELEMENT_IS_SINK}
    fromEnum ElementUnparenting = #{const GST_ELEMENT_UNPARENTING}
instance Flags ElementFlags

data StateChange = StateChangeNullToReady
                 | StateChangeReadyToPaused
                 | StateChangePausedToPlaying
                 | StateChangePlayingToPaused
                 | StateChangePausedToReady
                 | StateChangeReadyToNull
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

data BusFlags = BusFlushing
                deriving (Eq, Bounded, Show)
instance Enum BusFlags where
    toEnum n | n == #{const GST_BUS_FLUSHING} = BusFlushing
    fromEnum BusFlushing = #{const GST_BUS_FLUSHING}
instance Flags BusFlags

data ClockFlags = ClockCanDoSingleSync
                | ClockCanDoSingleAsync
                | ClockCanDoPeriodicSync
                | ClockCanDoPeriodicAsync
                | ClockCanSetResolution
                | ClockCanSetMaster
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

data IndexFlags = IndexWritable
                | IndexReadable
                  deriving (Eq, Bounded, Show)
instance Enum IndexFlags where
    toEnum n | n == #{const GST_INDEX_WRITABLE} = IndexWritable
             | n == #{const GST_INDEX_READABLE} = IndexReadable
    fromEnum IndexWritable = #{const GST_INDEX_WRITABLE}
    fromEnum IndexReadable = #{const GST_INDEX_READABLE}
instance Flags IndexFlags

data MiniObjectFlags = MiniObjectReadOnly
                       deriving (Eq, Bounded, Show)
instance Enum MiniObjectFlags where
    toEnum n | n == #{const GST_MINI_OBJECT_FLAG_READONLY} = MiniObjectReadOnly
    fromEnum MiniObjectReadOnly = #{const GST_MINI_OBJECT_FLAG_READONLY}
instance Flags MiniObjectFlags

data BufferFlags = BufferPreroll
                 | BufferDiscont
                 | BufferInCaps
                 | BufferGap
                 | BufferDeltaUnit
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

bufferOffsetNone :: Word64
bufferOffsetNone = #{const GST_BUFFER_OFFSET_NONE}

data EventType = EventUnknown
               | EventFlushStart
               | EventFlushStop
               | EventEOS
               | EventNewSegment
               | EventTag
               | EventBufferSize
               | EventQOS
               | EventSeek
               | EventNavigation
               | EventLatency
               | EventCustomUpstream
               | EventCustomDownstream
               | EventCustomDownstreamOOB
               | EventCustomBoth
               | EventCustomBothOOB
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
             | n == #{const GST_EVENT_LATENCY} = EventLatency
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
    fromEnum EventLatency             = #{const GST_EVENT_LATENCY}
    fromEnum EventCustomUpstream      = #{const GST_EVENT_CUSTOM_UPSTREAM}
    fromEnum EventCustomDownstream    = #{const GST_EVENT_CUSTOM_DOWNSTREAM}
    fromEnum EventCustomDownstreamOOB = #{const GST_EVENT_CUSTOM_DOWNSTREAM_OOB}
    fromEnum EventCustomBoth          = #{const GST_EVENT_CUSTOM_BOTH}
    fromEnum EventCustomBothOOB       = #{const GST_EVENT_CUSTOM_BOTH_OOB}

data MessageType = MessageEOS
                 | MessageError
                 | MessageWarning
                 | MessageInfo
                 | MessageTag
                 | MessageBuffering
                 | MessageStateChanged
                 | MessageStateDirty
                 | MessageStepDone
                 | MessageClockProvide
                 | MessageClockLost
                 | MessageNewClock
                 | MessageStructureChange
                 | MessageStreamStatus
                 | MessageApplication
                 | MessageElement
                 | MessageSegmentStart
                 | MessageSegmentDone
                 | MessageDuration
                 | MessageLatency
                 | MessageAsyncStart
                 | MessageAsyncDone
                   deriving (Eq, Bounded, Show)
instance Enum MessageType where
    toEnum n | n == #{const GST_MESSAGE_EOS}               = MessageEOS
             | n == #{const GST_MESSAGE_ERROR}             = MessageError
             | n == #{const GST_MESSAGE_WARNING}           = MessageWarning
             | n == #{const GST_MESSAGE_INFO}              = MessageInfo
             | n == #{const GST_MESSAGE_TAG}               = MessageTag
             | n == #{const GST_MESSAGE_BUFFERING}         = MessageBuffering
             | n == #{const GST_MESSAGE_STATE_CHANGED}     = MessageStateChanged
             | n == #{const GST_MESSAGE_STATE_DIRTY}       = MessageStateDirty
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
             | n == #{const GST_MESSAGE_LATENCY}           = MessageLatency
             | n == #{const GST_MESSAGE_ASYNC_START}       = MessageAsyncStart
             | n == #{const GST_MESSAGE_ASYNC_DONE}        = MessageAsyncDone
    fromEnum MessageEOS             = #{const GST_MESSAGE_EOS}
    fromEnum MessageError           = #{const GST_MESSAGE_ERROR}
    fromEnum MessageWarning         = #{const GST_MESSAGE_WARNING}
    fromEnum MessageInfo            = #{const GST_MESSAGE_INFO}
    fromEnum MessageTag             = #{const GST_MESSAGE_TAG}
    fromEnum MessageBuffering       = #{const GST_MESSAGE_BUFFERING}
    fromEnum MessageStateChanged    = #{const GST_MESSAGE_STATE_CHANGED}
    fromEnum MessageStateDirty      = #{const GST_MESSAGE_STATE_DIRTY}
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
    fromEnum MessageLatency         = #{const GST_MESSAGE_LATENCY}
    fromEnum MessageAsyncStart      = #{const GST_MESSAGE_ASYNC_START}
    fromEnum MessageAsyncDone       = #{const GST_MESSAGE_ASYNC_DONE}
instance Flags MessageType

data CapsFlags = CapsAny
                 deriving (Eq, Bounded, Show)
instance Enum CapsFlags where
    toEnum n | n == #{const GST_CAPS_FLAGS_ANY} = CapsAny
    fromEnum CapsAny = #{const GST_CAPS_FLAGS_ANY}
instance Flags CapsFlags
