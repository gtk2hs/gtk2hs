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
module Media.Streaming.GStreamer.Core.Message (
  
  Message,
  MessageClass,
  castToMessage,
  toMessage,
  
  MessageType(..),
  messageTypeGetName,
  messageTypeToQuark,
  
  messageSrc,
  messageTimestamp,
  messageType,
  messageTypeName,
  messageStructure,
  
  messageNewApplication,
  messageNewClockProvide,
  messageNewClockLost,
  messageNewCustom,
  messageNewElement,
  messageNewEOS,
  messageNewError,
#if GSTREAMER_CHECK_VERSION(0,10,12)
  messageNewInfo,
#endif
  messageNewNewClock,
  messageNewSegmentDone,
  messageNewSegmentStart,
  messageNewStateChanged,
  messageNewTag,
#if GSTREAMER_CHECK_VERSION(0,10,11)
  messageNewBuffering,
#endif
  messageNewWarning,
  messageNewDuration,
  messageNewStateDirty,
#if GSTREAMER_CHECK_VERSION(0,10,12)
  messageNewLatency,
#endif
  
  messageParseClockLost,
  messageParseClockProvide,
  messageParseError,
#if GSTREAMER_CHECK_VERSION(0,10,12)
  messageParseInfo,
#endif
  messageParseNewClock,
  messageParseSegmentDone,
  messageParseSegmentStart,
  messageParseStateChanged,
  messageParseTag,
#if GSTREAMER_CHECK_VERSION(0,10,11)
  messageParseBuffering,
#endif
  messageParseWarning,
  messageParseDuration, 
  
  ) where

import Control.Monad (liftM)
{#import Media.Streaming.GStreamer.Core.Types#}
import System.Glib.FFI
import System.Glib.GObject
import System.Glib.UTFString
import System.Glib.GError

{# context lib = "gstreamer" prefix = "gst" #}

messageTypeGetName :: MessageType
                   -> String
messageTypeGetName messageType =
    unsafePerformIO $
        {# call message_type_get_name #} (fromIntegral $ fromEnum messageType) >>=
            peekUTFString

messageTypeToQuark :: MessageType
                   -> Quark
messageTypeToQuark messageType =
    {# call fun message_type_to_quark #} (fromIntegral $ fromEnum messageType)

messageSrc :: Message
           -> Object
messageSrc message =
    unsafePerformIO $ withMiniObject message {# get GstMessage->src #} >>=
        peekObject

messageTimestamp :: Message
                 -> ClockTime
messageTimestamp message =
    fromIntegral $ unsafePerformIO $ withMiniObject message {# get GstMessage->timestamp #}

messageType :: Message
            -> MessageType
messageType message =
    toEnum $ fromIntegral $ unsafePerformIO $
        withMiniObject message cMessageGetMessageType
foreign import ccall unsafe "_hs_gst_message_get_message_type"
    cMessageGetMessageType :: Ptr Message
                           -> IO {# type GstMessageType #}

messageTypeName :: Message
                -> String
messageTypeName =
    messageTypeGetName . messageType

messageStructure :: Message
                 -> Structure
messageStructure message =
    unsafePerformIO $
        {# call message_get_structure #} message >>=
            peekStructure

messageNewApplication :: ObjectClass objectT
                      => objectT
                      -> Structure
                      -> Message
messageNewApplication src structure =
    unsafePerformIO $
        giveStructure structure
                      ({# call message_new_application #} (toObject src)) >>=
            takeMiniObject

messageNewClockProvide :: (ObjectClass objectT, ClockClass clockT)
                       => objectT
                       -> clockT
                       -> Bool
                       -> Message
messageNewClockProvide src clock ready =
    unsafePerformIO $
        {# call message_new_clock_provide #} (toObject src)
                                             (toClock clock)
                                             (fromBool ready) >>=
            takeMiniObject

messageNewClockLost :: (ObjectClass objectT, ClockClass clockT)
                    => objectT
                    -> clockT
                    -> Message
messageNewClockLost src clock =
    unsafePerformIO $
        {# call message_new_clock_lost #} (toObject src)
                                          (toClock clock) >>=
            takeMiniObject

messageNewCustom :: ObjectClass objectT
                 => MessageType
                 -> objectT
                 -> Maybe Structure
                 -> Message
messageNewCustom messageType src structure =
    unsafePerformIO $
        (case structure of
           Just structure' ->
               giveStructure structure' messageNewCustom'
           Nothing ->
               messageNewCustom' $ Structure nullForeignPtr) >>=
            takeMiniObject
    where
      messageNewCustom' =
          {# call message_new_custom #} (cFromEnum messageType)
                                        (toObject src)

messageNewElement :: ObjectClass objectT
                  => objectT
                  -> Maybe Structure
                  -> Message
messageNewElement src structure =
    unsafePerformIO $
        (case structure of
           Just structure' ->
               giveStructure structure' messageNewElement'
           Nothing ->
               messageNewElement' $ Structure nullForeignPtr) >>=
            takeMiniObject
    where
      messageNewElement' =
          {# call message_new_element #} (toObject src)

messageNewEOS :: ObjectClass objectT
              => objectT
              -> Message
messageNewEOS src =
    unsafePerformIO $
        {# call message_new_eos #} (toObject src) >>=
            takeMiniObject

messageNewError :: ObjectClass objectT
                => objectT
                -> GError
                -> String
                -> Message
messageNewError src error debug =
    unsafePerformIO $
       with error $ \gErrorPtr -> withUTFString debug $ \debugPtr ->
           {# call message_new_error #} (toObject src)
                                        (castPtr gErrorPtr)
                                        debugPtr >>=
               takeMiniObject

#if GSTREAMER_CHECK_VERSION(0,10,12)
messageNewInfo :: ObjectClass objectT
               => objectT
               -> GError
               -> String
               -> Message
messageNewInfo src error debug =
    unsafePerformIO $
       with error $ \gErrorPtr -> withUTFString debug $ \debugPtr ->
           {# call message_new_info #} (toObject src)
                                       (castPtr gErrorPtr)
                                       debugPtr >>=
               takeMiniObject
#endif

messageNewNewClock :: (ObjectClass objectT, ClockClass clockT)
                   => objectT
                   -> clockT
                   -> Message
messageNewNewClock src clock =
    unsafePerformIO $
        {# call message_new_new_clock #} (toObject src)
                                         (toClock clock) >>=
            takeMiniObject

messageNewSegmentDone :: ObjectClass objectT
                      => objectT
                      -> Format
                      -> Int64
                      -> Message
messageNewSegmentDone src format position =
    unsafePerformIO $
        {# call message_new_segment_done #} (toObject src)
                                            (fromIntegral $ fromFormat format)
                                            (fromIntegral position) >>=
            takeMiniObject

messageNewSegmentStart :: ObjectClass objectT
                       => objectT
                       -> Format
                       -> Int64
                       -> Message
messageNewSegmentStart src format position =
    unsafePerformIO $
        {# call message_new_segment_start #} (toObject src)
                                             (fromIntegral $ fromFormat format)
                                             (fromIntegral position) >>=
            takeMiniObject

messageNewStateChanged :: ObjectClass objectT
                       => objectT
                       -> State
                       -> State
                       -> State
                       -> Message
messageNewStateChanged src oldState newState pending =
    unsafePerformIO $
       {# call message_new_state_changed #} (toObject src)
                                            (cFromEnum oldState)
                                            (cFromEnum newState)
                                            (cFromEnum pending) >>=
           takeMiniObject

messageNewTag :: ObjectClass objectT
              => objectT
              -> TagList
              -> Message
messageNewTag src tagList =
    unsafePerformIO $ withTagList tagList $ \tagListPtr ->
        {# call message_new_tag #} (toObject src)
                                   (castPtr tagListPtr) >>=
            takeMiniObject

#if GSTREAMER_CHECK_VERSION(0,10,11)
messageNewBuffering :: ObjectClass objectT
                    => objectT
                    -> Int
                    -> Message
messageNewBuffering src percent =
    unsafePerformIO $
        {# call message_new_buffering #} (toObject src)
                                         (fromIntegral percent) >>=
            takeMiniObject
#endif

messageNewWarning :: ObjectClass objectT
                  => objectT
                  -> GError
                  -> String
                  -> Message
messageNewWarning src error debug =
    unsafePerformIO $
       with error $ \gErrorPtr -> withUTFString debug $ \debugPtr ->
           {# call message_new_warning #} (toObject src)
                                          (castPtr gErrorPtr)
                                          debugPtr >>=
               takeMiniObject

messageNewDuration :: ObjectClass objectT
                   => objectT
                   -> Format
                   -> Int64
                   -> Message
messageNewDuration src format duration =
    unsafePerformIO $
        {# call message_new_duration #} (toObject src)
                                        (fromIntegral $ fromFormat format)
                                        (fromIntegral duration) >>=
           takeMiniObject

messageNewStateDirty :: ObjectClass objectT
                     => objectT
                     -> Message
messageNewStateDirty src =
    unsafePerformIO $
        {# call message_new_state_dirty #} (toObject src) >>=
            takeMiniObject

#if GSTREAMER_CHECK_VERSION(0,10,12)
messageNewLatency :: ObjectClass objectT
                  => objectT
                  -> Message
messageNewLatency src =
    unsafePerformIO $
        {# call message_new_latency #} (toObject src) >>=
            takeMiniObject
#endif

messageParseClockLost :: Message
                      -> Maybe Clock
messageParseClockLost message | messageType message == MessageClockLost =
    Just $ unsafePerformIO $ alloca $ \clockPtr ->
        do {# call message_parse_clock_lost #} message $ castPtr clockPtr
           peek clockPtr >>= peekObject
                              | otherwise = Nothing

messageParseClockProvide :: Message
                         -> Maybe (Clock, Bool)
messageParseClockProvide message | messageType message == MessageClockProvide =
    Just $ unsafePerformIO $ alloca $ \clockPtr ->
        alloca $ \readyPtr ->
            do {# call message_parse_clock_provide #} message (castPtr clockPtr) readyPtr
               clock <- peek clockPtr >>= peekObject
               ready <- liftM toBool $ peek readyPtr
               return (clock, ready)
                                 | otherwise = Nothing

messageParseError :: Message
                  -> Maybe (GError, String)
messageParseError message | messageType message == MessageError =
    Just $ unsafePerformIO $ alloca $ \gErrorPtr ->
        alloca $ \debugPtr ->
            do {# call message_parse_error #} message (castPtr gErrorPtr) debugPtr
               gError <- do ptr <- peek gErrorPtr
                            gError <- peek ptr
                            {# call g_error_free #} $ castPtr ptr
                            return gError
               debug <- peek debugPtr >>= readUTFString
               return (gError, debug)
                          | otherwise = Nothing

#if GSTREAMER_CHECK_VERSION(0,10,12)
messageParseInfo :: Message
                 -> Maybe (GError, String)
messageParseInfo message | messageType message == MessageInfo =
    Just $ unsafePerformIO $ alloca $ \gErrorPtr ->
        alloca $ \debugPtr ->
            do {# call message_parse_info #} message (castPtr gErrorPtr) debugPtr
               gError <- do ptr <- peek gErrorPtr
                            gError <- peek ptr
                            {# call g_error_free #} $ castPtr ptr
                            return gError
               debug <- peek debugPtr >>= readUTFString
               return (gError, debug)
                         | otherwise = Nothing
#endif

messageParseNewClock :: Message
                     -> Maybe Clock
messageParseNewClock message | messageType message == MessageNewClock =
    Just $ unsafePerformIO $ alloca $ \clockPtr ->
        do {# call message_parse_clock_lost #} message $ castPtr clockPtr
           peek clockPtr >>= peekObject
                             | otherwise = Nothing

messageParseSegmentDone :: Message
                        -> Maybe (Format, Int64)
messageParseSegmentDone message | messageType message == MessageSegmentDone =
    Just $ unsafePerformIO $ alloca $ \formatPtr ->
        alloca $ \positionPtr ->
            do {# call message_parse_segment_done #} message formatPtr positionPtr
               format <- liftM (toFormat . fromIntegral) $ peek formatPtr
               position <- liftM fromIntegral $ peek positionPtr
               return (format, position)
                                | otherwise = Nothing

messageParseSegmentStart :: Message
                         -> Maybe (Format, Int64)
messageParseSegmentStart message | messageType message == MessageSegmentStart =
    Just $ unsafePerformIO $ alloca $ \formatPtr ->
        alloca $ \positionPtr ->
            do {# call message_parse_segment_start #} message formatPtr positionPtr
               format <- liftM (toFormat . fromIntegral) $ peek formatPtr
               position <- liftM fromIntegral $ peek positionPtr
               return (format, position)
                                 | otherwise = Nothing

messageParseStateChanged :: Message
                         -> Maybe (State, State, State)
messageParseStateChanged message | messageType message == MessageStateChanged =
    Just $ unsafePerformIO $ alloca $ \oldStatePtr ->
        alloca $ \newStatePtr -> alloca $ \pendingPtr ->
            do {# call message_parse_state_changed #} message oldStatePtr newStatePtr pendingPtr
               oldState <- liftM cToEnum $ peek oldStatePtr
               newState <- liftM cToEnum $ peek newStatePtr
               pending  <- liftM cToEnum $ peek pendingPtr
               return (oldState, newState, pending)
                                 | otherwise = Nothing

messageParseTag :: Message
                -> Maybe TagList
messageParseTag message | messageType message == MessageTag =
    Just $ unsafePerformIO $ alloca $ \tagListPtr ->
        do {# call message_parse_tag #} message $ castPtr tagListPtr
           peek tagListPtr >>= takeTagList
                        | otherwise = Nothing

#if GSTREAMER_CHECK_VERSION(0,10,11)
messageParseBuffering :: Message
                      -> Maybe Int
messageParseBuffering message | messageType message == MessageBuffering =
    Just $ fromIntegral $ unsafePerformIO $ alloca $ \percentPtr ->
        do {# call message_parse_buffering #} message percentPtr
           peek percentPtr
                              | otherwise = Nothing
#endif

messageParseWarning :: Message
                    -> Maybe (Maybe GError, Maybe String)
messageParseWarning message | messageType message == MessageWarning =
    Just $ unsafePerformIO $ alloca $ \gErrorPtr ->
        alloca $ \debugPtr ->
            do {# call message_parse_warning #} message (castPtr gErrorPtr) debugPtr
               gError <- peek gErrorPtr >>=
                             (maybePeek $ \ptr ->
                              do gError <- peek ptr
                                 {# call g_error_free #} $ castPtr ptr
                                 return gError)
               debug <- peek debugPtr >>= maybePeek readUTFString
               return (gError, debug)
                            | otherwise = Nothing

messageParseDuration :: Message
                     -> Maybe (Format, Int64)
messageParseDuration message | messageType message == MessageDuration =
    Just $ unsafePerformIO $ alloca $ \formatPtr ->
        alloca $ \positionPtr ->
            do {# call message_parse_duration #} message formatPtr positionPtr
               format <- liftM (toFormat . fromIntegral) $ peek formatPtr
               position <- liftM fromIntegral $ peek positionPtr
               return (format, position)
                             | otherwise = Nothing
