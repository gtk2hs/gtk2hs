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
module Media.Streaming.GStreamer.Core.Message (
  
  Message,
  MessageClass,
  castToMessage,
  toMessage,
  fromMessage,
  MessageType(..),
  messageTypeGetName,
  messageTypeToQuark,
  messageSrc,
  messageTimestamp,
  messageType,
  messageTypeName,
  messageStructure,
  messageNewApplication,
  messageParseClockLost,
  messageParseClockProvide,
  messageParseError,
  messageParseInfo,
  messageParseNewClock,
  messageParseSegmentDone,
  messageParseSegmentStart,
  messageParseStateChanged,
  messageParseTag,
  messageParseBuffering,
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
    unsafePerformIO $ withMessage message {# get GstMessage->src #} >>=
        peekObject

messageTimestamp :: Message
                 -> ClockTime
messageTimestamp message =
    fromIntegral $ unsafePerformIO $ withMessage message {# get GstMessage->timestamp #}

messageType :: Message
            -> MessageType
messageType message =
    toEnum $ fromIntegral $ unsafePerformIO $
        withMessage message cMessageGetMessageType
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
    unsafePerformIO $ {# call message_get_structure #} message >>= peekStructure

messageNewApplication :: Object
                      -> Structure
                      -> IO Message
messageNewApplication object structure =
    (giveStructure structure $ {# call message_new_application #} object) >>=
        takeMessage

messageParseClockLost :: Message
                      -> Maybe Clock
messageParseClockLost message =
    unsafePerformIO $ alloca $ \clockPtr ->
        do poke clockPtr nullPtr
           {# call message_parse_clock_lost #} message $ castPtr clockPtr
           liftM Just $ peek clockPtr >>= peekClock

messageParseClockProvide :: Message
                         -> Maybe (Clock, Bool)
messageParseClockProvide message =
    unsafePerformIO $ alloca $ \clockPtr ->
        alloca $ \readyPtr ->
            do poke clockPtr nullPtr
               poke readyPtr $ fromBool False
               {# call message_parse_clock_provide #} message (castPtr clockPtr) readyPtr
               clock <- peek clockPtr >>= maybePeek peekClock
               ready <- peek readyPtr
               return $ maybe Nothing (\clock -> Just (clock, toBool ready)) clock

messageParseError :: Message
                  -> (Maybe GError, Maybe String)
messageParseError message =
    unsafePerformIO $ alloca $ \gErrorPtr ->
        alloca $ \debugPtr ->
            do poke gErrorPtr nullPtr
               poke debugPtr nullPtr
               {# call message_parse_error #} message (castPtr gErrorPtr) debugPtr
               gError <- peek gErrorPtr >>=
                             (maybePeek $ \ptr ->
                              do gError <- peek ptr
                                 {# call g_error_free #} $ castPtr ptr
                                 return gError)
               debug <- peek debugPtr >>= maybePeek readUTFString
               return (gError, debug)

messageParseInfo :: Message
                 -> (Maybe GError, Maybe String)
messageParseInfo message =
    unsafePerformIO $ alloca $ \gErrorPtr ->
        alloca $ \debugPtr ->
            do poke gErrorPtr nullPtr
               poke debugPtr nullPtr
               {# call message_parse_info #} message (castPtr gErrorPtr) debugPtr
               gError <- peek gErrorPtr >>=
                             (maybePeek $ \ptr ->
                              do gError <- peek ptr
                                 {# call g_error_free #} $ castPtr ptr
                                 return gError)
               debug <- peek debugPtr >>= maybePeek readUTFString
               return (gError, debug)

messageParseNewClock :: Message
                     -> Maybe Clock
messageParseNewClock message =
    unsafePerformIO $ alloca $ \clockPtr ->
        do poke clockPtr nullPtr
           {# call message_parse_clock_lost #} message $ castPtr clockPtr
           peek clockPtr >>= maybePeek peekClock

messageParseSegmentDone :: Message
                        -> (Format, Int64)
messageParseSegmentDone message =
    unsafePerformIO $ alloca $ \formatPtr ->
        alloca $ \positionPtr ->
            do poke formatPtr $ fromIntegral $ fromEnum FormatUndefined
               poke positionPtr 0
               {# call message_parse_segment_done #} message formatPtr positionPtr
               format <- peek formatPtr
               position <- peek positionPtr
               return (toEnum $ fromIntegral format, fromIntegral position)

messageParseSegmentStart :: Message
                         -> (Format, Int64)
messageParseSegmentStart message =
    unsafePerformIO $ alloca $ \formatPtr ->
        alloca $ \positionPtr ->
            do poke formatPtr $ fromIntegral $ fromEnum FormatUndefined
               poke positionPtr 0
               {# call message_parse_segment_start #} message formatPtr positionPtr
               format <- peek formatPtr
               position <- peek positionPtr
               return (toEnum $ fromIntegral format, fromIntegral position)

messageParseStateChanged :: Message
                         -> (State, State, State)
messageParseStateChanged message =
    unsafePerformIO $ alloca $ \oldStatePtr ->
        alloca $ \newStatePtr -> alloca $ \pendingPtr ->
            do poke oldStatePtr 0
               poke newStatePtr 0
               poke pendingPtr  0
               {# call message_parse_state_changed #} message oldStatePtr newStatePtr pendingPtr
               oldState <- liftM (toEnum . fromIntegral) $ peek oldStatePtr
               newState <- liftM (toEnum . fromIntegral) $ peek newStatePtr
               pending  <- liftM (toEnum . fromIntegral) $ peek pendingPtr
               return (oldState, newState, pending)

messageParseTag :: Message
                -> TagList
messageParseTag message =
    unsafePerformIO $ alloca $ \tagListPtr ->
        do poke tagListPtr nullPtr
           {# call message_parse_tag #} message $ castPtr tagListPtr
           peek tagListPtr >>= takeTagList

messageParseBuffering :: Message
                      -> Int
messageParseBuffering message =
    fromIntegral $ unsafePerformIO $ alloca $ \percentPtr ->
        do poke percentPtr 0
           {# call message_parse_buffering #} message percentPtr
           peek percentPtr

messageParseWarning :: Message
                    -> (Maybe GError, Maybe String)
messageParseWarning message =
    unsafePerformIO $ alloca $ \gErrorPtr ->
        alloca $ \debugPtr ->
            do poke gErrorPtr nullPtr
               poke debugPtr nullPtr
               {# call message_parse_warning #} message (castPtr gErrorPtr) debugPtr
               gError <- peek gErrorPtr >>=
                             (maybePeek $ \ptr ->
                              do gError <- peek ptr
                                 {# call g_error_free #} $ castPtr ptr
                                 return gError)
               debug <- peek debugPtr >>= maybePeek readUTFString
               return (gError, debug)

messageParseDuration :: Message
                     -> (Format, Int64)
messageParseDuration message =
    unsafePerformIO $ alloca $ \formatPtr ->
        alloca $ \positionPtr ->
            do poke formatPtr $ fromIntegral $ fromEnum FormatUndefined
               poke positionPtr 0
               {# call message_parse_duration #} message formatPtr positionPtr
               format <- peek formatPtr
               position <- peek positionPtr
               return (toEnum $ fromIntegral format, fromIntegral position)
