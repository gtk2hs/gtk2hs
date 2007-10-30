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
module Media.Streaming.GStreamer.Core.Buffer (
  
  Buffer,
  BufferClass,
  BufferFlags(..),
  toBuffer,
  castToBuffer,
  isBuffer,
  bufferOffsetNone,
  bufferGetFlags,
  bufferGetFlagsM,
  bufferSetFlagsM,
  bufferUnsetFlagsM,
  bufferGetSize,
  bufferGetSizeM,
  bufferGetData,
  bufferGetDataM,
  bufferSetDataM,
  bufferGetTimestamp,
  bufferGetTimestampM,
  bufferSetTimestampM,
  bufferGetDuration,
  bufferGetDurationM,
  bufferSetDurationM,
  bufferGetCaps,
  bufferGetCapsM,
  bufferSetCapsM,
  bufferGetOffset,
  bufferGetOffsetM,
  bufferSetOffsetM,
  bufferGetOffsetEnd,
  bufferGetOffsetEndM,
  bufferSetOffsetEndM,
  bufferDurationIsValid,
  bufferDurationIsValidM,
  bufferTimestampIsValid,
  bufferTimestampIsValidM,
  bufferOffsetIsValid,
  bufferOffsetIsValidM,
  bufferOffsetEndIsValid,
  bufferOffsetEndIsValidM,
  bufferIsDiscont,
  bufferIsDiscontM,
  
  bufferCreateEmpty,
  bufferCreate,
  bufferCreateSub,
  
  bufferIsSpanFast,
  bufferSpan,
  bufferStampM,
  bufferMerge
  
  ) where

import Control.Monad ( liftM
                     , when )
import Control.Monad.Trans
import qualified Data.ByteString as BS
{#import Media.Streaming.GStreamer.Core.Types#}
import System.Glib.FFI

#if __GLASGOW_HASKELL__ >= 606 && __GLASGOW_HASKELL__ < 608
#define OLD_BYTESTRING
#endif

{# context lib = "gstreamer" prefix = "gst" #}


bufferGetFlags :: BufferClass bufferT
               => bufferT
               -> [BufferFlags]
bufferGetFlags = mkMiniObjectGetFlags

bufferGetFlagsM :: BufferClass bufferT
                => MiniObjectM bufferT [BufferFlags]
bufferGetFlagsM = mkMiniObjectGetFlagsM

bufferSetFlagsM :: BufferClass bufferT
                => [BufferFlags]
                -> MiniObjectM bufferT () 
bufferSetFlagsM = mkMiniObjectSetFlagsM

bufferUnsetFlagsM :: BufferClass bufferT
                  => [BufferFlags]
                  -> MiniObjectM bufferT ()
bufferUnsetFlagsM = mkMiniObjectUnsetFlagsM

bufferGetSize :: BufferClass bufferT
              => bufferT
              -> Word
bufferGetSize buffer =
    fromIntegral $ unsafePerformIO $
        withMiniObject buffer {# get GstBuffer->size #}

marshalBufferM :: BufferClass bufferT
               => (Ptr Buffer -> IO a)
               -> MiniObjectM bufferT a
marshalBufferM action =
    MiniObjectM $ flip (withMiniObject . toBuffer) $ action

bufferGetSizeM :: BufferClass bufferT
               => MiniObjectM bufferT Word
bufferGetSizeM =
    liftM fromIntegral $
        marshalBufferM {# get GstBuffer->size #}

bufferGetData :: BufferClass bufferT
              => bufferT
              -> BS.ByteString
bufferGetData buffer =
    unsafePerformIO $ withMiniObject buffer $ \bufferPtr ->
        do ptr <- {# get GstBuffer->data #} bufferPtr
           size <- {# get GstBuffer->size #} bufferPtr
#ifdef OLD_BYTESTRING
           BS.copyCStringLen (castPtr ptr, fromIntegral size)
#else
           BS.packCStringLen (castPtr ptr, fromIntegral size)
#endif

bufferGetDataM :: BufferClass bufferT
               => MiniObjectM bufferT BS.ByteString
bufferGetDataM =
    marshalBufferM $ \bufferPtr ->
        do ptr <- {# get GstBuffer->data #} bufferPtr
           size <- {# get GstBuffer->size #} bufferPtr
#ifdef OLD_BYTESTRING
           BS.copyCStringLen (castPtr ptr, fromIntegral size)
#else
           BS.packCStringLen (castPtr ptr, fromIntegral size)
#endif

bufferSetDataM :: BufferClass bufferT
               => BS.ByteString
               -> MiniObjectM bufferT ()
bufferSetDataM bs =
    marshalBufferM $ \bufferPtr ->
        BS.useAsCStringLen bs $ \(origData, size) ->
            do mallocData <- {# get GstBuffer->malloc_data #} bufferPtr
               when (mallocData /= nullPtr) $
                   {# call g_free #} $ castPtr mallocData
               newData <- liftM castPtr $ {# call g_malloc #} $ fromIntegral size
               copyBytes (castPtr newData) origData size
               {# set GstBuffer->data #} bufferPtr newData
               {# set GstBuffer->malloc_data #} bufferPtr newData
               {# set GstBuffer->size #} bufferPtr $ fromIntegral size

bufferGetTimestamp :: BufferClass bufferT
                   => bufferT
                   -> ClockTime
bufferGetTimestamp buffer =
    fromIntegral $ unsafePerformIO $
        withMiniObject buffer {# get GstBuffer->timestamp #}

bufferGetTimestampM :: BufferClass bufferT
                    => MiniObjectM bufferT ClockTime
bufferGetTimestampM =
    liftM fromIntegral $
        marshalBufferM {# get GstBuffer->timestamp #}

bufferSetTimestampM :: BufferClass bufferT
                    => ClockTime
                    -> MiniObjectM bufferT ()
bufferSetTimestampM timestamp =
    marshalBufferM $ flip {# set GstBuffer->timestamp #} $ fromIntegral timestamp

bufferGetDuration :: BufferClass bufferT
                  => bufferT
                  -> ClockTime
bufferGetDuration buffer =
    fromIntegral $ unsafePerformIO $
        withMiniObject buffer {# get GstBuffer->duration #}

bufferGetDurationM :: BufferClass bufferT
                   => MiniObjectM bufferT ClockTime
bufferGetDurationM =
    liftM fromIntegral $
        marshalBufferM {# get GstBuffer->duration #}

bufferSetDurationM :: BufferClass bufferT
                   => ClockTime
                   -> MiniObjectM bufferT ()
bufferSetDurationM duration =
    marshalBufferM $
        flip {# set GstBuffer->duration #} $ fromIntegral duration

bufferGetCaps :: BufferClass bufferT
              => bufferT
              -> Maybe Caps
bufferGetCaps buffer =
    unsafePerformIO $
        {# call buffer_get_caps #} (toBuffer buffer) >>=
            maybePeek takeCaps

bufferGetCapsM :: BufferClass bufferT
               => MiniObjectM bufferT (Maybe Caps)
bufferGetCapsM =
    MiniObjectM $ \buffer ->
        {# call buffer_get_caps #} (toBuffer buffer) >>=
            maybePeek takeCaps

bufferSetCapsM :: BufferClass bufferT
               => Caps
               -> MiniObjectM bufferT ()
bufferSetCapsM caps =
    MiniObjectM $ \buffer ->
        {# call buffer_set_caps #} (toBuffer buffer) caps

bufferGetOffset :: BufferClass bufferT
                => bufferT
                -> Word64
bufferGetOffset buffer =
    fromIntegral $ unsafePerformIO $
        withMiniObject buffer {# get GstBuffer->offset #}

bufferGetOffsetM :: BufferClass bufferT
                 => MiniObjectM bufferT Word64
bufferGetOffsetM =
    liftM fromIntegral $
        marshalBufferM {# get GstBuffer->offset #}

bufferSetOffsetM :: BufferClass bufferT
                 => Word64
                 -> MiniObjectM bufferT ()
bufferSetOffsetM offset =
    marshalBufferM $
        flip {# set GstBuffer->offset #} $ fromIntegral offset

bufferGetOffsetEnd :: BufferClass bufferT
                   => bufferT
                   -> Word64
bufferGetOffsetEnd buffer =
    fromIntegral $ unsafePerformIO $
        withMiniObject buffer {# get GstBuffer->offset_end #}

bufferGetOffsetEndM :: BufferClass bufferT
                    => MiniObjectM bufferT Word64
bufferGetOffsetEndM =
    liftM fromIntegral $
        marshalBufferM {# get GstBuffer->offset_end #}

bufferSetOffsetEndM :: BufferClass bufferT
                    => Word64
                    -> MiniObjectM bufferT ()
bufferSetOffsetEndM offsetEnd =
    marshalBufferM $
        flip {# set GstBuffer->offset_end #} $ fromIntegral offsetEnd

bufferDurationIsValid :: BufferClass bufferT
                      => bufferT
                      -> Bool
bufferDurationIsValid =
    (/= clockTimeNone) . bufferGetOffset

bufferDurationIsValidM :: BufferClass bufferT
                       => MiniObjectM bufferT Bool
bufferDurationIsValidM =
    liftM (/= clockTimeNone) $ bufferGetDurationM

bufferTimestampIsValid :: BufferClass bufferT
                       => bufferT
                       -> Bool
bufferTimestampIsValid =
    (/= clockTimeNone) . bufferGetTimestamp

bufferTimestampIsValidM :: BufferClass bufferT
                        => MiniObjectM bufferT Bool
bufferTimestampIsValidM =
    liftM (/= clockTimeNone) $ bufferGetTimestampM

bufferOffsetIsValid :: BufferClass bufferT
                    => bufferT
                    -> Bool
bufferOffsetIsValid =
    (/= bufferOffsetNone) . bufferGetOffset

bufferOffsetIsValidM :: BufferClass bufferT
                     => MiniObjectM bufferT Bool
bufferOffsetIsValidM =
    liftM (/= bufferOffsetNone) $ bufferGetOffsetM

bufferOffsetEndIsValid :: BufferClass bufferT
                       => bufferT
                       -> Bool
bufferOffsetEndIsValid =
    (/= bufferOffsetNone) . bufferGetOffsetEnd

bufferOffsetEndIsValidM :: BufferClass bufferT
                        => MiniObjectM bufferT Bool
bufferOffsetEndIsValidM =
    liftM (/= bufferOffsetNone) $ bufferGetOffsetEndM

bufferIsDiscont :: BufferClass bufferT
                => bufferT
                -> Bool
bufferIsDiscont =
    (elem BufferDiscont) . bufferGetFlags

bufferIsDiscontM :: BufferClass bufferT
                 => MiniObjectM bufferT Bool
bufferIsDiscontM =
    liftM (elem BufferDiscont) $ bufferGetFlagsM

bufferCreateEmpty :: MiniObjectM Buffer a
                  -> IO (Buffer, a)
bufferCreateEmpty =
    marshalMiniObjectModify {# call buffer_new #}

bufferCreate :: Word
             -> MiniObjectM Buffer a
             -> IO (Buffer, a)
bufferCreate size =
    marshalMiniObjectModify $
        {# call buffer_new_and_alloc #} $ fromIntegral size

bufferCreateSub :: BufferClass bufferT
                => bufferT
                -> Word
                -> Word
                -> MiniObjectM Buffer a
                -> IO (Buffer, a)
bufferCreateSub parent offset size =
    marshalMiniObjectModify $
        {# call buffer_create_sub #} (toBuffer parent)
                                     (fromIntegral offset)
                                     (fromIntegral size)

bufferIsSpanFast :: (BufferClass bufferT1, BufferClass bufferT2)
                 => bufferT1
                 -> bufferT2
                 -> Bool
bufferIsSpanFast buffer1 buffer2 =
    toBool $ unsafePerformIO $
        {# call buffer_is_span_fast #} (toBuffer buffer1)
                                       (toBuffer buffer2)

bufferSpan :: (BufferClass bufferT1, BufferClass bufferT2)
           => bufferT1
           -> Word32
           -> bufferT2
           -> Word32
           -> Maybe Buffer
bufferSpan buffer1 offset buffer2 len =
    unsafePerformIO $
        {# call buffer_span #} (toBuffer buffer1)
                               (fromIntegral offset)
                               (toBuffer buffer2)
                               (fromIntegral len) >>=
            maybePeek takeMiniObject

bufferStampM :: (BufferClass srcT, BufferClass destT)
             => srcT
             -> MiniObjectM destT ()
bufferStampM src =
    MiniObjectM $ \dest ->
        {# call buffer_stamp #} (toBuffer dest)
                                (toBuffer src)

bufferMerge :: (BufferClass bufferT1, BufferClass bufferT2)
            => bufferT1
            -> bufferT2
            -> Buffer
bufferMerge buffer1 buffer2 =
    unsafePerformIO $
        {# call buffer_merge #} (toBuffer buffer1)
                                (toBuffer buffer2) >>=
            takeMiniObject
