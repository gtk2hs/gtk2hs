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
--
--   Data-passing buffer type, supporting sub-buffers.
module Media.Streaming.GStreamer.Core.Buffer (
  
-- * Types

  -- | Buffers are the basic unit of data transfer in GStreamer. The
  --   'Buffer' type provides all the state necessary to define a
  --   region of memory as part of a stream. Sub-buffers are also
  --   supported, allowing a smaller region of a buffer to become its
  --   own buffer, with mechansims in place to ensure that neither
  --   memory space goes away prematurely.
  Buffer,
  BufferClass,
  BufferFlags(..),
  -- Safely downcast an 'Object' to a 'Bin'.
  castToBuffer,
  -- Upcast to a 'Bin'.
  toBuffer,
  -- See if an 'Object' is a 'Bin'.
  isBuffer,

-- * Buffer Operations
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
  unsafeBufferGetPtrM,
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
  bufferIsDiscont,
  bufferIsDiscontM,
  
  bufferCreateEmpty,
  bufferCreate,
  bufferCreateSub,
  
  bufferIsSpanFast,
  bufferSpan,
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

-- | Get the flags set on this buffer.
bufferGetFlags :: BufferClass bufferT
               => bufferT
               -> [BufferFlags]
bufferGetFlags = mkMiniObjectGetFlags

-- | Get the flags set on the current buffer.
bufferGetFlagsM :: (BufferClass bufferT, MonadIO m)
                => MiniObjectT bufferT m [BufferFlags]
bufferGetFlagsM = mkMiniObjectGetFlagsM

-- | Set flags on the current buffer.
bufferSetFlagsM :: (BufferClass bufferT, MonadIO m)
                => [BufferFlags]
                -> MiniObjectT bufferT m ()
bufferSetFlagsM = mkMiniObjectSetFlagsM

-- | Unset flags on the current buffer.
bufferUnsetFlagsM :: (BufferClass bufferT, MonadIO m)
                  => [BufferFlags]
                  -> MiniObjectT bufferT m ()
bufferUnsetFlagsM = mkMiniObjectUnsetFlagsM

-- | Get the size of the buffer.
bufferGetSize :: BufferClass bufferT
              => bufferT
              -> Word
bufferGetSize buffer =
    fromIntegral $ unsafePerformIO $
        withMiniObject buffer {# get GstBuffer->size #}

marshalBufferM :: (BufferClass bufferT, MonadIO m)
               => (Ptr Buffer -> IO a)
               -> MiniObjectT bufferT m a
marshalBufferM action = do
  buffer <- askMiniObject
  liftIO $ withMiniObject (toBuffer buffer) action

-- | Get the size of the current buffer.
bufferGetSizeM :: (BufferClass bufferT, MonadIO m)
               => MiniObjectT bufferT m Word
bufferGetSizeM =
    liftM fromIntegral $
        marshalBufferM {# get GstBuffer->size #}

-- | Make an O(n) copy of the buffer.
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

-- | Make an O(n) copy of the current buffer.
bufferGetDataM :: (BufferClass bufferT, MonadIO m)
               => MiniObjectT bufferT m BS.ByteString
bufferGetDataM =
    marshalBufferM $ \bufferPtr ->
        do ptr <- {# get GstBuffer->data #} bufferPtr
           size <- {# get GstBuffer->size #} bufferPtr
#ifdef OLD_BYTESTRING
           BS.copyCStringLen (castPtr ptr, fromIntegral size)
#else
           BS.packCStringLen (castPtr ptr, fromIntegral size)
#endif

-- | Store an O(n) copy of the provided data in the current buffer.
bufferSetDataM :: (BufferClass bufferT, MonadIO m)
               => BS.ByteString
               -> MiniObjectT bufferT m ()
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

-- | Get a raw pointer to the internal data area for the current
--   buffer. The pointer may be used to write into the data area if
--   desired. This function is unsafe in that the pointer should not
--   be used once the buffer is returned.
unsafeBufferGetPtrM :: (BufferClass bufferT, MonadIO m)
                    => MiniObjectT bufferT m (Ptr Word8)
unsafeBufferGetPtrM = do
  buffer <- askMiniObject
  liftIO $ liftM castPtr $
      withMiniObject buffer $
          {# get GstBuffer->data #} . castPtr

marshalGetNum :: (BufferClass bufferT, Integral intT, Num numT)
              => (Ptr Buffer -> IO intT)
              -> numT
              -> bufferT
              -> Maybe numT
marshalGetNum getAction invalid buffer =
    let n = fromIntegral $ unsafePerformIO $
                withMiniObject (toBuffer buffer) getAction
    in if n /= invalid
          then Just n
          else Nothing

marshalGetNumM :: (BufferClass bufferT, Integral intT, Num numT, MonadIO m)
              => (Ptr Buffer -> IO intT)
              -> numT
              -> MiniObjectT bufferT m (Maybe numT)
marshalGetNumM getAction invalid =
    marshalBufferM $ \bufferPtr -> do
      n <- liftM fromIntegral $ getAction bufferPtr
      return $ if n /= invalid
                  then Just n
                  else Nothing

marshalSetNumM :: (BufferClass bufferT, Integral intT, Num numT, MonadIO m)
               => (Ptr Buffer -> numT -> IO ())
               -> intT
               -> Maybe intT
               -> MiniObjectT bufferT m ()
marshalSetNumM setAction invalid nM =
    let n = case nM of
              Just n' -> n'
              Nothing -> invalid
    in marshalBufferM $ flip setAction $ fromIntegral n

-- | Get the timestamp for this buffer.
bufferGetTimestamp :: BufferClass bufferT
                   => bufferT
                   -> Maybe ClockTime
bufferGetTimestamp =
    marshalGetNum {# get GstBuffer->timestamp #} clockTimeNone

-- | Get the timestamp of the current buffer.
bufferGetTimestampM :: (BufferClass bufferT, MonadIO m)
                    => MiniObjectT bufferT m (Maybe ClockTime)
bufferGetTimestampM =
    marshalGetNumM {# get GstBuffer->timestamp #} clockTimeNone

-- | Set the timestamp of the current buffer.
bufferSetTimestampM :: (BufferClass bufferT, MonadIO m)
                    => Maybe ClockTime
                    -> MiniObjectT bufferT m ()
bufferSetTimestampM =
    marshalSetNumM {# set GstBuffer->timestamp #} clockTimeNone

-- | Get the duration of the buffer.
bufferGetDuration :: BufferClass bufferT
                  => bufferT
                  -> Maybe ClockTime
bufferGetDuration =
    marshalGetNum {# get GstBuffer->duration #} clockTimeNone

-- | Get the duration of the current buffer.
bufferGetDurationM :: (BufferClass bufferT, MonadIO m)
                   => MiniObjectT bufferT m (Maybe ClockTime)
bufferGetDurationM =
    marshalGetNumM {# get GstBuffer->duration #} clockTimeNone

-- | Set the duration of the current buffer.
bufferSetDurationM :: (BufferClass bufferT, MonadIO m)
                   => Maybe ClockTime
                   -> MiniObjectT bufferT m ()
bufferSetDurationM =
    marshalSetNumM {# set GstBuffer->duration #} clockTimeNone

-- | Get the caps of the buffer.
bufferGetCaps :: BufferClass bufferT
              => bufferT
              -> Maybe Caps
bufferGetCaps buffer =
    unsafePerformIO $
        {# call buffer_get_caps #} (toBuffer buffer) >>=
            maybePeek takeCaps

-- | Get the caps of the current buffer.
bufferGetCapsM :: (BufferClass bufferT, MonadIO m)
               => MiniObjectT bufferT m (Maybe Caps)
bufferGetCapsM = do
  buffer <- askMiniObject
  liftIO $ {# call buffer_get_caps #} (toBuffer buffer) >>=
               maybePeek takeCaps

-- | Set the caps of the current buffer.
bufferSetCapsM :: (BufferClass bufferT, MonadIO m)
               => Maybe Caps
               -> MiniObjectT bufferT m ()
bufferSetCapsM capsM = do
  buffer <- askMiniObject
  liftIO $ {# call buffer_set_caps #} (toBuffer buffer) $
               case capsM of
                 Just caps -> caps
                 Nothing   -> Caps nullForeignPtr

-- | Get the offset of the buffer.
bufferGetOffset :: BufferClass bufferT
                => bufferT
                -> Maybe Word64
bufferGetOffset =
    marshalGetNum {# get GstBuffer->offset #} bufferOffsetNone

-- | Get the start offset of the current buffer.
bufferGetOffsetM :: (BufferClass bufferT, MonadIO m)
                 => MiniObjectT bufferT m (Maybe Word64)
bufferGetOffsetM =
    marshalGetNumM {# get GstBuffer->offset #} bufferOffsetNone

-- | Set the start offset of the current buffer.
bufferSetOffsetM :: (BufferClass bufferT, MonadIO m)
                 => Maybe Word64
                 -> MiniObjectT bufferT m ()
bufferSetOffsetM =
    marshalSetNumM {# set GstBuffer->offset #} bufferOffsetNone

-- | Get the end offset of the buffer.
bufferGetOffsetEnd :: BufferClass bufferT
                   => bufferT
                   -> Maybe Word64
bufferGetOffsetEnd =
    marshalGetNum {# get GstBuffer->offset_end #} bufferOffsetNone

-- | Get the end offset of the current buffer.
bufferGetOffsetEndM :: (BufferClass bufferT, MonadIO m)
                    => MiniObjectT bufferT m (Maybe Word64)
bufferGetOffsetEndM =
    marshalGetNumM {# get GstBuffer->offset_end #} bufferOffsetNone

-- | Set the end offset of the current buffer.
bufferSetOffsetEndM :: (BufferClass bufferT, MonadIO m)
                    => Maybe Word64
                    -> MiniObjectT bufferT m ()
bufferSetOffsetEndM =
    marshalSetNumM {# set GstBuffer->offset_end #} bufferOffsetNone

-- | Return 'True' if the buffer marks a discontinuity in a stream, or
--   'False' otherwise. This typically occurs after a seek or a
--   dropped buffer from a live or network source.
bufferIsDiscont :: BufferClass bufferT
                => bufferT
                -> Bool
bufferIsDiscont =
    (elem BufferDiscont) . bufferGetFlags

-- | Return 'True' if the current buffer marks a discontinuity in a
--   stream, or 'False' otherwise.
bufferIsDiscontM :: (BufferClass bufferT, MonadIO m)
                 => MiniObjectT bufferT m Bool
bufferIsDiscontM =
    liftM (elem BufferDiscont) $ bufferGetFlagsM

-- | Create an empty buffer and mutates it according to the given
--   action. Once this function returns, the buffer is immutable.
bufferCreateEmpty :: MonadIO m
                  => MiniObjectT Buffer m a -- ^ the mutating action
                  -> m (Buffer, a)          -- ^ the new buffer and the action's result
bufferCreateEmpty =
    marshalMiniObjectModify $ liftIO {# call buffer_new #}

-- | Create and mutate a buffer of the given size.
bufferCreate :: MonadIO m
             => Word                   -- ^ the size of the buffer to be created
             -> MiniObjectT Buffer m a -- ^ the mutating action
             -> m (Buffer, a)          -- ^ the new buffer and the action's result
bufferCreate size =
    marshalMiniObjectModify $
        liftIO $ {# call buffer_new_and_alloc #} $ fromIntegral size

-- | Create a sub-buffer from an existing buffer with the given offset
--   and size. This sub-buffer uses the actual memory space of the
--   parent buffer. Thus function will copy the offset and timestamp
--   fields when the offset is 0. Otherwise, they will both be set to
--   'Nothing'. If the offset is 0 and the size is the total size of
--   the parent, the duration and offset end fields are also
--   copied. Otherwise they will be set to 'Nothing'.
bufferCreateSub :: BufferClass bufferT
                => bufferT           -- ^ the parent buffer
                -> Word              -- ^ the offset
                -> Word              -- ^ the size
                -> IO (Maybe Buffer) -- ^ the new sub-buffer
bufferCreateSub parent offset size =
    {# call buffer_create_sub #} (toBuffer parent)
                                 (fromIntegral offset)
                                 (fromIntegral size) >>=
        maybePeek takeMiniObject

-- | Return 'True' if 'bufferSpan' can be done without copying the
--   data, or 'False' otherwise.
bufferIsSpanFast :: (BufferClass bufferT1, BufferClass bufferT2)
                 => bufferT1
                 -> bufferT2
                 -> Bool
bufferIsSpanFast buffer1 buffer2 =
    toBool $ unsafePerformIO $
        {# call buffer_is_span_fast #} (toBuffer buffer1)
                                       (toBuffer buffer2)

-- | Create a new buffer that consists of a span across the given
--   buffers. Logically, the buffers are concatenated to make a larger
--   buffer, and a new buffer is created at the given offset and with
--   the given size.
--   
--   If the two buffers are children of the same larger buffer, and
--   are contiguous, no copying is necessary. You can use
--   'bufferIsSpanFast' to determine if copying is needed.
bufferSpan :: (BufferClass bufferT1, BufferClass bufferT2)
           => bufferT1     -- ^ the first buffer
           -> Word32       -- ^ the offset into the concatenated buffer
           -> bufferT2     -- ^ the second buffer
           -> Word32       -- ^ the length of the final buffer
           -> Maybe Buffer -- ^ the spanning buffer, or 'Nothing' if
                           --   the arguments are invalid
bufferSpan buffer1 offset buffer2 len =
    unsafePerformIO $
        {# call buffer_span #} (toBuffer buffer1)
                               (fromIntegral offset)
                               (toBuffer buffer2)
                               (fromIntegral len) >>=
            maybePeek takeMiniObject

-- | Concatenate two buffers. If the buffers point to contiguous memory
--   areas, no copying will occur.
bufferMerge :: (BufferClass bufferT1, BufferClass bufferT2)
            => bufferT1 -- ^ a buffer
            -> bufferT2 -- ^ a buffer
            -> Buffer   -- ^ the concatenation of the buffers
bufferMerge buffer1 buffer2 =
    unsafePerformIO $
        {# call buffer_merge #} (toBuffer buffer1)
                                (toBuffer buffer2) >>=
            takeMiniObject
