{-# LANGUAGE CPP #-}
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
--  Data-passing buffer type, supporting sub-buffers.
module Media.Streaming.GStreamer.Core.Buffer (
  
-- * Types

  -- | 'Buffer's are the basic unit of data transfer in GStreamer. The
  --   'Buffer' type provides all the state necessary to define a
  --   region of memory as part of a stream. Sub-buffers are also
  --   supported, allowing a smaller region of a 'Buffer' to become its
  --   own 'Buffer', with mechansims in place to ensure that neither
  --   memory space goes away prematurely.
  Buffer,
  BufferClass,
  castToBuffer,
  gTypeBuffer,

  BufferFlags(..),

-- * Buffer Operations
  bufferOffsetNone,
  bufferGetFlags,
  bufferGetFlagsM,
  bufferSetFlagsM,
  bufferUnsetFlagsM,
  bufferGetSize,
  bufferGetSizeM,
#if __GLASGOW_HASKELL__ >= 606
  bufferGetData,
  bufferGetDataM,
  bufferSetDataM,
#endif
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
#if __GLASGOW_HASKELL__ >= 606
import qualified Data.ByteString as BS
#if __GLASGOW_HASKELL__ < 608
#define OLD_BYTESTRING
#endif
#endif

{#import Media.Streaming.GStreamer.Core.Types#}
import System.Glib.FFI

{# context lib = "gstreamer" prefix = "gst" #}

-- | Get the flags set on @buffer@.
bufferGetFlags :: BufferClass bufferT
               => bufferT       -- ^ @buffer@ - a 'Buffer'
               -> [BufferFlags] -- ^ the flags set on @buffer@
bufferGetFlags = mkMiniObjectGetFlags

-- | Get the flags set on the current 'Buffer'.
bufferGetFlagsM :: (BufferClass bufferT, MonadIO m)
                => MiniObjectT bufferT m [BufferFlags] -- ^ the flags set on the current 'Buffer'
bufferGetFlagsM = mkMiniObjectGetFlagsM

-- | Set flags on the current 'Buffer'.
bufferSetFlagsM :: (BufferClass bufferT, MonadIO m)
                => [BufferFlags]            -- ^ @flags@ - the flags to set on the current 'Buffer'
                -> MiniObjectT bufferT m ()
bufferSetFlagsM = mkMiniObjectSetFlagsM

-- | Unset flags on the current 'Buffer'.
bufferUnsetFlagsM :: (BufferClass bufferT, MonadIO m)
                  => [BufferFlags]                    -- ^ @flags@ - the flags to unset on the current 'Buffer'
                  -> MiniObjectT bufferT m ()
bufferUnsetFlagsM = mkMiniObjectUnsetFlagsM

-- | Get @buffer@'s size in bytes.
bufferGetSize :: BufferClass bufferT
              => bufferT -- ^ @buffer@ - a 'Buffer'
              -> Word    -- ^ the size of @buffer@ in bytes
bufferGetSize buffer =
    fromIntegral $ unsafePerformIO $
        withMiniObject buffer {# get GstBuffer->size #}

marshalBufferM :: (BufferClass bufferT, MonadIO m)
               => (Ptr Buffer -> IO a)
               -> MiniObjectT bufferT m a
marshalBufferM action = do
  ptr <- askMiniObjectPtr
  liftIO $ action $ castPtr ptr

-- | Get the size of the current 'Buffer' in bytes.
bufferGetSizeM :: (BufferClass bufferT, MonadIO m)
               => MiniObjectT bufferT m Word -- ^ the size of the current 'Buffer' in bytes
bufferGetSizeM =
    liftM fromIntegral $
        marshalBufferM {# get GstBuffer->size #}

#if __GLASGOW_HASKELL__ >= 606
-- | Make an O(n) copy of the data stored in @buffer@.
bufferGetData :: BufferClass bufferT
              => bufferT       -- ^ @buffer@ - a 'Buffer'
              -> BS.ByteString -- ^ the data stored in @buffer@
bufferGetData buffer =
    unsafePerformIO $ withMiniObject buffer $ \bufferPtr ->
        do ptr <- {# get GstBuffer->data #} bufferPtr
           size <- {# get GstBuffer->size #} bufferPtr
#ifdef OLD_BYTESTRING
           BS.copyCStringLen (castPtr ptr, fromIntegral size)
#else
           BS.packCStringLen (castPtr ptr, fromIntegral size)
#endif

-- | Make an O(n) copy of the current 'Buffer'.
bufferGetDataM :: (BufferClass bufferT, MonadIO m)
               => MiniObjectT bufferT m BS.ByteString -- ^ the data stored in the current 'Buffer'
bufferGetDataM =
    marshalBufferM $ \bufferPtr ->
        do ptr <- {# get GstBuffer->data #} bufferPtr
           size <- {# get GstBuffer->size #} bufferPtr
#ifdef OLD_BYTESTRING
           BS.copyCStringLen (castPtr ptr, fromIntegral size)
#else
           BS.packCStringLen (castPtr ptr, fromIntegral size)
#endif

-- | Store an O(n) copy of the provided data in the current 'Buffer'.
bufferSetDataM :: (BufferClass bufferT, MonadIO m)
               => BS.ByteString            -- ^ @bs@ - the data to store in the current 'Buffer'
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
#endif

-- | Get a raw pointer to the internal data area for the current
--   buffer. The pointer may be used to write into the data area if
--   desired. This function is unsafe in that the pointer should not
--   be used once the 'Buffer' is returned.
unsafeBufferGetPtrM :: (BufferClass bufferT, MonadIO m)
                    => MiniObjectT bufferT m (Ptr Word8) -- ^ a pointer to the data stored in the current 'Buffer'
unsafeBufferGetPtrM = do
  ptr <- askMiniObjectPtr
  liftIO $ liftM castPtr $
      {# get GstBuffer->data #} ptr

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

-- | Get the timestamp on @buffer@.
bufferGetTimestamp :: BufferClass bufferT
                   => bufferT         -- ^ @buffer@ - a 'Buffer'
                   -> Maybe ClockTime -- ^ the timestamp on @buffer@
bufferGetTimestamp =
    marshalGetNum {# get GstBuffer->timestamp #} clockTimeNone

-- | Get the timestamp on the current 'Buffer'.
bufferGetTimestampM :: (BufferClass bufferT, MonadIO m)
                    => MiniObjectT bufferT m (Maybe ClockTime) -- ^ the timestamp on the current 'Buffer'
bufferGetTimestampM =
    marshalGetNumM {# get GstBuffer->timestamp #} clockTimeNone

-- | Set the timestamp on the current 'Buffer'.
bufferSetTimestampM :: (BufferClass bufferT, MonadIO m)
                    => Maybe ClockTime          -- ^ @timestamp@ - the timestamp to set on the current 'Buffer'
                    -> MiniObjectT bufferT m ()
bufferSetTimestampM =
    marshalSetNumM {# set GstBuffer->timestamp #} clockTimeNone

-- | Get the duration of @buffer@.
bufferGetDuration :: BufferClass bufferT
                  => bufferT         -- ^ @buffer@ - a 'Buffer'
                  -> Maybe ClockTime -- ^ the duration of @buffer@
bufferGetDuration =
    marshalGetNum {# get GstBuffer->duration #} clockTimeNone

-- | Get the duration of the current 'Buffer'.
bufferGetDurationM :: (BufferClass bufferT, MonadIO m)
                   => MiniObjectT bufferT m (Maybe ClockTime) -- ^ the duration of the current 'Buffer'
bufferGetDurationM =
    marshalGetNumM {# get GstBuffer->duration #} clockTimeNone

-- | Set the duration of the current 'Buffer'.
bufferSetDurationM :: (BufferClass bufferT, MonadIO m)
                   => Maybe ClockTime          -- ^ @duration@ - the duration to set on the current 'Buffer'
                   -> MiniObjectT bufferT m ()
bufferSetDurationM =
    marshalSetNumM {# set GstBuffer->duration #} clockTimeNone

-- | Get the 'Caps' of @buffer@.
bufferGetCaps :: BufferClass bufferT
              => bufferT    -- ^ @buffer@ - a buffer
              -> Maybe Caps -- ^ the 'Caps' of @buffer@ if set, otherwise 'Nothing'
bufferGetCaps buffer =
    unsafePerformIO $
        {# call buffer_get_caps #} (toBuffer buffer) >>=
            maybePeek takeCaps

-- | Get the caps of the current 'Buffer'.
bufferGetCapsM :: (BufferClass bufferT, MonadIO m)
               => MiniObjectT bufferT m (Maybe Caps) -- ^ the 'Caps' of the current 'Buffer'
                                                     --   if set, otherwise 'Nothing'
bufferGetCapsM = do
  ptr <- askMiniObjectPtr
  liftIO $ gst_buffer_get_caps (castPtr ptr) >>=
               maybePeek takeCaps
  where _ = {# call buffer_get_caps #}

-- | Set the caps of the current 'Buffer'.
bufferSetCapsM :: (BufferClass bufferT, MonadIO m)
               => Maybe Caps               -- ^ @caps@ - the 'Caps' to set on the current
                                           --   'Buffer', or 'Nothing' to unset them
               -> MiniObjectT bufferT m ()
bufferSetCapsM capsM = do
  ptr <- askMiniObjectPtr
  liftIO $ withForeignPtr (case capsM of
                             Just caps -> unCaps caps
                             Nothing   -> nullForeignPtr)
                          (gst_buffer_set_caps $ castPtr ptr)
  where _ = {# call buffer_set_caps #}

-- | Get the start offset of the 'Buffer'.
bufferGetOffset :: BufferClass bufferT
                => bufferT      -- ^ @buffer@ - a buffer
                -> Maybe Word64 -- ^ the start offset of @buffer@ if set, otherwise 'Nothing'
bufferGetOffset =
    marshalGetNum {# get GstBuffer->offset #} bufferOffsetNone

-- | Get the start offset of the current 'Buffer'.
bufferGetOffsetM :: (BufferClass bufferT, MonadIO m)
                 => MiniObjectT bufferT m (Maybe Word64) -- ^ the start offset of the current
                                                         --   'Buffer' if set, otherwise 'Nothing'
bufferGetOffsetM =
    marshalGetNumM {# get GstBuffer->offset #} bufferOffsetNone

-- | Set the start offset of the current 'Buffer'.
bufferSetOffsetM :: (BufferClass bufferT, MonadIO m)
                 => Maybe Word64             -- ^ @offset@ - the start offset to set on the current buffer
                 -> MiniObjectT bufferT m ()
bufferSetOffsetM =
    marshalSetNumM {# set GstBuffer->offset #} bufferOffsetNone

-- | Get the end offset of the 'Buffer'.
bufferGetOffsetEnd :: BufferClass bufferT
                   => bufferT      -- ^ @buffer@ - a buffer
                   -> Maybe Word64 -- ^ the end offset of @buffer@ if set, otherwise 'Nothing'
bufferGetOffsetEnd =
    marshalGetNum {# get GstBuffer->offset_end #} bufferOffsetNone

-- | Get the end offset of the current 'Buffer'.
bufferGetOffsetEndM :: (BufferClass bufferT, MonadIO m)
                    => MiniObjectT bufferT m (Maybe Word64) -- ^ the start offset of the current
                                                            --   'Buffer' if set, otherwise 'Nothing'
bufferGetOffsetEndM =
    marshalGetNumM {# get GstBuffer->offset_end #} bufferOffsetNone

-- | Set the end offset of the current 'Buffer'.
bufferSetOffsetEndM :: (BufferClass bufferT, MonadIO m)
                    => Maybe Word64             -- ^ @offset@ - the end offset to set on the current buffer
                    -> MiniObjectT bufferT m ()
bufferSetOffsetEndM =
    marshalSetNumM {# set GstBuffer->offset_end #} bufferOffsetNone

-- | Return 'True' if the 'Buffer' marks a discontinuity in a stream, or
--   'False' otherwise. This typically occurs after a seek or a
--   dropped buffer from a live or network source.
bufferIsDiscont :: BufferClass bufferT
                => bufferT -- ^ @buffer@ - a buffer
                -> Bool    -- ^ 'True' if @buffer@ marks a discontinuity in a stream
bufferIsDiscont =
    (elem BufferDiscont) . bufferGetFlags

-- | Return 'True' if the current 'Buffer' marks a discontinuity in a
--   stream, or 'False' otherwise.
bufferIsDiscontM :: (BufferClass bufferT, MonadIO m)
                 => MiniObjectT bufferT m Bool -- ^ 'True' if the current buffer marks a
                                               --   discontinuity in a stream
bufferIsDiscontM =
    liftM (elem BufferDiscont) $ bufferGetFlagsM

-- | Create an empty 'Buffer' and mutate it according to the given
--   action. Once this function returns, the 'Buffer' is immutable.
bufferCreateEmpty :: MonadIO m
                  => MiniObjectT Buffer m a -- ^ @mutate@ - the mutating action
                  -> m (Buffer, a)          -- ^ the new buffer and the action's result
bufferCreateEmpty =
    marshalMiniObjectModify $ liftIO {# call buffer_new #}

-- | Create and mutate a 'Buffer' of the given size.
bufferCreate :: MonadIO m
             => Word                   -- ^ @size@ - the size of the 'Buffer' to be created
             -> MiniObjectT Buffer m a -- ^ @mutate@ - the mutating action
             -> m (Buffer, a)          -- ^ the new 'Buffer' and the action's result
bufferCreate size =
    marshalMiniObjectModify $
        liftIO $ {# call buffer_new_and_alloc #} $ fromIntegral size

-- | Create a sub-buffer from an existing 'Buffer' with the given offset
--   and size. This sub-buffer uses the actual memory space of the
--   parent buffer. Thus function will copy the offset and timestamp
--   fields when the offset is 0. Otherwise, they will both be set to
--   'Nothing'. If the offset is 0 and the size is the total size of
--   the parent, the duration and offset end fields are also
--   copied. Otherwise they will be set to 'Nothing'.
bufferCreateSub :: BufferClass bufferT
                => bufferT      -- ^ @parent@ - the parent buffer
                -> Word         -- ^ @offset@ - the offset
                -> Word         -- ^ @size@ - the size
                -> Maybe Buffer -- ^ the new sub-buffer
bufferCreateSub parent offset size =
    unsafePerformIO $
        {# call buffer_create_sub #} (toBuffer parent)
                                     (fromIntegral offset)
                                     (fromIntegral size) >>=
            maybePeek takeMiniObject

-- | Return 'True' if 'bufferSpan' can be done without copying the
--   data, or 'False' otherwise.
bufferIsSpanFast :: (BufferClass bufferT1, BufferClass bufferT2)
                 => bufferT1 -- ^ @buffer1@ - the first buffer
                 -> bufferT2 -- ^ @buffer2@ - the second buffer
                 -> Bool     -- ^ 'True' if the buffers are contiguous,
                             --   or 'False' if copying would be
                             --   required
bufferIsSpanFast buffer1 buffer2 =
    toBool $ unsafePerformIO $
        {# call buffer_is_span_fast #} (toBuffer buffer1)
                                       (toBuffer buffer2)

-- | Create a new 'Buffer' that consists of a span across the given
--   buffers. Logically, the buffers are concatenated to make a larger
--   buffer, and a new buffer is created at the given offset and with
--   the given size.
--   
--   If the two buffers are children of the same larger buffer, and
--   are contiguous, no copying is necessary. You can use
--   'bufferIsSpanFast' to determine if copying is needed.
bufferSpan :: (BufferClass bufferT1, BufferClass bufferT2)
           => bufferT1     -- ^ @buffer1@ - the first buffer
           -> Word32       -- ^ @offset@ - the offset into the concatenated buffer
           -> bufferT2     -- ^ @buffer2@ - the second buffer
           -> Word32       -- ^ @len@ - the length of the final buffer
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
            => bufferT1 -- ^ @buffer1@ - a buffer
            -> bufferT2 -- ^ @buffer2@ - a buffer
            -> Buffer   -- ^ the concatenation of the buffers
bufferMerge buffer1 buffer2 =
    unsafePerformIO $
        {# call buffer_merge #} (toBuffer buffer1)
                                (toBuffer buffer2) >>=
            takeMiniObject
