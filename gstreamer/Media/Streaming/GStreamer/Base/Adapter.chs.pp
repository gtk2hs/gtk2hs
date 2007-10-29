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
module Media.Streaming.GStreamer.Base.Adapter (
  
  Adapter,
  AdapterClass,
  toAdapter,
  castToAdapter,
  isAdapter,
  adapterNew,
  adapterClear,
  adapterPush,
  adapterPeek,
  adapterCopy,
  adapterCopyInto,
  adapterFlush,
  adapterAvailable,
  adapterAvailableFast,
  adapterTake,
  adapterTakeBuffer
  
  ) where

#if __GLASGOW_HASKELL__ >= 606 && __GLASGOW_HASKELL__ < 608
#define OLD_BYTESTRING
#endif

import Control.Monad (liftM)
import qualified Data.ByteString as BS
#ifdef OLD_BYTESTRING
import qualified Data.ByteString.Base as BS
#else
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Internal as BS
#endif
{#import Media.Streaming.GStreamer.Base.Types#}
import System.Glib.FFI
import System.Glib.GObject
import System.Glib.Flags
import System.Glib.Attributes
{#import System.Glib.Properties#}

{# context lib = "gstreamer" prefix = "gst" #}

adapterNew :: IO Adapter
adapterNew =
    constructNewGObject mkAdapter {# call adapter_new #}

adapterClear :: AdapterClass adapterT
             => adapterT
             -> IO ()
adapterClear =
    {# call adapter_clear #} . toAdapter

adapterPush :: (AdapterClass adapterT, BufferClass bufferT)
            => adapterT
            -> bufferT
            -> IO ()
adapterPush adapter buffer =
    {# call adapter_push #} (toAdapter adapter) (toBuffer buffer)

adapterPeek :: AdapterClass adapterT
            => adapterT
            -> Word
            -> IO (Maybe BS.ByteString)
adapterPeek adapter size =
    do ptr <- {# call adapter_peek #} (toAdapter adapter) (fromIntegral size)
       if ptr == nullPtr
           then return Nothing
#ifdef OLD_BYTESTRING
           else liftM Just $ BS.copyCStringLen (castPtr ptr, fromIntegral size)
#else
           else liftM Just $ BS.packCStringLen (castPtr ptr, fromIntegral size)
#endif

adapterCopy :: AdapterClass adapterT
            => adapterT
            -> Word
            -> Word
            -> IO BS.ByteString
adapterCopy adapter offset size = do
    BS.create (fromIntegral size) $ \dest ->
        {# call adapter_copy #} (toAdapter adapter)
                                (castPtr dest)
                                (fromIntegral offset)
                                (fromIntegral size)

adapterCopyInto :: AdapterClass adapterT
                => adapterT
                -> BS.ByteString
                -> Word
                -> IO ()
adapterCopyInto adapter dest offset =
    BS.useAsCStringLen dest $ \(destPtr, size) ->
        {# call adapter_copy #} (toAdapter adapter)
                                (castPtr destPtr)
                                (fromIntegral offset)
                                (fromIntegral size)

adapterFlush :: AdapterClass adapterT
             => adapterT
             -> Word
             -> IO ()
adapterFlush adapter flush =
    {# call adapter_flush #} (toAdapter adapter) $ fromIntegral flush

adapterAvailable :: AdapterClass adapterT
                 => adapterT
                 -> IO Word
adapterAvailable adapter =
    liftM fromIntegral $
        {# call adapter_available #} $ toAdapter adapter

adapterAvailableFast :: AdapterClass adapterT
                     => adapterT
                     -> IO Word
adapterAvailableFast adapter =
    liftM fromIntegral $
        {# call adapter_available_fast #} $ toAdapter adapter

adapterTake :: AdapterClass adapterT
            => adapterT
            -> Word
            -> IO (Maybe BS.ByteString)
adapterTake adapter nBytes =
    do ptr <- {# call adapter_take #} (toAdapter adapter)
                                      (fromIntegral nBytes)
       if ptr == nullPtr
          then do fPtr <- newForeignPtr (castPtr ptr) gFreePtr
                  return $ Just $
                      BS.fromForeignPtr (castForeignPtr fPtr)
#ifndef OLD_BYTESTRING
                                        0
#endif
                                        (fromIntegral nBytes)
          else return Nothing
foreign import ccall unsafe "&g_free"
    gFreePtr :: FunPtr (Ptr () -> IO ())

adapterTakeBuffer :: AdapterClass adapterT
                  => adapterT
                  -> Word
                  -> IO (Maybe Buffer)
adapterTakeBuffer adapter nBytes =
    {# call adapter_take_buffer #} (toAdapter adapter) (fromIntegral nBytes) >>=
        maybePeek takeMiniObject
