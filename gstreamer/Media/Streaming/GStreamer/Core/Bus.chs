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
module Media.Streaming.GStreamer.Core.Bus (
  
  Bus,
  BusClass,
  castToBus,
  toBus,
  busNew,
  busPost,
  busHavePending,
  busPeek,
  busPop,
  busTimedPop,
  busSetFlushing,
  busCreateWatch,
  busAddWatch,
  busAddSignalWatch,
  busRemoveSignalWatch,
  busPoll,
  
  onBusMessage,
  afterBusMessage
  
  ) where

import Control.Monad (liftM)
{#import Media.Streaming.GStreamer.Core.Types#}
{#import Media.Streaming.GStreamer.Core.Signals#}
{#import System.Glib.MainLoop#}
import System.Glib.Flags
import System.Glib.FFI
{#import System.Glib.GObject#}
{#import System.Glib.MainLoop#}

{# context lib = "gstreamer" prefix = "gst" #}

busGetFlags :: BusClass busT
            => busT
            -> IO [BusFlags]
busGetFlags = mkObjectGetFlags

busSetFlags :: BusClass busT
            => busT
            -> [BusFlags]
            -> IO ()
busSetFlags = mkObjectSetFlags

busUnsetFlags :: BusClass busT
              => busT
              -> [BusFlags]
              -> IO ()
busUnsetFlags = mkObjectUnsetFlags

busNew :: IO Bus
busNew =
    {# call bus_new #} >>= takeObject

busPost :: Bus
        -> Message
        -> IO Bool
busPost bus message =
    do {# call gst_mini_object_ref #} (toMiniObject message)
       liftM toBool $ {# call bus_post #} bus message

busHavePending :: Bus
               -> IO Bool
busHavePending bus =
    liftM toBool $ {# call bus_have_pending #} bus

busPeek :: Bus
        -> IO (Maybe Message)
busPeek bus =
    {# call bus_peek #} bus >>= maybePeek takeMiniObject

busPop :: Bus
       -> IO (Maybe Message)
busPop bus =
    {# call bus_pop #} bus >>= maybePeek takeMiniObject

busTimedPop :: Bus
            -> ClockTime
            -> IO (Maybe Message)
busTimedPop bus timeout =
    {# call bus_timed_pop #} bus (fromIntegral timeout) >>=
        maybePeek takeMiniObject

busSetFlushing :: Bus
               -> Bool
               -> IO ()
busSetFlushing bus flushing =
    {# call bus_set_flushing #} bus $ fromBool flushing

busCreateWatch :: Bus
               -> IO Source
busCreateWatch bus =
    liftM Source $ {# call bus_create_watch #} bus >>=
        flip newForeignPtr sourceFinalizer
foreign import ccall unsafe "&g_source_unref"
    sourceFinalizer :: FunPtr (Ptr Source -> IO ())

type CBusFunc =  Ptr Bus
              -> Ptr Message
              -> {# type gpointer #}
              -> IO {# type gboolean #}
marshalBusFunc :: BusFunc
               -> IO {# type GstBusFunc #}
marshalBusFunc busFunc =
    makeBusFunc cBusFunc
    where cBusFunc :: CBusFunc
          cBusFunc busPtr messagePtr userData =
              do bus <- peekObject busPtr
                 message <- peekMiniObject messagePtr
                 liftM fromBool $ busFunc bus message
foreign import ccall "wrapper"
    makeBusFunc :: CBusFunc
                -> IO {# type GstBusFunc #}

busAddWatch :: Bus
            -> Priority
            -> BusFunc
            -> IO Word
busAddWatch bus priority func =
    do busFuncPtr <- marshalBusFunc func
       destroyNotify <- mkFunPtrDestroyNotify busFuncPtr
       liftM fromIntegral $
           {# call bus_add_watch_full #}
               bus
               (fromIntegral priority)
               busFuncPtr
               nullPtr
               destroyNotify

busAddSignalWatch :: Bus
                  -> Priority
                  -> IO ()
busAddSignalWatch bus priority =
    {# call bus_add_signal_watch_full #} bus $ fromIntegral priority

busRemoveSignalWatch :: Bus
                     -> IO ()
busRemoveSignalWatch =
    {# call bus_remove_signal_watch #}

busPoll :: Bus
        -> [MessageType]
        -> ClockTimeDiff
        -> IO Message
busPoll bus events timeout =
    {# call bus_poll #} bus
                        (fromIntegral $ fromFlags events)
                        (fromIntegral timeout) >>=
        takeMiniObject

onBusMessage, afterBusMessage :: BusClass bus
                              => bus
                              -> (Message -> IO ())
                              -> IO (ConnectId bus)
onBusMessage =
    connect_BOXED__NONE "message" peekMiniObject False
afterBusMessage =
    connect_BOXED__NONE "message" peekMiniObject True
