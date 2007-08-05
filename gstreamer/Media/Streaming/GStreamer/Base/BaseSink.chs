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
module Media.Streaming.GStreamer.Base.BaseSink (
  
  BaseSink,
  BaseSinkClass,
  castToBaseSink,
  toBaseSink,
  baseSinkQueryLatency,
  baseSinkGetLatency,
  baseSinkWaitPreroll,
  baseSinkSetSync,
  baseSinkGetSync,
  baseSinkSetMaxLateness,
  baseSinkGetMaxLateness,
  baseSinkIsQOSEnabled,
  baseSinkSetQOSEnabled,
  baseSinkPrerollQueueLength,
  baseSinkGetPad
  
  ) where

import Control.Monad (liftM, liftM4)
{#import Media.Streaming.GStreamer.Base.Types#}
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.Attributes
{#import System.Glib.Properties#}

{# context lib = "gstreamer" prefix = "gst" #}

baseSinkQueryLatency :: BaseSinkClass baseSinkT
                     => baseSinkT
                     -> IO (Maybe (Bool, Bool, ClockTime, ClockTime))
baseSinkQueryLatency baseSink =
    alloca $ \livePtr -> alloca $ \upstreamLivePtr ->
        alloca $ \minLatencyPtr -> alloca $ \maxLatencyPtr ->
            do result <- {# call base_sink_query_latency #} (toBaseSink baseSink)
                                                            livePtr
                                                            upstreamLivePtr
                                                            minLatencyPtr
                                                            maxLatencyPtr
               if toBool result
                   then do live <- peek livePtr
                           upstreamLive <- peek upstreamLivePtr
                           minLatency <- peek minLatencyPtr
                           maxLatency <- peek maxLatencyPtr
                           return $ Just (toBool live,
                                          toBool upstreamLive,
                                          cToEnum minLatency,
                                          cToEnum maxLatency)
                   else return Nothing

baseSinkGetLatency :: BaseSinkClass baseSinkT
                   => baseSinkT
                   -> IO ClockTime
baseSinkGetLatency baseSink =
    liftM cToEnum $
        {# call base_sink_get_latency #} (toBaseSink baseSink)

baseSinkWaitPreroll :: BaseSinkClass baseSinkT
                    => baseSinkT
                    -> IO FlowReturn
baseSinkWaitPreroll baseSink =
    liftM cToEnum $
        {# call base_sink_wait_preroll #} (toBaseSink baseSink)

baseSinkSetSync :: BaseSinkClass baseSinkT
                => baseSinkT
                -> Bool
                -> IO ()
baseSinkSetSync baseSink sync =
    {# call base_sink_set_sync #} (toBaseSink baseSink) $ fromBool sync

baseSinkGetSync :: BaseSinkClass baseSinkT
                => baseSinkT
                -> IO Bool
baseSinkGetSync baseSink =
    liftM toBool $
        {# call base_sink_get_sync #} (toBaseSink baseSink)

baseSinkSetMaxLateness :: BaseSinkClass baseSinkT
                       => baseSinkT
                       -> Word64
                       -> IO ()
baseSinkSetMaxLateness baseSink maxLateness =
    {# call base_sink_set_max_lateness #} (toBaseSink baseSink) $ fromIntegral maxLateness

baseSinkGetMaxLateness :: BaseSinkClass baseSinkT
                       => baseSinkT
                       -> IO Word64
baseSinkGetMaxLateness baseSink =
    liftM fromIntegral $
        {# call base_sink_get_max_lateness #} (toBaseSink baseSink)

baseSinkIsQOSEnabled :: BaseSinkClass baseSinkT
                     => baseSinkT
                     -> IO Bool
baseSinkIsQOSEnabled baseSink =
    liftM toBool $
        {# call base_sink_is_qos_enabled #} (toBaseSink baseSink)

baseSinkSetQOSEnabled :: BaseSinkClass baseSinkT
                      => baseSinkT
                      -> Bool
                      -> IO ()
baseSinkSetQOSEnabled baseSink enabled =
    {# call base_sink_set_qos_enabled #} (toBaseSink baseSink) $ fromBool enabled

baseSinkPrerollQueueLength :: BaseSinkClass baseSinkT
                           => Attr baseSinkT Int
baseSinkPrerollQueueLength =
    newAttrFromUIntProperty "preroll-queue-len"

baseSinkGetPad :: BaseSinkClass baseSinkT
               => baseSinkT
               -> IO Pad
baseSinkGetPad baseSink =
    withBaseSink (toBaseSink baseSink) cBaseSinkGetPad >>= peekPad
foreign import ccall unsafe "_hs_gst_base_sink_get_pad"
    cBaseSinkGetPad :: Ptr BaseSink
                    -> IO (Ptr Pad)
