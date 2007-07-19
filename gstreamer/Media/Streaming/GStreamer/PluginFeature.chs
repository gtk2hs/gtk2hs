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
module Media.Streaming.GStreamer.PluginFeature (
  PluginFeature,
  PluginFeatureClass,
  pluginFeatureLoad,
  pluginFeatureCheckVersion
  ) where

import Control.Monad (liftM)
{#import Media.Streaming.GStreamer.Types#}
import System.Glib.FFI
import GHC.Base (unsafeCoerce#)

{# context lib = "gstreamer" prefix = "gst" #}

pluginFeatureLoad :: PluginFeatureClass pluginFeature
                  => pluginFeature
                  -> IO pluginFeature
pluginFeatureLoad pluginFeature =
    liftM unsafeCoerce# $
        {# call plugin_feature_load #} (toPluginFeature pluginFeature) >>=
            newPluginFeature

pluginFeatureCheckVersion :: PluginFeatureClass pluginFeature
                          => pluginFeature
                          -> Word
                          -> Word
                          -> Word
                          -> IO Bool
pluginFeatureCheckVersion pluginFeature minMajor minMinor minMicro =
    liftM toBool $
        {# call plugin_feature_check_version #} (toPluginFeature pluginFeature)
                                                (fromIntegral minMajor)
                                                (fromIntegral minMinor)
                                                (fromIntegral minMicro)
