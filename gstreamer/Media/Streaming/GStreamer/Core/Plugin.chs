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
module Media.Streaming.GStreamer.Core.Plugin (
  Plugin,
  PluginClass,
  castToPlugin,
  toPlugin,
  pluginGetName,
  pluginGetDescription,
  pluginGetFilename,
  pluginGetLicense,
  pluginGetPackage,
  pluginGetOrigin,
  pluginGetSource,
  pluginGetVersion,
  pluginIsLoaded,
  pluginLoadByName,
  pluginLoad,
  
  pluginName,
  pluginDescription,
  pluginFilename,
  pluginLicense,
  pluginPackage,
  pluginOrigin,
  pluginSource,
  pluginVersion
  
  ) where

import Control.Monad (liftM)
{#import Media.Streaming.GStreamer.Core.Types#}
import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes ( ReadAttr
                              , readAttr )
import GHC.Base (unsafeCoerce#)

{# context lib = "gstreamer" prefix = "gst" #}

pluginGetName :: PluginClass plugin
              => plugin
              -> IO String
pluginGetName plugin =
    {# call plugin_get_name #} (toPlugin plugin) >>= peekUTFString

pluginGetDescription :: PluginClass plugin
                     => plugin
                     -> IO String
pluginGetDescription plugin =
    {# call plugin_get_description #} (toPlugin plugin) >>= peekUTFString

pluginGetFilename :: PluginClass plugin
                  => plugin
                  -> IO FilePath
pluginGetFilename plugin =
    {# call plugin_get_filename #} (toPlugin plugin) >>= peekUTFString

pluginGetLicense :: PluginClass plugin
                 => plugin
                 -> IO String
pluginGetLicense plugin =
    {# call plugin_get_license #} (toPlugin plugin) >>= peekUTFString

pluginGetPackage :: PluginClass plugin
                 => plugin
                 -> IO String
pluginGetPackage plugin =
    {# call plugin_get_package #} (toPlugin plugin) >>= peekUTFString

pluginGetOrigin :: PluginClass plugin
                => plugin
                -> IO String
pluginGetOrigin plugin =
    {# call plugin_get_origin #} (toPlugin plugin) >>= peekUTFString

pluginGetSource :: PluginClass plugin
                => plugin
                -> IO String
pluginGetSource plugin =
    {# call plugin_get_source #} (toPlugin plugin) >>= peekUTFString

pluginGetVersion :: PluginClass plugin
                 => plugin
                 -> IO String
pluginGetVersion plugin =
    {# call plugin_get_version #} (toPlugin plugin) >>= peekUTFString

pluginIsLoaded :: PluginClass plugin
               => plugin
               -> IO Bool
pluginIsLoaded =
    (liftM toBool) . {# call plugin_is_loaded #} . toPlugin

pluginName :: PluginClass plugin
           => ReadAttr plugin String
pluginName = readAttr pluginGetName

pluginDescription :: PluginClass plugin
                  => ReadAttr plugin String
pluginDescription = readAttr pluginGetDescription

pluginFilename :: PluginClass plugin
               => ReadAttr plugin FilePath
pluginFilename = readAttr pluginGetFilename

pluginLicense :: PluginClass plugin
              => ReadAttr plugin String
pluginLicense = readAttr pluginGetLicense

pluginPackage :: PluginClass plugin
              => ReadAttr plugin String
pluginPackage = readAttr pluginGetPackage

pluginOrigin :: PluginClass plugin
             => ReadAttr plugin String
pluginOrigin = readAttr pluginGetOrigin

pluginSource :: PluginClass plugin
             => ReadAttr plugin String
pluginSource = readAttr pluginGetSource

pluginVersion :: PluginClass plugin
              => ReadAttr plugin String
pluginVersion = readAttr pluginGetVersion

pluginLoad :: PluginClass plugin
           => plugin
           -> IO plugin
pluginLoad plugin =
    liftM unsafeCoerce# $ {# call plugin_load #} (toPlugin plugin) >>=
        takePlugin

pluginLoadByName :: String
                 -> IO Plugin
pluginLoadByName name =
    withUTFString name {# call plugin_load_by_name #} >>=
        takePlugin
