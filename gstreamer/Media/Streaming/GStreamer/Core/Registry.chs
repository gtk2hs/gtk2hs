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
module Media.Streaming.GStreamer.Core.Registry (
  
  Registry,
  RegistryClass,
  castToRegistry,
  toRegistry,
  defaultRegistry,
  registryGetFeatureList,
  registryGetFeatureListByPlugin,
  registryGetPathList,
  registryGetPluginList,
  registryAddPlugin,
  registryRemovePlugin,
  registryPluginFilter,
  registryFeatureFilter,
  registryFindPlugin,
  registryFindFeature,
  registryLookupFeature,
  registryScanPath,
  registryXMLReadCache,
  registryXMLWriteCache,
  registryLookup,
  registryRemoveFeature,
  registryAddFeature,
  defaultRegistryCheckFeatureVersion,
  defaultRegistryGetPathList,
  defaultRegistryAddPlugin,
  defaultRegistryScanPath,
  defaultRegistryFindPlugin,
  defaultRegistryFindFeature,
  defaultRegistryFeatureFilter,
  onRegistryFeatureAdded,
  afterRegistryFeatureAdded,
  onRegistryPluginAdded,
  afterRegistryPluginAdded
  
  ) where

import Control.Monad (liftM)
{#import Media.Streaming.GStreamer.Core.Types#}
{#import Media.Streaming.GStreamer.Core.Signals#}
{#import System.Glib.GType#}
import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Signals
{#import System.Glib.GList#}

{# context lib = "gstreamer" prefix = "gst" #}

defaultRegistry :: Registry
defaultRegistry =
    unsafePerformIO $ {# call registry_get_default #} >>= peekRegistry

registryGetFeatureList :: RegistryClass registry
                       => registry
                       -> GType
                       -> IO [PluginFeature]
registryGetFeatureList registry gType =
    {# call registry_get_feature_list #} (toRegistry registry) gType >>=
            fromGList >>= mapM takePluginFeature

registryGetFeatureListByPlugin :: RegistryClass registry
                               => registry
                               -> String
                               -> IO [PluginFeature]
registryGetFeatureListByPlugin registry name =
    withUTFString name $ \cName ->
        {# call registry_get_feature_list_by_plugin #} (toRegistry registry) cName >>=
            fromGList >>= mapM takePluginFeature

registryGetPathList :: RegistryClass registry
                    => registry
                    -> IO [FilePath]
registryGetPathList registry =
    {# call registry_get_path_list #} (toRegistry registry) >>=
        fromGList >>= mapM peekUTFString

registryGetPluginList :: RegistryClass registry
                      => registry
                      -> IO [Plugin]
registryGetPluginList registry =
    {# call registry_get_plugin_list #} (toRegistry registry) >>=
        fromGList >>= mapM takePlugin

registryAddPlugin :: (RegistryClass registry, PluginClass plugin)
                  => registry
                  -> plugin
                  -> IO Bool
registryAddPlugin registry plugin =
    liftM toBool $ {# call registry_add_plugin #} (toRegistry registry) (toPlugin plugin)

registryRemovePlugin :: (RegistryClass registry, PluginClass plugin)
                     => registry
                     -> plugin
                     -> IO ()
registryRemovePlugin registry plugin =
    {# call registry_remove_plugin #} (toRegistry registry) (toPlugin plugin)

type CPluginFilter = Ptr Plugin
                  -> {# type gpointer #}
                  -> IO {# type gboolean #}
marshalPluginFilter :: PluginFilter
                    -> IO {# type GstPluginFilter #}
marshalPluginFilter pluginFilter =
    makePluginFilter cPluginFilter
    where cPluginFilter :: CPluginFilter
          cPluginFilter pluginPtr _ =
               liftM fromBool $ peekPlugin pluginPtr >>= pluginFilter
foreign import ccall "wrapper"
    makePluginFilter :: CPluginFilter
                     -> IO {# type GstPluginFilter #}

registryPluginFilter :: RegistryClass registry
                     => registry
                     -> PluginFilter
                     -> Bool
                     -> IO [Plugin]
registryPluginFilter registry pluginFilter first =
    do cPluginFilter <- marshalPluginFilter pluginFilter
       {# call registry_plugin_filter #} (toRegistry registry)
                                         cPluginFilter
                                         (fromBool first)
                                         nullPtr >>=
           fromGList >>= mapM takePlugin


type CPluginFeatureFilter = Ptr PluginFeature
                         -> {# type gpointer #}
                         -> IO {# type gboolean #}
marshalPluginFeatureFilter :: PluginFeatureFilter
                           -> IO {# type GstPluginFeatureFilter #}
marshalPluginFeatureFilter pluginFeatureFilter =
    makePluginFeatureFilter cPluginFeatureFilter
    where cPluginFeatureFilter :: CPluginFeatureFilter
          cPluginFeatureFilter pluginFeaturePtr _ =
               liftM fromBool $ peekPluginFeature pluginFeaturePtr >>= pluginFeatureFilter
foreign import ccall "wrapper"
    makePluginFeatureFilter :: CPluginFeatureFilter
                            -> IO {# type GstPluginFeatureFilter #}

registryFeatureFilter :: RegistryClass registry
                     => registry
                     -> PluginFeatureFilter
                     -> Bool
                     -> IO [PluginFeature]
registryFeatureFilter registry featureFilter first =
    do cPluginFeatureFilter <- marshalPluginFeatureFilter featureFilter
       {# call registry_feature_filter #} (toRegistry registry)
                                         cPluginFeatureFilter
                                         (fromBool first)
                                         nullPtr >>=
           fromGList >>= mapM takePluginFeature


registryFindPlugin :: RegistryClass registry
                   => registry
                   -> String
                   -> IO (Maybe Plugin)
registryFindPlugin registry name =
    (withUTFString name $ {# call registry_find_plugin #} (toRegistry registry)) >>=
        maybePeek takePlugin

registryFindFeature :: RegistryClass registry
                    => registry
                    -> String
                    -> GType
                    -> IO (Maybe PluginFeature)
registryFindFeature registry name gType =
    withUTFString name $ \cName ->
        {# call registry_find_feature #} (toRegistry registry)
                                         cName
                                         gType >>=
            maybePeek takePluginFeature

registryLookupFeature :: RegistryClass registry
                      => registry
                      -> String
                      -> IO (Maybe PluginFeature)
registryLookupFeature registry name =
    (withUTFString name $ {# call registry_lookup_feature #} (toRegistry registry)) >>=
        maybePeek takePluginFeature

registryScanPath :: RegistryClass registry
                 => registry
                 -> FilePath
                 -> IO Bool
registryScanPath registry path =
    liftM toBool $
        withUTFString path $
            {# call registry_scan_path #} (toRegistry registry)

registryXMLReadCache :: RegistryClass registry
                     => registry
                     -> FilePath
                     -> IO Bool
registryXMLReadCache registry location =
    liftM toBool $
        withUTFString location $
            {# call registry_xml_read_cache #} (toRegistry registry)

registryXMLWriteCache :: RegistryClass registry
                      => registry
                      -> FilePath
                      -> IO Bool
registryXMLWriteCache registry location =
    liftM toBool $
        withUTFString location $
            {# call registry_xml_write_cache #} (toRegistry registry)

registryLookup :: RegistryClass registry
               => registry
               -> FilePath
               -> IO Plugin
registryLookup registry filename =
    (withUTFString filename $
         {# call registry_lookup #} (toRegistry registry)) >>=
        takePlugin

registryRemoveFeature :: (RegistryClass registry, PluginFeatureClass pluginFeature)
                      => registry
                      -> pluginFeature
                      -> IO ()
registryRemoveFeature registry pluginFeature =
    {# call registry_remove_feature #} (toRegistry registry) (toPluginFeature pluginFeature)

registryAddFeature :: (RegistryClass registry, PluginFeatureClass pluginFeature)
                   => registry
                   -> pluginFeature
                   -> IO Bool
registryAddFeature registry pluginFeature =
    liftM toBool $
        {# call registry_add_feature #} (toRegistry registry) (toPluginFeature pluginFeature)

defaultRegistryCheckFeatureVersion :: String
                                   -> Word
                                   -> Word
                                   -> Word
                                   -> IO Bool
defaultRegistryCheckFeatureVersion feature minMajor minMinor minMicro =
    liftM toBool $ withUTFString feature $ \cFeature ->
        {# call default_registry_check_feature_version #} cFeature
                                                          (fromIntegral minMajor)
                                                          (fromIntegral minMinor)
                                                          (fromIntegral minMicro)

defaultRegistryGetPathList :: IO [FilePath]
defaultRegistryGetPathList =
    registryGetPathList defaultRegistry

defaultRegistryAddPlugin :: PluginClass plugin
                         => plugin
                         -> IO Bool
defaultRegistryAddPlugin =
    registryAddPlugin defaultRegistry

defaultRegistryScanPath :: String
                        -> IO Bool
defaultRegistryScanPath =
    registryScanPath defaultRegistry

defaultRegistryFindPlugin :: String
                          -> IO (Maybe Plugin)
defaultRegistryFindPlugin =
    registryFindPlugin defaultRegistry

defaultRegistryFindFeature :: String
                           -> GType
                           -> IO (Maybe PluginFeature)
defaultRegistryFindFeature =
    registryFindFeature defaultRegistry

defaultRegistryFeatureFilter :: PluginFeatureFilter
                             -> Bool
                             -> IO [PluginFeature]
defaultRegistryFeatureFilter =
    registryFeatureFilter defaultRegistry

onRegistryFeatureAdded, afterRegistryFeatureAdded :: RegistryClass registry
                                                  => registry
                                                  -> (PluginFeature -> IO ())
                                                  -> IO (ConnectId registry)
onRegistryFeatureAdded =
    connect_OBJECT__NONE "feature-added" False
afterRegistryFeatureAdded =
    connect_OBJECT__NONE "feature-added" False

onRegistryPluginAdded, afterRegistryPluginAdded :: RegistryClass registry
                                                => registry
                                                -> (Plugin -> IO ())
                                                -> IO (ConnectId registry)
onRegistryPluginAdded =
    connect_OBJECT__NONE "plugin-added" False
afterRegistryPluginAdded =
    connect_OBJECT__NONE "plugin-added" False
