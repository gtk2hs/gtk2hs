-- GIMP Toolkit (GTK) Binding for Haskell: binding to libgnomevfs   -*-haskell-*-
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
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.Gnome.VFS.VolumeMonitor (
  
-- * Types
  
  -- | An object that monitors volume mounts and unmounts.
  VolumeMonitor,
  VolumeMonitorClass,
  
-- * Values
  volumeMonitor,
  
-- * Operations
  volumeMonitorGetConnectedDrives,
  volumeMonitorGetDriveByID,
  volumeMonitorGetMountedVolumes,
  volumeMonitorGetVolumeByID,
  volumeMonitorGetVolumeForPath,
  
  onVolumeMonitorVolumeMounted,
  afterVolumeMonitorVolumeMounted,
  onVolumeMonitorVolumePreUnmount,
  afterVolumeMonitorVolumePreUnmount,
  onVolumeMonitorVolumeUnmounted,
  afterVolumeMonitorVolumeUnmounted
  
  ) where

import Control.Exception
import Control.Monad (liftM)
import System.Glib.FFI
import System.Glib.GList (readGList)
import System.Glib.UTFString
import System.Gnome.VFS.Marshal
{#import System.Gnome.VFS.Types#}
{#import System.Gnome.VFS.Signals#}
import System.IO (FilePath)

{# context lib = "gnomevfs" prefix = "gnome_vfs" #}

-- | The global volume monitor object.
volumeMonitor :: VolumeMonitor
volumeMonitor = unsafePerformIO $ {# call get_volume_monitor #} >>= wrapVolumeMonitor

-- | 
volumeMonitorGetConnectedDrives :: VolumeMonitorClass volumeMonitor =>
                                   volumeMonitor
                                -> IO [Drive]
volumeMonitorGetConnectedDrives volumeMonitor =
    {# call volume_monitor_get_connected_drives #} (castToVolumeMonitor volumeMonitor) >>=
        readGList >>= mapM newDrive

volumeMonitorGetDriveByID :: VolumeMonitorClass volumeMonitor =>
                             volumeMonitor
                          -> Word
                          -> IO Drive
volumeMonitorGetDriveByID volumeMonitor id =
    {# call volume_monitor_get_drive_by_id #} (castToVolumeMonitor volumeMonitor) (fromIntegral id) >>=
        newDrive

volumeMonitorGetMountedVolumes :: VolumeMonitorClass volumeMonitor =>
                                  volumeMonitor
                               -> IO [Volume]
volumeMonitorGetMountedVolumes volumeMonitor =
    {# call volume_monitor_get_mounted_volumes #} (castToVolumeMonitor volumeMonitor) >>=
        readGList >>= mapM newVolume

volumeMonitorGetVolumeByID :: VolumeMonitorClass volumeMonitor =>
                              volumeMonitor
                           -> Word
                           -> IO Drive
volumeMonitorGetVolumeByID volumeMonitor id =
    {# call volume_monitor_get_drive_by_id #} (castToVolumeMonitor volumeMonitor) (fromIntegral id) >>=
        newDrive

volumeMonitorGetVolumeForPath :: VolumeMonitorClass volumeMonitor =>
                                 volumeMonitor
                              -> FilePath
                              -> IO Volume
volumeMonitorGetVolumeForPath volumeMonitor path =
    (withUTFString path $ {# call volume_monitor_get_volume_for_path #} (castToVolumeMonitor volumeMonitor)) >>=
        newVolume

onVolumeMonitorDriveConnected,
    afterVolumeMonitorDriveConnected,
    onVolumeMonitorDriveDisconnected,
    afterVolumeMonitorDriveDisconnected,
    onVolumeMonitorVolumeMounted,
    afterVolumeMonitorVolumeMounted,
    onVolumeMonitorVolumePreUnmount,
    afterVolumeMonitorVolumePreUnmount,
    onVolumeMonitorVolumeUnmounted,
    afterVolumeMonitorVolumeUnmounted
    :: (VolumeMonitorClass drive, VolumeClass volume) =>
       drive
    -> (volume -> IO ())
    -> IO (ConnectId drive)

onVolumeMonitorDriveConnected       = connect_OBJECT__NONE "drive-connected" False
afterVolumeMonitorDriveConnected    = connect_OBJECT__NONE "drive-connected" True

onVolumeMonitorDriveDisconnected    = connect_OBJECT__NONE "drive-disconnected" False
afterVolumeMonitorDriveDisconnected = connect_OBJECT__NONE "drive-disconnected" True

onVolumeMonitorVolumeMounted        = connect_OBJECT__NONE "volume-mounted" False
afterVolumeMonitorVolumeMounted     = connect_OBJECT__NONE "volume-mounted" True

onVolumeMonitorVolumePreUnmount     = connect_OBJECT__NONE "volume-pre-unmount" False
afterVolumeMonitorVolumePreUnmount  = connect_OBJECT__NONE "volume-pre-unmount" True

onVolumeMonitorVolumeUnmounted      = connect_OBJECT__NONE "volume-unmounted" False
afterVolumeMonitorVolumeUnmounted   = connect_OBJECT__NONE "volume-unmounted" True
