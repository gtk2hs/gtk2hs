{-# LANGUAGE CPP #-}
--  GIMP Toolkit (GTK) Binding for Haskell: binding to libgnomevfs -*-haskell-*-
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
--  GnomeVFS, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GnomeVFS documentation,
--  Copyright (c) 2001 Seth Nickell <snickell@stanford.edu>. The
--  documentation is covered by the GNU Free Documentation License,
--  version 1.2.
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
{#import System.Gnome.VFS.BasicTypes#}
{#import System.Gnome.VFS.Signals#}
import System.IO (FilePath)

{# context lib = "gnomevfs" prefix = "gnome_vfs" #}

-- | The global volume monitor object.
volumeMonitor :: VolumeMonitor
volumeMonitor = unsafePerformIO $ {# call get_volume_monitor #} >>= wrapVolumeMonitor

-- | Returns a list of all drives connected to the machine.
volumeMonitorGetConnectedDrives :: VolumeMonitorClass volumeMonitor =>
                                   volumeMonitor -- ^ @volumeMonitor@ - the volume monitor
                                -> IO [Drive]    -- ^ the drives connected to the machine
volumeMonitorGetConnectedDrives volumeMonitor =
    {# call volume_monitor_get_connected_drives #} (castToVolumeMonitor volumeMonitor) >>=
        readGList >>= mapM newDrive

-- | Try to find the 'Drive' with ID @id@.
volumeMonitorGetDriveByID :: VolumeMonitorClass volumeMonitor =>
                             volumeMonitor    -- ^ @volumeMonitor@ - the volume monitor
                          -> DriveID          -- ^ @id@ - the drive ID
                          -> IO (Maybe Drive) -- ^ the requested
                                              --   drive, or 'Nothing'
                                              --   if no drive with
                                              --   that ID could be
                                              --   found
volumeMonitorGetDriveByID volumeMonitor id =
    {# call volume_monitor_get_drive_by_id #} (castToVolumeMonitor volumeMonitor) id >>=
        maybePeek newDrive

-- | Returns a list of all volumes currently mounted on the machine.
volumeMonitorGetMountedVolumes :: VolumeMonitorClass volumeMonitor =>
                                  volumeMonitor -- ^ @volumeMonitor@ - the volume monitor
                               -> IO [Volume]   -- ^ the volumes
                                                --   currently mounted
                                                --   on the machine
volumeMonitorGetMountedVolumes volumeMonitor =
    {# call volume_monitor_get_mounted_volumes #} (castToVolumeMonitor volumeMonitor) >>=
        readGList >>= mapM newVolume

-- | Try to find the 'Volume' with ID @id@.
volumeMonitorGetVolumeByID :: VolumeMonitorClass volumeMonitor =>
                              volumeMonitor     -- ^ @volumeMonitor@ - the volume monitor
                           -> VolumeID          -- ^ @id@ - the volume ID
                           -> IO (Maybe Volume) -- ^ the requested
                                                --   volume, or
                                                --   'Nothing' if no
                                                --   volume with that
                                                --   ID could be found
volumeMonitorGetVolumeByID volumeMonitor id =
    {# call volume_monitor_get_volume_by_id #} (castToVolumeMonitor volumeMonitor) id >>=
        maybePeek newVolume

-- | Returns the 'Volume' corresponding to path, or 'Nothing'.
--   
--   The volume referring to path is found by calling @stat@ on path,
--   and then iterating through the list of volumes that refer to
--   currently mounted local file systems. The first volume in this
--   list maching the path's UNIX device is returned.
--   
--   If the @stat@ on path was not successful, or no volume matches
--   path, 'Nothing' is returned.
volumeMonitorGetVolumeForPath :: VolumeMonitorClass volumeMonitor =>
                                 volumeMonitor     -- ^ @volumeMonitor@ - the volume monitor
                              -> FilePath          -- ^ the path to
                                                   --   find the volume
                                                   --   for
                              -> IO (Maybe Volume) -- ^ the volume the
                                                   --   path resides
                                                   --   on, or
                                                   --   'Nothing' if
                                                   --   the volume
                                                   --   could not be
                                                   --   determined
volumeMonitorGetVolumeForPath volumeMonitor path =
    (withUTFString path $ {# call volume_monitor_get_volume_for_path #} (castToVolumeMonitor volumeMonitor)) >>=
        maybePeek newVolume

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
    :: (VolumeMonitorClass volumeMonitor) =>
       volumeMonitor                -- ^ @volumeMonitor@ - the volume monitor
    -> (Volume -> IO ())            -- ^ @handler@ - the signal handling function
    -> IO (ConnectId volumeMonitor) -- ^ the identifier for the connection

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
