{-# LANGUAGE CPP #-}
--  GIMP Toolkit (GTK) Binding for Haskell: binding to gio -*-haskell-*-
--
--  Author : Andy Stewart
--  Created: 30-Apirl-2010
--
--  Copyright (c) 2010 Andy Stewart
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
--  GIO, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GIO documentation.
--
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.GIO.Volumes.VolumeMonitor (
-- * Details
--
-- | 'VolumeMonitor' is for listing the user interesting devices and volumes on the computer. In other
-- words, what a file selector or file manager would show in a sidebar.
--
-- 'VolumeMonitor' is not thread-default-context aware, and so should not be used other than from the
-- main thread, with no thread-default-context active.

-- * Types
    VolumeMonitor(..),
    VolumeMonitorClass,

-- * Methods
    volumeMonitorGet,
    volumeMonitorGetConnectedDrives,
    volumeMonitorGetVolumes,
    volumeMonitorGetMounts,
    volumeMonitorGetMountForUUID,
    volumeMonitorGetVolumeForUUID,

-- * Signals
    vmDriveChanged,
    vmDriveConnected,
    vmDriveDisconnected,
#if GLIB_CHECK_VERSION(2,18,0)
    vmDriveEjectButton,
#endif
#if GLIB_CHECK_VERSION(2,22,0)
    vmDriveStopButton,
#endif
    vmMountAdded,
    vmMountChanged,
    vmMountPreUnmount,
    vmMountRemoved,
    vmVolumeAdded,
    vmVolumeChanged,
    vmVolumeRemoved,
    ) where

import Control.Monad
import System.GIO.Enums
import System.Glib.Attributes
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.GList
import System.Glib.GObject
import System.Glib.Properties
import System.Glib.Signals
import System.Glib.UTFString
{#import System.GIO.Signals#}
{#import System.GIO.Types#}

{# context lib = "gio" prefix = "g" #}

--------------------
-- Methods
-- | Gets the volume monitor used by gio.
volumeMonitorGet :: IO VolumeMonitor
volumeMonitorGet =
  wrapNewGObject mkVolumeMonitor $
  {#call g_volume_monitor_get #}

-- | Gets a list of drives connected to the system.
volumeMonitorGetConnectedDrives :: VolumeMonitorClass monitor => monitor
 -> IO [Drive]
volumeMonitorGetConnectedDrives monitor = do
  glistPtr <- {#call g_volume_monitor_get_connected_drives #} (toVolumeMonitor monitor)
  drivePtrs <- fromGList glistPtr
  mapM (wrapNewGObject mkDrive . return) drivePtrs

-- | Gets a list of the volumes on the system.
volumeMonitorGetVolumes :: VolumeMonitorClass monitor => monitor
 -> IO [Drive]
volumeMonitorGetVolumes monitor = do
  glistPtr <- {#call g_volume_monitor_get_volumes #} (toVolumeMonitor monitor)
  volumePtrs <- fromGList glistPtr
  mapM (wrapNewGObject mkDrive . return) volumePtrs

-- | Gets a list of the mounts on the system.
volumeMonitorGetMounts :: VolumeMonitorClass monitor => monitor
 -> IO [Drive]
volumeMonitorGetMounts monitor = do
  glistPtr <- {#call g_volume_monitor_get_mounts #} (toVolumeMonitor monitor)
  mountPtrs <- fromGList glistPtr
  mapM (wrapNewGObject mkDrive . return) mountPtrs

-- | Finds a 'Mount' object by its UUID (see 'mountGetUuid'
volumeMonitorGetMountForUUID :: (VolumeMonitorClass monitor, GlibString string) => monitor
 -> string -- ^ @uuid@           the UUID to look for
 -> IO (Maybe Mount)               -- ^ returns        a 'Mount' or 'Nothing' if no such mount is available.
volumeMonitorGetMountForUUID monitor uuid =
  maybeNull (wrapNewGObject mkMount) $
  withUTFString uuid $ \ uuidPtr ->
  {#call g_volume_monitor_get_mount_for_uuid#} (toVolumeMonitor monitor) uuidPtr

-- | Finds a 'Volume' object by its UUID (see 'volumeGetUuid')
volumeMonitorGetVolumeForUUID :: (VolumeMonitorClass monitor, GlibString string) => monitor
 -> string -- ^ @uuid@           the UUID to look for
 -> IO (Maybe Volume)               -- ^ returns        a 'Volume' or 'Nothing' if no such volume is available.
volumeMonitorGetVolumeForUUID monitor uuid =
  maybeNull (wrapNewGObject mkVolume) $
  withUTFString uuid $ \ uuidPtr ->
  {#call g_volume_monitor_get_volume_for_uuid#} (toVolumeMonitor monitor) uuidPtr

--------------------
-- Signals
-- | Emitted when a drive changes.
vmDriveChanged :: VolumeMonitorClass monitor => Signal monitor (Drive -> IO ())
vmDriveChanged = Signal (connect_OBJECT__NONE "drive-changed")

-- | Emitted when a drive changes.
vmDriveConnected :: VolumeMonitorClass monitor => Signal monitor (Drive -> IO ())
vmDriveConnected = Signal (connect_OBJECT__NONE "drive-connected")

-- | Emitted when a drive changes.
vmDriveDisconnected :: VolumeMonitorClass monitor => Signal monitor (Drive -> IO ())
vmDriveDisconnected = Signal (connect_OBJECT__NONE "drive-disconnected")

#if GLIB_CHECK_VERSION(2,18,0)
-- | Emitted when the eject button is pressed on drive.
vmDriveEjectButton :: VolumeMonitorClass monitor => Signal monitor (Drive -> IO ())
vmDriveEjectButton = Signal (connect_OBJECT__NONE "drive-eject-button")
#endif

#if GLIB_CHECK_VERSION(2,22,0)
-- | Emitted when the stop button is pressed on drive.
vmDriveStopButton :: VolumeMonitorClass monitor => Signal monitor (Drive -> IO ())
vmDriveStopButton = Signal (connect_OBJECT__NONE "drive-stop-button")
#endif

-- | Emitted when a mount is added.
vmMountAdded :: VolumeMonitorClass monitor => Signal monitor (Mount -> IO ())
vmMountAdded = Signal (connect_OBJECT__NONE "mount-added")

-- | Emitted when a mount is changed.
vmMountChanged :: VolumeMonitorClass monitor => Signal monitor (Mount -> IO ())
vmMountChanged = Signal (connect_OBJECT__NONE "mount-changed")

-- | Emitted when a mount is about to be removed.
vmMountPreUnmount :: VolumeMonitorClass monitor => Signal monitor (Mount -> IO ())
vmMountPreUnmount = Signal (connect_OBJECT__NONE "mount-pre-unmount")

-- | Emitted when a mount is removed.
vmMountRemoved :: VolumeMonitorClass monitor => Signal monitor (Mount -> IO ())
vmMountRemoved = Signal (connect_OBJECT__NONE "mount-removed")

-- | Emitted when a volume is added.
vmVolumeAdded :: VolumeMonitorClass monitor => Signal monitor (Volume -> IO ())
vmVolumeAdded = Signal (connect_OBJECT__NONE "volume-added")

-- | Emitted when a volume is changed.
vmVolumeChanged :: VolumeMonitorClass monitor => Signal monitor (Volume -> IO ())
vmVolumeChanged = Signal (connect_OBJECT__NONE "volume-changed")

-- | Emitted when a volume is removed.
vmVolumeRemoved :: VolumeMonitorClass monitor => Signal monitor (Volume -> IO ())
vmVolumeRemoved = Signal (connect_OBJECT__NONE "volume-removed")

