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
module System.Gnome.VFS.Drive (

-- * Types
  -- | A container for 'Volume's.
  Drive,
  DriveClass,
  DriveID,
  DeviceType,

-- * Type Conversion
  castToDrive,

-- * Drive Comparison
  driveCompare,

-- * Drive Properties
  driveGetActivationURI,
  driveGetDevicePath,
  driveGetDeviceType,
  driveGetDisplayName,
  driveGetHalUDI,
  driveGetIcon,
  driveGetID,

-- * Drive State
  driveIsConnected,
  driveIsMounted,
  driveIsUserVisible,
  driveGetMountedVolumes,

-- * Drive Operations
  driveEject,
  driveMount,
  
-- * Drive Signals
  onDriveVolumeMounted,
  afterDriveVolumeMounted,
  onDriveVolumePreUnmount,
  afterDriveVolumePreUnmount,
  onDriveVolumeUnmounted,
  afterDriveVolumeUnmounted
    
  ) where

import Control.Exception
import Control.Monad                ( liftM )
import System.Glib.UTFString
import System.Glib.FFI
import System.Glib.GList            ( fromGList )
{#import System.Glib.Signals#}
{#import System.Gnome.VFS.Marshal#}
{#import System.Gnome.VFS.Types#}
{#import System.Gnome.VFS.Signals#}

{# context lib = "gnomevfs" prefix = "gnome_vfs" #}

-- | Compares two 'DriveClass' objects @a@ and @b@. Two 'DriveClass'
--   objects referring to different drives are guaranteed to not
--   return 'EQ' when comparing them. If they refer to the same drive 'EQ'
--   is returned.
--   
--   The resulting gint should be used to determine the order in which
--   @a@ and @b@ are displayed in graphical user interfaces.
--   
--   The comparison algorithm first of all peeks the device type of
--   @a@ and @b@, they will be sorted in the following order:
--   
--     * Magnetic and opto-magnetic drives (ZIP, floppy)
--   
--     * Optical drives (CD, DVD)
--   
--     * External drives (USB sticks, music players)
--   
--     * Mounted hard disks
--   
--     * Other drives
--   
--   Afterwards, the display name of @a@ and @b@ is compared using a
--   locale-sensitive sorting algorithm.
--   
--   If two drives have the same display name, their unique ID is
--   compared which can be queried using 'driveGetID'.
driveCompare :: DriveClass drive =>
                drive       -- ^ @a@ - the first drive
             -> drive       -- ^ @b@ - the second drive
             -> IO Ordering -- ^ the ordering relationship between the drives
driveCompare a b =
    do result <- liftM fromIntegral $ {# call drive_compare #} (castToDrive a) (castToDrive b)
       let ordering | result < 0 = LT
                    | result > 0 = GT
                    | otherwise  = EQ
       return ordering

-- | If drive has associated 'Volume' objects, all of them will
--   be unmounted by calling 'volumeUnmount' for each volume in
--   'driveGetMountedVolumes', except for the last one, for which
--   'volumeEject' is called to ensure that the drive's media is
--   ejected.
driveEject :: DriveClass drive =>
              drive                   -- ^ @drive@ - the drive to be ejected
           -> VolumeOpSuccessCallback -- ^ @successCallback@ - the
                                      --   action to be performed on
                                      --   successful ejection
           -> VolumeOpFailureCallback -- ^ @failureCallback@ - the
                                      --   action to be performed on
                                      --   failure
           -> IO ()
driveEject drive successCallback failureCallback =
    do cCallback <- volumeOpCallbackMarshal successCallback failureCallback
       {# call drive_eject #} (castToDrive drive) cCallback $ castFunPtrToPtr cCallback

marshalString cAction drive =
    cAction (castToDrive drive) >>= readUTFString
marshalMaybeString cAction drive =
    cAction (castToDrive drive) >>= (maybePeek readUTFString)

-- | Returns the activation URI of @drive@.
--   
--   The returned URI usually refers to a valid location. You can
--   check the validity of the location by calling 'uriFromString'
--   with the URI, and checking whether the return value is not
--   'Nothing'.
driveGetActivationURI :: DriveClass drive
                      => drive      -- ^ @drive@ - the drive object to query
                      -> IO String  -- ^ the drive's activation URI
driveGetActivationURI =
    marshalString {# call drive_get_activation_uri #}

-- | Returns the device path of a 'Drive' object.
--   
--   For HAL drives, this returns the value of the drive's
--   @block.device@ key. For UNIX mounts, it returns the @mntent@'s
--   @mnt_fsname@ entry.
--   
--   Otherwise, it returns 'Nothing'.
driveGetDevicePath :: DriveClass drive =>
                      drive             -- ^ @drive@ - the drive object to query
                   -> IO (Maybe String) -- ^ the drive's device path
driveGetDevicePath =
    marshalMaybeString {# call drive_get_device_path #}

-- | Returns the 'DeviceType' of a 'Drive' object.
driveGetDeviceType :: DriveClass drive =>
                      drive         -- ^ @drive@ - the drive object to query
                   -> IO DeviceType -- ^ the drive's device type
driveGetDeviceType drive =
    liftM cToEnum $ {# call drive_get_device_type #} (castToDrive drive)

-- | Returns the display name of a 'Drive' object.
driveGetDisplayName :: DriveClass drive =>
                       drive     -- ^ @drive@ - the drive object to query
                    -> IO String -- ^ the drive's display name
driveGetDisplayName =
    marshalString {# call drive_get_display_name #}

-- | Returns the HAL UDI of a 'Drive' object.
--   
--   For HAL drives, this matches the value of the @info.udi@ key,
--   for other drives it is 'Nothing'.
driveGetHalUDI :: DriveClass drive =>
                  drive             -- ^ @drive@ - the drive object to query
               -> IO (Maybe String) -- ^ the drive's HAL UDI
driveGetHalUDI =
    marshalMaybeString {# call drive_get_hal_udi #}

-- | Returns the icon filename for a 'Drive' object.
driveGetIcon :: DriveClass drive =>
                drive       -- ^ @drive@ - a drive object
             -> IO FilePath -- ^ the icon that should be used for this drive
driveGetIcon =
    marshalString {# call drive_get_icon #}

-- | Returns a unique identifier for a 'Drive' object.
driveGetID :: DriveClass drive =>
              drive      -- ^ @drive@ - a drive object
           -> IO DriveID -- ^ a unique identifier for the drive
driveGetID drive =
    {# call drive_get_id #} (castToDrive drive)

-- | Returns a list of mounted volumes for a 'Drive' object.
driveGetMountedVolumes :: DriveClass drive =>
                          drive       -- ^ @drive@ - a drive object
                       -> IO [Volume] -- ^ the 'Volume's currently
                                      --   mounted on the drive
driveGetMountedVolumes drive =
    {# call drive_get_mounted_volumes #} (castToDrive drive) >>=
        fromGList >>=
        mapM newVolume

marshalBool cAction drive =
    liftM toBool $ cAction (castToDrive drive)

-- | Returns a 'Bool' for whether a drive is connected.
driveIsConnected :: DriveClass drive =>
                    drive   -- ^ @drive@ - a drive object
                 -> IO Bool -- ^ 'True' if the drive is connected,
                            --   'False' otherwise
driveIsConnected =
    marshalBool {# call drive_is_connected #}

-- | Returns a 'Bool' for whether a drive is mounted.
driveIsMounted :: DriveClass drive =>
                  drive   -- ^ @drive@ - a drive object
               -> IO Bool -- ^ 'True' if the drive is mounted,
                          --   'False' otherwise
driveIsMounted =
    marshalBool {# call drive_is_mounted #}

-- | Returns a 'Bool' for whether a drive is user-visible. This should
--   be used by applications to determine whether the drive should be
--   listed in user interfaces listing available drives.
driveIsUserVisible :: DriveClass drive =>
                      drive   -- ^ @drive@ - a drive object
                   -> IO Bool -- ^ 'True' if the drive is
                              --   user-visible, 'False' otherwise
driveIsUserVisible =
    marshalBool {# call drive_is_user_visible #}

-- | Mounts a 'Drive' object.
driveMount :: DriveClass drive =>
              drive                   -- ^ @drive@ - a drive object
           -> VolumeOpSuccessCallback -- ^ @successCallback@ - the
                                      --   action to be performed on
                                      --   successful mount
           -> VolumeOpFailureCallback -- ^ @failureCallback@ - the
                                      --   action to be performed on
                                      --   failure
           -> IO ()
driveMount drive successCallback failureCallback =
    do cCallback <- volumeOpCallbackMarshal successCallback failureCallback
       {# call drive_eject #} (castToDrive drive) cCallback $ castFunPtrToPtr cCallback

onDriveVolumeMounted,
    afterDriveVolumeMounted,
    onDriveVolumePreUnmount,
    afterDriveVolumePreUnmount,
    onDriveVolumeUnmounted,
    afterDriveVolumeUnmounted
    :: (DriveClass drive) =>
       drive                -- ^ @drive@ - the drive to connect the signal handler to
    -> (Volume -> IO ())    -- ^ @handler@ - the signal handling function
    -> IO (ConnectId drive) -- ^ the identifier for the connection

onDriveVolumeMounted       = connect_OBJECT__NONE "volume-mounted" False
afterDriveVolumeMounted    = connect_OBJECT__NONE "volume-mounted" True

onDriveVolumePreUnmount    = connect_OBJECT__NONE "volume-pre-unmount" False
afterDriveVolumePreUnmount = connect_OBJECT__NONE "volume-pre-unmount" True

onDriveVolumeUnmounted     = connect_OBJECT__NONE "volume-unmounted" False
afterDriveVolumeUnmounted  = connect_OBJECT__NONE "volume-unmounted" True
