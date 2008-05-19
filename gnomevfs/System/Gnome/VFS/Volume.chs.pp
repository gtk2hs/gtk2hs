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
module System.Gnome.VFS.Volume (
  
-- * Types
  -- | An abstraction for a mounted filesystem or network location.
  Volume,
  VolumeClass,
  VolumeID,
  -- | Safely cast an object to a 'Volume'.
  castToVolume,
  
-- * Volume Operations
  volumeCompare,
  volumeEject,
  volumeGetActivationURI,
  volumeGetDevicePath,
  volumeGetDeviceType,
  volumeGetDisplayName,
  volumeGetDrive,
  volumeGetFilesystemType,
#if GNOME_VFS_CHECK_VERSION(2,8,0)
  volumeGetHalUDI,
#endif
  volumeGetIcon,
  volumeGetID,
  volumeGetVolumeType,
  volumeHandlesTrash,
  volumeIsMounted,
  volumeIsReadOnly,
  volumeIsUserVisible,
  volumeUnmount
  
  ) where

import Control.Exception
import Control.Monad (liftM)
import System.Glib.UTFString
import System.Glib.FFI
{#import System.Gnome.VFS.Marshal#}
{#import System.Gnome.VFS.Types#}

{# context lib = "gnomevfs" prefix = "gnome_vfs" #}

-- | Compares two 'Volume' objects @a@ and @b@. Two 'Volume'
--   objects referring to different volumes are guaranteed to not
--   return 'EQ' when comparing them. If they refer to the same volume 'EQ'
--   is returned.
--   
--   The resulting gint should be used to determine the order in which
--   @a@ and @b@ are displayed in graphical user interfaces.
--   
--   The comparison algorithm first of all peeks the device type of
--   @a@ and @b@, they will be sorted in the following order:
--   
--     * Magnetic and opto-magnetic volumes (ZIP, floppy)
--   
--     * Optical volumes (CD, DVD)
--   
--     * External volumes (USB sticks, music players)
--   
--     * Mounted hard disks
--   
--     * Network mounts
--   
--     * Other volumes
--   
--   Afterwards, the display name of @a@ and @b@ is compared using a
--   locale-sensitive sorting algorithm.
--   
--   If two volumes have the same display name, their unique ID is
--   compared which can be queried using 'volumeGetID'.
volumeCompare :: (VolumeClass volume1, VolumeClass volume2)
              => volume1
              -> volume2
              -> IO Ordering
volumeCompare a b =
    do result <- liftM fromIntegral $ {# call volume_compare #} (castToVolume a) (castToVolume b)
       let ordering | result < 0 = LT
                    | result > 0 = GT
                    | otherwise  = EQ
       return ordering

-- Requests ejection of a 'Volume'.
-- 
-- Before the unmount operation is executed, the
-- 'Volume' object's @pre-unmount@ signal is emitted.
-- 
-- If the volume is a mount point, i.e. its type is
-- 'VolumeTypeMountpoint', it is unmounted, and if it refers to a
-- disk, it is also ejected.
-- 
-- If the volume is a special VFS mount, i.e. its type is
-- 'VolumeTypeMount', it is ejected.
-- 
-- If the volume is a connected server, it is removed from the list of
-- connected servers.
-- 
-- Otherwise, no further action is done.
volumeEject   :: VolumeClass volume
              => volume                  -- ^ @volume@ - the volume to eject
              -> VolumeOpSuccessCallback -- ^ @successCallback@ - the
                                         --   callback to call once
                                         --   the operation has
                                         --   completed successfully
              -> VolumeOpFailureCallback -- ^ @failureCallback@ - the
                                         --   callback to call if the
                                         --   operation fails
              -> IO ()
volumeEject volume successCallback failureCallback =
    do cCallback <- volumeOpCallbackMarshal successCallback failureCallback
       {# call volume_eject #} (castToVolume volume) cCallback $ castFunPtrToPtr cCallback

marshalString cAction volume =
    cAction (castToVolume volume) >>= readUTFString
marshalMaybeString cAction volume =
    cAction (castToVolume volume) >>= maybePeek readUTFString

-- | Returns the activation URI of @volume@.
--   
--   The returned URI usually refers to a valid location. You can
--   check the validity of the location by calling
--   'System.Gnome.VFS.URI.uriFromString' with the URI, and checking
--   whether the return value is not 'Nothing'.
volumeGetActivationURI  :: VolumeClass volume
                        => volume     -- ^ @volume@ - the volume to query
                        -> IO TextURI -- ^ the volume's activation URI.
volumeGetActivationURI =
    marshalString {# call volume_get_activation_uri #}

-- | Returns the device path of a 'Volume' object.
--   
--   For HAL volumes, this returns the value of the volume's
--   @block.device@ key. For UNIX mounts, it returns the @mntent@'s
--   @mnt_fsname@ entry.
--   
--   Otherwise, it returns 'Nothing'.
volumeGetDevicePath     :: VolumeClass volume =>
                           volume    -- ^ @volume@ - the volume object to query
                        -> IO String -- ^ the volume's device path
volumeGetDevicePath =
    marshalString {# call volume_get_device_path #}

-- | Returns the 'DeviceType' of a 'Volume' object.
volumeGetDeviceType     :: VolumeClass volume =>
                           volume        -- ^ @volume@ - the volume object to query
                        -> IO DeviceType -- the volume's device type
volumeGetDeviceType volume =
    liftM cToEnum $ {# call volume_get_device_type #} (castToVolume volume)

-- | Returns the display name of a 'Volume' object.
volumeGetDisplayName    :: VolumeClass volume =>
                           volume    -- ^ @volume@ - the volume object to query
                        -> IO String -- ^ the volume's display name
volumeGetDisplayName =
    marshalString {# call volume_get_display_name #}

-- | Returns the 'Drive' that @volume@ is on.
volumeGetDrive          :: VolumeClass volume =>
                           volume   -- ^ @volume@ - the volume object to query
                        -> IO Drive -- ^ the containing drive
volumeGetDrive volume =
    {# call volume_get_drive #} (castToVolume volume) >>= newDrive

-- | Returns a string describing the file system on @volume@, or
--   'Nothing' if no information on the underlying file system is
--   available.
--   
--   The file system may be used to provide special functionality that
--   depends on the file system type, for instance to determine
--   whether trashing is supported (cf. 'volumeHandlesTrash').
--   
--   For HAL mounts, this returns the value of the @\"volume.fstype\"@
--   key, for traditional UNIX mounts it is set to the mntent's
--   mnt_type key, for connected servers, 'Nothing' is returned.
volumeGetFilesystemType :: VolumeClass volume =>
                           volume            -- ^ @volume@ - the
                                             -- volume object to query
                        -> IO (Maybe String) -- ^ a string describing
                                             -- the filesystem type,
                                             -- or 'Nothing' if no
                                             -- information is
                                             -- available
volumeGetFilesystemType =
    marshalMaybeString {# call volume_get_filesystem_type #}

#if GNOME_VFS_CHECK_VERSION(2,8,0)
-- | Returns the HAL UDI of a 'Volume' object.
--   
--   For HAL volumes, this matches the value of the @info.udi@ key,
--   for other volumes it is 'Nothing'.
volumeGetHalUDI :: VolumeClass volume =>
                   volume            -- ^ @volume@ - the volume object to query
                -> IO (Maybe String) -- ^ the volume's HAL UDI
volumeGetHalUDI =
    marshalMaybeString {# call volume_get_hal_udi #}
#endif

-- | Returns the icon filename for a 'Volume' object.
volumeGetIcon :: VolumeClass volume =>
                 volume      -- ^ @volume@ - a volume object
              -> IO FilePath -- ^ the icon that should be used for this volume
volumeGetIcon =
    marshalString {# call volume_get_icon #}

-- | Returns a unique identifier for a 'Volume' object.
volumeGetID :: VolumeClass volume =>
               volume      -- ^ @volume@ - a volume object
            -> IO VolumeID -- ^ a unique identifier for the volume
volumeGetID volume =
    {# call volume_get_id #} (castToVolume volume)

-- | Returns the volume type of @volume@.
volumeGetVolumeType     :: VolumeClass volume =>
                           volume        -- ^ @volume@ - the volume object to query
                        -> IO VolumeType -- ^ the volume's volume type
volumeGetVolumeType volume =
    liftM cToEnum $ {# call volume_get_volume_type #} (castToVolume volume)

marshalBool cAction volume =
    liftM toBool $ cAction (castToVolume volume)

-- | Returns whether the file system on a volume supports trashing of
--   files.
--   
--   If the volume has an AutoFS file system (i.e.,
--   'volumeGetDeviceType' returns 'DeviceTypeAutofs'), or if the
--   volume is mounted read-only (i.e., 'volumeIsReadOnly' returns
--   'True'), it is assumed to not support trashing of files.
--   
--   Otherwise, if the volume provides file system information, it is
--   determined whether the file system supports trashing of
--   files.
volumeHandlesTrash      :: VolumeClass volume =>
                           volume  -- ^ @volume@ - 
                        -> IO Bool -- ^ 'True' if the volume handles trash, otherwise 'False'
volumeHandlesTrash =
    marshalBool {# call volume_handles_trash #}

-- | Returns whether the file system on a volume is currently mounted.
--   
--   For HAL volumes, this reflects the value of the
--   @\"volume.is_mounted\"@ key, for traditional UNIX mounts and
--   connected servers, 'True' is returned, because their existence
--   implies that they are mounted.
volumeIsMounted         :: VolumeClass volume =>
                           volume  -- ^ @volume@ - 
                        -> IO Bool -- ^ 'True' if the volume is mounted, otherwise 'False'
volumeIsMounted =
    marshalBool {# call volume_is_mounted #}

-- | Returns whether the file system on a volume is read-only.
--   
--   For HAL volumes, the @\"volume.is_mounted_read_only\"@ key is
--   authoritative, for traditional UNIX mounts it returns TRUE if the
--   mount was done with the @\"ro\"@ option. For servers, 'False' is
--   returned.
volumeIsReadOnly        :: VolumeClass volume =>
                           volume  -- ^ @volume@ - 
                        -> IO Bool -- ^ 'True' if the volume is read-only, otherwise 'False'
volumeIsReadOnly =
    marshalBool {# call volume_is_read_only #}

-- | Returns a 'Bool' for whether a volume is user-visible. This should
--   be used by applications to determine whether the volume should be
--   listed in user interfaces listing available volumes.
volumeIsUserVisible     :: VolumeClass volume =>
                           volume  -- @volume@ - 
                        -> IO Bool -- ^ 'True' if the volume is user visible, otherwise 'False'
volumeIsUserVisible =
    marshalBool {# call volume_is_user_visible #}

-- Requests unmount of a 'Volume'.
-- 
-- Note that 'volumeUnmount' may also unvoke 'volumeEject', if
-- @volume@ signals that it should be ejected when it is unmounted.
-- This may be true for CD-ROMs, USB sticks, and other devices,
-- depending on the backend providing the volume.
volumeUnmount :: VolumeClass volume
              => volume                  -- ^ @volume@ - the volume to eject
              -> VolumeOpSuccessCallback -- ^ @successCallback@ - the
                                         --   callback to call once
                                         --   the operation has
                                         --   completed successfully
              -> VolumeOpFailureCallback -- ^ @failureCallback@ - the
                                         --   callback to call if the
                                         --   operation fails
              -> IO ()
volumeUnmount volume successCallback failureCallback =
    do cCallback <- volumeOpCallbackMarshal successCallback failureCallback
       {# call volume_unmount #} (castToVolume volume) cCallback $ castFunPtrToPtr cCallback
