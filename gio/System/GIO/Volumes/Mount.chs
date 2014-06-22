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
module System.GIO.Volumes.Mount (
-- * Details
--
-- | The 'Mount' interface represents user-visible mounts. Note, when porting from GnomeVFS, 'Mount' is the
-- moral equivalent of GnomeVFSVolume.
--
-- 'Mount' is a "mounted" filesystem that you can access. Mounted is in quotes because it's not the same
-- as a unix mount, it might be a gvfs mount, but you can still access the files on it if you use
-- GIO. Might or might not be related to a volume object.
--
-- Unmounting a 'Mount' instance is an asynchronous operation. For more information about asynchronous
-- operations, see 'AsyncReady' and GSimpleAsyncReady. To unmount a 'Mount' instance, first call
-- 'mountUnmountWithOperation' the 'Mount' instance and a 'AsyncReadyCallback'. The
-- callback will be fired when the operation has resolved (either with success or failure), and a
-- 'AsyncReady' structure will be passed to the callback. That callback should then call
-- 'mountUnmountWithOperationFinish' with the 'Mount' and the 'AsyncReady' data to see if the
-- operation was completed successfully. If an error is present when
-- 'mountUnmountWithOperationFinish' is called, then it will be filled with any error
-- information.

-- * Types
    Mount(..),
    MountClass,

-- * Methods
    mountGetName,
    mountGetUUID,
    mountGetIcon,
    mountGetDrive,
    mountGetRoot,
    mountGetVolume,
#if GLIB_CHECK_VERSION(2,24,0)
    mountGetDefaultLocation,
#endif
    mountCanUnmount,
#if GLIB_CHECK_VERSION(2,22,0)
    mountUnmountWithOperation,
    mountUnmountWithOperationFinish,
#endif
    mountRemount,
    mountRemountFinish,
    mountCanEject,
#if GLIB_CHECK_VERSION(2,22,0)
    mountEjectWithOperation,
    mountEjectWithOperationFinish,
#endif
#if GLIB_CHECK_VERSION(2,18,0)
    mountGuessContentType,
    mountGuessContentTypeFinish,
    mountGuessContentTypeSync,
#endif
#if GLIB_CHECK_VERSION(2,20,0)
    mountIsShadowed,
    mountShadow,
    mountUnshadow,
#endif

-- * Signals
    mountChanged,
#if GLIB_CHECK_VERSION(2,22,0)
    mountPreUnmount,
#endif
    mountUnmounted,
    ) where

import Control.Monad
import Data.Maybe (fromMaybe)
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
{#import System.GIO.Async.AsyncResult#}
{#import System.GIO.Signals#}
{#import System.GIO.Types#}

{# context lib = "gio" prefix = "g" #}

--------------------
-- Methods
-- | Gets the name of mount.
mountGetName :: (MountClass mount, GlibString string) => mount
 -> IO string  -- ^ returns the name for the given mount.
mountGetName mount =
  {#call g_mount_get_name#} (toMount mount)
  >>= readUTFString

-- | Gets the UUID for the mount. The reference is typically based on the file system UUID for the mount
-- in question and should be considered an opaque string. Returns 'Nothing' if there is no UUID available.
mountGetUUID :: (MountClass mount, GlibString string) => mount
 -> IO (Maybe string)             -- ^ returns the UUID for mount or 'Nothing' if no UUID can be computed.
mountGetUUID mount = do
  {#call g_mount_get_uuid#} (toMount mount)
  >>= maybePeek readUTFString

-- | Gets the icon for mount.
mountGetIcon :: MountClass mount => mount
 -> IO Icon -- ^ returns a 'Icon'.
mountGetIcon mount =
  wrapNewGObject mkIcon $
  {#call g_mount_get_icon#} (toMount mount)

-- | Gets the drive for the mount.
--
-- This is a convenience method for getting the 'Volume' and then using that object to get the 'Drive'.
mountGetDrive :: MountClass mount => mount
 -> IO (Maybe Drive)             -- ^ returns the 'Drive' for mount or 'Nothing' if no 'Drive' can be computed.
mountGetDrive mount =
  maybeNull (wrapNewGObject mkDrive) $
  {#call g_mount_get_drive#} (toMount mount)

-- | Gets the root directory on mount.
mountGetRoot :: MountClass mount => mount
 -> IO File
mountGetRoot mount =
  wrapNewGObject mkFile $
  {#call g_mount_get_root#} (toMount mount)

-- | Gets the volume directory on mount.
mountGetVolume :: MountClass mount => mount
 -> IO (Maybe Volume)        -- ^ returns a 'Volume' or 'Nothing' if mount is not associated with a volume.
mountGetVolume mount =
  maybeNull (wrapNewGObject mkVolume) $
  {#call g_mount_get_volume#} (toMount mount)

#if GLIB_CHECK_VERSION(2,24,0)
-- | Gets the default location of mount. The default location of the given mount is a path that reflects
-- the main entry point for the user (e.g. the home directory, or the root of the volume).
-- | Gets the root directory on mount.
mountGetDefaultLocation :: MountClass mount => mount
 -> IO File
mountGetDefaultLocation mount =
  wrapNewGObject mkFile $
  {#call g_mount_get_default_location#} (toMount mount)
#endif

-- | Checks if mount can be mounted.
mountCanUnmount :: MountClass mount => mount
 -> IO Bool -- ^ returns 'True' if the mount can be unmounted.
mountCanUnmount mount =
  liftM toBool $
  {#call g_mount_can_unmount#} (toMount mount)

#if GLIB_CHECK_VERSION(2,22,0)
-- | Unmounts a mount. This is an asynchronous operation, and is finished by calling
-- 'mountUnmountWithOperationFinish' with the mount and 'AsyncResult' data returned in the
-- callback.
mountUnmountWithOperation :: MountClass mount
 => mount
 -> [MountUnmountFlags] -- ^ @flags@           flags affecting the unmount if required for eject
 -> Maybe MountOperation -- ^ @mountOperation@ a 'MountOperation' or 'Nothing' to avoid user interaction.
 -> Maybe Cancellable -- ^ @cancellable@     optional 'Cancellable' object, 'Nothing' to ignore.
 -> AsyncReadyCallback -- ^ @callback@        a 'AsyncReadyCallback'
 -> IO ()
mountUnmountWithOperation mount flags mountOperation cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_mount_unmount_with_operation #}
        (toMount mount)
        ((fromIntegral . fromFlags) flags)
        (fromMaybe (MountOperation nullForeignPtr) mountOperation)
        (fromMaybe (Cancellable nullForeignPtr) cancellable)
        cCallback
        (castFunPtrToPtr cCallback)

-- | Finishes unmounting a mount. If any errors occurred during the operation, error will be set to
-- contain the errors and 'False' will be returned.
--
-- Throws a 'GError' if an error occurs.
mountUnmountWithOperationFinish :: MountClass mount
 => mount
 -> AsyncResult -- ^ @result@  a 'AsyncResult'.
 -> IO ()
mountUnmountWithOperationFinish mount result =
    propagateGError (\gErrorPtr -> do
                       {#call g_mount_unmount_with_operation_finish #}
                          (toMount mount)
                          result
                          gErrorPtr
                       return ())
#endif

-- | Remounts a mount. This is an asynchronous operation, and is finished by calling
-- 'mountRemountFinish' with the mount and 'AsyncResult's data returned in the callback.
--
-- Remounting is useful when some setting affecting the operation of the volume has been changed, as
-- these may need a remount to take affect. While this is semantically equivalent with unmounting and
-- then remounting not all backends might need to actually be unmounted.
mountRemount :: MountClass mount
 => mount
 -> [MountMountFlags] -- ^ @flags@           flags affecting the unmount if required for eject
 -> Maybe MountOperation -- ^ @mountOperation@ a 'MountOperation' or 'Nothing' to avoid user interaction.
 -> Maybe Cancellable -- ^ @cancellable@     optional 'Cancellable' object, 'Nothing' to ignore.
 -> AsyncReadyCallback -- ^ @callback@        a 'AsyncReadyCallback'
 -> IO ()
mountRemount mount flags mountOperation cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_mount_remount #}
        (toMount mount)
        ((fromIntegral . fromFlags) flags)
        (fromMaybe (MountOperation nullForeignPtr) mountOperation)
        (fromMaybe (Cancellable nullForeignPtr) cancellable)
        cCallback
        (castFunPtrToPtr cCallback)

-- | Finishes remounting a mount. If any errors occurred during the operation, error will be set to
-- contain the errors and 'False' will be returned.
--
-- Throws a 'GError' if an error occurs.
mountRemountFinish :: MountClass mount
 => mount
 -> AsyncResult -- ^ @result@  a 'AsyncResult'.
 -> IO ()
mountRemountFinish mount result =
    propagateGError (\gErrorPtr -> do
                        {#call g_mount_remount_finish #}
                           (toMount mount)
                           result
                           gErrorPtr
                        return ())

-- | Checks if mount can be eject.
mountCanEject :: MountClass mount => mount
 -> IO Bool  -- ^ returns 'True' if the mount can be ejected.
mountCanEject mount =
  liftM toBool $
  {#call g_mount_can_eject#} (toMount mount)

#if GLIB_CHECK_VERSION(2,22,0)
-- | Ejects a mount. This is an asynchronous operation, and is finished by calling
-- 'mountEjectWithOperationFinish' with the mount and 'AsyncResult' data returned in the callback.
mountEjectWithOperation :: MountClass mount
 => mount
 -> [MountUnmountFlags] -- ^ @flags@           flags affecting the unmount if required for eject
 -> Maybe MountOperation -- ^ @mountOperation@ a 'MountOperation' or 'Nothing' to avoid user interaction.
 -> Maybe Cancellable -- ^ @cancellable@     optional 'Cancellable' object, 'Nothing' to ignore.
 -> AsyncReadyCallback -- ^ @callback@        a 'AsyncReadyCallback'
 -> IO ()
mountEjectWithOperation mount flags mountOperation cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_mount_eject_with_operation #}
        (toMount mount)
        ((fromIntegral . fromFlags) flags)
        (fromMaybe (MountOperation nullForeignPtr) mountOperation)
        (fromMaybe (Cancellable nullForeignPtr) cancellable)
        cCallback
        (castFunPtrToPtr cCallback)

-- | Finishes ejecting a mount. If any errors occurred during the operation.
--
-- Throws a 'GError' if an error occurs.
mountEjectWithOperationFinish :: MountClass mount
 => mount
 -> AsyncResult -- ^ @result@  a 'AsyncResult'.
 -> IO ()
mountEjectWithOperationFinish mount result =
    propagateGError (\gErrorPtr -> do
                        {#call g_mount_eject_with_operation_finish #}
                            (toMount mount)
                            result
                            gErrorPtr
                        return ())
#endif

#if GLIB_CHECK_VERSION(2,18,0)
-- | Tries to guess the type of content stored on mount. Returns one or more textual identifiers of
-- well-known content types (typically prefixed with \"x-content/\"), e.g. x-content/image-dcf for camera
-- memory cards. See the shared-mime-info specification for more on x-content types.
--
-- This is an asynchronous operation (see 'mountGuessContentTypeSync' for the synchronous
-- version), and is finished by calling 'mountGuessContentTypeFinish' with the mount and
-- 'AsyncResult' data returned in the callback.
mountGuessContentType :: MountClass mount => mount
 -> Bool -- ^ @forceRescan@ Whether to force a rescan of the content. Otherwise a cached result will be used if available
 -> Maybe Cancellable -- ^ @cancellable@  optional 'Cancellable' object, 'Nothing' to ignore
 -> AsyncReadyCallback -- ^ @callback@     a 'AsyncReadyCallback'
 -> IO ()
mountGuessContentType mount forceRescan cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_mount_guess_content_type #}
        (toMount mount)
        (fromBool forceRescan)
        (fromMaybe (Cancellable nullForeignPtr) cancellable)
        cCallback
        (castFunPtrToPtr cCallback)

-- | Finishes guessing content types of mount. If any errors occured during the operation, error will be
-- set to contain the errors and 'False' will be returned. In particular, you may get an
-- 'IoErrorNotSupported' if the mount does not support content guessing.
mountGuessContentTypeFinish :: (MountClass mount, GlibString string) => mount
 -> AsyncResult -- ^ @result@  a 'AsyncResult'.
 -> IO [string] -- ^ returns 'True' if the mount was successfully ejected. 'False' otherwise.
mountGuessContentTypeFinish mount result =
    propagateGError ({#call g_mount_guess_content_type_finish #} (toMount mount) result)
    >>= readUTFStringArray0

-- | Tries to guess the type of content stored on mount. Returns one or more textual identifiers of
-- well-known content types (typically prefixed with \"x-content/\"), e.g. x-content/image-dcf for camera
-- memory cards. See the shared-mime-info specification for more on x-content types.
--
-- This is an synchronous operation and as such may block doing IO; see 'mountGuessContentType'
-- for the asynchronous version.
mountGuessContentTypeSync :: (MountClass mount, GlibString string) => mount
 -> Bool -- ^ @forceRescan@ Whether to force a rescan of the content. Otherwise a cached result will be used if available
 -> Maybe Cancellable -- ^ @cancellable@  optional 'Cancellable' object, 'Nothing' to ignore
 -> IO [string]
mountGuessContentTypeSync mount forceRescan cancellable =
    propagateGError ({#call g_mount_guess_content_type_sync #}
                       (toMount mount)
                       (fromBool forceRescan)
                       (fromMaybe (Cancellable nullForeignPtr) cancellable)
                    )
    >>= readUTFStringArray0
#endif

#if GLIB_CHECK_VERSION(2,20,0)
-- | Determines if mount is shadowed. Applications or libraries should avoid displaying mount in the user
-- interface if it is shadowed.
--
-- A mount is said to be shadowed if there exists one or more user visible objects (currently 'Mount'
-- objects) with a root that is inside the root of mount.
--
-- One application of shadow mounts is when exposing a single file system that is used to address
-- several logical volumes. In this situation, a 'VolumeMonitor' implementation would create two 'Volume'
-- objects (for example, one for the camera functionality of the device and one for a SD card reader on
-- the device) with activation URIs gphoto2://[usb:001,002]/store1/ and
-- gphoto2://[usb:001,002]/store2/. When the underlying mount (with root gphoto2://[usb:001,002]/) is
-- mounted, said 'VolumeMonitor' implementation would create two 'Mount' objects (each with their root
-- matching the corresponding volume activation root) that would shadow the original mount.
--
-- The proxy monitor in GVfs 2.26 and later, automatically creates and manage shadow mounts (and
-- shadows the underlying mount) if the activation root on a 'Volume' is set.
mountIsShadowed :: MountClass mount => mount
 -> IO Bool  -- ^ returns 'True' if mount is shadowed.
mountIsShadowed mount =
  liftM toBool $
  {#call g_mount_is_shadowed#} (toMount mount)

-- | Increments the shadow count on mount. Usually used by 'VolumeMonitor' implementations when creating a
-- shadow mount for mount, see 'mountIsShadowed' for more information. The caller will need to emit
-- the "changed" signal on mount manually.
mountShadow :: MountClass mount => mount -> IO ()
mountShadow mount =
  {#call g_mount_shadow#} (toMount mount)

-- | Decrements the shadow count on mount. Usually used by 'VolumeMonitor' implementations when destroying
-- a shadow mount for mount, see 'mountIsShadowed' for more information. The caller will need to
-- emit the "changed" signal on mount manually.
mountUnshadow :: MountClass mount => mount -> IO ()
mountUnshadow mount =
  {#call g_mount_unshadow#} (toMount mount)
#endif

--------------------
-- Signals
-- | Emitted when the mount has been changed.
mountChanged :: MountClass mount => Signal mount (IO ())
mountChanged = Signal (connect_NONE__NONE "changed")

#if GLIB_CHECK_VERSION(2,22,0)
-- | This signal is emitted when the 'Mount' is about to be unmounted.
mountPreUnmount :: MountClass mount => Signal mount (IO ())
mountPreUnmount = Signal (connect_NONE__NONE "pre-unmount")
#endif

-- | This signal is emitted when the 'Mount' have been unmounted. If the recipient is holding references
-- to the object they should release them so the object can be finalized.
mountUnmounted :: MountClass mount => Signal mount (IO ())
mountUnmounted = Signal (connect_NONE__NONE "unmounted")
