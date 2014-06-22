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
module System.GIO.Volumes.Volume (
-- * Details
--
-- | The 'Volume' interface represents user-visible objects that can be mounted. Note, when porting from
-- GnomeVFS, 'Volume' is the moral equivalent of GnomeVFSDrive.
--
-- Mounting a 'Volume' instance is an asynchronous operation. For more information about asynchronous
-- operations, see 'AsyncReady' and GSimpleAsyncReady. To mount a 'Volume', first call 'volumeMount'
-- with (at least) the 'Volume' instance, optionally a 'MountOperation' object and a 'AsyncReadyCallback'.
--
-- Typically, one will only want to pass 'Nothing' for the 'MountOperation' if automounting all volumes when
-- a desktop session starts since it's not desirable to put up a lot of dialogs asking for credentials.
--
-- The callback will be fired when the operation has resolved (either with success or failure), and a
-- 'AsyncReady' structure will be passed to the callback. That callback should then call
-- 'volumeMountFinish' with the 'Volume' instance and the 'AsyncReady' data to see if the operation
-- was completed successfully. If an error is present when 'volumeMountFinish' is called, then it
-- will be filled with any error information.
--
-- It is sometimes necessary to directly access the underlying operating system object behind a volume
-- (e.g. for passing a volume to an application via the commandline). For this purpose, GIO allows to
-- obtain an 'identifier' for the volume. There can be different kinds of identifiers, such as Hal
-- UDIs, filesystem labels, traditional Unix devices (e.g. /dev/sda2), uuids. GIO uses predefind
-- strings as names for the different kinds of identifiers: 'VolumeIdentifierKindHalUdi',
-- 'VolumeIdentifierKindLabel', etc. Use 'volumeGetIdentifier' to obtain an identifier for a
-- volume.
--
-- Note that 'VolumeIdentifierKindHalUdi' will only be available when the gvfs hal volume monitor
-- is in use. Other volume monitors will generally be able to provide the
-- 'VolumeIdentifierKindUnixDevice' identifier, which can be used to obtain a hal device by means
-- of 'mangerFindDeviceStringMatch'.

-- * Types
    Volume(..),
    VolumeClass,

-- * Methods
    volumeGetName,
    volumeGetUUID,
    volumeGetIcon,
    volumeGetDrive,
    volumeGetMount,
    volumeCanMount,
    volumeShouldAutomount,
#if GLIB_CHECK_VERSION(2,18,0)
    volumeGetActivationRoot,
#endif
    volumeMount,
    volumeMountFinish,
    volumeCanEject,
#if GLIB_CHECK_VERSION(2,22,0)
    volumeEjectWithOperation,
    volumeEjectWithOperationFinish,
#endif
    volumeEnumerateIdentifiers,
    volumeGetIdentifier,

-- * Signals
    volumeChanged,
    volumeRemoved,
    ) where

import Control.Monad
import Data.Maybe (fromMaybe)
import System.GIO.Enums
import System.Glib.Attributes
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
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
-- | Gets the name of volume.
volumeGetName :: (VolumeClass volume, GlibString string) => volume
              -> IO string  -- ^ returns the name for the given volume.
volumeGetName volume =
    {#call g_volume_get_name#} (toVolume volume)
    >>= readUTFString

-- | Gets the UUID for the volume. The reference is typically based on the file system UUID for the
-- volume in question and should be considered an opaque string. Returns 'Nothing' if there is no UUID
-- available.
volumeGetUUID :: (VolumeClass volume, GlibString string) => volume
              -> IO (Maybe string)  -- ^ returns the UUID for volume or 'Nothing' if no UUID can be computed.
volumeGetUUID volume =
  {#call g_volume_get_uuid#} (toVolume volume)
  >>= maybePeek readUTFString

-- | Gets the icon for volume.
volumeGetIcon :: VolumeClass volume => volume
              -> IO Icon
volumeGetIcon volume =
  wrapNewGObject mkIcon $
  {#call g_volume_get_icon#} (toVolume volume)

-- | Gets the drive for the volume.
volumeGetDrive :: VolumeClass volume => volume
               -> IO (Maybe Drive)  -- ^ returns a 'Drive' or 'Nothing' if volume is not associated with a drive.
volumeGetDrive volume =
  maybeNull (wrapNewGObject mkDrive) $
  {#call g_volume_get_drive#} (toVolume volume)

-- | Gets the mount for the volume.
volumeGetMount :: VolumeClass volume => volume
               -> IO (Maybe Mount)  -- ^ returns a 'Mount' or 'Nothing' if volume is not associated with a mount.
volumeGetMount volume =
  maybeNull (wrapNewGObject mkMount) $
  {#call g_volume_get_mount#} (toVolume volume)

-- | Checks if a volume can be mounted.
volumeCanMount :: VolumeClass volume => volume
               -> IO Bool -- ^ returns 'True' if the volume can be mounted. 'False' otherwise.
volumeCanMount volume =
  liftM toBool $
  {#call g_volume_can_mount#} (toVolume volume)

-- | Returns whether the volume should be automatically mounted.
volumeShouldAutomount :: VolumeClass volume => volume
                      -> IO Bool -- ^ returns 'True' if the volume should be automatically mounted.
volumeShouldAutomount volume =
  liftM toBool $
  {#call g_volume_should_automount#} (toVolume volume)

#if GLIB_CHECK_VERSION(2,18,0)
-- | Gets the activation root for a 'Volume' if it is known ahead of mount time. Returns 'Nothing'
-- otherwise. If not 'Nothing' and if volume is mounted, then the result of 'mountGetRoot' on the 'Mount'
-- object obtained from 'volumeGetMount' will always either be equal or a prefix of what this
-- function returns.
volumeGetActivationRoot :: VolumeClass volume => volume
                        -> IO (Maybe File) -- ^ returns the activation root of volume or 'Nothing'.
volumeGetActivationRoot volume =
  maybeNull (wrapNewGObject mkFile) $
  {#call g_volume_get_activation_root#} (toVolume volume)
#endif

-- | Mounts a volume. This is an asynchronous operation, and is finished by calling
-- 'volumeMountFinish' with the volume and 'AsyncResult' returned in the callback.
volumeMount :: VolumeClass volume => volume
            -> [MountMountFlags] -- ^ @flags@           flags affecting the operation
            -> Maybe MountOperation -- ^ @mountOperation@ a 'MountOperation' or 'Nothing' to avoid user interaction.
            -> Maybe Cancellable -- ^ @cancellable@     optional 'Cancellable' object, 'Nothing' to ignore.
            -> AsyncReadyCallback -- ^ @callback@        a 'AsyncReadyCallback'
            -> IO ()
volumeMount volume flags mountOperation cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_volume_mount #}
        (toVolume volume)
        ((fromIntegral . fromFlags) flags)
        (fromMaybe (MountOperation nullForeignPtr) mountOperation)
        (fromMaybe (Cancellable nullForeignPtr) cancellable)
        cCallback
        (castFunPtrToPtr cCallback)

-- | Finishes mounting a volume. If any errors occured during the operation, error will be set to contain
-- the errors and 'False' will be returned.
--
-- If the mount operation succeeded, 'volumeGetMount' on volume is guaranteed to return the mount
-- right after calling this function; there's no need to listen for the 'mount-added' signal on
-- 'VolumeMonitor'.
--
-- Throws a 'GError' if an error occurs.
volumeMountFinish :: VolumeClass volume => volume
                  -> AsyncResult -- ^ @result@  a 'AsyncResult'
                  -> IO ()
volumeMountFinish volume result =
    propagateGError (\gErrorPtr -> do
                        {#call g_volume_mount_finish#}
                           (toVolume volume)
                           result
                           gErrorPtr
                        return ())

-- | Checks if a volume can be ejected.
volumeCanEject :: VolumeClass volume => volume
               -> IO Bool -- ^ returns 'True' if the volume can be ejected. 'False' otherwise.
volumeCanEject volume =
  liftM toBool $
  {#call g_volume_can_eject#} (toVolume volume)

#if GLIB_CHECK_VERSION(2,22,0)
-- | Ejects a volume. This is an asynchronous operation, and is finished by calling
-- 'volumeEjectWithOperationFinish' with the volume and 'AsyncResult' data returned in the
-- callback.
volumeEjectWithOperation :: VolumeClass volume => volume
 -> [MountUnmountFlags] -- ^ @flags@           flags affecting the unmount if required for eject
 -> Maybe MountOperation -- ^ @mountOperation@ a 'MountOperation' or 'Nothing' to avoid user interaction.
 -> Maybe Cancellable -- ^ @cancellable@     optional 'Cancellable' object, 'Nothing' to ignore.
 -> AsyncReadyCallback -- ^ @callback@        a 'AsyncReadyCallback'
 -> IO ()
volumeEjectWithOperation volume flags mountOperation cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_volume_eject_with_operation #}
        (toVolume volume)
        ((fromIntegral . fromFlags) flags)
        (fromMaybe (MountOperation nullForeignPtr) mountOperation)
        (fromMaybe (Cancellable nullForeignPtr) cancellable)
        cCallback
        (castFunPtrToPtr cCallback)

-- | Finishes ejecting a volume. If any errors occurred during the operation, error will be set to
-- contain the errors and 'False' will be returned.
--
-- Throws a 'GError' if an error occurs.
volumeEjectWithOperationFinish :: VolumeClass volume => volume
                               -> AsyncResult -- ^ @result@  a 'AsyncResult'.
                               -> IO ()
volumeEjectWithOperationFinish volume result =
    propagateGError (\gErrorPtr -> do
                       {#call g_volume_eject_with_operation_finish #}
                          (toVolume volume)
                          result
                          gErrorPtr
                       return ())
#endif

-- | Gets the kinds of identifiers that volume has. Use 'volumeGetIdentifer' to obtain the
-- identifiers themselves.
volumeEnumerateIdentifiers :: (VolumeClass volume, GlibString string) => volume
                           -> IO [string]
volumeEnumerateIdentifiers volume =
  {#call g_volume_enumerate_identifiers#} (toVolume volume)
  >>= readUTFStringArray0

-- | Gets the identifier of the given kind for volume. See the introduction for more information about
-- volume identifiers.
volumeGetIdentifier :: (VolumeClass volume, GlibString string) => volume
                    -> string  -- ^ @kind@    the kind of identifier to return
                    -> IO string
volumeGetIdentifier volume kind =
  withUTFString kind $ \ kindPtr ->
  {#call g_volume_get_identifier#} (toVolume volume) kindPtr
  >>= readUTFString

--------------------
-- Signals
-- | Emitted when the volume has been changed.
volumeChanged :: VolumeClass volume => Signal volume (IO ())
volumeChanged = Signal (connect_NONE__NONE "changed")

-- | This signal is emitted when the 'Volume' have been removed. If the recipient is holding references to
-- the object they should release them so the object can be finalized.
volumeRemoved :: VolumeClass volume => Signal volume (IO ())
volumeRemoved = Signal (connect_NONE__NONE "removed")

