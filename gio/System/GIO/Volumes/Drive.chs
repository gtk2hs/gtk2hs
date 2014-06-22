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
module System.GIO.Volumes.Drive (
-- * Details
--
-- | 'Drive' - this represent a piece of hardware connected to the machine. It's generally only created
-- for removable hardware or hardware with removable media.
--
-- 'Drive' is a container class for 'Volume' objects that stem from the same piece of media. As such,
-- 'Drive' abstracts a drive with (or without) removable media and provides operations for querying
-- whether media is available, determing whether media change is automatically detected and ejecting
-- the media.
--
-- If the 'Drive' reports that media isn't automatically detected, one can poll for media; typically one
-- should not do this periodically as a poll for media operation is potententially expensive and may
-- spin up the drive creating noise.
--
-- 'Drive' supports starting and stopping drives with authentication support for the former. This can be
-- used to support a diverse set of use cases including connecting/disconnecting iSCSI devices,
-- powering down external disk enclosures and starting/stopping multi-disk devices such as RAID
-- devices. Note that the actual semantics and side-effects of starting/ stopping a 'Drive' may vary
-- according to implementation. To choose the correct verbs in e.g. a file manager, use
-- 'driveGetStartStopType'.
--
-- For porting from GnomeVFS note that there is no equivalent of 'Drive' in that API.

-- * Types
    Drive(..),
    DriveClass,

-- * Enums,
#if GLIB_CHECK_VERSION(2,22,0)
    DriveStartStopType (..),
    DriveStartFlags (..),
#endif

-- * Methods
    driveGetName,
    driveGetIcon,
    driveHasVolumes,
    driveGetVolumes,
    driveCanEject,
#if GLIB_CHECK_VERSION(2,22,0)
    driveGetStartStopType,
    driveCanStart,
    driveCanStartDegraded,
    driveCanStop,
#endif
    driveCanPollForMedia,
    drivePollForMedia,
    driveHasMedia,
    driveIsMediaCheckAutomatic,
    driveIsMediaRemovable,
#if GLIB_CHECK_VERSION(2,22,0)
    driveEjectWithOperation,
    driveEjectWithOperationFinish,
    driveStart,
    driveStartFinish,
    driveStopFinish,
#endif
    driveEnumerateIdentifiers,
    driveGetIdentifier,

-- * Signals
    driveChanged,
    driveDisconnected,
    driveEjectButton,
#if GLIB_CHECK_VERSION(2,22,0)
    driveStopButton,
#endif
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
-- | Gets the name of drive.
driveGetName :: (DriveClass drive, GlibString string) => drive
 -> IO string  -- ^ returns the name for the given drive.
driveGetName drive =
  {#call g_drive_get_name#} (toDrive drive)
  >>= readUTFString

-- | Gets the icon for drive.
driveGetIcon :: DriveClass drive => drive
 -> IO Icon -- ^ returns a 'Icon'.
driveGetIcon drive =
  wrapNewGObject mkIcon $
  {#call g_drive_get_icon#} (toDrive drive)

-- | Check if drive has any mountable volumes.
driveHasVolumes :: DriveClass drive => drive
 -> IO Bool  -- ^ returns 'True' if the drive contains volumes, 'False' otherwise.
driveHasVolumes drive =
  liftM toBool $
  {#call g_drive_has_volumes#} (toDrive drive)

-- | Get a list of mountable volumes for drive.
driveGetVolumes :: DriveClass drive => drive
 -> IO [Volume]
driveGetVolumes drive = do
  glistPtr <- {#call g_drive_get_volumes #} (toDrive drive)
  volumePtrs <- fromGList glistPtr
  mapM (wrapNewGObject mkVolume . return) volumePtrs

-- | Checks if drive can be eject.
driveCanEject :: DriveClass drive => drive
 -> IO Bool  -- ^ returns 'True' if the drive can be ejected.
driveCanEject drive =
  liftM toBool $
  {#call g_drive_can_eject#} (toDrive drive)

#if GLIB_CHECK_VERSION(2,22,0)
-- | Gets a hint about how a drive can be started/stopped.
driveGetStartStopType :: DriveClass drive => drive
 -> IO DriveStartStopType -- ^ returns A value from the 'DriveStartStopType' enumeration.
driveGetStartStopType drive =
  liftM (toEnum . fromIntegral) $
  {#call g_drive_get_start_stop_type#} (toDrive drive)

-- | Checks if a drive can be started.
driveCanStart :: DriveClass drive => drive
 -> IO Bool  -- ^ returns 'True' if the drive can be started, 'False' otherwise.
driveCanStart drive =
  liftM toBool $
  {#call g_drive_can_start#} (toDrive drive)

-- | Checks if a drive can be started degraded.
driveCanStartDegraded :: DriveClass drive => drive
 -> IO Bool -- ^ returns 'True' if the drive can be started degraded, 'False' otherwise.
driveCanStartDegraded drive =
  liftM toBool $
  {#call g_drive_can_start_degraded#} (toDrive drive)

-- | Checks if a drive can be stoped.
driveCanStop :: DriveClass drive => drive
 -> IO Bool  -- ^ returns 'True' if the drive can be stoped, 'False' otherwise.
driveCanStop drive =
  liftM toBool $
  {#call g_drive_can_stop#} (toDrive drive)
#endif

-- | Checks if a drive can be polled for media changes.
driveCanPollForMedia :: DriveClass drive => drive
 -> IO Bool
driveCanPollForMedia drive =
  liftM toBool $
  {#call g_drive_can_poll_for_media#} (toDrive drive)

-- | Asynchronously polls drive to see if media has been inserted or removed.
--
-- When the operation is finished, callback will be called. You can then call
-- 'drivePollForMediaFinish' to obtain the result of the operation.
drivePollForMedia :: DriveClass drive => drive
 -> Maybe Cancellable
 -> AsyncReadyCallback
 -> IO ()
drivePollForMedia drive cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_drive_poll_for_media #}
        (toDrive drive)
        (fromMaybe (Cancellable nullForeignPtr) cancellable)
        cCallback
        (castFunPtrToPtr cCallback)

-- | Finishes an operation started with 'drivePollForMedia' on a drive.
--
-- Throws a 'GError' if an error occurs.
drivePollForMediaFinish :: DriveClass drive => drive
 -> AsyncResult -- ^ @result@  a 'AsyncResult'.
 -> IO ()
drivePollForMediaFinish drive result =
    propagateGError (\gErrorPtr -> do
                       {#call g_drive_poll_for_media_finish #}
                          (toDrive drive)
                          result
                          gErrorPtr
                       return ())

-- | Checks if the drive has media. Note that the OS may not be polling the drive for media changes; see
-- 'driveIsMediaCheckAutomatic' for more details.
driveHasMedia :: DriveClass drive => drive
 -> IO Bool  -- ^ returns 'True' if drive has media, 'False' otherwise.
driveHasMedia drive =
  liftM toBool $
  {#call g_drive_has_media#} (toDrive drive)

-- | Checks if drive is capabable of automatically detecting media changes.
driveIsMediaCheckAutomatic :: DriveClass drive => drive
 -> IO Bool -- ^ returns 'True' if the drive is capabable of automatically detecting media changes, 'False' otherwise.
driveIsMediaCheckAutomatic drive =
  liftM toBool $
  {#call g_drive_is_media_check_automatic#} (toDrive drive)

-- | Checks if the drive supports removable media.
driveIsMediaRemovable :: DriveClass drive => drive
 -> IO Bool -- ^ returns 'True' if drive supports removable media, 'False' otherwise.
driveIsMediaRemovable drive =
  liftM toBool $
  {#call g_drive_is_media_removable#} (toDrive drive)

#if GLIB_CHECK_VERSION(2,22,0)
-- | Ejects a drive. This is an asynchronous operation, and is finished by calling
-- 'driveEjectWithOperationFinish' with the drive and 'AsyncResult' data returned in the callback.
driveEjectWithOperation :: DriveClass drive
 => drive
 -> [MountUnmountFlags] -- ^ @flags@           flags affecting the unmount if required for eject
 -> Maybe MountOperation -- ^ @mountOperation@ a 'MountOperation' or 'Nothing' to avoid user interaction.
 -> Maybe Cancellable -- ^ @cancellable@     optional 'Cancellable' object, 'Nothing' to ignore.
 -> AsyncReadyCallback -- ^ @callback@        a 'AsyncReadyCallback'
 -> IO ()
driveEjectWithOperation drive flags mountOperation cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_drive_eject_with_operation #}
        (toDrive drive)
        ((fromIntegral . fromFlags) flags)
        (fromMaybe (MountOperation nullForeignPtr) mountOperation)
        (fromMaybe (Cancellable nullForeignPtr) cancellable)
        cCallback
        (castFunPtrToPtr cCallback)

-- | Finishes ejecting a drive. If any errors occurred during the operation, error will be set to contain
-- the errors and 'False' will be returned.
--
-- Throws a 'GError' if an error occurs.
driveEjectWithOperationFinish :: DriveClass drive
 => drive
 -> AsyncResult -- ^ @result@  a 'AsyncResult'.
 -> IO ()
driveEjectWithOperationFinish drive result =
    propagateGError (\gErrorPtr -> do
                        {#call g_drive_eject_with_operation_finish #}
                           (toDrive drive)
                           result
                           gErrorPtr
                        return ())

-- | Asynchronously starts a drive.
--
-- When the operation is finished, callback will be called. You can then call 'driveStartFinish' to
-- obtain the result of the operation.
driveStart :: DriveClass drive
 => drive
 -> [DriveStartFlags] -- ^ @flags@           flags affecting the start operation.
 -> Maybe MountOperation -- ^ @mountOperation@ a 'MountOperation' or 'Nothing' to avoid user interaction.
 -> Maybe Cancellable -- ^ @cancellable@     optional 'Cancellable' object, 'Nothing' to ignore.
 -> AsyncReadyCallback -- ^ @callback@        a 'AsyncReadyCallback'
 -> IO ()
driveStart drive flags mountOperation cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_drive_start #}
        (toDrive drive)
        ((fromIntegral . fromFlags) flags)
        (fromMaybe (MountOperation nullForeignPtr) mountOperation)
        (fromMaybe (Cancellable nullForeignPtr) cancellable)
        cCallback
        (castFunPtrToPtr cCallback)

-- | Finishes starting a drive.
--
-- Throws a 'GError' if an error occurs.
driveStartFinish :: DriveClass drive
 => drive
 -> AsyncResult -- ^ @result@  a 'AsyncResult'.
 -> IO ()
driveStartFinish drive result =
    propagateGError (\gErrorPtr -> do
                       {#call g_drive_start_finish #}
                           (toDrive drive)
                           result
                           gErrorPtr
                       return ())

-- | Asynchronously stops a drive.
--
-- When the operation is finished, callback will be called. You can then call 'driveStopFinish' to
-- obtain the result of the operation.
driveStop :: DriveClass drive
 => drive
 -> [MountUnmountFlags] -- ^ @flags@           flags affecting the stop operation.
 -> Maybe MountOperation -- ^ @mountOperation@ a 'MountOperation' or 'Nothing' to avoid user interaction.
 -> Maybe Cancellable -- ^ @cancellable@     optional 'Cancellable' object, 'Nothing' to ignore.
 -> AsyncReadyCallback -- ^ @callback@        a 'AsyncReadyCallback'
 -> IO ()
driveStop drive flags mountOperation cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_drive_stop #}
        (toDrive drive)
        ((fromIntegral . fromFlags) flags)
        (fromMaybe (MountOperation nullForeignPtr) mountOperation)
        (fromMaybe (Cancellable nullForeignPtr) cancellable)
        cCallback
        (castFunPtrToPtr cCallback)

-- | Finishes stoping a drive.
--
-- Throws a 'GError' if an error occurs.
driveStopFinish :: DriveClass drive
 => drive
 -> AsyncResult -- ^ @result@  a 'AsyncResult'.
 -> IO ()
driveStopFinish drive result =
    propagateGError (\gErrorPtr -> do
                        {#call g_drive_stop_finish #}
                             (toDrive drive)
                             result
                             gErrorPtr
                        return ())
#endif

-- | Gets the kinds of identifiers that drive has. Use 'driveGetIdentifer' to obtain the
-- identifiers themselves.
driveEnumerateIdentifiers :: (DriveClass drive, GlibString string) => drive
                           -> IO [string]
driveEnumerateIdentifiers drive =
  {#call g_drive_enumerate_identifiers#} (toDrive drive)
  >>= readUTFStringArray0

-- | Gets the identifier of the given kind for drive. See the introduction for more information about
-- drive identifiers.
driveGetIdentifier :: (DriveClass drive, GlibString string) => drive
                    -> string  -- ^ @kind@    the kind of identifier to return
                    -> IO string
driveGetIdentifier drive kind =
  withUTFString kind $ \ kindPtr ->
  {#call g_drive_get_identifier#} (toDrive drive) kindPtr
  >>= readUTFString

--------------------
-- Signals
-- | Emitted when a drive changes.
driveChanged :: DriveClass drive => Signal drive (Drive -> IO ())
driveChanged = Signal (connect_OBJECT__NONE "drive-changed")

-- | Emitted when a drive changes.
driveDisconnected :: DriveClass drive => Signal drive (Drive -> IO ())
driveDisconnected = Signal (connect_OBJECT__NONE "drive-disconnected")

-- | Emitted when the eject button is pressed on drive.
driveEjectButton :: DriveClass drive => Signal drive (Drive -> IO ())
driveEjectButton = Signal (connect_OBJECT__NONE "drive-eject-button")

#if GLIB_CHECK_VERSION(2,22,0)
-- | Emitted when the stop button is pressed on drive.
driveStopButton :: DriveClass drive => Signal drive (Drive -> IO ())
driveStopButton = Signal (connect_OBJECT__NONE "drive-stop-button")
#endif
