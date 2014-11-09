{-# LANGUAGE CPP, DeriveDataTypeable #-}
--  GIMP Toolkit (GTK) Binding for Haskell: binding to gio -*-haskell-*-
--
--  Author : Andy Stewart
--  Created: 30-April-2010
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
module System.GIO.Enums (
-- * Enums
    FileQueryInfoFlags(..),
    FileCreateFlags(..),
    FileCopyFlags(..),
    FileMonitorFlags(..),
    FilesystemPreviewType(..),
    FileType(..),
    FileAttributeStatus(..),
    FileAttributeInfoFlags(..),

    AskPasswordFlags(..),
    PasswordSave(..),
    MountOperationResult(..),

    IOErrorEnum(..),

    FileMonitorEvent(..),
    MountMountFlags(..),
    MountUnmountFlags(..),

#if GLIB_CHECK_VERSION(2,22,0)
    DriveStartFlags(..),
    DriveStartStopType(..),
#endif

#if GLIB_CHECK_VERSION(2,18,0)
    EmblemOrigin(..),
#endif

    AppInfoCreateFlags(..),
    ) where

import Control.Monad
import Data.Typeable
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.GObject
import System.Glib.UTFString

{# context lib = "gio" prefix = "g" #}

-- | Flags used when querying a 'FileInfo'.
{# enum GFileQueryInfoFlags as FileQueryInfoFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags FileQueryInfoFlags

-- | Flags used when an operation may create a file.
{# enum GFileCreateFlags as FileCreateFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags FileCreateFlags

-- | Flags used when copying or moving files.
{# enum GFileCopyFlags as FileCopyFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags FileCopyFlags

-- | Flags used to set what a 'FileMonitor' will watch for.
{# enum GFileMonitorFlags as FileMonitorFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags FileMonitorFlags

-- | Indicates a hint from the file system whether files should be previewed in a file manager. Returned
-- as the value of the key 'FileAttributeFilesystemUsePreview'.
{# enum GFilesystemPreviewType as FilesystemPreviewType {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

-- | Indicates the file's on-disk type.
{# enum GFileType as FileType {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

-- | Used by 'fileSetAttributesFromInfo' when setting file attributes.
{# enum GFileAttributeStatus as FileAttributeStatus {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

-- | Flags specifying the behaviour of an attribute.
{# enum GFileAttributeInfoFlags as FileAttributeInfoFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Read, Show) #}
instance Flags FileAttributeInfoFlags

-- | 'AskPasswordFlags' are used to request specific information from the user, or to notify the user of
-- their choices in an authentication situation.
{# enum GAskPasswordFlags as AskPasswordFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags AskPasswordFlags

-- | 'PasswordSave' is used to indicate the lifespan of a saved password.
{# enum GPasswordSave as PasswordSave {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

-- | 'MountOperationResult' is returned as a result when a request for information is send by the mounting
-- operation.
{# enum GMountOperationResult as MountOperationResult {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

-- | Error codes returned by GIO functions.
{# enum GIOErrorEnum as IOErrorEnum {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

-- | Specifies what type of event a monitor event is.
{# enum GFileMonitorEvent as FileMonitorEvent {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

-- | Flags used when mounting a mount.
{# enum GMountMountFlags as MountMountFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags MountMountFlags

-- | Flags used when an unmounting a mount.
{# enum GMountUnmountFlags as MountUnmountFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags MountUnmountFlags

#if GLIB_CHECK_VERSION(2,22,0)
-- | Flags used when starting a drive.
{# enum GDriveStartFlags as DriveStartFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags DriveStartFlags

-- | Enumeration describing how a drive can be started/stopped.
{# enum GDriveStartStopType as DriveStartStopType {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
#endif

#if GLIB_CHECK_VERSION(2,18,0)
-- | 'EmblemOrigin' is used to add information about the origin of the emblem to 'Emblem'.
{# enum GEmblemOrigin as EmblemOrigin {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
#endif

-- | Flags used when creating a 'AppInfo'.
{# enum GAppInfoCreateFlags as AppInfoCreateFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags AppInfoCreateFlags
