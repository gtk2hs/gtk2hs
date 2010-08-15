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

    DriveStartFlags(..),
    DriveStartStopType(..),

    EmblemOrigin(..),

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

{# enum GFileQueryInfoFlags as FileQueryInfoFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags FileQueryInfoFlags

{# enum GFileCreateFlags as FileCreateFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags FileCreateFlags

{# enum GFileCopyFlags as FileCopyFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags FileCopyFlags

{# enum GFileMonitorFlags as FileMonitorFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags FileMonitorFlags

{# enum GFilesystemPreviewType as FilesystemPreviewType {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

{# enum GFileType as FileType {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

{# enum GFileAttributeStatus as FileAttributeStatus {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

{# enum GFileAttributeInfoFlags as FileAttributeInfoFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Read, Show) #}
instance Flags FileAttributeInfoFlags

{# enum GAskPasswordFlags as AskPasswordFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags AskPasswordFlags

{# enum GPasswordSave as PasswordSave {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

{# enum GMountOperationResult as MountOperationResult {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

{# enum GIOErrorEnum as IOErrorEnum {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

{# enum GFileMonitorEvent as FileMonitorEvent {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

{# enum GMountMountFlags as MountMountFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags MountMountFlags

{# enum GMountUnmountFlags as MountUnmountFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags MountUnmountFlags

{# enum GDriveStartFlags as DriveStartFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags DriveStartFlags

{# enum GDriveStartStopType as DriveStartStopType {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

{# enum GEmblemOrigin as EmblemOrigin {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}

{# enum GAppInfoCreateFlags as AppInfoCreateFlags {underscoreToCase} with prefix = "G" deriving (Eq, Ord, Bounded, Show, Typeable) #}
instance Flags AppInfoCreateFlags
