{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- -*-haskell-*-

#include <gio/gio.h>

--  GIMP Toolkit (GTK) Binding for Haskell: binding to gio -*-haskell-*-
--
--  Author : Peter Gavin, Andy Stewart
--  Created: 13-Oct-2008
--
--  Copyright (c) 2008 Peter Gavin
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
module System.GIO.File.FileAttribute (
-- * Details
-- | File attributes in GIO consist of a list of key-value pairs.
--
-- Keys are strings that contain a key namespace and a key name, separated by a colon,
-- e.g. "namespace:keyname". Namespaces are included to sort key-value pairs by namespaces for
-- relevance. Keys can be retrived using wildcards, e.g. \"standard::*\" will return all of the keys in
-- the "standard" namespace.
--
-- Values are stored within the list in 'FileAttributeValue' structures. Values can store different
-- types, listed in the enum 'FileAttributeType'. Upon creation of a 'FileAttributeValue', the type will
-- be set to 'FileAttributeTypeInvalid'.
--
-- The list of possible attributes for a filesystem (pointed to by a 'File') is availible as a
-- 'FileAttributeInfoList'. This list is queryable by key names as indicated earlier.
--
-- Classes that implement 'FileIface' will create a 'FileAttributeInfoList' and install default keys and
-- values for their given file system, architecture, and other possible implementation details (e.g.,
-- on a UNIX system, a file attribute key will be registered for the user id for a given file).

-- * Types
    FileAttributeType (..),
    FileAttributeInfo (..),

-- * Enums
    FileAttributeInfoFlags (..),

-- * Methods
    fileAttributeStandardType,
    fileAttributeStandardIsHidden,
    fileAttributeStandardIsBackup,
    fileAttributeStandardIsSymlink,
    fileAttributeStandardIsVirtual,
    fileAttributeStandardName,
    fileAttributeStandardDisplayName,
    fileAttributeStandardEditName,
    fileAttributeStandardCopyName,
    fileAttributeStandardIcon,
    fileAttributeStandardContentType,
    fileAttributeStandardFastContentType,
    fileAttributeStandardSize,
#if GLIB_CHECK_VERSION(2,20,0)
    fileAttributeStandardAllocatedSize,
#endif
    fileAttributeStandardSymlinkTarget,
    fileAttributeStandardTargetURI,
    fileAttributeStandardSortOrder,
    fileAttributeEtagValue,
    fileAttributeIDFile,
    fileAttributeIDFilesystem,
    fileAttributeAccessCanRead,
    fileAttributeAccessCanWrite,
    fileAttributeAccessCanExecute,
    fileAttributeAccessCanDelete,
    fileAttributeAccessCanTrash,
    fileAttributeAccessCanRename,
    fileAttributeMountableCanMount,
    fileAttributeMountableCanUnmount,
    fileAttributeMountableCanEject,
    fileAttributeMountableUnixDevice,
#if GLIB_CHECK_VERSION(2,22,0)
    fileAttributeMountableUnixDeviceFile,
    fileAttributeMountableCanStart,
    fileAttributeMountableCanDegraded,
    fileAttributeMountableCanStop,
    fileAttributeMountableStartStopType,
    fileAttributeMountableCanPoll,
#endif
    fileAttributeMountableHalUDI,
    fileAttributeTimeModified,
    fileAttributeTimeModifiedUSec,
    fileAttributeTimeAccess,
    fileAttributeTimeAccessUSec,
    fileAttributeTimeChanged,
    fileAttributeTimeChangedUSec,
    fileAttributeTimeCreated,
    fileAttributeTimeCreatedUSec,
    fileAttributeUnixDevice,
    fileAttributeUnixInode,
    fileAttributeUnixMode,
    fileAttributeUnixNLink,
    fileAttributeUnixUID,
    fileAttributeUnixGID,
    fileAttributeUnixRDev,
    fileAttributeUnixBlockSize,
    fileAttributeUnixBlocks,
    fileAttributeDosIsMountpoint,
    fileAttributeDosIsArchive,
    fileAttributeDosIsSystem,
    fileAttributeOwnerUser,
    fileAttributeOwnerUserReal,
    fileAttributeOwnerGroup,
    fileAttributeThumbnailPath,
    fileAttributeThumbnailingFailed,
#if GLIB_CHECK_VERSION(2,20,0)
    fileAttributePreviewIcon,
#endif
    fileAttributeFilesystemSize,
    fileAttributeFilesystemFree,
    fileAttributeFilesystemType,
    fileAttributeFilesystemReadonly,
    fileAttributeGVfsBackend,
    fileAttributeSELinuxContext,
    fileAttributeTrashItemCount,
    fileAttributeFilesystemUsePreview,
    fileAttributeStandardDescription,
#if GLIB_CHECK_VERSION(2,24,0)
    fileAttributeTrashOrigPath,
    fileAttributeTrashDeletionDate,
#endif
    ) where


import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Flags

import System.GIO.Enums

data FileAttributeType = FileAttributeTypeInvalid
                       | FileAttributeTypeString
                       | FileAttributeTypeByteString
                       | FileAttributeTypeBool
                       | FileAttributeTypeWord32
                       | FileAttributeTypeInt32
                       | FileAttributeTypeWord64
                       | FileAttributeTypeInt64
                       | FileAttributeTypeObject
#if GLIB_CHECK_VERSION(2,22,0)
                       | FileAttributeTypeStringList
#endif
                         deriving (Eq, Ord, Bounded, Show, Read)
instance Enum FileAttributeType where
    toEnum #{const G_FILE_ATTRIBUTE_TYPE_INVALID}     = FileAttributeTypeInvalid
    toEnum #{const G_FILE_ATTRIBUTE_TYPE_STRING}      = FileAttributeTypeString
    toEnum #{const G_FILE_ATTRIBUTE_TYPE_BYTE_STRING} = FileAttributeTypeByteString
    toEnum #{const G_FILE_ATTRIBUTE_TYPE_BOOLEAN}     = FileAttributeTypeBool
    toEnum #{const G_FILE_ATTRIBUTE_TYPE_UINT32}      = FileAttributeTypeWord32
    toEnum #{const G_FILE_ATTRIBUTE_TYPE_INT32}       = FileAttributeTypeInt32
    toEnum #{const G_FILE_ATTRIBUTE_TYPE_UINT64}      = FileAttributeTypeWord64
    toEnum #{const G_FILE_ATTRIBUTE_TYPE_INT64}       = FileAttributeTypeInt64
    toEnum #{const G_FILE_ATTRIBUTE_TYPE_OBJECT}      = FileAttributeTypeObject
#if GLIB_CHECK_VERSION(2,22,0)
    toEnum #{const G_FILE_ATTRIBUTE_TYPE_STRINGV}     = FileAttributeTypeStringList
#endif

    fromEnum FileAttributeTypeInvalid    = #{const G_FILE_ATTRIBUTE_TYPE_INVALID}
    fromEnum FileAttributeTypeString     = #{const G_FILE_ATTRIBUTE_TYPE_STRING}
    fromEnum FileAttributeTypeByteString = #{const G_FILE_ATTRIBUTE_TYPE_BYTE_STRING}
    fromEnum FileAttributeTypeBool       = #{const G_FILE_ATTRIBUTE_TYPE_BOOLEAN}
    fromEnum FileAttributeTypeWord32     = #{const G_FILE_ATTRIBUTE_TYPE_UINT32}
    fromEnum FileAttributeTypeInt32      = #{const G_FILE_ATTRIBUTE_TYPE_INT32}
    fromEnum FileAttributeTypeWord64     = #{const G_FILE_ATTRIBUTE_TYPE_UINT64}
    fromEnum FileAttributeTypeInt64      = #{const G_FILE_ATTRIBUTE_TYPE_INT64}
    fromEnum FileAttributeTypeObject     = #{const G_FILE_ATTRIBUTE_TYPE_OBJECT}
#if GLIB_CHECK_VERSION(2,22,0)
    fromEnum FileAttributeTypeStringList = #{const G_FILE_ATTRIBUTE_TYPE_STRINGV}
#endif

data FileAttributeInfo =
    FileAttributeInfo
    { fileAttributeInfoName :: DefaultGlibString
    , fileAttributeInfoType :: FileAttributeType
    , fileAttributeInfoFlags :: [FileAttributeInfoFlags]
    } deriving (Eq, Read, Show)

instance Storable FileAttributeInfo where
    sizeOf _ = #{size GFileAttributeInfo}
    alignment _ = alignment (undefined :: Ptr ())
    peek ptr = do
      retName <- #{peek GFileAttributeInfo, name} ptr >>= readUTFString
      retType <- (#{peek GFileAttributeInfo, type} ptr :: IO CInt) >>= return . (toEnum . fromIntegral)
      retFlags <- (#{peek GFileAttributeInfo, flags} ptr :: IO CInt) >>= return . (toFlags . fromIntegral)
      return $ FileAttributeInfo
               { fileAttributeInfoName = retName
               , fileAttributeInfoType = retType
               , fileAttributeInfoFlags = retFlags }

    poke _ = error "not implemented"

fileAttributeStandardType,
    fileAttributeStandardIsHidden,
    fileAttributeStandardIsBackup,
    fileAttributeStandardIsSymlink,
    fileAttributeStandardIsVirtual,
    fileAttributeStandardName,
    fileAttributeStandardDisplayName,
    fileAttributeStandardEditName,
    fileAttributeStandardCopyName,
    fileAttributeStandardIcon,
    fileAttributeStandardContentType,
    fileAttributeStandardFastContentType,
    fileAttributeStandardSize,
#if GLIB_CHECK_VERSION(2,20,0)
    fileAttributeStandardAllocatedSize,
#endif
    fileAttributeStandardSymlinkTarget,
    fileAttributeStandardTargetURI,
    fileAttributeStandardSortOrder,
    fileAttributeEtagValue,
    fileAttributeIDFile,
    fileAttributeIDFilesystem,
    fileAttributeAccessCanRead,
    fileAttributeAccessCanWrite,
    fileAttributeAccessCanExecute,
    fileAttributeAccessCanDelete,
    fileAttributeAccessCanTrash,
    fileAttributeAccessCanRename,
    fileAttributeMountableCanMount,
    fileAttributeMountableCanUnmount,
    fileAttributeMountableCanEject,
    fileAttributeMountableUnixDevice,
#if GLIB_CHECK_VERSION(2,22,0)
    fileAttributeMountableUnixDeviceFile,
    fileAttributeMountableCanStart,
    fileAttributeMountableCanDegraded,
    fileAttributeMountableCanStop,
    fileAttributeMountableStartStopType,
    fileAttributeMountableCanPoll,
#endif
    fileAttributeMountableHalUDI,
    fileAttributeTimeModified,
    fileAttributeTimeModifiedUSec,
    fileAttributeTimeAccess,
    fileAttributeTimeAccessUSec,
    fileAttributeTimeChanged,
    fileAttributeTimeChangedUSec,
    fileAttributeTimeCreated,
    fileAttributeTimeCreatedUSec,
    fileAttributeUnixDevice,
    fileAttributeUnixInode,
    fileAttributeUnixMode,
    fileAttributeUnixNLink,
    fileAttributeUnixUID,
    fileAttributeUnixGID,
    fileAttributeUnixRDev,
    fileAttributeUnixBlockSize,
    fileAttributeUnixBlocks,
    fileAttributeDosIsMountpoint,
    fileAttributeDosIsArchive,
    fileAttributeDosIsSystem,
    fileAttributeOwnerUser,
    fileAttributeOwnerUserReal,
    fileAttributeOwnerGroup,
    fileAttributeThumbnailPath,
    fileAttributeThumbnailingFailed,
#if GLIB_CHECK_VERSION(2,20,0)
    fileAttributePreviewIcon,
#endif
    fileAttributeFilesystemSize,
    fileAttributeFilesystemFree,
    fileAttributeFilesystemType,
    fileAttributeFilesystemReadonly,
    fileAttributeGVfsBackend,
    fileAttributeSELinuxContext,
    fileAttributeTrashItemCount,
    fileAttributeFilesystemUsePreview,
#if GLIB_CHECK_VERSION(2,24,0)
    fileAttributeTrashOrigPath,
    fileAttributeTrashDeletionDate,
#endif
    fileAttributeStandardDescription
    :: String
fileAttributeStandardType            = #{const_str G_FILE_ATTRIBUTE_STANDARD_TYPE}
fileAttributeStandardIsHidden        = #{const_str G_FILE_ATTRIBUTE_STANDARD_IS_HIDDEN}
fileAttributeStandardIsBackup        = #{const_str G_FILE_ATTRIBUTE_STANDARD_IS_BACKUP}
fileAttributeStandardIsSymlink       = #{const_str G_FILE_ATTRIBUTE_STANDARD_IS_SYMLINK}
fileAttributeStandardIsVirtual       = #{const_str G_FILE_ATTRIBUTE_STANDARD_IS_VIRTUAL}
fileAttributeStandardName            = #{const_str G_FILE_ATTRIBUTE_STANDARD_NAME}
fileAttributeStandardDisplayName     = #{const_str G_FILE_ATTRIBUTE_STANDARD_DISPLAY_NAME}
fileAttributeStandardEditName        = #{const_str G_FILE_ATTRIBUTE_STANDARD_EDIT_NAME}
fileAttributeStandardCopyName        = #{const_str G_FILE_ATTRIBUTE_STANDARD_COPY_NAME}
fileAttributeStandardIcon            = #{const_str G_FILE_ATTRIBUTE_STANDARD_ICON}
fileAttributeStandardContentType     = #{const_str G_FILE_ATTRIBUTE_STANDARD_CONTENT_TYPE}
fileAttributeStandardFastContentType = #{const_str G_FILE_ATTRIBUTE_STANDARD_FAST_CONTENT_TYPE}
fileAttributeStandardSize            = #{const_str G_FILE_ATTRIBUTE_STANDARD_SIZE}
#if GLIB_CHECK_VERSION(2,20,0)
fileAttributeStandardAllocatedSize   = #{const_str G_FILE_ATTRIBUTE_STANDARD_ALLOCATED_SIZE}
#endif
fileAttributeStandardSymlinkTarget   = #{const_str G_FILE_ATTRIBUTE_STANDARD_SYMLINK_TARGET}
fileAttributeStandardTargetURI       = #{const_str G_FILE_ATTRIBUTE_STANDARD_TARGET_URI}
fileAttributeStandardSortOrder       = #{const_str G_FILE_ATTRIBUTE_STANDARD_SORT_ORDER}
fileAttributeEtagValue               = #{const_str G_FILE_ATTRIBUTE_ETAG_VALUE}
fileAttributeIDFile                  = #{const_str G_FILE_ATTRIBUTE_ID_FILE}
fileAttributeIDFilesystem            = #{const_str G_FILE_ATTRIBUTE_ID_FILESYSTEM}
fileAttributeAccessCanRead           = #{const_str G_FILE_ATTRIBUTE_ACCESS_CAN_READ}
fileAttributeAccessCanWrite          = #{const_str G_FILE_ATTRIBUTE_ACCESS_CAN_WRITE}
fileAttributeAccessCanExecute        = #{const_str G_FILE_ATTRIBUTE_ACCESS_CAN_EXECUTE}
fileAttributeAccessCanDelete         = #{const_str G_FILE_ATTRIBUTE_ACCESS_CAN_DELETE}
fileAttributeAccessCanTrash          = #{const_str G_FILE_ATTRIBUTE_ACCESS_CAN_TRASH}
fileAttributeAccessCanRename         = #{const_str G_FILE_ATTRIBUTE_ACCESS_CAN_RENAME}
fileAttributeMountableCanMount       = #{const_str G_FILE_ATTRIBUTE_MOUNTABLE_CAN_MOUNT}
fileAttributeMountableCanUnmount     = #{const_str G_FILE_ATTRIBUTE_MOUNTABLE_CAN_UNMOUNT}
fileAttributeMountableCanEject       = #{const_str G_FILE_ATTRIBUTE_MOUNTABLE_CAN_EJECT}
fileAttributeMountableUnixDevice     = #{const_str G_FILE_ATTRIBUTE_MOUNTABLE_UNIX_DEVICE}
#if GLIB_CHECK_VERSION(2,22,0)
fileAttributeMountableUnixDeviceFile = #{const_str G_FILE_ATTRIBUTE_MOUNTABLE_UNIX_DEVICE_FILE}
fileAttributeMountableCanStart       = #{const_str G_FILE_ATTRIBUTE_MOUNTABLE_CAN_START}
fileAttributeMountableCanDegraded    = #{const_str G_FILE_ATTRIBUTE_MOUNTABLE_CAN_START_DEGRADED}
fileAttributeMountableCanStop        = #{const_str G_FILE_ATTRIBUTE_MOUNTABLE_CAN_STOP}
fileAttributeMountableStartStopType  = #{const_str G_FILE_ATTRIBUTE_MOUNTABLE_START_STOP_TYPE}
fileAttributeMountableCanPoll        = #{const_str G_FILE_ATTRIBUTE_MOUNTABLE_CAN_POLL}
fileAttributeMountableIsMediaCheckAutomatic = #{const_str G_FILE_ATTRIBUTE_MOUNTABLE_IS_MEDIA_CHECK_AUTOMATIC}
#endif
fileAttributeMountableHalUDI         = #{const_str G_FILE_ATTRIBUTE_MOUNTABLE_HAL_UDI}
fileAttributeTimeModified            = #{const_str G_FILE_ATTRIBUTE_TIME_MODIFIED}
fileAttributeTimeModifiedUSec        = #{const_str G_FILE_ATTRIBUTE_TIME_MODIFIED_USEC}
fileAttributeTimeAccess              = #{const_str G_FILE_ATTRIBUTE_TIME_ACCESS}
fileAttributeTimeAccessUSec          = #{const_str G_FILE_ATTRIBUTE_TIME_ACCESS_USEC}
fileAttributeTimeChanged             = #{const_str G_FILE_ATTRIBUTE_TIME_CHANGED}
fileAttributeTimeChangedUSec         = #{const_str G_FILE_ATTRIBUTE_TIME_CHANGED_USEC}
fileAttributeTimeCreated             = #{const_str G_FILE_ATTRIBUTE_TIME_CREATED}
fileAttributeTimeCreatedUSec         = #{const_str G_FILE_ATTRIBUTE_TIME_CREATED_USEC}
fileAttributeUnixDevice              = #{const_str G_FILE_ATTRIBUTE_UNIX_DEVICE}
fileAttributeUnixInode               = #{const_str G_FILE_ATTRIBUTE_UNIX_INODE}
fileAttributeUnixMode                = #{const_str G_FILE_ATTRIBUTE_UNIX_MODE}
fileAttributeUnixNLink               = #{const_str G_FILE_ATTRIBUTE_UNIX_NLINK}
fileAttributeUnixUID                 = #{const_str G_FILE_ATTRIBUTE_UNIX_UID}
fileAttributeUnixGID                 = #{const_str G_FILE_ATTRIBUTE_UNIX_GID}
fileAttributeUnixRDev                = #{const_str G_FILE_ATTRIBUTE_UNIX_RDEV}
fileAttributeUnixBlockSize           = #{const_str G_FILE_ATTRIBUTE_UNIX_BLOCK_SIZE}
fileAttributeUnixBlocks              = #{const_str G_FILE_ATTRIBUTE_UNIX_BLOCKS}
fileAttributeDosIsMountpoint         = #{const_str G_FILE_ATTRIBUTE_UNIX_IS_MOUNTPOINT}
fileAttributeDosIsArchive            = #{const_str G_FILE_ATTRIBUTE_DOS_IS_ARCHIVE}
fileAttributeDosIsSystem             = #{const_str G_FILE_ATTRIBUTE_DOS_IS_SYSTEM}
fileAttributeOwnerUser               = #{const_str G_FILE_ATTRIBUTE_OWNER_USER}
fileAttributeOwnerUserReal           = #{const_str G_FILE_ATTRIBUTE_OWNER_USER_REAL}
fileAttributeOwnerGroup              = #{const_str G_FILE_ATTRIBUTE_OWNER_GROUP}
fileAttributeThumbnailPath           = #{const_str G_FILE_ATTRIBUTE_THUMBNAIL_PATH}
fileAttributeThumbnailingFailed      = #{const_str G_FILE_ATTRIBUTE_THUMBNAILING_FAILED}
#if GLIB_CHECK_VERSION(2,20,0)
fileAttributePreviewIcon             = #{const_str G_FILE_ATTRIBUTE_PREVIEW_ICON}
#endif
fileAttributeFilesystemSize          = #{const_str G_FILE_ATTRIBUTE_FILESYSTEM_SIZE}
fileAttributeFilesystemFree          = #{const_str G_FILE_ATTRIBUTE_FILESYSTEM_FREE}
fileAttributeFilesystemType          = #{const_str G_FILE_ATTRIBUTE_FILESYSTEM_TYPE}
fileAttributeFilesystemReadonly      = #{const_str G_FILE_ATTRIBUTE_FILESYSTEM_READONLY}
fileAttributeGVfsBackend             = #{const_str G_FILE_ATTRIBUTE_GVFS_BACKEND}
fileAttributeSELinuxContext          = #{const_str G_FILE_ATTRIBUTE_SELINUX_CONTEXT}
fileAttributeTrashItemCount          = #{const_str G_FILE_ATTRIBUTE_TRASH_ITEM_COUNT}
fileAttributeFilesystemUsePreview    = #{const_str G_FILE_ATTRIBUTE_FILESYSTEM_USE_PREVIEW}
fileAttributeStandardDescription     = #{const_str G_FILE_ATTRIBUTE_STANDARD_DESCRIPTION}
#if GLIB_CHECK_VERSION(2,24,0)
fileAttributeTrashOrigPath           = #{const_str G_FILE_ATTRIBUTE_TRASH_ORIG_PATH}
fileAttributeTrashDeletionDate       = #{const_str G_FILE_ATTRIBUTE_TRASH_DELETION_DATE}
#endif
