--  GIMP Toolkit (GTK) Binding for Haskell: binding to gio -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 13-Oct-2008
--
--  Copyright (c) 2008 Peter Gavin
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
module System.GIO.FileAttribute (
    FileAttributeType (..),
    FileAttributeInfo (..),
    FileAttributeInfoFlags (..),
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
    fileAttributeDosIsArchive,
    fileAttributeDosIsSystem,
    fileAttributeOwnerUser,
    fileAttributeOwnerUserReal,
    fileAttributeOwnerGroup,
    fileAttributeThumbnailPath,
    fileAttributeThumbnailingFailed,
    fileAttributeFilesystemSize,
    fileAttributeFilesystemFree,
    fileAttributeFilesystemType,
    fileAttributeFilesystemReadonly,
    fileAttributeGVfsBackend,
    fileAttributeSELinuxContext,
    fileAttributeTrashItemCount,
    fileAttributeFilesystemUsePreview,
    fileAttributeStandardDescription,
    ) where

import System.Glib.FFI
import System.Glib.UTFString

import System.GIO.Base

data FileAttributeType = FileAttributeTypeInvalid
                       | FileAttributeTypeString
                       | FileAttributeTypeByteString
                       | FileAttributeTypeBool
                       | FileAttributeTypeWord32
                       | FileAttributeTypeInt32
                       | FileAttributeTypeWord64
                       | FileAttributeTypeInt64
                       | FileAttributeTypeObject
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
    
    fromEnum FileAttributeTypeInvalid    = #{const G_FILE_ATTRIBUTE_TYPE_INVALID}
    fromEnum FileAttributeTypeString     = #{const G_FILE_ATTRIBUTE_TYPE_STRING}
    fromEnum FileAttributeTypeByteString = #{const G_FILE_ATTRIBUTE_TYPE_BYTE_STRING}
    fromEnum FileAttributeTypeBool       = #{const G_FILE_ATTRIBUTE_TYPE_BOOLEAN}
    fromEnum FileAttributeTypeWord32     = #{const G_FILE_ATTRIBUTE_TYPE_UINT32}
    fromEnum FileAttributeTypeInt32      = #{const G_FILE_ATTRIBUTE_TYPE_INT32}
    fromEnum FileAttributeTypeWord64     = #{const G_FILE_ATTRIBUTE_TYPE_UINT64}
    fromEnum FileAttributeTypeInt64      = #{const G_FILE_ATTRIBUTE_TYPE_INT64}
    fromEnum FileAttributeTypeObject     = #{const G_FILE_ATTRIBUTE_TYPE_OBJECT}

data FileAttributeInfo =
    FileAttributeInfo
    { fileAttributeInfoName :: String
    , fileAttributeInfoType :: FileAttributeType
    , fileAttributeInfoFlags :: [FileAttributeInfoFlags]
    } deriving (Eq, Read, Show)

instance Storable FileAttributeInfo where
    sizeOf _ = #{size GFileAttributeInfo}
    alignment _ = alignment (undefined :: Ptr ())
    peek ptr = do
      retName <- #{peek GFileAttributeInfo, name} ptr >>= peekUTFString
      retType <- (#{peek GFileAttributeInfo, type} ptr :: IO CInt) >>= return . cToEnum
      retFlags <- (#{peek GFileAttributeInfo, flags} ptr :: IO CInt) >>= return . cToFlags
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
    fileAttributeDosIsArchive,
    fileAttributeDosIsSystem,
    fileAttributeOwnerUser,
    fileAttributeOwnerUserReal,
    fileAttributeOwnerGroup,
    fileAttributeThumbnailPath,
    fileAttributeThumbnailingFailed,
    fileAttributeFilesystemSize,
    fileAttributeFilesystemFree,
    fileAttributeFilesystemType,
    fileAttributeFilesystemReadonly,
    fileAttributeGVfsBackend,
    fileAttributeSELinuxContext,
    fileAttributeTrashItemCount,
    fileAttributeFilesystemUsePreview,
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
fileAttributeDosIsArchive            = #{const_str G_FILE_ATTRIBUTE_DOS_IS_ARCHIVE}
fileAttributeDosIsSystem             = #{const_str G_FILE_ATTRIBUTE_DOS_IS_SYSTEM}
fileAttributeOwnerUser               = #{const_str G_FILE_ATTRIBUTE_OWNER_USER}
fileAttributeOwnerUserReal           = #{const_str G_FILE_ATTRIBUTE_OWNER_USER_REAL}
fileAttributeOwnerGroup              = #{const_str G_FILE_ATTRIBUTE_OWNER_GROUP}
fileAttributeThumbnailPath           = #{const_str G_FILE_ATTRIBUTE_THUMBNAIL_PATH}
fileAttributeThumbnailingFailed      = #{const_str G_FILE_ATTRIBUTE_THUMBNAILING_FAILED}
fileAttributeFilesystemSize          = #{const_str G_FILE_ATTRIBUTE_FILESYSTEM_SIZE}
fileAttributeFilesystemFree          = #{const_str G_FILE_ATTRIBUTE_FILESYSTEM_FREE}
fileAttributeFilesystemType          = #{const_str G_FILE_ATTRIBUTE_FILESYSTEM_TYPE}
fileAttributeFilesystemReadonly      = #{const_str G_FILE_ATTRIBUTE_FILESYSTEM_READONLY}
fileAttributeGVfsBackend             = #{const_str G_FILE_ATTRIBUTE_GVFS_BACKEND}
fileAttributeSELinuxContext          = #{const_str G_FILE_ATTRIBUTE_SELINUX_CONTEXT}
fileAttributeTrashItemCount          = #{const_str G_FILE_ATTRIBUTE_TRASH_ITEM_COUNT}
fileAttributeFilesystemUsePreview    = #{const_str G_FILE_ATTRIBUTE_FILESYSTEM_USE_PREVIEW}
fileAttributeStandardDescription     = #{const_str G_FILE_ATTRIBUTE_STANDARD_DESCRIPTION}
