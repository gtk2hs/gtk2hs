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

-- #hide

-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.Gnome.VFS.Types (
  
  Handle(..),
  withHandle,
  
  Result(..),
  Error(..),
  
  OpenMode(..),
  SeekPosition(..),
  
  FileInfo(..),
  FileFlags(..),
  FileInfoFields(..),
  SetFileInfoMask(..),
  FileInfoOptions(..),
  FilePermissions(..),
  FileSize,
  FileOffset,
  FileType(..),
  InodeNumber,
  IDs,
  
  MonitorHandle(..),
  withMonitorHandle,
  MonitorCallback,
  MonitorType,
  MonitorEventType,
  
  URI(..),
  TextURI,
  newURI,
  withURI,
  ToplevelURI(..),
  newToplevelURI,
  withToplevelURI,
  URIHideOptions(..),
  
  DirectoryHandle(..),
  withDirectoryHandle,
  
  MakeURIDirs(..),
  DirectoryVisitOptions(..),
  DirectoryVisitCallback,
  DirectoryVisitResult(..),
  FindDirectoryKind(..),
  
  XferOptions(..),
  XferProgressStatus(..),
  XferOverwriteMode(..),
  XferOverwriteAction(..),
  XferErrorMode(..),
  XferErrorAction(..),
  XferPhase(..),
  XferProgressInfo(..),
  XferProgressCallback,
  XferErrorCallback,
  XferOverwriteCallback,
  XferDuplicateCallback,
  
  Cancellation(..),
  newCancellation,
  withCancellation,
  
  VolumeOpSuccessCallback,
  VolumeOpFailureCallback,
  CVolumeOpCallback,
  VolumeType,
  DeviceType,
  
  MIMEType,
  
  module System.Gnome.VFS.Hierarchy,
  
  DriveID,
  newDrive,
  withDrive,
  
  VolumeID,
  newVolume,
  withVolume,
  
  wrapVolumeMonitor,
  withVolumeMonitor
  
  ) where

import Control.Exception (assert)
import Control.Monad
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Typeable
import Data.Word (Word64)
import System.Glib.FFI
import System.Glib.Flags
{#import System.Glib.GObject#} (GObject(..),
                                GObjectClass,
                                toGObject,
                                unsafeCastGObject)
{#import System.Glib.GType#} (GType,
                              typeInstanceIsA)
{#import System.Gnome.VFS.Hierarchy#}

import System.Posix.Types (DeviceID, EpochTime)

--------------------------------------------------------------------

gTypeCast :: (GObjectClass obj, GObjectClass obj') => GType -> String
                                                   -> (obj ->  obj')
-- The usage of foreignPtrToPtr should be safe as the evaluation will only be
-- forced if the object is used afterwards
gTypeCast gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objFPtr)
      | typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr) objFPtr) gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName

--------------------------------------------------------------------

-- | The result of a file operation.
{# enum GnomeVFSResult as Result {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Bounded, Show, Typeable) #}

newtype Error = Error Result
                deriving (Show, Typeable)

-- | A handle to an open file
{# pointer *GnomeVFSHandle as Handle foreign newtype #}
withHandle (Handle cHandle) = withForeignPtr cHandle

-- | Specifies the start position for a seek operation.
{# enum GnomeVFSSeekPosition    as SeekPosition    {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Bounded, Show) #}
{# enum GnomeVFSOpenMode        as OpenMode        {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Bounded, Show) #}

--------------------------------------------------------------------

-- | A record type containing information about a file.
data FileInfo = FileInfo {
  fileInfoName        :: Maybe String,            -- ^ the name of the file,
                                                  --   without the path
  fileInfoType        :: Maybe FileType,          -- ^ the type of the file;
                                                  --   i.e. regular, directory,
                                                  --   block-device, etc.
  fileInfoPermissions :: Maybe [FilePermissions], -- ^ the permissions for the
                                                  --   file
  fileInfoFlags       :: Maybe [FileFlags],       -- ^ flags providing
                                                  --   additional information
                                                  --   about the file
  fileInfoDevice      :: Maybe DeviceID,          -- ^ the device the file
                                                  --   resides on
  fileInfoInode       :: Maybe InodeNumber,       -- ^ the inode number of the
                                                  --   file
  fileInfoLinkCount   :: Maybe Int,               -- ^ the total number of
                                                  --   hard links to the file
  fileInfoIDs         :: Maybe IDs,               -- ^ the user and group IDs
                                                  --   owning the file
  fileInfoSize        :: Maybe FileSize,          -- ^ the size of the file in
                                                  --   bytes
  fileInfoBlockCount  :: Maybe FileSize,          -- ^ the size of the file in
                                                  --   filesystem blocks
  fileInfoIOBlockSize :: Maybe FileSize,          -- ^ the optimal buffer size
                                                  --   for reading from and
                                                  --   writing to the file
  fileInfoATime       :: Maybe EpochTime,         -- ^ the time of last access
  fileInfoMTime       :: Maybe EpochTime,         -- ^ the time of last modification
  fileInfoCTime       :: Maybe EpochTime,         -- ^ the time of last attribute modification
  fileInfoSymlinkName :: Maybe String,            -- ^ the location this
                                                  --   symlink points to, if
                                                  --   @fileInfoFlags@ contains 'FileFlagsSymlink'
  fileInfoMIMEType    :: Maybe MIMEType           -- ^ the MIME-type of the
                                                  --   file
  } deriving (Eq, Show)

{# enum GnomeVFSFileInfoFields  as FileInfoFields  {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Bounded, Show) #}

-- | Options for reading information from a file.
{# enum GnomeVFSFileInfoOptions as FileInfoOptions {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Bounded, Show) #}

-- | Flags specifying additional information about a file.
{# enum GnomeVFSFileFlags       as FileFlags       {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Bounded, Show) #}

-- | Flags specifying the attributes of a file that should be changed.
{# enum GnomeVFSSetFileInfoMask as SetFileInfoMask {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Bounded, Show) #}

-- | Identifies the type of a file.
{# enum GnomeVFSFileType        as FileType        {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Show) #}

instance Flags FileInfoOptions
instance Flags FileInfoFields
instance Flags FileFlags
instance Flags SetFileInfoMask

-- | An integral type wide enough to hold the size of a file.
type FileSize        = Word64

-- | An integral type wide enough to hold an offset into a file.
type FileOffset      = Word64

-- | An integral type wide enough to hold the inode number of a file.
type InodeNumber     = Word64

-- | A pair holding the user ID and group ID of a file owner.
type IDs             = (Int, Int)

-- | UNIX-like permissions for a file.
data FilePermissions = PermSUID              -- ^ the set-user-ID bit
                     | PermSGID              -- ^ the set-group-ID bit
                     | PermSticky            -- ^ the \"sticky\" bit
                     | PermUserRead          -- ^ owner read permission
                     | PermUserWrite         -- ^ owner write permission
                     | PermUserExec          -- ^ owner execute permission
                     | PermUserAll           -- ^ equivalent to
                                             --   @['PermUserRead', 'PermUserWrite', 'PermUserExec']@
                     | PermGroupRead         -- ^ group read permission
                     | PermGroupWrite        -- ^ group write permission
                     | PermGroupExec         -- ^ group execute permission
                     | PermGroupAll          -- ^ equivalent to
                                             --   @['PermGroupRead', 'PermGroupWrite', 'PermGroupExec']@
                     | PermOtherRead         -- ^ world read permission
                     | PermOtherWrite        -- ^ world write permission
                     | PermOtherExec         -- ^ world execute permission
                     | PermOtherAll          -- ^ equivalent to
                                             --   @['PermOtherRead', 'PermOtherWrite', 'PermOtherExec']@
                     | PermAccessReadable    -- ^ readable by the current process
                     | PermAccessWritable    -- ^ writable by the current process
                     | PermAccessExecutable  -- ^ executable by the current process
                       deriving (Eq, Bounded, Show)
instance Flags FilePermissions
instance Enum FilePermissions where
    fromEnum PermSUID             =   2048
    fromEnum PermSGID             =   1024
    fromEnum PermSticky           =    512
    fromEnum PermUserRead         =    256
    fromEnum PermUserWrite        =    128
    fromEnum PermUserExec         =     64
    fromEnum PermUserAll          =    448
    fromEnum PermGroupRead        =     32
    fromEnum PermGroupWrite       =     16
    fromEnum PermGroupExec        =      8
    fromEnum PermGroupAll         =     56
    fromEnum PermOtherRead        =      4
    fromEnum PermOtherWrite       =      2
    fromEnum PermOtherExec        =      1
    fromEnum PermOtherAll         =      7
    fromEnum PermAccessReadable   =  65536
    fromEnum PermAccessWritable   = 131072
    fromEnum PermAccessExecutable = 262144
    
    toEnum   2048 = PermSUID
    toEnum   1024 = PermSGID
    toEnum    512 = PermSticky
    toEnum    256 = PermUserRead
    toEnum    128 = PermUserWrite
    toEnum     64 = PermUserExec
    toEnum    448 = PermUserAll
    toEnum     32 = PermGroupRead
    toEnum     16 = PermGroupWrite
    toEnum      8 = PermGroupExec
    toEnum     56 = PermGroupAll
    toEnum      4 = PermOtherRead
    toEnum      2 = PermOtherWrite
    toEnum      1 = PermOtherExec
    toEnum      7 = PermOtherAll
    toEnum  65536 = PermAccessReadable
    toEnum 131072 = PermAccessWritable
    toEnum 262144 = PermAccessExecutable
    
    toEnum unmatched = error ("FilePermissions.toEnum: Cannot match " ++ show unmatched)

--------------------------------------------------------------------

-- | A 'URI' is a semi-textual representation of a uniform
--   resource identifier. It contains the information about a resource
--   location encoded as canononicalized text, but also holds extra
--   information about the context in which the URI is used.
{# pointer *GnomeVFSURI as URI foreign newtype #}

newURI :: Ptr URI
       -> IO URI
newURI cURI | cURI /= nullPtr =
    liftM URI $ newForeignPtr cURI cURIFinalizer
wrapURI :: Ptr URI
        -> IO URI
wrapURI cURI | cURI /= nullPtr =
    liftM URI $ newForeignPtr_ cURI
foreign import ccall "&gnome_vfs_uri_unref"
  cURIFinalizer :: FunPtr (Ptr URI -> IO ())

withURI (URI cURI) = withForeignPtr cURI

-- | The toplevel URI element used to access resources stored on a
--   remote server.
{# pointer *GnomeVFSToplevelURI as ToplevelURI foreign newtype #}
withToplevelURI (ToplevelURI cToplevelURI) = withForeignPtr cToplevelURI
newToplevelURI :: Ptr ToplevelURI
               -> IO ToplevelURI
newToplevelURI cToplevelURI = liftM ToplevelURI $ newForeignPtr_ cToplevelURI

-- | Flags specifying which fields of a 'URI' should be hidden when
--   converted to a string using 'uriToString'.
{# enum GnomeVFSURIHideOptions as URIHideOptions {
        GNOME_VFS_URI_HIDE_NONE as URIHideNone,
        GNOME_VFS_URI_HIDE_USER_NAME as URIHideUserName,
        GNOME_VFS_URI_HIDE_PASSWORD as URIHidePassword,
        GNOME_VFS_URI_HIDE_HOST_NAME as URIHideHostName,
        GNOME_VFS_URI_HIDE_HOST_PORT as URIHideHostPort,
        GNOME_VFS_URI_HIDE_TOPLEVEL_METHOD as URIHideToplevelMethod,
        GNOME_VFS_URI_HIDE_FRAGMENT_IDENTIFIER as URIHideFragmentIdentifier
        } deriving (Eq, Bounded, Show) #}
instance Flags URIHideOptions

-- | A string that can be passed to 'uriFromString' to create a valid
--   'URI'.
type TextURI = String

--------------------------------------------------------------------

-- | A handle to an open directory.
{# pointer *GnomeVFSDirectoryHandle as DirectoryHandle foreign newtype #}
withDirectoryHandle (DirectoryHandle cDirectoryHandle) = withForeignPtr cDirectoryHandle

-- | Options controlling the way in which a directories are visited.
{# enum GnomeVFSDirectoryVisitOptions as DirectoryVisitOptions {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Bounded, Show) #}
instance Flags DirectoryVisitOptions

-- | A callback that will be called for each entry when passed to
--   'directoryVisit', 'directoryVisitURI', 'directoryVisitFiles', or
--   'directoryVisitFilesAtURI'.
type DirectoryVisitCallback =  String                  -- ^ the path of the visited file, relative to the base directory
                            -> FileInfo                -- ^ the 'FileInfo' for the visited file
                            -> Bool                    -- ^ 'True' if returning 'DirectoryVisitRecurse' will cause a loop
                            -> IO DirectoryVisitResult -- ^ the next action to be taken

-- | An enumerated value that must be returned from a
--   'DirectoryVisitCallback'. The 'directoryVisit' and related
--   functions will perform the action specified.
data DirectoryVisitResult = DirectoryVisitStop     -- ^ stop visiting files
                          | DirectoryVisitContinue -- ^ continue as normal
                          | DirectoryVisitRecurse  -- ^ recursively visit the current entry
                            deriving (Eq, Enum)

-- | Specifies which kind of directory 'findDirectory' should look for.
{# enum GnomeVFSFindDirectoryKind as FindDirectoryKind {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Show) #}

--------------------------------------------------------------------

-- | Flags that may be passed to 'makeURIFromInputWithDirs'. If the
--   path passed is non-absolute (i.e., a relative path), the
--   directories specified will be searched as well.
{# enum GnomeVFSMakeURIDirs as MakeURIDirs {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Bounded, Show) #}
instance Flags MakeURIDirs

--------------------------------------------------------------------

-- | A handle to a file-system monitor.
newtype MonitorHandle = MonitorHandle (ForeignPtr MonitorHandle, {# type GnomeVFSMonitorCallback #})
withMonitorHandle (MonitorHandle (monitorHandleForeignPtr, _)) = withForeignPtr monitorHandleForeignPtr

-- | A callback that must be passed to 'monitorAdd'.  It will be
--   called any time a file or directory is changed.
type MonitorCallback =  MonitorHandle    -- ^ the handle to a filesystem monitor
                     -> TextURI          -- ^ the URI being monitored
                     -> TextURI          -- ^ the actual file that was modified
                     -> MonitorEventType -- ^ the event that occured
                     -> IO ()

-- | The type of filesystem object that is to be monitored.
{# enum GnomeVFSMonitorType      as MonitorType      {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Bounded, Show) #}

-- | The type of event that caused a 'MonitorCallback' to be called.
{# enum GnomeVFSMonitorEventType as MonitorEventType {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Bounded, Show) #}
wrapMonitorHandle :: (Ptr MonitorHandle, {# type GnomeVFSMonitorCallback #})
                  -> IO MonitorHandle
wrapMonitorHandle (cMonitorHandle, cMonitorCallback) =
    do monitorHandleForeignPtr <- newForeignPtr_ cMonitorHandle
       return $ MonitorHandle (monitorHandleForeignPtr, cMonitorCallback)

--------------------------------------------------------------------

-- | Options controlling how the 'System.Gnome.VFS.Xfer.xferURI' and related functions behave.
{# enum GnomeVFSXferOptions         as XferOptions         {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Bounded, Show) #}
instance Flags XferOptions

{# enum GnomeVFSXferProgressStatus  as XferProgressStatus  {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Show) #}
{# enum GnomeVFSXferOverwriteMode   as XferOverwriteMode   {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Show) #}
{# enum GnomeVFSXferOverwriteAction as XferOverwriteAction {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Show) #}
{# enum GnomeVFSXferErrorMode       as XferErrorMode       {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Show) #}
{# enum GnomeVFSXferErrorAction     as XferErrorAction     {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Show) #}
{# enum GnomeVFSXferPhase           as XferPhase           {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Show) #}

data XferProgressInfo = XferProgressInfo {
    xferProgressInfoVFSStatus        :: Result,       -- ^ current VFS status
    xferProgressInfoPhase            :: XferPhase,    -- ^ phase of the transfer
    xferProgressInfoSourceName       :: Maybe String, -- ^ currently transferring source URI
    xferProgressInfoTargetName       :: Maybe String, -- ^ currently transferring target URI
    xferProgressInfoFileIndex        :: Word,         -- ^ index of the file currently being transferred
    xferProgressInfoFilesTotal       :: Word,         -- ^ total number of files being transferred
    xferProgressInfoBytesTotal       :: FileSize,     -- ^ total size of all files in bytes
    xferProgressInfoFileSize         :: FileSize,     -- ^ size of the file currently being transferred
    xferProgressInfoBytesCopied      :: FileSize,     -- ^ number of bytes already transferred in the current file
    xferProgressInfoTotalBytesCopied :: FileSize,     -- ^ total number of bytes already transferred
    xferProgressInfoTopLevelItem     :: Bool          -- ^ 'True' if the file being transferred is a top-level item;
                                                      --   'False' if it is inside a directory
    } deriving (Eq)

-- | The type of the first callback that is passed to
--   'System.Gnome.VFS.Xfer.xferURI' and related functions. This
--   callback will be called periodically during transfers that are
--   progressing normally.
type XferProgressCallback  =  XferProgressInfo -- ^ @info@ - information about the progress of the current transfer
                           -> IO Bool          -- ^ return 'Prelude.False' to abort the transfer, 'Prelude.True' otherwise.

-- | The type of the second callback that is passed to
--   'System.Gnome.VFS.Xfer.xferURI'. This callback will be called
--   whenever an error occurs.
type XferErrorCallback     =  XferProgressInfo   -- ^ @info@ - information about the progress of the current transfer
                           -> IO XferErrorAction -- ^ the action to be performed in response to the error

-- | The type of the third callback that is passed to
--   'System.Gnome.VFS.Xfer.xferURI'. This callback will be called
--   when a file would be overwritten.
type XferOverwriteCallback =  XferProgressInfo       -- ^ @info@ - information about the progress of the current transfer
                           -> IO XferOverwriteAction -- ^ the action to be performed when the target file already exists

-- | The type of the fourth callback that is passed to
--   'System.Gnome.VFS.Xfer.xferURI'. This callback will be called
--   when a duplicate filename is found.
type XferDuplicateCallback =  XferProgressInfo   -- ^ @info@ - information about the progress of the current transfer
                           -> String             -- ^ @duplicateName@ - the name of the target file
                           -> Int                -- ^ @duplicateCount@ - the number of duplicates that exist
                           -> IO (Maybe String)  -- ^ the new filename that should be used, or 'Prelude.Nothing' to abort.

--------------------------------------------------------------------

-- | An object that can be used for signalling cancellation of an
--   operation.
{# pointer *GnomeVFSCancellation as Cancellation foreign newtype #}

newCancellation :: Ptr Cancellation
                -> IO Cancellation
newCancellation cCancellationPtr | cCancellationPtr /= nullPtr =
    liftM Cancellation $ newForeignPtr cCancellationPtr cancellationFinalizer
foreign import ccall unsafe "&gnome_vfs_cancellation_destroy"
  cancellationFinalizer :: FunPtr (Ptr Cancellation -> IO ())
withCancellation (Cancellation cCancellation) = withForeignPtr cCancellation

--------------------------------------------------------------------

withVolume (Volume cVolume) = withForeignPtr cVolume
newVolume :: Ptr Volume
          -> IO Volume
newVolume cVolume | cVolume /= nullPtr =
   liftM Volume $ newForeignPtr cVolume volumeFinalizer
foreign import ccall unsafe "&gnome_vfs_volume_unref"
  volumeFinalizer :: FunPtr (Ptr Volume -> IO ())

-- | An action to be performed when a volume operation completes successfully.
type VolumeOpSuccessCallback = IO ()
-- | An action to be performed when a volume operation fails.
type VolumeOpFailureCallback =  String
                             -> String
                             -> IO ()

-- | Identifies the device type of a 'Volume' or 'Drive'.
{#enum GnomeVFSDeviceType as DeviceType {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Show) #}
-- | Identifies the type of a 'Volume'.
{#enum GnomeVFSVolumeType as VolumeType {underscoreToCase} with prefix = "GNOME_VFS" deriving (Eq, Show) #}

type CVolumeOpCallback =  {# type gboolean #}
                       -> CString
                       -> CString
                       -> Ptr ()
                       -> IO ()

--------------------------------------------------------------------

-- | Identifies a 'Drive'
type DriveID = {# type gulong #}

withDrive (Drive cDrive) = withForeignPtr cDrive
newDrive :: Ptr Drive
          -> IO Drive
newDrive cDrive | cDrive /= nullPtr =
   liftM Drive $ newForeignPtr cDrive driveFinalizer
foreign import ccall unsafe "&gnome_vfs_drive_unref"
  driveFinalizer :: FunPtr (Ptr Drive -> IO ())

--------------------------------------------------------------------

-- | Identifies a 'Volume'.
type VolumeID = {# type gulong #}

withVolumeMonitor (VolumeMonitor cVolumeMonitor) = withForeignPtr cVolumeMonitor
wrapVolumeMonitor :: Ptr VolumeMonitor
                  -> IO VolumeMonitor
wrapVolumeMonitor cVolumeMonitor | cVolumeMonitor /= nullPtr =
   liftM VolumeMonitor $ newForeignPtr_ cVolumeMonitor

--------------------------------------------------------------------

-- | A string that will be treated as a MIME-type.
type MIMEType = String
