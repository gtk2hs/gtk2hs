{-# LANGUAGE CPP, DeriveDataTypeable #-}
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
module System.GIO.File.File (
-- * Details
--
-- | 'File' is a high level abstraction for manipulating files on a virtual file system. 'File's are
-- lightweight, immutable objects that do no I/O upon creation. It is necessary to understand that
-- 'File' objects do not represent files, merely an identifier for a file. All file content I/O is
-- implemented as streaming operations (see GInputStream and GOutputStream).
--
-- To construct a 'File', you can use: 'fileFromPath' if
-- you have a URI.  'fileNewForCommandlineArg'
-- from a utf8 string gotten from 'fileGetParseName'.
--
-- One way to think of a 'File' is as an abstraction of a pathname. For normal files the system pathname
-- is what is stored internally, but as 'File's are extensible it could also be something else that
-- corresponds to a pathname in a userspace implementation of a filesystem.
--
-- 'File's make up hierarchies of directories and files that correspond to the files on a
-- filesystem. You can move through the file system with 'File' using 'fileGetParent' to get an
-- identifier for the parent directory, 'fileGetChild' to get a child within a directory,
-- 'fileResolveRelativePath' to resolve a relative path between two 'File's. There can be multiple
-- hierarchies, so you may not end up at the same root if you repeatedly call 'fileGetParent' on
-- two different files.
--
-- All 'File's have a basename (get with 'fileGetBasename'. These names are byte strings that are
-- used to identify the file on the filesystem (relative to its parent directory) and there is no
-- guarantees that they have any particular charset encoding or even make any sense at all. If you want
-- to use filenames in a user interface you should use the display name that you can get by requesting
-- the 'FileAttributeStandardDisplayName' attribute with 'fileQueryInfo'. This is guaranteed to
-- be in utf8 and can be used in a user interface. But always store the real basename or the 'File' to
-- use to actually access the file, because there is no way to go from a display name to the actual
-- name.
--
-- Using 'File' as an identifier has the same weaknesses as using a path in that there may be multiple
-- aliases for the same file. For instance, hard or soft links may cause two different 'File's to refer
-- to the same file. Other possible causes for aliases are: case insensitive filesystems, short and
-- long names on Fat/NTFS, or bind mounts in Linux. If you want to check if two 'File's point to the
-- same file you can query for the 'FileAttributeIdFile' attribute. Note that 'File' does some
-- trivial canonicalization of pathnames passed in, so that trivial differences in the path string used
-- at creation (duplicated slashes, slash at end of path, "." or ".." path segments, etc) does not
-- create different 'File's.
--
-- Many 'File' operations have both synchronous and asynchronous versions to suit your
-- application. Asynchronous versions of synchronous functions simply have _async() appended to their
-- function names. The asynchronous I/O functions call a 'AsyncReadyCallback' which is then used to
-- finalize the operation, producing a 'AsyncResult' which is then passed to the function's matching
-- _finish() operation.
--
-- Some 'File' operations do not have synchronous analogs, as they may take a very long time to finish,
-- and blocking may leave an application unusable. Notable cases include: 'fileMountMountable' to
-- mount a mountable file.  'fileUnmountMountableWithOperation' to unmount a mountable
-- file. 'fileEjectMountableWithOperation' to eject a mountable file.
--
-- One notable feature of 'File's are entity tags, or "etags" for short. Entity tags are somewhat like a
-- more abstract version of the traditional mtime, and can be used to quickly determine if the file has
-- been modified from the version on the file system. See the HTTP 1.1 specification for HTTP Etag
-- headers, which are a very similar concept.

-- * Types.
    File(..),
    FileClass,
    FileProgressCallback,
    FileReadMoreCallback,
    Offset,
    FileInputStream,
    FileInputStreamClass,
    FileOutputStream,
    FileOutputStreamClass,
    InputStream,
    InputStreamClass,
    OutputStream,
    OutputStreamClass,
    BufferedInputStream,
    BufferedInputStreamClass,
    BufferedOutputStream,
    BufferedOutputStreamClass,
    MemoryInputStream,
    MemoryInputStreamClass,
    MemoryOutputStream,
    MemoryOutputStreamClass,
    FilterInputStream,
    FilterInputStreamClass,
    FilterOutputStream,
    FilterOutputStreamClass,
    DataInputStream,
    DataInputStreamClass,
    DataOutputStream,
    DataOutputStreamClass,

-- * Enums
    FileType (..),
    FileCopyFlags (..),
    FileQueryInfoFlags (..),
    FileCreateFlags (..),
    FileMonitorFlags (..),
    MountMountFlags (..),
    MountUnmountFlags (..),

-- * Methods
    fileFromPath,
    fileFromURI,
    fileFromCommandlineArg,
    fileFromParseName,
    fileEqual,
    fileBasename,
    filePath,
#if GLIB_CHECK_VERSION(2,24,0)
    fileHasParent,
#endif
    fileURI,
    fileParseName,
    fileGetChild,
    fileGetChildForDisplayName,
    fileHasPrefix,
    fileGetRelativePath,
    fileResolveRelativePath,
    fileIsNative,
    fileHasURIScheme,
    fileURIScheme,
    fileRead,
    fileReadAsync,
    fileReadFinish,
    fileAppendTo,
    fileCreate,
    fileReplace,
    fileAppendToAsync,
    fileAppendToFinish,
    fileCreateAsync,
    fileCreateFinish,
    fileReplaceAsync,
    fileReplaceFinish,
    fileQueryInfo,
    fileQueryInfoAsync,
    fileQueryInfoFinish,
    fileQueryExists,
#if GLIB_CHECK_VERSION(2,18,0)
    fileQueryFileType,
#endif
    fileQueryFilesystemInfo,
    fileQueryFilesystemInfoAsync,
    fileQueryFilesystemInfoFinish,
    fileQueryDefaultHandler,
    fileFindEnclosingMount,
    fileFindEnclosingMountAsync,
    fileFindEnclosingMountFinish,
    fileEnumerateChildren,
    fileEnumerateChildrenAsync,
    fileEnumerateChildrenFinish,
    fileSetDisplayName,
    fileSetDisplayNameAsync,
    fileSetDisplayNameFinish,
    fileDelete,
    fileTrash,
    fileCopy,
    fileCopyAsync,
    fileCopyFinish,
    fileMove,
    fileMakeDirectory,
#if GLIB_CHECK_VERSION(2,18,0)
    fileMakeDirectoryWithParents,
#endif
    fileMakeSymbolicLink,
    fileQuerySettableAttributes,
    fileQueryWritableNamespaces,

    fileSetAttributesFromInfo,
    fileSetAttributesFromInfoAsync,
    fileSetAttributesFinish,
    fileSetAttributeString,
    fileSetAttributeByteString,
    fileSetAttributeWord32,
    fileSetAttributeInt32,
    fileSetAttributeWord64,
    fileSetAttributeInt64,
    fileCopyAttributes,

    fileMonitorDirectory,
    fileMonitorFile,
#if GLIB_CHECK_VERSION(2,18,0)
    fileMonitor,
#endif

    fileMountMountable,
    fileMountMountableFinish,
#if GLIB_CHECK_VERSION(2,22,0)
    fileUnmountMountableWithOperation,
    fileUnmountMountableWithOperationFinish,
    fileEjectMountableWithOperation,
    fileEjectMountableWithOperationFinish,
    fileStartMountable,
    fileStartMountableFinish,
    fileStopMountable,
    fileStopMountableFinish,
    filePollMountable,
    filePollMountableFinish,
    fileMountEnclosingVolume,
    fileMountEnclosingVolumeFinish,
    fileSupportsThreadContexts,
#endif
) where

import Control.Monad
import Data.Typeable
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import Data.ByteString (useAsCString)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer)
import System.GIO.Enums
import System.GIO.File.FileAttribute
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.GObject
import System.Glib.UTFString
{#import System.GIO.Async.AsyncResult#}
{#import System.GIO.Types#}

import qualified Data.ByteString as BS

{# context lib = "gio" prefix = "g" #}

type Offset = {# type goffset #}

{#pointer GFileProgressCallback #}

type FileProgressCallback = Offset -> Offset -> IO ()

foreign import ccall "wrapper" mkFileProgressCallback ::
     ({#type goffset #} -> {#type goffset #} -> Ptr () -> IO ()) -> IO GFileProgressCallback

marshalFileProgressCallback :: FileProgressCallback -> IO GFileProgressCallback
marshalFileProgressCallback fileProgressCallback =
    mkFileProgressCallback $ \ cCurrentNumBytes cTotalNumBytes _ -> do
      fileProgressCallback
        (fromIntegral cCurrentNumBytes)
        (fromIntegral cTotalNumBytes)

type FileReadMoreCallback = BS.ByteString -> IO Bool

-- | Constructs a 'File' for a given path. This operation never fails, but the returned object might not
-- support any I/O operation if path is malformed.
fileFromPath :: ByteString -> File
fileFromPath path =
    unsafePerformIO $ wrapNewGObject mkFile $
    useAsCString path $ \cPath -> {# call file_new_for_path #} cPath

-- | Constructs a 'File' for a given URI. This operation never fails, but the returned object might not
-- support any I/O operation if uri is malformed or if the uri type is not supported.
fileFromURI :: GlibString string => string -> File
fileFromURI uri =
    unsafePerformIO $ wrapNewGObject mkFile $
    withUTFString uri $ \cURI -> {# call file_new_for_uri #} cURI

-- | Creates a 'File' with the given argument from the command line. The value of arg can be either a URI,
-- an absolute path or a relative path resolved relative to the current working directory. This
-- operation never fails, but the returned object might not support any I/O operation if arg points to
-- a malformed path.
fileFromCommandlineArg :: ByteString -> File
fileFromCommandlineArg arg =
    unsafePerformIO $ wrapNewGObject mkFile $
    useAsCString arg $ \cArg -> {# call file_new_for_commandline_arg #} cArg

-- | Constructs a 'File' with the given name (i.e. something given by 'fileParseName'. This
-- operation never fails, but the returned object might not support any I/O operation if the @parseName@
-- cannot be parsed.
fileFromParseName :: GlibString string => string -> File
fileFromParseName parseName =
    unsafePerformIO $ wrapNewGObject mkFile $
    withUTFString parseName $ \cParseName -> {# call file_parse_name #} cParseName

-- | Compare two file descriptors for equality. This test is also used to
--   implement the '(==)' function, that is, comparing two descriptions
--   will compare their content, not the pointers to the two structures.
--
fileEqual :: (FileClass file1, FileClass file2)
          => file1 -> file2 -> Bool
fileEqual file1 file2 =
    unsafePerformIO $ liftM toBool $ {# call file_equal #} (toFile file1) (toFile file2)

instance Eq File where
    (==) = fileEqual

-- | Gets the base name (the last component of the path) for a given 'File'.
--
-- If called for the top level of a system (such as the filesystem root or a uri like sftp://host/) it
-- will return a single directory separator (and on Windows, possibly a drive letter).
--
-- The base name is a byte string (*not* UTF-8). It has no defined encoding or rules other than it may
-- not contain zero bytes.  If you want to use filenames in a user interface you should use the display
-- name that you can get by requesting the 'FileAttributeStandardDisplayName' attribute with
-- 'fileQueryInfo'.
--
-- This call does no blocking i/o.
fileBasename :: FileClass file => file -> ByteString
fileBasename file =
    unsafePerformIO $ do
    sPtr <- {# call file_get_basename #}
               (toFile file)
    sLen <- lengthArray0 0 sPtr
    unsafePackCStringFinalizer (castPtr sPtr) (fromIntegral sLen)
        ({#call unsafe g_free#} (castPtr sPtr))

-- | Gets the local pathname for 'File', if one exists.
--
-- This call does no blocking i/o.
filePath :: FileClass file => file -> ByteString
filePath file =
    unsafePerformIO $ do
    sPtr <- {# call file_get_path #}
                (toFile file)
    sLen <- lengthArray0 0 sPtr
    unsafePackCStringFinalizer (castPtr sPtr) (fromIntegral sLen)
        ({#call unsafe g_free#} (castPtr sPtr))

-- | Gets the URI for the file.
--
-- This call does no blocking i/o.
fileURI :: (FileClass file, GlibString string) => file -> string
fileURI file =
    unsafePerformIO $ {# call file_get_uri #} (toFile file) >>= readUTFString

-- | Gets the parse name of the file. A parse name is a UTF-8 string that describes the file such that
-- one can get the 'File' back using 'fileParseName'.
--
-- This is generally used to show the 'File' as a nice full-pathname kind of string in a user interface,
-- like in a location entry.
--
-- For local files with names that can safely be converted to UTF8 the pathname is used, otherwise the
-- IRI is used (a form of URI that allows UTF8 characters unescaped).
--
-- This call does no blocking i/o.
fileParseName :: (FileClass file, GlibString string) => file -> string
fileParseName file =
    unsafePerformIO $ {# call file_get_parse_name #} (toFile file) >>= readUTFString

-- | Gets the parent directory for the file. If the file represents the root directory of the file
-- system, then 'Nothing' will be returned.
--
-- This call does no blocking i/o.
fileParent :: FileClass file => file -> Maybe File
fileParent file =
    unsafePerformIO $ maybeNull (wrapNewGObject mkFile) $
    {# call file_get_parent #} (toFile file)

#if GLIB_CHECK_VERSION(2,24,0)
-- | Checks if file has a parent, and optionally, if it is parent.
--
-- If parent is 'Nothing' then this function returns 'True' if file has any parent at all. If parent is
-- non-'Nothing' then 'True' is only returned if file is a child of parent.
fileHasParent :: FileClass file => file -> Maybe File -> Bool
fileHasParent file parent =
    unsafePerformIO $ liftM toBool $
    {#call g_file_has_parent#}
    (toFile file)
    (fromMaybe (File nullForeignPtr) parent)
#endif

-- | Gets a child of file with basename equal to name.
--
-- Note that the file with that specific name might not exist, but you can still have a 'File' that
-- points to it. You can use this for instance to create that file.
--
-- This call does no blocking i/o.
fileGetChild :: FileClass file => file -> ByteString -> File
fileGetChild file name =
    unsafePerformIO $ wrapNewGObject mkFile $
        useAsCString name $ \cName ->
        {# call file_get_child #} (toFile file) cName

-- | Gets the child of file for a given 'name (i.e. a UTF8 version of the name)'. If this function
-- fails, it throws a GError. This is very useful when constructing a 'File' for a
-- new file and the user entered the filename in the user interface, for instance when you select a
-- directory and type a filename in the file selector.
--
-- This call does no blocking i/o.
fileGetChildForDisplayName :: (FileClass file, GlibString string) => file -> string -> File
fileGetChildForDisplayName file displayName =
    unsafePerformIO $ (wrapNewGObject mkFile) $
        withUTFString displayName $ \cDisplayName ->
        propagateGError ({# call file_get_child_for_display_name #} (toFile file) cDisplayName)

-- | Checks whether file has the prefix specified by prefix. In other word, if the names of inital
-- elements of files pathname match prefix. Only full pathname elements are matched, so a path like
-- /foo is not considered a prefix of /foobar, only of / foo/bar.
--
-- This call does no i/o, as it works purely on names. As such it can sometimes return 'False' even if
-- file is inside a prefix (from a filesystem point of view), because the prefix of file is an alias of
-- prefix.
fileHasPrefix :: (FileClass file1, FileClass file2) => file1 -> file2 -> Bool
fileHasPrefix file1 file2 =
    unsafePerformIO $
        liftM toBool $ {# call file_has_prefix #} (toFile file1) (toFile file2)

-- | Gets the path for descendant relative to parent.
--
-- This call does no blocking i/o.
fileGetRelativePath :: (FileClass file1, FileClass file2) => file1 -> file2 -> Maybe ByteString
fileGetRelativePath file1 file2 =
    unsafePerformIO $ do
    sPtr <- {# call file_get_relative_path #} (toFile file1) (toFile file2)
    if sPtr == nullPtr
       then return Nothing
       else do
         sLen <- lengthArray0 0 sPtr
         liftM Just $ unsafePackCStringFinalizer (castPtr sPtr) (fromIntegral sLen)
                          ({#call unsafe g_free#} (castPtr sPtr))

-- | Resolves a relative path for file to an absolute path.
--
-- This call does no blocking i/o.
fileResolveRelativePath :: FileClass file => file -> ByteString -> Maybe File
fileResolveRelativePath file relativePath =
    unsafePerformIO $ maybeNull (wrapNewGObject mkFile) $
        useAsCString relativePath $ \cRelativePath ->
        {# call file_resolve_relative_path #} (toFile file) cRelativePath

-- | Checks to see if a file is native to the platform.
--
-- A native file s one expressed in the platform-native filename format, e.g. \"C:\Windows\" or
-- \"/usr/bin/\". This does not mean the file is local, as it might be on a locally mounted remote
-- filesystem.
--
-- On some systems non-native files may be available using the native filesystem via a userspace
-- filesystem (FUSE), in these cases this call will return 'False', but 'fileGetPath' will still
-- return a native path.
--
-- This call does no blocking i/o.
fileIsNative :: FileClass file => file -> Bool
fileIsNative =
    unsafePerformIO .
        liftM toBool . {# call file_is_native #} . toFile

-- | Checks to see if a 'File' has a given URI scheme.
--
-- This call does no blocking i/o.
fileHasURIScheme :: (FileClass file, GlibString string) => file -> string -> Bool
fileHasURIScheme file uriScheme =
    unsafePerformIO $
        withUTFString uriScheme $ \cURIScheme ->
        liftM toBool $ {# call file_has_uri_scheme #} (toFile file) cURIScheme

-- | Gets the URI scheme for a 'File'. RFC 3986 decodes the scheme as:
--
-- URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
--
-- Common schemes include "file", "http", "ftp", etc.
--
-- This call does no blocking i/o.
fileURIScheme :: (FileClass file, GlibString string) => file -> string
fileURIScheme file =
    unsafePerformIO $ {# call file_get_uri_scheme #} (toFile file) >>= readUTFString

-- | Opens a file for reading. The result is a 'FileInputStream' that can be used to read the contents of
-- the file.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
--
-- If the file does not exist, the 'IoErrorNotFound' error will be returned. If the file is a
-- directory, the 'IoErrorIsDirectory' error will be returned. Other errors are possible too, and
-- depend on what kind of filesystem the file is on.
fileRead :: FileClass file => file -> Maybe Cancellable -> IO FileInputStream
fileRead file cancellable =
    wrapNewGObject mkFileInputStream $
            propagateGError ({#call g_file_read#}
                              (toFile file)
                              (fromMaybe (Cancellable nullForeignPtr) cancellable))

-- | Asynchronously opens file for reading.
--
-- For more details, see 'fileRead' which is the synchronous version of this call.
--
-- When the operation is finished, callback will be called. You can then call 'fileReadFinish' to
-- get the result of the operation.
fileReadAsync :: FileClass file
              => file
              -> Int
              -> Maybe Cancellable
              -> AsyncReadyCallback
              -> IO ()
fileReadAsync file ioPriority mbCancellable callback = do
  cCallback <- marshalAsyncReadyCallback callback
  {#call g_file_read_async#}
     (toFile file)
     (fromIntegral ioPriority)
     (fromMaybe (Cancellable nullForeignPtr) mbCancellable)
     cCallback
     (castFunPtrToPtr cCallback)

-- | Finishes an asynchronous file read operation started with 'fileReadAsync'.
fileReadFinish :: FileClass file
               => file
               -> AsyncResult
               -> IO FileInputStream
fileReadFinish file asyncResult =
    wrapNewGObject mkFileInputStream $
    propagateGError ({# call file_read_finish #} (toFile file) asyncResult)

-- | Gets an output stream for appending data to the file. If the file doesn't already exist it is
-- created.
--
-- By default files created are generally readable by everyone, but if you pass 'FileCreatePrivate'
-- in flags the file will be made readable only to the current user, to the level that is supported on
-- the target filesystem.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
--
-- Some file systems don't allow all file names, and may return an 'IoErrorInvalidFilename'
-- error. If the file is a directory the 'IoErrorIsDirectory' error will be returned. Other errors
-- are possible too, and depend on what kind of filesystem the file is on.
fileAppendTo :: FileClass file
             => file
             -> [FileCreateFlags]
             -> Maybe Cancellable
             -> IO FileOutputStream
fileAppendTo file flags cancellable =
    wrapNewGObject mkFileOutputStream $
        propagateGError ({#call g_file_append_to #}
                           (toFile file)
                           ((fromIntegral . fromFlags) flags)
                           (fromMaybe (Cancellable nullForeignPtr) cancellable)
                        )

-- | Creates a new file and returns an output stream for writing to it. The file must not already exist.
--
-- By default files created are generally readable by everyone, but if you pass 'FileCreatePrivate'
-- in flags the file will be made readable only to the current user, to the level that is supported on
-- the target filesystem.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
--
-- If a file or directory with this name already exists the 'IoErrorExists' error will be
-- returned. Some file systems don't allow all file names, and may return an
-- 'IoErrorInvalidFilename' error, and if the name is to long 'IoErrorFilenameTooLong' will be
-- returned. Other errors are possible too, and depend on what kind of filesystem the file is on.
fileCreate :: FileClass file
           => file
           -> [FileCreateFlags]
           -> Maybe Cancellable
           -> IO FileOutputStream
fileCreate file flags cancellable =
    wrapNewGObject mkFileOutputStream $
        propagateGError ({#call g_file_create #}
                          (toFile file)
                         ((fromIntegral . fromFlags) flags)
                         (fromMaybe (Cancellable nullForeignPtr) cancellable)
                        )

-- | Returns an output stream for overwriting the file, possibly creating a backup copy of the file
-- first. If the file doesn't exist, it will be created.
--
-- This will try to replace the file in the safest way possible so that any errors during the writing
-- will not affect an already existing copy of the file. For instance, for local files it may write to
-- a temporary file and then atomically rename over the destination when the stream is closed.
--
-- By default files created are generally readable by everyone, but if you pass 'FileCreatePrivate'
-- in flags the file will be made readable only to the current user, to the level that is supported on
-- the target filesystem.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
--
-- If you pass in a non-'Nothing' etag value, then this value is compared to the current entity tag of the
-- file, and if they differ an 'IoErrorWrongEtag' error is returned. This generally means that the
-- file has been changed since you last read it. You can get the new etag from
-- 'fileOutputStreamGetEtag' after you've finished writing and closed the 'FileOutputStream'.
-- When you load a new file you can use 'fileInputStreamQueryInfo' to get the etag of the file.
--
-- If @makeBackup@ is 'True', this function will attempt to make a backup of the current file before
-- overwriting it. If this fails a 'IoErrorCantCreateBackup' error will be returned. If you want to
-- replace anyway, try again with @makeBackup@ set to 'False'.
--
-- If the file is a directory the 'IoErrorIsDirectory' error will be returned, and if the file is
-- some other form of non-regular file then a 'IoErrorNotRegularFile' error will be returned. Some
-- file systems don't allow all file names, and may return an 'IoErrorInvalidFilename' error, and if
-- the name is to long 'IoErrorFilenameTooLong' will be returned. Other errors are possible too,
-- and depend on what kind of filesystem the file is on.
fileReplace :: (FileClass file, GlibString string)
            => file
            -> Maybe string
            -> Bool
            -> [FileCreateFlags]
            -> Maybe Cancellable
            -> IO FileOutputStream
fileReplace file etag makeBackup flags cancellable =
    wrapNewGObject mkFileOutputStream $
        maybeWith withUTFString etag $ \cEtag ->
        propagateGError ({#call g_file_replace#}
                           (toFile file)
                           cEtag
                           (fromBool makeBackup)
                           ((fromIntegral . fromFlags) flags)
                           (fromMaybe (Cancellable nullForeignPtr) cancellable))

-- | Asynchronously opens file for appending.
--
-- For more details, see 'fileAppendTo' which is the synchronous version of this call.
--
-- When the operation is finished, callback will be called. You can then call 'fileAppendToFinish'
-- to get the result of the operation.
fileAppendToAsync :: FileClass file
                  => file
                  -> [FileCreateFlags]
                  -> Int
                  -> Maybe Cancellable
                  -> AsyncReadyCallback
                  -> IO ()
fileAppendToAsync file flags ioPriority cancellable callback = do
          cCallback <- marshalAsyncReadyCallback callback
          {#call g_file_append_to_async#}
            (toFile file)
            ((fromIntegral . fromFlags) flags)
            (fromIntegral ioPriority)
            (fromMaybe (Cancellable nullForeignPtr) cancellable)
            cCallback
            (castFunPtrToPtr cCallback)

-- | Finishes an asynchronous file append operation started with 'fileAppendToAsync'.
fileAppendToFinish :: FileClass file
                   => file
                   -> AsyncResult
                   -> IO FileOutputStream
fileAppendToFinish file asyncResult =
    wrapNewGObject mkFileOutputStream $
    propagateGError ({# call file_append_to_finish #} (toFile file) asyncResult)

-- | Asynchronously creates a new file and returns an output stream for writing to it. The file must not
-- already exist.
--
-- For more details, see 'fileCreate' which is the synchronous version of this call.
--
-- When the operation is finished, callback will be called. You can then call 'fileCreateFinish' to
-- get the result of the operation.
fileCreateAsync :: FileClass file
                  => file
                  -> [FileCreateFlags]
                  -> Int
                  -> Maybe Cancellable
                  -> AsyncReadyCallback
                  -> IO ()
fileCreateAsync file flags ioPriority cancellable callback = do
          cCallback <- marshalAsyncReadyCallback callback
          {#call g_file_create_async #}
            (toFile file)
            ((fromIntegral . fromFlags) flags)
            (fromIntegral ioPriority)
            (fromMaybe (Cancellable nullForeignPtr) cancellable)
            cCallback
            (castFunPtrToPtr cCallback)

-- | Finishes an asynchronous file create operation started with 'fileCreateAsync'.
fileCreateFinish :: FileClass file
                   => file
                   -> AsyncResult
                   -> IO FileOutputStream
fileCreateFinish file asyncResult =
    wrapNewGObject mkFileOutputStream $
    propagateGError ({# call file_create_finish #} (toFile file) asyncResult)

-- | Asynchronously overwrites the file, replacing the contents, possibly creating a backup copy of the
-- file first.
--
-- For more details, see 'fileReplace' which is the synchronous version of this call.
--
-- When the operation is finished, callback will be called. You can then call 'fileReplaceFinish'
-- to get the result of the operation.
fileReplaceAsync :: (FileClass file, GlibString string)
                 => file
                 -> string
                 -> Bool
                 -> [FileCreateFlags]
                 -> Int
                 -> Maybe Cancellable
                 -> AsyncReadyCallback
                 -> IO ()
fileReplaceAsync file etag makeBackup flags ioPriority cancellable callback =
        withUTFString etag $ \cEtag -> do
          cCallback <- marshalAsyncReadyCallback callback
          {#call g_file_replace_async #}
            (toFile file)
            cEtag
            (fromBool makeBackup)
            ((fromIntegral . fromFlags) flags)
            (fromIntegral ioPriority)
            (fromMaybe (Cancellable nullForeignPtr) cancellable)
            cCallback
            (castFunPtrToPtr cCallback)

-- | Finishes an asynchronous file replace operation started with 'fileReplaceAsync'.
fileReplaceFinish :: FileClass file
                  => file
                  -> AsyncResult
                  -> IO FileOutputStream
fileReplaceFinish file asyncResult =
    wrapNewGObject mkFileOutputStream $
    propagateGError ({# call file_replace_finish #} (toFile file) asyncResult)

-- | Gets the requested information about specified file. The result is a 'FileInfo' object that contains
-- key-value attributes (such as the type or size of the file).
--
-- The attribute value is a string that specifies the file attributes that should be gathered. It is
-- not an error if it's not possible to read a particular requested attribute from a file - it just
-- won't be set. attribute should be a comma-separated list of attribute or attribute wildcards. The
-- wildcard \"*\" means all attributes, and a wildcard like \"standard::*\" means all attributes in the
-- standard namespace. An example attribute query be \"standard::*,'user'\". The standard attributes
-- are available as defines, like 'FileAttributeStandardName'.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
--
-- For symlinks, normally the information about the target of the symlink is returned, rather than
-- information about the symlink itself. However if you pass 'FileQueryInfoNofollowSymlinks' in
-- flags the information about the symlink itself will be returned. Also, for symlinks that point to
-- non-existing files the information about the symlink itself will be returned.
--
-- If the file does not exist, the 'IoErrorNotFound' error will be returned. Other errors are
-- possible too, and depend on what kind of filesystem the file is on.
fileQueryInfo :: (FileClass file, GlibString string)
              => file
              -> string
              -> [FileQueryInfoFlags]
              -> Maybe Cancellable
              -> IO FileInfo
fileQueryInfo file attributes flags cancellable =
    makeNewGObject mkFileInfo $
        withUTFString attributes $ \cAttributes ->
        propagateGError ({#call g_file_query_info #}
                           (toFile file)
                           cAttributes
                           ((fromIntegral . fromFlags) flags)
                           (fromMaybe (Cancellable nullForeignPtr) cancellable)
                        )

-- | Asynchronously gets the requested information about specified file. The result is a 'FileInfo' object
-- that contains key-value attributes (such as type or size for the file).
--
-- For more details, see 'fileQueryInfo' which is the synchronous version of this call.
--
-- When the operation is finished, callback will be called. You can then call
-- 'fileQueryInfoFinish' to get the result of the operation.
fileQueryInfoAsync :: (FileClass file, GlibString string)
                   => file
                   -> string
                   -> [FileQueryInfoFlags]
                   -> Int
                   -> Maybe Cancellable
                   -> AsyncReadyCallback
                   -> IO ()
fileQueryInfoAsync file attributes flags ioPriority cancellable callback =
        withUTFString attributes $ \cAttributes -> do
          cCallback <- marshalAsyncReadyCallback callback
          {#call g_file_query_info_async #}
            (toFile file)
            cAttributes
            ((fromIntegral . fromFlags) flags)
            (fromIntegral ioPriority)
            (fromMaybe (Cancellable nullForeignPtr) cancellable)
            cCallback
            (castFunPtrToPtr cCallback)

-- | Finishes an asynchronous file info query. See 'fileQueryInfoAsync'.
fileQueryInfoFinish :: FileClass file
                    => file
                    -> AsyncResult
                    -> IO FileInfo
fileQueryInfoFinish file asyncResult =
    makeNewGObject mkFileInfo $
    propagateGError ({#call file_query_info_finish #} (toFile file) asyncResult)

-- | Utility function to check if a particular file exists. This is implemented using 'fileQueryInfo'
-- and as such does blocking I/O.
--
-- Note that in many cases it is racy to first check for file existence and then execute something
-- based on the outcome of that, because the file might have been created or removed in between the
-- operations. The general approach to handling that is to not check, but just do the operation and
-- handle the errors as they come.
--
-- As an example of race-free checking, take the case of reading a file, and if it doesn't exist,
-- creating it. There are two racy versions: read it, and on error create it; and: check if it exists,
-- if not create it. These can both result in two processes creating the file (with perhaps a partially
-- written file as the result). The correct approach is to always try to create the file with
-- 'fileCreate' which will either atomically create the file or fail with a 'IoErrorExists' error.
--
-- However, in many cases an existence check is useful in a user interface, for instance to make a menu
-- item sensitive/ insensitive, so that you don't have to fool users that something is possible and
-- then just show and error dialog. If you do this, you should make sure to also handle the errors that
-- can happen due to races when you execute the operation.
fileQueryExists :: FileClass file
                => file
                -> Maybe Cancellable
                -> Bool
fileQueryExists file cancellable =
    unsafePerformIO $
    liftM toBool $
              {#call g_file_query_exists #}
                (toFile file)
                (fromMaybe (Cancellable nullForeignPtr) cancellable)

#if GLIB_CHECK_VERSION(2,18,0)
-- | Utility function to inspect the 'FileType' of a file. This is implemented using 'fileQueryInfo'
-- and as such does blocking I/O.
--
-- The primary use case of this method is to check if a file is a regular file, directory, or symlink.
fileQueryFileType :: FileClass file
                    => file
                    -> [FileQueryInfoFlags]
                    -> Maybe Cancellable
                    -> FileType
fileQueryFileType file flags cancellable =
    (toEnum . fromIntegral) $
    unsafePerformIO $
        ({#call g_file_query_file_type #}
           (toFile file)
           ((fromIntegral . fromFlags) flags)
           (fromMaybe (Cancellable nullForeignPtr) cancellable) )

#endif

-- | Similar to 'fileQueryInfo', but obtains information about the filesystem the file is on, rather
-- than the file itself.  For instance the amount of space available and the type of the filesystem.
--
-- The attribute value is a string that specifies the file attributes that should be gathered. It is
-- not an error if it's not possible to read a particular requested attribute from a file - it just
-- won't be set. attribute should be a comma-separated list of attribute or attribute wildcards. The
-- wildcard \"*\" means all attributes, and a wildcard like "fs:*" means all attributes in the fs
-- namespace. The standard namespace for filesystem attributes is "fs". Common attributes of interest
-- are 'FILEAttributeFilesystemSize (The Total Size Of The Filesystem In Bytes)',
-- 'FILEAttributeFilesystemFree (Number Of Bytes Available)', and 'FileAttributeFilesystemType'
-- (type of the filesystem).
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
--
-- If the file does not exist, the 'IoErrorNotFound' error will be returned. Other errors are
-- possible too, and depend on what kind of filesystem the file is on.
fileQueryFilesystemInfo :: (FileClass file, GlibString string)
                        => file
                        -> string
                        -> Maybe Cancellable
                        -> IO FileInfo
fileQueryFilesystemInfo file attributes cancellable =
    makeNewGObject mkFileInfo $
        withUTFString attributes $ \cAttributes ->
        propagateGError ({#call g_file_query_filesystem_info #}
                           (toFile file)
                           cAttributes
                           (fromMaybe (Cancellable nullForeignPtr) cancellable) )

-- | Asynchronously gets the requested information about the filesystem that the specified file is
-- on. The result is a 'FileInfo' object that contains key-value attributes (such as type or size for
-- the file).
--
-- For more details, see 'fileQueryFilesystemInfo' which is the synchronous version of this call.
--
-- When the operation is finished, callback will be called. You can then call
-- 'fileQueryInfoFinish' to get the result of the operation.
fileQueryFilesystemInfoAsync :: (FileClass file, GlibString string)
                             => file
                             -> string
                             -> Int
                             -> Maybe Cancellable
                             -> AsyncReadyCallback
                             -> IO ()
fileQueryFilesystemInfoAsync file attributes ioPriority cancellable callback =
        withUTFString attributes $ \cAttributes -> do
          cCallback <- marshalAsyncReadyCallback callback
          {#call g_file_query_filesystem_info_async #}
            (toFile file)
            cAttributes
            (fromIntegral ioPriority)
            (fromMaybe (Cancellable nullForeignPtr) cancellable)
            cCallback
            (castFunPtrToPtr cCallback)

-- | Finishes an asynchronous filesystem info query. See 'fileQueryFilesystemInfoAsync'.
fileQueryFilesystemInfoFinish :: FileClass file
                              => file
                              -> AsyncResult
                              -> IO FileInfo
fileQueryFilesystemInfoFinish file asyncResult =
    makeNewGObject mkFileInfo $
    propagateGError ({# call file_query_filesystem_info_finish #} (toFile file) asyncResult)

-- | Returns the 'AppInfo' that is registered as the default application to handle the file specified by
-- file.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileQueryDefaultHandler :: FileClass file
                        => file
                        -> Maybe Cancellable
                        -> IO AppInfo
fileQueryDefaultHandler file cancellable =
    wrapNewGObject mkAppInfo $
        propagateGError ({#call g_file_query_default_handler #}
                           (toFile file)
                           (fromMaybe (Cancellable nullForeignPtr) cancellable) )

-- | Gets a 'Mount' for the 'File'.
--
-- If the 'FileIface' for file does not have a mount (e.g. possibly a remote share), error will be set
-- to 'IoErrorNotFound' and 'Nothing' will be returned.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileFindEnclosingMount :: FileClass file
                       => file
                       -> Maybe Cancellable
                       -> IO Mount
fileFindEnclosingMount file cancellable =
    wrapNewGObject mkMount $
        propagateGError ({#call g_file_find_enclosing_mount #}
                           (toFile file)
                           (fromMaybe (Cancellable nullForeignPtr) cancellable)
                        )

-- | Asynchronously gets the mount for the file.
--
-- For more details, see 'fileFindEnclosingMount' which is the synchronous version of this call.
--
-- When the operation is finished, callback will be called. You can then call
-- 'fileFindEnclosingMountFinish' to get the result of the operation.
fileFindEnclosingMountAsync :: FileClass file
                            => file
                            -> Int
                            -> Maybe Cancellable
                            -> AsyncReadyCallback
                            -> IO ()
fileFindEnclosingMountAsync file ioPriority cancellable callback = do
          cCallback <- marshalAsyncReadyCallback callback
          {#call g_file_find_enclosing_mount_async #}
            (toFile file)
            (fromIntegral ioPriority)
            (fromMaybe (Cancellable nullForeignPtr) cancellable)
            cCallback
            (castFunPtrToPtr cCallback)

-- | Finishes an asynchronous find mount request. See 'fileFindEnclosingMountAsync'.
fileFindEnclosingMountFinish :: FileClass file
                             => file
                             -> AsyncResult
                             -> IO Mount
fileFindEnclosingMountFinish file asyncResult =
    wrapNewGObject mkMount $
    propagateGError ({# call file_find_enclosing_mount_finish #} (toFile file) asyncResult)

-- | Gets the requested information about the files in a directory. The result is a 'FileEnumerator'
-- object that will give out 'FileInfo' objects for all the files in the directory.
--
-- The attribute value is a string that specifies the file attributes that should be gathered. It is
-- not an error if it's not possible to read a particular requested attribute from a file - it just
-- won't be set. attribute should be a comma-separated list of attribute or attribute wildcards. The
-- wildcard \"*\" means all attributes, and a wildcard like \"standard::*\" means all attributes in the
-- standard namespace. An example attribute query be \"standard::*,'user'\". The standard attributes
-- are available as defines, like 'FileAttributeStandardName'.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
--
-- If the file does not exist, the 'IoErrorNotFound' error will be returned. If the file is not a
-- directory, the 'FileErrorNotdir' error will be returned. Other errors are possible too.
fileEnumerateChildren :: FileClass file
                      => file
                      -> String
                      -> [FileQueryInfoFlags]
                      -> Maybe Cancellable
                      -> IO FileEnumerator
fileEnumerateChildren file attributes flags cancellable =
    wrapNewGObject mkFileEnumerator $
        withCString attributes $ \cAttributes ->
        propagateGError ({#call g_file_enumerate_children #}
                           (toFile file)
                           cAttributes
                           ((fromIntegral . fromFlags) flags)
                           (fromMaybe (Cancellable nullForeignPtr) cancellable)
                        )

-- | Asynchronously gets the requested information about the files in a directory. The result is a
-- 'FileEnumerator' object that will give out 'FileInfo' objects for all the files in the directory.
--
-- For more details, see 'fileEnumerateChildren' which is the synchronous version of this call.
--
-- When the operation is finished, callback will be called. You can then call
-- 'fileEnumerateChildrenFinish' to get the result of the operation.
fileEnumerateChildrenAsync :: (FileClass file, GlibString string)
                           => file
                           -> string
                           -> [FileQueryInfoFlags]
                           -> Int
                           -> Maybe Cancellable
                           -> AsyncReadyCallback
                           -> IO ()
fileEnumerateChildrenAsync file attributes flags ioPriority cancellable callback =
        withUTFString attributes $ \cAttributes -> do
          cCallback <- marshalAsyncReadyCallback callback
          {#call g_file_enumerate_children_async #}
            (toFile file)
            cAttributes
            ((fromIntegral . fromFlags) flags)
            (fromIntegral ioPriority)
            (fromMaybe (Cancellable nullForeignPtr) cancellable)
            cCallback
            (castFunPtrToPtr cCallback)

-- | Finishes an async enumerate children operation. See 'fileEnumerateChildrenAsync'.
fileEnumerateChildrenFinish :: FileClass file
                             => file
                             -> AsyncResult
                             -> IO FileEnumerator
fileEnumerateChildrenFinish file asyncResult =
    wrapNewGObject mkFileEnumerator $
    propagateGError ({# call file_enumerate_children_finish #} (toFile file) asyncResult)

-- | Renames file to the specified display name.
--
-- The display name is converted from UTF8 to the correct encoding for the target filesystem if
-- possible and the file is renamed to this.
--
-- If you want to implement a rename operation in the user interface the edit name
-- ('FileAttributeStandardEditName') should be used as the initial value in the rename widget, and
-- then the result after editing should be passed to 'fileSetDisplayName'.
--
-- On success the resulting converted filename is returned.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileSetDisplayName :: (FileClass file, GlibString string)
                   => file
                   -> string
                   -> Maybe Cancellable
                   -> IO File
fileSetDisplayName file displayName cancellable =
    wrapNewGObject mkFile $
        withUTFString displayName $ \cDisplayName ->
        propagateGError ({#call g_file_set_display_name #}
                           (toFile file)
                           cDisplayName
                           (fromMaybe (Cancellable nullForeignPtr) cancellable) )

-- | Asynchronously sets the display name for a given 'File'.
--
-- For more details, see 'fileSetDisplayName' which is the synchronous version of this call.
--
-- When the operation is finished, callback will be called. You can then call
-- 'fileSetDisplayNameFinish' to get the result of the operation.
fileSetDisplayNameAsync :: (FileClass file, GlibString string)
                        => file
                        -> string
                        -> Int
                        -> Maybe Cancellable
                        -> AsyncReadyCallback
                        -> IO ()
fileSetDisplayNameAsync file displayName ioPriority cancellable callback =
        withUTFString displayName $ \cDisplayName -> do
          cCallback <- marshalAsyncReadyCallback callback
          {#call g_file_set_display_name_async #}
             (toFile file)
             cDisplayName
             (fromIntegral ioPriority)
             (fromMaybe (Cancellable nullForeignPtr) cancellable)
             cCallback
             (castFunPtrToPtr cCallback)

-- | Finishes setting a display name started with 'fileSetDisplayNameAsync'.
fileSetDisplayNameFinish :: FileClass file
                         => file
                         -> AsyncResult
                         -> IO File
fileSetDisplayNameFinish file asyncResult =
    wrapNewGObject mkFile $
    propagateGError ({# call file_set_display_name_finish #} (toFile file) asyncResult)

-- | Deletes a file. If the file is a directory, it will only be deleted if it is empty.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileDelete :: FileClass file
           => file
           -> Maybe Cancellable
           -> IO ()
fileDelete file cancellable =
        propagateGError ({#call g_file_delete #}
                           (toFile file)
                           (fromMaybe (Cancellable nullForeignPtr) cancellable)
                        ) >> return ()

-- | Sends file to the "Trashcan", if possible. This is similar to deleting it, but the user can recover
-- it before emptying the trashcan. Not all file systems support trashing, so this call can return the
-- 'IoErrorNotSupported' error.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileTrash :: FileClass file
           => file
           -> Maybe Cancellable
           -> IO ()
fileTrash file cancellable =
        propagateGError ({#call g_file_trash #}
                           (toFile file)
                           (fromMaybe (Cancellable nullForeignPtr) cancellable)
                        ) >> return ()

-- | Copies the file source to the location specified by destination. Can not handle recursive copies of
-- directories.
--
-- If the flag 'FileCopyOverwrite' is specified an already existing destination file is overwritten.
--
-- If the flag 'FileCopyNofollowSymlinks' is specified then symlinks will be copied as symlinks,
-- otherwise the target of the source symlink will be copied.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
--
-- If @progressCallback@ is not 'Nothing', then the operation can be monitored by setting this to a
-- 'FileProgressCallback' function.  @progressCallbackData@ will be passed to this function. It is
-- guaranteed that this callback will be called after all data has been transferred with the total
-- number of bytes copied during the operation.
--
-- If the source file does not exist then the 'IoErrorNotFound' error is returned, independent on
-- the status of the destination.
--
-- If 'FileCopyOverwrite' is not specified and the target exists, then the error 'IoErrorExists' is
-- returned.
--
-- If trying to overwrite a file over a directory the 'IoErrorIsDirectory' error is returned. If
-- trying to overwrite a directory with a directory the 'IoErrorWouldMerge' error is returned.
--
-- If the source is a directory and the target does not exist, or 'FileCopyOverwrite' is specified
-- and the target is a file, then the 'IoErrorWouldRecurse' error is returned.
--
-- If you are interested in copying the 'File' object itself (not the on-disk file), see 'fileDup'.
fileCopy :: (FileClass source, FileClass destination)
         => source
         -> destination
         -> [FileCopyFlags]
         -> Maybe Cancellable
         -> Maybe FileProgressCallback
         -> IO ()
fileCopy source destination flags cancellable progressCallback = do
          cProgressCallback <- maybe (return nullFunPtr) marshalFileProgressCallback progressCallback
          propagateGError $ \cError -> do
            {#call g_file_copy #}
                      (toFile source)
                      (toFile destination)
                      ((fromIntegral . fromFlags) flags)
                      (fromMaybe (Cancellable nullForeignPtr) cancellable)
                      cProgressCallback
                      nullPtr
                      cError
            when (cProgressCallback /= nullFunPtr) $
              freeHaskellFunPtr cProgressCallback

-- | Copies the file source to the location specified by destination asynchronously. For details of the
-- behaviour, see 'fileCopy'.
--
-- If @progressCallback@ is not 'Nothing', then that function that will be called just like in 'fileCopy',
-- however the callback will run in the main loop, not in the thread that is doing the I/O operation.
--
-- When the operation is finished, callback will be called. You can then call 'fileCopyFinish' to
-- get the result of the operation.
fileCopyAsync :: (FileClass source, FileClass destination)
              => source
              -> destination
              -> [FileCopyFlags]
              -> Int
              -> Maybe Cancellable
              -> Maybe FileProgressCallback
              -> AsyncReadyCallback
              -> IO ()
fileCopyAsync source destination flags ioPriority cancellable progressCallback callback = do
          cProgressCallback <- maybe (return nullFunPtr) marshalFileProgressCallback progressCallback
          cCallback <- marshalAsyncReadyCallback $ \sourceObject res -> do
                         when (cProgressCallback /= nullFunPtr) $
                           freeHaskellFunPtr cProgressCallback
                         callback sourceObject res
          {#call g_file_copy_async #}
            (toFile source)
            (toFile destination)
            ((fromIntegral . fromFlags) flags)
            (fromIntegral ioPriority)
            (fromMaybe (Cancellable nullForeignPtr) cancellable)
            cProgressCallback
            nullPtr
            cCallback
            (castFunPtrToPtr cCallback)

-- | Finishes copying the file started with 'fileCopyAsync'.
--
-- Throws a 'GError' if an error occurs.
fileCopyFinish :: FileClass file
               => file
               -> AsyncResult
               -> IO ()
fileCopyFinish file asyncResult =
    propagateGError (\gErrorPtr -> do
                       {# call file_copy_finish #}
                         (toFile file)
                         asyncResult
                         gErrorPtr
                       return ())

-- | Tries to move the file or directory source to the location specified by destination. If native move
-- operations are supported then this is used, otherwise a copy + delete fallback is used. The native
-- implementation may support moving directories (for instance on moves inside the same filesystem),
-- but the fallback code does not.
--
-- If the flag 'FileCopyOverwrite' is specified an already existing destination file is overwritten.
--
-- If the flag 'FileCopyNofollowSymlinks' is specified then symlinks will be copied as symlinks,
-- otherwise the target of the source symlink will be copied.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
--
-- If @progressCallback@ is not 'Nothing', then the operation can be monitored by setting this to a
-- 'FileProgressCallback' function.  @progressCallbackData@ will be passed to this function. It is
-- guaranteed that this callback will be called after all data has been transferred with the total
-- number of bytes copied during the operation.
--
-- If the source file does not exist then the 'IoErrorNotFound' error is returned, independent on
-- the status of the destination.
--
-- If 'FileCopyOverwrite' is not specified and the target exists, then the error 'IoErrorExists' is
-- returned.
--
-- If trying to overwrite a file over a directory the 'IoErrorIsDirectory' error is returned. If
-- trying to overwrite a directory with a directory the 'IoErrorWouldMerge' error is returned.
--
-- If the source is a directory and the target does not exist, or 'FileCopyOverwrite' is specified
-- and the target is a file, then the 'IoErrorWouldRecurse' error may be returned (if the native
-- move operation isn't available).
fileMove :: (FileClass source, FileClass destination)
         => source
         -> destination
         -> [FileCopyFlags]
         -> Maybe Cancellable
         -> Maybe FileProgressCallback
         -> IO ()
fileMove source destination flags cancellable progressCallback = do
          cProgressCallback <- maybe (return nullFunPtr) marshalFileProgressCallback progressCallback
          propagateGError $ \cError -> do
            {#call g_file_move #}
                    (toFile source)
                    (toFile destination)
                    ((fromIntegral . fromFlags) flags)
                    (fromMaybe (Cancellable nullForeignPtr) cancellable)
                    cProgressCallback
                    nullPtr
                    cError
            when (cProgressCallback /= nullFunPtr) $
              freeHaskellFunPtr cProgressCallback

-- | Creates a directory. Note that this will only create a child directory of the immediate parent
-- directory of the path or URI given by the 'File'. To recursively create directories, see
-- 'fileMakeDirectoryWithParents'. This function will fail if the parent directory does not
-- exist, setting error to 'IoErrorNotFound'. If the file system doesn't support creating
-- directories, this function will fail, setting error to 'IoErrorNotSupported'.
--
-- For a local 'File' the newly created directory will have the default (current) ownership and
-- permissions of the current process.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileMakeDirectory :: FileClass file
                  => file
                  -> Maybe Cancellable
                  -> IO ()
fileMakeDirectory file cancellable = do
          propagateGError $ {#call g_file_make_directory #}
                              (toFile file)
                              (fromMaybe (Cancellable nullForeignPtr) cancellable)
          return ()

#if GLIB_CHECK_VERSION(2,18,0)
-- | Creates a directory and any parent directories that may not exist similar to 'mkdir -p'. If the file
-- system does not support creating directories, this function will fail, setting error to
-- 'IoErrorNotSupported'.
--
-- For a local 'File' the newly created directories will have the default (current) ownership and
-- permissions of the current process.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileMakeDirectoryWithParents :: FileClass file
                             => file
                             -> Maybe Cancellable
                             -> IO ()
fileMakeDirectoryWithParents file cancellable = do
          propagateGError $ {#call g_file_make_directory_with_parents #}
                           (toFile file)
                           (fromMaybe (Cancellable nullForeignPtr) cancellable)
          return ()
#endif

-- | Creates a symbolic link.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileMakeSymbolicLink :: FileClass file
                     => file
                     -> ByteString
                     -> Maybe Cancellable
                     -> IO ()
fileMakeSymbolicLink file symlinkValue cancellable =
        useAsCString symlinkValue $ \cSymlinkValue -> do
          propagateGError $ {#call g_file_make_symbolic_link #}
                              (toFile file)
                              cSymlinkValue
                              (fromMaybe (Cancellable nullForeignPtr) cancellable)
          return ()

{# pointer *FileAttributeInfoList newtype #}
takeFileAttributeInfoList :: Ptr FileAttributeInfoList
                          -> IO [FileAttributeInfo]
takeFileAttributeInfoList ptr =
    do cInfos <- liftM castPtr $ {# get FileAttributeInfoList->infos #} ptr
       cNInfos <- {# get FileAttributeInfoList->n_infos #} ptr
       infos <- peekArray (fromIntegral cNInfos) cInfos
       return infos

-- | Obtain the list of settable attributes for the file.
--
-- Returns the type and full attribute name of all the attributes that can be set on this file. This
-- doesn't mean setting it will always succeed though, you might get an access failure, or some
-- specific file may not support a specific attribute.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileQuerySettableAttributes :: FileClass file
                            => file
                            -> Maybe Cancellable
                            -> IO [FileAttributeInfo]
fileQuerySettableAttributes file cancellable = do
          ptr <- propagateGError $
                {#call g_file_query_settable_attributes #}
                   (toFile file)
                   (fromMaybe (Cancellable nullForeignPtr) cancellable)
          infos <- takeFileAttributeInfoList ptr
          return infos

-- | Obtain the list of attribute namespaces where new attributes can be created by a user. An example of
-- this is extended attributes (in the "xattr" namespace).
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileQueryWritableNamespaces :: FileClass file
                            => file
                            -> Maybe Cancellable
                            -> IO [FileAttributeInfo]
fileQueryWritableNamespaces file cancellable = do
          ptr <- propagateGError $
                {#call g_file_query_writable_namespaces #}
                           (toFile file)
                           (fromMaybe (Cancellable nullForeignPtr) cancellable)
          infos <- takeFileAttributeInfoList ptr
          return infos

-- | Tries to set all attributes in the 'FileInfo' on the target values, not stopping on the first error.
--
-- If there is any error during this operation then error will be set to the first error. Error on
-- particular fields are flagged by setting the "status" field in the attribute value to
-- 'FileAttributeStatusErrorSetting', which means you can also detect further errors.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileSetAttributesFromInfo :: FileClass file => file
 -> FileInfo
 -> [FileQueryInfoFlags]
 -> Maybe Cancellable -- ^ @cancellable@ optional 'Cancellable' object, 'Nothing' to ignore.
 -> IO ()
fileSetAttributesFromInfo file fileInfo flags cancellable =
    propagateGError (\gErrorPtr -> do
                       {#call g_file_set_attributes_from_info #}
                            (toFile file)
                            (toFileInfo fileInfo)
                            ((fromIntegral . fromFlags) flags)
                            (fromMaybe (Cancellable nullForeignPtr) cancellable)
                            gErrorPtr
                       return ())

-- | Asynchronously sets the attributes of file with info.
--
-- For more details, see 'fileSetAttributesFromInfo' which is the synchronous version of this
-- call.
--
-- When the operation is finished, callback will be called. You can then call
-- 'fileSetAttributesFinish' to get the result of the operation.
fileSetAttributesFromInfoAsync :: FileClass file => file
 -> FileInfo
 -> [FileQueryInfoFlags]
 -> Int  -- ^ @ioPriority@ the I/O priority of the request.
 -> Maybe Cancellable -- ^ @cancellable@ optional 'Cancellable' object, 'Nothing' to ignore.
 -> AsyncReadyCallback
 -> IO () -- ^ returns     'True' if there was any error, 'False' otherwise.
fileSetAttributesFromInfoAsync file fileInfo flags ioPriority cancellable callback = do
            cCallback <- marshalAsyncReadyCallback callback
            {#call g_file_set_attributes_async #}
              (toFile file)
              (toFileInfo fileInfo)
              ((fromIntegral . fromFlags) flags)
              (fromIntegral ioPriority)
              (fromMaybe (Cancellable nullForeignPtr) cancellable)
              cCallback
              (castFunPtrToPtr cCallback)

-- | Finishes setting an attribute started in 'fileSetAttributesAsync'.
--
-- Throws a 'GError' if an error occurs.
fileSetAttributesFinish :: FileClass file
 => file
 -> AsyncResult
 -> FileInfo
 -> IO ()
fileSetAttributesFinish file asyncResult fileInfo =
    withForeignPtr (unFileInfo fileInfo) $ \cFileInfo ->
    propagateGError (\gErrorPtr -> do
                       {# call g_file_set_attributes_finish #}
                        (toFile file)
                        asyncResult
                        cFileInfo
                        gErrorPtr
                       return ())

-- | Sets attribute of type 'FileAttributeTypeString' to value. If attribute is of a different type,
-- this operation will fail.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileSetAttributeString :: (FileClass file, GlibString string) => file
 -> string -- ^ @attribute@   a string containing the attribute's name.
 -> string -- ^ @value@       a string containing the attribute's value.
 -> [FileQueryInfoFlags] -- ^ @flags@       'FileQueryInfoFlags'.
 -> Maybe Cancellable -- ^ @cancellable@ optional 'Cancellable' object, 'Nothing' to ignore.
 -> IO ()
fileSetAttributeString  file attribute value flags cancellable =
    withUTFString attribute $ \ attributePtr ->
    withUTFString value $ \ valuePtr -> do
            propagateGError (\gErrorPtr -> do
                               {#call g_file_set_attribute_string #}
                                   (toFile file)
                                   attributePtr
                                   valuePtr
                                   ((fromIntegral . fromFlags) flags)
                                   (fromMaybe (Cancellable nullForeignPtr) cancellable)
                                   gErrorPtr
                               return ())

-- | Sets attribute of type 'FileAttributeTypeByteString' to value. If attribute is of a different
-- type, this operation will fail, returning 'False'.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileSetAttributeByteString :: (FileClass file, GlibString string) => file
 -> string -- ^ @attribute@   a string containing the attribute's name.
 -> string -- ^ @value@       a string containing the attribute's value.
 -> [FileQueryInfoFlags] -- ^ @flags@       'FileQueryInfoFlags'.
 -> Maybe Cancellable -- ^ @cancellable@ optional 'Cancellable' object, 'Nothing' to ignore.
 -> IO ()
fileSetAttributeByteString  file attribute value flags cancellable =
    withUTFString attribute $ \ attributePtr ->
    withUTFString value $ \ valuePtr -> do
            propagateGError (\gErrorPtr -> do
                                {#call g_file_set_attribute_byte_string #}
                                   (toFile file)
                                   attributePtr
                                   valuePtr
                                   ((fromIntegral . fromFlags) flags)
                                   (fromMaybe (Cancellable nullForeignPtr) cancellable)
                                   gErrorPtr
                                return ())

-- | Sets attribute of type 'FileAttributeTypeUint32' to value. If attribute is of a different type,
-- this operation will fail.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileSetAttributeWord32 :: (FileClass file, GlibString string) => file
 -> string -- ^ @attribute@   a string containing the attribute's name.
 -> Word32 -- ^ @value@       a Word32 containing the attribute's new value.
 -> [FileQueryInfoFlags] -- ^ @flags@       'FileQueryInfoFlags'.
 -> Maybe Cancellable -- ^ @cancellable@ optional 'Cancellable' object, 'Nothing' to ignore.
 -> IO ()
fileSetAttributeWord32  file attribute value flags cancellable =
    withUTFString attribute $ \ attributePtr ->
            propagateGError (\gErrorPtr -> do
                                {#call g_file_set_attribute_uint32 #}
                                  (toFile file)
                                  attributePtr
                                  (fromIntegral value)
                                  ((fromIntegral . fromFlags) flags)
                                  (fromMaybe (Cancellable nullForeignPtr) cancellable)
                                  gErrorPtr
                                return ())

-- | Sets attribute of type 'FileAttributeTypeInt32' to value. If attribute is of a different type,
-- this operation will fail.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileSetAttributeInt32 :: (FileClass file, GlibString string) => file
 -> string -- ^ @attribute@   a string containing the attribute's name.
 -> Int32 -- ^ @value@       a Int32 containing the attribute's new value.
 -> [FileQueryInfoFlags] -- ^ @flags@       'FileQueryInfoFlags'.
 -> Maybe Cancellable -- ^ @cancellable@ optional 'Cancellable' object, 'Nothing' to ignore.
 -> IO ()
fileSetAttributeInt32  file attribute value flags cancellable =
    withUTFString attribute $ \ attributePtr ->
            propagateGError (\gErrorPtr -> do
                                {#call g_file_set_attribute_int32 #}
                                   (toFile file)
                                   attributePtr
                                   (fromIntegral value)
                                   ((fromIntegral . fromFlags) flags)
                                   (fromMaybe (Cancellable nullForeignPtr) cancellable)
                                   gErrorPtr
                                return ())

-- | Sets attribute of type 'FileAttributeTypeUint64' to value. If attribute is of a different type,
-- this operation will fail.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileSetAttributeWord64 :: (FileClass file, GlibString string) => file
 -> string -- ^ @attribute@   a string containing the attribute's name.
 -> Word64 -- ^ @value@       a Word64 containing the attribute's new value.
 -> [FileQueryInfoFlags] -- ^ @flags@       'FileQueryInfoFlags'.
 -> Maybe Cancellable -- ^ @cancellable@ optional 'Cancellable' object, 'Nothing' to ignore.
 -> IO ()
fileSetAttributeWord64  file attribute value flags cancellable =
    withUTFString attribute $ \ attributePtr ->
            propagateGError (\gErrorPtr -> do
                               {#call g_file_set_attribute_uint64 #}
                                   (toFile file)
                                   attributePtr
                                   (fromIntegral value)
                                   ((fromIntegral . fromFlags) flags)
                                   (fromMaybe (Cancellable nullForeignPtr) cancellable)
                                   gErrorPtr
                               return ())

-- | Sets attribute of type 'FileAttributeTypeInt64' to value. If attribute is of a different type,
-- this operation will fail.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileSetAttributeInt64 :: (FileClass file, GlibString string) => file
 -> string -- ^ @attribute@   a string containing the attribute's name.
 -> Int64 -- ^ @value@       a Int64 containing the attribute's new value.
 -> [FileQueryInfoFlags] -- ^ @flags@       'FileQueryInfoFlags'.
 -> Maybe Cancellable -- ^ @cancellable@ optional 'Cancellable' object, 'Nothing' to ignore.
 -> IO ()
fileSetAttributeInt64  file attribute value flags cancellable =
    withUTFString attribute $ \ attributePtr ->
            propagateGError (\gErrorPtr -> do
                               {#call g_file_set_attribute_int64 #}
                                   (toFile file)
                                   attributePtr
                                   (fromIntegral value)
                                   ((fromIntegral . fromFlags) flags)
                                   (fromMaybe (Cancellable nullForeignPtr) cancellable)
                                   gErrorPtr
                               return ())

-- | Copies the file attributes from source to destination.
--
-- Normally only a subset of the file attributes are copied, those that are copies in a normal file
-- copy operation (which for instance does not include e.g. owner). However if 'FileCopyAllMetadata'
-- is specified in flags, then all the metadata that is possible to copy is copied. This is useful when
-- implementing move by copy + delete source.
fileCopyAttributes :: (FileClass source, FileClass destination)
 => source -- ^ @source@      a 'File' with attributes.
 -> destination -- ^ @destination@ a 'File' to copy attributes to.
 -> [FileCopyFlags] -- ^ @flags@       a set of 'FileCopyFlags'.
 -> Maybe Cancellable -- ^ @cancellable@ optional 'Cancellable' object, 'Nothing' to ignore.
 -> IO ()
fileCopyAttributes source destination flags cancellable =
    propagateGError (\gErrorPtr -> do
                       {#call g_file_copy_attributes #}
                           (toFile source)
                           (toFile destination)
                           ((fromIntegral . fromFlags) flags)
                           (fromMaybe (Cancellable nullForeignPtr) cancellable)
                           gErrorPtr
                       return ())

-- | Obtains a directory monitor for the given file. This may fail if directory monitoring is not
-- supported.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileMonitorDirectory :: FileClass file
                     => file
                     -> [FileMonitorFlags]
                     -> Maybe Cancellable
                     -> IO FileMonitor
fileMonitorDirectory file flags cancellable =
    wrapNewGObject mkFileMonitor $
        propagateGError ({#call g_file_monitor_directory #}
                           (toFile file)
                           ((fromIntegral . fromFlags) flags)
                           (fromMaybe (Cancellable nullForeignPtr) cancellable)
                        )

-- | Obtains a file monitor for the given file. If no file notification mechanism exists, then regular
-- polling of the file is used.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileMonitorFile :: FileClass file
                     => file
                     -> [FileMonitorFlags]
                     -> Maybe Cancellable
                     -> IO FileMonitor
fileMonitorFile file flags cancellable =
    wrapNewGObject mkFileMonitor $
        propagateGError ({#call g_file_monitor_file #}
                           (toFile file)
                           ((fromIntegral . fromFlags) flags)
                           (fromMaybe (Cancellable nullForeignPtr) cancellable)
                        )

#if GLIB_CHECK_VERSION(2,18,0)
-- | Obtains a file or directory monitor for the given file, depending on the type of the file.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileMonitor :: FileClass file
                     => file
                     -> [FileMonitorFlags]
                     -> Maybe Cancellable
                     -> IO FileMonitor
fileMonitor file flags cancellable =
    wrapNewGObject mkFileMonitor $
        propagateGError ({#call g_file_monitor #}
                           (toFile file)
                           ((fromIntegral . fromFlags) flags)
                           (fromMaybe (Cancellable nullForeignPtr) cancellable)
                        )
#endif

-- | Mounts a file of type 'FileTypeMountable'. Using @mountOperation@, you can request callbacks when,
-- for instance, passwords are needed during authentication.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
--
-- When the operation is finished, callback will be called. You can then call
-- 'fileMountMountableFinish' to get the result of the operation.
fileMountMountable :: FileClass file => file
 -> [MountMountFlags] -- ^ @flags@           flags affecting the operation
 -> Maybe MountOperation -- ^ @mountOperation@ a 'MountOperation' or 'Nothing' to avoid user interaction.
 -> Maybe Cancellable -- ^ @cancellable@     optional 'Cancellable' object, 'Nothing' to ignore.
 -> AsyncReadyCallback -- ^ @callback@        a 'AsyncReadyCallback'
 -> IO ()
fileMountMountable file flags mountOperation cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_file_mount_mountable #}
        (toFile file)
        ((fromIntegral . fromFlags) flags)
        (fromMaybe (MountOperation nullForeignPtr) mountOperation)
        (fromMaybe (Cancellable nullForeignPtr) cancellable)
        cCallback
        (castFunPtrToPtr cCallback)

-- | Finishes a mount operation. See 'fileMountMountable' for details.
--
-- Finish an asynchronous mount operation that was started with 'fileMountMountable'.
fileMountMountableFinish :: FileClass file => file
 -> AsyncResult -- ^ @result@  a 'AsyncResult'
 -> IO File
fileMountMountableFinish file result =
    wrapNewGObject mkFile $
    propagateGError ({#call g_file_mount_mountable_finish#} (toFile file) result)

#if GLIB_CHECK_VERSION(2,22,0)
-- | Unmounts a file of type 'FileTypeMountable'.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
--
-- When the operation is finished, callback will be called. You can then call
-- 'fileUnmountMountableFinish' to get the result of the operation.
fileUnmountMountableWithOperation :: FileClass file => file
 -> [MountUnmountFlags] -- ^ @flags@           flags affecting the operation
 -> Maybe MountOperation -- ^ @mountOperation@ a 'MountOperation' or 'Nothing' to avoid user interaction.
 -> Maybe Cancellable -- ^ @cancellable@     optional 'Cancellable' object, 'Nothing' to ignore.
 -> AsyncReadyCallback -- ^ @callback@        a 'AsyncReadyCallback'
 -> IO ()
fileUnmountMountableWithOperation file flags mountOperation cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_file_unmount_mountable_with_operation #}
          (toFile file)
          ((fromIntegral . fromFlags) flags)
          (fromMaybe (MountOperation nullForeignPtr) mountOperation)
          (fromMaybe (Cancellable nullForeignPtr) cancellable)
          cCallback
          (castFunPtrToPtr cCallback)

-- | Finishes an unmount operation, see 'fileUnmountMountableWithOperation' for details.
--
-- Finish an asynchronous unmount operation that was started with
-- 'fileUnmountMountableWithOperation'.
--
-- Throws a 'GError' if an error occurs.
fileUnmountMountableWithOperationFinish :: FileClass file => file
 -> AsyncResult -- ^ @result@  a 'AsyncResult'
 -> IO ()
fileUnmountMountableWithOperationFinish file result =
    propagateGError (\gErrorPtr -> do
                       {#call g_file_unmount_mountable_with_operation_finish#}
                          (toFile file)
                          result
                          gErrorPtr
                       return ())

-- | Starts an asynchronous eject on a mountable. When this operation has completed, callback will be
-- called with @userUser@ data, and the operation can be finalized with
-- 'fileEjectMountableWithOperationFinish'.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileEjectMountableWithOperation :: FileClass file => file
 -> [MountUnmountFlags] -- ^ @flags@           flags affecting the operation
 -> Maybe MountOperation -- ^ @mountOperation@ a 'MountOperation' or 'Nothing' to avoid user interaction.
 -> Maybe Cancellable -- ^ @cancellable@     optional 'Cancellable' object, 'Nothing' to ignore.
 -> AsyncReadyCallback -- ^ @callback@        a 'AsyncReadyCallback'
 -> IO ()
fileEjectMountableWithOperation file flags mountOperation cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_file_eject_mountable_with_operation #}
          (toFile file)
          ((fromIntegral . fromFlags) flags)
          (fromMaybe (MountOperation nullForeignPtr) mountOperation)
          (fromMaybe (Cancellable nullForeignPtr) cancellable)
          cCallback
          (castFunPtrToPtr cCallback)

-- | Finishes an asynchronous eject operation started by 'fileEjectMountableWithOperation'.
--
-- Throws a 'GError' if an error occurs.
fileEjectMountableWithOperationFinish :: FileClass file => file
 -> AsyncResult -- ^ @result@  a 'AsyncResult'
 -> IO ()
fileEjectMountableWithOperationFinish file result =
    propagateGError (\gErrorPtr -> do
                        {#call g_file_eject_mountable_with_operation_finish#}
                              (toFile file)
                              result
                              gErrorPtr
                        return ())

-- | Starts a file of type 'FileTypeMountable'. Using @startOperation@, you can request callbacks when,
-- for instance, passwords are needed during authentication.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
--
-- When the operation is finished, callback will be called. You can then call
-- 'fileMountMountableFinish' to get the result of the operation.
fileStartMountable :: FileClass file
 => file
 -> [DriveStartFlags] -- ^ @flags@           flags affecting the start operation.
 -> Maybe MountOperation -- ^ @mountOperation@ a 'MountOperation' or 'Nothing' to avoid user interaction.
 -> Maybe Cancellable -- ^ @cancellable@     optional 'Cancellable' object, 'Nothing' to ignore.
 -> AsyncReadyCallback -- ^ @callback@        a 'AsyncReadyCallback'
 -> IO ()
fileStartMountable file flags mountOperation cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_file_start_mountable #}
        (toFile file)
        ((fromIntegral . fromFlags) flags)
        (fromMaybe (MountOperation nullForeignPtr) mountOperation)
        (fromMaybe (Cancellable nullForeignPtr) cancellable)
        cCallback
        (castFunPtrToPtr cCallback)

-- | Finishes a start operation. See 'fileStartMountable' for details.
--
-- Finish an asynchronous start operation that was started with 'fileStartMountable'.
--
-- Throws a 'GError' if an error occurs.
fileStartMountableFinish :: FileClass file
 => file
 -> AsyncResult -- ^ @result@  a 'AsyncResult'.
 -> IO ()
fileStartMountableFinish file result =
    propagateGError (\gErrorPtr -> do
                       {#call g_file_start_mountable_finish #}
                          (toFile file)
                          result
                          gErrorPtr
                       return ())

-- | Stops a file of type 'FileTypeMountable'.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
--
-- When the operation is finished, callback will be called. You can then call
-- 'fileStopMountableFinish' to get the result of the operation.
fileStopMountable :: FileClass file
 => file
 -> [MountUnmountFlags] -- ^ @flags@           flags affecting the stop operation.
 -> Maybe MountOperation -- ^ @mountOperation@ a 'MountOperation' or 'Nothing' to avoid user interaction.
 -> Maybe Cancellable -- ^ @cancellable@     optional 'Cancellable' object, 'Nothing' to ignore.
 -> AsyncReadyCallback -- ^ @callback@        a 'AsyncReadyCallback'
 -> IO ()
fileStopMountable file flags mountOperation cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_file_stop_mountable #}
        (toFile file)
        ((fromIntegral . fromFlags) flags)
        (fromMaybe (MountOperation nullForeignPtr) mountOperation)
        (fromMaybe (Cancellable nullForeignPtr) cancellable)
        cCallback
        (castFunPtrToPtr cCallback)

-- | Finishes a stop operation. See 'fileStopMountable' for details.
--
-- Finish an asynchronous stop operation that was stoped with 'fileStopMountable'.
--
-- Throws a 'GError' if an error occurs.
fileStopMountableFinish :: FileClass file
 => file
 -> AsyncResult -- ^ @result@  a 'AsyncResult'.
 -> IO ()
fileStopMountableFinish file result =
    propagateGError (\gErrorPtr -> do
                       {#call g_file_stop_mountable_finish #}
                          (toFile file)
                          result
                          gErrorPtr
                       return ())

-- | Polls a file of type 'FileTypeMountable'.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
--
-- When the operation is finished, callback will be called. You can then call
-- 'fileMountMountableFinish' to get the result of the operation.
filePollMountable :: FileClass file => file
 -> Maybe Cancellable
 -> AsyncReadyCallback
 -> IO ()
filePollMountable file cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_file_poll_mountable #}
          (toFile file)
          (fromMaybe (Cancellable nullForeignPtr) cancellable)
          cCallback
          (castFunPtrToPtr cCallback)

-- | Finishes a poll operation. See 'filePollMountable' for details.
--
-- Finish an asynchronous poll operation that was polled with 'filePollMountable'.
filePollMountableFinish :: FileClass file
 => file
 -> AsyncResult -- ^ @result@  a 'AsyncResult'.
 -> IO Bool -- ^ returns 'True' if the file was successfully ejected. 'False' otherwise.
filePollMountableFinish file result =
  liftM toBool $
    propagateGError ({#call g_file_poll_mountable_finish #} (toFile file) result)
#endif

-- | Starts a @mountOperation@, mounting the volume that contains the file location.
--
-- When this operation has completed, callback will be called with @userUser@ data, and the operation
-- can be finalized with 'fileMountEnclosingVolumeFinish'.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be
-- returned.
fileMountEnclosingVolume :: FileClass file => file
 -> [MountMountFlags] -- ^ @flags@           flags affecting the operation
 -> Maybe MountOperation -- ^ @mountOperation@ a 'MountOperation' or 'Nothing' to avoid user interaction.
 -> Maybe Cancellable -- ^ @cancellable@     optional 'Cancellable' object, 'Nothing' to ignore.
 -> AsyncReadyCallback -- ^ @callback@        a 'AsyncReadyCallback'
 -> IO ()
fileMountEnclosingVolume file flags mountOperation cancellable callback = do
      cCallback <- marshalAsyncReadyCallback callback
      {#call g_file_mount_enclosing_volume #}
        (toFile file)
        ((fromIntegral . fromFlags) flags)
        (fromMaybe (MountOperation nullForeignPtr) mountOperation)
        (fromMaybe (Cancellable nullForeignPtr) cancellable)
        cCallback
        (castFunPtrToPtr cCallback)

-- | Finishes a mount operation started by 'fileMountEnclosingVolume'.
fileMountEnclosingVolumeFinish :: FileClass file
 => file
 -> AsyncResult -- ^ @result@  a 'AsyncResult'.
 -> IO Bool -- ^ returns 'True' if the file was successfully ejected. 'False' otherwise.
fileMountEnclosingVolumeFinish file result =
  liftM toBool $
    propagateGError ({#call g_file_mount_enclosing_volume_finish #} (toFile file) result)

#if GLIB_CHECK_VERSION(2,22,0)
-- | Checks if file supports thread-default contexts. If this returns 'False', you cannot perform
-- asynchronous operations on file in a thread that has a thread-default context.
fileSupportsThreadContexts :: FileClass file => file
 -> IO Bool  -- ^ returns Whether or not file supports thread-default contexts.
fileSupportsThreadContexts file =
  liftM toBool $
  {#call g_file_supports_thread_contexts#} (toFile file)
#endif
