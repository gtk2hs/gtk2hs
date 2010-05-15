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
-- | GFile is a high level abstraction for manipulating files on a virtual file system. GFiles are
-- lightweight, immutable objects that do no I/O upon creation. It is necessary to understand that
-- GFile objects do not represent files, merely an identifier for a file. All file content I/O is
-- implemented as streaming operations (see GInputStream and GOutputStream).
-- 
-- To construct a GFile, you can use: 'fileNewForPath' if
-- you have a URI.  'fileNewForCommandlineArg'
-- from a utf8 string gotten from 'fileGetParseName'.
-- 
-- One way to think of a GFile is as an abstraction of a pathname. For normal files the system pathname
-- is what is stored internally, but as GFiles are extensible it could also be something else that
-- corresponds to a pathname in a userspace implementation of a filesystem.
-- 
-- GFiles make up hierarchies of directories and files that correspond to the files on a
-- filesystem. You can move through the file system with GFile using 'fileGetParent' to get an
-- identifier for the parent directory, 'fileGetChild' to get a child within a directory,
-- 'fileResolveRelativePath' to resolve a relative path between two GFiles. There can be multiple
-- hierarchies, so you may not end up at the same root if you repeatedly call 'fileGetParent' on
-- two different files.
-- 
-- All GFiles have a basename (get with 'fileGetBasename'. These names are byte strings that are
-- used to identify the file on the filesystem (relative to its parent directory) and there is no
-- guarantees that they have any particular charset encoding or even make any sense at all. If you want
-- to use filenames in a user interface you should use the display name that you can get by requesting
-- the GFileAttributeStandardDisplayName attribute with 'fileQueryInfo'. This is guaranteed to
-- be in utf8 and can be used in a user interface. But always store the real basename or the GFile to
-- use to actually access the file, because there is no way to go from a display name to the actual
-- name.
-- 
-- Using GFile as an identifier has the same weaknesses as using a path in that there may be multiple
-- aliases for the same file. For instance, hard or soft links may cause two different GFiles to refer
-- to the same file. Other possible causes for aliases are: case insensitive filesystems, short and
-- long names on Fat/NTFS, or bind mounts in Linux. If you want to check if two GFiles point to the
-- same file you can query for the GFileAttributeIdFile attribute. Note that GFile does some
-- trivial canonicalization of pathnames passed in, so that trivial differences in the path string used
-- at creation (duplicated slashes, slash at end of path, "." or ".." path segments, etc) does not
-- create different GFiles.
-- 
-- Many GFile operations have both synchronous and asynchronous versions to suit your
-- application. Asynchronous versions of synchronous functions simply have _async() appended to their
-- function names. The asynchronous I/O functions call a GAsyncReadyCallback which is then used to
-- finalize the operation, producing a GAsyncResult which is then passed to the function's matching
-- _finish() operation.
-- 
-- Some GFile operations do not have synchronous analogs, as they may take a very long time to finish,
-- and blocking may leave an application unusable. Notable cases include: 'fileMountMountable' to
-- mount a mountable file.  'fileUnmountMountableWithOperation' to unmount a mountable
-- file. 'fileEjectMountableWithOperation' to eject a mountable file.
-- 
-- One notable feature of GFiles are entity tags, or "etags" for short. Entity tags are somewhat like a
-- more abstract version of the traditional mtime, and can be used to quickly determine if the file has
-- been modified from the version on the file system. See the HTTP 1.1 specification for HTTP Etag
-- headers, which are a very similar concept.

-- * Types.
    FileProgressCallback,
    FileReadMoreCallback,
    FileClass,

-- * Enums
    File(..),
    FileQueryInfoFlags(..),
    FileCreateFlags(..),
    FileCopyFlags(..),
    FileMonitorFlags(..),
    FilesystemPreviewType(..),
    FileType(..),

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
    fileQueryWritableNamespaces
    ) where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Typeable

import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.UTFString
import System.Glib.GObject

{#import System.GIO.Base#}
{#import System.GIO.Types#}
import System.GIO.File.FileAttribute

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

type FileProgressCallback = Offset -> Offset -> IO ()
type CFileProgressCallback = {# type goffset #} -> {# type goffset #} -> Ptr () -> IO ()
foreign import ccall "wrapper"
    makeFileProgressCallback :: CFileProgressCallback
                             -> IO {# type GFileProgressCallback #}

marshalFileProgressCallback :: FileProgressCallback -> IO {# type GFileProgressCallback #}
marshalFileProgressCallback fileProgressCallback =
    makeFileProgressCallback cFileProgressCallback
    where cFileProgressCallback :: CFileProgressCallback
          cFileProgressCallback cCurrentNumBytes cTotalNumBytes _ = do
            fileProgressCallback (fromIntegral cCurrentNumBytes)
                                 (fromIntegral cTotalNumBytes)

type FileReadMoreCallback = BS.ByteString -> IO Bool

-- | Constructs a GFile for a given path. This operation never fails, but the returned object might not
-- support any I/O operation if path is malformed.
fileFromPath :: FilePath -> File
fileFromPath path =
    unsafePerformIO $ withUTFString path $ \cPath -> {# call file_new_for_path #} cPath >>= takeGObject

-- | Constructs a GFile for a given URI. This operation never fails, but the returned object might not
-- support any I/O operation if uri is malformed or if the uri type is not supported.
fileFromURI :: String -> File
fileFromURI uri =
    unsafePerformIO $ withUTFString uri $ \cURI -> {# call file_new_for_uri #} cURI >>= takeGObject

-- | Creates a GFile with the given argument from the command line. The value of arg can be either a URI,
-- an absolute path or a relative path resolved relative to the current working directory. This
-- operation never fails, but the returned object might not support any I/O operation if arg points to
-- a malformed path.
fileFromCommandlineArg :: String -> File
fileFromCommandlineArg arg =
    unsafePerformIO $ withUTFString arg $ \cArg -> {# call file_new_for_commandline_arg #} cArg >>= takeGObject

-- | Constructs a GFile with the given 'name (i.e. something given by gFileGetParseName'. This
-- operation never fails, but the returned object might not support any I/O operation if the @parseName@
-- cannot be parsed.
fileFromParseName :: String -> File
fileFromParseName parseName =
    unsafePerformIO $ withUTFString parseName $ \cParseName -> {# call file_parse_name #} cParseName >>= takeGObject

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

-- | Gets the base name (the last component of the path) for a given GFile.
-- 
-- If called for the top level of a system (such as the filesystem root or a uri like sftp://host/) it
-- will return a single directory separator (and on Windows, possibly a drive letter).
-- 
-- The base name is a byte string (*not* UTF-8). It has no defined encoding or rules other than it may
-- not contain zero bytes.  If you want to use filenames in a user interface you should use the display
-- name that you can get by requesting the GFileAttributeStandardDisplayName attribute with
-- 'fileQueryInfo'.
-- 
-- This call does no blocking i/o.
fileBasename :: FileClass file => file -> String
fileBasename file =
    unsafePerformIO $ {# call file_get_basename #} (toFile file) >>= readUTFString

-- | Gets the local pathname for GFile, if one exists.
-- 
-- This call does no blocking i/o.
filePath :: FileClass file => file -> FilePath
filePath file =
    unsafePerformIO $ {# call file_get_path #} (toFile file) >>= readUTFString

-- | Gets the URI for the file.
-- 
-- This call does no blocking i/o.
fileURI :: FileClass file => file -> String
fileURI file =
    unsafePerformIO $ {# call file_get_uri #} (toFile file) >>= readUTFString

-- | Gets the parse name of the file. A parse name is a UTF-8 string that describes the file such that
-- one can get the GFile back using 'fileParseName'.
-- 
-- This is generally used to show the GFile as a nice full-pathname kind of string in a user interface,
-- like in a location entry.
-- 
-- For local files with names that can safely be converted to UTF8 the pathname is used, otherwise the
-- IRI is used (a form of URI that allows UTF8 characters unescaped).
-- 
-- This call does no blocking i/o.
fileParseName :: FileClass file => file -> String
fileParseName file =
    unsafePerformIO $ {# call file_get_parse_name #} (toFile file) >>= readUTFString

-- | Gets the parent directory for the file. If the file represents the root directory of the file
-- system, then 'Nothing' will be returned.
-- 
-- This call does no blocking i/o.
fileParent :: FileClass file => file -> Maybe File
fileParent file =
    unsafePerformIO $ {# call file_get_parent #} (toFile file) >>= maybePeek takeGObject

#if GLIB_CHECK_VERSION(2,24,0)
-- | Checks if file has a parent, and optionally, if it is parent.
-- 
-- If parent is 'Nothing' then this function returns 'True' if file has any parent at all. If parent is
-- non-'Nothing' then 'True' is only returned if file is a child of parent.
fileHasParent :: FileClass file => file -> Maybe File -> Bool 
fileHasParent file parent = 
    unsafePerformIO $ liftM toBool $
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject parent $ \parentPtr ->
                  g_file_has_parent cFile parentPtr
    where _ = {#call file_has_parent #}
#endif

-- | Gets a child of file with basename equal to name.
-- 
-- Note that the file with that specific name might not exist, but you can still have a GFile that
-- points to it. You can use this for instance to create that file.
-- 
-- This call does no blocking i/o.
fileGetChild :: FileClass file => file -> String -> File
fileGetChild file name =
    unsafePerformIO $
        withUTFString name $ \cName ->
        {# call file_get_child #} (toFile file) cName >>= takeGObject

-- | Gets the child of file for a given 'name (i.e. a UTF8 version of the name)'. If this function
-- fails, it returns 'Nothing' and error will be set. This is very useful when constructing a GFile for a
-- new file and the user entered the filename in the user interface, for instance when you select a
-- directory and type a filename in the file selector.
-- 
-- This call does no blocking i/o.
fileGetChildForDisplayName :: FileClass file => file -> String -> Maybe File
fileGetChildForDisplayName file displayName =
    unsafePerformIO $
        withUTFString displayName $ \cDisplayName ->
        propagateGError ({# call file_get_child_for_display_name #} (toFile file) cDisplayName) >>=
        maybePeek takeGObject

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
fileGetRelativePath :: (FileClass file1, FileClass file2) => file1 -> file2 -> Maybe FilePath
fileGetRelativePath file1 file2 =
    unsafePerformIO $
        {# call file_get_relative_path #} (toFile file1) (toFile file2) >>=
        maybePeek readUTFString

-- | Resolves a relative path for file to an absolute path.
-- 
-- This call does no blocking i/o.
fileResolveRelativePath :: FileClass file => file -> FilePath -> Maybe File
fileResolveRelativePath file relativePath =
    unsafePerformIO $
        withUTFString relativePath $ \cRelativePath ->
        {# call file_resolve_relative_path #} (toFile file) cRelativePath >>=
        maybePeek takeGObject

-- | Checks to see if a file is native to the platform.
-- 
-- A native file s one expressed in the platform-native filename format, e.g. \"C:\\Windows\" or
-- \"/usr/bin/\". This does not mean the file is local, as it might be on a locally mounted remote
-- filesystem.
-- 
-- On some systems non-native files may be available using the native filesystem via a userspace
-- filesystem (FUSE), in these cases this call will return @False@, but 'fileGetPath' will still
-- return a native path.
-- 
-- This call does no blocking i/o.
fileIsNative :: FileClass file => file -> Bool
fileIsNative =
    unsafePerformIO .
        liftM toBool . {# call file_is_native #} . toFile

-- | Checks to see if a 'GFile' has a given URI scheme.
-- 
-- This call does no blocking i/o.
fileHasURIScheme :: FileClass file => file -> String -> Bool
fileHasURIScheme file uriScheme =
    unsafePerformIO $
        withUTFString uriScheme $ \cURIScheme ->
        liftM toBool $ {# call file_has_uri_scheme #} (toFile file) cURIScheme

-- | Gets the URI scheme for a GFile. RFC 3986 decodes the scheme as:
-- 
-- URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
-- 
-- Common schemes include "file", "http", "ftp", etc.
-- 
-- This call does no blocking i/o.
fileURIScheme :: FileClass file => file -> String
fileURIScheme file =
    unsafePerformIO $ {# call file_get_uri_scheme #} (toFile file) >>= readUTFString

-- | Opens a file for reading. The result is a GFileInputStream that can be used to read the contents of
-- the file.
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
-- 
-- If the file does not exist, the GIoErrorNotFound error will be returned. If the file is a
-- directory, the GIoErrorIsDirectory error will be returned. Other errors are possible too, and
-- depend on what kind of filesystem the file is on.
fileRead :: FileClass file => file -> Maybe Cancellable -> IO FileInputStream
fileRead file (Just cancellable) = constructNewGObject mkFileInputStream $
    propagateGError $ {# call file_read #} (toFile file) cancellable

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
fileReadAsync file ioPriority mbCancellable callback =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject mbCancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_read_async cFile
                            (fromIntegral ioPriority)
                            cCancellable
                            cCallback
                            (castFunPtrToPtr cCallback)
    where _ = {# call file_read_async #}

-- | Finishes an asynchronous file read operation started with 'fileReadAsync'.
fileReadFinish :: FileClass file
               => file
               -> AsyncResult
               -> IO FileInputStream
fileReadFinish file asyncResult =
    propagateGError ({# call file_read_finish #} (toFile file) asyncResult) >>= takeGObject

-- | Gets an output stream for appending data to the file. If the file doesn't already exist it is
-- created.
-- 
-- By default files created are generally readable by everyone, but if you pass GFileCreatePrivate
-- in flags the file will be made readable only to the current user, to the level that is supported on
-- the target filesystem.
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
-- 
-- Some file systems don't allow all file names, and may return an GIoErrorInvalidFilename
-- error. If the file is a directory the GIoErrorIsDirectory error will be returned. Other errors
-- are possible too, and depend on what kind of filesystem the file is on.
fileAppendTo :: FileClass file
             => file
             -> [FileCreateFlags]
             -> Maybe Cancellable
             -> IO FileOutputStream
fileAppendTo file flags cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_append_to cFile (cFromFlags flags) cCancellable) >>=
        takeGObject
    where _ = {# call file_append_to #}

-- | Creates a new file and returns an output stream for writing to it. The file must not already exist.
-- 
-- By default files created are generally readable by everyone, but if you pass GFileCreatePrivate
-- in flags the file will be made readable only to the current user, to the level that is supported on
-- the target filesystem.
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
-- 
-- If a file or directory with this name already exists the GIoErrorExists error will be
-- returned. Some file systems don't allow all file names, and may return an
-- GIoErrorInvalidFilename error, and if the name is to long GIoErrorFilenameTooLong will be
-- returned. Other errors are possible too, and depend on what kind of filesystem the file is on.
fileCreate :: FileClass file
           => file
           -> [FileCreateFlags]
           -> Maybe Cancellable
           -> IO FileOutputStream
fileCreate file flags cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_create cFile (cFromFlags flags) cCancellable) >>=
        takeGObject
    where _ = {# call file_create #}

-- | Returns an output stream for overwriting the file, possibly creating a backup copy of the file
-- first. If the file doesn't exist, it will be created.
-- 
-- This will try to replace the file in the safest way possible so that any errors during the writing
-- will not affect an already existing copy of the file. For instance, for local files it may write to
-- a temporary file and then atomically rename over the destination when the stream is closed.
-- 
-- By default files created are generally readable by everyone, but if you pass GFileCreatePrivate
-- in flags the file will be made readable only to the current user, to the level that is supported on
-- the target filesystem.
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
-- 
-- If you pass in a non-'Nothing' etag value, then this value is compared to the current entity tag of the
-- file, and if they differ an GIoErrorWrongEtag error is returned. This generally means that the
-- file has been changed since you last read it. You can get the new etag from
-- 'fileOutputStreamGetEtag' after you've finished writing and closed the GFileOutputStream.
-- When you load a new file you can use 'fileInputStreamQueryInfo' to get the etag of the file.
-- 
-- If @makeBackup@ is 'True', this function will attempt to make a backup of the current file before
-- overwriting it. If this fails a GIoErrorCantCreateBackup error will be returned. If you want to
-- replace anyway, try again with @makeBackup@ set to 'False'.
-- 
-- If the file is a directory the GIoErrorIsDirectory error will be returned, and if the file is
-- some other form of non-regular file then a GIoErrorNotRegularFile error will be returned. Some
-- file systems don't allow all file names, and may return an GIoErrorInvalidFilename error, and if
-- the name is to long GIoErrorFilenameTooLong will be returned. Other errors are possible too,
-- and depend on what kind of filesystem the file is on.
fileReplace :: FileClass file
            => file
            -> Maybe String
            -> Bool
            -> [FileCreateFlags]
            -> Maybe Cancellable
            -> IO FileOutputStream
fileReplace file etag makeBackup flags cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withUTFString etag $ \cEtag ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_replace cFile
                                        cEtag
                                        (fromBool makeBackup)
                                        (cFromFlags flags) cCancellable) >>=
        takeGObject
    where _ = {# call file_replace #}

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
fileAppendToAsync file flags ioPriority cancellable callback =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_append_to_async cFile
                                 (cFromFlags flags)
                                 (fromIntegral ioPriority)
                                 cCancellable
                                 cCallback
                                 (castFunPtrToPtr cCallback)
    where _ = {# call file_append_to_async #}

-- | Finishes an asynchronous file append operation started with 'fileAppendToAsync'.
fileAppendToFinish :: FileClass file
                   => file
                   -> AsyncResult
                   -> IO FileOutputStream
fileAppendToFinish file asyncResult =
    propagateGError ({# call file_append_to_finish #} (toFile file) asyncResult) >>= takeGObject

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
fileCreateAsync file flags ioPriority cancellable callback =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_create_async cFile
                              (cFromFlags flags)
                              (fromIntegral ioPriority)
                              cCancellable
                              cCallback
                              (castFunPtrToPtr cCallback)
    where _ = {# call file_create_async #}

-- | Finishes an asynchronous file create operation started with 'fileCreateAsync'.
fileCreateFinish :: FileClass file
                   => file
                   -> AsyncResult
                   -> IO FileOutputStream
fileCreateFinish file asyncResult =
    propagateGError ({# call file_create_finish #} (toFile file) asyncResult) >>= takeGObject

-- | Asynchronously overwrites the file, replacing the contents, possibly creating a backup copy of the
-- file first.
-- 
-- For more details, see 'fileReplace' which is the synchronous version of this call.
-- 
-- When the operation is finished, callback will be called. You can then call 'fileReplaceFinish'
-- to get the result of the operation.
fileReplaceAsync :: FileClass file
                 => file
                 -> String
                 -> Bool
                 -> [FileCreateFlags]
                 -> Int
                 -> Maybe Cancellable
                 -> AsyncReadyCallback
                 -> IO ()
fileReplaceAsync file etag makeBackup flags ioPriority cancellable callback =
    withGObject (toFile file) $ \cFile ->
        withUTFString etag $ \cEtag ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_replace_async cFile
                               cEtag
                               (fromBool makeBackup)
                               (cFromFlags flags)
                               (fromIntegral ioPriority)
                               cCancellable
                               cCallback
                               (castFunPtrToPtr cCallback)
    where _ = {# call file_replace_async #}

-- | Finishes an asynchronous file replace operation started with 'fileReplaceAsync'.
fileReplaceFinish :: FileClass file
                  => file
                  -> AsyncResult
                  -> IO FileOutputStream
fileReplaceFinish file asyncResult =
    propagateGError ({# call file_replace_finish #} (toFile file) asyncResult) >>= takeGObject

-- | Gets the requested information about specified file. The result is a 'GFileInfo' object that contains
-- key-value attributes (such as the type or size of the file).
-- 
-- The attribute value is a string that specifies the file attributes that should be gathered. It is
-- not an error if it's not possible to read a particular requested attribute from a file - it just
-- won't be set. attribute should be a comma-separated list of attribute or attribute wildcards. The
-- wildcard \"*\" means all attributes, and a wildcard like \"standard::*\" means all attributes in the
-- standard namespace. An example attribute query be \"standard::*,'user'\". The standard attributes
-- are available as defines, like 'GFileAttributeStandardName'.
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'GIoErrorCancelled' will be
-- returned.
-- 
-- For symlinks, normally the information about the target of the symlink is returned, rather than
-- information about the symlink itself. However if you pass 'GFileQueryInfoNofollowSymlinks' in
-- flags the information about the symlink itself will be returned. Also, for symlinks that point to
-- non-existing files the information about the symlink itself will be returned.
-- 
-- If the file does not exist, the 'GIoErrorNotFound' error will be returned. Other errors are
-- possible too, and depend on what kind of filesystem the file is on.
fileQueryInfo :: FileClass file
              => file
              -> String
              -> [FileQueryInfoFlags]
              -> Maybe Cancellable
              -> IO FileInfo
fileQueryInfo file attributes flags cancellable =
    withGObject (toFile file) $ \cFile ->
        withUTFString attributes $ \cAttributes ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_query_info cFile cAttributes (cFromFlags flags) cCancellable) >>=
        takeGObject
    where _ = {# call file_query_info #}

-- | Asynchronously gets the requested information about specified file. The
-- result is a 'GFileInfo' object that contains key-value attributes (such as
-- type or size for the file).
--  
-- For more details, see 'fileQueryInfo' which is the synchronous version of
-- this call.
--  
-- When the operation is finished, callback will be called. You can then call
-- 'fileQueryInfoFinish' to get the result of the operation.
fileQueryInfoAsync :: FileClass file
                   => file
                   -> String
                   -> [FileQueryInfoFlags]
                   -> Int
                   -> Maybe Cancellable
                   -> AsyncReadyCallback
                   -> IO ()
fileQueryInfoAsync file attributes flags ioPriority cancellable callback =
    withGObject (toFile file) $ \cFile ->
        withUTFString attributes $ \cAttributes ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_query_info_async cFile
                                  cAttributes
                                  (cFromFlags flags)
                                  (fromIntegral ioPriority)
                                  cCancellable
                                  cCallback
                                  (castFunPtrToPtr cCallback)
    where _ = {# call file_query_info_async #}

-- | Finishes an asynchronous file info query. See 'fileQueryInfoAsync'.
fileQueryInfoFinish :: FileClass file
                    => file
                    -> AsyncResult
                    -> IO FileInfo
fileQueryInfoFinish file asyncResult =
    propagateGError ({#call file_query_info_finish #} (toFile file) asyncResult) >>= takeGObject

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
-- 'fileCreate' which will either atomically create the file or fail with a GIoErrorExists error.
-- 
-- However, in many cases an existence check is useful in a user interface, for instance to make a menu
-- item sensitive/ insensitive, so that you don't have to fool users that something is possible and
-- then just show and error dialog. If you do this, you should make sure to also handle the errors that
-- can happen due to races when you execute the operation.
fileQueryExists :: FileClass file
                => file
                -> Maybe Cancellable
                -> IO Bool
fileQueryExists file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
            liftM toBool $ g_file_query_exists cFile cCancellable
    where _ = {# call file_query_exists #}

#if GLIB_CHECK_VERSION(2,18,0)
-- | Utility function to inspect the GFileType of a file. This is implemented using 'fileQueryInfo'
-- and as such does blocking I/O.
-- 
-- The primary use case of this method is to check if a file is a regular file, directory, or symlink.
fileQueryFileType :: FileClass file 
                    => file 
                    -> [FileQueryInfoFlags]
                    -> Maybe Cancellable
                    -> IO FileType
fileQueryFileType file flags cancellable = 
    liftM (toEnum . fromIntegral) $
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
        (g_file_query_file_type cFile (cFromFlags flags) cCancellable)
    where _ = {# call file_query_file_type #}
#endif

-- | Similar to 'fileQueryInfo', but obtains information about the filesystem the file is on, rather
-- than the file itself.  For instance the amount of space available and the type of the filesystem.
-- 
-- The attribute value is a string that specifies the file attributes that should be gathered. It is
-- not an error if it's not possible to read a particular requested attribute from a file - it just
-- won't be set. attribute should be a comma-separated list of attribute or attribute wildcards. The
-- wildcard "*" means all attributes, and a wildcard like "fs:*" means all attributes in the fs
-- namespace. The standard namespace for filesystem attributes is "fs". Common attributes of interest
-- are 'FILEAttributeFilesystemSize (The Total Size Of The Filesystem In Bytes)',
-- 'FILEAttributeFilesystemFree (Number Of Bytes Available)', and GFileAttributeFilesystemType
-- (type of the filesystem).
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
-- 
-- If the file does not exist, the GIoErrorNotFound error will be returned. Other errors are
-- possible too, and depend on what kind of filesystem the file is on.
fileQueryFilesystemInfo :: FileClass file
                        => file
                        -> String
                        -> Maybe Cancellable
                        -> IO FileInfo
fileQueryFilesystemInfo file attributes cancellable =
    withGObject (toFile file) $ \cFile ->
        withUTFString attributes $ \cAttributes ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_query_filesystem_info cFile cAttributes cCancellable) >>=
        takeGObject
    where _ = {# call file_query_filesystem_info #}

-- | Asynchronously gets the requested information about the filesystem that the specified file is
-- on. The result is a GFileInfo object that contains key-value attributes (such as type or size for
-- the file).
-- 
-- For more details, see 'fileQueryFilesystemInfo' which is the synchronous version of this call.
-- 
-- When the operation is finished, callback will be called. You can then call
-- 'fileQueryInfoFinish' to get the result of the operation.
fileQueryFilesystemInfoAsync :: FileClass file
                             => file
                             -> String
                             -> Int
                             -> Maybe Cancellable
                             -> AsyncReadyCallback
                             -> IO ()
fileQueryFilesystemInfoAsync file attributes ioPriority cancellable callback =
    withGObject (toFile file) $ \cFile ->
        withUTFString attributes $ \cAttributes ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_query_filesystem_info_async cFile
                                             cAttributes
                                             (fromIntegral ioPriority)
                                             cCancellable
                                             cCallback
                                             (castFunPtrToPtr cCallback)
    where _ = {# call file_query_filesystem_info_async #}

-- | Finishes an asynchronous filesystem info query. See 'fileQueryFilesystemInfoAsync'.
fileQueryFilesystemInfoFinish :: FileClass file
                              => file
                              -> AsyncResult
                              -> IO FileInfo
fileQueryFilesystemInfoFinish file asyncResult =
    propagateGError ({# call file_query_filesystem_info_finish #} (toFile file) asyncResult) >>=
        takeGObject

-- | Returns the GAppInfo that is registered as the default application to handle the file specified by
-- file.
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
fileQueryDefaultHandler :: FileClass file
                        => file
                        -> Maybe Cancellable
                        -> IO AppInfo
fileQueryDefaultHandler file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_query_default_handler cFile cCancellable) >>=
        takeGObject
    where _ = {# call file_query_default_handler #}

-- | Gets a GMount for the GFile.
-- 
-- If the GFileIface for file does not have a mount (e.g. possibly a remote share), error will be set
-- to GIoErrorNotFound and 'Nothing' will be returned.
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
fileFindEnclosingMount :: FileClass file
                       => file
                       -> Maybe Cancellable
                       -> IO Mount
fileFindEnclosingMount file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_find_enclosing_mount cFile cCancellable) >>=
        takeGObject
    where _ = {# call file_find_enclosing_mount #}

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
fileFindEnclosingMountAsync file ioPriority cancellable callback =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_find_enclosing_mount_async cFile
                                            (fromIntegral ioPriority)
                                            cCancellable
                                            cCallback
                                            (castFunPtrToPtr cCallback)
    where _ = {# call file_find_enclosing_mount_async #}

-- | Finishes an asynchronous find mount request. See 'fileFindEnclosingMountAsync'.
fileFindEnclosingMountFinish :: FileClass file
                             => file
                             -> AsyncResult
                             -> IO Mount
fileFindEnclosingMountFinish file asyncResult =
    propagateGError ({# call file_find_enclosing_mount_finish #} (toFile file) asyncResult) >>=
        takeGObject

-- | Gets the requested information about the files in a directory. The result is a 'GFileEnumerator'
-- object that will give out 'GFileInfo' objects for all the files in the directory.
-- 
-- The attribute value is a string that specifies the file attributes that should be gathered. It is
-- not an error if it's not possible to read a particular requested attribute from a file - it just
-- won't be set. attribute should be a comma-separated list of attribute or attribute wildcards. The
-- wildcard \"*\" means all attributes, and a wildcard like \"standard::*\" means all attributes in the
-- standard namespace. An example attribute query be \"standard::*,'user'\". The standard attributes
-- are available as defines, like 'GFileAttributeStandardName'.
-- 
-- If cancellable is not @Nothing@, then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'GIoErrorCancelled' will be
-- returned.
-- 
-- If the file does not exist, the 'GIoErrorNotFound' error will be returned. If the file is not a
-- directory, the 'GFileErrorNotdir' error will be returned. Other errors are possible too.
fileEnumerateChildren :: FileClass file
                      => file
                      -> String
                      -> [FileQueryInfoFlags]
                      -> Maybe Cancellable
                      -> IO FileEnumerator
fileEnumerateChildren file attributes flags cancellable =
    withGObject (toFile file) $ \cFile ->
        withUTFString attributes $ \cAttributes ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_enumerate_children cFile cAttributes (cFromFlags flags) cCancellable) >>=
        takeGObject
    where _ = {# call file_enumerate_children #}

-- | Asynchronously gets the requested information about the files in a directory. The result is a
-- GFileEnumerator object that will give out GFileInfo objects for all the files in the directory.
-- 
-- For more details, see 'fileEnumerateChildren' which is the synchronous version of this call.
-- 
-- When the operation is finished, callback will be called. You can then call
-- 'fileEnumerateChildrenFinish' to get the result of the operation.
fileEnumerateChildrenAsync :: FileClass file
                           => file
                           -> String
                           -> [FileQueryInfoFlags]
                           -> Int
                           -> Maybe Cancellable
                           -> AsyncReadyCallback
                           -> IO ()
fileEnumerateChildrenAsync file attributes flags ioPriority cancellable callback =
    withGObject (toFile file) $ \cFile ->
        withUTFString attributes $ \cAttributes ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_enumerate_children_async cFile
                                          cAttributes
                                          (cFromFlags flags)
                                          (fromIntegral ioPriority)
                                          cCancellable
                                          cCallback
                                          (castFunPtrToPtr cCallback)
    where _ = {# call file_enumerate_children_async #}

-- | Finishes an async enumerate children operation. See 'fileEnumerateChildrenAsync'.
fileEnumerateChildrenFinish :: FileClass file
                             => file
                             -> AsyncResult
                             -> IO FileEnumerator
fileEnumerateChildrenFinish file asyncResult =
    propagateGError ({# call file_enumerate_children_finish #} (toFile file) asyncResult) >>=
        takeGObject

-- | Renames file to the specified display name.
-- 
-- The display name is converted from UTF8 to the correct encoding for the target filesystem if
-- possible and the file is renamed to this.
-- 
-- If you want to implement a rename operation in the user interface the edit name
-- (GFileAttributeStandardEditName) should be used as the initial value in the rename widget, and
-- then the result after editing should be passed to 'fileSetDisplayName'.
-- 
-- On success the resulting converted filename is returned.
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
fileSetDisplayName :: FileClass file
                   => file
                   -> String
                   -> Maybe Cancellable
                   -> IO File
fileSetDisplayName file displayName cancellable =
    withGObject (toFile file) $ \cFile ->
        withUTFString displayName $ \cDisplayName ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_set_display_name cFile cDisplayName cCancellable) >>=
        takeGObject
    where _ = {# call file_set_display_name #}

-- | Asynchronously sets the display name for a given GFile.
-- 
-- For more details, see 'fileSetDisplayName' which is the synchronous version of this call.
-- 
-- When the operation is finished, callback will be called. You can then call
-- 'fileSetDisplayNameFinish' to get the result of the operation.
fileSetDisplayNameAsync :: FileClass file
                        => file
                        -> String
                        -> Int
                        -> Maybe Cancellable
                        -> AsyncReadyCallback
                        -> IO ()
fileSetDisplayNameAsync file displayName ioPriority cancellable callback =
    withGObject (toFile file) $ \cFile ->
        withUTFString displayName $ \cDisplayName ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cCallback <- marshalAsyncReadyCallback callback
          g_file_set_display_name_async cFile
                                        cDisplayName
                                        (fromIntegral ioPriority)
                                        cCancellable
                                        cCallback
                                        (castFunPtrToPtr cCallback)
    where _ = {# call file_set_display_name_async #}

-- | Finishes setting a display name started with 'fileSetDisplayNameAsync'.
fileSetDisplayNameFinish :: FileClass file
                         => file
                         -> AsyncResult
                         -> IO File
fileSetDisplayNameFinish file asyncResult =
    propagateGError ({# call file_set_display_name_finish #} (toFile file) asyncResult) >>=
        takeGObject

-- | Deletes a file. If the file is a directory, it will only be deleted if it is empty.
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
fileDelete :: FileClass file
           => file
           -> Maybe Cancellable
           -> IO ()
fileDelete file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_delete cFile cCancellable) >> return ()
    where _ = {# call file_delete #}

-- | Sends file to the "Trashcan", if possible. This is similar to deleting it, but the user can recover
-- it before emptying the trashcan. Not all file systems support trashing, so this call can return the
-- GIoErrorNotSupported error.
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
fileTrash :: FileClass file
           => file
           -> Maybe Cancellable
           -> IO ()
fileTrash file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
        propagateGError (g_file_trash cFile cCancellable) >> return ()
    where _ = {# call file_trash #}

-- | Copies the file source to the location specified by destination. Can not handle recursive copies of
-- directories.
-- 
-- If the flag GFileCopyOverwrite is specified an already existing destination file is overwritten.
-- 
-- If the flag GFileCopyNofollowSymlinks is specified then symlinks will be copied as symlinks,
-- otherwise the target of the source symlink will be copied.
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
-- 
-- If @progressCallback@ is not 'Nothing', then the operation can be monitored by setting this to a
-- GFileProgressCallback function.  @progressCallbackData@ will be passed to this function. It is
-- guaranteed that this callback will be called after all data has been transferred with the total
-- number of bytes copied during the operation.
-- 
-- If the source file does not exist then the GIoErrorNotFound error is returned, independent on
-- the status of the destination.
-- 
-- If GFileCopyOverwrite is not specified and the target exists, then the error GIoErrorExists is
-- returned.
-- 
-- If trying to overwrite a file over a directory the GIoErrorIsDirectory error is returned. If
-- trying to overwrite a directory with a directory the GIoErrorWouldMerge error is returned.
-- 
-- If the source is a directory and the target does not exist, or GFileCopyOverwrite is specified
-- and the target is a file, then the GIoErrorWouldRecurse error is returned.
-- 
-- If you are interested in copying the GFile object itself (not the on-disk file), see 'fileDup'.
fileCopy :: (FileClass source, FileClass destination)
         => source
         -> destination
         -> [FileCopyFlags]
         -> Maybe Cancellable
         -> Maybe FileProgressCallback
         -> IO Bool
fileCopy source destination flags cancellable progressCallback =
    withGObject (toFile source) $ \cSource ->
        withGObject (toFile destination) $ \cDestination ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cProgressCallback <- maybe (return nullFunPtr) marshalFileProgressCallback progressCallback
          propagateGError $ \cError -> do
            ret <- g_file_copy cSource
                               cDestination
                               (cFromFlags flags)
                               cCancellable
                               cProgressCallback
                               nullPtr
                               cError
            when (cProgressCallback /= nullFunPtr) $
              freeHaskellFunPtr cProgressCallback
            return $ toBool ret
    where _ = {# call file_copy #}

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
fileCopyAsync source destination flags ioPriority cancellable progressCallback callback =
    withGObject (toFile source) $ \cSource ->
        withGObject (toFile destination) $ \cDestination ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cProgressCallback <- maybe (return nullFunPtr) marshalFileProgressCallback progressCallback
          cCallback <- marshalAsyncReadyCallback $ \sourceObject res -> do
                         when (cProgressCallback /= nullFunPtr) $
                           freeHaskellFunPtr cProgressCallback
                         callback sourceObject res
          g_file_copy_async cSource
                            cDestination
                            (cFromFlags flags)
                            (fromIntegral ioPriority)
                            cCancellable
                            cProgressCallback
                            nullPtr
                            cCallback
                            (castFunPtrToPtr cCallback)
    where _ = {# call file_copy_async #}

-- | Finishes copying the file started with 'fileCopyAsync'.
fileCopyFinish :: FileClass file
               => file
               -> AsyncResult
               -> IO Bool
fileCopyFinish file asyncResult =
    liftM toBool $ propagateGError ({# call file_copy_finish #} (toFile file) asyncResult)

-- | Tries to move the file or directory source to the location specified by destination. If native move
-- operations are supported then this is used, otherwise a copy + delete fallback is used. The native
-- implementation may support moving directories (for instance on moves inside the same filesystem),
-- but the fallback code does not.
-- 
-- If the flag GFileCopyOverwrite is specified an already existing destination file is overwritten.
-- 
-- If the flag GFileCopyNofollowSymlinks is specified then symlinks will be copied as symlinks,
-- otherwise the target of the source symlink will be copied.
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
-- 
-- If @progressCallback@ is not 'Nothing', then the operation can be monitored by setting this to a
-- GFileProgressCallback function.  @progressCallbackData@ will be passed to this function. It is
-- guaranteed that this callback will be called after all data has been transferred with the total
-- number of bytes copied during the operation.
-- 
-- If the source file does not exist then the GIoErrorNotFound error is returned, independent on
-- the status of the destination.
-- 
-- If GFileCopyOverwrite is not specified and the target exists, then the error GIoErrorExists is
-- returned.
-- 
-- If trying to overwrite a file over a directory the GIoErrorIsDirectory error is returned. If
-- trying to overwrite a directory with a directory the GIoErrorWouldMerge error is returned.
-- 
-- If the source is a directory and the target does not exist, or GFileCopyOverwrite is specified
-- and the target is a file, then the GIoErrorWouldRecurse error may be returned (if the native
-- move operation isn't available).
fileMove :: (FileClass source, FileClass destination)
         => source
         -> destination
         -> [FileCopyFlags]
         -> Maybe Cancellable
         -> Maybe FileProgressCallback
         -> IO Bool
fileMove source destination flags cancellable progressCallback =
    withGObject (toFile source) $ \cSource ->
        withGObject (toFile destination) $ \cDestination ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          cProgressCallback <- maybe (return nullFunPtr) marshalFileProgressCallback progressCallback
          propagateGError $ \cError -> do
            ret <- g_file_move cSource
                               cDestination
                               (cFromFlags flags)
                               cCancellable
                               cProgressCallback
                               nullPtr
                               cError
            when (cProgressCallback /= nullFunPtr) $
              freeHaskellFunPtr cProgressCallback
            return $ toBool ret
    where _ = {# call file_move #}

-- | Creates a directory. Note that this will only create a child directory of the immediate parent
-- directory of the path or URI given by the GFile. To recursively create directories, see
-- 'fileMakeDirectoryWithParents'. This function will fail if the parent directory does not
-- exist, setting error to GIoErrorNotFound. If the file system doesn't support creating
-- directories, this function will fail, setting error to GIoErrorNotSupported.
-- 
-- For a local GFile the newly created directory will have the default (current) ownership and
-- permissions of the current process.
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
fileMakeDirectory :: FileClass file
                  => file
                  -> Maybe Cancellable
                  -> IO ()
fileMakeDirectory file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          propagateGError $ g_file_make_directory cFile cCancellable
          return ()
    where _ = {# call file_make_directory #}

#if GLIB_CHECK_VERSION(2,18,0)
-- | Creates a directory and any parent directories that may not exist similar to 'mkdir -p'. If the file
-- system does not support creating directories, this function will fail, setting error to
-- GIoErrorNotSupported.
-- 
-- For a local GFile the newly created directories will have the default (current) ownership and
-- permissions of the current process.
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
fileMakeDirectoryWithParents :: FileClass file
                             => file
                             -> Maybe Cancellable
                             -> IO ()
fileMakeDirectoryWithParents file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          propagateGError $ g_file_make_directory_with_parents cFile cCancellable
          return ()
    where _ = {# call file_make_directory_with_parents #}
#endif

-- | Creates a symbolic link.
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
fileMakeSymbolicLink :: FileClass file
                     => file
                     -> String
                     -> Maybe Cancellable
                     -> IO ()
fileMakeSymbolicLink file symlinkValue cancellable =
    withGObject (toFile file) $ \cFile ->
        withUTFString symlinkValue $ \cSymlinkValue ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          propagateGError $ g_file_make_symbolic_link cFile cSymlinkValue cCancellable
          return ()
    where _ = {# call file_make_symbolic_link #}

{# pointer *FileAttributeInfoList newtype #}
takeFileAttributeInfoList :: Ptr FileAttributeInfoList
                          -> IO [FileAttributeInfo]
takeFileAttributeInfoList ptr =
    do cInfos <- liftM castPtr $ {# get FileAttributeInfoList->infos #} ptr
       cNInfos <- {# get FileAttributeInfoList->n_infos #} ptr
       infos <- peekArray (fromIntegral cNInfos) cInfos
       g_file_attribute_info_list_unref ptr
       return infos
    where _ = {# call file_attribute_info_list_unref #}

-- | Obtain the list of settable attributes for the file.
-- 
-- Returns the type and full attribute name of all the attributes that can be set on this file. This
-- doesn't mean setting it will always succeed though, you might get an access failure, or some
-- specific file may not support a specific attribute.
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
fileQuerySettableAttributes :: FileClass file
                            => file
                            -> Maybe Cancellable
                            -> IO [FileAttributeInfo]
fileQuerySettableAttributes file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          ptr <- propagateGError $ g_file_query_settable_attributes cFile cCancellable
          infos <- takeFileAttributeInfoList ptr
          return infos
    where _ = {# call file_query_settable_attributes #}

-- | Obtain the list of attribute namespaces where new attributes can be created by a user. An example of
-- this is extended attributes (in the "xattr" namespace).
-- 
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error GIoErrorCancelled will be
-- returned.
fileQueryWritableNamespaces :: FileClass file
                            => file
                            -> Maybe Cancellable
                            -> IO [FileAttributeInfo]
fileQueryWritableNamespaces file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable -> do
          ptr <- propagateGError $ g_file_query_writable_namespaces cFile cCancellable
          infos <- takeFileAttributeInfoList ptr
          return infos
    where _ = {# call file_query_writable_namespaces #}

