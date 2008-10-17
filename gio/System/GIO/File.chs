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
--  this library is based on the original GIO documentation,
--  Copyright (c) 2001 Seth Nickell <snickell@stanford.edu>. The
--  documentation is covered by the GNU Free Documentation License,
--  version 1.2.
--  
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.GIO.File (
    File,
    FileClass,
    FileQueryInfoFlags,
    FileCreateFlags,
    FileCopyFlags,
    FileMonitorFlags,
    FilesystemPreviewType,
    FileProgressCallback,
    FileReadMoreCallback,
    fileNewForPath,
    fileNewForURI,
    fileNewForCommandlineArg,
    fileParseName,
    fileDup,
    fileEqual,
    fileGetBasename,
    fileGetPath,
    fileGetURI,
    fileGetParseName,
    fileGetChild,
    fileGetChildForDisplayName,
    fileHasPrefix,
    fileGetRelativePath,
    fileResolveRelativePath,
    fileIsNative,
    fileHasURIScheme,
    fileGetURIScheme,
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
    fileQueryExists,
    fileQueryFilesystemInfo
    ) where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Typeable

import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.UTFString

import System.GIO.Base
{#import System.GIO.Types#}

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

type FileProgressCallback = {# type goffset #} -> {# type goffset #} -> IO ()
type FileReadMoreCallback = BS.ByteString -> IO Bool

fileNewForPath :: FilePath -> IO File
fileNewForPath path =
    withUTFString path $ {# call file_new_for_path #} >=> takeGObject

fileNewForURI :: String -> IO File
fileNewForURI uri =
    withUTFString uri $ {# call file_new_for_uri #} >=> takeGObject

fileNewForCommandlineArg :: String -> IO File
fileNewForCommandlineArg arg =
    withUTFString arg $ {# call file_new_for_commandline_arg #} >=> takeGObject

fileParseName :: String -> IO File
fileParseName parseName =
    withUTFString parseName $ {# call file_parse_name #} >=> takeGObject

fileDup :: FileClass file
        => file -> IO file
fileDup =
    {# call file_dup #} . toFile >=> takeGObject . castPtr

fileEqual :: (FileClass file1, FileClass file2)
          => file1 -> file2 -> IO Bool
fileEqual file1 file2 =
    liftM toBool $ {# call file_equal #} (toFile file1) (toFile file2)

fileGetBasename :: FileClass file => file -> IO String
fileGetBasename =
    {# call file_get_basename #} . toFile >=> readUTFString

fileGetPath :: FileClass file => file -> IO FilePath
fileGetPath =
    {# call file_get_path #} . toFile >=> readUTFString

fileGetURI :: FileClass file => file -> IO String
fileGetURI =
    {# call file_get_uri #} . toFile >=> readUTFString

fileGetParseName :: FileClass file => file -> IO String
fileGetParseName =
    {# call file_get_parse_name #} . toFile >=> readUTFString

fileGetParent :: FileClass file => file -> IO (Maybe File)
fileGetParent =
    {# call file_get_parent #} . toFile >=> maybePeek takeGObject

fileGetChild :: FileClass file => file -> String -> IO (Maybe File)
fileGetChild file name =
    withUTFString name $ {# call file_get_child #} (toFile file) >=> maybePeek takeGObject

fileGetChildForDisplayName :: FileClass file => file -> String -> IO File
fileGetChildForDisplayName file displayName =
    withUTFString displayName $ \cDisplayName ->
        propagateGError ({# call file_get_child_for_display_name #} (toFile file) cDisplayName) >>=
        takeGObject

fileHasPrefix :: (FileClass file1, FileClass file2) => file1 -> file2 -> IO Bool
fileHasPrefix file1 file2 =
    liftM toBool $ {# call file_has_prefix #} (toFile file1) (toFile file2)

fileGetRelativePath :: (FileClass file1, FileClass file2) => file1 -> file2 -> IO FilePath
fileGetRelativePath file1 file2 =
    {# call file_get_relative_path #} (toFile file1) (toFile file2) >>= readUTFString

fileResolveRelativePath :: FileClass file => file -> FilePath -> IO (Maybe File)
fileResolveRelativePath file relativePath =
    withUTFString relativePath $ \cRelativePath ->
        {# call file_resolve_relative_path #} (toFile file) cRelativePath >>=
        maybePeek takeGObject

fileIsNative :: FileClass file => file -> IO Bool
fileIsNative =
    liftM toBool . {# call file_is_native #} . toFile

fileHasURIScheme :: FileClass file => file -> String -> IO Bool
fileHasURIScheme file uriScheme =
    withUTFString uriScheme $ \cURIScheme ->
        liftM toBool $ {# call file_has_uri_scheme #} (toFile file) cURIScheme

fileGetURIScheme :: FileClass file => file -> IO String
fileGetURIScheme =
    {# call file_get_uri_scheme #} . toFile >=> readUTFString

fileRead :: FileClass file => file -> Maybe Cancellable -> IO FileInputStream
fileRead file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
            propagateGError (g_file_read cFile cCancellable) >>= takeGObject
    where _ = {# call file_read #}

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

fileReadFinish :: FileClass file
               => file
               -> AsyncResult
               -> IO FileInputStream
fileReadFinish file asyncResult =
    propagateGError ({# call file_read_finish #} (toFile file) asyncResult) >>= takeGObject

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

fileAppendToFinish :: FileClass file
                   => file
                   -> AsyncResult
                   -> IO FileOutputStream
fileAppendToFinish file asyncResult =
    propagateGError ({# call file_append_to_finish #} (toFile file) asyncResult) >>= takeGObject

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

fileCreateFinish :: FileClass file
                   => file
                   -> AsyncResult
                   -> IO FileOutputStream
fileCreateFinish file asyncResult =
    propagateGError ({# call file_create_finish #} (toFile file) asyncResult) >>= takeGObject

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

fileReplaceFinish :: FileClass file
                  => file
                  -> AsyncResult
                  -> IO FileOutputStream
fileReplaceFinish file asyncResult =
    propagateGError ({# call file_replace_finish #} (toFile file) asyncResult) >>= takeGObject

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

fileQueryExists :: FileClass file
                => file
                -> Maybe Cancellable
                -> IO Bool
fileQueryExists file cancellable =
    withGObject (toFile file) $ \cFile ->
        maybeWith withGObject cancellable $ \cCancellable ->
            liftM toBool $ g_file_query_exists cFile cCancellable
    where _ = {# call file_query_exists #}

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
