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
module System.GIO.File.FileEnumerator (
-- * Details
--
-- | 'FileEnumerator' allows you to operate on a set of 'File's, returning a 'FileInfo' structure for each
-- file enumerated (e.g.  'fileEnumerateChildren' will return a 'FileEnumerator' for each of the
-- children within a directory).
--
-- To get the next file's information from a 'FileEnumerator', use 'fileEnumeratorNextFile' or its
-- asynchronous version, 'fileEnumeratorNextFilesAsync'. Note that the asynchronous version will
-- return a list of 'FileInfo', whereas the synchronous will only return the next file in the
-- enumerator.
--
-- To close a 'FileEnumerator', use 'fileEnumeratorClose', or its asynchronous version,
-- 'fileEnumeratorCloseAsync'.
--
-- * Types
    FileEnumerator (..),
    FileEnumeratorClass,

-- * Methods
    fileEnumeratorNextFile,
    fileEnumeratorClose,
    fileEnumeratorNextFilesAsync,
    fileEnumeratorNextFilesFinish,
    fileEnumeratorCloseAsync,
    fileEnumeratorCloseFinish,
    fileEnumeratorIsClosed,
    fileEnumeratorHasPending,
    fileEnumeratorSetPending,
#if GLIB_CHECK_VERSION(2,18,0)
    fileEnumeratorGetContainer,
#endif
    ) where

import Control.Monad
import Data.Maybe (fromMaybe)
import System.GIO.Enums
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.GList
import System.Glib.GObject
import System.Glib.UTFString
{#import System.GIO.Async.AsyncResult#}
{#import System.GIO.Types#}

{# context lib = "gio" prefix = "g" #}

-- | Returns information for the next file in the enumerated object. Will block until the information is
-- available. The 'FileInfo' returned from this function will contain attributes that match the
-- attribute string that was passed when the 'FileEnumerator' was created.
--
-- On error, a 'GError' is thrown. If the enumerator is at the end, 'Nothing' will be
-- returned.
fileEnumeratorNextFile :: FileEnumeratorClass enumerator => enumerator
 -> Maybe Cancellable -- ^ @cancellable@ optional 'Cancellable' object, 'Nothing' to ignore.
 -> IO (Maybe FileInfo)     -- ^ returns     A 'FileInfo' or 'Nothing' on error or end of enumerator.
fileEnumeratorNextFile enumerator cancellable =
    checkGError ( \gErrorPtr ->
                      maybeNull (makeNewGObject mkFileInfo) $
                          {#call g_file_enumerator_next_file #}
                            (toFileEnumerator enumerator)
                            (fromMaybe (Cancellable nullForeignPtr) cancellable)
                            gErrorPtr)
                (\ _ -> return Nothing)


-- | Releases all resources used by this enumerator, making the enumerator return GIoErrorClosed on
-- all calls.
--
-- This will be automatically called when the last reference is dropped, but you might want to call
-- this function to make sure resources are released as early as possible.
--
-- Throws a 'GError' if an error occurs.
fileEnumeratorClose :: FileEnumeratorClass enumerator => enumerator
 -> Maybe Cancellable -- ^ @cancellable@ optional 'Cancellable' object, 'Nothing' to ignore.
 -> IO ()
fileEnumeratorClose enumerator cancellable =
    propagateGError $ \gErrorPtr -> do
        {#call g_file_enumerator_close#}
          (toFileEnumerator enumerator)
          (fromMaybe (Cancellable nullForeignPtr) cancellable)
          gErrorPtr
        return ()

-- | Request information for a number of files from the enumerator asynchronously. When all i/o for the
-- operation is finished the callback will be called with the requested information.
--
-- The callback can be called with less than @numFiles@ files in case of error or at the end of the
-- enumerator. In case of a partial error the callback will be called with any succeeding items and no
-- error, and on the next request the error will be reported. If a request is cancelled the callback
-- will be called with 'IoErrorCancelled'.
--
-- During an async request no other sync and async calls are allowed, and will result in
-- 'IoErrorPending' errors.
--
-- Any outstanding i/o request with higher priority (lower numerical value) will be executed before an
-- outstanding request with lower priority. Default priority is GPriorityDefault.
fileEnumeratorNextFilesAsync :: FileEnumeratorClass enumerator
 => enumerator
 -> Int  -- ^ @numFiles@   the number of file info objects to request
 -> Int -- ^ @ioPriority@ the io priority of the request.
 -> Maybe Cancellable -- ^ @cancellable@ optional 'Cancellable' object, 'Nothing' to ignore.
 -> AsyncReadyCallback -- ^ @callback@    a 'AsyncReadyCallback' to call when the request is satisfied
 -> IO ()
fileEnumeratorNextFilesAsync enumerator numFiles ioPriority cancellable callback = do
    cCallback <- marshalAsyncReadyCallback callback
    {#call g_file_enumerator_next_files_async #}
      (toFileEnumerator enumerator)
      (fromIntegral numFiles)
      (fromIntegral ioPriority)
      (fromMaybe (Cancellable nullForeignPtr) cancellable)
      cCallback
      (castFunPtrToPtr cCallback)

-- | Finishes the asynchronous operation started with 'fileEnumeratorNextFilesAsync'.
fileEnumeratorNextFilesFinish :: FileEnumeratorClass enumerator => enumerator
 -> AsyncResult
 -> IO [FileInfo]
fileEnumeratorNextFilesFinish enumerator asyncResult =
    propagateGError ({# call g_file_enumerator_next_files_finish #} (toFileEnumerator enumerator) asyncResult)
      >>= \glistPtr -> do
        infoPtrs <- fromGList glistPtr
        mapM (makeNewGObject mkFileInfo . return) infoPtrs

-- | Asynchronously closes the file enumerator.
--
-- If cancellable is not 'Nothing', then the operation can be cancelled by triggering the cancellable object
-- from another thread. If the operation was cancelled, the error 'IoErrorCancelled' will be returned
-- in 'fileEnumeratorCloseFinish'.
fileEnumeratorCloseAsync :: FileEnumeratorClass enumerator
 => enumerator
 -> Int -- ^ @ioPriority@ the I/O priority of the request.
 -> Maybe Cancellable -- ^ @cancellable@ optional 'Cancellable' object, 'Nothing' to ignore.
 -> AsyncReadyCallback -- ^ @callback@    a 'AsyncReadyCallback' to call when the request is satisfied
 -> IO ()
fileEnumeratorCloseAsync enumerator ioPriority mbCancellable callback = do
    cCallback <- marshalAsyncReadyCallback callback
    {#call g_file_enumerator_close_async #}
      (toFileEnumerator enumerator)
      (fromIntegral ioPriority)
      (fromMaybe (Cancellable nullForeignPtr) mbCancellable)
      cCallback
      (castFunPtrToPtr cCallback)

-- | Finishes closing a file enumerator, started from 'fileEnumeratorCloseAsync'.
--
-- If the file enumerator was already closed when 'fileEnumeratorCloseAsync' was called, then this
-- function will report GIoErrorClosed in error, and return 'False'. If the file enumerator had
-- pending operation when the close operation was started, then this function will report
-- 'IoErrorPending', and return 'False'. If cancellable was not 'Nothing', then the operation may have been
-- cancelled by triggering the cancellable object from another thread. If the operation was cancelled,
-- the 'GError' 'IoErrorCancelled' will be thrown.
fileEnumeratorCloseFinish :: FileEnumeratorClass enumerator => enumerator
 -> AsyncResult
 -> IO ()
fileEnumeratorCloseFinish enumerator asyncResult =
    propagateGError (\gErrorPtr -> do
                       {# call g_file_enumerator_close_finish #}
                          (toFileEnumerator enumerator)
                          asyncResult
                          gErrorPtr
                       return ())

-- | Checks if the file enumerator has been closed.
fileEnumeratorIsClosed :: FileEnumeratorClass enumerator => enumerator
 -> IO Bool  -- ^ returns    'True' if the enumerator is closed.
fileEnumeratorIsClosed enumerator =
  liftM toBool $
  {#call g_file_enumerator_is_closed#} (toFileEnumerator enumerator)

-- | Checks if the file enumerator has pending operations.
fileEnumeratorHasPending :: FileEnumeratorClass enumerator => enumerator
 -> IO Bool -- ^ returns    'True' if the enumerator has pending operations.
fileEnumeratorHasPending enumerator =
  liftM toBool $
  {#call g_file_enumerator_has_pending#} (toFileEnumerator enumerator)

-- | Sets the file enumerator as having pending operations.
fileEnumeratorSetPending :: FileEnumeratorClass enumerator => enumerator
 -> Bool
 -> IO ()
fileEnumeratorSetPending enumerator pending =
  {#call g_file_enumerator_set_pending#} (toFileEnumerator enumerator) (fromBool pending)

#if GLIB_CHECK_VERSION(2,18,0)
-- | Get the 'File' container which is being enumerated.
fileEnumeratorGetContainer :: FileEnumeratorClass enumerator => enumerator
 -> IO File
fileEnumeratorGetContainer enumerator =
    makeNewGObject mkFile $
    {#call g_file_enumerator_get_container#} (toFileEnumerator enumerator)
#endif
