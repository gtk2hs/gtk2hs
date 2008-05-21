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
--  
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.Gnome.VFS.Ops (
  
-- * Types
  Handle,
  Result(..),
  OpenMode(..),
  SeekPosition(..),
  FilePermissions(..),
  FileSize,
  FileOffset,
  
-- * I\/O Operations
  open,
  openURI,
  create,
  createURI,
  close,
#if __GLASGOW_HASKELL__ >= 606
  read,
  write,
#endif
  seek,
  tell,
#if GNOME_VFS_CHECK_VERSION(2,12,0)
  forgetCache,
#endif
  
-- * Truncation
  truncate,
  truncateURI,
  truncateHandle,

-- * File Information
  getFileInfo,
  getFileInfoURI,
  getFileInfoFromHandle,
  setFileInfo,
  setFileInfoURI
  
  ) where

#if __GLASGOW_HASKELL__ >= 606 && __GLASGOW_HASKELL__ < 608
#define OLD_BYTESTRING
#endif

import Control.Exception
import Control.Monad (liftM)
#if __GLASGOW_HASKELL__ >= 606
import qualified Data.ByteString as BS (ByteString, useAsCStringLen)
#ifdef OLD_BYTESTRING
import qualified Data.ByteString.Base as BS (fromForeignPtr)
#else
import qualified Data.ByteString.Internal as BS (fromForeignPtr)
#endif
#endif
import Prelude hiding (read, truncate)
import System.Glib.FFI
import System.Glib.UTFString (withUTFString, peekUTFString)
{#import System.Gnome.VFS.Types#}
{#import System.Gnome.VFS.FileInfo#}
{#import System.Gnome.VFS.Marshal#}

{# context lib = "gnomevfs" prefix = "gnome_vfs" #}

-- | Open the file at @textURI@.
open :: TextURI   -- ^ @textURI@ -
     -> OpenMode  -- ^ @openMode@ -
     -> IO Handle -- ^ a handle to the opened file
open textURI openMode =
    let cOpenMode = cFromEnum openMode
    in withUTFString textURI $ \cTextURI ->
        newObjectResultMarshal Handle $ \cHandlePtr ->
            {# call gnome_vfs_open #} (castPtr cHandlePtr) cTextURI cOpenMode

-- | Open the file at @uri@.
openURI :: URI       -- ^ @uri@ -
        -> OpenMode  -- ^ @openMode@ - 
        -> IO Handle -- ^ a handle to the opened file
openURI uri openMode =
    let cOpenMode = cFromEnum openMode
    in newObjectResultMarshal Handle $ \cHandlePtr ->
        {# call open_uri #} (castPtr cHandlePtr) uri cOpenMode

-- | Create a file at @textURI@.
create :: TextURI           -- ^ @textURI@ - 
       -> OpenMode          -- ^ @openMode@ - 
       -> Bool              -- ^ @exclusive@ - 
       -> [FilePermissions] -- ^ @perm@ - 
       -> IO Handle         -- ^ a handle to the created file
create textURI openMode exclusive perm =
    let cOpenMode = cFromEnum openMode
        cExclusive = fromBool exclusive
        cPerm = cFromFlags perm
    in withUTFString textURI $ \cTextURI ->
        newObjectResultMarshal Handle $ \cHandlePtr ->
            {# call gnome_vfs_create #} (castPtr cHandlePtr) cTextURI cOpenMode cExclusive cPerm

-- | Create a file at @uri@.
createURI :: URI                -- ^ @uri@ - 
          -> OpenMode           -- ^ @openMode@ - 
          -> Bool               -- ^ @exclusive@ - 
          -> [FilePermissions]  -- ^ @perm@ - 
          -> IO Handle          -- ^ a handle to the created file
createURI uri openMode exclusive perm =
    let cOpenMode = cFromEnum openMode
        cExclusive = fromBool exclusive
        cPerm = cFromFlags perm
    in newObjectResultMarshal Handle $ \cHandlePtr ->
        {# call create_uri #} (castPtr cHandlePtr) uri cOpenMode cExclusive cPerm

-- | Close a 'Handle'.
close :: Handle -- ^ @handle@ - 
      -> IO ()
close handle =
    voidResultMarshal $ {# call gnome_vfs_close #} handle

#if __GLASGOW_HASKELL__ >= 606
-- | Read data from a file.
read :: Handle           -- ^ @handle@ - 
     -> FileSize         -- ^ @bytes@ - 
     -> IO BS.ByteString -- ^ the data read from the file
read handle bytes =
    let cBytes = fromIntegral bytes
    in do buffer <- mallocForeignPtrBytes $ fromIntegral bytes
          withForeignPtr buffer $ \cBuffer ->
              alloca $ \cBytesReadPtr ->
                  genericResultMarshal
                      (do poke cBytesReadPtr 0
                          {# call gnome_vfs_read #} handle cBuffer cBytes cBytesReadPtr)
                      (do bytesRead <- liftM fromIntegral $ peek cBytesReadPtr
                          assert (bytesRead /= 0 || cBytes == 0) $ return ()
                          return $ BS.fromForeignPtr (castForeignPtr buffer)
#ifndef OLD_BYTESTRING
                                                     0
#endif
                                                     (fromIntegral bytes))
                      (do bytesRead <- liftM fromIntegral $ peek cBytesReadPtr
                          assert (bytesRead == 0) $ return ())

-- | Write data to a file.
write :: Handle        -- ^ @handle@ - 
      -> BS.ByteString -- ^ @byteString@ - 
      -> IO FileSize   -- ^ the number of bytes actually written
write handle byteString =
    BS.useAsCStringLen byteString $ \(cBuffer, bytes) ->
        let cBytes = fromIntegral bytes
        in  alloca $ \cBytesWrittenPtr ->
            genericResultMarshal
                (do poke cBytesWrittenPtr 0
                    {# call gnome_vfs_write #} handle (castPtr cBuffer) cBytes cBytesWrittenPtr)
                (do bytesWritten <- liftM fromIntegral $ peek cBytesWrittenPtr
                    assert (bytesWritten /= 0 || cBytes == 0) $ return ()
                    return bytesWritten)
                (do bytesWritten <- liftM fromIntegral $ peek cBytesWrittenPtr
                    assert (bytesWritten == 0) $ return ())
#endif

-- | Seek to a position in a file.
seek :: Handle       -- ^ @handle@ - 
     -> SeekPosition -- ^ @whence@ - 
     -> FileOffset   -- ^ @offset@ - 
     -> IO ()
seek handle whence offset =
    let cWhence = cFromEnum whence
        cOffset = fromIntegral offset
    in voidResultMarshal $ {# call gnome_vfs_seek #} handle cWhence cOffset

-- | Return the current position in the file.
tell :: Handle      -- ^ @handle@ - 
     -> IO FileSize -- ^ the current position in the file
tell handle =
    alloca $ \cOffsetReturnPtr ->
        genericResultMarshal
            (do poke cOffsetReturnPtr 0
                {# call gnome_vfs_tell #} handle cOffsetReturnPtr)
            (liftM fromIntegral $ peek cOffsetReturnPtr)
            (do cOffsetReturn <- peek cOffsetReturnPtr
                assert (cOffsetReturn == 0) $ return ())

#if GNOME_VFS_CHECK_VERSION(2,12,0)
-- | Free any cache associated with the file opened on @handle@,
--   in the region of @size@ bytes starting at @offset@.
forgetCache :: Handle
            -> FileOffset
            -> FileSize
            -> IO ()
forgetCache handle offset size =
    let cOffset = fromIntegral offset
        cSize = fromIntegral size
    in voidResultMarshal $ {# call forget_cache #} handle cOffset cSize
#endif

-- | Truncate the file at @textURI@ to @length@ bytes.
truncate :: String
         -> FileSize
         -> IO ()
truncate textURI length =
    let cLength = fromIntegral length
    in withUTFString textURI $ \cTextURI ->
        voidResultMarshal $ {# call gnome_vfs_truncate #} cTextURI cLength

-- | Truncate the file at @uri@ to @length@ bytes.
truncateURI :: URI
            -> FileSize
            -> IO ()
truncateURI uri length =
    let cLength = fromIntegral length
    in voidResultMarshal $ {# call truncate_uri #} uri cLength

-- | Truncate the file opened on @handle@ to @length@ bytes.
truncateHandle :: Handle
               -> FileSize
               -> IO ()
truncateHandle handle length =
    let cLength = fromIntegral length
    in voidResultMarshal $ {# call truncate_handle #} handle cLength

-- | Get the file information for the file at @textURI@.
getFileInfo :: String
            -> [FileInfoOptions]
            -> IO FileInfo
getFileInfo textURI options =
    let cOptions = cFromFlags options
    in withUTFString textURI $ \cTextURI ->
        bracket {# call file_info_new #}
                {# call file_info_unref #}
                (\cFileInfo ->
                     genericResultMarshal
                         ({# call get_file_info #} cTextURI cFileInfo cOptions)
                         (peek $ castPtr cFileInfo)
                         (return ()))

-- | Get the file information for the file at @uri@.
getFileInfoURI :: URI
               -> [FileInfoOptions]
               -> IO FileInfo
getFileInfoURI uri options =
    let cOptions = cFromFlags options
    in bracket {# call file_info_new #}
               {# call file_info_unref #}
               (\cFileInfo ->
                genericResultMarshal
                    ({# call get_file_info_uri #} uri cFileInfo cOptions)
                    (peek $ castPtr cFileInfo)
                    (return ()))

-- | Get the file information for the file opened on @handle@.
getFileInfoFromHandle :: Handle
                      -> [FileInfoOptions]
                      -> IO FileInfo
getFileInfoFromHandle handle options =
    let cOptions = cFromFlags options
    in bracket {# call file_info_new #}
               {# call file_info_unref #}
               (\cFileInfo ->
                    genericResultMarshal
                        ({# call get_file_info_from_handle #} handle cFileInfo cOptions)
                        (peek $ castPtr cFileInfo)
                        (return ()))

-- | Set the file information for the file at @textURI@.
setFileInfo :: String
            -> FileInfo
            -> [SetFileInfoMask]
            -> IO ()
setFileInfo textURI info mask =
    withUTFString textURI $ \cTextURI ->
        with info $ \cInfo ->
             voidResultMarshal $ {# call set_file_info #} cTextURI (castPtr cInfo) $ cFromFlags mask

-- | Set the file information for the file at @uri@.
setFileInfoURI :: URI
               -> FileInfo
               -> [SetFileInfoMask]
               -> IO ()
setFileInfoURI uri info mask =
    with info $ \cInfo ->
        voidResultMarshal $ {# call set_file_info_uri #} uri (castPtr cInfo) $ cFromFlags mask
