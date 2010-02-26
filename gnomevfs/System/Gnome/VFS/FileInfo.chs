{-# LANGUAGE CPP #-}
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
module System.Gnome.VFS.FileInfo (
  
  -- * Types
  FileInfo(..),
  FileFlags(..),
  FileType(..),
  InodeNumber,
  IDs,
  
  ) where

import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import System.Glib.Flags
import System.Glib.FFI
import System.Glib.UTFString
{#import System.Gnome.VFS.Marshal#}
{#import System.Gnome.VFS.Types#}
import System.Posix.Types (DeviceID, EpochTime)

{# context lib = "gnomevfs" prefix = "gnome_vfs" #}

{- typedef struct {
 -     char *name;
 -     GnomeVFSFileInfoFields valid_fields;
 -     GnomeVFSFileType type;
 -     GnomeVFSFilePermissions permissions;
 -     GnomeVFSFileFlags flags;
 -     dev_t device;
 -     GnomeVFSInodeNumber inode;
 -     guint link_count;
 -     guint uid;
 -     guint gid;
 -     GnomeVFSFileSize size;
 -     GnomeVFSFileSize block_count;
 -     guint io_block_size;
 -     time_t atime;
 -     time_t mtime;
 -     time_t ctime;
 -     char *symlink_name;
 -     char *mime_type;
 -     guint refcount;
 -     GnomeVFSACL *acl;
 -     char* selinux_context;
 - } GnomeVFSFileInfo;
 -}

instance Storable FileInfo where
    sizeOf _ = {# sizeof GnomeVFSFileInfo #}
    alignment _ = alignment (undefined :: CString)
    peek ptr =
        do name <- {# get GnomeVFSFileInfo->name #} ptr >>= maybePeek peekUTFString
           
           validFields <- liftM cToFlags $ {# get GnomeVFSFileInfo->valid_fields #} ptr
           
           let maybeField field result = if elem field validFields
                                            then liftM Just result
                                            else return Nothing
           
           fileType <- maybeField FileInfoFieldsType $
                       liftM cToEnum $ cFileInfoGetType ptr
           permissions <- maybeField FileInfoFieldsPermissions $
                          liftM cToFlags $ {# get GnomeVFSFileInfo->permissions #} ptr
           fileFlags <- maybeField FileInfoFieldsFlags $
                        liftM cToFlags $ {# get GnomeVFSFileInfo->flags #} ptr
           
           device <- maybeField FileInfoFieldsDevice $
                     liftM cToEnum $ {# get GnomeVFSFileInfo->device #} ptr
           
           inode <- maybeField FileInfoFieldsInode $
                    liftM fromIntegral $ cFileInfoGetInode ptr
           linkCount <- maybeField FileInfoFieldsLinkCount $
                        liftM fromIntegral $ {# get GnomeVFSFileInfo->link_count #} ptr
#if GNOME_VFS_CHECK_VERSION(2,14,0)           
           ids <- maybeField FileInfoFieldsIds $
                  do uid <- liftM fromIntegral $ {# get GnomeVFSFileInfo->uid #} ptr
                     gid <- liftM fromIntegral $ {# get GnomeVFSFileInfo->gid #} ptr
                     return $ (uid, gid)
#else
           uid <- liftM fromIntegral $ {# get GnomeVFSFileInfo->uid #} ptr
           gid <- liftM fromIntegral $ {# get GnomeVFSFileInfo->gid #} ptr
           let ids = Just (uid, gid)
#endif
           
           size <- maybeField FileInfoFieldsSize $
                   liftM fromIntegral $ cFileInfoGetSize ptr
           blockCount <- maybeField FileInfoFieldsBlockCount $
                         liftM fromIntegral $ {# get GnomeVFSFileInfo->block_count #} ptr
           
           ioBlockSize <- maybeField FileInfoFieldsIoBlockSize $
                          liftM fromIntegral $ {# get GnomeVFSFileInfo->io_block_size #} ptr
           
           aTime <- maybeField FileInfoFieldsAtime $
                    liftM cToEnum $ {# get GnomeVFSFileInfo->atime #} ptr
           mTime <- maybeField FileInfoFieldsMtime $
                    liftM cToEnum $ {# get GnomeVFSFileInfo->mtime #} ptr
           cTime <- maybeField FileInfoFieldsCtime $
                    liftM cToEnum $ {# get GnomeVFSFileInfo->ctime #} ptr
           symlinkName <-  maybeField FileInfoFieldsSymlinkName $
                           {# get GnomeVFSFileInfo->symlink_name #} ptr >>= peekUTFString
#if GNOME_VFS_CHECK_VERSION(2,14,0)
           mimeType <- maybeField FileInfoFieldsMimeType $
                       {# call file_info_get_mime_type #} (castPtr ptr) >>= peekUTFString
#endif
           return $ FileInfo name
                             fileType
                             permissions
                             fileFlags
                             device
                             inode
                             linkCount
                             ids
                             size
                             blockCount
                             ioBlockSize
                             aTime
                             mTime
                             cTime
                             symlinkName
#if GNOME_VFS_CHECK_VERSION(2,14,0)
                             mimeType
#endif
    poke ptr (FileInfo name
                       fileType
                       permissions
                       fileFlags
                       device
                       inode
                       linkCount
                       ids
                       size
                       blockCount
                       ioBlockSize
                       aTime
                       mTime
                       cTime
                       symlinkName
#if GNOME_VFS_CHECK_VERSION(2,14,0)
                       mimeType
#endif
             ) =
        do let marshaller :: FileInfoFields
                          -> Maybe a
                          -> b
                          -> (a -> IO b)
                          -> (Ptr FileInfo -> b -> IO ())
                          -> IO (Maybe FileInfoFields)
               marshaller field Nothing dflt _ action =
                   do action ptr dflt
                      return Nothing
               marshaller field (Just value) _ cast action =
                   do cast value >>= action ptr
                      return $ Just field
           
           case name of
             Just name' -> newUTFString name' >>= {# set GnomeVFSFileInfo->name #} ptr
             Nothing    -> return ()
           
           validFields <- liftM catMaybes $ sequence $ 
                          [ marshaller FileInfoFieldsType
                                       fileType
                                       0
                                       (return . cFromEnum)
                                       cFileInfoSetType,
                            
                            marshaller FileInfoFieldsPermissions
                                       permissions
                                       0
                                       (return . cFromFlags)
                                       {# set GnomeVFSFileInfo->permissions #},
                            
                            marshaller FileInfoFieldsFlags
                                       fileFlags
                                       0
                                       (return . cFromFlags)
                                       {# set GnomeVFSFileInfo->flags #},
                            
                            marshaller FileInfoFieldsDevice
                                       device
                                       0
                                       (return . cFromEnum)
                                       {# set GnomeVFSFileInfo->device #},
                            
                            marshaller FileInfoFieldsInode
                                       inode
                                       0
                                       (return . fromIntegral)
                                       {# set GnomeVFSFileInfo->inode #},
                            
                            marshaller FileInfoFieldsLinkCount
                                       linkCount
                                       0
                                       (return . fromIntegral)
                                       {# set GnomeVFSFileInfo->link_count #},

#if GTK_CHECK_VERSION(2,14,0)                            
                            marshaller FileInfoFieldsIds
                                       ids
                                       (0, 0)
                                       (\(uid, gid) -> return (fromIntegral uid, fromIntegral gid))
                                       (\ptr (uid, gid) ->
                                        do {# set GnomeVFSFileInfo->uid #} ptr uid
                                           {# set GnomeVFSFileInfo->gid #} ptr gid),
#endif
                            
                            marshaller FileInfoFieldsSize
                                       size
                                       0
                                       (return . fromIntegral)
                                       cFileInfoSetSize,
                            
                            marshaller FileInfoFieldsBlockCount
                                       blockCount
                                       0
                                       (return . fromIntegral)
                                       {# set GnomeVFSFileInfo->block_count #},
                            
                            marshaller FileInfoFieldsIoBlockSize
                                       ioBlockSize
                                       0
                                       (return . fromIntegral)
                                       {# set GnomeVFSFileInfo->io_block_size #},
                            
                            marshaller FileInfoFieldsAtime
                                       aTime
                                       0
                                       (return . cFromEnum)
                                       {# set GnomeVFSFileInfo->atime #},
                            
                            marshaller FileInfoFieldsMtime
                                       mTime
                                       0
                                       (return . cFromEnum)
                                       {# set GnomeVFSFileInfo->mtime #},
                            
                            marshaller FileInfoFieldsCtime
                                       cTime
                                       0
                                       (return . cFromEnum)
                                       {# set GnomeVFSFileInfo->ctime #},
                            
                            marshaller FileInfoFieldsSymlinkName
                                       symlinkName
                                       nullPtr
                                       newUTFString
                                       (\ptr str ->
                                        do {# get GnomeVFSFileInfo->symlink_name #} ptr >>= (gFree . castPtr)
                                           {# set GnomeVFSFileInfo->symlink_name #} ptr str),
                            
                            marshaller FileInfoFieldsMimeType
                                       symlinkName
                                       nullPtr
                                       newUTFString
                                       (\ptr str ->
                                        do {# get GnomeVFSFileInfo->mime_type #} ptr >>= (gFree . castPtr)
                                           {# set GnomeVFSFileInfo->mime_type #} ptr str) ]
           
#if !GTK_CHECK_VERSION(2,14,0)
           case ids of
             Just (uid, gid) ->
               do {# set GnomeVFSFileInfo->uid #} ptr $ fromIntegral uid
                  {# set GnomeVFSFileInfo->gid #} ptr $ fromIntegral gid
             Nothing ->
               return ()
#endif

           {# set GnomeVFSFileInfo->valid_fields #} ptr $ cFromFlags validFields

gFree = {# call g_free #}

foreign import ccall unsafe "_hs_gnome_vfs_file_info_get_type"
  cFileInfoGetType :: Ptr FileInfo
                   -> IO {# type GnomeVFSFileType #}
foreign import ccall unsafe "_hs_gnome_vfs_file_info_get_inode"
  cFileInfoGetInode :: Ptr FileInfo
                    -> IO CULLong
foreign import ccall unsafe "_hs_gnome_vfs_file_info_get_size"
  cFileInfoGetSize :: Ptr FileInfo
                   -> IO CULLong
foreign import ccall unsafe "_hs_gnome_vfs_file_info_get_block_count"
  cFileInfoGetBlockCount :: Ptr FileInfo
                         -> IO CULLong

foreign import ccall unsafe "_hs_gnome_vfs_file_info_set_type"
  cFileInfoSetType :: Ptr FileInfo
                   -> {# type GnomeVFSFileType #}
                   -> IO ()
foreign import ccall unsafe "_hs_gnome_vfs_file_info_get_inode"
  cFileInfoSetInode :: Ptr FileInfo
                    -> CULLong
                    -> IO ()
foreign import ccall unsafe "_hs_gnome_vfs_file_info_set_size"
  cFileInfoSetSize :: Ptr FileInfo
                   -> {# type GnomeVFSFileSize #}
                   -> IO ()
foreign import ccall unsafe "_hs_gnome_vfs_file_info_set_block_count"
  cFileInfoSetBlockCount :: Ptr FileInfo
                         -> CULLong
                         -> IO ()
