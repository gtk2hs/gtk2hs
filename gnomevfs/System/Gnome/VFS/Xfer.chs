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
module System.Gnome.VFS.Xfer (
  
-- * Types
  XferProgressInfo(..),
  XferOptions       ( XferFollowLinks
                    , XferRecursive
                    , XferSamefs
                    , XferDeleteItems
                    , XferEmptyDirectories
                    , XferNewUniqueDirectory
                    , XferRemovesource
                    , XferUseUniqueNames
                    , XferLinkItems
                    , XferFollowLinksRecursive
#if GNOME_VFS_CHECK_VERSION(2,12,0)
                    , XferTargetDefaultPerms 
#endif
                    ),
  XferOverwriteMode ( XferOverwriteModeAbort
                    , XferOverwriteModeReplace
                    , XferOverwriteModeSkip ),
  XferErrorAction(..),
  XferOverwriteAction(..),
  XferProgressCallback,
  XferErrorCallback,
  XferOverwriteCallback,
  XferDuplicateCallback,
  
-- * Operations
  xferURI,
  xferURIList,
  xferDeleteList
  
  ) where

import Control.Monad
import Data.Maybe (fromMaybe)
import System.Glib.FFI
import System.Glib.GList
import System.Glib.UTFString
{#import System.Gnome.VFS.Marshal#}
-- {#import System.Gnome.VFS.Types#}
{#import System.Gnome.VFS.BasicTypes#}

{# context lib = "gnomevfs" prefix = "gnome_vfs" #}

{- typedef struct {
 -     GnomeVFSXferProgressStatus status;
 -     GnomeVFSResult vfs_status;
 -     GnomeVFSXferPhase phase;
 -     gchar *source_name;
 -     gchar *target_name;
 -     gulong file_index;
 -     gulong files_total;
 -     GnomeVFSFileSize bytes_total;
 -     GnomeVFSFileSize file_size;
 -     GnomeVFSFileSize bytes_copied;
 -     GnomeVFSFileSize total_bytes_copied;
 -     gchar *duplicate_name;
 -     int duplicate_count;
 -     gboolean top_level_item;
 - } GnomeVFSXferProgressInfo;
 -}

instance Storable XferProgressInfo where
    sizeOf _ = {# sizeof GnomeVFSXferProgressInfo #}
    alignment _ = alignment (undefined :: CString)
    peek ptr =
        do vfsStatus <- liftM cToEnum $ {# get GnomeVFSXferProgressInfo->vfs_status #} ptr
           phase <- liftM cToEnum $ {# get GnomeVFSXferProgressInfo->phase #} ptr
           sourceName <- {# get GnomeVFSXferProgressInfo->source_name #} ptr >>= maybePeek peekUTFString
           targetName <- {# get GnomeVFSXferProgressInfo->target_name #} ptr >>= maybePeek peekUTFString
           fileIndex <- liftM fromIntegral $ {# get GnomeVFSXferProgressInfo->file_index #} ptr
           filesTotal <- liftM fromIntegral $ {# get GnomeVFSXferProgressInfo->files_total #} ptr
           bytesTotal <- liftM fromIntegral $ {# get GnomeVFSXferProgressInfo->bytes_total #} ptr
           fileSize <- liftM fromIntegral $ {# get GnomeVFSXferProgressInfo->file_size #} ptr
           bytesCopied <- liftM fromIntegral $ {# get GnomeVFSXferProgressInfo->bytes_copied #} ptr
           totalBytesCopied <- liftM fromIntegral $ {# get GnomeVFSXferProgressInfo->total_bytes_copied #} ptr
           topLevelItem <- liftM toBool $ {# get GnomeVFSXferProgressInfo->top_level_item #} ptr
           
           return $ XferProgressInfo vfsStatus
                                     phase
                                     sourceName
                                     targetName
                                     fileIndex
                                     filesTotal
                                     bytesTotal
                                     fileSize
                                     bytesCopied
                                     totalBytesCopied
                                     topLevelItem
    poke _ = error "XferProgressInfo.poke not implemented"

type CXferProgressCallback =  Ptr ()
                           -> {# type gpointer #}
                           -> IO CInt
xferProgressCallbackMarshal :: Maybe XferProgressCallback
                            -> XferErrorCallback
                            -> XferOverwriteCallback
                            -> Maybe XferDuplicateCallback
                            -> IO (FunPtr CXferProgressCallback)
xferProgressCallbackMarshal progressCallback
                            errorCallback
                            overwriteCallback
                            duplicateCallback =
    makeXferProgressCallback cCallback
    where cCallback :: CXferProgressCallback
          cCallback cInfo cUserData =
              do status <- liftM cToEnum $ {# get GnomeVFSXferProgressInfo->status #} $ castPtr cInfo
                 info <- peek $ castPtr cInfo
                 case status of
                   XferProgressStatusOk ->
                       liftM fromBool $ progressCallback' info
                   XferProgressStatusVfserror ->
                       liftM cFromEnum $ errorCallback info
                   XferProgressStatusOverwrite ->
                       liftM cFromEnum $ overwriteCallback info
                   XferProgressStatusDuplicate ->
                       do duplicateCount <- liftM fromIntegral $ {# get GnomeVFSXferProgressInfo->duplicate_count #} cInfo
                          duplicatePtr <- {# get GnomeVFSXferProgressInfo->duplicate_name #} cInfo
                          duplicateName <- peekUTFString duplicatePtr
                          newDuplicateName <- duplicateCallback' info duplicateName duplicateCount
                          case newDuplicateName of
                            Just newDuplicateName' ->
                                do {# call g_free #} $ castPtr duplicatePtr
                                   newUTFString newDuplicateName' >>=
                                       {# set GnomeVFSXferProgressInfo->duplicate_name #} cInfo
                                   return 1
                            Nothing ->
                                return 0
          progressCallback' =
              fromMaybe (const $ return True) progressCallback
          duplicateCallback' =
              fromMaybe (\_ name _ -> return Nothing) duplicateCallback

foreign import ccall safe "wrapper"
  makeXferProgressCallback :: CXferProgressCallback
                           -> IO (FunPtr CXferProgressCallback)

type CXfer =  {# type GnomeVFSXferOptions #}
           -> {# type GnomeVFSXferErrorMode #}
           -> {# type GnomeVFSXferOverwriteMode #}
           -> FunPtr CXferProgressCallback
           -> {# type gpointer #}
           -> IO {# type GnomeVFSResult #}
type Xfer = [XferOptions]
          -> Maybe XferProgressCallback
          -> Maybe XferErrorCallback
          -> Either XferOverwriteMode XferOverwriteCallback
          -> Maybe XferDuplicateCallback
          -> IO ()
marshalXfer :: CXfer
            -> Xfer
marshalXfer cXfer xferOptions progressCallback errorCallback overwriteOpt duplicateCallback =
    voidResultMarshal $ do
      cProgressCallback <- xferProgressCallbackMarshal
                               progressCallback
                               errorCallback'
                               overwriteCallback
                               duplicateCallback
      cResult <- cXfer (cFromFlags xferOptions)
                       (cFromEnum errorMode)
                       (cFromEnum overwriteMode)
                       cProgressCallback nullPtr
      freeHaskellFunPtr cProgressCallback
      return cResult
    where 
          (overwriteMode, overwriteCallback) =
              case overwriteOpt of
                Left overwriteMode ->
                    (overwriteMode,
                     const $ return $ error "marshalXfer: overwrite callback called unexpectedly")
                Right overwriteCallback ->
                    (XferOverwriteModeQuery,
                     overwriteCallback)
          (errorMode, errorCallback') =
              case errorCallback of
                Just errorCallback' ->
                    (XferErrorModeQuery,
                     errorCallback')
                Nothing ->
                    (XferErrorModeAbort,
                     const $ return $ error "marshalXfer: error callback called unexpectedly")

-- | Transfer the file located at @sourceURI@ to @targetURI@, using 
--   the specified options and callbacks.
xferURI :: URI                                            -- ^ @sourceURI@ - the source URI
        -> URI                                            -- ^ @targetURI@ - the target URI
        -> [XferOptions]                                  -- ^ @options@ - 
        -> Maybe XferProgressCallback                     -- ^ @progressCallback@ - 
        -> Maybe XferErrorCallback                        -- ^ @errorCallback@ - 
        -> Either XferOverwriteMode XferOverwriteCallback -- ^ @overwriteOpt@ - 
        -> Maybe XferDuplicateCallback                    -- ^ @duplicateCallback@ - 
        -> IO ()
xferURI sourceURI targetURI =
    marshalXfer ({# call xfer_uri #} sourceURI targetURI)

withURIList :: [URI]
            -> (GList -> IO a)
            -> IO a
withURIList uriList action =
    withMany withURI uriList $ \cURIList ->
        toGList cURIList >>= action

-- | For each pair in @sourceTargetURIList@, transfer the file at the
--   first 'URI' to the second 'URI'.
xferURIList :: [(URI, URI)]                                   -- ^ @sourceTargetURIList@ - 
            -> [XferOptions]                                  -- ^ @options@ - 
            -> Maybe XferProgressCallback                     -- ^ @progressCallback@ - 
            -> Maybe XferErrorCallback                        -- ^ @errorCallback@ - 
            -> Either XferOverwriteMode XferOverwriteCallback -- ^ @overwriteOpt@ - 
            -> Maybe XferDuplicateCallback                    -- ^ @duplicateCallback@ - 
            -> IO ()
xferURIList sourceTargetURIList xferOptions progressCallback errorCallback overwriteOpt duplicateCallback =
    withURIList sourceURIList $ \cSourceURIList ->
        withURIList targetURIList $ \cTargetURIList ->
            marshalXfer ({# call xfer_uri_list #} cSourceURIList cTargetURIList) 
                        xferOptions progressCallback
                        errorCallback overwriteOpt duplicateCallback
    where (sourceURIList, targetURIList) = unzip sourceTargetURIList

-- | Delete the files at the 'URI's in @sourceURIList@.
xferDeleteList :: [URI]                      -- ^ @sourceURIList@ - 
               -> [XferOptions]              -- ^ @options@ - 
               -> Maybe XferProgressCallback -- ^ @progressCallback@ - 
               -> Maybe XferErrorCallback    -- ^ @errorCallback@ - 
               -> IO ()
xferDeleteList sourceURIList xferOptions progressCallback errorCallback =
    withURIList sourceURIList $ \cSourceURIList ->
       do cProgressCallback <- xferProgressCallbackMarshal progressCallback
                                                           errorCallback'
                                                           (return $ error "xferDeleteList: overwrite callback called unexpectedly")
                                                           (return $ error "xferDeleteList: duplicate callback called unexpectedly")
          voidResultMarshal $
                    {# call xfer_delete_list #}
                    cSourceURIList
                    (cFromEnum errorMode)
                    (cFromFlags xferOptions)
                    cProgressCallback
                    nullPtr
    where (errorMode, errorCallback') =
              case errorCallback of
                Just errorCallback' ->
                    (XferErrorModeQuery,
                     errorCallback')
                Nothing ->
                    (XferErrorModeAbort,
                     const $ return XferErrorActionAbort)
