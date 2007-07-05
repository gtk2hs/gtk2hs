-- GIMP Toolkit (GTK) Binding for Haskell: binding to libgnomevfs   -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Copyright (c) 2007 Peter Gavin
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
--   
--   Functions for creating, removing, and accessing directories and
--   their contents.
--   
module System.Gnome.VFS.Directory (

-- * Types
  DirectoryHandle,
  DirectoryVisitOptions(..),
  DirectoryVisitResult(..),
  
-- * Directory Creation
  makeDirectory,
  makeDirectoryForURI,

-- * Directory Removal
  removeDirectory,
  removeDirectoryFromURI,

-- * Directory Access
  directoryOpen,
  directoryOpenFromURI,
  directoryReadNext,
  directoryClose,
  directoryListLoad,

-- * Directory Traversal
  directoryVisit,
  directoryVisitURI,
  directoryVisitFiles,
  directoryVisitFilesAtURI
  
  ) where

import Control.Exception     ( assert
                             , bracket )
import Control.Monad 	     ( liftM )
import System.Glib.GList     ( GList()
                             , toGList
                             , readGList )
import System.Glib.UTFString ( withUTFString
                             , peekUTFString
                             , newUTFString )
import System.Glib.FFI
{#import System.Gnome.VFS.FileInfo#}
{#import System.Gnome.VFS.Types#}
{#import System.Gnome.VFS.Marshal#}

{# context lib = "gnomevfs" prefix = "gnome_vfs" #}

-- | Create @textURI@ as a directory. Only succeeds if a file or
--   directory does not already exist at @textURI@.
makeDirectory :: TextURI           -- ^ @textURI@ - String representation of the URI of the directory to create
              -> [FilePermissions] -- ^ @perm@ - 'FilePermissions' for the newly created directory
              -> IO ()
makeDirectory textURI perm =
    let cPerm = cFromFlags perm
    in withUTFString textURI $ \cTextURI ->
        voidResultMarshal $ {# call make_directory #} cTextURI cPerm

-- | Create @uri@ as a directory. Only succeeds if a file or
--   directory does not already exist at @uri@.
makeDirectoryForURI :: URI               -- ^ @uri@ - 'URI' of the directory to be created
                    -> [FilePermissions] -- ^ @perm@ - 'FilePermissions' for the newly created directory
                    -> IO ()
makeDirectoryForURI uri perm =
    let cPerm = cFromFlags perm
    in voidResultMarshal $ {# call make_directory_for_uri #} uri cPerm

-- | Remove the directory at @textURI@. The object at @textURI@ must be an empty directory.
removeDirectory :: TextURI -- ^ @textURI@ - URI of the directory to be removed
                -> IO ()
removeDirectory textURI =
    withUTFString textURI $ voidResultMarshal . {# call remove_directory #}

-- | Remove the directory at @uri@. The object at @uri@ must be an empty directory.
removeDirectoryFromURI :: URI   -- ^ @uri@ - 'URI' of the directory to be removed
                       -> IO ()
removeDirectoryFromURI uri =
    voidResultMarshal $ {# call remove_directory_from_uri #} uri

-- | Open directory textURI for reading. Returns a 'DirectoryHandle'
--   which can be used to read directory entries one by one.
directoryOpen :: TextURI            -- ^ @textURI@ - String representation of the URI of the directory to open
              -> [FileInfoOptions]  -- ^ @fileInfoOptions@ - options for reading file information
              -> IO DirectoryHandle -- ^ handle to the opened directory
directoryOpen textURI fileInfoOptions =
    let cFileInfoOptions = cFromFlags fileInfoOptions
    in withUTFString textURI $ \cTextURI ->
        newObjectResultMarshal DirectoryHandle $ \cHandlePtr ->
            {# call directory_open #} (castPtr cHandlePtr) cTextURI cFileInfoOptions

-- | Open directory textURI for reading. Returns a 'DirectoryHandle'
--   which can be used to read directory entries one by one.
directoryOpenFromURI :: URI                -- ^ @uri@ - 'URI' of the directory to open
                     -> [FileInfoOptions]  -- ^ @fileInfoOptions@ - options for reading file information
                     -> IO DirectoryHandle -- ^ handle to the opened directory
directoryOpenFromURI uri fileInfoOptions =
    let cFileInfoOptions = cFromFlags fileInfoOptions
    in newObjectResultMarshal DirectoryHandle $ \cHandlePtr ->
        {# call directory_open_from_uri #} (castPtr cHandlePtr) uri cFileInfoOptions

-- | Read the next directory entry from a 'DirectoryHandle'.
directoryReadNext :: DirectoryHandle -- ^ @handle@ - a directory handle
                  -> IO FileInfo     -- ^ file information for the next directory entry
directoryReadNext handle =
    alloca $ \(cFileInfoPtr :: Ptr FileInfo) ->
    genericResultMarshal ({# call directory_read_next #} handle $ castPtr cFileInfoPtr)
                          (peek cFileInfoPtr)
                          (return ())

-- | Close a 'DirectoryHandle'.
directoryClose :: DirectoryHandle -- ^ @handle@ - a directory handle
               -> IO ()
directoryClose handle =
    voidResultMarshal $ {# call directory_close #} handle

type CDirectoryVisitFunc =  CString                 -- rel_path
                         -> Ptr FileInfo            -- info
                         -> {# type gboolean #}     -- recursing_will_loop
                         -> {# type gpointer #}     -- user_data
                         -> Ptr {# type gboolean #} -- recurse
                         -> IO {# type gboolean #}
directoryVisitCallbackMarshal :: DirectoryVisitCallback
                              -> IO {# type GnomeVFSDirectoryVisitFunc #}
directoryVisitCallbackMarshal callback =
    let cCallback :: CDirectoryVisitFunc
        cCallback cRelPath cInfo cRecursingWillLoop cUserData cRecursePtr =
            do relPath <- peekUTFString cRelPath
               info <- peek cInfo
               let recursingWillLoop = toBool cRecursingWillLoop
               result <- callback relPath info recursingWillLoop
               case result of
                 DirectoryVisitStop     -> return $ fromBool False
                 DirectoryVisitContinue -> return $ fromBool True
                 DirectoryVisitRecurse  -> do poke cRecursePtr $ fromBool True
                                              return $ fromBool True
    in makeDirectoryVisitFunc cCallback
foreign import ccall safe "wrapper"
  makeDirectoryVisitFunc :: CDirectoryVisitFunc
                         -> IO {# type GnomeVFSDirectoryVisitFunc #}

type DirectoryVisit    =  [FileInfoOptions]
                       -> [DirectoryVisitOptions]
                       -> DirectoryVisitCallback
                       -> IO ()
type CDirectoryVisit   =  {# type GnomeVFSFileInfoOptions #}
                       -> {# type GnomeVFSDirectoryVisitOptions #}
                       -> {# type GnomeVFSDirectoryVisitFunc #}
                       -> {# type gpointer #}
                       -> IO {# type GnomeVFSResult #}

directoryVisitMarshal :: CDirectoryVisit
                       -> DirectoryVisit
directoryVisitMarshal cVisitAction infoOptions visitOptions callback =
    let cInfoOptions = cFromFlags infoOptions
        cVisitOptions = cFromFlags visitOptions
    in bracket (directoryVisitCallbackMarshal callback)
               freeHaskellFunPtr
               (\cDirectoryVisitFunc ->
                voidResultMarshal $ cVisitAction cInfoOptions cVisitOptions cDirectoryVisitFunc nullPtr)

-- | Visit each entry in a directory at a 'TextURI', calling a
--   'DirectoryVisitCallback' for each one.
directoryVisit :: String                  -- ^ @textURI@ - string representation of the URI of the directory to visit
               -> [FileInfoOptions]       -- ^ @infoOptions@ - options for reading file information
               -> [DirectoryVisitOptions] -- ^ @visitOptions@ - options for visiting the directory
               -> DirectoryVisitCallback  -- ^ @callback@ - a function to be called for each entry
               -> IO ()
directoryVisit textURI infoOptions visitOptions callback =
    withUTFString textURI $ \cTextURI ->
        directoryVisitMarshal ({# call directory_visit #} cTextURI) infoOptions visitOptions callback

-- | Visit each entry in a directory at a 'URI', calling a
--   'DirectoryVisitCallback' for each one.
directoryVisitURI :: URI                     -- ^ @uri@ - the URI of the directory to visit
                  -> [FileInfoOptions]       -- ^ @infoOptions@ - options for reading file information
                  -> [DirectoryVisitOptions] -- ^ @visitOptions@ - options for visiting the directory
                  -> DirectoryVisitCallback  -- ^ @callback@ - a function to be called for each entry
                  -> IO ()
directoryVisitURI uri =
    directoryVisitMarshal ({# call directory_visit_uri #} uri)

-- | Visit each file in a list contained with a directory at a
--   'TextURI', calling a 'DirectoryVisitCallback' for each one.
directoryVisitFiles :: TextURI                 -- ^ @textURI@ - string representation of the URI of the directory to visit
                    -> [String]                -- ^ @files@ - the files contained in @textURI@ to be visited
                    -> [FileInfoOptions]       -- ^ @infoOptions@ - options for reading file information
                    -> [DirectoryVisitOptions] -- ^ @visitOptions@ - options for visiting the directory
                    -> DirectoryVisitCallback  -- ^ @callback@ - a function to be called for each entry
                    -> IO ()
directoryVisitFiles textURI files infoOptions visitOptions callback =
    do cFiles <- mapM newUTFString files >>= toGList
       withUTFString textURI $ \cTextURI ->
           directoryVisitMarshal ({# call directory_visit_files #} cTextURI cFiles) infoOptions visitOptions callback

-- | Visit each file in a list contained with a directory at a
--   'URI', calling a 'DirectoryVisitCallback' for each one.
directoryVisitFilesAtURI :: URI                     -- ^ @uri@ - the 'URI' of the directory to visit
                         -> [String]                -- ^ @files@ - the files contained in @textURI@ to be visited
                         -> [FileInfoOptions]       -- ^ @infoOptions@ - options for reading file information
                         -> [DirectoryVisitOptions] -- ^ @visitOptions@ - options for visiting the directory
                         -> DirectoryVisitCallback  -- ^ @callback@ - a function to be called for each entry
                         -> IO ()
directoryVisitFilesAtURI uri files infoOptions visitOptions callback =
     do cFiles <- mapM newUTFString files >>= toGList
        directoryVisitMarshal ({# call directory_visit_files_at_uri #} uri cFiles) infoOptions visitOptions callback

-- | Create a list of 'FileInfo' objects representing each entry in the
--   directory at @textURI@, using options @options@.
directoryListLoad :: TextURI           -- ^ @textURI@ - String representation of the URI of the directory to load
                  -> [FileInfoOptions] -- ^ @options@ - options for reading file information
                  -> IO [FileInfo]     -- ^ the entries contined in the directory
directoryListLoad textURI options =
    let cOptions = cFromFlags options
    in withUTFString textURI $ \cTextURI ->
        alloca  $ \cListPtr ->
            genericResultMarshal ({# call directory_list_load #} cListPtr cTextURI cOptions)
                                  (peek cListPtr >>= readGList >>= mapM peek)
                                  (do cList <- peek cListPtr
                                      assert (cList == nullPtr) $ return ())
