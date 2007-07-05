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
module System.Gnome.VFS.MIME (
  
-- * Types
  MIMEType,

-- * MIME Type Operations
  mimeTypeFromNameOrDefault,
  getMIMETypeCommon,
  getMIMETypeFromURI,
  getFileMIMETypeFast,
  getFileMIMEType,
  mimeTypeIsSupertype,
  getSupertypeFromMIMEType,
  mimeInfoCacheReload
  
  ) where

import Control.Monad (liftM)
import System.Glib.Flags
import System.Glib.FFI
import System.Glib.UTFString
{#import System.Gnome.VFS.Types#}
{#import System.Gnome.VFS.Marshal#}

#c
#include <libgnomevfs/gnome-vfs-mime.h>
#endc

{# context lib = "gnomevfs" prefix = "gnome_vfs" #}

-- | Try to determine the MIME-type of the file at @filename@, using
--   only the filename and the Gnome VFS MIME type database. If the
--   MIME-type is not found, return @defaultv@.
mimeTypeFromNameOrDefault :: FilePath       -- ^ @filename@ - the file
                                            --   to get the MIME-type
                                            --   for
                          -> Maybe MIMEType -- ^ @defaultv@ - the
                                            --   default MIME-type to
                                            --   return if no match is
                                            --   found
                          -> Maybe MIMEType -- ^ the MIME-type of the
                                            --   filename, or @defaultv@
mimeTypeFromNameOrDefault filename defaultv =
    unsafePerformIO $ maybeWith withUTFString defaultv $ \cDefaultv ->
        withUTFString filename $ \cFilename ->
            {# call mime_type_from_name_or_default #} cFilename cDefaultv >>=
                maybePeek peekUTFString

-- | Try to get the MIME-type of the file represented by @uri@. This
--   function favors the contents of the file over the extension of
--   the filename. If the file does not exist, the MIME-type for the
--   extension is returned. If no MIME-type can be found for the file,
--   the function returns \"application\/octet-stream\".
--   
--   Note: This function will not necessarily return the same
--   MIME-type as 'getFileInfo'.
getMIMETypeCommon :: URI       -- ^ @uri@ - the URI of the file to examine
                  -> IO String -- ^ the guessed MIME-type
getMIMETypeCommon uri =
    {# call get_mime_type_common #} uri >>= peekUTFString

-- | Try to get the MIME-type of the file represented by @uri@. This
--   function looks only at the filename pointed to by @uri@.
getMIMETypeFromURI :: URI       -- ^ @uri@ - the URI to examine
                   -> IO String -- ^ the guessed MIME-type
getMIMETypeFromURI uri =
    {# call get_mime_type_from_uri #} uri >>= peekUTFString

getFileMIMETypeFast :: FilePath  -- ^ 
                    -> IO String
getFileMIMETypeFast path =
    withUTFString path $ \cPath ->
        {# call get_file_mime_type_fast #} cPath nullPtr >>=
        peekUTFString

-- | Try to guess the MIME-type of the file represented by @path@. If
--   @suffixOnly@ is 'False', use the MIME-magic based lookup
--   first. Handles non-existant files by returning a type based on
--   the file extension.
getFileMIMEType :: FilePath
                -> Bool
                -> IO String
getFileMIMEType path suffixOnly =
    withUTFString path $ \cPath ->
        let cSuffixOnly = fromBool suffixOnly
        in {# call get_file_mime_type #} cPath nullPtr cSuffixOnly >>=
               peekUTFString

-- | Returns 'True' if @mimeType@ is of the form @foo\/\*@, and 'False'
--   otherwise.
mimeTypeIsSupertype :: String
                    -> Bool
mimeTypeIsSupertype mimeType =
    toBool $ unsafePerformIO $
        withUTFString mimeType {# call mime_type_is_supertype #}

-- | Returns the supertype for @mimeType@. The supertype of an
--   application is computed by removing its suffix, and replacing it
--   with @\*@. Thus, @foo\/bar@ will be converted to @foo\/\*@.
getSupertypeFromMIMEType :: String
                         -> String
getSupertypeFromMIMEType mimeType =
    unsafePerformIO $ withUTFString mimeType {# call get_supertype_from_mime_type #} >>=
        readUTFString

-- | Reload the MIME information for the specified directory.
mimeInfoCacheReload :: FilePath
                    -> IO ()
mimeInfoCacheReload dir =
    withUTFString dir {# call mime_info_cache_reload #}

-- | Reload the MIME database.
mimeReload :: IO ()
mimeReload = {# call mime_reload #}
