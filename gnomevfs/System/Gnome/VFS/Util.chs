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
module System.Gnome.VFS.Util (

-- * String Formatting Functions
  formatFileSizeForDisplay,
  formatURIForDisplay,

-- * External Applications
  urlShow,
  urlShowWithEnv,
  isExecutableCommandString,
  
-- * String Escaping Functions
  escapeString,
  escapePathString,
  escapeHostAndPathString,
  escapeSlashes,
  escapeSet,
  unescapeString,
  unescapeStringForDisplay,

-- * 'TextURI' and Path Functions
  makeURICanonical,
  makeURICanonicalStripFragment,
  makePathNameCanonical,
  makeURIFromInput,
  makeURIFromInputWithDirs,
  makeURIFromShellArg,
  expandInitialTilde,
  getLocalPathFromURI,
  getURIFromLocalPath,
  iconPathFromFilename,
  getVolumeFreeSpace,
  urisMatch,
  getURIScheme,

-- * Miscellaneous Functions
  isPrimaryThread,
  openFD,
  
  ) where

import Control.Exception (assert)
import Control.Monad (liftM)
import System.Posix.Types (Fd)
import System.Glib.FFI
import System.Glib.UTFString
{#import System.Gnome.VFS.Types#}
{#import System.Gnome.VFS.Marshal#}

{# context lib = "gnomevfs" prefix = "gnome_vfs" #}

-- | Formats @size@ so that it is easy for the user to read. Gives the
--   size in bytes, kilobytes, megabytes or gigabytes, choosing
--   whatever is appropriate.
formatFileSizeForDisplay :: FileSize -- ^ @size@ - the file size to be formatted
                         -> String   -- ^ the formatted size ready for display
formatFileSizeForDisplay size =
    unsafePerformIO $ {# call format_file_size_for_display #} (fromIntegral size) >>= readUTFString

-- | Filter, modify, unescape, and change @textURI@ to make it appropriate
--   for display to users.
--   
--   Rules: A @file:@ URI without fragments should appear as a local
--   path. A @file:@ URI with fragments should appear as @file:uri@. All
--   other URIs appear as expected.
formatURIForDisplay :: TextURI       -- ^ @textURI@ - the URI to format
                    -> Maybe String  -- ^ the formatted URI ready for display
formatURIForDisplay textURI =
    unsafePerformIO $ withUTFString textURI {# call format_uri_for_display #} >>= maybePeek readUTFString

-- | Launches the default application or component associated with the
--   given URL.
urlShow :: String -- ^ @url@ - the URL to launch an application for
        -> IO ()
urlShow url =
    voidResultMarshal $ withUTFString url {# call url_show #}

-- | Like 'urlShow', but using the specified environment variables.
urlShowWithEnv :: String   -- ^ @url@ - the URL to launch an application for
               -> [String] -- ^ @env@ - a list of strings @[\"VARIABLE1=value1\", \"VARIABLE2=value2\", ...]@
               -> IO ()
urlShowWithEnv url env =
    voidResultMarshal $ withUTFStringArray env $ \cEnv ->
        withUTFString url $ \cURL ->
            {# call url_show_with_env #} cURL cEnv

marshalPureString :: IO CString
                  -> String
marshalPureString cAction =
    unsafePerformIO $ cAction >>= readUTFString

marshalPureMaybeString :: IO CString
                        -> Maybe String
marshalPureMaybeString cAction =
    unsafePerformIO $ cAction >>= maybePeek readUTFString

-- | Escapes @string@, replacing any and all special characters with
--   equivalent escape sequences.
escapeString :: String -- ^ @string@ - the string to be escaped
             -> String -- the escaped string
escapeString string =
    marshalPureString $ withUTFString string {# call escape_string #}

-- | Escapes path, replacing only special characters that would not be
--   found in paths (so @\'\/\'@, @\'&\'@, and @\'=\'@ will not be escaped by this
--   function).
escapePathString :: FilePath -- ^ @path@ - the path string to be escaped
                 -> String   -- ^ the escaped string
escapePathString path =
    marshalPureString $ withUTFString path {# call escape_path_string #}

-- | Escapes path, replacing only special characters that would not be
--   found in a path or host name (so @\'\/\'@, @\'&\'@, @\'=\'@, @\':\'@ and @\'\@\'@ will
--   not be escaped by this function).
escapeHostAndPathString :: FilePath -- ^ @path@ - the path to be escaped
                        -> String   -- ^ the escaped string
escapeHostAndPathString path =
    marshalPureString $ withUTFString path {# call escape_host_and_path_string #}

-- | Escapes only @\'\/\'@ and @\'%\'@ characters in @string@, replacing
--   them with their escape sequence equivalents.
escapeSlashes :: String -- ^ @string@ - the string to be escaped
              -> String -- ^ the escaped string
escapeSlashes string =
    marshalPureString $ withUTFString string {# call escape_slashes #}

-- | Escapes the characters listed in @matchSet@ in @string@.
escapeSet :: String -- ^ @string@ - the string to be escaped
          -> String -- ^ @matchSet@ - the characters to escape
          -> String -- ^ the escaped string
escapeSet string matchSet =
    marshalPureString $ withUTFString matchSet $ \cMatchSet ->
        withUTFString string $ \cString ->
            {# call escape_set #} cString cMatchSet

-- | Decodes escaped characters (i.e., @%xx@ sequences) in
--   @escapedString@. Characters are decoded in @%xx@ form, where
--   @xx@ is the hex code for an ASCII character.
unescapeString :: String -- @string@ - the string to be unescaped
               -> String -- @illegalCharacters@ - the characters that must not be escaped
               -> String -- ^ the unescaped string
unescapeString escapedString illegalCharacters =
    marshalPureString $ withUTFString illegalCharacters $ \cIllegalCharacters ->
        withUTFString escapedString $ \cEscapedString ->
            {# call unescape_string #} cEscapedString cIllegalCharacters

-- | Standardizes the format of @uri@, so that it can be used later
--   in other functions that expect a canonical URI.
makeURICanonical :: TextURI       -- ^ @textURI@ - an absolute or relative URI; it may have a scheme
                 -> Maybe TextURI -- ^ the canonical representation of the URI
makeURICanonical textURI =
    unsafePerformIO $ withUTFString textURI {# call make_uri_canonical #} >>= maybePeek readUTFString

-- | Returns a canonicalized URI. If @uri@ contains a fragment
--   (anything after a @\'#\'@), it is stripped off, and the resulting
--   URI is made canonical.
makeURICanonicalStripFragment :: TextURI       -- ^ @textURI@ - the URI to canonicalize
                              -> Maybe TextURI -- ^ the canonical representation of the URI
makeURICanonicalStripFragment textURI =
    unsafePerformIO $ withUTFString textURI {# call make_uri_canonical_strip_fragment #} >>= maybePeek readUTFString


-- | Returns a canonicalized path name.
makePathNameCanonical :: FilePath      -- ^ @pathName@ - the path name to canonicalize
                      -> Maybe TextURI -- ^ the canonicalized path name
makePathNameCanonical pathName =
    unsafePerformIO $ withUTFString pathName {# call make_path_name_canonical #} >>= maybePeek readUTFString

-- | Takes a user input path\/URI and makes a valid URI out of it.
--   
--   This function is the reverse of 'formatURIForDisplay'.
makeURIFromInput :: String        -- ^ @location@ - the input to try to parse
                 -> Maybe TextURI -- ^ the resulting URI, or 'Nothing' if @location@ is invalid
makeURIFromInput location =
    unsafePerformIO $ withUTFString location {# call make_uri_from_input #} >>= maybePeek readUTFString

-- | Determine a fully qualified URI from a relative or absolute input
--   path. The directories specified by @dirs@ are searched when the
--   path is relative.
makeURIFromInputWithDirs :: FilePath      -- ^ @location@ - the relative or absolute input path to resolve
                         -> [MakeURIDirs] -- ^ @dirs@ - the directories to search
                         -> IO TextURI    -- ^ the resulting URI
makeURIFromInputWithDirs location dirs =
    (withUTFString location $ flip {# call make_uri_from_input_with_dirs #} $ cFromFlags dirs) >>= readUTFString

-- | Similar to 'makeURIFromInput', except:
--   
--   1. guesses relative paths instead of HTTP domains
--   
--   2. doesn\'t bother stripping leading\/trailing white space
--   
--   3. doesn\'t bother with tilde expansion -- that\'s done by the shell
makeURIFromShellArg :: String
                    -> String
makeURIFromShellArg uri =
    unsafePerformIO $ withUTFString uri {# call make_uri_from_shell_arg #} >>= readUTFString

-- | If @path@ begins with a tilde, representing the user's home
--   directory, expand it to the actual directory.
expandInitialTilde :: String
                   -> IO String
expandInitialTilde path =
    withUTFString path {# call expand_initial_tilde #} >>= readUTFString

-- | Similar to @unescapeString@, but returns something
--   semi-intelligible to the user, even upon receiving traumatic
--   input such as @00@ or URIs in bad form.
--   
--   WARNING: You should never use this function on a whole URI! It
--   unescapes reserved characters, and can result in a mangled URI
--   that can not be re-entered. For example, it unescapes @\'#\'@, @\'&\'@ and
--   @\'?\'@, which have special meanings in URI strings.
unescapeStringForDisplay :: String
                         -> String
unescapeStringForDisplay escaped =
    marshalPureString $ withUTFString escaped {# call unescape_string_for_display #}

-- | Create a local path for a uri.
--
--   If @uri@ is not a @file:\/\/\/@ URI, or it contains a fragment
--   identifier or is chained, this function returns 'Nothing'.
getLocalPathFromURI :: TextURI        -- ^ the URI to convert
                    -> Maybe FilePath -- ^ the resulting path
getLocalPathFromURI uri =
    marshalPureMaybeString $ withUTFString uri {# call get_local_path_from_uri #}

-- | Returns a @file:\/\/\/@ URI for the local path @localFullPath@,
--   such as a path provided by
--   'Graphics.UI.Gtk.Selectors.FileChooser.fileChooserGetFilename'. The
--   resulting URI may be provided, for instance, to
--   'System.Gnome.VFS.URI.uriFromString'.
--   
--   On Windows 'localFullPath' should be in the UTF-8 encoding, and
--   can start with a drive letter, but doesn't have to.
getURIFromLocalPath :: FilePath -- ^ @localFullPath@ - 
                    -> TextURI  -- ^ the resulting URI
getURIFromLocalPath localFullPath =
    marshalPureString $ withUTFString localFullPath {# call get_uri_from_local_path #}

-- | Checks if @commandString@ starts with the full path of an
--   executable file or an executable in the system path.
isExecutableCommandString :: String  -- ^ @commandString@ - 
                          -> IO Bool -- 'True' is @commandString@ is an executable command string, otherwise 'False'
isExecutableCommandString commandString =
    liftM toBool $ withUTFString commandString {# call is_executable_command_string #}

-- | Stores the amount of free space in bytes on @uri@'s volume in
-- | size.
getVolumeFreeSpace :: URI         -- ^ @uri@ - a URI to a file on a volume
                   -> IO FileSize -- ^ the free space in bytes on the volume
getVolumeFreeSpace uri =
    alloca $ \cFileSizePtr ->
        genericResultMarshal ({# call get_volume_free_space #} uri cFileSizePtr)
                              (liftM fromIntegral $ peek cFileSizePtr)
                              (do cFileSize <- peek cFileSizePtr
                                  assert (cFileSize == 0) $ return ())

-- | Returns the icon path for @filename@. Example:
--   
--   @'iconPathFromFilename' \"nautilus\/nautilus-desktop.png\"@ will
--   return a string forming the full path of the file
--   @nautilus-desktop.png@, i.e.
--   @${prefix}\/share\/pixmaps\/nautilus\/nautilus-desktop.png@.
iconPathFromFilename :: String    -- ^ @filename@ - a relative or absolute pathname
                     -> IO String -- ^ the absolute path to the icon file
iconPathFromFilename filename =
    withUTFString filename {# call icon_path_from_filename #} >>= readUTFString

-- | Check if the current thread is the thread with the main glib
--   event loop.
isPrimaryThread :: IO Bool -- ^ 'True' if the current thread is the
                           --   thread with the main glib event loop,
                           --   otherwise 'False'
isPrimaryThread =
    liftM toBool {# call is_primary_thread #}

-- | Retrieves the scheme used in @uri@.
getURIScheme :: TextURI      -- ^ @uri@ - 
             -> Maybe String -- ^ the scheme used in @uri@, or 'Nothing' if @uri@ does not use a scheme
getURIScheme uri =
    marshalPureMaybeString $ withUTFString uri {# call get_uri_scheme #}

-- | Compare two URIs.
urisMatch :: TextURI -- ^ @uri1@ - 
          -> TextURI -- ^ @uri2@ - 
          -> Bool    -- ^ 'True' if the URIs are the same, 'False' otherwise.
urisMatch uri1 uri2 =
    unsafePerformIO $ liftM toBool $ withUTFString uri1 $ \cURI1 ->
        withUTFString uri2 $ {# call uris_match #} cURI1

-- | Convert an open unix file descriptor into a 'Handle' object.
openFD :: Fd        -- ^ @filedes@ - the file descriptor to use
       -> IO Handle -- ^ the returned handle
openFD filedes =
    newObjectResultMarshal Handle $ \cHandlePtr ->
        {# call open_fd #} (castPtr cHandlePtr) $ fromIntegral filedes
