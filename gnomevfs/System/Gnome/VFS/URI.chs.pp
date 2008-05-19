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
module System.Gnome.VFS.URI (

-- * Types  
  URI,
  ToplevelURI,
  TextURI,
  URIHideOptions(..),

-- * Operations
  uriFromString,
  uriResolveRelative,
#if GNOME_VFS_CHECK_VERSION(2,16,0)
  uriResolveSymbolicLink,
#endif
  uriAppendString,
  uriAppendPath,
  uriAppendFileName,
  uriToString,
  uriIsLocal,
  uriHasParent,
  uriGetParent,
  uriGetToplevel,
  uriGetHostName,
  uriGetScheme,
  uriGetHostPort,
  uriGetUserName,
  uriGetPassword,
  uriSetHostName,
  uriSetHostPort,
  uriSetUserName,
  uriSetPassword,
  uriEqual,
  uriIsParent,
  uriGetPath,
  uriGetFragmentIdentifier,
  uriExtractDirname,
  uriExtractShortName,
  uriExtractShortPathName,
  uriListParse,
  uriMakeFullFromRelative
  ) where

import Control.Monad (liftM)
{#import System.Gnome.VFS.Marshal#}
{#import System.Gnome.VFS.Types#}
import System.Glib.FFI
import System.Glib.UTFString
{#import System.Glib.GList#}
import System.IO (FilePath)

{# context lib = "gnomevfs" prefix = "gnome_vfs" #}

-- | Create a new 'URI' from @textURI@.  Unsupported and unsafe
--   methods are not allowed and will result in 'Nothing' being
--   returned. URL transforms are allowed.
uriFromString :: TextURI
              -> Maybe URI
uriFromString textURI = 
    unsafePerformIO $ withUTFString textURI {# call uri_new #} >>= maybePeek newURI

-- | Create a new uri from @relativeReference@, relative to
--   @base@. The resolution algorithm in some aspects follows RFC
--   2396, section 5.2, but is not identical due to some extra
--   assumptions GnomeVFS makes about URIs.
--   
--   If relative_reference begins with a valid scheme identifier
--   followed by @\':\'@, it is assumed to refer to an absolute URI, and a
--   'URI' is created from it using 'uriFromString'.
--   
--   Otherwise, depending on its precise syntax, it inherits some
--   aspects of the parent URI, but the parents' fragment and query
--   components are ignored.
--   
--   If relative_reference begins with @\"\/\/\"@, it only inherits the
--   base scheme; if it begins with @\'\/\'@ (i.e., it is an absolute
--   path reference), it inherits everything except the base
--   path. Otherwise, it replaces the part of base after the last
--   @\'\/\'@.
--   
--   Note: This function should not be used by application authors
--   unless they expect very distinct semantics. Instead, authors
--   should use 'uriAppendFileName', 'uriAppendPath',
--   'uriAppendString' or 'uriResolveSymbolicLink'.
uriResolveRelative :: URI       -- ^ @base@ - the base URI
                   -> String    -- ^ @relativeReference@ - a string
                                --   representing a possibly relative
                                --   URI reference
                   -> Maybe URI -- ^ a new URI referring to
                                --   @relativeReference@, or 'Nothing'
                                --   if @relativeReference@ is
                                --   malformed.
uriResolveRelative base relativeReference =
    unsafePerformIO $ (withUTFString relativeReference $
                          {# call uri_resolve_relative #} base) >>= maybePeek newURI

#if GNOME_VFS_CHECK_VERSION(2,16,0)
-- | Create a new uri from @symbolicLink@, relative to @base@.
--   
--   If symbolic_link begins with a @\'\/\'@, it replaces the path of base,
--   otherwise it is appended after the last @\'\/\'@ character of base.
uriResolveSymbolicLink :: URI
                       -> String
                       -> Maybe URI
uriResolveSymbolicLink base symbolicLink =
    unsafePerformIO $ (withUTFString symbolicLink $
                          {# call uri_resolve_symbolic_link #} base) >>= maybePeek newURI
#endif

-- | Create a new URI obtained by appending @uriFragment@ to @uri@. This
--   will take care of adding an appropriate directory separator
--   between the end of @uri@ and the start of @uriFragment@ if
--   necessary.
--   
--   This function will return 'Nothing' if the resulting URI is not
--   valid.
uriAppendString :: URI       -- ^ @uri@ - the base URI
                -> String    -- ^ @uriFragment@ - an escaped URI fragment
                -> Maybe URI -- ^ the new URI
uriAppendString uri uriFragment =
    unsafePerformIO $ (withUTFString uriFragment $
                           {# call uri_append_string #} uri) >>= maybePeek newURI

-- | Create a new uri obtained by appending @path@ to @uri@. This will
--   take care of adding an appropriate directory separator between
--   the end of @uri@ and the start of @path@ if necessary, as well as
--   escaping @path@ as necessary.
--   
--   This function will return 'Nothing' if the resulting URI is not
--   valid.
uriAppendPath :: URI       -- ^ @uri@ - the base URI
              -> FilePath  -- ^ @path@ - a non-escaped file path
              -> Maybe URI -- ^ the new URI
uriAppendPath uri path =
    unsafePerformIO $ (withUTFString path $
                           {# call uri_append_path #} uri) >>= maybePeek newURI

-- | Create a new URI obtained by appending @fileName@ to @uri@. This
--   will take care of adding an appropriate directory separator
--   between the end of @uri@ and the start of @fileName@ if
--   necessary. @fileName@ might, for instance, be the result of a call
--   to 'System.Posix.Directory.readDirStream'.
--   
--   This function will return 'Nothing' if the resulting URI is not
--   valid.
uriAppendFileName :: URI
                  -> FilePath
                  -> Maybe URI
uriAppendFileName uri fileName =
    unsafePerformIO $ (withUTFString fileName $
                           {# call uri_append_file_name #} uri) >>= maybePeek newURI

-- | Translate @uri@ into a printable string. The string will not
--   contain the URI elements specified by @hideOptions@.
--   
--   A @file:@ URI on Win32 might look like
--   @file:\/\/\/x:\/foo\/bar.txt@. Note that the part after
--   @file:\/\/@ is not a legal file name, you need to remove the @\/@
--   in front of the drive letter. This function does that
--   automatically if @hideOptions@ specifies that the toplevel
--   method, user name, password, host name and host port should be
--   hidden.
--   
--   On the other hand, a @file:@ URI for a UNC path looks like
--   @file:\/\/\/\/server\/share\/foo\/bar.txt@, and in that case the part
--   after @file:\/\/@ is the correct file name.
uriToString :: URI            -- ^ @uri@ - a URI
            -> URIHideOptions -- ^ @hideOptions@ - the URI elements that should not be included in the resulting string
            -> TextURI        -- ^ the resulting string
uriToString uri hideOptions =
    unsafePerformIO $ ({# call uri_to_string #} uri $
                           cFromEnum hideOptions) >>= readUTFString

-- | Check if @uri@ is a local URI. Note that the return value of this
--   function entirely depends on the method associated with
--   the URI. It is up to the method author to distinguish between
--   remote URIs and URIs referring to entities on the local computer.
--   
--   Warning, this can be slow, as it does I\/O to detect things like
--   NFS mounts.
uriIsLocal :: URI     -- ^ @uri@ - 
           -> IO Bool -- ^ 'True' if @uri@ is local, 'False' otherwise
uriIsLocal uri =
    liftM toBool $ {# call uri_is_local #} uri

-- | Check whether @uri@ has a parent or not.
uriHasParent :: URI  -- ^ @uri@ - 
             -> Bool -- ^ 'True' if @uri@ has a parent, 'False' otherwise
uriHasParent uri =
    unsafePerformIO $ liftM toBool $ {# call uri_has_parent #} uri

-- | Retrieve @uri@'s parent URI.
uriGetParent :: URI       -- ^ @uri@ - 
             -> Maybe URI -- ^ the parent URI, or 'Nothing' if @uri@ has no parent
uriGetParent uri =
    unsafePerformIO $ {# call uri_get_parent #} uri >>= maybePeek newURI

-- | Retrieve @uri@'s toplevel URI.
uriGetToplevel :: URI         -- ^ @uri@ - 
               -> ToplevelURI -- ^ the toplevel URI
uriGetToplevel uri =
    unsafePerformIO $ {# call uri_get_toplevel #} uri >>= newToplevelURI

-- | Retrieve the hostname for @uri@.
uriGetHostName :: URI          -- ^ @uri@ -
               -> Maybe String -- ^ the hostname, or 'Nothing' if @uri@ has no hostname
uriGetHostName uri =
    unsafePerformIO $ {# call uri_get_host_name #} uri >>= (maybePeek peekUTFString)

-- | Retrieve the scheme for @uri@.
uriGetScheme :: URI          -- ^ @uri@ - 
             -> Maybe String -- ^ the scheme, or 'Nothing' if @uri@ has no scheme
uriGetScheme uri =
    unsafePerformIO $ {# call uri_get_scheme #} uri >>= (maybePeek peekUTFString)

-- | Retrieve the host port for @uri@.
uriGetHostPort :: URI        -- ^ @uri@ - 
               -> Word       -- ^ the host port, or @0@ if the default port
                             --   value for the specified toplevel access
                             --   method is used
uriGetHostPort uri =
    unsafePerformIO $ liftM cToEnum $ {# call uri_get_host_port #} uri

-- | Retrieve the user name for @uri@.
uriGetUserName :: URI          -- ^ @uri@ - 
               -> Maybe String -- ^ the user name, or 'Nothing' if @uri@ has no user name
uriGetUserName uri =
    unsafePerformIO $ {# call uri_get_user_name #} uri >>= (maybePeek peekUTFString)

-- | Retrieve the password for @uri@.
uriGetPassword :: URI          -- ^ @uri@ - 
               -> Maybe String -- ^ the password, or 'Nothing' if @uri@ has no password
uriGetPassword uri =
    unsafePerformIO $ {# call uri_get_password #} uri >>= (maybePeek peekUTFString)

marshalSet :: (URI -> a -> IO ())
           -> URI
           -> a
           -> URI
marshalSet setAction uri newVal =
    unsafePerformIO $ do uri <- {# call uri_dup #} uri >>= newURI
                         setAction uri newVal
                         return uri

-- | Create a new 'URI' using @uri@, replacing the host name by @hostName@.
uriSetHostName :: URI          -- ^ @uri@ - 
               -> Maybe String -- ^ @hostName@ - the new hostname
               -> URI          -- ^ the resulting URI
uriSetHostName =
    marshalSet $ \uri hostName ->
        maybeWith withUTFString hostName $ {# call uri_set_host_name #} uri

-- | Create a new 'URI' using @uri@, replacing the host port by @hostPort@.
--   
--   If @hostPort@ is @0@, use the default port for @uri@'s toplevel
--   access method.
uriSetHostPort :: URI        -- ^ @uri@ - 
               -> Word       -- ^ @hostPort@ - the new host port
               -> URI        -- ^ the resulting URI
uriSetHostPort =
    marshalSet $ \uri hostPort ->
        {# call uri_set_host_port #} uri $ cFromEnum hostPort

-- | Create a new 'URI' using @uri@, replacing the user name by @userName@.
uriSetUserName :: URI          -- ^ @uri@ - 
               -> Maybe String -- ^ @userName@ - the new user name
               -> URI          -- ^ the resulting URI
uriSetUserName =
    marshalSet $ \uri userName ->
        maybeWith withUTFString userName $ {# call uri_set_user_name #} uri

-- | Create a new 'URI' using @uri@, replacing the password by @password@.
uriSetPassword :: URI          -- ^ @uri@ - 
               -> Maybe String -- ^ @password@ - the new password
               -> URI          -- ^ the resulting URI
uriSetPassword  =
    marshalSet $ \uri password ->
        maybeWith withUTFString password $ {# call uri_set_password #} uri

-- | Compare two 'URI's for equality.
uriEqual :: URI  -- ^ @a@ - 
         -> URI  -- ^ @b@ - 
         -> Bool -- ^ 'True' if the URIs are the same, 'False' otherwise.
uriEqual a b =
    unsafePerformIO $ liftM toBool $ {# call uri_equal #} a b

-- | Check if @possibleChild@ is contained in @possibleParent@. If
--   @recursive@ is 'False', just try the immediate parent; otherwise
--   search up through the heirarchy.
uriIsParent :: URI  -- ^ @possibleParent@ - 
            -> URI  -- ^ @possibleChild@ -
            -> Bool -- ^ @recursive@ - 'True' if parents should be
                    --   checked recursively, 'False' otherwise
            -> Bool -- ^ 'True' if @possibleChild@ is contained in
                    --   @possibleParent@, otherwise 'False'
uriIsParent possibleParent possibleChild recursive =
    unsafePerformIO $ liftM toBool $
              {# call uri_is_parent #} possibleParent possibleChild $ fromBool recursive

-- | Retrieve the path name for @uri@.
uriGetPath :: URI            -- ^ @uri@ - 
           -> Maybe FilePath -- ^ the path name, or 'Nothing' if @uri@
                             --   has no path name
uriGetPath uri =
    unsafePerformIO $ {# call uri_get_path #} uri >>= (maybePeek peekUTFString)

-- | Retrieve the fragment identifier for @uri@.
uriGetFragmentIdentifier :: URI          -- ^ @uri@ - 
                         -> Maybe String -- ^ the fragment identifier,
                                         --   or 'Nothing' if @uri@
                                         --   has no fragment
                                         --   identifier
uriGetFragmentIdentifier uri =
    unsafePerformIO $ {# call uri_get_fragment_identifier #} uri >>= (maybePeek peekUTFString)

-- | Extract the name of the directory in which the file pointed to by
--   @uri@ is stored as a string. The string will end with a directory
--   separator.
uriExtractDirname :: URI            -- ^ @uri@ - 
                  -> Maybe FilePath -- ^ the directory name, or
                                    --   'Nothing' if @uri@ has no
                                    --   directory name
uriExtractDirname uri =
    unsafePerformIO $ {# call uri_extract_dirname #} uri >>= (maybePeek readUTFString)


-- | Retrieve base file name for @uri@, ignoring any trailing path
--   separators. This matches the XPG definition of basename, but not
--   'System.FilePath.basename'. This is often useful when you want
--   the name of something that's pointed to by a URI, and don't care
--   whether the uri has a directory or file form. If @uri@ points to
--   the root of a domain, returns the host name. If there's no host
--   name, returns the path separator.
--   
--   See also: 'uriExtractShortPathName'.
uriExtractShortName :: URI    -- ^ @uri@ - 
                    -> String -- the unescaped short form of the name
uriExtractShortName uri =
    unsafePerformIO $ {# call uri_extract_short_name #} uri >>= readUTFString

-- | Retrieve base file name for @uri@, ignoring any trailing path
--   separators. This matches the XPG definition of basename, but not
--   'System.FilePath.basename'. This is often useful when you want
--   the name of something that's pointed to by a URI, and don't care
--   whether the uri has a directory or file form. If @uri@ points to
--   the root of any domain, returns the path separator.
--   
--   See also: 'uriExtractShortName'.
uriExtractShortPathName :: URI    -- ^ @uri@ - 
                        -> String -- the
uriExtractShortPathName uri =
    unsafePerformIO $ {# call uri_extract_short_path_name #} uri >>= readUTFString

-- | Extracts a list of URIs from a standard @text\/uri-list@, such as
--   one would get on a drop operation.
uriListParse :: String -- ^ @uriList@ - a list of URIs, separated by newlines
             -> [URI]  -- ^ the list of URIs
uriListParse uriList =
    unsafePerformIO $ do uriList <- withUTFString uriList $ \cURIList ->
                                    {# call uri_list_parse #} cURIList >>= fromGList
                         sequence $ map newURI uriList

-- | Returns a full URI given a full base URI, and a secondary URI
--   which may be relative.
uriMakeFullFromRelative :: String       -- ^ @baseURI@ - 
                        -> String       -- ^ @relativeURI@ - 
                        -> Maybe String -- ^ the resulting URI
uriMakeFullFromRelative baseURI relativeURI =
    unsafePerformIO $ (withUTFString baseURI $ \cBaseURI ->
                           withUTFString relativeURI $ \cRelativeURI ->
                               {# call uri_make_full_from_relative #} cBaseURI cRelativeURI) >>= maybePeek readUTFString
