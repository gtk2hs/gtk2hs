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
module System.GIO.File.ContentType (
-- * Details
--
-- | A content type is a platform specific string that defines the type of a file. On unix it is a mime
-- type, on win32 it is an extension string like ".doc", ".txt" or a percieved string like
-- "audio". Such strings can be looked up in the registry at HkeyClassesRoot.

-- * Methods
    contentTypeEquals,
    contentTypeIsA,
    contentTypeIsUnknown,
    contentTypeGetDescription,
    contentTypeGetMimeType,
    contentTypeGetIcon,
    contentTypeCanBeExecutable,
#if GLIB_CHECK_VERSION(2,18,0)
    contentTypeFromMimeType,
#endif
    contentTypeGuess,
#if GLIB_CHECK_VERSION(2,18,0)
    contentTypeGuessForTree,
#endif
    contentTypesGetRegistered,
    ) where

import Control.Monad
import System.GIO.Enums
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.GList
import System.Glib.GObject
import System.Glib.UTFString
{#import System.GIO.Types#}

{# context lib = "gio" prefix = "g" #}

-- | Compares two content types for equality.
contentTypeEquals ::
    GlibString string
 => string
 -> string
 -> Bool -- ^ returns 'True' if the two strings are identical or equivalent, 'False' otherwise.
contentTypeEquals type1 type2 =
  toBool $ unsafePerformIO $
  withUTFString type1 $ \ type1Ptr ->
  withUTFString type2 $ \ type2Ptr ->
  {#call g_content_type_equals#} type1Ptr type2Ptr

-- | Determines if type is a subset of supertype.
contentTypeIsA ::
    GlibString string
 => string
 -> string
 -> Bool -- ^ returns   'True' if type is a kind of supertype, 'False' otherwise.
contentTypeIsA type1 supertype =
  toBool $ unsafePerformIO $
  withUTFString type1 $ \ type1Ptr ->
  withUTFString supertype $ \ supertypePtr ->
  {#call g_content_type_equals#} type1Ptr supertypePtr

-- | Checks if the content type is the generic "unknown" type. On unix this is the
-- "application/octet-stream" mimetype, while on win32 it is \"*\".
contentTypeIsUnknown ::
    GlibString string
 => string
 -> Bool  -- ^ returns 'True' if the type is the unknown type.
contentTypeIsUnknown typ =
  toBool $ unsafePerformIO $
  withUTFString typ $ \ typPtr ->
  {#call g_content_type_is_unknown#} typPtr

-- | Gets the human readable description of the content type.
contentTypeGetDescription ::
    GlibString string
 => string
 -> string  -- ^ returns a short description of the content type type.
contentTypeGetDescription typ =
  unsafePerformIO $
  withUTFString typ $ \ typPtr ->
  {#call g_content_type_get_description#} typPtr
  >>= readUTFString

-- | Gets the mime-type for the content type. If one is registered
contentTypeGetMimeType ::
    GlibString string
 => string
 -> string -- ^ returns the registered mime-type for the given type, or 'Nothing' if unknown.
contentTypeGetMimeType typ =
  unsafePerformIO $
  withUTFString typ $ \ typPtr ->
  {#call g_content_type_get_mime_type#} typPtr
  >>= readUTFString

-- | Gets the icon for a content type.
contentTypeGetIcon ::
    GlibString string
 => string
 -> Icon  -- ^ returns 'Icon' corresponding to the content type.
contentTypeGetIcon typ =
  unsafePerformIO $ wrapNewGObject mkIcon $
  withUTFString typ $ \ typPtr ->
  {#call g_content_type_get_icon#} typPtr

-- | Checks if a content type can be executable. Note that for instance things like text files can be
-- executables (i.e. scripts and batch files).
contentTypeCanBeExecutable ::
    GlibString string
 => string
 -> Bool  -- ^ returns 'True' if the file type corresponds to a type that can be executable, 'False' otherwise.
contentTypeCanBeExecutable typ =
  toBool $ unsafePerformIO $
  withUTFString typ $ \ typPtr ->
  {#call g_content_type_can_be_executable#} typPtr

#if GLIB_CHECK_VERSION(2,18,0)
-- | Tries to find a content type based on the mime type name.
contentTypeFromMimeType ::
    GlibString string
 => string -- ^ @mimeType@ a mime type string.
 -> string
contentTypeFromMimeType mimeType =
  unsafePerformIO $
  withUTFString mimeType $ \ mimeTypePtr ->
  {#call g_content_type_from_mime_type#} mimeTypePtr
  >>= readUTFString
#endif

-- | Guesses the content type based on example data. If the function is uncertain, @resultUncertain@ will
-- be set to 'True'. Either filename or data may be 'Nothing', in which case the guess will be based solely on
-- the other argument.
contentTypeGuess ::
    (GlibFilePath fp, GlibString string)
 => fp
 -> string  -- ^ @data@             a stream of data,
 -> Int -- ^ @dataSize@        the size of data
 -> IO (Bool, string)  -- ^ returns          a string indicating a guessed content type for the given data.
contentTypeGuess filename dat dataSize  =
  withUTFFilePath filename $ \ filenamePtr ->
  withUTFString dat $ \ datPtr ->
  alloca $ \ resultUncertainPtr -> do
  strPtr <- {#call g_content_type_guess#}
           filenamePtr
           (castPtr datPtr)
           (fromIntegral dataSize)
           (castPtr resultUncertainPtr)
  resultUncertain <- peek resultUncertainPtr
  str <- readUTFString strPtr
  return (resultUncertain, str)

#if GLIB_CHECK_VERSION(2,18,0)
-- | Tries to guess the type of the tree with root root, by looking at the files it contains. The result
-- is an array of content types, with the best guess coming first.
--
-- The types returned all have the form x-content/foo, e.g. x-content/audio-cdda (for audio CDs) or
-- x-content/image-dcf (for a camera memory card). See the shared-mime-info specification for more on
-- x-content types.
--
-- This function is useful in the implementation of 'mountGuessContentType'.
contentTypeGuessForTree ::
    (FileClass file, GlibString string)
 => file  -- ^ @root@    the root of the tree to guess a type for
 -> IO [string]  -- ^ returns a list of possible content types
contentTypeGuessForTree root =
  {#call g_content_type_guess_for_tree#} (toFile root)
  >>= readUTFStringArray0
#endif

-- | Gets a list of strings containing all the registered content types known to the system.
contentTypesGetRegistered ::
    GlibString string
 => IO [string]  -- ^ returns GList of the registered content types.
contentTypesGetRegistered = do
  glistPtr <- {#call g_content_types_get_registered#}
  strPtrs <- fromGList glistPtr
  mapM readUTFString strPtrs


